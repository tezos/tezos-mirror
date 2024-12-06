(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Evm_context_types

type init_status = Loaded | Created

type head = {
  current_block_hash : Ethereum_types.block_hash;
  finalized_number : Ethereum_types.quantity;
  next_blueprint_number : Ethereum_types.quantity;
  evm_state : Evm_state.t;
  pending_upgrade : Evm_events.Upgrade.t option;
}

type parameters = {
  kernel_path : string option;
  data_dir : string;
  preimages : string;
  preimages_endpoint : Uri.t option;
  smart_rollup_address : string option;
  fail_on_missing_blueprint : bool;
  store_perm : [`Read_only | `Read_write];
  block_storage_sqlite3 : bool;
  garbage_collector : Configuration.garbage_collector option;
  sequencer_wallet : (Client_keys.sk_uri * Client_context.wallet) option;
}

type session_state = {
  mutable context : Irmin_context.rw;
  mutable finalized_number : Ethereum_types.quantity;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  mutable pending_upgrade : Evm_store.pending_kernel_upgrade option;
  mutable evm_state : Evm_state.t;
  mutable last_split_block : (Ethereum_types.quantity * Time.Protocol.t) option;
      (** Garbage collector session related information. *)
  mutable last_gc_block : (Ethereum_types.quantity * Time.Protocol.t) option;
      (** Garbage collector session related information. *)
}

type history =
  | Full of {parameters : Configuration.garbage_collector}
  | Archive

type t = {
  data_dir : string;
  index : Irmin_context.rw_index;
  preimages : string;
  preimages_endpoint : Uri.t option;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  store : Evm_store.t;
  session : session_state;
  fail_on_missing_blueprint : bool;
  block_storage_sqlite3 : bool;
  history : history;
  sequencer_wallet : (Client_keys.sk_uri * Client_context.wallet) option;
}

type store_info = {
  rollup_address : Address.t;
  current_number : Ethereum_types.quantity;
}

let session_to_head_info session =
  {
    evm_state = session.evm_state;
    finalized_number = session.finalized_number;
    next_blueprint_number = session.next_blueprint_number;
    current_block_hash = session.current_block_hash;
    pending_upgrade =
      Option.map
        (fun pending_upgrade -> pending_upgrade.Evm_store.kernel_upgrade)
        session.pending_upgrade;
  }

let pvm_config ctxt =
  Config.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

module Types = struct
  type state = t

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = Evm_context_events.section @ ["worker"]

  let pp _fmt () = ()

  let equal () () = true
end

let head_info, head_info_waker = Lwt.task ()

let init_status, init_status_waker = Lwt.task ()

let execution_config, execution_config_waker = Lwt.task ()

let global_lockfile_path ~data_dir = Filename.Infix.(data_dir // "lock")

type error += Data_dir_locked of string

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.datadir_is_locked"
    ~title:"Data directory of EVM node is locked"
    ~description:"Data directory of EVM node is locked by another process."
    ~pp:(fun ppf dir ->
      Format.fprintf
        ppf
        "The data directory %s of the EVM node is locked by another process."
        dir)
    Data_encoding.(obj1 (req "data_dir" string))
    (function Data_dir_locked dir -> Some dir | _ -> None)
    (fun dir -> Data_dir_locked dir)

let lock_data_dir ~data_dir =
  let open Lwt_result_syntax in
  let*! () = Lwt_utils_unix.create_dir data_dir in
  let filename = global_lockfile_path ~data_dir in
  (* It's okay not to release the lock because once you have taken it,
     you don't want the data directory to be overwritten while the node
     is running. The lock will be released once the node stops. *)
  let* _fd =
    Lwt_lock_file.lock ~when_locked:(`Fail (Data_dir_locked data_dir)) ~filename
  in
  return_unit

module State = struct
  let with_store_transaction ctxt k =
    Evm_store.use ctxt.store @@ fun conn ->
    Evm_store.with_transaction conn @@ fun conn -> k conn

  let store_path ~data_dir = Filename.Infix.(data_dir // "store")

  let load ~data_dir ~store_perm:perm index =
    let open Lwt_result_syntax in
    let* store = Evm_store.init ~data_dir ~perm () in
    Evm_store.use store @@ fun conn ->
    let* latest = Evm_store.Context_hashes.find_latest conn in
    match latest with
    | Some (Qty latest_blueprint_number, checkpoint) ->
        let*! context = Irmin_context.checkout_exn index checkpoint in
        let*! evm_state = Irmin_context.PVMState.get context in
        let+ current_block_hash = Evm_state.current_block_hash evm_state in
        ( store,
          context,
          Ethereum_types.Qty Z.(succ latest_blueprint_number),
          current_block_hash,
          Loaded )
    | None ->
        let context = Irmin_context.empty index in
        return
          ( store,
            context,
            Ethereum_types.Qty Z.zero,
            Ethereum_types.genesis_parent_hash,
            Created )

  let commit store (context : Irmin_context.rw) evm_state number =
    let open Lwt_result_syntax in
    let*! context = Irmin_context.PVMState.set context evm_state in
    let*! checkpoint = Irmin_context.commit context in
    let* () = Evm_store.Context_hashes.store store number checkpoint in
    return context

  let maybe_split_context ctxt conn timestamp level =
    let open Lwt_result_syntax in
    match ctxt.history with
    | Archive -> return_none
    | Full {parameters} ->
        let split =
          match ctxt.session.last_split_block with
          | None -> true
          | Some (_level, last_split_timestamp) ->
              let timestamp = Time.Protocol.to_seconds timestamp in
              let last_split_timestamp =
                Time.Protocol.to_seconds last_split_timestamp
              in
              Compare.Int64.(
                timestamp
                >= Int64.(
                     add
                       last_split_timestamp
                       (of_int parameters.split_frequency_in_seconds)))
        in
        if split then (
          Irmin_context.split ctxt.index ;
          let* () = Evm_store.GC.update_last_split conn level timestamp in
          let*! () = Evm_context_events.gc_split level timestamp in
          return_some (level, timestamp))
        else return_none

  let maybe_gc ctxt conn timestamp level =
    let open Lwt_result_syntax in
    match ctxt.history with
    | Archive -> return_none
    | Full {parameters} -> (
        match ctxt.session.last_gc_block with
        | None ->
            (* The GC was never called, we do nothing and consider
               that the GC was done. *)
            let* () = Evm_store.GC.update_last_gc conn level timestamp in
            return_some (level, timestamp)
        | Some (last_gc_level, last_gc_timestamp) ->
            let gc =
              let timestamp = Time.Protocol.to_seconds timestamp in
              let last_gc_timestamp =
                Time.Protocol.to_seconds last_gc_timestamp
              in
              Compare.Int64.(
                timestamp
                >= Int64.(
                     add
                       last_gc_timestamp
                       (of_int parameters.history_to_keep_in_seconds)))
            in
            if gc then (
              let*! () =
                Evm_context_events.gc_started
                  ~gc_level:last_gc_level
                  ~head_level:level
              in
              let start_timestamp = Time.System.now () in
              let* hash = Evm_store.Context_hashes.find conn last_gc_level in
              let*? hash =
                Option.to_result
                  ~none:
                    [
                      error_of_fmt
                        "Context hash of GC candidate %a is missing from store"
                        Ethereum_types.pp_quantity
                        last_gc_level;
                    ]
                  hash
              in
              let* () = Evm_store.reset_before conn ~l2_level:last_gc_level in
              let*! () = Irmin_context.gc ctxt.index hash in
              let* () = Evm_store.GC.update_last_gc conn level timestamp in
              let gc_waiter () =
                let open Lwt_syntax in
                let* () = Irmin_context.wait_gc_completion ctxt.index in
                let stop_timestamp = Time.System.now () in
                Evm_context_events.gc_finished
                  ~gc_level:last_gc_level
                  ~head_level:level
                  (Ptime.diff stop_timestamp start_timestamp)
              in
              Lwt.dont_wait gc_waiter Evm_context_events.gc_waiter_failed ;
              return_some (level, timestamp))
            else return_none)

  let commit_next_head (ctxt : t) conn timestamp evm_state =
    let open Lwt_result_syntax in
    let* context =
      commit
        conn
        ctxt.session.context
        evm_state
        ctxt.session.next_blueprint_number
    in
    let* split_info =
      maybe_split_context ctxt conn timestamp ctxt.session.next_blueprint_number
    in
    let* gc_info =
      maybe_gc ctxt conn timestamp ctxt.session.next_blueprint_number
    in
    return (context, split_info, gc_info)

  let replace_current_commit (ctxt : t) conn evm_state =
    let (Qty next) = ctxt.session.next_blueprint_number in
    commit conn ctxt.session.context evm_state (Qty Z.(pred next))

  let on_modified_head ctxt evm_state context =
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context

  let on_new_delayed_transaction ~delayed_transaction evm_state =
    let open Lwt_result_syntax in
    let*! data_dir, config = execution_config in
    let* storage_version =
      let read path =
        let*! res = Evm_state.inspect evm_state path in
        return res
      in
      Durable_storage.storage_version read
    in
    if storage_version < 15 then
      Evm_state.execute
        ~data_dir
        ~config
        evm_state
        [
          `Input
            ("\254"
            ^ Bytes.to_string
                (Evm_events.Delayed_transaction.to_rlp delayed_transaction));
        ]
    else
      let*! evm_state =
        Evm_state.modify
          ~key:"/__delayed_input"
          ~value:
            (Bytes.to_string
               (Evm_events.Delayed_transaction.to_rlp delayed_transaction))
          evm_state
      in
      Evm_state.execute
        ~wasm_entrypoint:"populate_delayed_inbox"
        ~data_dir
        ~config
        evm_state
        []

  let background_preemptive_download ({preimages_endpoint; preimages; _} : t)
      Evm_events.Upgrade.{hash; timestamp = _} =
    match preimages_endpoint with
    | None -> ()
    | Some preimages_endpoint ->
        Lwt.async @@ fun () ->
        Misc.unwrap_error_monad @@ fun () ->
        let (Hash (Hex root_hash)) = hash in
        let root_hash = `Hex root_hash in
        Kernel_download.download ~preimages ~preimages_endpoint ~root_hash ()

  let reset_to_finalized_level ?expected_finalized_number exit_error ctxt conn =
    let open Lwt_result_syntax in
    let* safe_checkpoint = Evm_store.Context_hashes.find_finalized conn in
    match safe_checkpoint with
    | Some (finalized_number, checkpoint) ->
        let* () =
          match expected_finalized_number with
          | None -> return_unit
          | Some expected_finalized_number ->
              when_ (finalized_number <> expected_finalized_number) (fun () ->
                  let*! () =
                    Evm_context_events.reset_incoherent_finalized_state
                      ~expected_finalized_number
                      ~finalized_number
                  in
                  tzfail exit_error)
        in
        let*! () = Evm_context_events.reset_at_level finalized_number in
        (* Find the "finalized" evm_state. *)
        let*! context = Irmin_context.checkout_exn ctxt.index checkpoint in
        let*! evm_state = Irmin_context.PVMState.get context in
        (* Clear the store. *)
        let* () = Evm_store.reset_after conn ~l2_level:finalized_number in
        (* Update mutable session values. *)
        let next_blueprint_number =
          let (Qty finalized_number) = finalized_number in
          Ethereum_types.Qty (Z.succ finalized_number)
        in
        let* current_block_hash = Evm_state.current_block_hash evm_state in
        ctxt.session.next_blueprint_number <- next_blueprint_number ;
        ctxt.session.evm_state <- evm_state ;
        ctxt.session.current_block_hash <- current_block_hash ;
        ctxt.session.context <- context ;
        (* TODO: We need to fetch from the store (before it’s cleared) if there
           is a pending upgrade at level [finalized_level]. It should be something
           of the sort:

           SELECT * FROM kernel_upgrades
           WHERE injected_before <= {finalized_level}
              && (applied_before >= {finalized_level} || applied_before IS NULL) *)
        ctxt.session.pending_upgrade <- None ;
        let*! head_info in
        head_info := session_to_head_info ctxt.session ;
        return evm_state
    | None ->
        let*! () =
          Evm_context_events.reset_impossible_missing_finalized_state ()
        in
        tzfail exit_error

  let blueprint_applied_event ctxt conn evm_state on_success
      latest_finalized_level
      ({number = Qty number; hash = expected_block_hash} :
        Evm_events.Blueprint_applied.t) =
    let open Lwt_result_syntax in
    (* We use [max] to not rely on the order of the EVM events (because
       it is possible to see several blueprints applied during on L1
       level). *)
    let latest_finalized_level = Z.max latest_finalized_level number in
    let* block_hash_opt =
      if ctxt.block_storage_sqlite3 then
        Evm_store.Blocks.find_hash_of_number conn (Qty number)
      else
        let*! bytes =
          Evm_state.inspect
            evm_state
            (Durable_storage_path.Indexes.block_by_number (Nth number))
        in
        return (Option.map Ethereum_types.decode_block_hash bytes)
    in
    match block_hash_opt with
    | Some found_block_hash ->
        if found_block_hash = expected_block_hash then
          let*! () =
            Evm_events_follower_events.upstream_blueprint_applied
              (number, expected_block_hash)
          in
          return (evm_state, on_success, latest_finalized_level)
        else
          let*! () =
            Evm_events_follower_events.diverged
              (number, expected_block_hash, found_block_hash)
          in
          let exit_error =
            Node_error.Diverged
              (number, expected_block_hash, Some found_block_hash)
          in
          if ctxt.fail_on_missing_blueprint then
            (* Sequencer must fail in case of divergence. *)
            tzfail exit_error
          else
            (* Observers needs to reset at finalized level. *)
            let* evm_state = reset_to_finalized_level exit_error ctxt conn in
            return (evm_state, on_success, latest_finalized_level)
    | None when ctxt.fail_on_missing_blueprint ->
        let*! () =
          Evm_events_follower_events.missing_blueprint
            (number, expected_block_hash)
        in
        tzfail (Node_error.Diverged (number, expected_block_hash, None))
    | None ->
        let*! () = Evm_events_follower_events.rollup_node_ahead (Qty number) in
        let* () =
          Evm_store.Pending_confirmations.insert
            conn
            (Qty number)
            expected_block_hash
        in
        return (evm_state, on_success, latest_finalized_level)

  let current_blueprint_number ctxt =
    let (Qty next_blueprint_number) = ctxt.session.next_blueprint_number in
    Ethereum_types.Qty (Z.pred next_blueprint_number)

  let () =
    register_error_kind
      `Permanent
      ~id:"evm_node_dev_cannot_apply_blueprint"
      ~title:"Cannot apply a blueprint"
      ~description:
        "The EVM node could not apply a blueprint on top of its local EVM \
         state."
      ~pp:(fun ppf local_state_level ->
        Format.fprintf
          ppf
          "The EVM node could not apply a blueprint on top of its local EVM \
           state at level %a."
          Z.pp_print
          local_state_level)
      Data_encoding.(obj1 (req "current_state_level" n))
      (function
        | Cannot_apply_blueprint {local_state_level} -> Some local_state_level
        | _ -> None)
      (fun local_state_level -> Cannot_apply_blueprint {local_state_level})

  let check_pending_upgrade ctxt timestamp =
    match ctxt.session.pending_upgrade with
    | None -> None
    | Some upgrade ->
        if Time.Protocol.(upgrade.kernel_upgrade.timestamp <= timestamp) then
          Some upgrade
        else None

  let check_upgrade ctxt evm_state =
    let open Lwt_result_syntax in
    function
    | Some
        Evm_store.{kernel_upgrade = Evm_events.Upgrade.{hash = root_hash; _}; _}
      ->
        let*! bytes =
          Evm_state.inspect evm_state Durable_storage_path.kernel_root_hash
        in
        let new_hash_candidate =
          Option.map
            (fun bytes ->
              let (`Hex hex) = Hex.of_bytes bytes in
              Ethereum_types.hash_of_string hex)
            bytes
        in

        let*! () =
          match new_hash_candidate with
          | Some current_root_hash when root_hash = current_root_hash ->
              Events.applied_upgrade
                root_hash
                ctxt.session.next_blueprint_number
          | _ ->
              Events.failed_upgrade root_hash ctxt.session.next_blueprint_number
        in

        (* Even if the upgrade failed, we consider it has been applied. *)
        return_true
    | None -> return_false

  let store_block_unsafe conn evm_state block =
    let open Lwt_result_syntax in
    (* Store the block itself. *)
    let* () = Evm_store.Blocks.store conn block in
    (* Store all transactions from the block. *)
    let read path =
      let*! res = Evm_state.inspect evm_state path in
      return res
    in
    let* () =
      match block.transactions with
      | Ethereum_types.TxHash hashes ->
          List.iter_es
            (fun hash ->
              let* receipt =
                let* receipt_opt =
                  Durable_storage.transaction_receipt read hash
                in
                match receipt_opt with
                | Some receipt -> return receipt
                | None ->
                    failwith
                      "Receipt missing for %a"
                      Ethereum_types.pp_hash
                      hash
              in
              let* object_ =
                let* object_opt =
                  Durable_storage.transaction_object read hash
                in
                match object_opt with
                | Some object_ -> return object_
                | None ->
                    failwith "Object missing for %a" Ethereum_types.pp_hash hash
              in
              let info =
                Transaction_info.of_receipt_and_object receipt object_
              in
              Evm_store.Transactions.store conn info)
            hashes
      | TxFull _ ->
          (* Block must be read without the transactions objects. Even though
             we could be resilient and store the objects, it doesn't make sense
             at the moment in the code to deal this case. The node does not need
             to read the full block on block processing. If for some reasons
             the code reads the full block, simply store the transactions objects
             and fetch the receipts here. *)
          assert false
    in
    return_unit

  (** [apply_blueprint_store_unsafe ctxt payload delayed_transactions] applies
      the blueprint [payload] on the head of [ctxt], and commit the resulting
      state to Irmin and the node’s store.

      However, it does not modifies [ctxt] to make it aware of the new state.
      This is because [apply_blueprint_store_unsafe] is expected to be called
      within a SQL transaction to make sure the node’s store is not left in an
      inconsistent state in case of error. *)
  let apply_blueprint_store_unsafe ctxt conn timestamp payload
      delayed_transactions =
    let open Lwt_result_syntax in
    Evm_store.assert_in_transaction conn ;

    let*! data_dir, config = execution_config in
    let (Qty next) = ctxt.session.next_blueprint_number in
    let* () =
      List.iter_es
        (Evm_store.Delayed_transactions.store conn (Qty next))
        delayed_transactions
    in

    let time_processed = ref Ptime.Span.zero in

    let* try_apply =
      Misc.with_timing
        (fun time -> Lwt.return (time_processed := time))
        (fun () ->
          Evm_state.apply_blueprint
            ~wasm_pvm_fallback:(not @@ List.is_empty delayed_transactions)
            ~data_dir
            ~config
            ctxt.session.evm_state
            payload)
    in

    match try_apply with
    | Apply_success {evm_state; block} ->
        let* () =
          (* A dirty way to avoid doing this in sequencer mode, to refine. *)
          if ctxt.fail_on_missing_blueprint then return_unit
          else
            let* finalized_hash =
              Evm_store.Pending_confirmations.find_with_level conn block.number
            in
            match finalized_hash with
            | None -> return_unit
            | Some expected_block_hash ->
                if expected_block_hash = block.hash then
                  Evm_store.Pending_confirmations.delete_with_level
                    conn
                    block.number
                else
                  let Ethereum_types.(Qty number) = block.number in
                  let exit_error =
                    Node_error.Diverged
                      (number, expected_block_hash, Some block.hash)
                  in
                  let*! () =
                    Evm_events_follower_events.diverged
                      (number, expected_block_hash, block.hash)
                  in
                  let* _evm_state =
                    reset_to_finalized_level exit_error ctxt conn
                  in
                  tzfail
                    (Cannot_apply_blueprint {local_state_level = Z.pred next})
        in
        let number_of_transactions =
          match block.transactions with
          | TxHash l -> List.length l
          | TxFull l -> List.length l
        in
        Metrics.set_block
          ~time_processed:!time_processed
          ~transactions:number_of_transactions ;
        Option.iter
          (fun baseFeePerGas ->
            baseFeePerGas |> Ethereum_types.Qty.to_z |> Metrics.set_gas_price)
          block.baseFeePerGas ;
        let* () =
          Evm_store.Blueprints.store
            conn
            {number = block.number; timestamp; payload}
        in

        let* evm_state =
          if ctxt.block_storage_sqlite3 then
            let* () = store_block_unsafe conn evm_state block in
            let*! evm_state = Evm_state.clear_block_storage block evm_state in
            return evm_state
          else return evm_state
        in

        let upgrade_candidate = check_pending_upgrade ctxt timestamp in
        let* applied_kernel_upgrade =
          check_upgrade ctxt evm_state upgrade_candidate
        in
        let* () =
          when_ applied_kernel_upgrade @@ fun () ->
          Evm_store.Kernel_upgrades.record_apply
            conn
            ctxt.session.next_blueprint_number
        in

        let* context, split_info, gc_info =
          commit_next_head ctxt conn timestamp evm_state
        in
        return
          ( evm_state,
            context,
            block,
            applied_kernel_upgrade,
            split_info,
            gc_info )
    | Apply_failure (* Did not produce a block *) ->
        let*! () =
          if ctxt.fail_on_missing_blueprint then
            Blueprint_events.invalid_blueprint_produced next
          else Blueprint_events.invalid_blueprint_applied next
        in
        tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

  let on_new_head ?split_info ?gc_info ctxt ~applied_upgrade evm_state context
      block blueprint_with_events =
    let open Lwt_syntax in
    let (Qty level) = ctxt.session.next_blueprint_number in
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context ;
    ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
    ctxt.session.current_block_hash <- Ethereum_types.(block.hash) ;
    Option.iter
      (fun (split_level, split_timestamp) ->
        ctxt.session.last_split_block <- Some (split_level, split_timestamp))
      split_info ;
    Option.iter
      (fun gc_info -> ctxt.session.last_gc_block <- Some gc_info)
      gc_info ;
    Broadcast.notify @@ Broadcast.Blueprint blueprint_with_events ;
    if applied_upgrade then ctxt.session.pending_upgrade <- None ;
    let* head_info in
    head_info := session_to_head_info ctxt.session ;
    Metrics.set_level ~level ;
    return_unit

  type error +=
    | Invalid_rollup_node of {
        smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
        rollup_node_smart_rollup_address :
          Tezos_crypto.Hashed.Smart_rollup_address.t;
      }

  let prepare_local_flushed_blueprint cctxt smart_rollup_address sequencer_key
      evm_state parent_hash
      Evm_events.Flushed_blueprint.
        {hashes; timestamp; level = Qty flushed_level} =
    let open Lwt_result_syntax in
    let* delayed_transactions =
      List.map_es (Evm_state.get_delayed_inbox_item evm_state) hashes
    in
    let* blueprint_chunks =
      Sequencer_blueprint.prepare
        ~sequencer_key
        ~cctxt
        ~timestamp
        ~transactions:[]
        ~delayed_transactions:hashes
        ~parent_hash
        ~number:(Qty flushed_level)
    in
    let payload =
      Sequencer_blueprint.create_inbox_payload
        ~smart_rollup_address:
          (Tezos_crypto.Hashed.Smart_rollup_address.to_string
             smart_rollup_address)
        ~chunks:blueprint_chunks
    in
    return (payload, delayed_transactions)

  let rec apply_blueprint ?conn ?(events = []) ctxt timestamp payload
      delayed_transactions =
    let open Lwt_result_syntax in
    let* ( evm_state,
           context,
           current_block,
           applied_kernel_upgrade,
           split_info,
           gc_info ) =
      let kont conn =
        let* () = apply_evm_events conn ctxt events in
        apply_blueprint_store_unsafe
          ctxt
          conn
          timestamp
          payload
          delayed_transactions
      in
      match conn with
      | None -> with_store_transaction ctxt @@ fun conn -> kont conn
      | Some conn -> kont conn
    in
    let kernel_upgrade =
      match ctxt.session.pending_upgrade with
      | Some {injected_before; kernel_upgrade}
        when injected_before = ctxt.session.next_blueprint_number ->
          Some kernel_upgrade
      | _ -> None
    in

    let*! () =
      Misc.with_timing (Blueprint_events.blueprint_applied current_block)
      @@ fun () ->
      on_new_head
        ?split_info
        ?gc_info
        ctxt
        ~applied_upgrade:applied_kernel_upgrade
        evm_state
        context
        current_block
        {
          delayed_transactions;
          kernel_upgrade;
          blueprint =
            {number = ctxt.session.next_blueprint_number; timestamp; payload};
        }
    in
    return_unit

  and apply_evm_event_unsafe on_success ctxt conn evm_state event
      latest_finalized_level =
    let open Lwt_result_syntax in
    let*! () = Evm_events_follower_events.new_event event in
    match event with
    | Evm_events.Upgrade_event upgrade ->
        let on_success session =
          session.pending_upgrade <-
            Some
              {
                kernel_upgrade = upgrade;
                injected_before = ctxt.session.next_blueprint_number;
              } ;
          background_preemptive_download ctxt upgrade ;
          on_success session
        in
        let payload = Evm_events.Upgrade.to_bytes upgrade |> String.of_bytes in
        let*! evm_state =
          Evm_state.modify
            ~key:Durable_storage_path.kernel_upgrade
            ~value:payload
            evm_state
        in
        let* () =
          Evm_store.Kernel_upgrades.store
            conn
            ctxt.session.next_blueprint_number
            upgrade
        in
        let*! () = Events.pending_upgrade upgrade in
        return (evm_state, on_success, latest_finalized_level)
    | Sequencer_upgrade_event sequencer_upgrade ->
        let payload =
          Evm_events.Sequencer_upgrade.to_bytes sequencer_upgrade
          |> String.of_bytes
        in
        let*! evm_state =
          Evm_state.modify
            ~key:Durable_storage_path.sequencer_upgrade
            ~value:payload
            evm_state
        in
        return (evm_state, on_success, latest_finalized_level)
    | Blueprint_applied event ->
        blueprint_applied_event
          ctxt
          conn
          evm_state
          on_success
          latest_finalized_level
          event
    | New_delayed_transaction delayed_transaction ->
        let* evm_state =
          on_new_delayed_transaction ~delayed_transaction evm_state
        in
        return (evm_state, on_success, latest_finalized_level)
    | Flush_delayed_inbox flushed_blueprint ->
        let*! () =
          Evm_events_follower_events.flush_delayed_inbox
            ~timestamp:flushed_blueprint.timestamp
            flushed_blueprint.level
        in
        let* evm_state = flush_delayed_inbox ctxt conn flushed_blueprint in
        return (evm_state, on_success, latest_finalized_level)

  and apply_evm_events ?finalized_level conn (ctxt : t) events =
    let open Lwt_result_syntax in
    let* needs_process =
      match finalized_level with
      | None ->
          (* We are not taking events from a rollup block, we don't need to check
             if it's the last known l1 block successor. *)
          return_true
      | Some rollup_block_level -> (
          let* last_known_l1_block =
            let* level = Evm_store.L1_l2_levels_relationships.find conn in
            match level with
            | Some {l1_level; _} -> return_some l1_level
            | None -> return_none
          in
          match last_known_l1_block with
          | None -> return_true
          | Some last_known_l1_block ->
              let level_expected = Int32.succ last_known_l1_block in
              if Int32.equal rollup_block_level level_expected then return_true
              else if Compare.Int32.(rollup_block_level < level_expected) then
                let*! () =
                  Evm_context_events.unexpected_l1_block
                    ~expected_level:level_expected
                    ~provided_level:rollup_block_level
                in
                return false
              else
                tzfail
                  (Node_error.Out_of_sync
                     {level_received = rollup_block_level; level_expected}))
    in
    if needs_process then (
      let* context, evm_state, on_success =
        let* on_success, evm_state, context, Qty latest_finalized_number =
          match events with
          | [] ->
              (* Avoid an uncessary {!replace_current_commit} if the list is
                 empty. *)
              return
                ( ignore,
                  ctxt.session.evm_state,
                  ctxt.session.context,
                  ctxt.session.finalized_number )
          | events ->
              let* on_success, evm_state, latest_finalized_number =
                List.fold_left_es
                  (fun ( on_success,
                         evm_state,
                         Ethereum_types.Qty finalized_number )
                       event ->
                    let* evm_state, on_success, latest_finalized_level =
                      apply_evm_event_unsafe
                        on_success
                        ctxt
                        conn
                        evm_state
                        event
                        finalized_number
                    in
                    return
                      ( on_success,
                        evm_state,
                        Ethereum_types.Qty latest_finalized_level ))
                  (ignore, ctxt.session.evm_state, ctxt.session.finalized_number)
                  events
              in
              let* context = replace_current_commit ctxt conn evm_state in
              return (on_success, evm_state, context, latest_finalized_number)
        in
        (* Process the new `latest_finalized_number`. *)
        let on_success session =
          session.finalized_number <- Qty latest_finalized_number ;
          Metrics.set_confirmed_level ~level:latest_finalized_number ;
          on_success session
        in
        let* () =
          Option.iter_es
            (fun l1_level ->
              Evm_store.L1_l2_levels_relationships.store
                conn
                ~l1_level
                ~latest_l2_level:(current_blueprint_number ctxt)
                ~finalized_l2_level:(Qty latest_finalized_number))
            finalized_level
        in
        return (context, evm_state, on_success)
      in
      let*! () =
        Option.iter_s
          (fun l1_level ->
            Metrics.set_l1_level ~level:l1_level ;
            Evm_context_events.processed_l1_level l1_level)
          finalized_level
      in
      on_success ctxt.session ;
      on_modified_head ctxt evm_state context ;
      let*! head_info in
      head_info := session_to_head_info ctxt.session ;
      return_unit)
    else return_unit

  and flush_delayed_inbox ctxt conn flushed_blueprint =
    let open Lwt_result_syntax in
    let (Qty flushed_level) =
      flushed_blueprint.Evm_events.Flushed_blueprint.level
    in
    let before_flushed_level = Ethereum_types.Qty (Z.pred flushed_level) in
    (* The kernel has produced a block for level [flushed_level]. The first thing
       to do is go back to an EVM state before this flushed blueprint. *)
    let* evm_state =
      reset_to_finalized_level
        ~expected_finalized_number:before_flushed_level
        (Node_error.Cannot_handle_flushed_blueprint (Qty flushed_level))
        ctxt
        conn
    in
    let* parent_hash = Evm_state.current_block_hash evm_state in
    (* Prepare a blueprint payload signed by the sequencer to execute locally. *)
    let* payload, delayed_transactions =
      match ctxt.sequencer_wallet with
      | None ->
          failwith "Only sequencer is capable to handle the flushed blueprint"
      | Some (sequencer_key, cctxt) ->
          prepare_local_flushed_blueprint
            cctxt
            ctxt.smart_rollup_address
            sequencer_key
            evm_state
            parent_hash
            flushed_blueprint
    in
    (* Apply the blueprint. *)
    let timestamp = flushed_blueprint.Evm_events.Flushed_blueprint.timestamp in
    let* () =
      apply_blueprint ~conn ctxt timestamp payload delayed_transactions
    in
    return ctxt.session.evm_state

  let () =
    register_error_kind
      `Permanent
      ~id:"evm_node_dev_invalid_rollup_node"
      ~title:"Invalid rollup node"
      ~description:"The rollup node has a different smart rollup address."
      ~pp:(fun ppf (smart_rollup_address, rollup_node_smart_rollup_address) ->
        Format.fprintf
          ppf
          "The EVM node follows the smart rollup address %a but the rollup \
           node has the address %a, please change the rollup node endpoint."
          Tezos_crypto.Hashed.Smart_rollup_address.pp
          smart_rollup_address
          Tezos_crypto.Hashed.Smart_rollup_address.pp
          rollup_node_smart_rollup_address)
      Data_encoding.(
        obj2
          (req
             "smart_rollup_address"
             Tezos_crypto.Hashed.Smart_rollup_address.encoding)
          (req
             "rollup_node_smart_rollup_address"
             Tezos_crypto.Hashed.Smart_rollup_address.encoding))
      (function
        | Invalid_rollup_node
            {smart_rollup_address; rollup_node_smart_rollup_address} ->
            Some (smart_rollup_address, rollup_node_smart_rollup_address)
        | _ -> None)
      (fun (smart_rollup_address, rollup_node_smart_rollup_address) ->
        Invalid_rollup_node
          {smart_rollup_address; rollup_node_smart_rollup_address})

  let preload_kernel_from_level ctxt level =
    let open Lwt_result_syntax in
    let* hash_candidate =
      Evm_store.use ctxt.store @@ fun conn ->
      Evm_store.Context_hashes.find conn level
    in
    match hash_candidate with
    | None -> return_unit
    | Some hash ->
        let*! context = Irmin_context.checkout_exn ctxt.index hash in
        let*! evm_state = Irmin_context.PVMState.get context in
        let*! () = Evm_state.preload_kernel evm_state in
        return_unit

  let preload_known_kernels ctxt =
    let open Lwt_result_syntax in
    let* activation_levels =
      Evm_store.use ctxt.store Evm_store.Kernel_upgrades.activation_levels
    in
    let* earliest_info =
      Evm_store.use ctxt.store Evm_store.Context_hashes.find_earliest
    in
    let earliest_level =
      Option.fold ~none:[] ~some:(fun (l, _) -> [l]) earliest_info
    in
    List.iter_ep
      (preload_kernel_from_level ctxt)
      (earliest_level @ activation_levels)

  let init ?kernel_path ~block_storage_sqlite3 ?garbage_collector
      ~fail_on_missing_blueprint ~data_dir ~preimages ~preimages_endpoint
      ?smart_rollup_address ~store_perm ?sequencer_wallet () =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.create_dir (Evm_state.kernel_logs_directory ~data_dir)
    in
    let*! () = Lwt_utils_unix.create_dir preimages in
    let* index =
      Irmin_context.load
        ~cache_size:100_000
        Read_write
        (Evm_state.irmin_store_path ~data_dir)
    in
    let* store, context, next_blueprint_number, current_block_hash, init_status
        =
      load ~data_dir ~store_perm index
    in
    Evm_store.use store @@ fun conn ->
    let* () =
      let* is_empty = Evm_store.Pending_confirmations.is_empty conn in
      when_ (fail_on_missing_blueprint && not is_empty) (fun () ->
          failwith "Store has pending confirmation, state is not final")
    in
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    let* latest_relationship = Evm_store.L1_l2_levels_relationships.find conn in
    let finalized_number, l1_level =
      match latest_relationship with
      | None -> (Ethereum_types.Qty Z.zero, None)
      | Some {finalized; l1_level; _} -> (finalized, Some l1_level)
    in
    let smart_rollup_address =
      Option.map
        Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn
        smart_rollup_address
    in
    let* smart_rollup_address =
      let* found_smart_rollup_address = Evm_store.Metadata.find conn in
      match (found_smart_rollup_address, smart_rollup_address) with
      | Some found_smart_rollup_address, Some smart_rollup_address ->
          let* () =
            fail_unless
              (Tezos_crypto.Hashed.Smart_rollup_address.equal
                 smart_rollup_address
                 found_smart_rollup_address)
              (Invalid_rollup_node
                 {
                   smart_rollup_address = found_smart_rollup_address;
                   rollup_node_smart_rollup_address = smart_rollup_address;
                 })
          in
          return smart_rollup_address
      | None, Some smart_rollup_address ->
          let* () = Evm_store.Metadata.store conn smart_rollup_address in
          return smart_rollup_address
      | Some found_smart_rollup_address, None ->
          return found_smart_rollup_address
      | None, None ->
          (* Ok. This is dirty. In normal condition it does not make sense that
             the sr1 is missing from the local storage and not provided by
             the caller.

             However, it can happen if the node is ran in sandbox mode
             on a non existing context. Returning the zero address here is
             so much convenient for a case that's really not supposed to happen,
             ever. An error that would be detected relatively fast as well.
          *)
          return Tezos_crypto.Hashed.Smart_rollup_address.zero
    in
    let* evm_state, context =
      match kernel_path with
      | Some kernel ->
          if init_status = Loaded then
            let*! () = Events.ignored_kernel_arg () in
            let*! evm_state = Irmin_context.PVMState.get context in
            return (evm_state, context)
          else
            let* evm_state = Evm_state.init ~kernel in
            let (Qty next) = next_blueprint_number in
            let* context = commit conn context evm_state (Qty Z.(pred next)) in
            return (evm_state, context)
      | None ->
          if init_status = Loaded then
            let*! evm_state = Irmin_context.PVMState.get context in
            return (evm_state, context)
          else
            failwith
              "Cannot compute the initial EVM state without the path to the \
               initial kernel"
    in

    let* history, last_split_block, last_gc_block =
      match garbage_collector with
      | Some parameters ->
          let* last_split_opt = Evm_store.GC.last_split conn in
          let* last_gc_opt = Evm_store.GC.last_gc conn in
          return (Full {parameters}, last_split_opt, last_gc_opt)
      | None -> return (Archive, None, None)
    in

    let ctxt =
      {
        index;
        data_dir;
        preimages;
        preimages_endpoint;
        smart_rollup_address;
        session =
          {
            context;
            finalized_number;
            next_blueprint_number;
            current_block_hash;
            pending_upgrade;
            evm_state;
            last_split_block;
            last_gc_block;
          };
        store;
        fail_on_missing_blueprint;
        block_storage_sqlite3;
        history;
        sequencer_wallet;
      }
    in

    let*! () =
      Option.iter_s
        (fun upgrade -> Events.pending_upgrade upgrade.Evm_store.kernel_upgrade)
        pending_upgrade
    in

    return (ctxt, (init_status, smart_rollup_address, l1_level))

  let reset ~data_dir ~l2_level =
    let open Lwt_result_syntax in
    let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
    Evm_store.use store @@ fun conn ->
    Evm_store.with_transaction conn @@ fun store ->
    Evm_store.reset_after store ~l2_level

  let delayed_inbox_hashes evm_state =
    let open Lwt_syntax in
    let* keys =
      Evm_state.subkeys
        evm_state
        Durable_storage_path.Delayed_transaction.hashes
    in
    let hashes =
      (* Remove the empty, meta keys *)
      List.filter_map
        (fun key ->
          if key = "" || key = "meta" then None
          else Some (Ethereum_types.hash_of_string key))
        keys
    in
    return hashes

  let messages_of_level store level =
    let open Lwt_result_syntax in
    let open Octez_smart_rollup_node_store in
    let* hash = Store.L2_levels.find store level in
    let*? hash =
      Option.to_result
        ~none:[error_of_fmt "Block hash is not found for level %ld" level]
        hash
    in
    let* block = Store.L2_blocks.find store hash in
    let*? block =
      Option.to_result
        ~none:[error_of_fmt "Block is not found for hash %a" Block_hash.pp hash]
        block
    in
    let* messages = Store.Messages.find store block.header.inbox_witness in
    let*? messages =
      Option.to_result
        ~none:[error_of_fmt "No messages found for block %a" Block_hash.pp hash]
        messages
    in
    return messages

  (** [reconstruct_commit_blocks ~current_block_number ctxt execute
      evm_state] loops on [execute evm_state] until no new block is
      produced.  We cannot (easily) know how many blocks an execution
      is supposed to make, so we execute until no block is not
      produced. If no new block is produced by this function, no
      commits are produced. *)
  let rec reconstruct_commit_blocks ~current_block_number ctxt execute evm_state
      =
    let open Lwt_result_syntax in
    let* evm_state = execute evm_state in
    let*! (Qty number) = Evm_state.current_block_height evm_state in
    if number > current_block_number then (
      (* The kernel produced a new block, we commit the intermediate state. *)
      let* context =
        Evm_store.use ctxt.store @@ fun conn ->
        commit conn ctxt.session.context evm_state (Qty number)
      in
      ctxt.session.context <- context ;
      reconstruct_commit_blocks
        ~current_block_number:number
        ctxt
        execute
        evm_state)
    else
      (* The kernel did not produce a new block. It either means the block
         takes more than 1000 reboots to be processed, or no blueprints are
         found in the storage. *)
      return (current_block_number, evm_state)

  (** If we are reconstructing mainnet, the initial kernel is not
      sufficient.  We need to override the kernel with an additional
      patch that allows the compatibility with the reconstruct.

      We determine if the replacement is needed based on the smart rollup
      address. *)
  let replace_mainnet_kernel_if_needed ctxt evm_state =
    let open Lwt_result_syntax in
    if
      ctxt.smart_rollup_address
      = Address.of_b58check_exn "sr1Ghq66tYK9y3r8CC1Tf8i8m5nxh8nTvZEf"
    then
      let*! () = Evm_context_events.reconstruct_replace_mainnet_kernel () in
      let*! evm_state =
        Evm_state.modify
          ~key:"/kernel/boot.wasm"
          ~value:Evm_node_kernels.mainnet_initial_kernel_reconstruct_compatible
          evm_state
      in
      return evm_state
    else return evm_state

  let reconstruct_history ctxt ~rollup_node_store ~genesis_level
      ~finalized_level =
    let open Lwt_result_syntax in
    (* Smart Rollups do not process messages of genesis level. *)
    let first_level = Int32.succ genesis_level in
    assert (finalized_level > first_level) ;
    let config = pvm_config ctxt in
    let rec go ~count_progress ~current_block_number evm_state tezos_level =
      if tezos_level > finalized_level then return_unit
      else
        let*! () = count_progress 1 in
        let* messages = messages_of_level rollup_node_store tezos_level in
        (* For now we use the mocked sol, ipl and eol of the debugger. *)
        let messages =
          match messages with
          | _sol :: _ipl :: rst ->
              List.rev (Option.value ~default:[] @@ List.tl (List.rev rst))
          | _ -> assert false
        in
        (* Execute the PVM to replay the tezos level. *)
        let* evm_state =
          Evm_state.execute
            ~data_dir:ctxt.data_dir
            ~log_file:"reconstruct"
            ~config
            evm_state
            (List.map (fun m -> `Input m) messages)
        in
        let*! (Qty number) = Evm_state.current_block_height evm_state in
        let* current_block_number, evm_state =
          if number > current_block_number then (
            (* The execution with messages produced a block. We commit to this block,
               and loop in case of other blocks. *)
            let* context =
              Evm_store.use ctxt.store @@ fun conn ->
              commit conn ctxt.session.context evm_state (Qty number)
            in
            ctxt.session.context <- context ;
            reconstruct_commit_blocks
              ~current_block_number:number
              ctxt
              (fun evm_state ->
                Evm_state.execute
                  ~data_dir:ctxt.data_dir
                  ~log_file:"reconstruct"
                  ~config
                  evm_state
                  [])
              evm_state)
          else
            (* Otherwise just move to the next tezos level. *)
            return (current_block_number, evm_state)
        in
        go
          ~count_progress
          ~current_block_number
          evm_state
          (Int32.succ tezos_level)
    in
    (* We perform a first reboot to reveal the kernel. *)
    let* evm_state =
      Evm_state.execute
        ~data_dir:ctxt.data_dir
        ~log_file:"reconstruct"
        ~config
        ctxt.session.evm_state
        []
    in
    let* evm_state = replace_mainnet_kernel_if_needed ctxt evm_state in
    let*! evm_state =
      Evm_state.modify ~key:"/__at_most_one_block" ~value:"" evm_state
    in
    let total =
      Int32.sub finalized_level first_level |> Int32.to_int |> Int.succ
    in
    let progress_bar =
      Progress_bar.progress_bar ~counter:`Int ~message:"Reconstructing" total
    in
    Progress_bar.Lwt.with_reporter progress_bar @@ fun count_progress ->
    go ~count_progress ~current_block_number:Z.(pred zero) evm_state first_level

  let wasm_pvm_version (ctxt : t) =
    let open Lwt_result_syntax in
    let*! version = Evm_state.wasm_pvm_version ctxt.session.evm_state in
    return version

  let canonical_block_number ctxt = function
    | None -> None
    | Some block_number ->
        if Ethereum_types.Qty.(current_blueprint_number ctxt = block_number)
        then None
        else Some block_number

  let perform_commit = commit

  let patch_state (ctxt : t) ?block_number ~commit ~key ~value () =
    let open Lwt_result_syntax in
    let block_number = canonical_block_number ctxt block_number in
    let* evm_state =
      match block_number with
      | None -> return ctxt.session.evm_state
      | Some block_number -> (
          Evm_store.use ctxt.store @@ fun conn ->
          let* hash = Evm_store.Context_hashes.find conn block_number in
          match hash with
          | Some hash ->
              let*! context = Irmin_context.checkout_exn ctxt.index hash in
              let*! evm_state = Irmin_context.PVMState.get context in
              return evm_state
          | None ->
              failwith
                "Missing context for block number %a"
                Ethereum_types.pp_quantity
                block_number)
    in
    let*! evm_state = Evm_state.modify ~key ~value evm_state in
    let* (Qty number) =
      match block_number with
      | None ->
          if commit then (
            Evm_store.use ctxt.store @@ fun conn ->
            let* commit = replace_current_commit ctxt conn evm_state in
            on_modified_head ctxt evm_state commit ;
            return (current_blueprint_number ctxt))
          else (
            ctxt.session.evm_state <- evm_state ;
            return (current_blueprint_number ctxt))
      | Some block_number ->
          if commit then (
            Evm_store.use ctxt.store @@ fun conn ->
            let* context =
              perform_commit conn ctxt.session.context evm_state block_number
            in
            ctxt.session.context <- context ;
            return block_number)
          else return block_number
    in
    let*! () = Events.patched_state key (Qty Z.(succ number)) in
    return_unit
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Request

  type self = worker

  type launch_error = tztrace

  let on_launch _self ()
      {
        kernel_path : string option;
        data_dir : string;
        preimages : string;
        preimages_endpoint : Uri.t option;
        smart_rollup_address : string option;
        fail_on_missing_blueprint;
        store_perm;
        block_storage_sqlite3;
        garbage_collector;
        sequencer_wallet;
      } =
    let open Lwt_result_syntax in
    let* ctxt, status =
      State.init
        ?kernel_path
        ~data_dir
        ~preimages
        ~preimages_endpoint
        ?smart_rollup_address
        ~fail_on_missing_blueprint
        ~store_perm
        ~block_storage_sqlite3
        ?garbage_collector
        ?sequencer_wallet
        ()
    in
    Lwt.wakeup execution_config_waker @@ (ctxt.data_dir, pvm_config ctxt) ;
    Lwt.wakeup init_status_waker status ;
    let first_head = ref (session_to_head_info ctxt.session) in
    Lwt.wakeup head_info_waker first_head ;
    let* () = State.preload_known_kernels ctxt in
    return ctxt

  let on_request :
      type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_result_syntax in
    match request with
    | Apply_evm_events {finalized_level; events} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.with_store_transaction ctxt @@ fun conn ->
        State.apply_evm_events ?finalized_level conn ctxt events
    | Apply_blueprint {events; timestamp; payload; delayed_transactions} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.apply_blueprint
          ?events
          ctxt
          timestamp
          payload
          delayed_transactions
    | Blueprint {level} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        Evm_store.Blueprints.find_with_events conn level
    | Blueprints_range {from; to_} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        Evm_store.Blueprints.find_range conn ~from ~to_
    | Last_known_L1_level -> (
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        let+ level = Evm_store.L1_l2_levels_relationships.find conn in
        match level with Some {l1_level; _} -> Some l1_level | None -> None)
    | Delayed_inbox_hashes ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        let*! hashes = State.delayed_inbox_hashes ctxt.session.evm_state in
        return hashes
    | Reconstruct {rollup_node_store; genesis_level; finalized_level} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.reconstruct_history
          ctxt
          ~rollup_node_store
          ~genesis_level
          ~finalized_level
    | Patch_state {commit; key; value; block_number} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.patch_state ?block_number ctxt ~commit ~key ~value ()
    | Wasm_pvm_version ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.wasm_pvm_version ctxt

  let on_completion (type a err) _self (_r : (a, err) Request.t) (_res : a) _st
      =
    Lwt_syntax.return_unit

  let on_no_request _self = Lwt.return_unit

  let on_close _self = Lwt.return_unit

  module Eq = struct
    type (_, _) eq = Eq : ('a, 'a) eq

    let request : type a b. (a, b) Request.t -> (b, tztrace) eq = function
      | Apply_evm_events _ -> Eq
      | Apply_blueprint _ -> Eq
      | Blueprint _ -> Eq
      | Blueprints_range _ -> Eq
      | Last_known_L1_level -> Eq
      | Delayed_inbox_hashes -> Eq
      | Reconstruct _ -> Eq
      | Patch_state _ -> Eq
      | Wasm_pvm_version -> Eq
  end

  let on_error (type a b) _self _st (req : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with
    | Apply_evm_events _, [Node_error.Diverged _divergence]
    | Apply_blueprint _, [Node_error.Diverged _divergence] ->
        Lwt_exit.exit_and_raise Node_error.exit_code_when_diverge
    | ( Apply_evm_events _,
        [Node_error.Out_of_sync {level_expected; level_received}] ) ->
        let*! () =
          Evm_events_follower_events.out_of_sync
            ~expected:level_expected
            ~received:level_received
        in
        Lwt_exit.exit_and_raise Node_error.exit_code_when_out_of_sync
    | _ ->
        let Eq = Eq.request req in
        let request_view = Request.view req in
        let*! () = Evm_context_events.worker_request_failed request_view errs in
        return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_worker

let check_status_and_return worker =
  match Worker.status worker with
  | Closed (_, _, errs) -> (
      match errs with
      | Some errs -> Error errs
      | None -> error_with "Worker failed with no error")
  | _ -> Ok worker

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> check_status_and_return worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_worker))

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let return_ : (_, _ Worker.message_error) result -> _ =
  let open Lwt_result_syntax in
  function
  | Ok res -> return res
  | Error (Closed (Some trace)) -> Lwt.return (Error trace)
  | Error (Closed None) ->
      failwith
        "Cannot interact with the EVM context worker because it is closed"
  | Error (Request_error err) -> Lwt.return (Error err)
  | Error (Any exn) -> fail_with_exn exn

let worker_wait_for_request req =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! res = Worker.Queue.push_request_and_wait w req in
  return_ res

let export_store ~data_dir ~output_db_file =
  let open Lwt_result_syntax in
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  Evm_store.use store @@ fun conn ->
  let* rollup_address = Evm_store.Metadata.get conn in
  let* current_number, _ = Evm_store.Context_hashes.get_latest conn in
  let* () = Evm_store.vacuum ~conn ~output_db_file in
  return {rollup_address; current_number}

let start ?kernel_path ~data_dir ~preimages ~preimages_endpoint
    ?smart_rollup_address ~fail_on_missing_blueprint ~store_perm
    ~block_storage_sqlite3 ?garbage_collector ?sequencer_wallet () =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir in
  let* worker =
    Worker.launch
      table
      ()
      {
        kernel_path;
        data_dir;
        preimages;
        preimages_endpoint;
        smart_rollup_address;
        fail_on_missing_blueprint;
        store_perm;
        block_storage_sqlite3;
        garbage_collector;
        sequencer_wallet;
      }
      (module Handlers)
  in
  let*! () = Blueprint_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  let*! init_status, sr1, l1_level = init_status in
  let*! head_info in
  let (Qty finalized_number) = !head_info.finalized_number in
  let (Qty next_blueprint_number) = !head_info.next_blueprint_number in
  Metrics.set_level ~level:(Z.pred next_blueprint_number) ;
  Metrics.set_confirmed_level ~level:finalized_number ;
  Option.iter (fun l1_level -> Metrics.set_l1_level ~level:l1_level) l1_level ;
  let*! () = Evm_context_events.ready () in
  return (init_status, sr1)

let rollup_node_metadata ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let* metadata =
    Metadata.Versioned.read_metadata_file ~dir:rollup_node_data_dir
  in
  match metadata with
  | None -> failwith "missing metadata in the rollup node data dir"
  | Some (V0 _) -> failwith "metadata has version 0, we need at least version 1"
  | Some (V1 {rollup_address; genesis_info = {level; _}; _}) ->
      return (Address.to_string rollup_address, level)

let init_context_from_rollup_node ~data_dir ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let open Octez_smart_rollup_node_store in
  let* rollup_node_store =
    Store.init Read_only ~data_dir:rollup_node_data_dir
  in
  let rollup_node_store = Store.Normal rollup_node_store in
  let* final_l2_block = Store.L2_blocks.find_finalized rollup_node_store in
  let* final_l2_block =
    match final_l2_block with
    | Some b -> return b
    | None -> failwith "Rollup node has no finalized l2 block"
  in
  let checkpoint =
    Irmin_context.hash_of_context_hash final_l2_block.header.context
  in
  let rollup_node_context_dir =
    Filename.Infix.(rollup_node_data_dir // "context")
  in
  let evm_context_dir = Evm_state.irmin_store_path ~data_dir in
  let* () =
    Format.eprintf "Acquiring rollup node locks@." ;
    Lwt_lock_file.with_lock
      ~when_locked:`Block
      ~filename:(Filename.concat rollup_node_data_dir "gc_lock")
    @@ fun () ->
    Lwt_lock_file.with_lock
      ~when_locked:`Block
      ~filename:(Filename.concat rollup_node_data_dir "processing_lock")
    @@ fun () ->
    let message =
      Format.sprintf
        "Exporting context for %ld in %s"
        final_l2_block.header.level
        evm_context_dir
    in
    Tezos_stdlib_unix.Utils.copy_dir
      ~progress:(message, Terminal.Color.(rgb 255 153 51))
      rollup_node_context_dir
      evm_context_dir ;
    return_unit
  in
  let* evm_node_index =
    Irmin_context.load ~cache_size:100_000 Read_write evm_context_dir
  in
  let*! evm_node_context =
    Irmin_context.checkout_exn evm_node_index checkpoint
  in
  let*! evm_state = Irmin_context.PVMState.get evm_node_context in
  return (evm_node_context, evm_state, final_l2_block.header.level)

let init_store_from_rollup_node ~data_dir ~evm_state ~irmin_context =
  let open Lwt_result_syntax in
  (* Tell the kernel that it is executed by an EVM node *)
  let*! evm_state = Evm_state.flag_local_exec evm_state in
  (* We remove the delayed inbox from the EVM state. Its contents will be
     retrieved by the sequencer by inspecting the evm events. *)
  let*! evm_state = Evm_state.clear_delayed_inbox evm_state in

  (* For changes made to [evm_state] to take effect, we commit the result *)
  let*! evm_node_context = Irmin_context.PVMState.set irmin_context evm_state in
  let*! checkpoint = Irmin_context.commit evm_node_context in

  (* Assert we can read the current blueprint number *)
  let* current_blueprint_number =
    let*! current_blueprint_number_opt =
      Evm_state.inspect evm_state Durable_storage_path.Block.current_number
    in
    match current_blueprint_number_opt with
    | Some bytes -> return (Bytes.to_string bytes |> Z.of_bits)
    | None -> failwith "The blueprint number was not found"
  in

  (* Assert we can read the current block hash *)
  let* () =
    let*! current_block_hash_opt =
      Evm_state.inspect evm_state Durable_storage_path.Block.current_hash
    in
    match current_block_hash_opt with
    | Some _bytes -> return_unit
    | None -> failwith "The block hash was not found"
  in
  (* Init the store *)
  let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
  Evm_store.use store @@ fun conn ->
  let* () =
    Evm_store.Context_hashes.store
      conn
      (Qty current_blueprint_number)
      checkpoint
  in
  return_unit

let reset = State.reset

let get_evm_events_from_rollup_node_state ~omit_delayed_tx_events evm_state =
  let open Lwt_result_syntax in
  let* kernel_upgrade =
    let*! kernel_upgrade_payload =
      Evm_state.inspect evm_state Durable_storage_path.kernel_upgrade
    in
    Option.bind kernel_upgrade_payload Evm_events.Upgrade.of_bytes
    |> Option.map (fun e -> Evm_events.Upgrade_event e)
    |> return
  in

  let* sequencer_upgrade =
    let*! sequencer_upgrade_payload =
      Evm_state.inspect evm_state Durable_storage_path.sequencer_upgrade
    in
    Option.bind sequencer_upgrade_payload Evm_events.Sequencer_upgrade.of_bytes
    |> Option.map (fun e -> Evm_events.Sequencer_upgrade_event e)
    |> return
  in

  let* new_delayed_transactions =
    if omit_delayed_tx_events then return []
    else
      let*! hashes = State.delayed_inbox_hashes evm_state in
      let* events =
        List.map_es
          (fun hash ->
            let* item = Evm_state.get_delayed_inbox_item evm_state hash in
            return (Evm_events.New_delayed_transaction item))
          hashes
      in
      return events
  in

  return
  @@ Option.to_list kernel_upgrade
  @ Option.to_list sequencer_upgrade
  @ new_delayed_transactions

let apply_evm_events ?finalized_level events =
  worker_add_request ~request:(Apply_evm_events {finalized_level; events})

let reconstruct ~data_dir ~rollup_node_data_dir ~boot_sector =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir in
  let open Octez_smart_rollup_node_store in
  let* rollup_node_store =
    Store.init Read_only ~data_dir:rollup_node_data_dir
  in
  let rollup_node_store = Store.Normal rollup_node_store in
  let* finalized = Store.State.Finalized_level.get rollup_node_store in
  let* finalized_level =
    match finalized with
    | Some (_block, l) -> return l
    | None -> failwith "Rollup node has no finalized l2 block"
  in
  let* smart_rollup_address, genesis_level =
    rollup_node_metadata ~rollup_node_data_dir
  in
  let* _loaded =
    start
      ~kernel_path:boot_sector
      ~data_dir
      ~preimages:Filename.Infix.(rollup_node_data_dir // "wasm_2_0_0")
      ~preimages_endpoint:None
      ~smart_rollup_address
      ~fail_on_missing_blueprint:false
      ~store_perm:`Read_write
      ~block_storage_sqlite3:false
      ()
  in
  worker_wait_for_request
    (Reconstruct {rollup_node_store; genesis_level; finalized_level})

let init_from_rollup_node ~omit_delayed_tx_events ~data_dir
    ~rollup_node_data_dir () =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir in
  let* irmin_context, evm_state, finalized_level =
    init_context_from_rollup_node ~data_dir ~rollup_node_data_dir
  in
  let* evm_events =
    get_evm_events_from_rollup_node_state ~omit_delayed_tx_events evm_state
  in
  let* () = init_store_from_rollup_node ~data_dir ~evm_state ~irmin_context in
  let* smart_rollup_address, _genesis_level =
    rollup_node_metadata ~rollup_node_data_dir
  in
  let* _loaded =
    start
      ~data_dir
      ~preimages:Filename.Infix.(rollup_node_data_dir // "wasm_2_0_0")
      ~preimages_endpoint:None
      ~smart_rollup_address
      ~fail_on_missing_blueprint:false
      ~store_perm:`Read_write
      ~block_storage_sqlite3:false
      ()
  in
  worker_wait_for_request
    (Apply_evm_events
       {finalized_level = Some finalized_level; events = evm_events})

let apply_blueprint ?events timestamp payload delayed_transactions =
  worker_wait_for_request
    (Apply_blueprint {events; timestamp; payload; delayed_transactions})

let head_info () =
  let open Lwt_syntax in
  let+ head_info in
  !head_info

let next_blueprint_number () =
  let open Lwt_syntax in
  let+ head_info = head_info () in
  head_info.next_blueprint_number

let blueprint level = worker_wait_for_request (Blueprint {level})

let blueprints_range from to_ =
  worker_wait_for_request (Blueprints_range {from; to_})

let last_known_l1_level () = worker_wait_for_request Last_known_L1_level

let delayed_inbox_hashes () = worker_wait_for_request Delayed_inbox_hashes

let patch_kernel ?block_number path =
  let open Lwt_result_syntax in
  let* kernel, is_binary = Wasm_debugger.read_kernel_from_file path in
  let* version = worker_wait_for_request Wasm_pvm_version in
  let* () =
    Wasm_debugger.check_kernel
      ~binary:is_binary
      ~name:"boot.wasm"
      version
      kernel
  in
  let* () =
    worker_wait_for_request
      (Patch_state
         {
           commit = true;
           key = "/kernel/boot.wasm";
           value = kernel;
           block_number;
         })
  in
  return_unit

let patch_sequencer_key ?block_number pk =
  worker_wait_for_request
    (Patch_state
       {
         commit = false;
         key = Durable_storage_path.sequencer_key;
         value = Signature.Public_key.to_b58check pk;
         block_number;
       })

let patch_state ?block_number ~key ~value () =
  worker_wait_for_request
    (Patch_state {commit = true; key; value; block_number})

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_context_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
