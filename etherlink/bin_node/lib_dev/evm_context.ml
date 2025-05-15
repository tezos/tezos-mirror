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
  configuration : Configuration.t;
  kernel_path : Wasm_debugger.kernel option;
  data_dir : string;
  smart_rollup_address : string option;
  store_perm : [`Read_only | `Read_write];
  sequencer_wallet : (Client_keys.sk_uri * Client_context.wallet) option;
  snapshot_url : string option;
  tx_container : (module Services_backend_sig.Tx_container);
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
}

type t = {
  configuration : Configuration.t;
  data_dir : string;
  index : Irmin_context.rw_index;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  store : Evm_store.t;
  session : session_state;
  sequencer_wallet : (Client_keys.sk_uri * Client_context.wallet) option;
  legacy_block_storage : bool;
  tx_container : (module Services_backend_sig.Tx_container);
}

let is_sequencer t = Option.is_some t.sequencer_wallet

let pvm_config ctxt =
  Wasm_debugger.config
    ~preimage_directory:ctxt.configuration.kernel_execution.preimages
    ?preimage_endpoint:ctxt.configuration.kernel_execution.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

type error +=
  | Incorrect_history_mode of {
      store_history_mode : Configuration.history_mode;
      history_mode : Configuration.history_mode;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_cannot_apply_blueprint"
    ~title:"Cannot apply a blueprint"
    ~description:
      "The EVM node could not apply a blueprint on top of its local EVM state."
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

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_incorrect_history_mode"
    ~title:"Incorrect history mode"
    ~description:"The EVM has stored data with a different history mode."
    ~pp:(fun ppf (store_history_mode, history_mode) ->
      Format.fprintf
        ppf
        "The EVM node has stored data with history mode %a, it cannot be run \
         with history mode %a without risk. Please use the command switch \
         history mode to change history mode for the node."
        Configuration.pp_history_mode_debug
        store_history_mode
        Configuration.pp_history_mode_debug
        history_mode)
    Data_encoding.(
      obj2
        (req "store_history_mode_level" Configuration.history_mode_encoding)
        (req "history_mode_level" Configuration.history_mode_encoding))
    (function
      | Incorrect_history_mode {store_history_mode; history_mode} ->
          Some (store_history_mode, history_mode)
      | _ -> None)
    (fun (store_history_mode, history_mode) ->
      Incorrect_history_mode {store_history_mode; history_mode})

module Types = struct
  type state = t

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "evm_context"]

  let pp _fmt () = ()

  let equal () () = true
end

let head_info, head_info_waker = Lwt.task ()

let init_status, init_status_waker = Lwt.task ()

let execution_config, execution_config_waker = Lwt.task ()

let lock_data_dir ~data_dir = Data_dir.lock ~data_dir

let head_watcher :
    Transaction_object.t Ethereum_types.Subscription.output Lwt_watcher.input =
  Lwt_watcher.create_input ()

let receipt_watcher : Transaction_receipt.t Lwt_watcher.input =
  Lwt_watcher.create_input ()

let l1_l2_levels_watcher :
    Ethereum_types.Subscription.l1_l2_levels_output Lwt_watcher.input =
  Lwt_watcher.create_input ()

module State = struct
  let current_blueprint_number ctxt =
    let (Qty next_blueprint_number) = ctxt.session.next_blueprint_number in
    Ethereum_types.Qty (Z.pred next_blueprint_number)

  module Transaction : sig
    (** [run ctxt k] executes [k ctxt' conn] where [ctxt'] is a copy of [ctxt]
        whose [session] field can be safely modified in-place and [conn] is a
        handler to a SQLite connection with a pre-started transaction.

        If [k] returns an [Error] or raises an exception, the changes made to
        [ctxt'] by [k] are not applied to [ctxt] (i.e., it is left unchanged),
        and the SQLite transaction is aborted. *)
    val run : t -> (t -> Sqlite.conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

    val initialize_head_info : t -> unit
  end = struct
    let with_store_transaction ctxt k =
      Evm_store.use ctxt.store @@ fun conn ->
      Evm_store.with_transaction conn @@ fun conn -> k conn

    (* [dup session] creates a copy of [session] that can be safely modified
       without altering the initial value. *)
    let dup
        {
          context;
          finalized_number;
          next_blueprint_number;
          current_block_hash;
          pending_upgrade;
          evm_state;
          last_split_block;
        } =
      {
        context;
        finalized_number;
        next_blueprint_number;
        current_block_hash;
        pending_upgrade;
        evm_state;
        last_split_block;
      }

    (* [apply session session'] modifies [session] in-place to match the content of [session']. *)
    let apply session
        {
          context;
          finalized_number;
          next_blueprint_number;
          current_block_hash;
          pending_upgrade;
          evm_state;
          last_split_block;
        } =
      session.context <- context ;
      session.finalized_number <- finalized_number ;
      session.next_blueprint_number <- next_blueprint_number ;
      session.current_block_hash <- current_block_hash ;
      session.pending_upgrade <- pending_upgrade ;
      session.evm_state <- evm_state ;
      session.last_split_block <- last_split_block

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

    let run ctxt (k : t -> Sqlite.conn -> 'a tzresult Lwt.t) : 'a tzresult Lwt.t
        =
      let open Lwt_result_syntax in
      let ctxt' = {ctxt with session = dup ctxt.session} in
      with_store_transaction ctxt @@ fun conn ->
      let*! res = k ctxt' conn in
      match res with
      | Ok res ->
          apply ctxt.session ctxt'.session ;
          let*! head_info in
          head_info := session_to_head_info ctxt.session ;
          let (Qty level) = current_blueprint_number ctxt in
          let (Qty finalized_level) = ctxt.session.finalized_number in
          Metrics.set_confirmed_level ~level:finalized_level ;
          Metrics.set_level ~level ;
          return res
      | Error err -> fail err

    let initialize_head_info ctxt =
      let first_head = ref (session_to_head_info ctxt.session) in
      Lwt.wakeup head_info_waker first_head
  end

  let load ~l2_chains ~data_dir ~store_perm:perm index =
    let open Lwt_result_syntax in
    let* store = Evm_store.init ~data_dir ~perm () in
    Evm_store.use store @@ fun conn ->
    let* latest = Evm_store.Context_hashes.find_latest conn in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let chain_family = Configuration.retrieve_chain_family ~l2_chains in
    match latest with
    | Some (Qty latest_blueprint_number, checkpoint) ->
        let*! context = Irmin_context.checkout_exn index checkpoint in
        let*! evm_state = Irmin_context.PVMState.get context in
        let+ current_block_hash =
          Evm_state.current_block_hash ~chain_family evm_state
        in
        ( store,
          context,
          Ethereum_types.Qty Z.(succ latest_blueprint_number),
          current_block_hash,
          Loaded )
    | None ->
        let context = Irmin_context.empty index in
        let genesis_parent_hash = L2_types.genesis_parent_hash ~chain_family in
        return
          ( store,
            context,
            Ethereum_types.Qty Z.zero,
            genesis_parent_hash,
            Created )

  let commit store (context : Irmin_context.rw) evm_state number =
    let open Lwt_result_syntax in
    let*! context = Irmin_context.PVMState.set context evm_state in
    let*! checkpoint = Irmin_context.commit context in
    let* () = Evm_store.Context_hashes.store store number checkpoint in
    return context

  let gc ctxt conn gc_level head_level history_mode =
    let open Lwt_result_syntax in
    let*! () = Evm_context_events.gc_started ~gc_level ~head_level in
    let start_timestamp = Time.System.now () in
    let* hash = Evm_store.Context_hashes.find conn gc_level in
    let*? hash =
      Option.to_result
        ~none:
          [
            error_of_fmt
              "Context hash of GC candidate %a is missing from store"
              Ethereum_types.pp_quantity
              gc_level;
          ]
        hash
    in
    let* () = Evm_store.reset_before conn ~l2_level:gc_level ~history_mode in
    let*! () = Irmin_context.gc ctxt.index hash in
    Metrics.start_pruning () ;
    let gc_waiter () =
      let open Lwt_syntax in
      let* () = Irmin_context.wait_gc_completion ctxt.index in
      let stop_timestamp = Time.System.now () in
      Metrics.stop_pruning () ;
      Evm_context_events.gc_finished
        ~gc_level
        ~head_level
        (Ptime.diff stop_timestamp start_timestamp)
    in

    let data_dir = ctxt.data_dir in
    match ctxt.configuration.experimental_features.periodic_snapshot_path with
    | Some path ->
        let*! () =
          Lwt.catch gc_waiter (fun exn ->
              Metrics.stop_pruning () ;
              Evm_context_events.gc_waiter_failed exn ;
              Lwt.reraise exn)
        in
        let snapshot_file =
          if Filename.is_relative path then Filename.concat data_dir path
          else path
        in
        let* _dest =
          Snapshots.export ~snapshot_file ~compression:On_the_fly ~data_dir ()
        in
        return_unit
    | None ->
        Lwt.dont_wait gc_waiter (fun exn ->
            Metrics.stop_pruning () ;
            Evm_context_events.gc_waiter_failed exn) ;
        return_unit

  let maybe_split_context ctxt conn timestamp level =
    let open Lwt_result_syntax in
    let* history_mode = Evm_store.Metadata.get_history_mode conn in
    match history_mode with
    | Archive -> return_none
    | Rolling gc_param | Full gc_param ->
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
                       (of_int gc_param.split_frequency_in_seconds)))
        in
        if split then (
          Irmin_context.split ctxt.index ;
          let* () = Evm_store.Irmin_chunks.insert conn level timestamp in
          let*! () = Evm_context_events.gc_split level timestamp in
          let* chunk_needing_gc =
            Evm_store.Irmin_chunks.nth conn gc_param.number_of_chunks
          in
          match chunk_needing_gc with
          | Some (gc_level, _gc_timestamp) ->
              let* () = gc ctxt conn gc_level level history_mode in
              return_some (level, timestamp)
          | None -> return_none)
        else return_none

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
    return (context, split_info)

  let replace_current_commit (ctxt : t) conn evm_state =
    let (Qty next) = ctxt.session.next_blueprint_number in
    commit conn ctxt.session.context evm_state (Qty Z.(pred next))

  let on_modified_head ctxt evm_state context =
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context

  let read_from_state evm_state path =
    let open Lwt_result_syntax in
    let*! res = Evm_state.inspect evm_state path in
    return res

  let on_new_delayed_transaction ~native_execution ~delayed_transaction
      evm_state =
    let open Lwt_result_syntax in
    let*! data_dir, config = execution_config in
    let* storage_version =
      Durable_storage.storage_version (read_from_state evm_state)
    in
    if storage_version < 15 then
      Evm_state.execute
        ~data_dir
        ~config
        ~native_execution
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
        ~native_execution
        ~wasm_entrypoint:"populate_delayed_inbox"
        ~data_dir
        ~config
        evm_state
        []

  let background_preemptive_download
      (kernel_execution : Configuration.kernel_execution_config) upgrade_event =
    let open Lwt_syntax in
    let open Evm_events.Upgrade in
    let rec downloader root_hash preimages_endpoint preimages =
      Lwt.catch
        (fun () ->
          (* Download the kernel. *)
          Misc.unwrap_error_monad @@ fun () ->
          Kernel_download.download ~preimages ~preimages_endpoint ~root_hash ())
        (fun exn ->
          (*  Error handling. *)
          let* () =
            Events.predownload_kernel_failed
              root_hash
              [Error_monad.error_of_exn exn]
          in
          let* () = Lwt_unix.sleep 60.0 in
          downloader root_hash preimages_endpoint preimages)
    in
    match kernel_execution.preimages_endpoint with
    | None -> ()
    | Some preimages_endpoint ->
        let (Hash (Hex root_hash)) = upgrade_event.hash in
        let root_hash = `Hex root_hash in
        Lwt.async (fun () ->
            downloader root_hash preimages_endpoint kernel_execution.preimages)

  let reset_to_level ctxt conn l2_level checkpoint =
    let open Lwt_result_syntax in
    let*! () = Evm_context_events.reset_at_level l2_level in
    (* Find the [l2_level] evm_state. *)
    let*! context = Irmin_context.checkout_exn ctxt.index checkpoint in
    let*! evm_state = Irmin_context.PVMState.get context in
    (* Clear the TX queue if needed, to preserve its invariants about nonces always increasing. *)
    let* () =
      let (module Tx_container) = ctxt.tx_container in
      Tx_container.clear ()
    in
    (* Clear the store. *)
    let* () = Evm_store.reset_after conn ~l2_level in
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    (* Update mutable session values. *)
    let next_blueprint_number = Ethereum_types.Qty.next l2_level in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let chain_family =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in
    let* current_block_hash =
      Evm_state.current_block_hash ~chain_family evm_state
    in
    ctxt.session.next_blueprint_number <- next_blueprint_number ;
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.current_block_hash <- current_block_hash ;
    ctxt.session.context <- context ;
    ctxt.session.pending_upgrade <- pending_upgrade ;
    return evm_state

  let reset_to_finalized_level exit_error ctxt conn =
    let open Lwt_result_syntax in
    let* safe_checkpoint = Evm_store.Context_hashes.find_finalized conn in
    match safe_checkpoint with
    | Some (finalized_number, checkpoint) ->
        reset_to_level ctxt conn finalized_number checkpoint
    | None ->
        let*! () =
          Evm_context_events.reset_impossible_missing_finalized_state ()
        in
        tzfail exit_error

  let blueprint_applied_event ctxt conn evm_state latest_finalized_level
      ({number = Qty number; hash = expected_block_hash} :
        Evm_events.Blueprint_applied.t) =
    let open Lwt_result_syntax in
    (* We use [max] to not rely on the order of the EVM events (because
       it is possible to see several blueprints applied during on L1
       level). *)
    let latest_finalized_level = Z.max latest_finalized_level number in
    let* block_hash_opt =
      if not ctxt.legacy_block_storage then
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
          return (evm_state, latest_finalized_level)
        else
          let*! () =
            Evm_events_follower_events.diverged
              (number, expected_block_hash, found_block_hash)
          in
          let exit_error =
            Node_error.Diverged
              {
                level = number;
                expected_block_hash;
                found_block_hash = Some found_block_hash;
                must_exit = true;
              }
          in
          if is_sequencer ctxt then
            (* Sequencer must fail in case of divergence. *)
            tzfail exit_error
          else
            (* Observers needs to reset at finalized level. *)
            let* evm_state = reset_to_finalized_level exit_error ctxt conn in
            return (evm_state, latest_finalized_level)
    | None when is_sequencer ctxt ->
        let*! () =
          Evm_events_follower_events.missing_blueprint
            (number, expected_block_hash)
        in
        tzfail
          (Node_error.Diverged
             {
               level = number;
               expected_block_hash;
               found_block_hash = None;
               must_exit = true;
             })
    | None ->
        let*! () = Evm_events_follower_events.rollup_node_ahead (Qty number) in
        let* () =
          Evm_store.Pending_confirmations.insert
            conn
            (Qty number)
            expected_block_hash
        in
        return (evm_state, latest_finalized_level)

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
    match block.transactions with
    | Ethereum_types.TxHash hashes ->
        List.map_es
          (fun hash ->
            let* receipt =
              let* receipt_opt =
                Etherlink_durable_storage.transaction_receipt
                  (read_from_state evm_state)
                  ~block_hash:block.hash
                  hash
              in
              match receipt_opt with
              | Some receipt -> return receipt
              | None ->
                  failwith "Receipt missing for %a" Ethereum_types.pp_hash hash
            in
            let* object_ =
              let* object_opt =
                Etherlink_durable_storage.transaction_object
                  (read_from_state evm_state)
                  hash
              in
              match object_opt with
              | Some object_ -> return object_
              | None ->
                  failwith "Object missing for %a" Ethereum_types.pp_hash hash
            in
            let info = Transaction_info.of_receipt_and_object receipt object_ in
            let* () = Evm_store.Transactions.store conn info in
            return receipt)
          hashes
    | TxFull _ ->
        (* Block must be read without the transactions objects. Even though
           we could be resilient and store the objects, it doesn't make sense
           at the moment in the code to deal this case. The node does not need
           to read the full block on block processing. If for some reasons
           the code reads the full block, simply store the transactions objects
           and fetch the receipts here. *)
        assert false

  let store_tez_block_unsafe conn block =
    let open Lwt_result_syntax in
    (* Store the block itself. *)
    let* () = Evm_store.Blocks.tez_store conn block in
    return_nil

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

    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let chain_family =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in
    let* try_apply =
      Misc.with_timing
        (fun time -> Lwt.return (time_processed := time))
        (fun () ->
          Evm_state.apply_blueprint
            ~native_execution_policy:
              ctxt.configuration.kernel_execution.native_execution_policy
            ~wasm_pvm_fallback:(not @@ List.is_empty delayed_transactions)
            ~data_dir
            ~chain_family
            ~config
            ctxt.session.evm_state
            payload)
    in

    match try_apply with
    | Apply_success {evm_state; block} ->
        let block_number = L2_types.block_number block in
        let block_hash = L2_types.block_hash block in
        let* () =
          if is_sequencer ctxt then return_unit
          else
            let* finalized_hash =
              Evm_store.Pending_confirmations.find_with_level conn block_number
            in
            match finalized_hash with
            | None -> return_unit
            | Some expected_block_hash ->
                if expected_block_hash = block_hash then
                  Evm_store.Pending_confirmations.delete_with_level
                    conn
                    block_number
                else
                  let Ethereum_types.(Qty number) = block_number in

                  let*! () =
                    Evm_events_follower_events.diverged
                      (number, expected_block_hash, block_hash)
                  in
                  (* If the observer cannot reset to finalized level it must
                     exit. *)
                  let exit_error =
                    Node_error.Diverged
                      {
                        level = number;
                        expected_block_hash;
                        found_block_hash = Some block_hash;
                        must_exit = true;
                      }
                  in
                  let* (_ : Evm_state.t) =
                    reset_to_finalized_level exit_error ctxt conn
                  in
                  (* If the observer managed to reset to finalized level it must
                     not exit. *)
                  tzfail
                    (Node_error.Diverged
                       {
                         level = number;
                         expected_block_hash;
                         found_block_hash = Some block_hash;
                         must_exit = false;
                       })
        in
        (* Set metrics for the block *)
        let () =
          match block with
          | Eth block ->
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
                  baseFeePerGas |> Ethereum_types.Qty.to_z
                  |> Metrics.set_gas_price)
                block.baseFeePerGas
          | Tez _ ->
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
              ()
        in
        let* () =
          Evm_store.Blueprints.store
            conn
            {number = block_number; timestamp; payload}
        in

        let* evm_state, receipts =
          if not ctxt.legacy_block_storage then
            let* receipts =
              match block with
              | Eth block -> store_block_unsafe conn evm_state block
              | Tez block -> store_tez_block_unsafe conn block
            in
            let*! evm_state = Evm_state.clear_block_storage block evm_state in
            return (evm_state, receipts)
          else
            let*! receipts =
              match block with
              | Eth block ->
                  Etherlink_durable_storage.block_receipts_of_block
                    (read_from_state evm_state)
                    block
              | Tez _ ->
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
                  Lwt.return []
            in
            return (evm_state, receipts)
        in
        List.iter (Lwt_watcher.notify receipt_watcher) receipts ;

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

        let* context, split_info =
          commit_next_head ctxt conn timestamp evm_state
        in
        return (evm_state, context, block, applied_kernel_upgrade, split_info)
    | Apply_failure (* Did not produce a block *) ->
        let*! () =
          if is_sequencer ctxt then
            Blueprint_events.invalid_blueprint_produced next
          else Blueprint_events.invalid_blueprint_applied next
        in
        tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

  let on_new_head ?split_info ctxt ~applied_upgrade evm_state context block
      blueprint_with_events =
    let open Lwt_syntax in
    let block_hash = L2_types.block_hash block in
    let (Qty level) = ctxt.session.next_blueprint_number in
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context ;
    ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
    ctxt.session.current_block_hash <- block_hash ;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7865 *)
    (match block with
    | Eth block ->
        Lwt_watcher.notify
          head_watcher
          (Ethereum_types.Subscription.NewHeads block)
    | Tez _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
        ()) ;
    Option.iter
      (fun (split_level, split_timestamp) ->
        ctxt.session.last_split_block <- Some (split_level, split_timestamp))
      split_info ;
    Broadcast.notify @@ Broadcast.Blueprint blueprint_with_events ;
    if applied_upgrade then ctxt.session.pending_upgrade <- None ;
    return_unit

  type error +=
    | Invalid_rollup_node of {
        smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
        rollup_node_smart_rollup_address :
          Tezos_crypto.Hashed.Smart_rollup_address.t;
      }

  let prepare_local_flushed_blueprint ctxt parent_hash
      Evm_events.Flushed_blueprint.
        {transactions; timestamp; level = flushed_level} =
    let open Lwt_result_syntax in
    let hashes =
      List.map (fun tx -> tx.Evm_events.Delayed_transaction.hash) transactions
    in
    let* sequencer_key, cctxt =
      match ctxt.sequencer_wallet with
      | None ->
          failwith "Only sequencer is capable to handle the flushed blueprint"
      | Some (sequencer_key, cctxt) -> return (sequencer_key, cctxt)
    in
    let* blueprint_chunks =
      Sequencer_blueprint.prepare
        ~sequencer_key
        ~cctxt
        ~timestamp
        ~transactions:[]
        ~delayed_transactions:hashes
        ~parent_hash
        ~number:flushed_level
    in
    let payload =
      Sequencer_blueprint.create_inbox_payload
        ~smart_rollup_address:
          (Tezos_crypto.Hashed.Smart_rollup_address.to_string
             ctxt.smart_rollup_address)
        ~chunks:blueprint_chunks
    in
    return payload

  let clear_head_delayed_inbox ctxt =
    let open Lwt_result_syntax in
    let*! cleaned_evm_state =
      Evm_state.clear_delayed_inbox ctxt.session.evm_state
    in
    ctxt.session.evm_state <- cleaned_evm_state ;
    return_unit

  let rec apply_blueprint ?(events = []) ctxt conn timestamp payload
      delayed_transactions : 'a L2_types.block tzresult Lwt.t =
    let open Lwt_result_syntax in
    Misc.with_timing_f_e Blueprint_events.blueprint_applied @@ fun () ->
    let* evm_state, context, current_block, applied_kernel_upgrade, split_info =
      let* () = apply_evm_events conn ctxt events in
      apply_blueprint_store_unsafe
        ctxt
        conn
        timestamp
        payload
        delayed_transactions
    in
    let kernel_upgrade =
      match ctxt.session.pending_upgrade with
      | Some {injected_before; kernel_upgrade}
        when injected_before = ctxt.session.next_blueprint_number ->
          Some kernel_upgrade
      | _ -> None
    in

    let* current_block =
      match current_block with
      | Eth block ->
          let*? current_block =
            Transaction_object.reconstruct_block payload block
          in
          return (L2_types.Eth current_block)
      | Tez block -> return (L2_types.Tez block)
    in

    let*! () =
      on_new_head
        ?split_info
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
    return current_block

  and apply_evm_event_unsafe ctxt conn evm_state event latest_finalized_level =
    let open Lwt_result_syntax in
    let*! () = Evm_events_follower_events.new_event event in
    match event with
    | Evm_events.Upgrade_event upgrade ->
        ctxt.session.pending_upgrade <-
          Some
            {
              kernel_upgrade = upgrade;
              injected_before = ctxt.session.next_blueprint_number;
            } ;
        background_preemptive_download
          ctxt.configuration.kernel_execution
          upgrade ;
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
        return (evm_state, latest_finalized_level)
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
        return (evm_state, latest_finalized_level)
    | Blueprint_applied event ->
        blueprint_applied_event ctxt conn evm_state latest_finalized_level event
    | New_delayed_transaction delayed_transaction ->
        let* evm_state =
          on_new_delayed_transaction
            ~native_execution:
              (ctxt.configuration.kernel_execution.native_execution_policy
             = Always)
            ~delayed_transaction
            evm_state
        in
        return (evm_state, latest_finalized_level)
    | Flush_delayed_inbox flushed_blueprint ->
        let*! () =
          Evm_events_follower_events.flush_delayed_inbox
            ~timestamp:flushed_blueprint.timestamp
            flushed_blueprint.level
        in
        let* evm_state = flush_delayed_inbox ctxt conn flushed_blueprint in
        return (evm_state, latest_finalized_level)

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
      let* context, evm_state =
        let start_finalized_number = ctxt.session.finalized_number in
        let* evm_state, context, Qty latest_finalized_number =
          match events with
          | [] ->
              (* Avoid an uncessary {!replace_current_commit} if the list is
                 empty. *)
              return
                ( ctxt.session.evm_state,
                  ctxt.session.context,
                  ctxt.session.finalized_number )
          | events ->
              let* evm_state, latest_finalized_number =
                List.fold_left_es
                  (fun (evm_state, Ethereum_types.Qty finalized_number) event ->
                    let* evm_state, latest_finalized_level =
                      apply_evm_event_unsafe
                        ctxt
                        conn
                        evm_state
                        event
                        finalized_number
                    in
                    return (evm_state, Ethereum_types.Qty latest_finalized_level))
                  (ctxt.session.evm_state, ctxt.session.finalized_number)
                  events
              in
              let* context = replace_current_commit ctxt conn evm_state in
              return (evm_state, context, latest_finalized_number)
        in
        (* Process the new `latest_finalized_number`. *)
        ctxt.session.finalized_number <- Qty latest_finalized_number ;
        let* () =
          Option.iter_es
            (fun l1_level ->
              let* () =
                Evm_store.L1_l2_levels_relationships.store
                  conn
                  ~l1_level
                  ~latest_l2_level:(current_blueprint_number ctxt)
              in
              let* () =
                Evm_store.L1_l2_finalized_levels.store
                  conn
                  ~l1_level
                  ~start_l2_level:start_finalized_number
                  ~end_l2_level:(Qty latest_finalized_number)
              in
              Metrics.set_l1_level ~level:l1_level ;
              Lwt_watcher.notify
                l1_l2_levels_watcher
                {
                  l1_level;
                  start_l2_level = start_finalized_number;
                  end_l2_level = Qty latest_finalized_number;
                } ;
              let*! () =
                Evm_context_events.processed_l1_level
                  (l1_level, latest_finalized_number)
              in
              return_unit)
            finalized_level
        in
        return (context, evm_state)
      in
      on_modified_head ctxt evm_state context ;
      return_unit)
    else return_unit

  and flush_delayed_inbox ctxt conn
      (Evm_events.Flushed_blueprint.
         {transactions = delayed_transactions; timestamp; level = flushed_level}
       as flushed_blueprint) =
    let open Lwt_result_syntax in
    let before_flushed_level =
      Ethereum_types.Qty.pred
        flushed_blueprint.Evm_events.Flushed_blueprint.level
    in
    (* save upgrades before reset *)
    let* lost_upgrade =
      Evm_store.Kernel_upgrades.find_latest_injected_after
        conn
        before_flushed_level
    in
    (* The kernel has produced a block for level [flushed_level]. The first thing
       to do is go back to an EVM state before this flushed blueprint. *)
    let* (_ : Evm_state.t) =
      let* checkpoint =
        Evm_store.Context_hashes.find conn before_flushed_level
      in
      match checkpoint with
      | None ->
          let*! () = Evm_context_events.missing_state before_flushed_level in
          tzfail (Node_error.Cannot_handle_flushed_blueprint flushed_level)
      | Some checkpoint ->
          reset_to_level ctxt conn before_flushed_level checkpoint
    in
    (* Clean unreliable delayed inbox *)
    let* () = clear_head_delayed_inbox ctxt in
    (* Prepare an event list to be reapplied on current head *)
    let events = Evm_events.of_parts delayed_transactions lost_upgrade in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let chain_family =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in
    (* Prepare a blueprint payload signed by the sequencer to execute locally. *)
    let* parent_hash =
      Evm_state.current_block_hash ~chain_family ctxt.session.evm_state
    in
    let* payload =
      prepare_local_flushed_blueprint ctxt parent_hash flushed_blueprint
    in
    (* Apply the blueprint. *)
    let* _block =
      apply_blueprint ~events ctxt conn timestamp payload delayed_transactions
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

  let check_smart_rollup_address ~store_smart_rollup_address
      ~smart_rollup_address =
    let open Lwt_result_syntax in
    match (store_smart_rollup_address, smart_rollup_address) with
    | Some store_smart_rollup_address, Some smart_rollup_address ->
        let* () =
          fail_unless
            (Tezos_crypto.Hashed.Smart_rollup_address.equal
               smart_rollup_address
               store_smart_rollup_address)
            (Invalid_rollup_node
               {
                 smart_rollup_address = store_smart_rollup_address;
                 rollup_node_smart_rollup_address = smart_rollup_address;
               })
        in
        return smart_rollup_address
    | None, Some smart_rollup_address -> return smart_rollup_address
    | Some store_smart_rollup_address, None -> return store_smart_rollup_address
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

  let history_mode_switch_status h1 h2 =
    let open Configuration in
    match (h1, h2) with
    | Archive, Archive -> `No_switch
    | (Rolling gc, Rolling gc' | Full gc, Full gc')
      when gc.number_of_chunks = gc'.number_of_chunks ->
        `No_switch
    (* Keep this match explicit *)
    | Rolling _, Full _ | Rolling _, Archive | Full _, Archive -> `Cannot_switch
    | Rolling _, Rolling _
    | Full _, Full _
    | Full _, Rolling _
    | Archive, Rolling _
    | Archive, Full _ ->
        `Needs_switch

  let check_history_mode ?(switch = false) ~store_history_mode ~history_mode ()
      =
    let open Lwt_result_syntax in
    match (store_history_mode, history_mode) with
    | Some h, None -> return h
    | None, None -> return Configuration.default_history_mode
    | None, Some h -> return h
    | Some store_history_mode, Some history_mode -> (
        match history_mode_switch_status store_history_mode history_mode with
        | `No_switch ->
            if switch then
              Format.printf
                "History mode is already %a, not switching@."
                Configuration.pp_history_mode_debug
                store_history_mode ;
            return store_history_mode
        | `Needs_switch when switch ->
            let*! () =
              Evm_context_events.switching_history_mode
                ~from:store_history_mode
                ~to_:history_mode
            in
            return history_mode
        | _ ->
            tzfail (Incorrect_history_mode {store_history_mode; history_mode}))

  let check_metadata ~store_metadata ~smart_rollup_address ~history_mode () =
    let open Lwt_result_syntax in
    let* smart_rollup_address =
      check_smart_rollup_address
        ~store_smart_rollup_address:
          (Option.map
             (fun (metadata : Evm_store.metadata) ->
               metadata.smart_rollup_address)
             store_metadata)
        ~smart_rollup_address
    in
    let* history_mode =
      check_history_mode
        ~store_history_mode:
          (Option.map
             (fun (metadata : Evm_store.metadata) -> metadata.history_mode)
             store_metadata)
        ~history_mode
        ()
    in
    return ({smart_rollup_address; history_mode} : Evm_store.metadata)

  let irmin_load ?snapshot_url ~data_dir configuration =
    let open Lwt_result_syntax in
    let open Configuration in
    let*! store_already_exists =
      Lwt_utils_unix.dir_exists (Evm_state.irmin_store_path ~data_dir)
    in
    let* () =
      match snapshot_url with
      | Some snapshot_url when not store_already_exists -> (
          let* history_mode =
            Snapshots.import_from
              ~force:true
              ?history_mode:configuration.history_mode
              ~data_dir
              ~snapshot_file:snapshot_url
              ()
          in
          match history_mode with
          | Some h ->
              let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
              let* () =
                Evm_store.use store @@ fun conn ->
                Evm_store.Metadata.store_history_mode conn h
              in
              let*! () = Evm_store.close store in
              return_unit
          | None -> return_unit)
      | _ -> return_unit
    in
    Irmin_context.load
      ~cache_size:100_000
      Read_write
      (Evm_state.irmin_store_path ~data_dir)

  let on_disk_kernel = function Wasm_debugger.On_disk _ -> true | _ -> false

  let init ~(configuration : Configuration.t) ?kernel_path ~data_dir
      ?smart_rollup_address ~store_perm ?sequencer_wallet ?snapshot_url
      ~tx_container () =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.create_dir (Evm_state.kernel_logs_directory ~data_dir)
    in
    let*! () =
      Lwt_utils_unix.create_dir configuration.kernel_execution.preimages
    in
    let* index = irmin_load ?snapshot_url ~data_dir configuration in
    let* store, context, next_blueprint_number, current_block_hash, init_status
        =
      load
        ~l2_chains:configuration.experimental_features.l2_chains
        ~data_dir
        ~store_perm
        index
    in
    Evm_store.use store @@ fun conn ->
    let* () =
      let* is_empty = Evm_store.Pending_confirmations.is_empty conn in
      when_
        (Option.is_some sequencer_wallet && not is_empty)
        (fun () ->
          failwith "Store has pending confirmation, state is not final")
    in
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    let* latest = Evm_store.L1_l2_finalized_levels.last conn in
    let finalized_number, l1_level =
      match latest with
      | None -> (Ethereum_types.Qty Z.zero, None)
      | Some (l1_level, {end_l2_level; _}) -> (end_l2_level, Some l1_level)
    in
    let* {smart_rollup_address; history_mode} =
      let smart_rollup_address =
        Option.map
          Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn
          smart_rollup_address
      in
      let* store_metadata = Evm_store.Metadata.find conn in
      let* metadata =
        check_metadata
          ~store_metadata
          ~smart_rollup_address
          ~history_mode:configuration.history_mode
          ()
      in
      let* () =
        when_ (Option.is_none store_metadata) (fun () ->
            Evm_store.Metadata.store conn metadata)
      in
      return metadata
    in
    let*! () =
      match
        ( history_mode,
          configuration.experimental_features.periodic_snapshot_path )
      with
      | Archive, Some _ -> Events.ignored_periodic_snapshot ()
      | _ -> Lwt.return_unit
    in
    let*! () = Evm_context_events.start_history_mode history_mode in

    let* evm_state, context =
      match kernel_path with
      | Some kernel ->
          if init_status = Loaded then
            let* () =
              when_ (on_disk_kernel kernel) @@ fun () ->
              Lwt_result.ok (Events.ignored_kernel_arg ())
            in
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

    let* last_split_block =
      match history_mode with
      | Rolling _ | Full _ -> Evm_store.Irmin_chunks.latest conn
      | Archive -> return_none
    in

    let* legacy_block_storage = Evm_store.Block_storage_mode.legacy conn in

    let ctxt =
      {
        configuration;
        index;
        data_dir;
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
          };
        store;
        sequencer_wallet;
        legacy_block_storage;
        tx_container;
      }
    in

    let* () =
      unless legacy_block_storage (fun () ->
          let* ro_store = Evm_store.init ~data_dir ~perm:`Read_only () in
          let* evm_node_endpoint =
            if is_sequencer ctxt then return_none
            else
              match configuration.observer with
              | Some observer -> return_some observer.evm_node_endpoint
              | None -> return_none
          in

          Block_storage_setup.enable
            ~keep_alive:configuration.keep_alive
            ?evm_node_endpoint
            ro_store ;
          return_unit)
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

  let patch_state (ctxt : t) ?block_number ~commit ~key patch () =
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
    let*! previous_value = Evm_state.inspect evm_state key in
    let new_value = patch (Option.map Bytes.to_string previous_value) in
    let*! evm_state =
      match new_value with
      | Some value -> Evm_state.modify ~key ~value evm_state
      | None when Option.is_some previous_value ->
          Evm_state.delete ~kind:Value evm_state key
      | None -> Lwt.return evm_state
    in
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

  let observer_apply_reorg ctxt conn blueprint_with_events pred_number =
    let open Lwt_result_syntax in
    (* Checkout the state before the blueprint. *)
    let* checkpoint = Evm_store.Context_hashes.find conn pred_number in
    match checkpoint with
    | None ->
        let*! () =
          Evm_context_events.observer_reorg_cannot_find_state pred_number
        in
        return_none
    | Some checkpoint ->
        let* (_ : Evm_state.t) =
          reset_to_level ctxt conn pred_number checkpoint
        in
        (* Apply the blueprint. *)
        let events =
          Blueprint_types.events_of_blueprint_with_events blueprint_with_events
        in
        let* _block =
          apply_blueprint
            ~events
            ctxt
            conn
            blueprint_with_events.blueprint.timestamp
            blueprint_with_events.blueprint.payload
            blueprint_with_events.delayed_transactions
        in

        (* Ask the blueprint follower to restart at blueprint's successor. *)
        return_some ctxt.session.next_blueprint_number

  let rec potential_observer_reorg ~evm_node_endpoint conn ctxt
      blueprint_with_events =
    let open Lwt_result_syntax in
    (*
       1. Check if this blueprint was finalized on L1.
       2. Check if the blueprint is different than the one
          we already received.
       3. Check if the sequencer signed this new blueprint.
       4. Find the divergence point.
       5. Apply the reorganized blueprint.
       6. Restart blueprint follower after this level.
    *)
    let blueprint_number =
      blueprint_with_events.Blueprint_types.blueprint.number
    in
    let*! () = Evm_context_events.observer_potential_reorg blueprint_number in
    (* We check in the local storage if we already saw a blueprint
       for this number, and whether it's the same or not.

       If it's not the same blueprint, it might be the sign of a
       reorganization.
    *)
    let* is_it_the_same_blueprint =
      let+ blueprint_in_store =
        Evm_store.Blueprints.find_with_events conn blueprint_number
      in
      match blueprint_in_store with
      | None ->
          (* The blueprint has been removed from the store, we cannot check it. *)
          false
      | Some blueprint_in_store ->
          Blueprint_types.with_events_equal
            blueprint_in_store
            blueprint_with_events
    in
    if is_it_the_same_blueprint then
      (* The endpoint simply republished an old blueprint. *)
      let*! () =
        Evm_context_events.observer_reorg_old_blueprint blueprint_number
      in
      return_none
    else
      (* As we have a different blueprint we want to check if it was signed
         by the sequencer, otherwise it is meaningless.

         We also decode the blueprint to retrieve the parent hash to see
         if we agree with it.
      *)
      let* sequencer =
        Durable_storage.sequencer (read_from_state ctxt.session.evm_state)
      in
      let blueprint_parent_hash =
        Sequencer_blueprint.kernel_blueprint_parent_hash_of_payload
          sequencer
          blueprint_with_events.blueprint.payload
      in
      match blueprint_parent_hash with
      | Some blueprint_parent_hash -> (
          (* If the blueprint parent hash is not equal to whatever block hash
             we have for its predecessor, that means this is not the first
             block of the reorganization and we traverse backward the blueprints
             until we find the blueprint.
          *)
          let (Qty pred_number) = Ethereum_types.Qty.pred blueprint_number in
          let* local_parent_block_hash =
            if not ctxt.legacy_block_storage then
              Evm_store.Blocks.find_hash_of_number conn (Qty pred_number)
            else
              let*! bytes =
                Evm_state.inspect
                  ctxt.session.evm_state
                  (Durable_storage_path.Indexes.block_by_number
                     (Nth pred_number))
              in
              return (Option.map Ethereum_types.decode_block_hash bytes)
          in
          match local_parent_block_hash with
          | None ->
              let*! () =
                Evm_context_events.observer_reorg_cannot_find_divergence
                  blueprint_number
              in
              return_none
          | Some local_parent_block_hash ->
              if local_parent_block_hash <> blueprint_parent_hash then
                (* Reorganization started on an older block, we
                   traverse the chain backward. *)
                let* blueprint_pred =
                  Evm_services.get_blueprint
                    ~keep_alive:true
                    ~evm_node_endpoint
                    (Qty pred_number)
                in
                potential_observer_reorg
                  ~evm_node_endpoint
                  conn
                  ctxt
                  blueprint_pred
              else
                (* We agree with the blueprint predecessor but not the blueprint,
                   this is the divergence point. *)
                observer_apply_reorg
                  ctxt
                  conn
                  blueprint_with_events
                  (Qty pred_number))
      | None ->
          let*! () =
            Evm_context_events.observer_reorg_cannot_decode_blueprint
              blueprint_number
          in
          return_none
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Request

  type self = worker

  type launch_error = tztrace

  let on_launch _self ()
      {
        configuration : Configuration.t;
        kernel_path;
        data_dir : string;
        smart_rollup_address : string option;
        store_perm;
        sequencer_wallet;
        snapshot_url;
        tx_container;
      } =
    let open Lwt_result_syntax in
    let* ctxt, status =
      State.init
        ~configuration
        ?kernel_path
        ~data_dir
        ?smart_rollup_address
        ~store_perm
        ?sequencer_wallet
        ?snapshot_url
        ~tx_container
        ()
    in
    Lwt.wakeup execution_config_waker @@ (ctxt.data_dir, pvm_config ctxt) ;
    Lwt.wakeup init_status_waker status ;
    State.Transaction.initialize_head_info ctxt ;
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
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.apply_evm_events ?finalized_level conn ctxt events
    | Apply_blueprint {events; timestamp; payload; delayed_transactions} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        let* block =
          State.apply_blueprint
            ?events
            ctxt
            conn
            timestamp
            payload
            delayed_transactions
        in
        let tx_hashes =
          match block with
          | Eth block -> (
              match block.transactions with
              | TxHash tx_hashes -> List.to_seq tx_hashes
              | TxFull tx_objects ->
                  List.to_seq tx_objects |> Seq.map Transaction_object.hash)
          | Tez _ -> Seq.empty
        in
        return tx_hashes
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
    | Patch_state {commit; key; patch; block_number} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.patch_state ?block_number ctxt ~commit ~key patch ()
    | Wasm_pvm_version ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.wasm_pvm_version ctxt
    | Potential_observer_reorg {evm_node_endpoint; blueprint_with_events} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.potential_observer_reorg
          ~evm_node_endpoint
          conn
          ctxt
          blueprint_with_events

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
      | Last_known_L1_level -> Eq
      | Delayed_inbox_hashes -> Eq
      | Patch_state _ -> Eq
      | Wasm_pvm_version -> Eq
      | Potential_observer_reorg _ -> Eq
  end

  let on_error (type a b) _self _st (req : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with
    | Apply_evm_events _, [Node_error.Diverged {must_exit = true; _}]
    | Apply_blueprint _, [Node_error.Diverged {must_exit = true; _}] ->
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
        return `Continue
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

let start ~configuration ?kernel_path ~data_dir ?smart_rollup_address
    ~store_perm ?sequencer_wallet ?snapshot_url
    ~(tx_container : (module Services_backend_sig.Tx_container)) () =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir in
  let* worker =
    Worker.launch
      table
      ()
      {
        configuration;
        kernel_path;
        data_dir;
        smart_rollup_address;
        store_perm;
        sequencer_wallet;
        snapshot_url;
        tx_container;
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
  Evm_store.(
    use store @@ fun conn ->
    let* () = Block_storage_mode.force_legacy conn in
    Context_hashes.store conn (Qty current_blueprint_number) checkpoint)

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

let init_from_rollup_node ~configuration ~omit_delayed_tx_events ~data_dir
    ~rollup_node_data_dir ~tx_container () =
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
      ~configuration
      ~data_dir
      ~smart_rollup_address
      ~store_perm:`Read_write
      ~tx_container
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

let last_known_l1_level () = worker_wait_for_request Last_known_L1_level

let delayed_inbox_hashes () = worker_wait_for_request Delayed_inbox_hashes

let patch_kernel ?block_number kernel =
  let open Lwt_result_syntax in
  let* kernel, is_binary = Wasm_debugger.read_kernel kernel in
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
           patch = Fun.const (Some kernel);
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
         patch = Fun.const (Some (Signature.Public_key.to_b58check pk));
         block_number;
       })

let provision_balance ?block_number address value =
  worker_wait_for_request
    (Patch_state
       {
         commit = false;
         key = Durable_storage_path.Accounts.balance address;
         patch =
           (let open Ethereum_types in
            function
            | Some old_balance ->
                let (Qty v) =
                  decode_number_le (Bytes.unsafe_of_string old_balance)
                in
                let (Qty u) = value in
                Some (String.of_bytes (encode_u256_le (Qty Z.(u + v))))
            | None -> Some (String.of_bytes (encode_u256_le value)));
         block_number;
       })

let patch_state ?block_number ~key ~value () =
  worker_wait_for_request
    (Patch_state
       {commit = true; key; patch = Fun.const (Some value); block_number})

let potential_observer_reorg evm_node_endpoint blueprint_with_events =
  worker_wait_for_request
    (Potential_observer_reorg {evm_node_endpoint; blueprint_with_events})

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_context_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit

let check_history_mode = State.check_history_mode
