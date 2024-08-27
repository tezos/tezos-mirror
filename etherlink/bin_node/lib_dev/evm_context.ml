(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

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
}

type session_state = {
  mutable context : Irmin_context.rw;
  mutable finalized_number : Ethereum_types.quantity;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  mutable pending_upgrade : Evm_events.Upgrade.t option;
  mutable evm_state : Evm_state.t;
}

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
    pending_upgrade = session.pending_upgrade;
  }

let pvm_config ctxt =
  Config.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

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

module Request = struct
  type block_request =
    | Number of Ethereum_types.quantity
    | Hash of Ethereum_types.block_hash

  type (_, _) t =
    | Apply_evm_events : {
        finalized_level : int32 option;
        events : Evm_events.t list;
      }
        -> (unit, tztrace) t
    | Apply_blueprint : {
        timestamp : Time.Protocol.t;
        payload : Blueprint_types.payload;
        delayed_transactions : Evm_events.Delayed_transaction.t list;
      }
        -> (unit, tztrace) t
    | Last_produce_blueprint : (Blueprint_types.t, tztrace) t
    | Blueprint : {
        level : Ethereum_types.quantity;
      }
        -> (Blueprint_types.with_events option, tztrace) t
    | Blueprints_range : {
        from : Ethereum_types.quantity;
        to_ : Ethereum_types.quantity;
      }
        -> ((Ethereum_types.quantity * Blueprint_types.payload) list, tztrace) t
    | Last_known_L1_level : (int32 option, tztrace) t
    | New_last_known_L1_level : int32 -> (unit, tztrace) t
    | Delayed_inbox_hashes : (Ethereum_types.hash list, tztrace) t
    | Evm_state_after : block_request -> (Evm_state.t option, tztrace) t
    | Earliest_state : (Evm_state.t option, tztrace) t
    | Earliest_number : (Ethereum_types.quantity option, tztrace) t
    | Reconstruct : {
        rollup_node_data_dir : string;
        genesis_level : int32;
        finalized_level : int32;
        levels_to_hashes : [`Read] Rollup_node_storage.Levels_to_hashes.t;
        l2_blocks : [`Read] Rollup_node_storage.L2_blocks.t;
      }
        -> (unit, tztrace) t
    | Patch_state : {
        commit : bool;
        key : string;
        value : string;
        block_number : Ethereum_types.quantity option;
      }
        -> (unit, tztrace) t
    | Wasm_pvm_version : (Tezos_scoru_wasm.Wasm_pvm_state.version, tztrace) t

  type view = View : _ t -> view

  let view req = View req

  let block_request_encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"number"
          (obj2
             (req "kind" (constant "number"))
             (req "number" Ethereum_types.quantity_encoding))
          (function Number number -> Some ((), number) | _ -> None)
          (fun ((), number) -> Number number);
        case
          (Tag 1)
          ~title:"hash"
          (obj2
             (req "kind" (constant "number"))
             (req "hash" Ethereum_types.block_hash_encoding))
          (function Hash hash -> Some ((), hash) | _ -> None)
          (fun ((), hash) -> Hash hash);
      ]

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Apply_evm_events"
          (obj3
             (req "request" (constant "apply_evm_events"))
             (opt "finalized_level" int32)
             (req "events" (list Evm_events.encoding)))
          (function
            | View (Apply_evm_events {finalized_level; events}) ->
                Some ((), finalized_level, events)
            | _ -> None)
          (fun ((), finalized_level, events) ->
            View (Apply_evm_events {finalized_level; events}));
        case
          (Tag 1)
          ~title:"Apply_blueprint"
          (obj4
             (req "request" (constant "apply_blueprint"))
             (req "timestamp" Time.Protocol.encoding)
             (req "payload" Blueprint_types.payload_encoding)
             (req
                "delayed_transactions"
                (list Evm_events.Delayed_transaction.encoding)))
          (function
            | View (Apply_blueprint {timestamp; payload; delayed_transactions})
              ->
                Some ((), timestamp, payload, delayed_transactions)
            | _ -> None)
          (fun ((), timestamp, payload, delayed_transactions) ->
            View (Apply_blueprint {timestamp; payload; delayed_transactions}));
        case
          (Tag 2)
          ~title:"Last_produce_blueprint"
          (obj1 (req "request" (constant "last_produce_blueprint")))
          (function View Last_produce_blueprint -> Some () | _ -> None)
          (fun () -> View Last_produce_blueprint);
        case
          (Tag 4)
          ~title:"Blueprint"
          (obj2
             (req "request" (constant "blueprint"))
             (req "level" Ethereum_types.quantity_encoding))
          (function View (Blueprint {level}) -> Some ((), level) | _ -> None)
          (fun ((), level) -> View (Blueprint {level}));
        case
          (Tag 5)
          ~title:"Blueprints_range"
          (obj3
             (req "request" (constant "Blueprints_range"))
             (req "from" Ethereum_types.quantity_encoding)
             (req "to" Ethereum_types.quantity_encoding))
          (function
            | View (Blueprints_range {from; to_}) -> Some ((), from, to_)
            | _ -> None)
          (fun ((), from, to_) -> View (Blueprints_range {from; to_}));
        case
          (Tag 6)
          ~title:"Last_known_L1_level"
          (obj1 (req "request" (constant "last_known_l1_level")))
          (function View Last_known_L1_level -> Some () | _ -> None)
          (fun () -> View Last_known_L1_level);
        case
          (Tag 7)
          ~title:"New_last_known_L1_level"
          (obj2
             (req "request" (constant "new_last_known_l1_level"))
             (req "value" int32))
          (function
            | View (New_last_known_L1_level l) -> Some ((), l) | _ -> None)
          (fun ((), l) -> View (New_last_known_L1_level l));
        case
          (Tag 8)
          ~title:"Delayed_inbox_hashes"
          (obj1 (req "request" (constant "Delayed_inbox_hashes")))
          (function View Delayed_inbox_hashes -> Some () | _ -> None)
          (fun () -> View Delayed_inbox_hashes);
        case
          (Tag 9)
          ~title:"Evm_state_after"
          (obj2
             (req "request" (constant "evm_state_after"))
             (req "payload" block_request_encoding))
          (function
            | View (Evm_state_after block_request) -> Some ((), block_request)
            | _ -> None)
          (fun ((), block_request) -> View (Evm_state_after block_request));
        case
          (Tag 10)
          ~title:"Earliest_state"
          (obj1 (req "request" (constant "earliest_state")))
          (function View Earliest_state -> Some () | _ -> None)
          (fun () -> View Earliest_state);
        case
          (Tag 11)
          ~title:"Earliest_number"
          (obj1 (req "request" (constant "earliest_number")))
          (function View Earliest_number -> Some () | _ -> None)
          (fun () -> View Earliest_number);
        case
          (Tag 12)
          ~title:"Reconstruct"
          (obj4
             (req "request" (constant "reconstruct"))
             (req "rollup_node_data_dir" string)
             (req "genesis_level" int32)
             (req "finalized_level" int32))
          (function
            | View
                (Reconstruct
                  {rollup_node_data_dir; genesis_level; finalized_level; _}) ->
                Some ((), rollup_node_data_dir, genesis_level, finalized_level)
            | _ -> None)
          (fun _ ->
            assert false
            (* This case cannot be used to decode, which is acceptable because
               the only use case for the encoding is logging (so encoding). *));
        case
          (Tag 13)
          ~title:"Patch_state"
          (obj5
             (req "request" (constant "patch_state"))
             (req "commit" bool)
             (req "key" string)
             (req "value" (string' Hex))
             (opt "block_number" Ethereum_types.quantity_encoding))
          (function
            | View (Patch_state {commit; key; value; block_number}) ->
                Some ((), commit, key, value, block_number)
            | _ -> None)
          (fun ((), commit, key, value, block_number) ->
            View (Patch_state {commit; key; value; block_number}));
        case
          (Tag 14)
          ~title:"Wasm_pvm_version"
          (obj1 (req "request" (constant "wasm_pvm_version")))
          (function View Wasm_pvm_version -> Some () | _ -> None)
          (fun () -> View Wasm_pvm_version);
      ]

  let pp ppf view =
    Data_encoding.Json.pp ppf @@ Data_encoding.Json.construct encoding view
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

  let commit_next_head (ctxt : t) conn evm_state =
    commit
      conn
      ctxt.session.context
      evm_state
      ctxt.session.next_blueprint_number

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

  let apply_evm_event_unsafe on_success ctxt conn evm_state event =
    let open Lwt_result_syntax in
    let open Ethereum_types in
    let*! () = Evm_events_follower_events.new_event event in
    match event with
    | Evm_events.Upgrade_event upgrade ->
        let on_success session =
          session.pending_upgrade <- Some upgrade ;
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
        return (evm_state, on_success)
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
        return (evm_state, on_success)
    | Blueprint_applied {number = Qty number; hash = expected_block_hash} -> (
        let on_success session =
          let (Qty finalized) = session.finalized_number in
          (* We use [max] to not rely on the order of the EVM events (because
             it is possible to see several blueprints applied during on L1
             level). *)
          let new_finalized = Z.(max number finalized) in
          session.finalized_number <- Qty new_finalized ;
          Metrics.set_confirmed_level ~level:new_finalized ;
          on_success session
        in
        let* block_hash_opt =
          if ctxt.block_storage_sqlite3 then
            Evm_store.Blocks.find_hash_of_number conn (Qty number)
          else
            let*! bytes =
              Evm_state.inspect
                evm_state
                (Durable_storage_path.Indexes.block_by_number (Nth number))
            in
            return (Option.map decode_block_hash bytes)
        in
        match block_hash_opt with
        | Some found_block_hash ->
            if found_block_hash = expected_block_hash then
              let*! () =
                Evm_events_follower_events.upstream_blueprint_applied
                  (number, expected_block_hash)
              in
              return (evm_state, on_success)
            else
              let*! () =
                Evm_events_follower_events.diverged
                  (number, expected_block_hash, found_block_hash)
              in
              tzfail
                (Node_error.Diverged
                   (number, expected_block_hash, Some found_block_hash))
        | None when ctxt.fail_on_missing_blueprint ->
            let*! () =
              Evm_events_follower_events.missing_blueprint
                (number, expected_block_hash)
            in
            tzfail (Node_error.Diverged (number, expected_block_hash, None))
        | None ->
            let*! () =
              Evm_events_follower_events.rollup_node_ahead (Qty number)
            in
            return (evm_state, on_success))
    | New_delayed_transaction delayed_transaction ->
        let* evm_state =
          on_new_delayed_transaction ~delayed_transaction evm_state
        in
        return (evm_state, on_success)

  let current_blueprint_number ctxt =
    let (Qty next_blueprint_number) = ctxt.session.next_blueprint_number in
    Ethereum_types.Qty (Z.pred next_blueprint_number)

  let apply_evm_events ?finalized_level (ctxt : t) events =
    let open Lwt_result_syntax in
    let* context, evm_state, on_success =
      with_store_transaction ctxt @@ fun conn ->
      let* on_success, ctxt, evm_state =
        List.fold_left_es
          (fun (on_success, ctxt, evm_state) event ->
            let* evm_state, on_success =
              apply_evm_event_unsafe on_success ctxt conn evm_state event
            in
            return (on_success, ctxt, evm_state))
          (ignore, ctxt, ctxt.session.evm_state)
          events
      in
      let* _ =
        Option.map_es
          (fun l1_level ->
            let l2_level = current_blueprint_number ctxt in
            Evm_store.L1_l2_levels_relationships.store
              conn
              ~latest_l2_level:l2_level
              ~l1_level
              ~finalized_l2_level:ctxt.session.finalized_number)
          finalized_level
      in
      let* ctxt = replace_current_commit ctxt conn evm_state in
      return (ctxt, evm_state, on_success)
    in
    on_success ctxt.session ;
    on_modified_head ctxt evm_state context ;
    let*! head_info in
    head_info := session_to_head_info ctxt.session ;
    return_unit

  type error += Cannot_apply_blueprint of {local_state_level : Z.t}

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
        if Time.Protocol.(upgrade.timestamp <= timestamp) then Some upgrade
        else None

  let check_upgrade ctxt evm_state =
    let open Lwt_result_syntax in
    function
    | Some Evm_events.Upgrade.({hash = root_hash; _} as kernel_upgrade) ->
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
        return_some kernel_upgrade
    | None -> return_none

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
    let next = ctxt.session.next_blueprint_number in
    let* () =
      List.iter_es
        (Evm_store.Delayed_transactions.store conn next)
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
        let number_of_transactions =
          match block.transactions with
          | TxHash l -> List.length l
          | TxFull l -> List.length l
        in
        Metrics.set_block
          ~time_processed:!time_processed
          ~transactions:number_of_transactions ;
        let* () =
          Evm_store.Blueprints.store
            conn
            {number = block.number; timestamp; payload}
        in

        let* () =
          if ctxt.block_storage_sqlite3 then
            store_block_unsafe conn evm_state block
          else return_unit
        in

        let upgrade_candidate = check_pending_upgrade ctxt timestamp in
        let* kernel_upgrade = check_upgrade ctxt evm_state upgrade_candidate in

        let* () =
          when_ Option.(is_some kernel_upgrade) @@ fun () ->
          Evm_store.Kernel_upgrades.record_apply
            conn
            ctxt.session.next_blueprint_number
        in

        let* context = commit_next_head ctxt conn evm_state in
        return (evm_state, context, block.hash, kernel_upgrade)
    | Apply_failure (* Did not produce a block *) ->
        let (Qty next) = next in
        let*! () = Blueprint_events.invalid_blueprint_produced next in
        tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

  let on_new_head ctxt ~applied_upgrade evm_state context block_hash
      blueprint_with_events =
    let open Lwt_syntax in
    let (Qty level) = ctxt.session.next_blueprint_number in
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context ;
    ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
    ctxt.session.current_block_hash <- block_hash ;
    Blueprints_watcher.notify blueprint_with_events ;
    if applied_upgrade then ctxt.session.pending_upgrade <- None ;
    let* head_info in
    head_info := session_to_head_info ctxt.session ;
    Metrics.set_level ~level ;
    Blueprint_events.blueprint_applied (level, block_hash)

  let apply_blueprint ctxt timestamp payload delayed_transactions =
    let open Lwt_result_syntax in
    let* evm_state, context, current_block_hash, kernel_upgrade =
      with_store_transaction ctxt @@ fun conn ->
      apply_blueprint_store_unsafe
        ctxt
        conn
        timestamp
        payload
        delayed_transactions
    in
    let*! () =
      on_new_head
        ctxt
        ~applied_upgrade:Option.(is_some kernel_upgrade)
        evm_state
        context
        current_block_hash
        {
          delayed_transactions;
          kernel_upgrade;
          blueprint =
            {number = ctxt.session.next_blueprint_number; timestamp; payload};
        }
    in
    return_unit

  type error +=
    | Invalid_rollup_node of {
        smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
        rollup_node_smart_rollup_address :
          Tezos_crypto.Hashed.Smart_rollup_address.t;
      }

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

  let init ?kernel_path ~block_storage_sqlite3 ~fail_on_missing_blueprint
      ~data_dir ~preimages ~preimages_endpoint ?smart_rollup_address ~store_perm
      () =
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
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    let* latest_relationship = Evm_store.L1_l2_levels_relationships.find conn in
    let finalized_number =
      match latest_relationship with
      | None -> Ethereum_types.Qty Z.zero
      | Some {finalized; _} -> finalized
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
          };
        store;
        fail_on_missing_blueprint;
        block_storage_sqlite3;
      }
    in

    let*! () =
      Option.iter_s
        (fun upgrade -> Events.pending_upgrade upgrade)
        pending_upgrade
    in

    return (ctxt, (init_status, smart_rollup_address))

  let reset ~data_dir ~l2_level =
    let open Lwt_result_syntax in
    let* store = Evm_store.init ~data_dir ~perm:`Read_write () in
    Evm_store.use store @@ fun conn ->
    Evm_store.with_transaction conn @@ fun store ->
    Evm_store.reset store ~l2_level

  let last_produced_blueprint (ctxt : t) =
    let open Lwt_result_syntax in
    let (Qty next) = ctxt.session.next_blueprint_number in
    let current = Ethereum_types.Qty Z.(pred next) in
    Evm_store.use ctxt.store @@ fun conn ->
    let* blueprint = Evm_store.Blueprints.find conn current in
    match blueprint with
    | Some blueprint -> return blueprint
    | None -> failwith "Could not fetch the last produced blueprint"

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

  let block_number_of_hash evm_state hash =
    let open Lwt_result_syntax in
    let*! bytes =
      Evm_state.inspect evm_state (Durable_storage_path.Block.by_hash hash)
    in
    let block = Option.map Ethereum_types.block_from_rlp bytes in
    return (Option.map (fun block -> block.Ethereum_types.number) block)

  let blueprint_with_events ctxt level =
    let open Lwt_result_syntax in
    Evm_store.use ctxt.store @@ fun conn ->
    let* blueprint = Evm_store.Blueprints.find conn level in
    let* kernel_upgrade =
      Evm_store.Kernel_upgrades.find_applied_before conn level
    in
    match blueprint with
    | None -> return None
    | Some blueprint ->
        let* delayed_transactions =
          Evm_store.Delayed_transactions.at_level conn level
        in
        return_some
          Blueprint_types.{delayed_transactions; kernel_upgrade; blueprint}

  let messages_of_level ~levels_to_hashes ~l2_blocks ~messages level =
    let open Lwt_result_syntax in
    let open Rollup_node_storage in
    let* hash = Levels_to_hashes.find levels_to_hashes level in
    let*? hash =
      Option.to_result
        ~none:[error_of_fmt "Block hash is not found for level %ld" level]
        hash
    in
    let* block = L2_blocks.read l2_blocks hash in
    let*? _, header =
      Option.to_result
        ~none:[error_of_fmt "Block is not found for hash %a" Block_hash.pp hash]
        block
    in
    let* messages = Messages.read messages header.inbox_witness in
    let*? messages =
      Option.to_result
        ~none:[error_of_fmt "No messages found for block %a" Block_hash.pp hash]
        messages
    in
    return (fst messages)

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

  let reconstruct_history ctxt ~rollup_node_data_dir ~genesis_level
      ~finalized_level ~levels_to_hashes ~l2_blocks =
    let open Lwt_result_syntax in
    (* Smart Rollups do not process messages of genesis level. *)
    let first_level = Int32.succ genesis_level in
    assert (finalized_level > first_level) ;

    let* messages =
      Rollup_node_storage.load_messages ~rollup_node_data_dir ()
    in

    let messages_of_level =
      messages_of_level ~levels_to_hashes ~l2_blocks ~messages
    in

    let config = pvm_config ctxt in
    let rec go ~count_progress ~current_block_number evm_state tezos_level =
      if tezos_level > finalized_level then return_unit
      else
        let*! () = count_progress 1 in
        let* messages = messages_of_level tezos_level in
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
        State.apply_evm_events ?finalized_level ctxt events
    | Apply_blueprint {timestamp; payload; delayed_transactions} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.apply_blueprint ctxt timestamp payload delayed_transactions
    | Last_produce_blueprint ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.last_produced_blueprint ctxt
    | Blueprint {level} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.blueprint_with_events ctxt level
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
    | New_last_known_L1_level l1_level ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        let l2_level = State.current_blueprint_number ctxt in
        Evm_store.L1_l2_levels_relationships.store
          conn
          ~latest_l2_level:l2_level
          ~l1_level
          ~finalized_l2_level:ctxt.session.finalized_number
    | Delayed_inbox_hashes ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        let*! hashes = State.delayed_inbox_hashes ctxt.session.evm_state in
        return hashes
    | Evm_state_after block_request -> (
        protect @@ fun () ->
        let ctxt = Worker.state self in
        let* number =
          match block_request with
          | Number number -> return number
          | Hash hash -> (
              let* number =
                State.block_number_of_hash ctxt.session.evm_state hash
              in
              match number with
              | Some number -> return number
              | None ->
                  (* To respect EIP-1898 we can return error code -32001. *)
                  failwith "Block was not found")
        in
        Evm_store.use ctxt.store @@ fun conn ->
        let* checkpoint = Evm_store.Context_hashes.find conn number in
        match checkpoint with
        | Some checkpoint ->
            let*! context = Irmin_context.checkout_exn ctxt.index checkpoint in
            let*! evm_state = Irmin_context.PVMState.get context in
            return_some evm_state
        | None -> return_none)
    | Earliest_state -> (
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        let* checkpoint = Evm_store.Context_hashes.find_earliest conn in
        match checkpoint with
        | Some (_level, checkpoint) ->
            let*! context = Irmin_context.checkout_exn ctxt.index checkpoint in
            let*! evm_state = Irmin_context.PVMState.get context in
            return_some evm_state
        | None -> return_none)
    | Earliest_number -> (
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        let* checkpoint = Evm_store.Context_hashes.find_earliest conn in
        match checkpoint with
        | Some (level, _checkpoint) -> return_some level
        | None -> return_none)
    | Reconstruct
        {
          rollup_node_data_dir;
          genesis_level;
          finalized_level;
          levels_to_hashes;
          l2_blocks;
        } ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.reconstruct_history
          ctxt
          ~rollup_node_data_dir
          ~genesis_level
          ~finalized_level
          ~levels_to_hashes
          ~l2_blocks
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

  let on_error (type a b) _self _st (req : (a, b) Request.t) (errs : b) :
      unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with
    | Apply_evm_events _, [Node_error.Diverged _divergence] ->
        Lwt_exit.exit_and_raise Node_error.exit_code_when_diverge
    | _ -> return_unit
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
    ~block_storage_sqlite3 () =
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
      }
      (module Handlers)
  in
  let*! () = Blueprint_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  let*! init_status in
  let*! head_info in
  let (Qty finalized_number) = !head_info.finalized_number in
  let (Qty next_blueprint_number) = !head_info.next_blueprint_number in
  Metrics.set_level ~level:(Z.pred next_blueprint_number) ;
  Metrics.set_confirmed_level ~level:finalized_number ;
  let*! () = Evm_context_events.ready () in
  return init_status

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

let rollup_node_block_storage ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let open Rollup_node_storage in
  let* last_finalized_level, levels_to_hashes, l2_blocks =
    Rollup_node_storage.load ~rollup_node_data_dir ()
  in
  let* final_level = Last_finalized_level.read last_finalized_level in
  let*? final_level =
    Option.to_result
      ~none:
        [error_of_fmt "Rollup node storage is missing the last finalized level"]
      final_level
  in
  return (final_level, levels_to_hashes, l2_blocks)

let init_context_from_rollup_node ~data_dir ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let open Rollup_node_storage in
  let* final_level, levels_to_hashes, l2_blocks =
    rollup_node_block_storage ~rollup_node_data_dir
  in
  let* final_level_hash = Levels_to_hashes.find levels_to_hashes final_level in
  let*? final_level_hash =
    Option.to_result
      ~none:
        [
          error_of_fmt
            "Rollup node has no block hash for the l1 level %ld"
            final_level;
        ]
      final_level_hash
  in
  let* final_l2_block = L2_blocks.read l2_blocks final_level_hash in
  let* checkpoint =
    match final_l2_block with
    | Some Sc_rollup_block.(_, {context; _}) ->
        Irmin_context.hash_of_context_hash context |> return
    | None ->
        failwith
          "Rollup node has no l2 blocks for the l1 block hash %a"
          Block_hash.pp
          final_level_hash
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
        final_level
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
  return (evm_node_context, evm_state, final_level)

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
  let* finalized_level, levels_to_hashes, l2_blocks =
    rollup_node_block_storage ~rollup_node_data_dir
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
    (Reconstruct
       {
         rollup_node_data_dir;
         genesis_level;
         finalized_level;
         levels_to_hashes;
         l2_blocks;
       })

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

let apply_blueprint timestamp payload delayed_transactions =
  worker_wait_for_request
    (Apply_blueprint {timestamp; payload; delayed_transactions})

let last_produced_blueprint () = worker_wait_for_request Last_produce_blueprint

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

let new_last_known_l1_level l =
  worker_add_request ~request:(New_last_known_L1_level l)

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

let block_param_to_block_number
    (block_param : Ethereum_types.Block_parameter.extended) =
  let open Lwt_result_syntax in
  match block_param with
  | Block_parameter (Number n) -> return n
  | Block_parameter (Latest | Pending) ->
      let*! {next_blueprint_number = Qty next_number; _} = head_info () in
      return Ethereum_types.(Qty Z.(pred next_number))
  | Block_parameter Finalized ->
      let*! {finalized_number; _} = head_info () in
      return finalized_number
  | Block_parameter Earliest -> (
      let* res = worker_wait_for_request Earliest_number in
      match res with
      | Some res -> return res
      | None -> failwith "The EVM node does not have any state available")
  | Block_hash {hash; _} -> (
      let*! head_info = head_info () in
      let*! bytes =
        Evm_state.inspect
          head_info.evm_state
          Durable_storage_path.(Block.by_hash hash)
      in
      match bytes with
      | Some bytes ->
          let block = Ethereum_types.block_from_rlp bytes in
          return block.number
      | None -> failwith "Missing block %a" Ethereum_types.pp_block_hash hash)

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_context_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
