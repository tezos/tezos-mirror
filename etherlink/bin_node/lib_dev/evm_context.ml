(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type init_status = Loaded | Created

type head = {
  current_block_hash : Ethereum_types.block_hash;
  next_blueprint_number : Ethereum_types.quantity;
  evm_state : Evm_state.t;
}

type parameters = {
  kernel_path : string option;
  data_dir : string;
  preimages : string;
  preimages_endpoint : Uri.t option;
  smart_rollup_address : string;
}

type session_state = {
  mutable context : Irmin_context.rw;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  mutable pending_upgrade : Ethereum_types.Upgrade.t option;
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
}

let blueprint_watcher : Blueprint_types.t Lwt_watcher.input =
  Lwt_watcher.create_input ()

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
  type (_, _) t =
    | Apply_evm_events : {
        finalized_level : int32;
        events : Ethereum_types.Evm_events.t list;
      }
        -> (unit, tztrace) t
    | Apply_blueprint : {
        timestamp : Time.Protocol.t;
        payload : Blueprint_types.payload;
      }
        -> (unit, tztrace) t
    | Apply_sequencer_blueprint : {
        timestamp : Time.Protocol.t;
        blueprint : Sequencer_blueprint.t;
      }
        -> (unit, tztrace) t
    | Last_produce_blueprint : (Blueprint_types.t, tztrace) t
    | Head_info : (head, tztrace) t
    | Blueprint : {
        level : Ethereum_types.quantity;
      }
        -> (Blueprint_types.t option, tztrace) t
    | Blueprints_range : {
        from : Ethereum_types.quantity;
        to_ : Ethereum_types.quantity;
      }
        -> ((Ethereum_types.quantity * Blueprint_types.payload) list, tztrace) t
    | Last_known_L1_level : (int32 option, tztrace) t
    | New_last_known_L1_level : int32 -> (unit, tztrace) t
    | Delayed_inbox_hashes : (Ethereum_types.hash list, tztrace) t

  type view = View : _ t -> view

  let view req = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Apply_evm_events"
          (obj3
             (req "request" (constant "apply_evm_events"))
             (req "finalized_level" int32)
             (req "events" (list Ethereum_types.Evm_events.encoding)))
          (function
            | View (Apply_evm_events {finalized_level; events}) ->
                Some ((), finalized_level, events)
            | _ -> None)
          (fun ((), finalized_level, events) ->
            View (Apply_evm_events {finalized_level; events}));
        case
          (Tag 1)
          ~title:"Apply_blueprint"
          (obj3
             (req "request" (constant "apply_blueprint"))
             (req "timestamp" Time.Protocol.encoding)
             (req "payload" Blueprint_types.payload_encoding))
          (function
            | View (Apply_blueprint {timestamp; payload}) ->
                Some ((), timestamp, payload)
            | _ -> None)
          (fun ((), timestamp, payload) ->
            View (Apply_blueprint {timestamp; payload}));
        case
          (Tag 2)
          ~title:"Apply_sequencer_blueprint"
          (obj3
             (req "request" (constant "apply_sequencer_blueprint"))
             (req "timestamp" Time.Protocol.encoding)
             (req "blueprint" Blueprint_types.payload_encoding))
          (function
            | View (Apply_sequencer_blueprint {timestamp; blueprint}) ->
                Some ((), timestamp, blueprint)
            | _ -> None)
          (fun ((), timestamp, blueprint) ->
            View (Apply_sequencer_blueprint {timestamp; blueprint}));
        case
          (Tag 3)
          ~title:"Last_produce_blueprint"
          (obj1 (req "request" (constant "last_produce_blueprint")))
          (function View Last_produce_blueprint -> Some () | _ -> None)
          (fun () -> View Last_produce_blueprint);
        case
          (Tag 4)
          ~title:"Head_info"
          (obj1 (req "request" (constant "head_info")))
          (function View Head_info -> Some () | _ -> None)
          (fun () -> View Head_info);
        case
          (Tag 5)
          ~title:"Blueprint"
          (obj2
             (req "request" (constant "blueprint"))
             (req "level" Ethereum_types.quantity_encoding))
          (function View (Blueprint {level}) -> Some ((), level) | _ -> None)
          (fun ((), level) -> View (Blueprint {level}));
        case
          (Tag 6)
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
          (Tag 7)
          ~title:"Last_known_L1_level"
          (obj1 (req "request" (constant "last_known_l1_level")))
          (function View Last_known_L1_level -> Some () | _ -> None)
          (fun () -> View Last_known_L1_level);
        case
          (Tag 8)
          ~title:"New_last_known_L1_level"
          (obj2
             (req "request" (constant "new_last_known_l1_level"))
             (req "value" int32))
          (function
            | View (New_last_known_L1_level l) -> Some ((), l) | _ -> None)
          (fun ((), l) -> View (New_last_known_L1_level l));
        case
          (Tag 9)
          ~title:"Delayed_inbox_hashes"
          (obj1 (req "request" (constant "Delayed_inbox_hashes")))
          (function View Delayed_inbox_hashes -> Some () | _ -> None)
          (fun () -> View Delayed_inbox_hashes);
      ]

  let pp ppf view =
    Data_encoding.Json.pp ppf @@ Data_encoding.Json.construct encoding view
end

let init_status, init_status_waker = Lwt.task ()

let execution_config, execution_config_waker = Lwt.task ()

module State = struct
  let with_store_transaction ctxt k =
    Evm_store.with_transaction ctxt.store (fun txn_store ->
        k {ctxt with store = txn_store})

  let store_path ~data_dir = Filename.Infix.(data_dir // "store")

  let load ~data_dir index =
    let open Lwt_result_syntax in
    let* store = Evm_store.init ~data_dir in
    let* latest = Evm_store.Context_hashes.find_latest store in
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

  let commit store context evm_state number =
    let open Lwt_result_syntax in
    let*! context = Irmin_context.PVMState.set context evm_state in
    let*! checkpoint = Irmin_context.commit context in
    let* () = Evm_store.Context_hashes.store store number checkpoint in
    return context

  let commit_next_head (ctxt : t) evm_state =
    commit
      ctxt.store
      ctxt.session.context
      evm_state
      ctxt.session.next_blueprint_number

  let replace_current_commit (ctxt : t) evm_state =
    let (Qty next) = ctxt.session.next_blueprint_number in
    commit ctxt.store ctxt.session.context evm_state (Qty Z.(pred next))

  let inspect ctxt path =
    let open Lwt_syntax in
    let* res = Evm_state.inspect ctxt.session.evm_state path in
    return res

  let on_modified_head ctxt evm_state context =
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context

  let apply_evm_event_unsafe on_success ctxt evm_state event =
    let open Lwt_result_syntax in
    let open Ethereum_types in
    let*! () = Evm_events_follower_events.new_event event in
    match event with
    | Evm_events.Upgrade_event upgrade ->
        let on_success session =
          session.pending_upgrade <- Some upgrade ;
          on_success session
        in
        let payload =
          Ethereum_types.Upgrade.to_bytes upgrade |> String.of_bytes
        in
        let*! evm_state =
          Evm_state.modify
            ~key:Durable_storage_path.kernel_upgrade
            ~value:payload
            evm_state
        in
        let* () =
          Evm_store.Kernel_upgrades.store
            ctxt.store
            ctxt.session.next_blueprint_number
            upgrade
        in
        let*! () = Events.pending_upgrade upgrade in
        return (evm_state, on_success)
    | Sequencer_upgrade_event sequencer_upgrade ->
        let payload =
          Sequencer_upgrade.to_bytes sequencer_upgrade |> String.of_bytes
        in
        let*! evm_state =
          Evm_state.modify
            ~key:Durable_storage_path.sequencer_upgrade
            ~value:payload
            evm_state
        in
        return (evm_state, on_success)
    | Blueprint_applied {number = Qty number; hash = expected_block_hash} -> (
        let* block_hash_opt =
          let*! bytes =
            inspect
              ctxt
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
        | None ->
            let*! () =
              Evm_events_follower_events.missing_block
                (number, expected_block_hash)
            in
            tzfail (Node_error.Diverged (number, expected_block_hash, None)))
    | New_delayed_transaction delayed_transaction ->
        let*! data_dir, config = execution_config in
        let* evm_state =
          Evm_state.execute
            ~data_dir
            ~config
            evm_state
            [
              `Input
                ("\254"
                ^ Bytes.to_string
                    (Delayed_transaction.to_rlp delayed_transaction));
            ]
        in
        return (evm_state, on_success)

  let apply_evm_events ~finalized_level (ctxt : t) events =
    let open Lwt_result_syntax in
    let* context, evm_state, on_success =
      with_store_transaction ctxt @@ fun ctxt ->
      let* on_success, ctxt, evm_state =
        List.fold_left_es
          (fun (on_success, ctxt, evm_state) event ->
            let* evm_state, on_success =
              apply_evm_event_unsafe on_success ctxt evm_state event
            in
            return (on_success, ctxt, evm_state))
          (ignore, ctxt, ctxt.session.evm_state)
          events
      in
      let* _ =
        Evm_store.L1_latest_known_level.store ctxt.store finalized_level
      in
      let* ctxt = replace_current_commit ctxt evm_state in
      return (ctxt, evm_state, on_success)
    in
    on_modified_head ctxt evm_state context ;
    on_success ctxt.session ;
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
        if Time.Protocol.(upgrade.timestamp <= timestamp) then Some upgrade.hash
        else None

  let check_upgrade ctxt evm_state =
    let open Lwt_result_syntax in
    function
    | Some root_hash ->
        let* () =
          Evm_store.Kernel_upgrades.record_apply
            ctxt.store
            ctxt.session.next_blueprint_number
        in

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

        return_true
    | None -> return_false

  (** [apply_blueprint_store_unsafe ctxt payload] applies the blueprint [payload]
    on the head of [ctxt], and commit the resulting state to Irmin and the
    node’s store.

    However, it does not modifies [ctxt] to make it aware of the new state.
    This is because [apply_blueprint_store_unsafe] is expected to be called
    within a SQL transaction to make sure the node’s store is not left in an
    inconsistent state in case of error. *)
  let apply_blueprint_store_unsafe ctxt timestamp payload =
    let open Lwt_result_syntax in
    Evm_store.assert_in_transaction ctxt.store ;
    let*! data_dir, config = execution_config in
    let (Qty next) = ctxt.session.next_blueprint_number in

    let* try_apply =
      Evm_state.apply_blueprint ~data_dir ~config ctxt.session.evm_state payload
    in

    match try_apply with
    | Apply_success
        (evm_state, Block_height blueprint_number, current_block_hash)
      when Z.equal blueprint_number next ->
        let* () =
          Evm_store.Blueprints.store
            ctxt.store
            {number = Qty blueprint_number; timestamp; payload}
        in

        let root_hash_candidate = check_pending_upgrade ctxt timestamp in
        let* applied_upgrade =
          check_upgrade ctxt evm_state root_hash_candidate
        in

        let* context = commit_next_head ctxt evm_state in
        return (evm_state, context, current_block_hash, applied_upgrade)
    | Apply_success _ (* Produced a block, but not of the expected height *)
    | Apply_failure (* Did not produce a block *) ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6826 *)
        let*! () = Blueprint_events.invalid_blueprint_produced next in
        tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

  let on_new_head ctxt ~applied_upgrade evm_state timestamp context block_hash
      payload =
    let (Qty level) = ctxt.session.next_blueprint_number in
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context ;
    ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
    ctxt.session.current_block_hash <- block_hash ;
    Lwt_watcher.notify
      blueprint_watcher
      {number = Qty level; timestamp; payload} ;
    if applied_upgrade then ctxt.session.pending_upgrade <- None ;
    Blueprint_events.blueprint_applied (level, block_hash)

  let apply_sequencer_blueprint (ctxt : t) timestamp
      (blueprint : Sequencer_blueprint.t) =
    let open Lwt_result_syntax in
    let* evm_state, context, current_block_hash, applied_upgrade =
      with_store_transaction ctxt @@ fun ctxt ->
      let* current_block_hash =
        apply_blueprint_store_unsafe ctxt timestamp blueprint
      in
      return current_block_hash
    in
    let*! () =
      on_new_head
        ctxt
        ~applied_upgrade
        evm_state
        timestamp
        context
        current_block_hash
        blueprint
    in
    return_unit

  let apply_blueprint ctxt timestamp payload =
    let open Lwt_result_syntax in
    let* evm_state, context, current_block_hash, applied_upgrade =
      with_store_transaction ctxt @@ fun ctxt ->
      apply_blueprint_store_unsafe ctxt timestamp payload
    in
    let*! () =
      on_new_head
        ctxt
        ~applied_upgrade
        evm_state
        timestamp
        context
        current_block_hash
        payload
    in
    return_unit

  let init ?kernel_path ~data_dir ~preimages ~preimages_endpoint
      ~smart_rollup_address () =
    let open Lwt_result_syntax in
    let*! () =
      Lwt_utils_unix.create_dir (Evm_state.kernel_logs_directory ~data_dir)
    in
    let* index =
      Irmin_context.load ~cache_size:100_000 Read_write (store_path ~data_dir)
    in
    let destination =
      Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn
        smart_rollup_address
    in
    let* store, context, next_blueprint_number, current_block_hash, init_status
        =
      load ~data_dir index
    in
    let* pending_upgrade =
      Evm_store.Kernel_upgrades.find_latest_pending store
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
            let* context = commit store context evm_state (Qty Z.(pred next)) in
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
        smart_rollup_address = destination;
        session =
          {
            context;
            next_blueprint_number;
            current_block_hash;
            pending_upgrade;
            evm_state;
          };
        store;
      }
    in

    let*! () =
      Option.iter_s
        (fun upgrade -> Events.pending_upgrade upgrade)
        pending_upgrade
    in

    return (ctxt, init_status)

  let init_from_rollup_node ~data_dir ~rollup_node_data_dir =
    let open Lwt_result_syntax in
    let* Sc_rollup_block.(_, {context; _}) =
      let open Rollup_node_storage in
      let* last_finalized_level, levels_to_hashes, l2_blocks =
        Rollup_node_storage.load ~rollup_node_data_dir ()
      in
      let* final_level = Last_finalized_level.read last_finalized_level in
      let*? final_level =
        Option.to_result
          ~none:
            [
              error_of_fmt
                "Rollup node storage is missing the last finalized level";
            ]
          final_level
      in
      let* final_level_hash =
        Levels_to_hashes.find levels_to_hashes final_level
      in
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
      Lwt.return
      @@ Option.to_result
           ~none:
             [
               error_of_fmt
                 "Rollup node has no l2 blocks for the l1 block hash %a"
                 Block_hash.pp
                 final_level_hash;
             ]
           final_l2_block
    in
    let checkpoint =
      Smart_rollup_context_hash.to_bytes context |> Context_hash.of_bytes_exn
    in
    let rollup_node_context_dir =
      Filename.Infix.(rollup_node_data_dir // "context")
    in
    let* rollup_node_index =
      Irmin_context.load ~cache_size:100_000 Read_only rollup_node_context_dir
    in
    let evm_context_dir = store_path ~data_dir in
    let*! () = Lwt_utils_unix.create_dir evm_context_dir in
    let* () =
      Irmin_context.export_snapshot
        rollup_node_index
        checkpoint
        ~path:evm_context_dir
    in
    let* evm_node_index =
      Irmin_context.load ~cache_size:100_000 Read_write evm_context_dir
    in
    let*! evm_node_context =
      Irmin_context.checkout_exn evm_node_index checkpoint
    in
    let*! evm_state = Irmin_context.PVMState.get evm_node_context in

    (* Tell the kernel that it is executed by an EVM node *)
    let*! evm_state = Evm_state.flag_local_exec evm_state in
    (* We remove the delayed inbox from the EVM state. Its contents will be
       retrieved by the sequencer by inspecting the evm events. *)
    let*! evm_state = Evm_state.clear_delayed_inbox evm_state in

    (* For changes made to [evm_state] to take effect, we commit the result *)
    let*! evm_node_context =
      Irmin_context.PVMState.set evm_node_context evm_state
    in
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
    let* store = Evm_store.init ~data_dir in
    let* () =
      Evm_store.Context_hashes.store
        store
        (Qty current_blueprint_number)
        checkpoint
    in
    return_unit

  let last_produced_blueprint (ctxt : t) =
    let open Lwt_result_syntax in
    let (Qty next) = ctxt.session.next_blueprint_number in
    let current = Ethereum_types.Qty Z.(pred next) in
    let* blueprint = Evm_store.Blueprints.find ctxt.store current in
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
        smart_rollup_address : string;
      } =
    let open Lwt_result_syntax in
    let* ctxt, status =
      State.init
        ?kernel_path
        ~data_dir
        ~preimages
        ~preimages_endpoint
        ~smart_rollup_address
        ()
    in
    Lwt.wakeup execution_config_waker
    @@ ( ctxt.data_dir,
         Config.config
           ~preimage_directory:ctxt.preimages
           ?preimage_endpoint:ctxt.preimages_endpoint
           ~kernel_debug:true
           ~destination:ctxt.smart_rollup_address
           () ) ;
    Lwt.wakeup init_status_waker status ;
    return ctxt

  let on_request :
      type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_result_syntax in
    match request with
    | Apply_evm_events {finalized_level; events} ->
        let ctxt = Worker.state self in
        State.apply_evm_events ~finalized_level ctxt events
    | Apply_blueprint {timestamp; payload} ->
        let ctxt = Worker.state self in
        State.apply_blueprint ctxt timestamp payload
    | Apply_sequencer_blueprint {timestamp; blueprint} ->
        let ctxt = Worker.state self in
        State.apply_sequencer_blueprint ctxt timestamp blueprint
    | Last_produce_blueprint ->
        let ctxt = Worker.state self in
        State.last_produced_blueprint ctxt
    | Head_info ->
        let ctxt = Worker.state self in
        return
          {
            next_blueprint_number = ctxt.session.next_blueprint_number;
            current_block_hash = ctxt.session.current_block_hash;
            evm_state = ctxt.session.evm_state;
          }
    | Blueprint {level} ->
        let ctxt = Worker.state self in
        Evm_store.Blueprints.find ctxt.store level
    | Blueprints_range {from; to_} ->
        let ctxt = Worker.state self in
        Evm_store.Blueprints.find_range ctxt.store ~from ~to_
    | Last_known_L1_level ->
        let ctxt = Worker.state self in
        Evm_store.L1_latest_known_level.find ctxt.store
    | New_last_known_L1_level l ->
        let ctxt = Worker.state self in
        Evm_store.L1_latest_known_level.store ctxt.store l
    | Delayed_inbox_hashes ->
        let ctxt = Worker.state self in
        let*! hashes = State.delayed_inbox_hashes ctxt.session.evm_state in
        return hashes

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

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> Ok worker
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

let start ?kernel_path ~data_dir ~preimages ~preimages_endpoint
    ~smart_rollup_address () =
  let open Lwt_result_syntax in
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
      }
      (module Handlers)
  in
  let*! () = Blueprint_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  let*! init_status in
  let*! () = Evm_context_events.ready () in
  return init_status

let init_from_rollup_node = State.init_from_rollup_node

let apply_evm_events ~finalized_level events =
  worker_add_request ~request:(Apply_evm_events {finalized_level; events})

let apply_blueprint timestamp payload =
  worker_wait_for_request (Apply_blueprint {timestamp; payload})

let apply_sequencer_blueprint timestamp blueprint =
  worker_wait_for_request (Apply_sequencer_blueprint {timestamp; blueprint})

let last_produced_blueprint () = worker_wait_for_request Last_produce_blueprint

let head_info () = worker_wait_for_request Head_info

let execute_and_inspect ?wasm_entrypoint input =
  let open Lwt_result_syntax in
  let* {evm_state; _} = head_info () in
  let*! data_dir, config = execution_config in
  Evm_state.execute_and_inspect
    ~data_dir
    ?wasm_entrypoint
    ~config
    ~input
    evm_state

let inspect path =
  let open Lwt_result_syntax in
  let* {evm_state; _} = head_info () in
  let*! res = Evm_state.inspect evm_state path in
  return res

let blueprints_watcher () = Lwt_watcher.create_stream blueprint_watcher

let blueprint level = worker_wait_for_request (Blueprint {level})

let blueprints_range from to_ =
  worker_wait_for_request (Blueprints_range {from; to_})

let last_known_l1_level () = worker_wait_for_request Last_known_L1_level

let new_last_known_l1_level l =
  worker_add_request ~request:(New_last_known_L1_level l)

let delayed_inbox_hashes () = worker_wait_for_request Delayed_inbox_hashes

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_context_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
