(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type session_state = {
  mutable context : Irmin_context.rw;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  mutable pending_upgrade : Ethereum_types.Upgrade.t option;
}

type t = {
  data_dir : string;
  index : Irmin_context.rw_index;
  preimages : string;
  preimages_endpoint : Uri.t option;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  blueprint_watcher : Blueprint_types.t Lwt_watcher.input;
  store : Store.t;
  session : session_state;
  head_lock : Lwt_mutex.t;
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7073
         [head_lock] is only necessary because several workers can modify the
         current HEAD of the chain *)
}

let with_store_transaction ctxt k =
  Store.with_transaction ctxt.store (fun txn_store ->
      k {ctxt with store = txn_store})

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let load ~data_dir index =
  let open Lwt_result_syntax in
  let* store = Store.init ~data_dir in
  let* latest = Store.Context_hashes.find_latest store in
  match latest with
  | Some (Qty latest_blueprint_number, checkpoint) ->
      let*! context = Irmin_context.checkout_exn index checkpoint in
      let*! evm_state = Irmin_context.PVMState.get context in
      let+ current_block_hash = Evm_state.current_block_hash evm_state in
      ( store,
        context,
        Ethereum_types.Qty Z.(succ latest_blueprint_number),
        current_block_hash,
        true )
  | None ->
      let context = Irmin_context.empty index in
      return
        ( store,
          context,
          Ethereum_types.Qty Z.zero,
          Ethereum_types.genesis_parent_hash,
          false )

let commit_next_head (ctxt : t) evm_state =
  let open Lwt_result_syntax in
  let*! context = Irmin_context.PVMState.set ctxt.session.context evm_state in
  let*! checkpoint = Irmin_context.commit context in
  let* () =
    Store.Context_hashes.store
      ctxt.store
      ctxt.session.next_blueprint_number
      checkpoint
  in
  return context

let replace_current_commit (ctxt : t) evm_state =
  let open Lwt_result_syntax in
  let (Qty next) = ctxt.session.next_blueprint_number in
  let*! context = Irmin_context.PVMState.set ctxt.session.context evm_state in
  let*! checkpoint = Irmin_context.commit context in
  let* () =
    Store.Context_hashes.store ctxt.store (Qty Z.(pred next)) checkpoint
  in
  return context

let evm_state ctxt = Irmin_context.PVMState.get ctxt.session.context

let inspect ctxt path =
  let open Lwt_syntax in
  let* evm_state = evm_state ctxt in
  Evm_state.inspect evm_state path

let on_modified_head ctxt context = ctxt.session.context <- context

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
        Store.Kernel_upgrades.store
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

let apply_evm_events ~finalized_level (ctxt : t) events =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock ctxt.head_lock @@ fun () ->
  let* context, on_success =
    with_store_transaction ctxt @@ fun ctxt ->
    let*! evm_state = evm_state ctxt in
    let* on_success, ctxt, evm_state =
      List.fold_left_es
        (fun (on_success, ctxt, evm_state) event ->
          let* evm_state, on_success =
            apply_evm_event_unsafe on_success ctxt evm_state event
          in
          return (on_success, ctxt, evm_state))
        (ignore, ctxt, evm_state)
        events
    in
    let* _ = Store.L1_latest_known_level.store ctxt.store finalized_level in
    let* ctxt = replace_current_commit ctxt evm_state in
    return (ctxt, on_success)
  in
  on_modified_head ctxt context ;
  on_success ctxt.session ;
  return_unit

let execution_config ctxt =
  Config.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_prod_cannot_apply_blueprint"
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
        Store.Kernel_upgrades.record_apply
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
            Events.applied_upgrade root_hash ctxt.session.next_blueprint_number
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
  Store.assert_in_transaction ctxt.store ;
  let*! evm_state = evm_state ctxt in
  let config = execution_config ctxt in
  let (Qty next) = ctxt.session.next_blueprint_number in

  let* try_apply = Evm_state.apply_blueprint ~config evm_state payload in

  match try_apply with
  | Apply_success (evm_state, Block_height blueprint_number, current_block_hash)
    when Z.equal blueprint_number next ->
      let* () =
        Store.Executable_blueprints.store
          ctxt.store
          {number = Qty blueprint_number; timestamp; payload}
      in

      let root_hash_candidate = check_pending_upgrade ctxt timestamp in
      let* applied_upgrade = check_upgrade ctxt evm_state root_hash_candidate in

      let* context = commit_next_head ctxt evm_state in
      return (context, current_block_hash, applied_upgrade)
  | Apply_success _ (* Produced a block, but not of the expected height *)
  | Apply_failure (* Did not produce a block *) ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6826 *)
      let*! () = Blueprint_events.invalid_blueprint_produced next in
      tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

let on_new_head ctxt ~applied_upgrade timestamp context block_hash payload =
  let (Qty level) = ctxt.session.next_blueprint_number in
  ctxt.session.context <- context ;
  ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
  ctxt.session.current_block_hash <- block_hash ;
  Lwt_watcher.notify
    ctxt.blueprint_watcher
    {number = Qty level; timestamp; payload} ;
  if applied_upgrade then ctxt.session.pending_upgrade <- None ;
  Blueprint_events.blueprint_applied (level, block_hash)

let apply_and_publish_blueprint (ctxt : t) timestamp
    (blueprint : Sequencer_blueprint.t) =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock ctxt.head_lock @@ fun () ->
  let (Qty level) = ctxt.session.next_blueprint_number in
  let* context, current_block_hash, applied_upgrade =
    with_store_transaction ctxt @@ fun ctxt ->
    let* current_block_hash =
      apply_blueprint_store_unsafe ctxt timestamp blueprint.to_execute
    in
    let* () =
      Store.Publishable_blueprints.store
        ctxt.store
        (Qty level)
        blueprint.to_publish
    in
    return current_block_hash
  in
  let*! () =
    on_new_head
      ctxt
      ~applied_upgrade
      timestamp
      context
      current_block_hash
      blueprint.to_execute
  in
  Blueprints_publisher.publish level blueprint.to_publish

let apply_blueprint ctxt timestamp payload =
  let open Lwt_result_syntax in
  Lwt_mutex.with_lock ctxt.head_lock @@ fun () ->
  let* context, current_block_hash, applied_upgrade =
    with_store_transaction ctxt @@ fun ctxt ->
    apply_blueprint_store_unsafe ctxt timestamp payload
  in
  let*! () =
    on_new_head
      ctxt
      ~applied_upgrade
      timestamp
      context
      current_block_hash
      payload
  in
  return_unit

let init ?kernel_path ~data_dir ~preimages ~preimages_endpoint
    ~smart_rollup_address () =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load ~cache_size:100_000 Read_write (store_path ~data_dir)
  in
  let destination =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in
  let* store, context, next_blueprint_number, current_block_hash, loaded =
    load ~data_dir index
  in
  let* pending_upgrade = Store.Kernel_upgrades.find_latest_pending store in
  let ctxt =
    {
      index;
      data_dir;
      preimages;
      preimages_endpoint;
      smart_rollup_address = destination;
      session =
        {context; next_blueprint_number; current_block_hash; pending_upgrade};
      blueprint_watcher = Lwt_watcher.create_input ();
      store;
      head_lock = Lwt_mutex.create ();
    }
  in

  let* () =
    match kernel_path with
    | Some kernel ->
        if loaded then
          let*! () = Events.ignored_kernel_arg () in
          return_unit
        else
          let* evm_state = Evm_state.init ~kernel in
          let* context = replace_current_commit ctxt evm_state in
          on_modified_head ctxt context ;
          return_unit
    | None ->
        if loaded then return_unit
        else
          failwith
            "Cannot compute the initial EVM state without the path to the \
             initial kernel"
  in

  let*! () =
    Option.iter_s
      (fun upgrade -> Events.pending_upgrade upgrade)
      pending_upgrade
  in

  return (ctxt, loaded)

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
     retrieved by the sequencer by inspecting the rollup node durable storage.

     If we do not remove the delayed inbox from the state, the contents will
     never be flushed (because of the distinction between executable /
     publishable) *)
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
  let* store = Store.init ~data_dir in
  let* () =
    Store.Context_hashes.store store (Qty current_blueprint_number) checkpoint
  in
  return_unit

let execute_and_inspect ?wasm_entrypoint ~input ctxt =
  let open Lwt_result_syntax in
  let config = execution_config ctxt in
  let*! evm_state = evm_state ctxt in
  Evm_state.execute_and_inspect ?wasm_entrypoint ~config ~input evm_state

let last_produced_blueprint (ctxt : t) =
  let open Lwt_result_syntax in
  let (Qty next) = ctxt.session.next_blueprint_number in
  let current = Ethereum_types.Qty Z.(pred next) in
  let* blueprint = Store.Executable_blueprints.find ctxt.store current in
  match blueprint with
  | Some blueprint -> return blueprint
  | None -> failwith "Could not fetch the last produced blueprint"
