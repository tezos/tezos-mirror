(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  data_dir : string;
  mutable context : Irmin_context.rw;
  index : Irmin_context.rw_index;
  preimages : string;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  blueprint_watcher : Blueprint_types.t Lwt_watcher.input;
}

type metadata = {
  checkpoint : Context_hash.t;
  next_blueprint_number : Ethereum_types.quantity;
  current_block_hash : Ethereum_types.block_hash;
}

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let metadata_path ~data_dir = Filename.Infix.(data_dir // "metadata")

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {checkpoint; next_blueprint_number; current_block_hash} ->
      (checkpoint, next_blueprint_number, current_block_hash))
    (fun (checkpoint, next_blueprint_number, current_block_hash) ->
      {checkpoint; next_blueprint_number; current_block_hash})
    (obj3
       (req "checkpoint" Context_hash.encoding)
       (req "next_blueprint_number" Ethereum_types.quantity_encoding)
       (req "current_block_hash" Ethereum_types.block_hash_encoding))

let store_metadata ~data_dir metadata =
  let json = Data_encoding.Json.construct metadata_encoding metadata in
  Lwt_utils_unix.Json.write_file (metadata_path ~data_dir) json

let load_metadata ~data_dir index =
  let open Lwt_result_syntax in
  let path = metadata_path ~data_dir in
  let*! exists = Lwt_unix.file_exists path in
  if exists then
    let* content = Lwt_utils_unix.Json.read_file path in
    let {checkpoint; next_blueprint_number; current_block_hash} =
      Data_encoding.Json.destruct metadata_encoding content
    in
    let*! context = Irmin_context.checkout_exn index checkpoint in
    return (context, next_blueprint_number, current_block_hash, true)
  else
    let context = Irmin_context.empty index in
    return
      ( context,
        Ethereum_types.Qty Z.zero,
        Ethereum_types.genesis_parent_hash,
        false )

let commit (ctxt : t) evm_state =
  let open Lwt_result_syntax in
  let*! context = Irmin_context.PVMState.set ctxt.context evm_state in
  let*! checkpoint = Irmin_context.commit context in
  let* () =
    store_metadata
      ~data_dir:ctxt.data_dir
      {
        checkpoint;
        next_blueprint_number = ctxt.next_blueprint_number;
        current_block_hash = ctxt.current_block_hash;
      }
  in
  ctxt.context <- context ;
  return_unit

let evm_state {context; _} = Irmin_context.PVMState.get context

let store_blueprint ctxt number blueprint =
  Blueprint_store.store
    (Blueprint_store.make ~data_dir:ctxt.data_dir)
    number
    blueprint

let find_blueprint ctxt number =
  Blueprint_store.find
    ~kind:`Execute
    (Blueprint_store.make ~data_dir:ctxt.data_dir)
    number

let execution_config ctxt =
  Config.config
    ~preimage_directory:ctxt.preimages
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

let execute =
  let perform_commit = commit in
  fun ?(commit = false) ctxt inbox ->
    let open Lwt_result_syntax in
    let config = execution_config ctxt in
    let*! evm_state = evm_state ctxt in
    let* evm_state = Evm_state.execute ~config evm_state inbox in
    let* () = when_ commit (fun () -> perform_commit ctxt evm_state) in
    return (ctxt, evm_state)

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_apply_blueprint"
    ~title:"Cannot apply a blueprint"
    ~description:
      "The EVM node could not apply apply a blueprint on top of its local EVM \
       state."
    ~pp:(fun ppf local_state_level ->
      Format.fprintf
        ppf
        "The EVM node could not apply apply a blueprint on top of its local \
         EVM state at level %a."
        Z.pp_print
        local_state_level)
    Data_encoding.(obj1 (req "current_state_level" n))
    (function
      | Cannot_apply_blueprint {local_state_level} -> Some local_state_level
      | _ -> None)
    (fun local_state_level -> Cannot_apply_blueprint {local_state_level})

let apply_blueprint ctxt payload =
  let open Lwt_result_syntax in
  let*! evm_state = evm_state ctxt in
  let config = execution_config ctxt in
  let (Qty next) = ctxt.next_blueprint_number in
  let* try_apply = Evm_state.apply_blueprint ~config evm_state payload in

  match try_apply with
  | Apply_success (evm_state, Block_height blueprint_number, current_block_hash)
    when Z.equal blueprint_number next ->
      let* () =
        store_blueprint ~kind:`Execute ctxt payload (Qty blueprint_number)
      in
      ctxt.next_blueprint_number <- Qty (Z.succ blueprint_number) ;
      ctxt.current_block_hash <- current_block_hash ;
      let*! () = Blueprint_events.blueprint_applied blueprint_number in
      let* () = commit ctxt evm_state in
      Lwt_watcher.notify
        ctxt.blueprint_watcher
        {number = Qty blueprint_number; payload} ;
      return ctxt
  | Apply_success _ (* Produced a block, but not of the expected height *)
  | Apply_failure (* Did not produce a block *) ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6826 *)
      let*! () = Blueprint_events.invalid_blueprint_produced next in
      tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

let apply_and_publish_blueprint (ctxt : t) (blueprint : Sequencer_blueprint.t) =
  let open Lwt_result_syntax in
  let (Qty level) = ctxt.next_blueprint_number in
  let* ctxt = apply_blueprint ctxt blueprint.to_execute in
  let* () =
    store_blueprint ~kind:`Publish ctxt blueprint.to_publish (Qty level)
  in
  let* () = Blueprints_publisher.publish level blueprint.to_publish in
  return ctxt

let init ?(genesis_timestamp = Helpers.now ()) ?produce_genesis_with
    ?kernel_path ~data_dir ~preimages ~smart_rollup_address () =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load ~cache_size:100_000 Read_write (store_path ~data_dir)
  in
  let destination =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in
  let* context, next_blueprint_number, current_block_hash, loaded =
    load_metadata ~data_dir index
  in
  let ctxt =
    {
      index;
      context;
      data_dir;
      preimages;
      smart_rollup_address = destination;
      next_blueprint_number;
      current_block_hash;
      blueprint_watcher = Lwt_watcher.create_input ();
    }
  in
  let* ctxt =
    match kernel_path with
    | Some kernel -> (
        if loaded then
          let*! () = Events.ignored_kernel_arg () in
          return ctxt
        else
          let* evm_state = Evm_state.init ~kernel in
          let* () = commit ctxt evm_state in
          match produce_genesis_with with
          | Some secret_key ->
              (* Create the first empty block. *)
              let genesis =
                Sequencer_blueprint.create
                  ~secret_key
                  ~timestamp:genesis_timestamp
                  ~smart_rollup_address
                  ~transactions:[]
                  ~delayed_transactions:[]
                  ~number:Ethereum_types.(Qty Z.zero)
                  ~parent_hash:Ethereum_types.genesis_parent_hash
              in
              apply_and_publish_blueprint ctxt genesis
          | None -> return ctxt)
    | None ->
        if loaded then return ctxt
        else
          failwith
            "Cannot compute the initial EVM state without the path to the \
             initial kernel"
  in

  return ctxt

let init_from_rollup_node ~data_dir ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let* checkpoint =
    let l2_head_path =
      Filename.Infix.(rollup_node_data_dir // "storage" // "l2_head")
    in
    Lwt_io.with_file ~flags:[Unix.O_RDONLY; O_CLOEXEC] ~mode:Input l2_head_path
    @@ fun channel ->
    let*! raw_data = Lwt_io.read channel in
    let Sc_rollup_block.{header = {context; _}; _} =
      Data_encoding.Binary.of_string_exn Sc_rollup_block.encoding raw_data
    in
    Smart_rollup_context_hash.to_bytes context
    |> Context_hash.of_bytes_exn |> return
  in
  let rollup_node_context_dir =
    Filename.Infix.(rollup_node_data_dir // "context")
  in
  let* rollup_node_index =
    Irmin_context.load ~cache_size:100_000 Read_only rollup_node_context_dir
  in
  let*! rollup_node_context =
    Irmin_context.checkout_exn rollup_node_index checkpoint
  in
  let evm_context_dir = store_path ~data_dir in
  let*! () = Lwt_utils_unix.create_dir evm_context_dir in
  let* () =
    Irmin_context.export_snapshot
      rollup_node_context
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
  let* current_blueprint_number =
    let*! current_blueprint_number_opt =
      Evm_state.inspect evm_state Durable_storage_path.Block.current_number
    in
    match current_blueprint_number_opt with
    | Some bytes -> return (Bytes.to_string bytes |> Z.of_bits)
    | None -> failwith "The blueprint number was not found"
  in
  let* current_block_hash =
    let*! current_block_hash_opt =
      Evm_state.inspect evm_state Durable_storage_path.Block.current_hash
    in
    match current_block_hash_opt with
    | Some bytes ->
        return
          (Ethereum_types.block_hash_of_string (Hex.of_bytes bytes |> Hex.show))
    | None -> failwith "The block hash was not found"
  in
  let next_blueprint_number =
    Ethereum_types.Qty Z.(add one current_blueprint_number)
  in
  store_metadata
    ~data_dir
    {checkpoint; next_blueprint_number; current_block_hash}

let execute_and_inspect ~input ctxt =
  let open Lwt_result_syntax in
  let config = execution_config ctxt in
  let*! evm_state = evm_state ctxt in
  Evm_state.execute_and_inspect ~config ~input evm_state

let last_produced_blueprint (ctxt : t) =
  let open Lwt_syntax in
  let (Qty next) = ctxt.next_blueprint_number in
  let current = Ethereum_types.Qty Z.(pred next) in
  let* blueprint = find_blueprint ctxt current in
  match blueprint with
  | Some blueprint ->
      return_ok Blueprint_types.{number = current; payload = blueprint}
  | None -> failwith "Could not fetch the last produced blueprint"
