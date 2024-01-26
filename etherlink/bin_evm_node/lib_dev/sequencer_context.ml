(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  data_dir : string;
  context : Irmin_context.rw;
  kernel : string;
  preimages : string;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
}

type metadata = {
  checkpoint : Context_hash.t;
  next_blueprint_number : Ethereum_types.quantity;
}

let store_path ~data_dir = Filename.Infix.(data_dir // "store")

let metadata_path ~data_dir = Filename.Infix.(data_dir // "metadata")

let metadata_encoding =
  let open Data_encoding in
  conv
    (fun {checkpoint; next_blueprint_number} ->
      (checkpoint, next_blueprint_number))
    (fun (checkpoint, next_blueprint_number) ->
      {checkpoint; next_blueprint_number})
    (obj2
       (req "checkpoint" Context_hash.encoding)
       (req "next_blueprint_number" Ethereum_types.quantity_encoding))

let store_metadata ~data_dir metadata =
  let json = Data_encoding.Json.construct metadata_encoding metadata in
  Lwt_utils_unix.Json.write_file (metadata_path ~data_dir) json

let load_metadata ~data_dir index =
  let open Lwt_result_syntax in
  let path = metadata_path ~data_dir in
  let*! exists = Lwt_unix.file_exists path in
  if exists then
    let* content = Lwt_utils_unix.Json.read_file path in
    let {checkpoint; next_blueprint_number} =
      Data_encoding.Json.destruct metadata_encoding content
    in
    let*! context = Irmin_context.checkout_exn index checkpoint in
    return (context, next_blueprint_number, true)
  else
    let context = Irmin_context.empty index in
    return (context, Ethereum_types.Qty Z.zero, false)

let commit (ctxt : t) evm_state =
  let open Lwt_result_syntax in
  let*! context = Irmin_context.PVMState.set ctxt.context evm_state in
  let*! checkpoint = Irmin_context.commit context in
  let* () =
    store_metadata
      ~data_dir:ctxt.data_dir
      {checkpoint; next_blueprint_number = ctxt.next_blueprint_number}
  in
  return {ctxt with context}

let sync ctxt =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load
      ~cache_size:100_000
      Read_write
      (store_path ~data_dir:ctxt.data_dir)
  in
  let* context, next_blueprint_number, _loaded =
    load_metadata ~data_dir:ctxt.data_dir index
  in
  return {ctxt with context; next_blueprint_number}

let evm_state {context; _} = Irmin_context.PVMState.get context

let store_blueprint ctxt number blueprint =
  Blueprint_store.store
    (Blueprint_store.make ~data_dir:ctxt.data_dir)
    number
    blueprint

let find_blueprint ctxt number =
  Blueprint_store.find (Blueprint_store.make ~data_dir:ctxt.data_dir) number

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
    let* evm_state = Sequencer_state.execute ~config evm_state inbox in
    let* ctxt = if commit then perform_commit ctxt evm_state else return ctxt in
    return (ctxt, evm_state)

let apply_blueprint ctxt blueprint =
  let open Lwt_result_syntax in
  let*! evm_state = evm_state ctxt in
  let config = execution_config ctxt in
  let (Qty next) = ctxt.next_blueprint_number in
  let*! try_apply =
    Sequencer_state.apply_blueprint ~config evm_state blueprint
  in

  match try_apply with
  | Ok (evm_state, Block_height blueprint_number)
    when Z.equal blueprint_number next ->
      let* () = store_blueprint ctxt blueprint (Qty blueprint_number) in
      ctxt.next_blueprint_number <- Qty (Z.succ blueprint_number) ;
      let*! () = Blueprint_event.blueprint_produced blueprint_number in
      let* ctxt = commit ctxt evm_state in
      let* () = Blueprints_publisher.publish next blueprint in
      return ctxt
  | Ok _ | Error (Sequencer_state.Cannot_apply_blueprint :: _) ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6826 *)
      let*! () = Blueprint_event.invalid_blueprint_produced next in
      tzfail Sequencer_state.Cannot_apply_blueprint
  | Error err -> fail err

let init ~data_dir ~kernel ~preimages ~smart_rollup_address ~secret_key =
  let open Lwt_result_syntax in
  let* index =
    Irmin_context.load ~cache_size:100_000 Read_write (store_path ~data_dir)
  in
  let destination =
    Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn smart_rollup_address
  in
  let* context, next_blueprint_number, loaded = load_metadata ~data_dir index in
  let ctxt =
    {
      context;
      data_dir;
      kernel;
      preimages;
      smart_rollup_address = destination;
      next_blueprint_number;
    }
  in
  let* ctxt =
    if loaded then return ctxt
    else
      (* Create the first empty block. *)
      let* evm_state = Sequencer_state.init ~kernel in
      let* ctxt = commit ctxt evm_state in
      let genesis =
        Sequencer_blueprint.create
          ~secret_key
          ~timestamp:(Helpers.now ())
          ~smart_rollup_address
          ~transactions:[]
          ~number:Ethereum_types.(Qty Z.zero)
      in
      apply_blueprint ctxt genesis
  in

  return ctxt

let execute_and_inspect ~input ctxt =
  let open Lwt_result_syntax in
  let config = execution_config ctxt in
  let*! evm_state = evm_state ctxt in
  Sequencer_state.execute_and_inspect ~config ~input evm_state
