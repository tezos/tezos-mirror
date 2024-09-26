(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  preimages : string;
  preimages_endpoint : Uri.t option;
  data_dir : string;
  store : Evm_store.t;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  index : Irmin_context.ro_index;
  finalized_view : bool;
}

let load ?smart_rollup_address ~data_dir ~preimages ?preimages_endpoint
    ~finalized_view () =
  let open Lwt_result_syntax in
  let* store = Evm_store.init ~data_dir ~perm:`Read_only () in
  let* index =
    Irmin_context.(
      load ~cache_size:100_000 Read_only (Evm_state.irmin_store_path ~data_dir))
  in
  let+ smart_rollup_address =
    match smart_rollup_address with
    | None -> Evm_store.(use store Metadata.get)
    | Some smart_rollup_address -> return smart_rollup_address
  in
  {
    store;
    index;
    data_dir;
    preimages;
    preimages_endpoint;
    smart_rollup_address;
    finalized_view;
  }

let get_evm_state ctxt hash =
  let open Lwt_result_syntax in
  Irmin_context.reload ctxt.index ;
  let*! context = Irmin_context.checkout_exn ctxt.index hash in
  let*! res = Irmin_context.PVMState.get context in
  return res

let find_latest_hash ctxt =
  let open Lwt_result_syntax in
  let* res = Evm_store.(use ctxt.store Context_hashes.find_latest) in
  match res with
  | Some (_, hash) -> return hash
  | None -> failwith "No state available"

let find_finalized_hash ctxt =
  let open Lwt_result_syntax in
  let* res = Evm_store.(use ctxt.store Context_hashes.find_finalized) in
  match res with
  | Some (_, hash) -> return hash
  | None -> failwith "No state available"

let find_irmin_hash_from_number ctxt number =
  let open Lwt_result_syntax in
  let* res =
    Evm_store.(use ctxt.store @@ fun conn -> Context_hashes.find conn number)
  in
  match res with
  | Some hash -> return hash
  | None ->
      failwith
        "No state available for block %a"
        Ethereum_types.pp_quantity
        number

let find_irmin_hash ctxt (block : Ethereum_types.Block_parameter.extended) =
  let open Lwt_result_syntax in
  match block with
  | Block_parameter (Latest | Pending) when ctxt.finalized_view ->
      find_finalized_hash ctxt
  | Block_parameter (Latest | Pending) -> find_latest_hash ctxt
  | Block_parameter Earliest -> (
      let* res = Evm_store.(use ctxt.store Context_hashes.find_earliest) in
      match res with
      | Some (_, hash) -> return hash
      | None -> failwith "No state available")
  | Block_parameter Finalized -> find_finalized_hash ctxt
  | Block_parameter (Number number) -> (
      let* res =
        Evm_store.(
          use ctxt.store @@ fun conn -> Context_hashes.find conn number)
      in
      match res with
      | Some hash -> return hash
      | None ->
          failwith
            "No state available for block %a"
            Ethereum_types.pp_quantity
            number)
  | Block_hash {hash; require_canonical = _} -> (
      (* we use the latest state to read the contents of the block *)
      let* latest_hash = find_latest_hash ctxt in
      let* evm_tree = get_evm_state ctxt latest_hash in
      let*! res =
        Evm_state.inspect evm_tree Durable_storage_path.Block.(by_hash hash)
      in
      match res with
      | Some block_bytes ->
          let block = Ethereum_types.block_from_rlp block_bytes in
          find_irmin_hash_from_number ctxt block.number
      | None -> failwith "Unknown block %a" Ethereum_types.pp_block_hash hash)

module MakeBackend (Ctxt : sig
  val ctxt : t

  val evm_node_endpoint : Uri.t option

  val keep_alive : bool
end) =
struct
  module Reader = struct
    type state = Evm_state.t

    let get_state
        ?(block = Ethereum_types.Block_parameter.Block_parameter Latest) () =
      let open Lwt_result_syntax in
      let* hash = find_irmin_hash Ctxt.ctxt block in
      get_evm_state Ctxt.ctxt hash

    let read state path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.inspect state path in
      return res

    let subkeys state path =
      let open Lwt_result_syntax in
      let*! res = Evm_state.subkeys state path in
      return res
  end

  module TxEncoder = struct
    type transactions = (string * Ethereum_types.transaction_object) list

    type messages = string list

    let encode_transactions ~smart_rollup_address:_ ~transactions =
      let open Result_syntax in
      List.to_seq transactions
      |> Seq.map (fun (raw_tx, (obj : Ethereum_types.transaction_object)) ->
             (obj.hash, raw_tx))
      |> Seq.split
      |> fun (l, r) -> (List.of_seq l, List.of_seq r) |> return
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let check_response =
      let open Rpc_encodings.JSONRPC in
      let open Lwt_result_syntax in
      function
      | {value = Ok _; _} -> return_unit
      | {value = Error {message; _}; _} ->
          failwith "Send_raw_transaction failed with message \"%s\"" message

    let check_batched_response =
      let open Services in
      function
      | Batch l -> List.iter_es check_response l
      | Singleton r -> check_response r

    let send_raw_transaction_method txn =
      let open Rpc_encodings in
      let message =
        Hex.of_string txn |> Hex.show |> Ethereum_types.hex_of_string
      in
      JSONRPC.
        {
          method_ = Send_raw_transaction.method_;
          parameters =
            Some
              (Data_encoding.Json.construct
                 Send_raw_transaction.input_encoding
                 message);
          id = None;
        }

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Rollup_services in
      let open Lwt_result_syntax in
      match Ctxt.evm_node_endpoint with
      | Some evm_node_endpoint ->
          let methods = List.map send_raw_transaction_method messages in

          let* response =
            call_service
              ~keep_alive:Ctxt.keep_alive
              ~base:evm_node_endpoint
              (Services.dispatch_service ~path:Resto.Path.root)
              ()
              ()
              (Batch methods)
          in

          let* () = check_batched_response response in

          return_unit
      | None -> assert false
  end

  module SimulatorBackend = struct
    include Reader

    let modify ~key ~value state =
      let open Lwt_result_syntax in
      let*! state = Evm_state.modify ~key ~value state in
      return state

    let simulate_and_read ?state_override simulate_state ~input =
      let open Lwt_result_syntax in
      let config =
        Config.config
          ~preimage_directory:Ctxt.ctxt.preimages
          ?preimage_endpoint:Ctxt.ctxt.preimages_endpoint
          ~kernel_debug:false
          ~destination:Ctxt.ctxt.smart_rollup_address
          ()
      in
      let* simulate_state =
        State_override.update_accounts state_override simulate_state
      in
      let* raw_insights =
        Evm_state.execute_and_inspect
          ~config
          ~data_dir:Ctxt.ctxt.data_dir
          ~input
          simulate_state
      in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"
  end

  module Tracer = Tracer

  let smart_rollup_address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_string
      Ctxt.ctxt.smart_rollup_address

  let block_param_to_block_number
      (block_param : Ethereum_types.Block_parameter.extended) =
    let open Lwt_result_syntax in
    match block_param with
    | Block_parameter (Number n) -> return n
    | Block_parameter (Latest | Pending) when Ctxt.ctxt.finalized_view -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_finalized)
        in
        match res with
        | Some (latest, _) -> return latest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter (Latest | Pending) -> (
        let* res = Evm_store.(use Ctxt.ctxt.store Context_hashes.find_latest) in
        match res with
        | Some (latest, _) -> return latest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter Earliest -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_earliest)
        in
        match res with
        | Some (earliest, _) -> return earliest
        | None -> failwith "The EVM node does not have any state available")
    | Block_parameter Finalized -> (
        let* res =
          Evm_store.(use Ctxt.ctxt.store Context_hashes.find_finalized)
        in
        match res with
        | Some (finalized, _) -> return finalized
        | None -> failwith "The EVM node is not aware of any finalized block")
    | Block_hash {hash; _} -> (
        let* irmin_hash = find_irmin_hash Ctxt.ctxt block_param in
        let* evm_state = get_evm_state Ctxt.ctxt irmin_hash in
        let*! bytes =
          Evm_state.inspect evm_state Durable_storage_path.(Block.by_hash hash)
        in
        match bytes with
        | Some bytes ->
            let block = Ethereum_types.block_from_rlp bytes in
            return block.number
        | None -> failwith "Missing block %a" Ethereum_types.pp_block_hash hash)
end

module Make (Base : sig
  module Executor : Evm_execution.S

  val ctxt : t

  val evm_node_endpoint : Uri.t option

  val keep_alive : bool
end) =
  Services_backend_sig.Make (MakeBackend (Base)) (Base.Executor)

let pvm_config ctxt =
  Config.config
    ~preimage_directory:ctxt.preimages
    ?preimage_endpoint:ctxt.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ()

let replay ctxt ?(log_file = "replay") ?profile
    ?(alter_evm_state = Lwt_result_syntax.return) (Ethereum_types.Qty number) =
  let open Lwt_result_syntax in
  let* hash = find_irmin_hash_from_number ctxt (Qty Z.(pred number)) in
  let* evm_state = get_evm_state ctxt hash in
  let* evm_state = alter_evm_state evm_state in
  let* blueprint =
    Evm_store.use ctxt.store @@ fun conn ->
    Evm_store.Blueprints.get_with_events conn (Qty number)
  in
  Evm_state.apply_blueprint
    ~log_file
    ?profile
    ~data_dir:ctxt.data_dir
    ~config:(pvm_config ctxt)
    evm_state
    blueprint.blueprint.payload

let ro_backend ?evm_node_endpoint ctxt config =
  (module Make (struct
    module Executor = struct
      let pvm_config = pvm_config ctxt

      let replay = replay ctxt

      let execute ?(alter_evm_state = Lwt_result_syntax.return) input block =
        let open Lwt_result_syntax in
        let message =
          List.map (fun s -> `Input s) Simulation.Encodings.(input.messages)
        in
        let* hash = find_irmin_hash ctxt block in
        let* evm_state = get_evm_state ctxt hash in
        let* evm_state = alter_evm_state evm_state in
        Evm_state.execute
          ?log_file:input.log_kernel_debug_file
          ~data_dir:ctxt.data_dir
          ~config:pvm_config
          evm_state
          message
    end

    let ctxt = ctxt

    let evm_node_endpoint = evm_node_endpoint

    let keep_alive = config.Configuration.keep_alive
  end) : Services_backend_sig.S)

let next_blueprint_number ctxt =
  let open Lwt_result_syntax in
  let* Qty current_block_number, _ =
    Evm_store.use ctxt.store Evm_store.Context_hashes.get_latest
  in
  return (Ethereum_types.Qty Z.(succ current_block_number))

let preload_kernel_from_level ctxt level =
  let open Lwt_result_syntax in
  let* hash = find_irmin_hash_from_number ctxt level in
  let* evm_state = get_evm_state ctxt hash in
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

let evm_services_methods ctxt time_between_blocks =
  Rpc_server.
    {
      next_blueprint_number =
        (fun () ->
          let open Lwt_syntax in
          let+ res = next_blueprint_number ctxt in
          match res with
          | Ok res -> res
          | Error _ -> Stdlib.failwith "Couldn't fetch next blueprint number");
      find_blueprint =
        (fun level ->
          Evm_store.use ctxt.store (fun conn ->
              Evm_store.Blueprints.find_with_events conn level));
      smart_rollup_address = ctxt.smart_rollup_address;
      time_between_blocks;
    }
