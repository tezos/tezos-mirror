(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module MakeBackend (Ctxt : sig
  val ctxt : Evm_context.t

  val evm_node_endpoint : Uri.t
end) : Services_backend_sig.Backend = struct
  module READER = struct
    let read path =
      let open Lwt_result_syntax in
      let*! evm_state = Evm_context.evm_state Ctxt.ctxt in
      let*! res = Evm_state.inspect evm_state path in
      return res
  end

  module TxEncoder = struct
    type transactions = {
      raw : string list;
      delayed : Ethereum_types.Delayed_transaction.t list;
    }

    type messages = string list

    let encode_transactions ~smart_rollup_address:_
        ~(transactions : transactions) =
      let open Result_syntax in
      let hashes =
        List.map
          (fun transaction ->
            let tx_hash_str = Ethereum_types.hash_raw_tx transaction in
            Ethereum_types.(
              Hash Hex.(of_string tx_hash_str |> show |> hex_of_string)))
          transactions.raw
      in
      return (hashes, transactions.raw)
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
      let methods = List.map send_raw_transaction_method messages in

      let* response =
        call_service
          ~base:Ctxt.evm_node_endpoint
          (Services.dispatch_service ~path:Resto.Path.root)
          ()
          ()
          (Batch methods)
      in

      let* () = check_batched_response response in

      return_unit
  end

  module SimulatorBackend = struct
    let simulate_and_read ~input =
      let open Lwt_result_syntax in
      let* raw_insights = Evm_context.execute_and_inspect Ctxt.ctxt ~input in
      match raw_insights with
      | [Some bytes] -> return bytes
      | _ -> Error_monad.failwith "Invalid insights format"
  end

  let inject_kernel_upgrade upgrade =
    let open Lwt_result_syntax in
    let payload = Ethereum_types.Upgrade.to_bytes upgrade |> String.of_bytes in
    let*! evm_state = Evm_context.evm_state Ctxt.ctxt in
    let*! evm_state =
      Evm_state.modify
        ~key:Durable_storage_path.kernel_upgrade
        ~value:payload
        evm_state
    in
    let (Qty next) = Ctxt.ctxt.session.next_blueprint_number in
    let* () =
      Evm_context.commit ~number:(Qty Z.(pred next)) Ctxt.ctxt evm_state
    in
    let* () =
      Store.Kernel_upgrades.store
        Ctxt.ctxt.store
        Ctxt.ctxt.session.next_blueprint_number
        upgrade
    in
    return_unit

  let inject_sequencer_upgrade ~payload =
    let open Lwt_result_syntax in
    let*! evm_state = Evm_context.evm_state Ctxt.ctxt in
    let*! evm_state =
      Evm_state.modify
        ~key:Durable_storage_path.sequencer_upgrade
        ~value:payload
        evm_state
    in
    let (Qty next) = Ctxt.ctxt.session.next_blueprint_number in
    let* () =
      Evm_context.commit ~number:(Qty Z.(pred next)) Ctxt.ctxt evm_state
    in
    return_unit
end

let on_new_blueprint (ctxt : Evm_context.t) (blueprint : Blueprint_types.t) =
  let (Qty level) = blueprint.number in
  let (Qty number) = ctxt.session.next_blueprint_number in
  if Z.(equal level number) then
    Evm_context.apply_blueprint ctxt blueprint.payload
  else failwith "Received a blueprint with an unexpected number."

let main (ctxt : Evm_context.t) ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let rec loop stream =
    let*! candidate = Lwt_stream.get stream in
    match candidate with
    | Some blueprint ->
        let* () = on_new_blueprint ctxt blueprint in
        let* _ =
          Tx_pool.produce_block ~force:false ~timestamp:(Helpers.now ())
        in
        loop stream
    | None -> return_unit
  in

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6876
     Should be resilient to errors from the EVM node endpoint *)
  let*! blueprints_stream =
    Evm_services.monitor_blueprints
      ~evm_node_endpoint
      ctxt.session.next_blueprint_number
  in

  loop blueprints_stream

module Make (Ctxt : sig
  val ctxt : Evm_context.t

  val evm_node_endpoint : Uri.t
end) : Services_backend_sig.S =
  Services_backend_sig.Make (MakeBackend (Ctxt))
