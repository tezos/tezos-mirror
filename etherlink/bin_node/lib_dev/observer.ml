(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module MakeBackend (Ctxt : sig
  val ctxt : Evm_context.t
end) : Services_backend_sig.Backend = struct
  module READER = struct
    let read path =
      let open Lwt_result_syntax in
      let* ctxt = Evm_context.sync Ctxt.ctxt in
      let*! evm_state = Evm_context.evm_state ctxt in
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

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages:_ =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6882
         Forward transactions *)
      failwith "Forwarding transactions is not implemented yet"
  end

  module SimulatorBackend = struct
    let simulate_and_read ~input =
      let open Lwt_result_syntax in
      let* ctxt = Evm_context.sync Ctxt.ctxt in
      let* raw_insights = Evm_context.execute_and_inspect ctxt ~input in
      match Simulation.Encodings.insights_from_list raw_insights with
      | Some i -> return i
      | None -> Error_monad.failwith "Invalid insights format"
  end

  let inject_kernel_upgrade ~payload =
    let open Lwt_result_syntax in
    let* ctxt = Evm_context.sync Ctxt.ctxt in
    let*! evm_state = Evm_context.evm_state ctxt in
    let*! evm_state =
      Evm_state.modify
        ~key:Durable_storage_path.kernel_upgrade
        ~value:payload
        evm_state
    in
    let* _ctxt = Evm_context.commit ctxt evm_state in
    return_unit
end

let on_new_blueprint (ctxt : Evm_context.t) (blueprint : Blueprint_types.t) =
  let (Qty level) = ctxt.next_blueprint_number in
  let (Qty number) = ctxt.next_blueprint_number in
  if Z.(equal level number) then
    Evm_context.apply_blueprint ctxt blueprint.payload
  else failwith "Received a blueprint with an unexpected number."

let main (ctxt : Evm_context.t) ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let rec loop ctxt stream =
    let*! candidate = Lwt_stream.get stream in
    match candidate with
    | Some blueprint ->
        let* ctxt = on_new_blueprint ctxt blueprint in
        loop ctxt stream
    | None -> return_unit
  in

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/6876
     Should be resilient to errors from the EVM node endpoint *)
  let*! blueprints_stream =
    Evm_services.monitor_blueprints
      ~evm_node_endpoint
      ctxt.next_blueprint_number
  in

  loop ctxt blueprints_stream

module Make (Ctxt : sig
  val ctxt : Evm_context.t
end) : Services_backend_sig.S =
  Services_backend_sig.Make (MakeBackend (Ctxt))
