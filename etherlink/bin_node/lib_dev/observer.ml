(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

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
