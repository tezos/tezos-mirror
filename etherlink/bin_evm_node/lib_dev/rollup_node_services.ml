(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc
open Path

let smart_rollup_address :
    ([`GET], unit, unit, unit, unit, bytes) Service.service =
  Service.get_service
    ~description:"Smart rollup address"
    ~query:Query.empty
    ~output:(Data_encoding.Fixed.bytes 20)
    (open_root / "global" / "smart_rollup_address")

type state_value_query = {key : string}

let state_value_query : state_value_query Tezos_rpc.Query.t =
  let open Tezos_rpc.Query in
  query (fun key -> {key})
  |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
  |> seal

let durable_state_value :
    ([`GET], unit, unit, state_value_query, unit, bytes option) Service.service
    =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve value by key from PVM durable storage. PVM state is taken with \
       respect to the specified block level. Value returned in hex format."
    ~query:state_value_query
    ~output:Data_encoding.(option bytes)
    (open_root / "global" / "block" / "head" / "durable" / "wasm_2_0_0"
   / "value")

let batcher_injection :
    ([`POST], unit, unit, unit, string trace, string trace) Service.service =
  Tezos_rpc.Service.post_service
    ~description:"Inject messages in the batcher's queue"
    ~query:Tezos_rpc.Query.empty
    ~input:
      Data_encoding.(
        def "messages" ~description:"Messages to inject" (list (string' Hex)))
    ~output:
      Data_encoding.(
        def
          "message_hashes"
          ~description:"Hashes of injected L2 messages"
          (list string))
    (open_root / "local" / "batcher" / "injection")

let simulation :
    ( [`POST],
      unit,
      unit,
      unit,
      Simulation.Encodings.simulate_input,
      Data_encoding.json )
    Service.service =
  Tezos_rpc.Service.post_service
    ~description:
      "Simulate messages evaluation by the PVM, and find result in durable \
       storage"
    ~query:Tezos_rpc.Query.empty
    ~input:Simulation.Encodings.simulate_input
    ~output:Data_encoding.Json.encoding
    (open_root / "global" / "block" / "head" / "simulate")

let global_block_watcher :
    ([`GET], unit, unit, unit, unit, Data_encoding.json) Service.service =
  Tezos_rpc.Service.get_service
    ~description:"Monitor and streaming the L2 blocks"
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.json
    (open_root / "global" / "monitor_blocks")

let call_service ~base ?(media_types = Media_type.all_media_types) =
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service media_types ~base

(** [smart_rollup_address base] asks for the smart rollup node's
    address, using the endpoint [base]. *)
let smart_rollup_address base =
  let open Lwt_result_syntax in
  let*! answer =
    call_service
      ~base
      ~media_types:[Media_type.octet_stream]
      smart_rollup_address
      ()
      ()
      ()
  in
  match answer with
  | Ok address -> return (Bytes.to_string address)
  | Error tztrace ->
      failwith
        "Failed to communicate with %a, because %a"
        Uri.pp
        base
        pp_print_trace
        tztrace

let publish :
    rollup_node_endpoint:Uri.t ->
    [< `External of string] list ->
    unit tzresult Lwt.t =
 fun ~rollup_node_endpoint inputs ->
  let open Lwt_result_syntax in
  let inputs = List.map (function `External s -> s) inputs in
  let* _answer =
    call_service ~base:rollup_node_endpoint batcher_injection () () inputs
  in
  return_unit
