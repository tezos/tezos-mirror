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
        def "messages" ~description:"Messages to inject" (list string))
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

let call_service ~base ?(media_types = Media_type.all_media_types) =
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service media_types ~base
