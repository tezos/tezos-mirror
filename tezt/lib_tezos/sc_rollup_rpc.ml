(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2021-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*****************************************************************************)

let call_rpc ~smart_rollup_node ~service =
  let open Runnable.Syntax in
  let url =
    Printf.sprintf "%s/%s" (Sc_rollup_node.endpoint smart_rollup_node) service
  in
  let*! response = Curl.get url in
  return response

let ticks ?(block = "head") smart_rollup_node =
  let service = "global/block/" ^ block ^ "/ticks" in
  let* json = call_rpc ~smart_rollup_node ~service in
  return (JSON.as_int json)

let state_hash ?(block = "head") smart_rollup_node =
  let service = "global/block/" ^ block ^ "/state_hash" in
  let* json = call_rpc ~smart_rollup_node ~service in
  return (JSON.as_string json)
