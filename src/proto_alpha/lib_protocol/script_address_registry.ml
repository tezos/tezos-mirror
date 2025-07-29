(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let index ctxt contract =
  let open Lwt_result_syntax in
  let* {ctxt; index; existed} =
    Alpha_context.Address_registry_storage.add_if_missing ctxt contract
  in
  let diff =
    if existed then []
    else [Alpha_context.Address_registry.{address = contract; counter = index}]
  in
  return (ctxt, index, diff)

let get ctxt contract =
  Alpha_context.Address_registry_storage.find ctxt contract
