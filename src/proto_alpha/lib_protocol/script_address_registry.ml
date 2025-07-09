(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let index ctxt contract =
  let open Lwt_result_syntax in
  let* {ctxt; index; existed = _} =
    Alpha_context.Address_registry_storage.add_if_missing ctxt contract
  in
  return (ctxt, index)
