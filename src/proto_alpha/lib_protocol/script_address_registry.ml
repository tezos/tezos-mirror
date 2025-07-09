(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let set_counter ctxt contract =
  let open Lwt_result_syntax in
  let* ctxt, next_counter =
    Alpha_context.Address_registry.next_counter_and_incr ctxt
  in
  let* ctxt, _size_diff, _has_changed =
    Alpha_context.Address_registry.add ctxt contract next_counter
  in
  return (ctxt, next_counter)

let index ctxt contract =
  let open Lwt_result_syntax in
  let* ctxt, existing_counter =
    Alpha_context.Address_registry.find ctxt contract
  in
  match existing_counter with
  | Some c -> return (ctxt, c)
  | None -> set_counter ctxt contract
