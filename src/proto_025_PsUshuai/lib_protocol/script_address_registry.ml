(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type error += Address_registry_invalid_counter

let () =
  (* Registry returns a negative counter *)
  register_error_kind
    `Temporary
    ~id:"michelson_v1.address_registry_invalid_counterg"
    ~title:"Address registry returned counter is invalid."
    ~description:
      "The returned counter for the given address in the registry is negative."
    Data_encoding.empty
    (function Address_registry_invalid_counter -> Some () | _ -> None)
    (fun () -> Address_registry_invalid_counter)

let index ctxt contract =
  let open Lwt_result_syntax in
  let* {ctxt; index; existed} =
    Alpha_context.Address_registry.add_if_missing ctxt contract
  in
  let ctxt =
    if existed then ctxt
    else
      Alpha_context.Address_registry.(
        register_diff ctxt {address = contract; index})
  in
  let*? index =
    match Script_int.is_nat (Script_int.of_zint index) with
    (* Note that this case is impossible, as the counter is handled
       by the protocol. If the counter is negative, this implies the
       protocol code is broken. *)
    | None -> Result_syntax.tzfail Address_registry_invalid_counter
    | Some n -> ok n
  in
  return (index, ctxt)

let get ctxt contract =
  let open Lwt_result_syntax in
  let* ctxt, res = Alpha_context.Address_registry.find ctxt contract in
  let*? res =
    match res with
    | None -> ok None
    | Some res -> (
        match Script_int.is_nat (Script_int.of_zint res) with
        (* Note that this case is impossible, as the counter is handled
           by the protocol. If the counter is negative, this implies the
           protocol code is broken. *)
        | None -> Result_syntax.tzfail Address_registry_invalid_counter
        | Some n -> ok (Some n))
  in
  return (res, ctxt)
