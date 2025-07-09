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
  let* {ctxt; index; existed = _} =
    Alpha_context.Address_registry_storage.add_if_missing ctxt contract
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
