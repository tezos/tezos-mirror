(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

let assert_balance ctxt ~loc key expected =
  let open Lwt_result_wrap_syntax in
  let*@ balance, _ = Ticket_balance.get_balance ctxt key in
  match (balance, expected) with
  | Some b, Some eb -> Assert.equal_int ~loc (Z.to_int b) eb
  | None, Some eb -> failwith "Expected balance %d" eb
  | Some eb, None -> failwith "Expected None but got %d" (Z.to_int eb)
  | None, None -> return_unit

let string_ticket_token ticketer content =
  let open Lwt_result_wrap_syntax in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string.of_string content
  in
  let*?@ ticketer = Contract.of_b58check ticketer in
  return
    (Ticket_token.Ex_token
       {ticketer; contents_type = Script_typed_ir.string_t; contents})

let adjust_ticket_token_balance alpha_ctxt owner ticket_token ~delta =
  let open Lwt_result_wrap_syntax in
  let*@ ticket_token_hash, ctxt =
    Ticket_balance_key.of_ex_token alpha_ctxt ~owner ticket_token
  in
  let*@ (_ : Z.t), alpha_ctxt =
    Ticket_balance.adjust_balance ctxt ticket_token_hash ~delta
  in
  return (ticket_token_hash, alpha_ctxt)
