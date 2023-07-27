(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)

open Protocol
open Alpha_context

let assert_balance ctxt ~loc key expected =
  let open Lwt_result_syntax in
  let* balance, _ =
    Ticket_balance.get_balance ctxt key >|= Environment.wrap_tzresult
  in
  match (balance, expected) with
  | Some b, Some eb -> Assert.equal_int ~loc (Z.to_int b) eb
  | None, Some eb -> failwith "Expected balance %d" eb
  | Some eb, None -> failwith "Expected None but got %d" (Z.to_int eb)
  | None, None -> return_unit

let string_ticket_token ticketer content =
  let open Lwt_result_syntax in
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string.of_string content
  in
  let*? ticketer = Environment.wrap_tzresult @@ Contract.of_b58check ticketer in
  return
    (Ticket_token.Ex_token
       {ticketer; contents_type = Script_typed_ir.string_t; contents})

let adjust_ticket_token_balance alpha_ctxt owner ticket_token ~delta =
  let open Lwt_result_syntax in
  let* ticket_token_hash, ctxt =
    Ticket_balance_key.of_ex_token alpha_ctxt ~owner ticket_token
    >|= Environment.wrap_tzresult
  in
  let* _, alpha_ctxt =
    Ticket_balance.adjust_balance ctxt ticket_token_hash ~delta
    >|= Environment.wrap_tzresult
  in
  return (ticket_token_hash, alpha_ctxt)
