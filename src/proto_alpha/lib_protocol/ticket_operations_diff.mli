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
(*                                                                           *)
(*****************************************************************************)

(** A module that provides functionality for extracting ticket-token differences
    from a list of operations. *)

(** A type representing ticket-token balance differences. Each value consists
    of:
    - [ticket_token] - the type of the ticket.
    - [total_amount] - the total amount of transferred ticket-tokens.
    - [destinations] - a list of amount and contract pairs.
    Invariant: [total_amount] is the sum of the amounts in [destinations]. *)
type ticket_token_diff = private {
  ticket_token : Ticket_token.ex_token;
  total_amount : Script_int.n Script_int.num;
  destinations :
    (Alpha_context.Destination.t * Script_typed_ir.ticket_amount) list;
}

(** [ticket_diffs_of_operations ctxt ops] returns a
    list of ticket-tokens diffs given a context, [ctxt], and list of packed
    operations, [ops]. The diffs result from either a [Transaction] operation
    with parameters containing tickets, or an [Origination] operation with the
    initial storage containing tickets.
    *)
val ticket_diffs_of_operations :
  Alpha_context.context ->
  Script_typed_ir.packed_internal_operation list ->
  (ticket_token_diff list * Alpha_context.context) tzresult Lwt.t
