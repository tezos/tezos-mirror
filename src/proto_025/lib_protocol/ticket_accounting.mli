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

open Alpha_context

(** [ticket_diffs ctxt ~arg_type_has_tickets ~storage_type_has_tickets arg
       old_storage new_storage lazy_storage_diff] returns a map from
    ticket-tokens to balance-differences that represents the change in balance
    for a contract due to changes of tickets in the storage. The assumption is
    that before calling [ticket_diffs], all tickets that are owned by a contract
    exist either in the [old_storage] or the [arg]. After execution, only
    tickets in [new_storage] are owned by the contract. Note that this function
    avoids traversing the lazy part of the storage.
*)
val ticket_diffs :
  context ->
  self_contract:Contract.t ->
  arg_type_has_tickets:'arg Ticket_scanner.has_tickets ->
  storage_type_has_tickets:'storage Ticket_scanner.has_tickets ->
  arg:'arg ->
  old_storage:'storage ->
  new_storage:'storage ->
  lazy_storage_diff:Lazy_storage.diffs_item list ->
  (Z.t Ticket_token_map.t * Ticket_receipt.t * context) tzresult Lwt.t

(** [ticket_balances_of_value ctxt ~include_lazy has_tickets value]
    scans all tickets in the given [value] using the type-witness [has_tickets]
    and returns a map from ticket-tokens to the amount. *)
val ticket_balances_of_value :
  context ->
  include_lazy:bool ->
  'a Ticket_scanner.has_tickets ->
  'a ->
  (Z.t Ticket_token_map.t * context) tzresult Lwt.t

(** [update_ticket_balances ctxt ~self_contract ~ticket_diffs operations] updates the
    ticket balances according to the [ticket_diffs] map and the set of
    operations. The function also returns the storage size diff resulting from
    updating the ticket-balance table in the context.

    Invariant: this function must be called after applying the lazy-storage
    diffs affecting any contracts in the given operations.

    The function fails in case an invalid ticket-token-balance update is
    detected. The [ticket_diffs] argument represents the change of ticket-tokens
    for the [self] contract. It also specifies a "budget" for outgoing
    ticket-tokens.
*)
val update_ticket_balances :
  context ->
  self_contract:Contract.t ->
  ticket_diffs:Z.t Ticket_token_map.t ->
  Script_typed_ir.packed_internal_operation list ->
  (Z.t * context) tzresult Lwt.t
