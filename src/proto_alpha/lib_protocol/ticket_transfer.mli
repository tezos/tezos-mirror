(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Margiold <contact@marigold.dev>                        *)
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

(** [parse_ticket ~ticketer ~contents ~ty
    ctxt] reconstructs a ticket from individual parts submitted as
    part of a layer-1 operation. *)
val parse_ticket :
  consume_deserialization_gas:Script.consume_deserialization_gas ->
  ticketer:Contract.t ->
  contents:Script.lazy_expr ->
  ty:Script.lazy_expr ->
  context ->
  (context * Ticket_token.ex_token, error trace) result Lwt.t

(** Same as [parse_ticket], but in addition, build a transaction to
     let [sender] transfers [amount] units of said ticket to
     [destination]. *)
val parse_ticket_and_operation :
  consume_deserialization_gas:Script.consume_deserialization_gas ->
  ticketer:Contract.t ->
  contents:Script.lazy_expr ->
  ty:Script.lazy_expr ->
  sender:Destination.t ->
  destination:Contract_hash.t ->
  entrypoint:Entrypoint.t ->
  amount:Script_typed_ir.ticket_amount ->
  context ->
  (context * Ticket_token.ex_token * Script_typed_ir.packed_internal_operation)
  tzresult
  Lwt.t

(** [transfer_ticket_with_hashes ctxt ~sender_hash ~dst_hash qty] updates
    the table of tickets moves [qty] units of a given ticket from a
    sender to a destination, as encoded by [sender_hash] and [dst_hash].

    Consistency between [sender_hash] and [dst_hash] is the
    responsibility of the caller. Whenever possible, [transfer_ticket]
    should be preferred, but [transfer_ticket_with_hashes] could be
    preferred to reduce gas comsumption (e.g., to reuse hashes already
    computed).

    In addition to an updated context, this function returns the
    number of bytes that were newly allocated for the table of
    tickets. *)
val transfer_ticket_with_hashes :
  context ->
  sender_hash:Ticket_hash.t ->
  dst_hash:Ticket_hash.t ->
  Ticket_amount.t ->
  (context * Z.t) tzresult Lwt.t

(** [transfer_ticket ctxt ~sender ~dst ex_token qty] updates the table of
    tickets moves [qty] units of [ex_token] from [sender] to [dst], as
    encoded by [sender_hash] and [dst_hash].

    In addition to an updated context, this function returns the
    number of bytes that were newly allocated for the table of
    tickets. *)
val transfer_ticket :
  context ->
  sender:Destination.t ->
  dst:Destination.t ->
  Ticket_token.ex_token ->
  Ticket_amount.t ->
  (context * Z.t, error trace) result Lwt.t
