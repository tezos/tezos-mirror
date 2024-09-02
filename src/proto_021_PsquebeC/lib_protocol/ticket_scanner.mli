(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

(** This module provides an API for extracting tickets of arbitrary types
    from an OCaml values, given a type-witness. *)

open Alpha_context

(** A type for representing existentially quantified tickets (tickets with
    different types of payloads). An [ex_ticket] value consists of:
     - A type-witness representing the type of the content of the ticket.
     - A ticket value of the particular content type.
 *)
type ex_ticket =
  | Ex_ticket :
      'a Script_typed_ir.comparable_ty * 'a Script_typed_ir.ticket
      -> ex_ticket

(** A type-witness that contains information about which branches of a type ['a]
    include tickets. This value is used for traversing only the relevant
    branches of values when scanning for tickets. *)
type 'a has_tickets

(** [type_has_tickets ctxt ty] returns a [has_tickets] witness of the given
    shape [ty].
 *)
val type_has_tickets :
  context -> ('a, _) Script_typed_ir.ty -> ('a has_tickets * context) tzresult

(** [tickets_of_value ctxt ~include_lazy ht value] extracts all tickets from
    the given [value], using the type-witness [ht]. The [include_lazy] flag
    determines whether or not to traverse lazy structures (values from the context).
    In case the [include_lazy] flag is [true], any big-map contained in the value
    must have an empty overlay or else an error of type
    [Unsupported_non_empty_overlay] is returned. The reason for this restriction
    is that we assume that all lazy big-map diffs should be applied before
    calling this function. Dealing with non-empty overlays would be possible
    in theory, but practically difficult. The challenge is to distinguish
    between overlapping keys between the context and the overlay.
   *)
val tickets_of_value :
  context ->
  include_lazy:bool ->
  'a has_tickets ->
  'a ->
  (ex_ticket list * context) tzresult Lwt.t

(** [tickets_of_node ctxt ~include_lazy ht node] extracts all tickets from
    the given [node], using the type-witness [ht].If [ht] indicates that
    values of the corresponding type may not contain tickets, the node value is
    not parsed. The [include_lazy] flag determines whether or not to traverse
    lazy structures (values from the context). In case the [include_lazy] flag
    is [true], any big-map contained in the value must have an empty overlay or
    else an error of type [Unsupported_non_empty_overlay] is returned. The
    reason for this restriction is that we assume that all lazy big-map diffs
    should be applied before calling this function. Dealing with non-empty
    overlays would be possible in theory, but practically difficult. The
    challenge is to distinguish between overlapping keys between the context and
    the overlay.
   *)
val tickets_of_node :
  context ->
  include_lazy:bool ->
  'a has_tickets ->
  Script.node ->
  (ex_ticket list * context) tzresult Lwt.t

(** [has_tickets ht] returns whether or not the type of the given [has_tickets]
    witness [ht] has tickets. *)
val has_tickets : 'a has_tickets -> bool

(** [ex_ticket_size ctxt ex_ticket] returns the size of the in-memory representation of
    [ex_ticket] in bytes. *)
val ex_ticket_size :
  context ->
  ex_ticket ->
  (Saturation_repr.may_saturate Saturation_repr.t * context) tzresult Lwt.t

(** [ex_token_and_amount_of_ex_ticket ex_ticket] returns the token and amount of
    the given ticket [ex_ticket]. *)
val ex_token_and_amount_of_ex_ticket :
  ex_ticket -> Ticket_token.ex_token * Script_typed_ir.ticket_amount
