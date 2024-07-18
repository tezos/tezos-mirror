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

type error +=
  | Negative_ticket_balance of {key : Ticket_hash_repr.t; balance : Z.t}
  | Used_storage_space_underflow

(** [get_balance ctxt key] receives the ticket balance for the given
    [key] in the context [ctxt]. The [key] represents a ticket content and a
    ticket creator pair. In case there exists no value for the given [key],
    [None] is returned.
    *)
val get_balance :
  Raw_context.t ->
  Ticket_hash_repr.t ->
  (Z.t option * Raw_context.t) tzresult Lwt.t

(** [adjust_balance ctxt key ~delta] adjusts the balance of the
    given key (representing a ticket content, creator and owner pair)
    and [delta]. The value of [delta] can be positive as well as negative.
    If there is no pre-exising balance for the given ticket type and owner,
    it is assumed to be 0 and the new balance is [delta]. The function also
    returns the difference between the old and the new size of the storage.
    Note that the difference may be negative. For example, because when
    setting the balance to zero, an entry is removed.

    The function fails with a [Negative_ticket_balance] error
    in case the resulting balance is negative.
 *)
val adjust_balance :
  Raw_context.t ->
  Ticket_hash_repr.t ->
  delta:Z.t ->
  (Z.t * Raw_context.t) tzresult Lwt.t

(** [adjust_storage_space ctxt ~storage_diff] updates the used storage space
    for the ticket-table according to [storage_diff]. The additional positive
    amount of unpaid storage is returned. If no unpaid storage is consumed,
    this amount is 0.

    Note that when storage space for the ticket table is released we may later
    use that space for free. For this reason, the amount returned may be less
    than the given (positive) [storage_diff]. *)
val adjust_storage_space :
  Raw_context.t -> storage_diff:Z.t -> (Z.t * Raw_context.t) tzresult Lwt.t

module Internal_for_tests : sig
  (** [used_storage_space ctxt] returns the used ticket storage space. *)
  val used_storage_space : Raw_context.t -> (Z.t, error trace) result Lwt.t

  (** [paid_storage_space ctxt] returns the paid ticket storage space. *)
  val paid_storage_space : Raw_context.t -> (Z.t, error trace) result Lwt.t
end
