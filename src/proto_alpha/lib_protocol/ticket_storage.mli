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

(** A value of type [key_hash] is a hashed combination of:
  - Ticketer
  - Content type
  - Content
  - Owner
*)
type key_hash

(** [script_expr_hash_of_key_hash key_hash] returns a [Script_expr_hash.t] value
    representation of the given [key_hash]. This is useful for comparing and
    pretty-printing key-hash values. *)
val script_expr_hash_of_key_hash : key_hash -> Script_expr_hash.t

(** [make_key_hash ctxt ~ticketer ~typ ~contents ~owner] creates a hashed
    representation of the given [ticketer], [typ], [contents] and [owner].
*)
val make_key_hash :
  Raw_context.t ->
  ticketer:Script_repr.node ->
  typ:Script_repr.node ->
  contents:Script_repr.node ->
  owner:Script_repr.node ->
  (key_hash * Raw_context.t) tzresult

(** [get_balance ctxt key] receives the ticket balance for the given
    [key] in the context [ctxt]. The [key] represents a ticket content and a
    ticket creator pair. In case there exists no value for the given [key],
    [None] is returned.
    *)
val get_balance :
  Raw_context.t -> key_hash -> (Z.t option * Raw_context.t) tzresult Lwt.t

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
  Raw_context.t -> key_hash -> delta:Z.t -> (Z.t * Raw_context.t) tzresult Lwt.t
