(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [find ctxt contract] returns the delegate associated to [contract], or [None]
    if [contract] has no delegate. *)
val find :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t option tzresult Lwt.t

(** [registered ctxt delegate] returns true iff delegate is an implicit contract
    that delegates to itself. *)
val registered :
  Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

(** [init ctxt contract delegate] sets the [delegate] associated to [contract].

    This function is undefined if [contract] is not allocated, or if [contract]
    has already a delegate. *)
val init :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t ->
  Raw_context.t tzresult Lwt.t

(** [unlink ctxt contract] removes [contract] from the list of contracts that
    delegated to [find ctxt contract], i.e. the output of [delegated_contracts].
    This function does not affect the value of the expression
    [find ctxt contract].

    This function is undefined if [contract] is not allocated. *)
val unlink : Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

(** [delete ctxt contract] behaves as [unlink ctxt contract], but in addition
    removes the association of the [contract] to its current delegate, leaving
    the former without delegate.

    This function is undefined if [contract] is not allocated. *)
val delete : Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

(** [set ctxt contract delegate] updates the [delegate] associated to [contract].

    This function is undefined if [contract] is not allocated, or if [contract]
    does not have a delegate. *)
val set :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t ->
  Raw_context.t tzresult Lwt.t

(** [delegated_contracts ctxt delegate] returns the list of contracts (implicit
    or originated) that delegated to [delegate]. *)
val delegated_contracts :
  Raw_context.t -> Signature.Public_key_hash.t -> Contract_repr.t list Lwt.t
