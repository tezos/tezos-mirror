(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Error_monad

module type S = sig
  (** This is mostly [Hashtbl.S] with the following differences:

      Looking up an element and creating an element to insert in the table are
      the same operations. In other words:
      - The function [find_or_make t k gen] behaves in two separate ways
      depending if an element is already bound to key [k] in table [t].
      - If an element is bound, then it is returned.
      - Otherwise, an element is generated using the [gen] function and recorded
      in the table.

      The table does not record elements per se. Instead it records promises of
      results of elements. This means that [find_or_make t k gen] is a value
      within the lwt-error monad.

      The table automatically cleans itself of errors. Specifically, when one of
      the promises resolves as an error, all the caller of [find_or_make] for
      the matching key are woken up with [Error] and the value is removed from
      the table. The next call to [find_or_make] with the same key causes the
      provided [gen] function to be called. *)

  type key

  type 'a t

  val create : int -> 'a t

  val clear : 'a t -> unit

  val reset : 'a t -> unit

  (** [find_or_make t k gen] is [p] if [k] is already bound to [k] in [t]. In
      this case, no side-effect is performed.

      [find_or_make t k gen] is [r] if [k] is not bound in [t] where [r] is [gen
      ()]. In this case, [r] becomes bound to [k] in [t]. In addition, a
      listener is added to [r] so that if [r] resolves to [Error _], the binding
      is removed. *)
  val find_or_make :
    'a t -> key -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

  val remove : 'a t -> key -> unit

  (** [find_opt t k] is [None] if there are no bindings for [k] in [t], and
      [Some p] if [p] is bound to [k] in [t]. *)
  val find_opt : 'a t -> key -> 'a tzresult Lwt.t option

  val mem : 'a t -> key -> bool

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** [iter_{s,p} f t] iterates [f] over the promises of [t]. It blocks on
      unresolved promises and only applies the function on the ones that resolve
      successfully. *)
  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** [fold f t init] folds [f] over the successfully resolving promises
      of [t]. I.e., it goes through the promises in the table and waits for each
      of the promise to resolve in order to fold over it. *)
  val fold : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  (** [fold_promises f t init] folds [f] over the promises of [t]. *)
  val fold_promises :
    (key -> 'a tzresult Lwt.t -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_resolved f t init] folds [f] over the successfully resolved promises
      of [t]. *)
  val fold_resolved : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_keys f t init] folds [f] over the keys bound in [t]. *)
  val fold_keys : (key -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
end

(** Intended use: [Make(Hashtbl.Make(M))]. *)
module Make (T : Hashtbl.S) : S with type key = T.key
