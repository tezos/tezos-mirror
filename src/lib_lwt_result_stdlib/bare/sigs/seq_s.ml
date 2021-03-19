(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The [S] signature is similar to {!Seq.S} except that suspended nodes are
    wrapped in a promise.

    This allows some additional traversors ([map_s], etc.) to be applied lazily.

    The functions [of_seq] and [of_seq_s] allow conversion from vanilla
    sequences. *)
module type S = sig
  (** This is similar to [S.t] but the suspended node is a promise *)
  type +'a node = Nil | Cons of 'a * 'a t

  and 'a t = unit -> 'a node Lwt.t

  val empty : 'a t

  val return : 'a -> 'a t

  val return_s : 'a Lwt.t -> 'a t

  val cons : 'a -> 'a t -> 'a t

  val cons_s : 'a Lwt.t -> 'a t -> 'a t

  val append : 'a t -> 'a t -> 'a t

  (** Similar to {!fold_left} but applies to Lwt-suspended sequences. Because
      the nodes are suspended in promises, traversing may yield and,
      consequently, the function [fold_left] returns a promise. *)
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a Lwt.t

  (** Similar to {!fold_left} but wraps the traversal in {!result}. The
      traversal is interrupted if one of the step returns an [Error _]. *)
  val fold_left_e :
    ('a -> 'b -> ('a, 'trace) result) ->
    'a ->
    'b t ->
    ('a, 'trace) result Lwt.t

  (** Similar to {!fold_left} but the folder is within Lwt. *)
  val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t

  (** Similar to {!fold_left} but the folder is within result-Lwt. Traversal is
      interrupted if one of the step resolves to an [Error _]. *)
  val fold_left_es :
    ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
    'a ->
    'b t ->
    ('a, 'trace) result Lwt.t

  (** [iter f s] applies [f] to each element of [s]. *)
  val iter : ('a -> unit) -> 'a t -> unit Lwt.t

  (** Similar to {!iter} but wraps the iteration in {!result}. The iteration
      is interrupted if one of the steps returns an [Error _]. *)
  val iter_e :
    ('a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result Lwt.t

  (** Similar to {!iter} but wraps the iteration in {!Lwt}. Each step
      of the iteration is started after the previous one is resolved. *)
  val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** Similar to {!iter} but wraps the iteration in [result Lwt.t]. Each step
      of the iteration is started after the previous one resolved. The iteration
      is interrupted if one of the promise is rejected of fulfilled with an
      [Error _]. *)
  val iter_es :
    ('a -> (unit, 'trace) result Lwt.t) -> 'a t -> (unit, 'trace) result Lwt.t

  (** Similar to {!iter} but wraps the iteration in [result Lwt.t]. The
      steps of the iteration are started concurrently: one iteration starts
      as soon as a node becomes resolved. The promise [iter_ep]
      resolves once all the promises of the traversal resolve. At this point it
      either:
      - is rejected if at least one of the promises is, otherwise
      - is fulfilled with [Error _] if at least one of the promises is,
        otherwise
      - is fulfilled with [Ok ()] if all the promises are. *)
  val iter_ep :
    ('a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace list) result Lwt.t

  (** Similar to {!iter} but wraps the iteration in {!Lwt}. The
      steps of the iteration are started concurrently: one iteration is started
      as soon as the node becomes resolved. The promise [iter_p f s]
      is resolved only once all the promises of the iteration are. At this point
      it is either fulfilled if all promises are, or rejected if at least one of
      them is. *)
  val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t

  val filter : ('a -> bool) -> 'a t -> 'a t

  (** Similar to {!filter} but wraps the transformation in {!Lwt.t}. Each
      test of the predicate is done sequentially, only starting once the
      previous one has resolved. *)
  val filter_s : ('a -> bool Lwt.t) -> 'a t -> 'a t

  val filter_map : ('a -> 'b option) -> 'a t -> 'b t

  (** Similar to {!filter_map} but within [Lwt.t]. Not lazy and not
      tail-recursive. *)
  val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t

  val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t

  val unfold_s : ('b -> ('a * 'b) option Lwt.t) -> 'b -> 'a t

  val of_seq : 'a Stdlib.Seq.t -> 'a t

  val of_seq_s : 'a Lwt.t Stdlib.Seq.t -> 'a t
end
