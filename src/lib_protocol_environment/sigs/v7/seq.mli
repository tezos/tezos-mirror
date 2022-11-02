(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From Lwtreslib *)

type 'a t = unit -> 'a node

and +'a node = Nil | Cons of 'a * 'a t

val empty : 'a t

val return : 'a -> 'a t

val cons : 'a -> 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val iter : ('a -> unit) -> 'a t -> unit

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t

(** {3 Lwtreslib-specific extensions} *)

(** [first s] is [None] if [s] is empty, it is [Some x] where [x] is the
    first element of [s] otherwise.

    Note that [first] forces the first element of the sequence, which can have
    side-effects or be computationally expensive. Consider, e.g., the case
    where [s = filter (fun …) s']: [first s] can force multiple of the values
    from [s']. *)
val first : 'a t -> 'a option

(** Similar to {!fold_left} but wraps the traversal in {!result}. The
    traversal is interrupted if one of the step returns an [Error _]. *)
val fold_left_e :
  ('a -> 'b -> ('a, 'trace) result) -> 'a -> 'b t -> ('a, 'trace) result

(** Similar to {!fold_left} but wraps the traversing in {!Lwt}. Each step of
    the traversal is started after the previous one has resolved. The
    traversal is interrupted if one of the promise is rejected. *)
val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t

(** Similar to {!fold_left} but wraps the traversing in [result Lwt.t].
    Each step of the traversal is started after the previous one resolved. The
    traversal is interrupted if one of the step is rejected or is fulfilled
    with [Error _]. *)
val fold_left_es :
  ('a -> 'b -> ('a, 'trace) result Lwt.t) ->
  'a ->
  'b t ->
  ('a, 'trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in {!result}. The iteration
    is interrupted if one of the step returns an [Error _]. *)
val iter_e : ('a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

(** Similar to {!iter} but wraps the iteration in {!Lwt}. Each step
    of the iteration is started after the previous one resolved. The iteration
    is interrupted if one of the promise is rejected. *)
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

(** Similar to {!iter} but wraps the iteration in [result Lwt.t]. Each step
    of the iteration is started after the previous one resolved. The iteration
    is interrupted if one of the promise is rejected of fulfilled with an
    [Error _]. *)
val iter_es :
  ('a -> (unit, 'trace) result Lwt.t) -> 'a t -> (unit, 'trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in [result Lwt.t]. All the
    steps of the iteration are started concurrently. The promise [iter_ep]
    resolves once all the promises of the traversal resolve. At this point it
    either:
    - is rejected if at least one of the promises is, otherwise
    - is fulfilled with [Error _] if at least one of the promises is,
      otherwise
    - is fulfilled with [Ok ()] if all the promises are. *)
val iter_ep :
  ('a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
  'a t ->
  (unit, 'error Error_monad.trace) result Lwt.t

(** Similar to {!iter} but wraps the iteration in {!Lwt}. All the
    steps of the iteration are started concurrently. The promise [iter_p f s]
    is resolved only once all the promises of the iteration are. At this point
    it is either fulfilled if all promises are, or rejected if at least one of
    them is. *)
val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
