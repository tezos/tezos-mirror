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

(** Pipelines: carry a bundle of inputs through a series of transformations
    whilst preserving the order of the original input. *)

(** Steps are the building blocks of pipeline. A step is essentially a function
    from a given type to another. *)
type ('a, 'b) step

(** [sync f] is the function [f] as a synchronous step. Data that is transformed
    by a synchronous function is never buffered. Instead it is transformed as it
    arrives. *)
val sync: ('a -> 'b) -> ('a, 'b) step

(** [async_s f] is the function [f] as an asynchronous serial step. There is
    only ever at most one unresolved promise created by this function. If data
    arrives at this step and the function as already been called, the data is
    buffered until the promise created by the call resolves. *)
val async_s: ('a -> 'b Lwt.t) -> ('a, 'b) step

(** [async_p f] is the function [f] as an asynchronous parallel step. Multiple
    unresolved promise for this step can be unresolved at the same time. Data
    might still be buffered if the global limit on unresolved promises is
    reached. *)
val async_p: ('a -> 'b Lwt.t) -> ('a, 'b) step

(** Error management: these steps are helpers for managing errors through the
    [result] type. *)

(** [all_ok] is [sync (fun x -> Ok x)] and it is meant to inject all of the
    available data into the [result] type. *)
val all_ok:
  ('a, ('a, 'b) result) step

(** [map_in_err f s] is a step with the same synchronicity as [s]. On [Ok] data
    it acts the same as [s], but [Error] data is modified by [f] before being
    handled normally by [s]. *)
val map_in_err:
  ('erra -> 'errb) ->
  (('a, 'errb) result, 'b) step ->
  (('a, 'erra) result, 'b) step

(** [map_out_err f s] is a step with the same synchronicity as [s]. On [Ok] data
    it acts the same as [s], but [Error] data is modified by [f] after being
    handled normally by [s]. *)
val map_out_err:
  ('erra -> 'errb) ->
  ('a, ('b, 'erra) result) step ->
  ('a, ('b, 'errb) result) step

(** [with_err s] is a step with the same synchronicity as [s]. It acts as [s] on
    [Ok] data and it is a no-op on [Error] data. *)
val with_err:
  ('a, ('b, 'err) result) step ->
  (('a, 'err) result, ('b, 'err) result) step

(** [recover f] is [sync (function | Ok v -> v | Error e -> f e)]: it maps the
    [Error] data onto the same type as the [Ok] data and exits the [result]
    type. *)
val recover:
  ('err -> 'a) ->
  (('a, 'err) result, 'a) step

(** Carrying ID through a pipeline *)
val with_key:
  ('a, 'b) step ->
  (('key * 'a), ('key * 'b)) step
val init_key: ('a, ('a * 'a)) step


(** Pipelines are essentially lists of steps. *)
(* Recommended use: [cons f @@ cons g @@ nil] *)

type ('i, 'o) pipe
val nil: ('x, 'x) pipe
val cons: ('a, 'b) step -> ('b, 'c) pipe -> ('a, 'c) pipe


(** Core functionality:
    [run ?pool pipe input] runs all the elements of [input] through the steps of
    [pipeline]. All the while it maintains the following invariants:
    - There are never more than [pool] unresolved high-level promises at any one
    time. A high-level promise is one that corresponds to a call to one of the
    step function. (Note that each such promise can create additional promises
    which are not limited by the [pool] parameter of [run].) By default, impose
    no limits at all.
    - The elements maintain the same ordering all throughout the pipeline. In
    other words, if [x] is before [y] in [input], then for any step [s], the
    high-level promise of [s] for [x] will be created before the high-level
    promise of [s] for [y].
*)
val run: ?pool:int -> ('i, 'o) pipe -> 'i list -> ('o, exn) result list Lwt.t

(** Post-processing: useful to deal with pipeline built around error management
    or id marking combinators. *)
val partition_by_error:
  (('o, 'err) result, exn) result list ->
  ('o list * 'err list * exn list)
val index_by_key:
  (('key * 'o), exn) result list ->
  'index ->
  ('key -> 'o -> 'index -> 'index) ->
  'index * exn list
