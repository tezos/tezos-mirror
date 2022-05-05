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

(** {1 Seq}

    A replacement for {!Stdlib.Seq} which
    - is exception-safe,
    - includes Lwt-, result- and Lwt-result-aware traversal functions.

    See {!Lwtreslib} for a general description of traversors and the meaning for
    the name suffixes. A full description is also below.

    All traversal functions that are suffixed with [_e] are within the result
    monad. Note that these functions have a "fail-early" behaviour: the
    traversal is interrupted as soon as any of the intermediate application
    fails (i.e., returns an [Error _]).

    All traversal functions that are suffixed with [_s] are within the Lwt
    monad. These functions traverse the elements sequentially: the promise for a
    given step of the traversal is only initiated when the promise for the
    previous step is resolved. Note that these functions have a fail-early
    behaviour: the traversal is interrupted if any of the intermediate promise
    is rejected.

    All the traversal functions that are suffixed with [_p] are within Lwt.
    These functions traverse the elements concurrently: the promise for all the
    steps are created immediately. The suffix [_p] is chosen for similarity with
    the {!Lwt_list} functions even though, as with {!Lwt_list}'s functions there
    is no parallelism involved, only concurrency. Note that these functions have
    a “best-effort” behaviour: the whole-traversal promise (i.e., the promise
    returned by the [_p]-suffixed function) only resolves once each of the step
    promises have resolved. Even if one of the step promise is rejected, the
    whole-traversal promise is only rejected once all the other step promises
    have resolved.

    All the traversal functions that are suffixed with [_es] are within the
    combined error-and-Lwt monad. These function traverse the elements
    sequentially with a fail-early behaviour for both rejection (as an Lwt
    promise) and failure (as a result).

    All the traversal functions that are suffixed with [_ep] are within the
    combined error-and-Lwt monad. These function traverse the elements
    concurrently with a best-effort behaviour.
*)

(** {2 Special consideration}

    Because of the type of {!Stdlib.Seq.t}, some interactions with Lwt are not
    possible. Specifically, note that the type includes the variant
    [unit -> 'a node] which is not within Lwt nor within the result monad.
    As a result, some of the traversals ([map_s], [map_e], etc.) cannot be
    applied lazily.

    Check-out the [S] variants ({!Seq_s.S}, {!Seq_e.S}, and
    {!Seq_es.S}) that integrate the base sequence type better with the monads'
    type. It is recommended that you use the variant as appropriate to your
    traversal. Note the presence of [of_seq] in each of those variants to
    convert from the standard [S.t]. *)

module type S = sig
  (** {3 Common interface with Stdlib} *)

  include
    module type of Stdlib.Seq
      with type 'a t = 'a Stdlib.Seq.t
       and type 'a node = 'a Stdlib.Seq.node

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
    ('a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace list) result Lwt.t

  (** Similar to {!iter} but wraps the iteration in {!Lwt}. All the
      steps of the iteration are started concurrently. The promise [iter_p f s]
      is resolved only once all the promises of the iteration are. At this point
      it is either fulfilled if all promises are, or rejected if at least one of
      them is. *)
  val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** {3 Values which have made it to the Stdlib since then}

      This section is for forward compatibility: bringing you the features of
      more recent OCaml Stdlib than we compile against. *)

  (** [concat s] is a sequence containing the elements of the elements of [s]. *)
  val concat : 'a t t -> 'a t

  (** [concat_map] is an alias for {!flat_map} *)
  val concat_map : ('a -> 'b t) -> 'a t -> 'b t
end
