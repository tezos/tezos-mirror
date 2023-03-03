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

    Note the [Seq] module (along with the [Seq_*] modules) (unlike other
    modules of Lwtreslib) uses submodules to organise different monadic
    traversors. This is because the implementation of the [Seq] module is
    delegated to the [Seqes] library which uses functors which produces
    (sub)modules.

    All traversal functions that are inside the [E] submodule are within the
    result monad. Note that these functions have a "fail-early" behaviour: the
    traversal is interrupted as soon as any of the intermediate application
    fails (i.e., returns an [Error _]).

    All traversal functions that are inside the [S] submodule are within the Lwt
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

    All the traversal functions that are inside the [ES] submodule are within
    the combined error-and-Lwt monad. These function traverse the elements
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
    As a result, some of the traversals ([S.map], [E.map], etc.) cannot be
    applied lazily.

    Check-out the [S] variants ({!Seq_s.S}, {!Seq_e.S}, and
    {!Seq_es.S}) that integrate the base sequence type better with the monads'
    type. It is recommended that you use the variant as appropriate to your
    traversal. Note the presence of [of_seq] in each of those variants to
    convert from the standard [S.t]. *)

module type S = sig
  (** {3 Common interface with Stdlib}

      Note that some functions (namely [init], [take], and [drop]) are shadowed
      with exception-less versions.

      Note that [once] is not shadowed. Be careful when using [once]: the
      resulting sequence is {e ephemeral} and using in a non-ephemeral way
      raises an exception. As a safer alternative, you can use
      [Seq_e.of_seq_once] which gives you a result-based (exception-less)
      ephemeral sequence. *)
  include
    module type of Stdlib.Seq
      with type 'a t = 'a Stdlib.Seq.t
       and type 'a node = 'a Stdlib.Seq.node

  (** {3 Lwtreslib-specific safety-shadowing} *)

  val init :
    when_negative_length:'err -> int -> (int -> 'a) -> ('a t, 'err) result

  val take : when_negative_length:'err -> int -> 'a t -> ('a t, 'err) result

  val drop : when_negative_length:'err -> int -> 'a t -> ('a t, 'err) result

  module E :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) mon := ('a, 'e) result
      with type ('a, 'e) callermon := ('a, 'e) result
      with type ('a, 'e) t := 'a Stdlib.Seq.t

  module S :
    Seqes.Sigs.SEQMON1TRAVERSORS
      with type 'a mon := 'a Lwt.t
      with type 'a callermon := 'a Lwt.t
      with type 'a t := 'a Stdlib.Seq.t

  module ES :
    Seqes.Sigs.SEQMON2TRAVERSORS
      with type ('a, 'e) mon := ('a, 'e) result Lwt.t
      with type ('a, 'e) callermon := ('a, 'e) result Lwt.t
      with type ('a, 'e) t := 'a Stdlib.Seq.t

  (** {3 Lwtreslib-specific extensions} *)

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

  (** Similar to {!iteri} but wraps the iteration in [result Lwt.t]. All the
      steps of the iteration are started concurrently. The promise [iteri_ep]
      resolves once all the promises of the traversal resolve. At this point it
      either:
      - is rejected if at least one of the promises is, otherwise
      - is fulfilled with [Error _] if at least one of the promises is,
        otherwise
      - is fulfilled with [Ok ()] if all the promises are. *)
  val iteri_ep :
    (int -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace list) result Lwt.t

  (** Similar to {!iteri} but wraps the iteration in {!Lwt}. All the
      steps of the iteration are started concurrently. The promise [iter_p f s]
      is resolved only once all the promises of the iteration are. At this point
      it is either fulfilled if all promises are, or rejected if at least one of
      them is. *)
  val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** Similar to {!iter2} but wraps the iteration in [result Lwt.t]. All the
      steps of the iteration are started concurrently. The promise
      [iter2_ep f s1 s2] resolves once all the promises of the traversal resolve.
      At this point it is either:
      - rejected if at least one of the promises is,
      - fulfilled with [Error _] if at least one of the promises is,
      - fulfilled with [Ok ()] if all of the promises are.

      Note that similarly to {!Stdlib.Seq.iter2} this function iterates on the
      common-length prefix of the two sequences. As a result, the iteration can
      be successful even if the two sequences are of different lengths. *)
  val iter2_ep :
    ('a -> 'b -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    'b t ->
    (unit, 'trace list) result Lwt.t

  (** Similar to {!iter2} but wraps the iteration in {!Lwt}. All the
      steps of the iteration are started concurrently. The promise
      [iter2_p f s1 s2] resolves once all the promises of the traversal resolve.
      At this point it is either:
      - rejected if at least one of the promises is,
      - fulfilled with [()] if all of the promises are.

      Note that similarly to {!Stdlib.Seq.iter2} this function iterates on the
      common-length prefix of the two sequences. As a result, the iteration can
      be successful even if the two sequences are of different lengths. *)
  val iter2_p : ('a -> 'b -> unit Lwt.t) -> 'a t -> 'b t -> unit Lwt.t
end
