(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** {1:intro Lwtreslib: the Lwt- and result-aware Stdlib complement}

    Lwtreslib (or Lwt-result-stdlib) is a library to complement the OCaml's
    Stdlib in software projects that make heavy use of Lwt and the result type.

    {2 Introduction}

    Lwtreslib aims to
    {ul
      {li Replace exception-raising functions with exception-safe one. E.g.,
          functions that may raise {!Not_found} in the Stdlib are
          shadowed by functions that return an {!option}.}
      {li Provide an extensive set of Lwt-, result- and Lwt-result-traversors
          for the common data-types of the Stdlib. E.g., {!List.map} is
          available alongside [List.map_s] for Lwt sequential traversal,
          [List.map_e] for result traversal, etc.}
      {li Provide a uniform semantic, especially regarding error management.
          E.g., all sequential traversal functions have the same fail-early
          semantic, whereas all concurrent traversal functions have the same
          best-effort semantic.}
      {li Provide good documentation.}
    }

    {2 Semantic}

    The semantic of the functions exported by Lwtreslib is uniform and
    predictable. This applies to the Stdlib-like functions, the Lwt-aware
    functions, the result-aware functions, and the Lwt-and-result-aware
    functions.

    {3 Semantic of vanilla-functions}

    Functions that have the same signature as their Stdlib's counterpart have
    the same semantic.

    Functions exported by Lwtreslib do not raise exceptions. (With the exception
    of the functions exported by the {!WithExceptions} module.) If a function
    raises an exception in the Stdlib, its type is changed in Lwtreslib. In
    general the following substitutions apply:

    {ul
      {li Functions that may raise {!Not_found} (e.g., [List.find]) return an
          {!option} instead.}
      {li Functions that may fail because of indexing errors (e.g., [List.nth],
          [List.hd], etc.) also return an {!option} instead.}
      {li Functions that may raise {!Invalid_argument} (e.g., [List.iter2])
          return a {!result} type instead. They take an additional argument
          indicating what [Error] to return instead of the exception.}
    }

    {3 Semantic of Lwt-aware functions}

    Lwtreslib exports Lwt-aware functions for all traversal functions of the
    Stdlib.

    Functions with the [_s] suffix traverse their underlying collection
    sequentially, waiting for the promise associated to one element to resolve
    before processing to the next element. Note that for the [Seq*] modules (see
    below) the sequential traversors are bundled under an [S] submodules rather
    than suffixed with [_s].

    Functions with the [_p] suffix traverse their underlying collection
    concurrently, creating promises for all the elements and then waiting for
    all of them to resolve. The "p" in the [_p] suffix is for compatibility with
    Lwt and in particular [Lwt_list]. The mnemonic is "parallel" even though
    there is not parallelism, only concurrency.

    These [_s]- and [_p]-suffixed functions are semantically identical to their
    Lwt counterpart when it is available. Most notably, [Lwtreslib.List] is a
    strict superset of [Lwt_list].

    {3 Semantic of result-aware functions}

    Lwtreslib exports result-aware functions for all the traversal functions of
    the Stdlib. These function allow easy manipulation of [('a, 'e) result]
    values.

    Functions with the [_e] suffix traverse their underlying collection whilst
    wrapping the accumulator/result in a [result]. These functions have a
    fail-early semantic: if one of the steps returns an [Error _], then the whole
    traversal is interrupted and returns the same [Error _]. Note that for the
    [Seq*] modules (see below) the result-aware traversors are bundled under an
    [E] submodules rather than suffixed with [_e].

    {3 Semantic of Lwt-result-aware functions}

    Lwtreslib exports Lwt-result-aware functions for all the traversal functions
    of the Stdlib. These function allow easy manipulation of
    [('a, 'e) result Lwt.t] -- i.e., promises that may fail.

    Functions with the [_es] suffix traverse their underlying collection
    sequentially (like [_s] functions) whilst wrapping the accumulator/result in
    a [result] (like [_e] functions). These functions have a fail-early
    semantic: if one of the step returns a promise that resolves to an
    [Error _], then the whole traversal is interrupted and the returned promise
    resolves to the same [Error _]. Note that for the [Seq*] modules (see below)
    the Lwt-result-aware traversors are bundled under an [ES] submodules rather
    than suffixed with [_es].

    Functions with the [_ep] suffix traverse their underlying collection
    concurrently (like [_p] functions) whilst wrapping the accumulator/result in
    a [result] (like [_e] functions). These functions have a best-effort
    semantic: if one of the step returns a promise that resolves to an
    [Error _], the other promises are left to resolve; once all the promises
    have resolved, then the returned promise resolves with an [Error _] that
    carries all the other errors in a list. It is up to the user to convert this
    list to a more manageable type if needed.

    {3 A note on [Seq]}

    The [Seq] module exports a type that suspends nodes under a closure.
    Consequently, some interactions with result, Lwt, and result-Lwt is not
    possible. E.g., [map]ping can be either lazy or within Lwt but not both:
    [Seq.map_s] would have type [('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t] where
    the returned promise forces the whole sequence (and never resolves on
    infinite sequences).

    In Lwtreslib, [Seq] does not provide these additional transformers that would
    force the sequence simply due to the bad interaction of the Monads and the
    type of sequences. Instead, Lwtreslib provides

    - A subset of traversors where the laziness and the monad mix well (e.g.,
    [iter] but not [map]). These are exported under the modules [S], [E] and
    [ES].

    - Variants of [Seq] called [Seq_e], [Seq_s], and [Seq_es] where the
    combination with the monad is baked into the sequence type itself.

    If you want to map a sequnence using an Lwt-returning function, you should
    first convert the sequence to an Lwt-aware sequence using [Seq_s.of_seq],
    and then map this converted function using [Seq_s.S.map].
    Note that this returns a [Seq_s.t] sequence so further transformations will
    be within [Seq_s] and not within [Seq]. Once in a monad, you stay in the
    monad.

    {3 [Traced]}

    The {!Traced} module offers a small wrapper around Lwtreslib. This wrapper
    is intended to ease the use of [_ep] functions. It does so by introducing a
    trace data-type: a structured collection of errors.

    This trace data-type is used to collapse the types ['e] and ['e list] of
    errors. Indeed, without this collapse, chaining [_ep] together or chaining
    [_ep] with [_es] functions requires significant boilerplate to flatten
    lists, to listify single errors, etc. Need for boilerplate mostly vanishes
    when using the [Traced] wrapper.

    {2 Monad helpers}

    Lwtreslib also exports monadic operators (binds, return, etc.) for the
    Lwt-monad, the result-monad, and the combined Lwt-result-monad.

    {2 Exceptions}

    If at all possible, avoid exceptions.

    If possible, avoid exceptions.

    If you use exceptions, here are a few things to keep in mind:

    The [_p] functions are semantically equivalent to Lwt's. This means that
    some exceptions are dropped. Specifically, when more than one promise raises
    an exception in a concurrent traversor, only one is passed on to the user,
    the others are silently ignored.

    Use [raise] (rather than [Lwt.fail]) when within an Lwt callback.

    {2 [WithExceptions]}

    The [WithExceptions] module is there for convenience in non-production code
    and for the specific cases where it is guaranteed not to raise an exception.

    E.g., it is intended for removing the {!option} boxing in cases where the
    invariant is guaranteed by construction:

{[
(** Return an interval of integers, from 0 to its argument (if positive)
    or from its argument to 0 (otherwise). *)
let steps stop =
   if stop = 0 then
      []
   else if stop > 0 then
      List.init ~when_negative_length:() Fun.id
      |> WithExceptions.Option.get ~loc:__LOC__
   else
      let stop = Int.neg stop in
      List.init ~when_negative_length:() Int.neg
      |> WithExceptions.Option.get ~loc:__LOC__
]} *)

(** {1 Instance: [Bare]}

    [Bare] provides all the functions as described above. It is intended to be
    opened to shadow some modules of [Stdlib].

    All values within the modules follow the same naming and semantic
    conventions described above. The sequential traversors are fail-early:
    in the following example the code returns an [Error] and does not print
    anything.

{[
List.iter_e
   (fun x ->
      if x = "" then
         Error "empty string"
      else begin
         print_endline x;
         Ok ())
   [
      ""; (* This will cause the iteration to stop *)
      "this is not printed";
      "neither is this printed";
   ]
]}

    The concurrent (parallel) traversors are best-effort: in the following
    example the code prints all the non-empty strings in an unspecified order
    before returning an [Error].

{[
List.iter_ep
   (fun x ->
      if x = "" then
         Lwt.return (Error "empty string")
      else begin
         print_endline x;
         Lwt.return (Ok ()))
   [
      ""; (* This will cause the iteration to error in the end *)
      "this is printed";
      "this is printed as well";
   ]
]}

    The module [WithExceptions] provides some exception-raising helpers to
    reduce the boilerplate that the library imposes.

    {2 Comparison, Equality, etc.}

    When a function requires a comparison function, it takes a [compare] named
    parameter. This must define a total order as described in
    {!Stdlib.Map.OrderedType}.

    Note that the polymorphic structural comparison {!Stdlib.compare} is unsound
    for comparing some values; notably, it may fail when comparing
    data-structures that include functions or closures.

    Similarly and for the same reason, some functions take an [equal] function.
*)
module Bare : sig
  module Hashtbl : Bare_sigs.Hashtbl.S

  module List : Bare_sigs.List.S

  module Map : Bare_sigs.Map.S

  module Monad : Bare_sigs.Monad.S

  module Option : Bare_sigs.Option.S

  module Result : Bare_sigs.Result.S

  module Seq : Bare_sigs.Seq.S

  module Seq_e : Bare_sigs.Seq_e.S

  module Seq_s : Bare_sigs.Seq_s.S

  module Seq_es :
    Bare_sigs.Seq_es.S
      with type ('a, 'e) seq_e_t := ('a, 'e) Seq_e.t
       and type 'a seq_s_t := 'a Seq_s.t

  module Set : Bare_sigs.Set.S

  module Unit : Bare_sigs.Unit.S

  module WithExceptions : Bare_sigs.WithExceptions.S
end

(** A module with the [TRACE] signature provides the necessary type and functions
    to collect multiple errors into a single error data-structure. This, in turn,
    allows Lwtreslib to provide more usable [_ep] variants to standard traversal
    functions. *)
module type TRACE = Traced_sigs.Trace.S

module type TRACED_MONAD = Traced_sigs.Monad.S

(** [Traced] is a functor to generate advanced combined-monad replacements
    for parts of the Stdlib. The generated module is similar to [Bare] with the
    addition of traces: structured collections of errors.

    For convenience, the monad includes primitives to error directly with a
    trace rather than a bare error.

    All the [_ep] traversors return traces of errors rather than lists of
    errors. The [_ep] traversors preserve their best-effort semantic.

    Additional functions in the [Monad] allow the construction of sequential
    traces: functions to enrich traces with new errors. E.g.,

{[
let load_config file =
   Result.map_error
     (fun trace ->
        Trace.cons "cannot load configuration file" trace)
   @@ begin
     let open Lwt_result_syntax in
     let* file = open_file in
     let* lines = read_lines file in
     let* json = parse_config lines in
     make_dictionary json
   end
]}

    Example implementations of traces are provided in the [traces/] directory.
*)
module Traced (Trace : TRACE) : sig
  module Monad : TRACED_MONAD with type 'error trace = 'error Trace.trace

  module Hashtbl :
    Traced_sigs.Hashtbl.S with type 'error trace := 'error Trace.trace

  module List : Traced_sigs.List.S with type 'error trace := 'error Trace.trace

  module Map : Traced_sigs.Map.S with type 'error trace := 'error Trace.trace

  module Option : Traced_sigs.Option.S

  module Result : Traced_sigs.Result.S

  module Seq : Traced_sigs.Seq.S with type 'error trace := 'error Trace.trace

  module Seq_e : Traced_sigs.Seq_e.S

  module Seq_s :
    Traced_sigs.Seq_s.S with type 'error trace := 'error Trace.trace

  module Seq_es :
    Traced_sigs.Seq_es.S
      with type ('a, 'e) seq_e_t := ('a, 'e) Seq_e.t
       and type 'a seq_s_t := 'a Seq_s.t

  module Set : Traced_sigs.Set.S with type 'error trace := 'error Trace.trace

  module Unit : Traced_sigs.Unit.S

  module WithExceptions : Traced_sigs.WithExceptions.S
end
