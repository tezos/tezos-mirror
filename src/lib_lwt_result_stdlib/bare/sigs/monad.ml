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

(** {1 Lwt, result, and Lwt-result monad operators}

    This module provides the necessary functions and operators to use Lwt,
    Result and Lwt-Result as monads.

    {2 Basics}

    The three, tiered monads have each their own syntax module with
    {ul
      {li a [return] function,}
      {li preallocated [return_unit], [return_none], etc. values,}
      {li [let*] and [let+] bindind operators.}
    }
    In addition, the {!Lwt_syntax} module has [and*] and [and+] binding
    operators to allow concurrent evaluation of two or more promises, and the
    {!Result_syntax} and {!Lwt_result_syntax} have [fail] functions to
    error-out.

    {2 Joins}

    The {!Lwt_syntax.join} function takes a list of promises [ps] and returns a
    single promise [p] that resolves with [()] when all the promises of [ps]
    have resolved.

    The {!Lwt_syntax.all} function takes a list of promises [ps] and returns a
    single promise [p] that resolves when all the promises of [ps] have
    resolved. The value [p] resolves to is the list of values the promises of
    [ps] resolve to. The order is preserved.

    The {!Lwt_syntax.both} function takes two promises [p1] and [p2] and returns
    a single promise [p] that resolves when both promises [p1] and [p2] have
    resolved. The value [p] resolves to is the tuple of values the promises [p1]
    and [p2] resolve to.

    These Lwt-joining functions have a best-effort semantic: they only resolve
    once all the underlying promises have resolved.

    The {!Result_syntax} variants are equivalent for the result monad: the final
    result is [Ok] if all the underlying results are [Ok].

    The {!Lwt_result_syntax} variants are equivalent for the Lwt-result monad:
    the final promise resolves to [Ok] if all the underlying promise resolve to
    [Ok].

    {2 Lifting}

    Finally, the {!Lwt_result_syntax} module includes two facilities for lifting
    values from the more specilaised Lwt-only and Result-only monads.

    [let*!] binds a plain Lwt promise into an Lwt-Result promise.

{[
let open Lwt_result_syntax in
let*! x = f a b c in
…
]}

    [let*?] binds a plain result into an Lwt-Result promise.

{[
let open Lwt_result_syntax in
let*? y = f u v w in
…
]}

    In the cases where performance is not a grave concern, it is also possible to
    use [Lwt_result.ok] to lift Lwt-only expressions and [Lwt.return] to lift
    result-only expressions.
    More details on the matter within the documentation of
    {!Lwt_result_syntax.( let*! )} and {!Lwt_result_syntax.( let*? )}
    themselves. *)

module type S = sig
  (** {1 The tower of monads} *)

  (** {2 The Lwt monad: for concurrency} *)

  (** Syntax module for Lwt. This is intended to be opened locally in functions
      which use Lwt for control-flow. Within the scope of this module, the code
      can include binding operators, leading to a [let]-style syntax.

      See also {!Lwt} and {!Lwt.Syntax} *)
  module Lwt_syntax : sig
    (** [return x] is an Lwt promise that is already resolved to [x]. [return]
        is an alias for {!Lwt.return}. *)
    val return : 'a -> 'a Lwt.t

    (** [return_unit] is an Lwt promise that is already resolved to [()]. It is
        an alias for [Lwt.return_unit]. *)
    val return_unit : unit Lwt.t

    (** [return_none] is an Lwt promise that is already resolved to [None]. It
        is an alias for [Lwt.return_none]. *)
    val return_none : _ option Lwt.t

    (** [return_nil] is an Lwt promise that is already resolved to [[]]. It is
        an alias for [Lwt.return_nil]. *)
    val return_nil : _ list Lwt.t

    (** [return_true] is an Lwt promise that is already resolved to [true]. It is
        an alias for [Lwt.return_true]. *)
    val return_true : bool Lwt.t

    (** [return_false] is an Lwt promise that is already resolved to [false]. It is
        an alias for [Lwt.return_false]. *)
    val return_false : bool Lwt.t

    (** [return_some x] is an Lwt promise that is already resolved to [Some x].
        [return_some] is an alias for [Lwt.return_some]. *)
    val return_some : 'a -> 'a option Lwt.t

    (** [return_ok x] is an Lwt promise that is already resolved to [Ok x].
        [return_ok] is an alias for [Lwt.return_ok]. *)
    val return_ok : 'a -> ('a, _) result Lwt.t

    (** [return_error x] is an Lwt promise that is already resolved to [Error x].
        [return_error] is an alias for [Lwt.return_error]. *)
    val return_error : 'e -> (_, 'e) result Lwt.t

    (** [let*] is a binding operator alias for {!Lwt.bind} and {!Lwt.( >>= )}. *)
    val ( let* ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

    (** [and*] is a binding operator alias for {!Lwt.both} and {!Lwt.( <&> )}. *)
    val ( and* ) : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

    (** [let+] is a binding operator alias for {!Lwt.map} and {!Lwt.( >|= )}. *)
    val ( let+ ) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

    (** [and+] is a binding operator alias for {!Lwt.both} and {!Lwt.( <&> )}. *)
    val ( and+ ) : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t

    (** [join] is the joining of concurrent unit values (it is [Lwt.join]). *)
    val join : unit Lwt.t list -> unit Lwt.t

    (** [all] is the joining of concurrent non-unit values (it is [Lwt.all]). *)
    val all : 'a Lwt.t list -> 'a list Lwt.t

    (** [both] is the joining of two concurrent non-unit values (it is [Lwt.both]). *)
    val both : 'a Lwt.t -> 'b Lwt.t -> ('a * 'b) Lwt.t
  end

  (** {2 The (generic) Result monad: for success/failure} *)

  (** Syntax module for Result. This is intended to be opened locally in
      functions which use [result] for control-flow. Within the scope of this
      module, the code can include binding operators, leading to a [let]-style
      syntax.

      See also {!Result} *)
  module Result_syntax : sig
    (** [return x] is [Ok x]. *)
    val return : 'a -> ('a, 'e) result

    (** [return_unit] is [Ok ()]. *)
    val return_unit : (unit, 'e) result

    (** [return_none] is [Ok None]. *)
    val return_none : ('a option, 'e) result

    (** [return_some x] is [Ok (Some x)]. *)
    val return_some : 'a -> ('a option, 'e) result

    (** [return_nil] is [Ok []]. *)
    val return_nil : ('a list, 'e) result

    (** [return_true] is [Ok true]. *)
    val return_true : (bool, 'e) result

    (** [return_false] is [Ok false]. *)
    val return_false : (bool, 'e) result

    (** Note that we do not provide [return_ok] nor [return_error]. Both of
        these functions are possible but somewhat confusing and rarely useful in
        practice. If you need to carry [result]s within a Result-monad
        computation (yielding to values of the type
        [(('a, 'e) result, 'e) result]), you need to do so by hand:
        [return (Ok …)] and [return (Error …)]. *)

    (** [fail e] is [Error e]. It is also an alias for [error]. *)
    val fail : 'e -> ('a, 'e) result

    (** [let*] is a binding operator alias for {!Result.bind} and [>>?]. *)
    val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

    (** [let+] is a binding operator alias for {!Result.map} and [>|?]. *)
    val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

    (** Note that we do not provide [and*] nor [and+]. Both of these are
        possible but their type is unsatisfying because the errors do not
        compose well. You can use [both] (below) if need be. *)

    (** [join] is the joining of success/failure unit values. *)
    val join : (unit, 'e) result list -> (unit, 'e list) result

    (** [all] is the joining of success/failure non-unit values. *)
    val all : ('a, 'e) result list -> ('a list, 'e list) result

    (** [both] is the joining of two success/failure non-unit values. *)
    val both : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e list) result
  end

  (** {2 The combined Lwt+Result monad: for concurrent successes/failures} *)

  (** Syntax module for Lwt+Result. This is intended to be opened locally in
      functions which use Lwt and [result] for control-flow. Within the scope of
      this module, the code can include binding operators, leading to a
      [let]-style syntax.

      See also {!Lwt}, {!Result}, and {!Lwt_result}. *)
  module Lwt_result_syntax : sig
    (** [return x] is [Lwt.return (Ok x)] or [Lwt_result.return x]. *)
    val return : 'a -> ('a, 'e) result Lwt.t

    (** [return_unit] is [Lwt.return (Ok ())] . *)
    val return_unit : (unit, 'e) result Lwt.t

    (** [return_none] is [Lwt.return (Ok None)] . *)
    val return_none : ('a option, 'e) result Lwt.t

    (** [return_some x] is [Lwt.return (Ok (Some x))] . *)
    val return_some : 'a -> ('a option, 'e) result Lwt.t

    (** [return_nil] is [Lwt.return (Ok [])] . *)
    val return_nil : ('a list, 'e) result Lwt.t

    (** [return_true] is [Lwt.return (Ok true)] . *)
    val return_true : (bool, 'e) result Lwt.t

    (** [return_false] is [Lwt.return (Ok false)] . *)
    val return_false : (bool, 'e) result Lwt.t

    (** Note that we do not provide [return_ok] nor [return_error]. Both of
        these functions are possible but somewhat confusing and rarely useful in
        practice. If you need to carry [result]s within a LwtResult-monad
        computation (yielding values of the type
        [(('a, 'e) result, 'e) result Lwt.t]), you need to do so by hand:
        [return (Ok …)] and [return (Error …)]. *)

    (** [fail e] is [Lwt.return (Error e)]. *)
    val fail : 'e -> ('a, 'e) result Lwt.t

    (** [let*] is a binding operator alias for {!Lwt_result.bind}. *)
    val ( let* ) :
      ('a, 'e) result Lwt.t ->
      ('a -> ('b, 'e) result Lwt.t) ->
      ('b, 'e) result Lwt.t

    (** [let+] is a binding operator alias for {!Lwt_result.map}. *)
    val ( let+ ) : ('a, 'e) result Lwt.t -> ('a -> 'b) -> ('b, 'e) result Lwt.t

    (** Note that we do not provide [and*] nor [and+]. Both of these are
        possible but their type is unsatisfying because the errors do not
        compose well. You can use [both] (below) if need be. *)

    (** [lwt_map_error] is an Lwt-aware variant of {!Result.map_error}. It is
        intended for mapping the errors of Lwt-result values. The main use of
        this function is for mixing results that carry different types of
        errors.

        E.g., considering [fetch : unit -> (string, unit) result Lwt.t] and
        [emit : string -> (unit, int) result Lwt.t], you can write

{[
let* data = lwt_map_error (fun () -> "fetching failed") @@ fetch () in
let* () =
  lwt_map_error (fun code -> Format.asprintf "emit failed (%d)")
  @@ emit data
in
..
]}

        *)
    val lwt_map_error :
      ('e -> 'f) -> ('a, 'e) result Lwt.t -> ('a, 'f) result Lwt.t

    (** The following values are for mixing expressions that are Lwt-only or
        Result-only within the LwtResult monad. Note that there are fundamental
        differences between [result] and [Lwt.t]: the former can be simply
        matched on (i.e., it is possible to get out of the monad at any point)
        whereas the latter can only be bound on (i.e., it is not possible to get
        out of the monad). In addition, the former is for aborting computations
        on failures whereas the latter is for waiting before continuing.

        Still, from a syntax point-of-view, both are handled the same way: with
        a specialised binding operator. *)

    (** [let*!] is for binding Lwt-only expressions into the LwtResult combined
        monad.

{[
let open Lwt_result_syntax in
let* x = … in
let*! y = … in
return (x + y)
]}

        *)
    val ( let*! ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

    (** [let*?] is for binding the value from Result-only
        expressions into the LwtResult combined monad.

{[
let open Lwt_result_syntax in
let* x = … in
let*? y = … in
…
]} *)
    val ( let*? ) :
      ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

    (** Note that you can mix [let*], [let*!], and [let*?] as needed, within a
        single expression.

{[
let do_thing param =
  let open Lwt_result_syntax in
  let*? () = check_p param in (* Result-only for parameter checking *)
  let*! () = log "starting doing the thing" in (* Lwt-only for infallible logging *)
  let* r = thing param in
  let*! () = log "done doing the thing" in (* Lwt-only for infallible logging *)
  return r
]} *)

    (** [join] is the joining of concurrent success/failure unit values. *)
    val join : (unit, 'e) result Lwt.t list -> (unit, 'e list) result Lwt.t

    (** [all] is the joining of concurrent success/failure non-unit values. *)
    val all : ('a, 'e) result Lwt.t list -> ('a list, 'e list) result Lwt.t

    (** [both] is the joining of two concurrent success/failure non-unit values. *)
    val both :
      ('a, 'e) result Lwt.t ->
      ('b, 'e) result Lwt.t ->
      ('a * 'b, 'e list) result Lwt.t
  end
end
