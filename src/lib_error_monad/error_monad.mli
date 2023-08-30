(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Error Monad *)

(** {1 Categories of error}

    Note: this is only meaningful within the protocol. It may be removed from
    the error monad and pushed to the protocol environment in the future.
    See https://gitlab.com/tezos/tezos/-/issues/1576 *)
type error_category =
  [ `Branch  (** Errors that may not happen in another context *)
  | `Temporary  (** Errors that may not happen in a later context *)
  | `Permanent  (** Errors that will happen no matter the context *) ]

(** {1 Assembling the different components of the error monad.} *)

(** The main error type.

    Whenever you add a variant to this type (with [type Error_monad.error += …])
    you must also register the error with {!register_error_kind}.

    These errors are not meant to be inspected in general. Meaning that they
    should not be matched upon. Consequently it is acceptable to register an
    error in an implementation file and not mention it in the corresponding
    interface file. *)
type error = TzCore.error = ..

(** [CORE]: encoding and pretty-printing for errors *)
include
  Sig.CORE with type error := error and type error_category := error_category

(** [WITH_WRAPPED]: wrapping of errors from other instantiations within this
    one. Specifically, this is used to wrap errors of the economic protocol
    (e.g., operation is invalid) within the errors of the shell (e.g., failed to
    validate protocol data).

    Functions from this module should only be used within the environment. *)
include Sig.WITH_WRAPPED with type error := error

(** [TzTrace]: trace module specific to the Tezos Error monad. The [trace] type
    of this module is meant to become abstract in the medium-term (see
    https://gitlab.com/tezos/tezos/-/issues/1577). *)
module TzTrace : Sig.TRACE with type 'error trace = 'error list

type 'error trace = 'error TzTrace.trace

(** [TzMonad]: the Tezos-specific monad part of the [Error_monad]. It includes

    - syntax modules
    - consistent defaults,
    - some tracing helpers,
    - some other misc helpers. *)
include
  Monad_maker.S
    with type error := TzCore.error
     and type 'error trace := 'error TzTrace.trace

(* Other syntax module *)
module Option_syntax = TzLwtreslib.Monad.Option_syntax
module Lwt_option_syntax = TzLwtreslib.Monad.Lwt_option_syntax

(** {1 Exception-Error bridge}

    This part of the interface groups functions that are used to interact with
    code that raises exceptions. Typically, you should be using these functions
    when calling into a library that raises exceptions.

    Remember that the keyword [error] is for failure within the [Result] monad
    (or, more specifically, the [TracedResult] monad) whilst [fail] is for
    failure within the [LwtResult] monad (or, more specifically, the
    [LwtTracedResult] monad). *)

(** {2 Failing: to error out and to fail}

    This sub-part of the interface groups functions that fail (either in
    the [TracedResult] monad or the [LwtTracedResult] monad) whilst carrying
    information provided as argument. When reading this sub-part you should read
    [error] and [fail] as verbs. E.g., {!error_with_exn} errors out and carries
    a provided exception. The next sub-part will group noun-like, declarative
    functions. *)

(** [error_with fmt …] errors out: it fails within the [TracedResult] monad.
    The payload of the [Error] constructor is unspecified beyond the fact that
    it includes the string formatted by [fmt …]. E.g.,

{[
   if n < 0 then
      error_with "Index (%d) is negative" n
   else if n >= Array.length a then
      error_with "Index (%d) is beyond maximum index (%d)" n (Array.length a - 1)
   else
      Ok a.(n)
]}

    Note: this is somewhat equivalent to [Stdlib.failwith] in that it is a
    generic failure mechanism with a simple error message. Like
    [Stdlib.failwith] it should be replaced by a more specific error mechanism
    in most cases. *)
val error_with : ('a, Format.formatter, unit, 'b tzresult) format4 -> 'a

(** [failwith fmt …] fails: it fails within the [LwtTracedResult] monad.
    The payload of the [Error] constructor is unspecified beyond the fact that
    it includes the string formatted by [fmt …]. E.g.,

{[
   match find key store with
   | None ->
      failwith "Key %a not found in store" pp_key key
   | Some value ->
      LwtResult.return value
]}

    Note: this is somewhat equivalent to [Stdlib.failwith] in that it is a
    generic failure mechanism with a simple error message. Like
    [Stdlib.failwith] it should be replaced by a more specific error mechanism
    in most cases. *)
val failwith : ('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 -> 'a

(** [error_with_exn exc] errors out: it fails within the [TracedResult] monad.
    The payload of the [Error] constructor is unspecified but it includes the
    exception.

    It is meant as a way to switch from exception-based error management to
    tzresult-based error management, e.g., when calling external libraries that
    use exceptions.

{[
   try Ok (parse_input s) with Lex_error | Parse_error as exc -> error_with_exn exc
]}

    Whilst it is useful in specific places, it is generally better to use a
    dedicated error. *)
val error_with_exn : exn -> 'a tzresult

(** [fail_with_exn exc] fails: it fails within the [LwtTracedResult] monad.
    The payload of the [Error] constructor is unspecified but it includes the
    info from the exception.

    It is meant as a way to switch, inside of Lwt code, from exception-based
    error management to tzresult-based error management, e.g., when calling
    external libraries that use exceptions.

{[
   Lwt.catch
      (fun () -> parse_input s)
      (function
         | Lex_error | Parse_error as exc -> fail_with_exn exc
         | exn -> Lwt.reraise exn (* re-raise by default *))
]}

    Whilst it is useful in specific places, it is generally better to use a
    dedicated error. *)
val fail_with_exn : exn -> 'a tzresult Lwt.t

(** {2 Conversions: an exception, an error, a trace, a result}

    This sub-part of the interface groups declarative functions that convert
    between different styles of error (exceptions, errors, traces, results).
    By themselves these functions have no effect within the Result or LwtResult
    monad, and they are generally used along with constructors or combinators.
    *)

(** [error_of_exn e] is an error that carries the exception [e]. This function
    is intended to be used when interacting with a part of the code (most likely
    an external library) which uses exceptions. *)
val error_of_exn : exn -> error

(** [error_of_fmt …] is like [error_with …] but the error isn't wrapped in a
    trace in a [result]. Instead, an error is returned and the caller is
    expected to pass it to whichever error-combinator is appropriate to the
    situation. E.g.,

{[
   fail_unless (check_valid input) (error_of_fmt "Invalid_input: %a" pp input)
]}
    *)
val error_of_fmt : ('a, Format.formatter, unit, error) format4 -> 'a

(** {2 Standard errors} *)

(** Wrapped OCaml/Lwt exception *)
type error += Exn of exn

(** Cancelation *)
type error += Canceled

(** {2 Catching exceptions} *)

(** [protect] is a wrapper around [Lwt.catch] where the error handler operates
    over [trace] instead of [exn]. Besides, [protect ~on_error ~canceler ~f]
    may *cancel* [f] via a [Lwt_canceler.t].

    More precisely, [protect ~on_error ~canceler f] runs [f ()]. An Lwt failure
    triggered by [f ()] is wrapped into an [Exn]. If a [canceler] is given and
    [Lwt_canceler.cancellation canceler] is determined before [f ()],
    a [Canceled] error is returned.

    Errors are caught by [~on_error] (if given), otherwise the previous value
    is returned. An Lwt failure triggered by [~on_error] is wrapped into an
    [Exn] *)
val protect :
  ?on_error:(error trace -> 'a tzresult Lwt.t) ->
  ?canceler:Lwt_canceler.t ->
  (unit -> 'a tzresult Lwt.t) ->
  'a tzresult Lwt.t

(** [catch f] executes [f] within a try-with block and wraps exceptions within
    a [tzresult]. [catch f] is equivalent to
    [try Ok (f ()) with e -> Error (error_of_exn e)].

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory}.
    *)
val catch : ?catch_only:(exn -> bool) -> (unit -> 'a) -> 'a tzresult

(** [catch_e] is like {!catch} but when [f] returns a [tzresult].
    I.e., [catch_e f] is equivalent to
    [try f () with e -> Error (error_of_exn e)].

    [catch_only] has the same use as with {!catch}. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_e : ?catch_only:(exn -> bool) -> (unit -> 'a tzresult) -> 'a tzresult

(** [catch_f f handler] is equivalent to [map_error (catch f) handler].
    In other words, it catches exceptions in [f ()] and either returns the
    value in an [Ok] or passes the exception to [handler] for the [Error].

    No attempt is made to catch the exceptions raised by [handler].

    [catch_only] has the same use as with {!catch}. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_f :
  ?catch_only:(exn -> bool) -> (unit -> 'a) -> (exn -> error) -> 'a tzresult

(** [catch_s] is like {!catch} but when [f] returns a promise. It is equivalent
    to

{[
Lwt.try_bind f
  (fun v -> Lwt.return (Ok v))
  (fun e -> Lwt.return (Error (error_of_exn e)))
]}

    [catch_only] has the same use as with {!catch}. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a tzresult Lwt.t

(** [catch_es] is like {!catch_s} but when [f] returns a promise of a
    [tzresult].
    I.e., [catch_es f] is equivalent to
    [Lwt.catch f (fun e -> Lwt.return_error (error_of_exn e))].

    [catch_only] has the same use as with {!catch}. The same restriction on
    catching non-deterministic runtime exceptions applies. *)
val catch_es :
  ?catch_only:(exn -> bool) -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** [protect_result] is similar to {!protect} except that any non runtime exception
    raised by [Lwt.catch] is wrapped under an [Error] value. *)
val protect_result :
  ?canceler:Lwt_canceler.t -> (unit -> 'a Lwt.t) -> ('a, exn) result Lwt.t

(** {1 Misc} *)

type error += Timeout

val with_timeout :
  ?canceler:Lwt_canceler.t ->
  unit Lwt.t ->
  (Lwt_canceler.t -> 'a tzresult Lwt.t) ->
  'a tzresult Lwt.t

(**/**)

val errs_tag : error trace Tag.def

(** A wrapper around {!Lwt_canceler.cancel}.

   If {!Lwt_canceler.cancel} fails with a non-empty list of exception, the first
   one is raised. This behaviour attempts to follow the previous version (0.2)
   of {!Lwt_canceler} as closely as possible. This function is used temporarily
   until exceptions are explicitly handled by callers of {!Lwt_canceler.cancel}
   and it will be removed once this is done. Use of this function is
   discouraged. *)
val cancel_with_exceptions : Lwt_canceler.t -> unit Lwt.t

(** [either_f left right] returns [left] if it's a success and [right] is not
    called. If [left] fails, [right] is evaluated and its result is returned if
    it's a success. If both [left] and [right] fail, their traces are merged. *)
val either_f :
  'a tzresult Lwt.t -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t
