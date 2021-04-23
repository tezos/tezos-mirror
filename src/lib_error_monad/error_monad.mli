(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Protocol Implementation - Error Monad *)

(** Categories of error *)
type error_category =
  [ `Branch  (** Errors that may not happen in another context *)
  | `Temporary  (** Errors that may not happen in a later context *)
  | `Permanent  (** Errors that will happen no matter the context *) ]

type error = TzCore.error = ..

include Sig.CORE with type error := error

include Sig.EXT with type error := error

include Sig.WITH_WRAPPED with type error := error

module TzTrace : Sig.TRACE with type 'error trace = 'error list

type 'error trace = 'error TzTrace.trace

include Sig.MONAD with type 'error trace := 'error TzTrace.trace

include
  Sig.MONAD_EXT
    with type error := error
     and type 'error trace := 'error TzTrace.trace

(** Erroneous result (shortcut for generic errors) *)
val generic_error : ('a, Format.formatter, unit, 'b tzresult) format4 -> 'a

(** Erroneous return (shortcut for generic errors) *)
val failwith : ('a, Format.formatter, unit, 'b tzresult Lwt.t) format4 -> 'a

val error_exn : exn -> 'a tzresult

(** [error_of_exn e] is a trace that carries the exception [e]. This function is
    intended to be used when interacting with a part of the code (most likely an
    external library) which uses exception. E.g.,

{[
try
   Ok (make_some_call parameter)
with
  | (Not_found | Failure _) as e ->
        Error (error_of_exn e)
]} *)
val error_of_exn : exn -> error trace

(** [tzresult_of_exn_result r] wraps the payload construction of the [Error]
    constructor of a result into a [tzresult]. This is intended for use when
    interacting with code that uses exceptions wrapped in a [result]. E.g.,

{[
let p : int Lwt.t = … in
Lwt_result.catch p >|= tzresult_of_exn_result
]} *)
val tzresult_of_exn_result : ('a, exn) result -> 'a tzresult

val record_trace_exn : exn -> 'a tzresult -> 'a tzresult

val trace_exn : exn -> 'b tzresult Lwt.t -> 'b tzresult Lwt.t

val generic_trace :
  ( 'a,
    Format.formatter,
    unit,
    ('b, error trace) result Lwt.t -> ('b, error trace) result Lwt.t )
  format4 ->
  'a

val pp_exn : Format.formatter -> exn -> unit

val failure : ('a, Format.formatter, unit, error) format4 -> 'a

(** Wrapped OCaml/Lwt exception *)
type error += Exn of exn

type error += Canceled

(** [protect] is a wrapper around [Lwt.catch] where the error handler operates
    over `trace` instead of `exn`. Besides, [protect ~on_error ~canceler ~f]
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

(** [catch_s] is like [catch] but when [f] returns a promise. It is equivalent
    to

{[
Lwt.try_bind f
  (fun v -> Lwt.return (Ok v))
  (fun e -> Lwt.return (Error (error_of_exn e)))
]}

    If [catch_only] is set, then only exceptions [e] such that [catch_only e] is
    [true] are caught.

    Whether [catch_only] is set or not, this function never catches
    non-deterministic runtime exceptions of OCaml such as {!Stack_overflow} and
    {!Out_of_memory}.
    *)
val catch_s :
  ?catch_only:(exn -> bool) -> (unit -> 'a Lwt.t) -> 'a tzresult Lwt.t

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
   until exceptions are explicitely handled by callers of {!Lwt_canceler.cancel}
   and it will be removed once this is done. Use of this function is
   discouraged. *)
val cancel_with_exceptions : Lwt_canceler.t -> unit Lwt.t
