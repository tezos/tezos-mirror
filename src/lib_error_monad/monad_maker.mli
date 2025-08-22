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

(** [S] is the Tezos-specific extension to the generic monad
    provided by Lwtreslib. It sets some defaults (e.g., it defaults traced
    failures), it brings some qualified identifiers into the main unqualified
    part (e.g., [return_unit]), it provides some tracing helpers and some
    in-monad assertion checks. *)
module type S = sig
  (** for substitution *)
  type error

  (** for substitution *)
  type 'error trace

  type tztrace = error trace

  type 'a tzresult = ('a, tztrace) result

  module Lwt_syntax : module type of TzLwtreslib.Monad.Lwt_syntax

  module Result_syntax : sig
    include module type of TzLwtreslib.Monad.Result_syntax

    val tzfail : 'error -> ('a, 'error trace) result

    val ( and* ) :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result

    val ( and+ ) :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result

    val tzjoin : (unit, 'error trace) result list -> (unit, 'error trace) result

    val tzall : ('a, 'error trace) result list -> ('a list, 'error trace) result

    val tzboth :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result
  end

  module Lwt_result_syntax : sig
    include module type of TzLwtreslib.Monad.Lwt_result_syntax

    val tzfail : 'error -> ('a, 'error trace) result Lwt.t

    val ( and* ) :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t

    val ( and+ ) :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t

    val tzjoin :
      (unit, 'error trace) result Lwt.t list ->
      (unit, 'error trace) result Lwt.t

    val tzall :
      ('a, 'error trace) result Lwt.t list ->
      ('a list, 'error trace) result Lwt.t

    val tzboth :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t
  end

  val classify_trace : tztrace -> Error_classification.t

  (* Pretty-prints an error trace. *)
  val pp_print_trace : Format.formatter -> tztrace -> unit

  (** Pretty-prints the top error of a trace *)
  val pp_print_top_error_of_trace : Format.formatter -> tztrace -> unit

  val trace_encoding : tztrace Data_encoding.t

  (** A serializer for result of a given type *)
  val result_encoding : 'a Data_encoding.t -> 'a tzresult Data_encoding.t

  (** [record_trace err res] is either [res] if [res] is [Ok _], or it is
      [Error (Trace.cons err tr)] if [res] is [Error tr].

      In other words, [record_trace err res] enriches the trace that is carried
      by [res] (if it is carrying a trace) with the error [err]. It leaves [res]
      untouched if [res] is not carrying a trace.

      You can use this to add high-level information to potential low-level
      errors. E.g.,

{[
record_trace
   Failure_to_load_config
   (load_data_from_file config_encoding config_file_name)
]}

      Note that [record_trace] takes a {e fully evaluated} error [err] as
      argument. It means that, whatever the value of the result [res], the error
      [err] is evaluated. This is not an issue if the error is a simple
      expression (a literal or a constructor with simple parameters). However,
      for any expression that is more complex (e.g., one that calls a function)
      you should prefer [record_trace_eval]. *)
  val record_trace : 'err -> ('a, 'err trace) result -> ('a, 'err trace) result

  (** [trace] is identical to [record_trace] but applies to a promise. More
      formally, [trace err p] is a promise that resolves to [Ok v] if [p]
      resolves to [Ok v], or it resolves to [Error (Trace.cons err tr)] if
      [res] resolves to [Error tr].

      In other words, [trace err p] enriches the trace that [p] resolves to (if
      it does resolve to a trace) with the error [err]. It leaves the value that
      [p] resolves to untouched if it is not a trace.

      You can use this to add high-level information to potential low-level
      errors.

      Note that, like {!record_trace}, [trace] takes a fully evaluated error as
      argument. For a similar reason as explained there, you should only use
      [trace] with simple expressions (literal or constructor with simple
      parameters) and prefer [trace_eval] for any other expression (such as ones
      that include functions calls). *)
  val trace :
    'err -> ('b, 'err trace) result Lwt.t -> ('b, 'err trace) result Lwt.t

  (** [record_trace_eval] is identical to [record_trace] except that the error
      that enriches the trace is wrapped in a function that is evaluated only if
      it is needed. More formally [record_trace_eval mkerr res] is [res] if
      [res] is [Ok _], or it is [Error (Trace.cons (mkerr ()) tr)] if [res] is
      [Error tr].

      You can achieve the same effect by hand with

{[
match res with
| Ok _ -> res
| Error tr -> Error (Trace.cons (mkerr ()) tr)
]}

      Prefer [record_trace_eval] over [record_trace] when the enriching error is
      expensive to compute or heavy to allocate. *)
  val record_trace_eval :
    (unit -> 'err) -> ('a, 'err trace) result -> ('a, 'err trace) result

  (** [trace_eval] is identical to [trace] except that the error that enriches
      the trace is wrapped in a function that is evaluated only if {e and when}
      it is needed. More formally [trace_eval mkerr p] is a promise that
      resolves to [Ok v] if [p] resolves to [Ok v], or it resolves to
      [Error (Trace.cons err tr)] if [p] resolves to [Error tr] and then [mkerr
      ()] resolves to [err].

      You can achieve the same effect by hand with

{[
p >>= function
| Ok _ -> p
| Error tr ->
   mkerr () >>= fun err ->
   Lwt.return (Error (Trace.cons err tr))
]}

      Note that the evaluation of the error can be arbitrarily delayed. Avoid
      using references and other mutable values in the function [mkerr].

      Prefer [trace_eval] over [trace] when the enriching error is expensive to
      compute or heavy to allocate or when evaluating it requires the use of
      Lwt. *)
  val trace_eval :
    (unit -> 'err) ->
    ('b, 'err trace) result Lwt.t ->
    ('b, 'err trace) result Lwt.t

  (** [error_unless flag err] is [Ok ()] if [b] is [true], it is
      [Error (Trace.make err)] otherwise. *)
  val error_unless : bool -> 'err -> (unit, 'err trace) result

  (** [error_when flag err] is [Error (Trace.make err)] if [b] is [true], it is
      [Ok ()] otherwise. *)
  val error_when : bool -> 'err -> (unit, 'err trace) result

  (** [fail_unless flag err] is [Lwt.return @@ Ok ()] if [b] is [true], it is
      [Lwt.return @@ Error (Trace.make err)] otherwise. *)
  val fail_unless : bool -> 'err -> (unit, 'err trace) result Lwt.t

  (** [fail_when flag err] is [Lwt.return @@ Error (Trace.make err)] if [b] is
      [true], it is [Lwt.return @@ Ok ()] otherwise. *)
  val fail_when : bool -> 'err -> (unit, 'err trace) result Lwt.t

  (** [unless b f] is [f ()] if [b] is [false] and it is a promise already
      resolved to [Ok ()] otherwise.

      You can use [unless] to avoid having to write an [if] statement that you
      then need to populate entirely to satisfy the type-checker. E.g, you can
      write [unless b f] instead of [if not b then f () else return_unit]. *)
  val unless :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  (** [when_ b f] is [f ()] if [b] is [true] and it is a promise already
      resolved to [Ok ()] otherwise.

      You can use [when_] to avoid having to write an [if] statement that you
      then need to populate entirely to satisfy the type-checker. E.g, you can
      write [when_ b f] instead of [if b then f () else return_unit]. *)
  val when_ :
    bool -> (unit -> (unit, 'trace) result Lwt.t) -> (unit, 'trace) result Lwt.t

  (** Wrapper around [Lwt_utils.dont_wait] *)
  val dont_wait :
    (unit -> (unit, 'trace) result Lwt.t) ->
    ('trace -> unit) ->
    (exn -> unit) ->
    unit
end

module Make
    (Error : sig
      type error = ..

      include Sig.CORE with type error := error
    end)
    (Trace : Sig.TRACE)
    (Monad :
      Tezos_lwt_result_stdlib.Lwtreslib.TRACED_MONAD
        with type 'error trace := 'error Trace.trace) :
  S with type error := Error.error and type 'error trace := 'error Trace.trace
