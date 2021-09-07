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

(** [S] is the signature for a Lwt, result and Lwt-result combined monad. It is
    similar to {!Bare_sigs.Monad} with the addition of traces. Specifically:

    - The type ['error trace] is meant to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]).
    - The functions [error_trace] and [fail_trace] allow failing immediately
      with a trace-wrapped error.
    - [{join,all,both}_{e,ep}] return ['error trace] rather than ['error list].
    *)
module type S = sig
  (** Most of it is defined in the non-traced monad. The rest is trace-specific,
      occasionally shadowing. *)
  include Bare_sigs.Monad.S

  (** ['error trace] is intended to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]) *)
  type 'error trace

  (** {2 The traced Result monad: for success and traced failure}

      The [TracedResult] module is similar to the [Result] module with the
      following differences:
      - only the monadic-core is exposed (no [iter], no [is_ok], etc.; you
        need to manipulate the values explicitly to achieve that), and
      - all the returned [result] carry ['e trace] in their [Error] constructor
        (including [fail] which wraps the provided error into a singleton
        trace).
    *)
  module TracedResult : sig
    val return : 'a -> ('a, 'error trace) result

    val return_unit : (unit, 'error trace) result

    val return_none : ('a option, 'error trace) result

    val return_some : 'a -> ('a option, 'error trace) result

    val return_nil : ('a list, 'error trace) result

    val return_true : (bool, 'error trace) result

    val return_false : (bool, 'error trace) result

    (** [fail e] is [Error (Trace.make e)] where [Trace] is the
      {!Traced_sigs.Trace} module that provides the trace type and functions. *)
    val fail : 'error -> ('a, 'error trace) result

    val bind :
      ('a, 'error trace) result ->
      ('a -> ('b, 'error trace) result) ->
      ('b, 'error trace) result

    val map :
      ('a -> 'b) -> ('a, 'error trace) result -> ('b, 'error trace) result

    val iter : ('a -> unit) -> ('a, 'error trace) result -> unit
  end

  (** [error_trace e] is the monad-global alias for [TracedResult.fail e]. *)
  val error_trace : 'error -> ('a, 'error trace) result

  (** {2 The Lwt traced Result monad: for concurrent successes and traced failures}

      The [LwtTracedResult] module is similar to the [LwtResult] module with the
      following difference:
      - all the returned [result] carry ['e trace] in their [Error] constructor
        (including [fail] which wraps the provided error into a singleton
        trace).
  *)
  module LwtTracedResult : sig
    val return : 'a -> ('a, 'error trace) result Lwt.t

    val return_unit : (unit, 'error trace) result Lwt.t

    val return_none : ('a option, 'error trace) result Lwt.t

    val return_some : 'a -> ('a option, 'error trace) result Lwt.t

    val return_nil : ('a list, 'error trace) result Lwt.t

    val return_true : (bool, 'error trace) result Lwt.t

    val return_false : (bool, 'error trace) result Lwt.t

    (** [fail e] is [Lwt.return (Error (Trace.make e))] where [Trace] is the
      {!Traced_sigs.Trace} module that provides the trace type and functions.
      *)
    val fail : 'error -> ('a, 'error trace) result Lwt.t

    val bind :
      ('a, 'error trace) result Lwt.t ->
      ('a -> ('b, 'error trace) result Lwt.t) ->
      ('b, 'error trace) result Lwt.t

    val map :
      ('a -> 'b) ->
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t
  end

  (** [fail_trace e] is the monad-global alias for [LwtTracedResult.fail e]. *)
  val fail_trace : 'error -> ('a, 'error trace) result Lwt.t

  (** {1 Joins}

      Joins are similar to the non-traced monad's functions of the same names.
      The difference is that failures that are joined together are grouped in a
      traced (using [Trace.conp]/[Trace.conp_list]) rather than returned as a
      list. *)
  val join_e : (unit, 'error trace) result list -> (unit, 'error trace) result

  val all_e : ('a, 'error trace) result list -> ('a list, 'error trace) result

  val both_e :
    ('a, 'error trace) result ->
    ('b, 'error trace) result ->
    ('a * 'b, 'error trace) result

  val join_ep :
    (unit, 'error trace) result Lwt.t list -> (unit, 'error trace) result Lwt.t

  val all_ep :
    ('a, 'error trace) result Lwt.t list -> ('a list, 'error trace) result Lwt.t

  val both_ep :
    ('a, 'error trace) result Lwt.t ->
    ('b, 'error trace) result Lwt.t ->
    ('a * 'b, 'error trace) result Lwt.t
end
