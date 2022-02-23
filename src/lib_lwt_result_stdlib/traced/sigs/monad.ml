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
    - The functions {!Traced_result_syntax.fail} and
      {!Traced_lwt_result_syntax.fail} wrap the provided error in a trace,
    - [{join,all,both}] return ['error trace] rather than ['error list],
    - The binding operators {!Traced_result_syntax.( and* )} and
      {!Traced_lwt_result_syntax.( and* )} are available.
    *)
module type S = sig
  (** Import the non-traced modules as-is *)
  include Bare_sigs.Monad.S

  (** ['error trace] is intended to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]) *)
  type 'error trace

  (** {2 The traced Result monad: for successes and traced failures}

      The [Traced_result_syntax] module is similar to the [Result_syntax] module
      with the following differences:
      - [fail] wraps the error in a trace,
      - [and*] and [and+] are provided

      See {!Result_syntax}. *)
  module Traced_result_syntax : sig
    val return : 'a -> ('a, 'error) result

    val return_unit : (unit, 'error) result

    val return_none : ('a option, 'error) result

    val return_some : 'a -> ('a option, 'error) result

    val return_nil : ('a list, 'error) result

    val return_true : (bool, 'error) result

    val return_false : (bool, 'error) result

    (** [fail e] is [(Error (Trace.make e))] where [Trace] is the
      {!Traced_sigs.Trace} module that provides the trace type and functions. *)
    val fail : 'error -> ('a, 'error trace) result

    val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result

    val ( and* ) :
      ('a, 'e trace) result ->
      ('b, 'e trace) result ->
      ('a * 'b, 'e trace) result

    val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result

    val ( and+ ) :
      ('a, 'e trace) result ->
      ('b, 'e trace) result ->
      ('a * 'b, 'e trace) result

    val join : (unit, 'error trace) result list -> (unit, 'error trace) result

    val all : ('a, 'error trace) result list -> ('a list, 'error trace) result

    val both :
      ('a, 'error trace) result ->
      ('b, 'error trace) result ->
      ('a * 'b, 'error trace) result
  end

  (** {2 The Lwt traced Result monad: for concurrent successes and traced failures}

      The [Lwt_traced_result_syntax] module is similar to the
      [Lwt_result_syntax] module with the following difference:
      - [fail] wraps the error in a trace,
      - [and*] and [and+] are provided.

      See {!Lwt_result_syntax}. *)
  module Lwt_traced_result_syntax : sig
    val return : 'a -> ('a, 'error) result Lwt.t

    val return_unit : (unit, 'error) result Lwt.t

    val return_none : ('a option, 'error) result Lwt.t

    val return_some : 'a -> ('a option, 'error) result Lwt.t

    val return_nil : ('a list, 'error) result Lwt.t

    val return_true : (bool, 'error) result Lwt.t

    val return_false : (bool, 'error) result Lwt.t

    (** [fail e] is [Lwt.return (Error (Trace.make e))] where [Trace] is the
      {!Traced_sigs.Trace} module that provides the trace type and functions. *)
    val fail : 'error -> ('a, 'error trace) result Lwt.t

    val ( let* ) :
      ('a, 'e) result Lwt.t ->
      ('a -> ('b, 'e) result Lwt.t) ->
      ('b, 'e) result Lwt.t

    val ( and* ) :
      ('a, 'e trace) result Lwt.t ->
      ('b, 'e trace) result Lwt.t ->
      ('a * 'b, 'e trace) result Lwt.t

    val ( let+ ) : ('a, 'e) result Lwt.t -> ('a -> 'b) -> ('b, 'e) result Lwt.t

    val ( and+ ) :
      ('a, 'e trace) result Lwt.t ->
      ('b, 'e trace) result Lwt.t ->
      ('a * 'b, 'e trace) result Lwt.t

    val ( let*! ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

    val ( let*? ) :
      ('a, 'e) result -> ('a -> ('b, 'e) result Lwt.t) -> ('b, 'e) result Lwt.t

    val join :
      (unit, 'error trace) result Lwt.t list ->
      (unit, 'error trace) result Lwt.t

    val all :
      ('a, 'error trace) result Lwt.t list ->
      ('a list, 'error trace) result Lwt.t

    val both :
      ('a, 'error trace) result Lwt.t ->
      ('b, 'error trace) result Lwt.t ->
      ('a * 'b, 'error trace) result Lwt.t
  end
end
