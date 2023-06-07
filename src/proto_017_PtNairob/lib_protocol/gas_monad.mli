(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This monad combines:
    - a state monad where the state is the context
    - two levels of error monad to distinguish gas exhaustion from other errors

    It is useful for backtracking on type checking errors without backtracking
    the consumed gas.
*)
type ('a, 'trace) t

(** Alias of [('a, 'trace) t] to avoid confusion when the module is open *)
type ('a, 'trace) gas_monad = ('a, 'trace) t

(** [return x] returns a value in the gas-monad. *)
val return : 'a -> ('a, 'trace) t

(** [map f m] maps over successful results of [m] using [f]. *)
val map : ('a -> 'b) -> ('a, 'trace) t -> ('b, 'trace) t

(** [bind m f] binds successful results of [m] and feeds it to [f]. *)
val bind : ('a, 'trace) t -> ('a -> ('b, 'trace) t) -> ('b, 'trace) t

(** [bind_recover m f] binds the result of [m] and feeds it to [f]. It's another
    variant of [bind] that allows recovery from inner errors. *)
val bind_recover :
  ('a, 'trace) t -> (('a, 'trace) result -> ('b, 'trace') t) -> ('b, 'trace') t

(** [of_result r] is a gas-free embedding of the result [r] into the gas monad. *)
val of_result : ('a, 'trace) result -> ('a, 'trace) t

(** [consume_gas c] consumes c amounts of gas. It's a wrapper around
    [Gas.consume]. If that fails, the whole computation within the gas-monad
    returns an error. See the {!Alpha_context.Gas} module for details.*)
val consume_gas : Alpha_context.Gas.cost -> (unit, 'trace) t

(** [run ctxt m] runs [m] using the given context and returns the result along
    with the new context with updated gas. The given context has [unlimited]
    mode enabled, through [Gas.set_unlimited], no gas is consumed. *)
val run :
  Alpha_context.context ->
  ('a, 'trace) t ->
  (('a, 'trace) result * Alpha_context.context) tzresult

(** [record_trace_level ~error_details f m] returns a new gas-monad value that
     when run, records trace levels using [f]. This function has no effect in
    the case of a gas-exhaustion error or if [error_details] is [Fast]. *)
val record_trace_eval :
  error_details:('error_context, 'error_trace) Script_tc_errors.error_details ->
  ('error_context -> error) ->
  ('a, 'error_trace) t ->
  ('a, 'error_trace) t

(** [fail e] is [return (Error e)] . *)
val fail : 'trace -> ('a, 'trace) t

(** Syntax module for the {!Gas_monad}. This is intended to be opened locally in
    functions. Within the scope of this module, the code can include binding
    operators, leading to a [let]-style syntax. Similar to {!Lwt_result_syntax}
    and other syntax modules. *)
module Syntax : sig
  (** [return x] returns a value in the gas-monad. *)
  val return : 'a -> ('a, 'trace) t

  (** [return_unit] is [return ()] . *)
  val return_unit : (unit, 'trace) t

  (** [return_none] is [return None] . *)
  val return_none : ('a option, 'trace) t

  (** [return_some x] is [return (Some x)] . *)
  val return_some : 'a -> ('a option, 'trace) t

  (** [return_nil] is [return []] . *)
  val return_nil : ('a list, 'trace) t

  (** [return_true] is [return true] . *)
  val return_true : (bool, 'trace) t

  (** [return_false] is [return false] . *)
  val return_false : (bool, 'trace) t

  (** [fail e] is [return (Error e)] . *)
  val fail : 'trace -> ('a, 'trace) t

  (** [let*] is a binding operator alias for {!bind}. *)
  val ( let* ) : ('a, 'trace) t -> ('a -> ('b, 'trace) t) -> ('b, 'trace) t

  (** [let+] is a binding operator alias for {!map}. *)
  val ( let+ ) : ('a, 'trace) t -> ('a -> 'b) -> ('b, 'trace) t

  (** [let*?] is for binding the value from result-only expressions into the
      gas-monad. *)
  val ( let*? ) :
    ('a, 'trace) result -> ('a -> ('b, 'trace) t) -> ('b, 'trace) t
end
