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

open Alpha_context

(** This monad combines:
    - a state monad where the state is the context
    - two levels of error monad to distinguish gas exhaustion from other errors

    It is useful for backtracking on type checking errors without backtracking
    the consumed gas.
*)
type ('a, 'trace) t

(** Alias of [('a, 'trace) t] to avoid confusion when the module is open *)
type ('a, 'trace) gas_monad = ('a, 'trace) t

(** monadic return operator of the gas monad *)
val return : 'a -> ('a, 'trace) t

(** Specialized monadic return operator for unit value, strictly equivalent to
    [return ()]. *)
val return_unit : (unit, 'trace) t

(** Binding operator for the gas monad *)
val ( >>$ ) : ('a, 'trace) t -> ('a -> ('b, 'trace) t) -> ('b, 'trace) t

(** Mapping operator for the gas monad, [m >|$ f] is equivalent to
    [m >>$ fun x -> return (f x)] *)
val ( >|$ ) : ('a, 'trace) t -> ('a -> 'b) -> ('b, 'trace) t

(** Variant of [( >>$ )] to bind uncarbonated functions *)
val ( >?$ ) : ('a, 'trace) t -> ('a -> ('b, 'trace) result) -> ('b, 'trace) t

(** Another variant of [( >>$ )] that lets recover from inner errors *)
val ( >??$ ) :
  ('a, 'trace) t -> (('a, 'trace) result -> ('b, 'trace') t) -> ('b, 'trace') t

(** gas-free embedding of tzresult values. [of_result x] is equivalent to [return () >?$ fun () -> x] *)
val of_result : ('a, 'trace) result -> ('a, 'trace) t

(** A wrapper around Gas.consume. If that fails, the whole computation
    within the Gas_monad returns an error. See the Alpha_context.Gas module
    for details.*)
val consume_gas : Gas.cost -> (unit, 'trace) t

(** Escaping the gas monad. If the given context has [unlimited] mode enabled,
    through [Gas.set_unlimited], no gas is consumed. *)
val run : context -> ('a, 'trace) t -> (('a, 'trace) result * context) tzresult

(** re-export of [Error_monad.record_trace_eval]. This function has no
    effect in the case of a gas-exhaustion error
    or if [error_details] is [Fast]. *)
val record_trace_eval :
  error_details:'error_trace Script_tc_errors.error_details ->
  (unit -> error) ->
  ('a, 'error_trace) t ->
  ('a, 'error_trace) t
