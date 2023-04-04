(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** Extends the {!Lwt_result_syntax} with additional utilities for wrapping
    results produced by the protocol, i.e. [Environment.Error_monad.tzresult],
    to [tzresult Lwt.t] values used in the tests.

    The added utilities are binding operators. They use the same symbols as
    the ones from {!Lwt_result_syntax} with an added [@] character. This
    character symbolizes the {!e wrapping} of the internal error monad type in a
    shell error.  *)

include module type of Tezos_base.TzPervasives.Lwt_result_syntax

(** [wrap res] maps the result type contained in [res] to a tzresult
    value. *)
val wrap : 'a Environment.Error_monad.tzresult Lwt.t -> 'a tzresult Lwt.t

(** [let*@ x = m in f x] is equivalent to [let* x = wrap m in f x].

    Mnemonic: [@] "wraps" a protocol error in a shell error. *)
val ( let*@ ) :
  'a Environment.Error_monad.tzresult Lwt.t ->
  ('a -> 'b tzresult Lwt.t) ->
  'b tzresult Lwt.t

(** [let*?@ x = m in f x] is equivalent to [let*? x = Environment.wrap_tzresult
      m in f x].

      Mnemonic: [@] "wraps" a protocol error in a shell error. *)
val ( let*?@ ) :
  'a Environment.Error_monad.tzresult ->
  ('a -> 'b tzresult Lwt.t) ->
  'b tzresult Lwt.t

(** [let+@ x = m in f x] is equivalent to [let+ x = wrap m in f x].

      Mnemonic: [@] "wraps" a protocol error in a shell error. *)
val ( let+@ ) :
  'a Environment.Error_monad.tzresult Lwt.t -> ('a -> 'b) -> 'b tzresult Lwt.t
