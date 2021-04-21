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

(** Sets with the signature [S] are exception-safe replacements for
    sets with the {!Stdlib.Set.S} signature with Lwt- and result-aware
    traversal functions.

    See {!Lwtreslib}'s introductory documentation for explanations regarding
    [_e]-, [_s]-, [_es]-, [_p]-, and [_ep]-suffixed functions and exception
    safety. See {!Stdlib.Set.S} for explanations regarding OCaml's
    sets in general.

    Note that this signature is within the Traced part of the library. As a
    result, the [_ep] traversor returns en ['error trace]. *)
module type S = sig
  include Bare_functor_outputs.Set.S

  (** ['error trace] is intended to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]) *)
  type 'error trace

  val iter_ep :
    (elt -> (unit, 'error trace) result Lwt.t) ->
    t ->
    (unit, 'error trace) result Lwt.t
end
