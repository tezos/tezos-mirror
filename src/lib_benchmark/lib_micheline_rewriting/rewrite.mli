(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Rewriting Micheline terms. *)

(** Signature of rewriting module *)
module type S = sig
  type label

  type head

  type path

  type patt

  type node = (label, head) Micheline.node

  exception Rewrite_error of string * node option

  val get_subterm : term:node -> path:path -> node

  val subst : term:node -> path:path -> replacement:node -> node

  val pattern_matches : patt -> node -> bool

  val all_matches : patt -> node -> path list
end

module Make
    (X : Algebraic_signature.S)
    (M : Micheline_sig.S with type head = X.t)
    (Path : Path.S)
    (Patt :
      Pattern.S
        with type head = X.t
         and type path = Path.t
         and type node = M.node) :
  S
    with type label = M.label
     and type head = X.t
     and type path = Path.t
     and type patt = Patt.t
