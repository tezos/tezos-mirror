(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val err_implementation_mismatch : got:string -> 'a

(** Context wrappers translate from/to node-context and node-pvmstate
    PVMs internal representation to those used in the PVM. Each
    different PVM context will imply a dedicated wrapper.*)
module type S = sig
  type repo

  type tree

  val of_node_context : 'a Context.t -> ('a, repo, tree) Context_sigs.t

  val to_node_context : ('a, repo, tree) Context_sigs.t -> 'a Context.t

  val of_node_pvmstate : Context.pvmstate -> tree

  val to_node_pvmstate : tree -> Context.pvmstate
end

(** Specialized module to handle translation to/from Irmin_context.
    Directly used in Arith, Wasm_2_0_0 and RISC-V PVM *)
module Irmin :
  S with type repo = Irmin_context.repo and type tree = Irmin_context.tree
