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
open Protocol.Alpha_context

module type P = sig
  module Tree :
    Tezos_context_sigs.Context.TREE
      with type key = string list
       and type value = bytes

  type tree = Tree.tree

  val hash_tree : tree -> Sc_rollup.State_hash.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_before : proof -> Sc_rollup.State_hash.t

  val proof_after : proof -> Sc_rollup.State_hash.t

  val verify_proof :
    proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t

  val produce_proof :
    Tree.t -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t
end

(** [In_memory] is a context that can be used to instantiate an Arith
    or Wasm PVM. It's signature is
    {!Protocol.Alpha_context.Sc_rollup.ArithPVM.P} =
    {!Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM.P} *)
module In_memory : sig
  include
    P
      with type Tree.tree = Tezos_context_memory.Context_binary.tree
       and type Tree.t = Tezos_context_memory.Context_binary.t
       and type proof =
        Tezos_context_memory.Context.Proof.tree
        Tezos_context_memory.Context.Proof.t

  val make_empty_context : ?root:string -> unit -> Tree.t
end
