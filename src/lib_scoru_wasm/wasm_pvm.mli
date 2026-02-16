(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

module type CONTEXT_PROOF = sig
  type context

  module Wrapped_tree : Tezos_tree_encoding.TREE

  val empty_state : unit -> Wrapped_tree.tree

  val state_hash :
    Wrapped_tree.tree -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t Lwt.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val proof_stop_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val produce_proof :
    context ->
    Wrapped_tree.tree ->
    (Wrapped_tree.tree -> (Wrapped_tree.tree * 'a) Lwt.t) ->
    (proof * 'a) option Lwt.t

  val verify_proof :
    proof ->
    (Wrapped_tree.tree -> (Wrapped_tree.tree * 'a) Lwt.t) ->
    (Wrapped_tree.tree * 'a) option Lwt.t

  val cast_read_only : proof -> proof
end

val pvm_state_encoding :
  Wasm_pvm_state.Internal_state.pvm_state Tezos_tree_encoding.t

val durable_buffers_encoding :
  Tezos_webassembly_interpreter.Eval.buffers Tezos_tree_encoding.t

val durable_storage_encoding : Durable.t Tezos_tree_encoding.t

module Make_machine (T : Tezos_tree_encoding.TREE) :
  Wasm_pvm_sig.Machine with type state = T.tree

module Make_machine_with_vm (Wasm_vm : Wasm_vm_sig.S) :
    module type of Make_machine

module Make_pvm_machine (Context : CONTEXT_PROOF) :
  Wasm_pvm_sig.S
    with type context = Context.context
     and type proof = Context.proof
     and type state = Context.Wrapped_tree.tree

module Make_pvm_machine_with_vm (Wasm_vm : Wasm_vm_sig.S) :
    module type of Make_pvm_machine

module Wasm_pvm_in_memory :
  Wasm_pvm_sig.S
    with type context = Tezos_context_memory.Context_binary.context
     and type state = Tezos_context_memory.Context_binary.tree
     and type proof =
      Tezos_context_memory.Context.Proof.tree
      Tezos_context_memory.Context.Proof.t
