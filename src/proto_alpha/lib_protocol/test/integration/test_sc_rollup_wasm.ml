(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Testing
    -------
    Component:    sc rollup wasm
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- test "^sc rollup wasm$"
    Subject:      Test the WASM 2.0 PVM.
*)

open Protocol
open Alpha_context
module Context_binary = Tezos_context_memory.Context_binary

module Tree :
  Environment.Context.TREE
    with type t = Context_binary.t
     and type tree = Context_binary.tree
     and type key = string list
     and type value = bytes = struct
  type t = Context_binary.t

  type tree = Context_binary.tree

  type key = Context_binary.key

  type value = Context_binary.value

  include Context_binary.Tree
end

module WASM_P :
  Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM.P
    with type Tree.t = Context_binary.t
     and type Tree.tree = Context_binary.tree
     and type Tree.key = string list
     and type Tree.value = bytes
     and type proof = Context_binary.Proof.tree Context_binary.Proof.t = struct
  module Tree = Tree

  type tree = Tree.tree

  type proof = Context_binary.Proof.tree Context_binary.Proof.t

  let proof_encoding =
    Tezos_context_helpers.Merkle_proof_encoding.V2.Tree2.tree_proof_encoding

  let kinded_hash_to_state_hash :
      Context_binary.Proof.kinded_hash -> Sc_rollup.State_hash.t = function
    | `Value hash | `Node hash ->
        Sc_rollup.State_hash.context_hash_to_state_hash hash

  let proof_before proof =
    kinded_hash_to_state_hash proof.Context_binary.Proof.before

  let proof_after proof =
    kinded_hash_to_state_hash proof.Context_binary.Proof.after

  let produce_proof context tree step =
    let open Lwt_syntax in
    let* context = Context_binary.add_tree context [] tree in
    let _hash = Context_binary.commit ~time:Time.Protocol.epoch context in
    let index = Context_binary.index context in
    match Context_binary.Tree.kinded_key tree with
    | Some k ->
        let* p = Context_binary.produce_tree_proof index k step in
        return (Some p)
    | None ->
        Stdlib.failwith
          "produce_proof: internal error, [kinded_key] returned [None]"

  let verify_proof proof step =
    let open Lwt_syntax in
    let* result = Context_binary.verify_tree_proof proof step in
    match result with
    | Ok v -> return (Some v)
    | Error _ ->
        (* We skip the error analysis here since proof verification is not a
           job for the rollup node. *)
        return None
end

module Verifier = Alpha_context.Sc_rollup.Wasm_2_0_0PVM.ProtocolImplementation

module Prover = Alpha_context.Sc_rollup.Wasm_2_0_0PVM.Make (WASM_P)

let should_boot () =
  let open Lwt_result_syntax in
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let*! s = Prover.initial_state context "" in
  let*! s = Prover.eval s in
  let*! p_res = Prover.produce_proof context None s in
  match p_res with
  | Ok proof ->
      let*! is_correct = Verifier.verify_proof proof in
      if is_correct then return_unit else Stdlib.failwith "incorrect proof"
  | Error err ->
      failwith "Could not produce a proof %a" Environment.Error_monad.pp err

let tests = [Tztest.tztest "should boot" `Quick should_boot]
