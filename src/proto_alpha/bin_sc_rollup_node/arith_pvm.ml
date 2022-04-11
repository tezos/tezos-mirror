(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context

(** This module manifests the proof format used by the Arith PVM as defined by
    the Layer 1 implementation for it.

    It is imperative that this is aligned with the protocol's implementation.
*)
module Arith_proof_format = struct
  open Store

  type proof = IStoreProof.Proof.tree IStoreProof.Proof.t

  let verify_proof = IStoreProof.verify_tree_proof

  let kinded_hash_to_state_hash :
      IStoreProof.Proof.kinded_hash -> Sc_rollup.State_hash.t = function
    | `Value hash | `Node hash ->
        Sc_rollup.State_hash.hash_bytes [Context_hash.to_bytes hash]

  let proof_start_state proof =
    kinded_hash_to_state_hash proof.IStoreProof.Proof.before

  let proof_stop_state proof =
    kinded_hash_to_state_hash proof.IStoreProof.Proof.after

  let proof_encoding =
    Tezos_context_helpers.Merkle_proof_encoding.V2.Tree32.tree_proof_encoding
end

module Impl : Pvm.S = struct
  include Sc_rollup_arith.Make (struct
    open Store
    module Tree = IStoreTree

    type tree = IStoreTree.tree

    include Arith_proof_format
  end)

  let string_of_status status =
    match status with
    | Halted -> "Halted"
    | WaitingForInputMessage -> "WaitingForInputMessage"
    | Parsing -> "Parsing"
    | Evaluating -> "Evaluating"
end

include Impl
