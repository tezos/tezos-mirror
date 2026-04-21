(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2022 Marigold <contact@marigold.dev>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** In-memory state backend for the WASM PVM, backed by
    {!Tezos_context_memory.Context_binary}.

    This module provides the tree-based state representation and proof
    machinery needed to run and verify the WASM PVM without an on-disk
    Irmin context. *)

(** In-memory state implementation satisfying {!Wasm_pvm_sig.STATE_PROOF}.

    Uses {!Tezos_context_memory.Context_binary} as the underlying tree
    backend, providing proof production and verification over binary
    Merkle trees. *)
module State_in_memory :
  Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF
    with type context = Tezos_context_memory.Context_binary.t
     and type state = Tezos_context_memory.Context_binary.tree
     and type proof =
      Tezos_context_memory.Context_binary.Proof.tree
      Tezos_context_memory.Context_binary.Proof.t

(** [wasm_pvm_machine ~config] returns a WASM PVM machine over
    {!State_in_memory}, closing over the supplied [config]. The env
    implementation builds the config via
    {!Tezos_scoru_wasm.Wasm_pvm_config.of_signals}. Non-protocol
    consumers (node, benchmarks, tests) pass
    {!Tezos_scoru_wasm.Wasm_pvm_config.empty}. *)
val wasm_pvm_machine :
  config:Tezos_scoru_wasm.Wasm_pvm_config.t ->
  (module Tezos_scoru_wasm.Wasm_pvm_sig.S
     with type context = State_in_memory.context
      and type state = State_in_memory.state
      and type proof = State_in_memory.proof)
