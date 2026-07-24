(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** On-disk NDS backend for the dual-state WASM PVM.

    Adapts the rocksdb-backed RISC-V new durable storage
    ({!Octez_riscv_nds_disk}) to the
    {!Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND} interface the
    generic dual-state functor ({!Tezos_smart_rollup_wasm_dual_state.Make})
    consumes. *)

include
  Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND
    with module Proof = Octez_riscv_nds_disk.Proof
