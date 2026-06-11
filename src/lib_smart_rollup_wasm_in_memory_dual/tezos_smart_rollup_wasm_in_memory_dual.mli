(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025-2026 Nomadic Labs <contact@nomadic-labs.com> *)
(*                                                                           *)
(*****************************************************************************)

(** In-memory instantiation of {!Tezos_smart_rollup_wasm_dual_state}:
    applies the functor to the in-memory Irmin durable
    ({!Tezos_smart_rollup_wasm_in_memory.State_in_memory}) and an NDS
    backend over {!Octez_riscv_nds_memory}.

    See {!Tezos_smart_rollup_wasm_dual_state} for the proof model and
    soundness; this module supplies the backend and re-exports the
    specialised {!Dual_state} and {!wasm_pvm_machine_dual}. *)

module In_memory_backend :
  Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND
    with module Proof = Octez_riscv_nds_memory.Proof

module Irmin : sig
  include module type of Tezos_smart_rollup_wasm_in_memory.State_in_memory

  val find_value : state -> string list -> bytes option Lwt.t

  val set_value : state -> string list -> bytes -> state Lwt.t
end

include module type of
    Tezos_smart_rollup_wasm_dual_state.Make (Irmin) (In_memory_backend)
