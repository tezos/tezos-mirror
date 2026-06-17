(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025-2026 Nomadic Labs <contact@nomadic-labs.com> *)
(*                                                                           *)
(*****************************************************************************)

(** In-memory NDS backend: supplies the
    {!Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND} operations from
    {!Octez_riscv_nds_memory}. *)
module In_memory_backend :
  Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND
    with module Proof = Octez_riscv_nds_memory.Proof = struct
  module Proof = Octez_riscv_nds_memory.Proof

  (** Hash of an empty registry, computed once at module load.  Every
      empty [Normal] registry hashes to this value, so prover and
      verifier agree on the activation-tick marker. *)
  let empty_registry_hash =
    Octez_riscv_nds_memory.Normal.Registry.hash
      (Octez_riscv_nds_memory.Normal.Registry.create ())

  type prove_session = Octez_riscv_nds_memory.Prove.Registry.t

  let open_prove_session nds =
    match Octez_riscv_nds_memory.unwrap_normal nds with
    | None ->
        invalid_arg
          "In_memory_backend.open_prove_session: NDS handle is not \
           [Normal]-mode — only kernel-driven host-function execution should \
           be proved"
    | Some normal_registry ->
        let prove_registry =
          Octez_riscv_nds_memory.Prove.start_proof normal_registry
        in
        let prove_nds =
          Octez_riscv_nds_common.Nds.wrap
            Octez_riscv_nds_memory.Prove_tag
            (module Octez_riscv_nds_memory.Prove)
            prove_registry
        in
        (prove_registry, prove_nds)

  let produce_proof prove_registry =
    Octez_riscv_nds_memory.Prove.produce_proof prove_registry

  let open_verify_session nds_proof =
    let verify_registry =
      Octez_riscv_nds_memory.Verify.start_verify nds_proof
    in
    Octez_riscv_nds_common.Nds.wrap
      Octez_riscv_nds_memory.Verify_tag
      (module Octez_riscv_nds_memory.Verify)
      verify_registry

  let make_empty_nds = Octez_riscv_nds_memory.make_empty_normal_nds
end

module Irmin = struct
  include Tezos_smart_rollup_wasm_in_memory.State_in_memory

  let find_value = Tezos_context_memory.Context_binary.Tree.find

  let set_value = Tezos_context_memory.Context_binary.Tree.add
end

include Tezos_smart_rollup_wasm_dual_state.Make (Irmin) (In_memory_backend)
