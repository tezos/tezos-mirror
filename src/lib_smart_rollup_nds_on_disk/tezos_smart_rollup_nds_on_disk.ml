(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Proof = Octez_riscv_nds_disk.Proof

type prove_session = Octez_riscv_nds_disk.Prove.Registry.t

let open_prove_session nds =
  match Octez_riscv_nds_disk.unwrap_normal nds with
  | None ->
      invalid_arg
        "Tezos_smart_rollup_nds_on_disk.open_prove_session: NDS handle is not \
         [Normal]-mode — only kernel-driven host-function execution should be \
         proved"
  | Some normal_registry ->
      let prove_registry =
        Octez_riscv_nds_disk.Prove.start_proof normal_registry
      in
      let prove_nds =
        Octez_riscv_nds_common.Nds.wrap
          Octez_riscv_nds_disk.Prove_tag
          (module Octez_riscv_nds_disk.Prove)
          prove_registry
      in
      (prove_registry, prove_nds)

let produce_proof prove_registry =
  Octez_riscv_nds_disk.Prove.produce_proof prove_registry

let open_verify_session nds_proof =
  let verify_registry = Octez_riscv_nds_disk.Verify.start_verify nds_proof in
  Octez_riscv_nds_common.Nds.wrap
    Octez_riscv_nds_disk.Verify_tag
    (module Octez_riscv_nds_disk.Verify)
    verify_registry

let copy nds =
  match Octez_riscv_nds_disk.unwrap_normal nds with
  | None ->
      invalid_arg
        "Tezos_smart_rollup_nds_on_disk.copy: NDS handle is not [Normal]-mode \
         — [Prove]/[Verify] handles are transient in-step sessions and never \
         live in a state at rest"
  | Some normal_registry ->
      Octez_riscv_nds_common.Nds.wrap
        Octez_riscv_nds_disk.Normal_tag
        (module Octez_riscv_nds_disk.Normal)
        (Octez_riscv_nds_disk.Normal.Registry.duplicate normal_registry)
