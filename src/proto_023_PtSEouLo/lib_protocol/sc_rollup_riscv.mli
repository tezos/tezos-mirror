(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 - 2024 Nomadic Labs <contact@nomadic-labs.com>         *)
(* Copyright (c) 2024 - 2025 TriliTech <contact@trili.tech>                  *)
(*                                                                           *)
(*****************************************************************************)

type error += RISCV_proof_verification_failed

type error += RISCV_proof_production_failed

type state = Riscv.state

type proof = Riscv.proof

val make_empty_state : unit -> state

module Protocol_implementation :
  Sc_rollup_PVM_sig.PROTO_VERIFICATION
    with type context = unit
     and type state = Riscv.state
     and type proof = Riscv.proof
