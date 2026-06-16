(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Specialized module to handle translation to/from Irmin_context.
    Directly used in Arith, Wasm_2_0_0 and RISC-V PVM *)
module Irmin :
  Context.Wrapper.S
    with type repo = Irmin_context.repo
     and type state = Irmin_context.state
     and type mut_state = Irmin_context.mut_state

module Riscv :
  Context.Wrapper.S
    with type repo = Riscv_context.repo
     and type state = Riscv_context.state
     and type mut_state = Riscv_context.mut_state
