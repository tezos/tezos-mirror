(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

val err_implementation_mismatch : got:string -> 'a

(** Context wrappers translate from/to node-context and node-pvmstate
    PVMs internal representation to those used in the PVM. Each
    different PVM context will imply a dedicated wrapper.*)
module type S = sig
  type repo

  type tree

  val of_node_context : 'a Context.t -> ('a, repo, tree) Context_sigs.t

  val to_node_context : ('a, repo, tree) Context_sigs.t -> 'a Context.t

  val of_node_pvmstate : Context.pvmstate -> tree

  val to_node_pvmstate : tree -> Context.pvmstate
end

(** Specialized module to handle translation to/from Irmin_context.
    Directly used in Arith, Wasm_2_0_0 and RISC-V PVM *)
module Irmin :
  S with type repo = Irmin_context.repo and type tree = Irmin_context.tree

module Riscv :
  S with type repo = Riscv_context.repo and type tree = Riscv_context.tree
