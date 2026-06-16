(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Pre-instantiated WASM PVM machine for this protocol-environment
   version: the underlying [Wasm_vm.Make_vm] is closed over the empty
   feature-flag config since legacy environments predate the flagged
   evaluator behaviours. *)

module Vm = Tezos_scoru_wasm.Wasm_vm.Make_vm (struct
  let config = Tezos_scoru_wasm.Wasm_pvm_config.empty
end)

module Make (S : Tezos_scoru_wasm.Wasm_pvm_sig.STATE) =
  Tezos_scoru_wasm.Wasm_pvm.Make_machine (Vm) (S)
