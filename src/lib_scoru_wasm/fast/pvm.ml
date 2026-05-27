(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Re-export of {!Tezos_scoru_wasm.Wasm_vm.Params} so callers can
    write [Pvm.Make_machine (P) (S)] without also opening [Vm] or the
    inner WASM VM. *)
module type Params = Tezos_scoru_wasm.Wasm_vm.Params

(** Default {!Params} for a tree-only (single-storage) fast PVM: an empty
    config with no NDS factory, so the durable state lives entirely in the
    tree. Callers that do not activate the new durable storage can pass this
    straight to {!Make_machine} / {!Make_pvm} instead of re-defining the same
    defaults at every instantiation site. *)
module Default_params : Params = struct
  let config = Tezos_scoru_wasm.Wasm_pvm_config.empty

  let make_empty_nds = None
end

(** [Make_machine (P) (S)] builds the fast WASM PVM machine: the fast
    {!Vm.Make_vm} VM, parameterised by {!Params} [P], driven by the generic
    {!Tezos_scoru_wasm.Wasm_pvm} machine over the PVM state [S]. See
    {!Tezos_scoru_wasm.Wasm_vm} for the VM contract that [P] configures. *)
module Make_machine (P : Params) (S : Tezos_scoru_wasm.Wasm_pvm_sig.STATE) =
  Tezos_scoru_wasm.Wasm_pvm.Make_machine (Vm.Make_vm (P)) (S)

(** Like {!Make_machine}, but over a
    {!Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF} state so the resulting PVM
    can also produce and verify proofs. *)
module Make_pvm (P : Params) (S : Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF) =
  Tezos_scoru_wasm.Wasm_pvm.Make_pvm (Vm.Make_vm (P)) (S)
