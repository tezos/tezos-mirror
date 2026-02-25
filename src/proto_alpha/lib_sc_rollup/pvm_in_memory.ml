(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context

module type PVM =
  Sc_rollup.PVM.S
    with type context = Tezos_context_memory.Context_binary.t
     and type state = Tezos_context_memory.Context_binary.tree
     and type proof =
      Tezos_context_memory.Context_binary.Proof.tree
      Tezos_context_memory.Context_binary.Proof.t

module type S = sig
  include PVM

  val make_empty_context : unit -> context

  val make_empty_state : unit -> state
end

module Extend (Pvm : PVM) = struct
  include Pvm

  let make_empty_context =
    Tezos_context_memory.Context_binary.make_empty_context ~root:"dummy"

  let make_empty_state = Tezos_context_memory.Context_binary.make_empty_tree
end

module Arith : S = Extend (Sc_rollup.ArithPVM.Make (Context_helpers.In_memory))

module Wasm : S = Extend (Sc_rollup.Wasm_2_0_0PVM.Make_pvm (struct
  include Tezos_scoru_wasm.Wasm_pvm.Wasm_pvm_in_memory

  let compute_step =
    compute_step ~wasm_entrypoint:Tezos_scoru_wasm.Constants.wasm_entrypoint
end))
