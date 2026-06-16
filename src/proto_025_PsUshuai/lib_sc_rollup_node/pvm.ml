(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2022-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

module type S = Pvm_sig.S

let of_kind : Kind.t -> (module S) = function
  | Example_arith -> (module Arith_pvm)
  | Wasm_2_0_0 -> (module Wasm_2_0_0_pvm)
  | Riscv -> (module Riscv_pvm)

let context : Kind.t -> (module Context_sigs.S) =
  let module Irmin_context = struct
    include Irmin_context

    let load ~cache_size path = load ~cache_size path
  end in
  function
  | Example_arith -> (module Irmin_context)
  | Wasm_2_0_0 -> (module Irmin_context)
  | Riscv -> (module Riscv_context)
