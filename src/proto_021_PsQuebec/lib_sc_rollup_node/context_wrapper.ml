(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

module Irmin = Context.Wrapper.Make (struct
  include Irmin_context

  let load ~cache_size path = load ~cache_size path
end)

module Riscv = Context.Wrapper.Make (struct
  include Riscv_context

  type mut_state = Mutable_state.t

  let from_imm = Mutable_state.from_imm

  let to_imm = Mutable_state.to_imm
end)
