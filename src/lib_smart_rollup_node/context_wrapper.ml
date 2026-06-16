(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Irmin = Context.Wrapper.Make (struct
  include Irmin_context

  let load ~cache_size path = load ~cache_size path
end)

module Riscv = Context.Wrapper.Make (Riscv_context)
