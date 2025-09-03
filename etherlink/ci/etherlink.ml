(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module CI = Cacio.Make (struct
  let name = "etherlink"

  let paths = ["etherlink/**/*"; "src/kernel_sdk/**/*"; "sdk/rust/**/*"]
end)

let register () = ()
