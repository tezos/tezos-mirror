(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This type is used to register test based on which kernel version the test
    should run on. *)
type t =
  | All  (** The tests will run on all available kernels. *)
  | Mainnet  (** The tests will run on mainnet kernel only. *)
  | Latest  (** The tests will run latest kernel only. *)

(** Mainnet version is actually the ghostnet kernel. We can use the same
    declaration as long a both run the same code. *)
let mainnet = ("mainnet", Constant.WASM.ghostnet_evm_kernel)

let latest = ("latest", Constant.WASM.evm_kernel)

let to_uses_and_tags = function
  | All -> [mainnet; latest]
  | Mainnet -> [mainnet]
  | Latest -> [latest]
