(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Mainnet | Ghostnet | Latest

let all = [Mainnet; Ghostnet; Latest]

let to_uses_and_tags = function
  | Mainnet -> ("mainnet", Constant.WASM.mainnet_evm_kernel)
  | Ghostnet -> ("ghostnet", Constant.WASM.ghostnet_evm_kernel)
  | Latest -> ("latest", Constant.WASM.evm_kernel)

let commit_of = function
  | Mainnet -> Some Constant.WASM.mainnet_evm_commit
  | Ghostnet -> Some Constant.WASM.ghostnet_evm_commit
  | Latest -> None

let upgrade_to = function
  | Mainnet -> Latest
  | Ghostnet -> Latest
  | Latest -> Latest

let of_use u =
  if Uses.(tag u = tag Constant.WASM.mainnet_evm_kernel) then Mainnet
  else if Uses.(tag u = tag Constant.WASM.ghostnet_evm_kernel) then Ghostnet
  else if Uses.(tag u = tag Constant.WASM.evm_kernel) then Latest
  else raise (Invalid_argument "Kernel.of_use")
