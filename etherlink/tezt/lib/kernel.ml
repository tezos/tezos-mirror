(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Mainnet | Ghostnet | Latest

let all = [Mainnet; Ghostnet; Latest]

let to_uses_and_tags = function
  | Mainnet -> ("mainnet", Constant.WASM.mainnet_kernel)
  | Ghostnet -> ("ghostnet", Constant.WASM.ghostnet_kernel)
  | Latest -> ("latest", Constant.WASM.evm_kernel)

let commit_of = function
  | Latest -> None
  | Ghostnet -> Some Constant.WASM.ghostnet_commit
  | Mainnet -> Some Constant.WASM.mainnet_commit

let upgrade_to = function
  | Latest -> Latest
  | Ghostnet -> Latest
  | Mainnet -> Ghostnet

let of_tag_use u =
  if Uses.(tag u = tag Constant.WASM.mainnet_kernel) then Mainnet
  else if Uses.(tag u = tag Constant.WASM.ghostnet_kernel) then Ghostnet
  else if Uses.(tag u = tag Constant.WASM.evm_kernel) then Latest
  else raise (Invalid_argument "Kernel.of_use")

let supports_dal = function
  | Mainnet -> false
  | Ghostnet -> false
  | Latest -> true

(* Select the appropriate EVM version for the specified kernel.

   NOTE: This function must be updated when Ghostnet or Mainnet kernels start
   supporting configurable (overridable) EVM versions. *)
let select_evm_version ?evm_version kernel =
  match (evm_version, kernel) with
  | Some Evm_version.Shanghai, _ -> Evm_version.Shanghai
  | None, (Ghostnet | Mainnet) -> Evm_version.Cancun
  | None, Latest -> Cancun
  | Some v, Latest -> v
  | _ -> Test.fail "Invalid combination of kernel and evm_version"
