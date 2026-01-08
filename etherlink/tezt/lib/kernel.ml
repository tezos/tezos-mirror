(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = Mainnet | Latest

let all = [Mainnet; Latest]

let to_uses_and_tags = function
  | Mainnet -> ("mainnet", Constant.WASM.mainnet_kernel)
  | Latest -> ("latest", Constant.WASM.evm_kernel)

let commit_of = function
  | Latest -> None
  | Mainnet -> Some Constant.WASM.mainnet_commit

let upgrade_to = function Latest -> Latest | Mainnet -> Latest

let of_tag_use u =
  if Uses.(tag u = tag Constant.WASM.mainnet_kernel) then Mainnet
  else if Uses.(tag u = tag Constant.WASM.evm_kernel) then Latest
  else raise (Invalid_argument "Kernel.of_use")

let supports_dal = function Mainnet -> false | Latest -> true

let supports_revm = function
  | kernel when kernel = Constant.WASM.evm_kernel -> true
  | _ -> false

let of_tag tag =
  let contain_exp ~exp =
    let re = Str.regexp_string exp in
    try
      ignore (Str.search_forward re tag 0) ;
      true
    with Not_found -> false
  in
  if contain_exp ~exp:"mainnet" then Mainnet else Latest

(* Select the appropriate EVM version for the specified kernel.

   NOTE: This function must be updated when Mainnet kernels start
   supporting configurable (overridable) EVM versions. *)
let select_evm_version ?evm_version kernel =
  match (evm_version, kernel) with
  | _, Mainnet -> Evm_version.Prague
  | None, Latest -> Evm_version.Osaka
  | Some v, Latest -> v
