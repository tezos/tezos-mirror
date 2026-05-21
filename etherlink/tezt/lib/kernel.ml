(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = Mainnet | Previewnet | Latest

let etherlink_all = [Mainnet; Latest]

let tezosx_all = [Previewnet; Latest]

let to_uses = function
  | Mainnet -> Constant.WASM.mainnet_kernel
  | Previewnet -> Constant.WASM.previewnet_kernel
  | Latest -> Constant.WASM.evm_kernel

let to_tag = function
  | Mainnet -> "mainnet"
  | Previewnet -> "previewnet"
  | Latest -> "latest"

let path kernel = Uses.path (to_uses kernel)

let to_uses_and_tags kernel = (to_tag kernel, to_uses kernel)

let commit_of = function
  | Latest -> None
  | Mainnet -> Some Constant.WASM.mainnet_commit
  | Previewnet -> Some Constant.WASM.previewnet_commit

(* The previewnet kernel was built before V57 was introduced, so the
   installer must produce the pre-V57 ("farfadet-r2") layout: EVM
   config scalars at [/evm/X], block index at
   [/evm/world_state/indexes/blocks/]. The V57 migration moves them on
   first boot of the new kernel. *)
let name_of = function
  | Latest -> None
  | Mainnet -> Some "farfadet-r2"
  | Previewnet -> Some "farfadet-r2"

let upgrade_to = function
  | Latest -> Latest
  | Mainnet -> Latest
  | Previewnet -> Latest

let supports_dal = function
  | Mainnet -> false
  | Previewnet -> true
  | Latest -> true

(* Storage version baked into each kernel binary at build time, as read at
   each kernel's commit (see [commit_of]) from
   `etherlink/kernel_latest/kernel/src/storage.rs:STORAGE_VERSION`. Bump the
   value for a network when that network's kernel is rebaked; treat this as
   the single source of truth when picking storage-version-gated paths in
   tezt. *)
let storage_version = function Latest -> 58 | Previewnet -> 56 | Mainnet -> 46

let of_tag tag =
  let contain_exp ~exp =
    let re = Str.regexp_string exp in
    try
      ignore (Str.search_forward re tag 0) ;
      true
    with Not_found -> false
  in
  if contain_exp ~exp:"mainnet" then Mainnet
  else if contain_exp ~exp:"previewnet" then Previewnet
  else Latest

(* Select the appropriate EVM version for the specified kernel.

   NOTE: This function must be updated when Mainnet kernels start
   supporting configurable (overridable) EVM versions. *)
let select_evm_version ?evm_version kernel =
  match (evm_version, kernel) with
  | _, Mainnet -> Evm_version.Osaka
  | _, Previewnet -> Evm_version.Prague
  | None, Latest -> Evm_version.Osaka
  | Some v, Latest -> v
