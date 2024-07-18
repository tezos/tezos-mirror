(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let read_exn path =
  match Kernels.read path with Some kernel -> kernel | None -> raise Not_found

(* The kernel was produced by:

   Checkout the mainnet initial commit:
   - git checkout b9f6c9138719220db83086f0548e49c5c4c8421f
   Cherry-pick the commit to process at most one block per reboot
   - git cherry-pick aa6dd40dc05369b50bbe96f4f15008630a2719a8
   Compile the kernel
   - make -f etherlink.mk evm_kernel.wasm

   The cherry-pick will produce a small conflict on the changelog.
*)
let mainnet_initial_kernel_reconstruct_compatible =
  read_exn "mainnet-initial-kernel-reconstruct-compatible.wasm"
