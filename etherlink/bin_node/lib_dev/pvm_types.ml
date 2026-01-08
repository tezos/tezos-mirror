(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Describe where the kernel code can be found: either in-memory from a
    buffer, or on-disk using a given path. *)
type kernel = In_memory of string | On_disk of string

type config = Octez_smart_rollup_wasm_debugger_lib.Config.config
