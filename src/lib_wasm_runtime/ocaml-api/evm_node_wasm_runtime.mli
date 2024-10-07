(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [id tree] is the identity function, returning [tree] untouched.

    It will be removed in a later version, but is introduced temporarily to
    prove the library is working. *)
external wasm_runtime_id : Irmin_context.tree -> Irmin_context.tree
  = "wasm_runtime_id"
