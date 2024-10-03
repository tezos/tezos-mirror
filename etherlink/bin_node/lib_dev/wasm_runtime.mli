(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [run tree] calls the WASM runtime, feeding it [tree], and computing the
    next [tree]. *)
val run : Irmin_context.tree -> Irmin_context.tree Lwt.t
