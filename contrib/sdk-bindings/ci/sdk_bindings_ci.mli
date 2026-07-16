(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Files that should trigger the jobs of this module. *)
val changeset : Tezos_ci.Changeset.t

(** Register jobs and pipelines for SDK bindings. *)
val register : unit -> unit
