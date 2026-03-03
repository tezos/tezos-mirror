(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Job [oc.build_x86_64-released]. *)
val job_build_x86_64_released : Cacio.job

(** Register build jobs. *)
val register : unit -> unit
