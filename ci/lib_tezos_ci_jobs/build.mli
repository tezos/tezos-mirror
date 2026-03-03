(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Job [oc.build_x86_64-released]. *)
val job_build_x86_64_released : Cacio.job

(** Job [oc.build_amd64-extra-dev]. *)
val job_build_amd64_extra_dev : Cacio.job

(** Register build jobs. *)
val register : unit -> unit
