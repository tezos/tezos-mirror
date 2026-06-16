(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Job [oc.build_kernels]. *)
val job_build_kernels : Cacio.job

(** Register kernel jobs. *)
val register : unit -> unit
