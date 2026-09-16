(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Kernel jobs.

    They are registered when this module is initialized,
    i.e. when this library is linked. *)

(** Job [oc.build_kernels]. *)
val job_build_kernels : Cacio.job
