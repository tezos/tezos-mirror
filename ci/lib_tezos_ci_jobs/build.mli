(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Build jobs.

    They are registered when this module is initialized,
    i.e. when this library is linked. *)

(** Jobs [oc.build_*-released]. *)
val job_build_released : Tezos_ci.Runner.Arch.t -> Cacio.job

(** Jobs [oc.build_*-extra-dev]. *)
val job_build_extra_dev : Tezos_ci.Runner.Arch.t -> Cacio.job

(** Jobs [oc.build_*-exp]. *)
val job_build_exp : Tezos_ci.Runner.Arch.t -> Cacio.job

(** Jobs [oc.build:static-*-linux-binaries] (released executables). *)
val job_build_static_linux_released_binaries :
  Tezos_ci.Runner.Arch.t -> [`release | `test] -> Cacio.job

(** Jobs [oc.build:static-*-linux-experimental-binaries]. *)
val job_build_static_linux_experimental_binaries :
  Tezos_ci.Runner.Arch.t -> Cacio.job
