(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Information about CI runners. *)

module Provider : sig
  (** Cloud provider accounts.

      In most cases, you want to use [GCP].

      [AWS] is only used in very specific cases.

      [GCP_dev] can be used for testing changes to the infrastructure itself
      (do the changes in the staging environment, run some test pipelines
      where jobs are modified to use this [GCP_dev] account,
      without committing the changes to the infrastructure to prod).
      On [master], no job should be using [GCP_dev]. *)
  type t = GCP | GCP_dev | AWS

  (** Convert a cloud provider specification to a string. *)
  val show : t -> string
end

module Arch : sig
  (** Runner CPU architectures. *)
  type t = Amd64 | Arm64

  (** Convert a CPU architecture to a string (["amd64"] or ["arm64"]). *)
  val show_uniform : t -> string

  (** Convert a CPU architecture to a string (["x86_64"] or ["arm64"]). *)
  val show_easy_to_distinguish : t -> string
end

module CPU : sig
  (** Runner CPU powers. *)
  type t =
    | Normal  (** Use the default runners. *)
    | High  (** Use more powerful runners. *)
    | Very_high  (** Use even more powerful runners. *)
    | Tezt  (** Use even more powerful runners, for Tezt jobs. *)

  (** Convert a CPU specification into a string. *)
  val show : t -> string
end

module Storage : sig
  (** Runner storage methods. *)
  type t =
    | Network  (** Use the default runners. *)
    | Ramfs  (** Use runners with ramfs storage. *)

  (** Convert a storage method into a string. *)
  val show : t -> string
end

module Tag : sig
  (** Runner tags. *)
  type t =
    | Gcp  (** GCP prod AMD64 runner, general purpose. *)
    | Gcp_arm64  (** GCP prod ARM64 runner, general purpose. *)
    | Gcp_dev  (** GCP dev AMD64 runner, general purpose. *)
    | Gcp_dev_arm64  (** GCP dev ARM64 runner, general purpose. *)
    | Gcp_not_interruptible
        (** GCP prod AMD64 runner, suitable for jobs that should not be interrupted. *)
    | Gcp_not_interruptible_dev
        (** GCP dev AMD64 runner, suitable for jobs that should not be interrupted. *)
    | Gcp_tezt
        (** GCP prod AMD64 runner, suitable for tezt jobs (more RAM and CPU) *)
    | Gcp_tezt_dev
        (** GCP dev AMD64 runner, suitable for tezt jobs (more RAM and CPU) *)
    | Gcp_high_cpu
        (** GCP prod AMD64 runner, suitable for jobs needing high CPU. *)
    | Gcp_high_cpu_dev
        (** GCP dev AMD64 runner, suitable for jobs needing high CPU. *)
    | Gcp_very_high_cpu
        (** GCP prod AMD64 runner, suitable for jobs needing very high CPU. *)
    | Gcp_very_high_cpu_dev
        (** GCP dev AMD64 runner, suitable for jobs needing very high CPU. *)
    | Gcp_very_high_cpu_ramfs
        (** GCP prod AMD64 runner, suitable for jobs needing very high CPU and RAMFS. *)
    | Gcp_very_high_cpu_ramfs_dev
        (** GCP dev AMD64 runner, suitable for jobs needing very high CPU and RAMFS. *)
    | Aws_specific
        (** AWS runners, in cases where a CI is legacy or not suitable for GCP. *)
    | Dynamic
        (** The runner is dynamically set through the CI variable {!dynamic_tag_var}. *)

  (** The variable to set enabling dynamic runner selection.

      To dynamically set the runner of a job through a CI/CD variable,
      assign to this variable using [variables:] or [parallel:matrix:]. *)
  val dynamic_var : Gitlab_ci.Var.t

  (** Convert a tag to a string, suitable to be used in [tag:] clauses. *)
  val show : t -> string

  (** Get the cloud provider specification of a runner from its tag. *)
  val provider : t -> Provider.t option

  (** Get the architecture of a runner from its tag. *)
  val arch : t -> Arch.t option

  (** Get the CPU specification of a runner from its tag. *)
  val cpu : t -> CPU.t option

  (** Get the storage method of a runner from its tag. *)
  val storage : t -> Storage.t option

  (** Get whether a runner can interrupt itself unprompted.

      Interruptible runners should be avoided for publish jobs. *)
  val interruptible : t -> bool option

  (** Test if a tag has exactly some properties.

      Cannot be called on tag [Dynamic]. *)
  val has :
    ?provider:Provider.t ->
    ?arch:Arch.t ->
    ?cpu:CPU.t ->
    ?storage:Storage.t ->
    ?interruptible:bool ->
    t ->
    bool

  (** Choose a runner according to some constraints.

      If no runner satisfy the constraints, return [None].
      If multiple runners satisfy the constraints, return the first one
      according to an internal priority list. *)
  val choose :
    ?provider:Provider.t ->
    ?arch:Arch.t ->
    ?cpu:CPU.t ->
    ?storage:Storage.t ->
    ?interruptible:bool ->
    unit ->
    t option
end
