(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Arch = struct
  type t = Amd64 | Arm64

  let show_uniform = function Amd64 -> "amd64" | Arm64 -> "arm64"

  let show_easy_to_distinguish = function Amd64 -> "x86_64" | Arm64 -> "arm64"
end

module CPU = struct
  type t = Normal | High | Very_high
end

module Storage = struct
  type t = Network | Ramfs
end

module Tag = struct
  type t =
    | Gcp
    | Gcp_arm64
    | Gcp_dev
    | Gcp_dev_arm64
    | Gcp_not_interruptible
    | Gcp_not_interruptible_dev
    | Gcp_tezt
    | Gcp_tezt_dev
    | Gcp_high_cpu
    | Gcp_high_cpu_dev
    | Gcp_very_high_cpu
    | Gcp_very_high_cpu_dev
    | Gcp_very_high_cpu_ramfs
    | Gcp_very_high_cpu_ramfs_dev
    | Aws_specific
    | Dynamic

  let dynamic_var = Gitlab_ci.Var.make "TAGS"

  let show = function
    | Gcp -> "gcp"
    | Gcp_arm64 -> "gcp_arm64"
    | Gcp_dev -> "gcp_dev"
    | Gcp_dev_arm64 -> "gcp_dev_arm64"
    | Gcp_not_interruptible -> "gcp_not_interruptible"
    | Gcp_not_interruptible_dev -> "gcp_not_interruptible_dev"
    | Gcp_tezt -> "gcp_tezt"
    | Gcp_tezt_dev -> "gcp_tezt_dev"
    | Gcp_high_cpu -> "gcp_high_cpu"
    | Gcp_high_cpu_dev -> "gcp_high_cpu_dev"
    | Gcp_very_high_cpu -> "gcp_very_high_cpu"
    | Gcp_very_high_cpu_dev -> "gcp_very_high_cpu_dev"
    | Gcp_very_high_cpu_ramfs -> "gcp_very_high_cpu_ramfs"
    | Gcp_very_high_cpu_ramfs_dev -> "gcp_very_high_cpu_ramfs_dev"
    | Aws_specific -> "aws_specific"
    | Dynamic -> Gitlab_ci.Var.encode dynamic_var

  let arch : t -> Arch.t option = function
    | Gcp_arm64 | Gcp_dev_arm64 -> Some Arm64
    | Gcp | Gcp_dev | Gcp_not_interruptible | Gcp_not_interruptible_dev
    | Gcp_tezt | Gcp_tezt_dev | Gcp_high_cpu | Gcp_high_cpu_dev
    | Gcp_very_high_cpu | Gcp_very_high_cpu_dev | Gcp_very_high_cpu_ramfs
    | Gcp_very_high_cpu_ramfs_dev | Aws_specific ->
        Some Amd64
    | Dynamic -> None
end
