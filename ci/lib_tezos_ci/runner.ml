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

  let show = function
    | Normal -> "normal"
    | High -> "high"
    | Very_high -> "very_high"
end

module Storage = struct
  type t = Network | Ramfs

  let show = function Network -> "network" | Ramfs -> "ramfs"
end

module Tag = struct
  (* IMPORTANT: if you add a tag to this list, also add it to [list] just below. *)
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

  (* List of all tags ([Dynamic] excluded as it is not a real tag;
     it is instantiated by one of the other tags).

     IMPORTANT: in this list, tags are ordered in order of priority.
     The [choose] function will select the first tag from this list
     that should be chosen if there is an ambiguity. *)
  let list =
    [
      Gcp;
      Gcp_dev;
      Gcp_not_interruptible;
      Gcp_not_interruptible_dev;
      Gcp_high_cpu;
      Gcp_high_cpu_dev;
      Gcp_very_high_cpu;
      Gcp_very_high_cpu_dev;
      Gcp_very_high_cpu_ramfs;
      Gcp_very_high_cpu_ramfs_dev;
      Gcp_tezt;
      Gcp_tezt_dev;
      Gcp_arm64;
      Gcp_dev_arm64;
      Aws_specific;
    ]

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

  let cpu : t -> CPU.t option = function
    | Gcp_arm64 | Gcp_dev_arm64 | Gcp | Gcp_dev | Gcp_not_interruptible
    | Gcp_not_interruptible_dev | Aws_specific ->
        Some Normal
    | Gcp_high_cpu | Gcp_high_cpu_dev -> Some High
    | Gcp_very_high_cpu | Gcp_very_high_cpu_dev | Gcp_very_high_cpu_ramfs
    | Gcp_very_high_cpu_ramfs_dev ->
        Some Very_high
    | Gcp_tezt | Gcp_tezt_dev ->
        failwith "not implemented: Runner.Tag.cpu for Tezt runners"
    | Dynamic -> None

  let storage : t -> Storage.t option = function
    | Gcp | Gcp_dev | Gcp_not_interruptible | Gcp_not_interruptible_dev
    | Gcp_tezt | Gcp_tezt_dev | Gcp_high_cpu | Gcp_high_cpu_dev
    | Gcp_very_high_cpu | Gcp_very_high_cpu_dev | Aws_specific ->
        Some Network
    | Gcp_arm64 | Gcp_dev_arm64 | Gcp_very_high_cpu_ramfs
    | Gcp_very_high_cpu_ramfs_dev ->
        Some Ramfs
    | Dynamic -> None

  let has ?arch:requested_arch ?cpu:requested_cpu ?storage:requested_storage tag
      =
    let is get_from_tag = function
      | None -> true
      | Some requested -> (
          match get_from_tag tag with
          | None -> invalid_arg "cannot call Runner.Tag.has on Dynamic"
          | Some actual -> actual = requested)
    in
    is arch requested_arch && is cpu requested_cpu
    && is storage requested_storage

  let choose ?arch ?cpu ?storage () =
    List.find_opt (has ?arch ?cpu ?storage) list
end
