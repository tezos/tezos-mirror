(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [schedule_cache_refresh] pipeline.


   TODO add details. *)

open Tezos_ci

let jobs =
  Code_verification.jobs Merge_train
  @ !Hooks.before_merging
  @ Debian_repository.jobs Full
  @ Rpm_repository.jobs Full
  |> List.filter Tezos_ci.has_cache_or_start_images_stages
  |> List.map (Tezos_ci.set_tezos_job_cache_policy Gitlab_ci.Types.Push)
  |> List.map Tezos_ci.no_rules
  |> List.map Tezos_ci.when_always
  |> List.map (Tezos_ci.with_interruptible false)
