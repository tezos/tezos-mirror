(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs related to security scans.
   - Vulnerability detection in Docker images. *)

open Tezos_ci

let jobs : tezos_job list = [Common.job_datadog_pipeline_trace]
