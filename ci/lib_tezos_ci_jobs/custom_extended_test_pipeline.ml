(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [custom_extended_test_pipeline]
   pipeline.

   This generic pipeline aims to be used to run all tests of the code
   base with some particular options.  The goal of this pipeline is to
   test behaviours that are not enabled by default on the node, and
   thus, not tested through the tezt jobs. To do so, a particular
   configuration is set for each node that is run in the tezt jobs so
   that the feature to test is enabled.

   See [ci/bin/main.ml] for the list of custom extended test pipelines. *)

open Tezos_ci

(* The build_x86_64 jobs are split in two to keep the artifact size
   under the 1GB hard limit set by GitLab. *)

let jobs () =
  (* The jobs we want to run are a subset of the Tezt jobs that run
     in the schedule_extended_test pipeline. They are defined in [tezt.ml]. *)
  Cacio.get_custom_extended_test_jobs () @ [job_datadog_pipeline_trace]
