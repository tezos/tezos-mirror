(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [ocaml4_build] child
   pipeline.
*)
open Tezos_ci
open Common

let ocaml4_build_packages_image =
  Image.mk_external
    ~image_path:
      "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/oc.build-deps-ocaml4"

let jobs =
  let job_docker_build_ocaml4_dependencies =
    job_docker_authenticated
      ~__POS__
      ~stage:Stages.images
      ~name:"oc.build-deps-ocaml4"
      [
        "./scripts/ci/ocaml4_job/build_ocaml4_dependencies.sh \
         images/packages/debian-deps-build.Dockerfile";
      ]
  in
  let job_build_ocaml4 =
    job
      ~__POS__
      ~name:"oc.build-ocaml4"
      ~image:ocaml4_build_packages_image
      ~stage:Stages.build
      ~dependencies:(Dependent [Job job_docker_build_ocaml4_dependencies])
      ~retry:Gitlab_ci.Types.{max = 1; when_ = [Stuck_or_timeout_failure]}
      ["./scripts/ci/ocaml4_job/build_ocaml4_octez.sh"]
  in
  [job_docker_build_ocaml4_dependencies; job_build_ocaml4]

let child_pipeline_ocaml4 =
  Pipeline.register_child
    "ocaml4_manual"
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       octez with OCaml 4."
    ~jobs:(job_datadog_pipeline_trace :: jobs)
