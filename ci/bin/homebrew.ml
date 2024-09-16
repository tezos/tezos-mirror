(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Common

type homebrew_pipeline = Full | Release

let jobs pipeline_type : tezos_job list =
  let image = Images.debian_bookworm in
  let stage = Stages.build in

  (* this job creates a formula from a template
     using the homebrew_release.sh script *)
  let job_create_homebrew_formula : tezos_job =
    job
      ~__POS__
      ~name:"oc.build-homebrew"
      ~arch:Amd64
      ~image
      ~stage
      [
        "./scripts/ci/install-gsutil.sh";
        "apt-get update && apt-get install -y git curl";
        "./scripts/packaging/homebrew_release.sh";
      ]
    |> enable_networked_cargo
  in

  (* this job tests if the formula created by job_create_homebrew_formula
     can be compiled and installed *)
  let job_build_homebrew_formula : tezos_job =
    job
      ~__POS__
      ~name:"oc.install-homebrew"
      ~arch:Amd64
      ~image
      ~stage
      ~dependencies:(Dependent [Job job_create_homebrew_formula])
      ~before_script:
        [
          "apt-get update && apt-get install -y git curl";
          "./scripts/packaging/homebrew_install.sh";
          "eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)";
        ]
      [
        (* These packages are needed on Linux. For macOS, Homebrew will
           make those available locally. *)
        "apt-get install -y autoconf cmake g++ libev-dev libffi-dev libgmp-dev \
         libprotobuf-dev libsqlite3-dev protobuf-compiler libhidapi-dev \
         pkg-config zlib1g-dev libpq-dev";
        "./scripts/packaging/test_homebrew_install.sh";
      ]
    |> enable_networked_cargo
  in
  match pipeline_type with
  | Release -> [job_create_homebrew_formula]
  | Full -> [job_build_homebrew_formula; job_create_homebrew_formula]

let child_pipeline_release =
  let jobs : tezos_job list = jobs Release in
  Pipeline.register_child
    "homebrew_release"
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       homebrew packages."
    ~jobs

let jobs pipeline_type = job_datadog_pipeline_trace :: jobs pipeline_type

let child_pipeline_full =
  let jobs = jobs Full in
  Pipeline.register_child
    "homebrew"
    ~description:
      "A child pipeline of 'schedule_extended_test' testing the build of  \
       homebrew packages."
    ~jobs

let child_pipeline_full_auto =
  let jobs = jobs Full in
  Pipeline.register_child
    "homebrew_auto"
    ~description:
      "A child pipeline of 'schedule_extended_test' testing the build of all \
       .deb packages. This pipelines starts automatically"
    ~jobs
