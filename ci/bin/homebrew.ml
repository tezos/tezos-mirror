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
      ~cpu:Very_high
      ~allow_failure:Yes
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

  let job_build_homebrew_formula_macosx : tezos_job =
    job
      ~__POS__
      ~name:"oc.install-homebrew-macosx"
      ~image:Images.macosx_14
      ~variables:[("TAGS", "saas-macos-medium-m1")]
      ~dependencies:(Dependent [Job job_create_homebrew_formula])
      ~stage
      ~description:"Run the homebrew installation on gitlab MacOSX runners"
      ~allow_failure:Yes
      ~tag:Dynamic
      [
        "./scripts/packaging/homebrew_install.sh";
        "eval $(/opt/homebrew/bin/brew shellenv)";
        "./scripts/packaging/test_homebrew_install.sh";
      ]
  in

  match pipeline_type with
  | Release -> [job_create_homebrew_formula]
  | Full ->
      [
        job_build_homebrew_formula;
        job_create_homebrew_formula;
        job_build_homebrew_formula_macosx;
      ]

let jobs pipeline_type = job_datadog_pipeline_trace :: jobs pipeline_type

let child_pipeline_full =
  Pipeline.register_child
    "homebrew"
    ~description:
      "A child pipeline of 'schedule_extended_test' testing the build of  \
       homebrew packages."
    ~jobs:(jobs Full)

let child_pipeline_full_auto =
  Pipeline.register_child
    "homebrew_auto"
    ~description:
      "A child pipeline of 'schedule_extended_test' testing the homebrew \
       packaging. This pipelines starts automatically"
    ~jobs:(jobs Full)
