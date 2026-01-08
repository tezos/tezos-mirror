(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Tezos_ci.Cache

type homebrew_pipeline = Full | Release

let image = Images.Base_images.homebrew

let stage = Stages.build

(* this job creates a formula from a template
     using the homebrew_release.sh script *)
let job_create_homebrew_formula : tezos_job =
  job
    ~__POS__
    ~name:"oc.create-homebrew-formula"
    ~arch:Amd64
    ~image
    ~stage
    ["./scripts/packaging/homebrew_release.sh"]

(* this job tests if the formula created by job_create_homebrew_formula
     can be compiled and installed *)
let job_build_homebrew_formula : tezos_job =
  job
    ~__POS__
    ~name:"oc.build-homebrew-formula"
    ~arch:Amd64
    ~cpu:Very_high
    ~allow_failure:No
    ~image
    ~stage
    ~dependencies:(Dependent [Job job_create_homebrew_formula])
    ~variables:[("DUNE_BUILD_JOBS", "-j 12")]
    ["./scripts/packaging/test_homebrew_install.sh"]
  |> enable_networked_cargo

let job_build_homebrew_formula_macosx : tezos_job =
  job
    ~__POS__
    ~name:"oc.build-homebrew-formula-macosx"
    ~image:Images.macosx_15
    ~variables:[("TAGS", "saas-macos-large-m2pro")]
    ~dependencies:(Dependent [Job job_create_homebrew_formula])
    ~stage
    ~description:"Run the homebrew installation on MacOSX 15"
    ~allow_failure:Yes
    ~tag:Dynamic
    [
      "./scripts/packaging/homebrew_install.sh";
      "eval $(/opt/homebrew/bin/brew shellenv)";
      "./scripts/packaging/test_homebrew_install.sh";
    ]

let jobs pipeline_type : tezos_job list =
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
      "A child pipeline of 'before_merging' building and testing the homebrew \
       packaging. Manually triggered."
    ~jobs:(jobs Full)

let child_pipeline_full_auto =
  Pipeline.register_child
    "homebrew_auto"
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       and testing the homebrew packaging. Starts automatically on certain \
       conditions."
    ~jobs:(jobs Full)
