(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
open Tezos_ci.Cache

let image = Images.Base_images.debian_homebrew_trixie

let stage = Stages.build

(* this job creates a formula from a template
     using the homebrew_release.sh script *)
let make_job_create_homebrew_formula ?rules ?(dependencies = Dependent []) () :
    tezos_job =
  job
    ~__POS__
    ~name:"oc.create-homebrew-formula"
    ~arch:Amd64
    ~image
    ~stage
    ?rules
    ~dependencies
    ["./scripts/packaging/homebrew_release.sh"]

let job_create_homebrew_formula : tezos_job =
  make_job_create_homebrew_formula ()

(* this job tests if the formula created by job_create_homebrew_formula
     can be compiled and installed *)
let make_job_build_homebrew_formula ?rules ~create_job () : tezos_job =
  job
    ~__POS__
    ~name:"oc.build-homebrew-formula"
    ~arch:Amd64
    ~cpu:Very_high
    ~allow_failure:No
    ~image
    ~stage
    ?rules
    ~dependencies:(Dependent [Job create_job])
    ~variables:
      [
        ("DUNE_BUILD_JOBS", "-j 12");
        ("HOMEBREW_KISSCACHE", "http://kisscache.kisscache.svc.cluster.local");
        ("HOMEBREW_OPAMFETCH", "scripts/kiss-fetch.sh");
      ]
    [
      (* configure cargo to use the [crates-io-proxy] internal
         mirror instead of crates.io *)
      "cp images/ci/.cargo/config.toml .cargo/";
      "./scripts/packaging/test_homebrew_install.sh";
    ]
  |> enable_networked_cargo

let job_build_homebrew_formula : tezos_job =
  make_job_build_homebrew_formula ~create_job:job_create_homebrew_formula ()

let make_job_build_homebrew_formula_macosx ?rules ~create_job () : tezos_job =
  job
    ~__POS__
    ~name:"oc.build-homebrew-formula-macosx"
    ~image:(Image.mk_external ~image_path:"$MACOS_IMAGE")
    ~variables:[("TAGS", "saas-macos-large-m2pro")]
    ~parallel:
      (Matrix [[("MACOS_IMAGE", ["macos-15-xcode-16"; "macos-26-xcode-26"])]])
    ~dependencies:(Dependent [Job create_job])
    ~stage
    ~description:"Run the homebrew installation on MacOSX"
    ~allow_failure:Yes
    ?rules
    ~tag:Dynamic
    [
      "./scripts/packaging/homebrew_install.sh";
      "eval $(/opt/homebrew/bin/brew shellenv)";
      "./scripts/packaging/test_homebrew_install.sh";
    ]

let job_build_homebrew_formula_macosx : tezos_job =
  make_job_build_homebrew_formula_macosx
    ~create_job:job_create_homebrew_formula
    ()

let jobs =
  [
    Tezos_ci.job_datadog_pipeline_trace;
    job_build_homebrew_formula;
    job_create_homebrew_formula;
    job_build_homebrew_formula_macosx;
  ]
