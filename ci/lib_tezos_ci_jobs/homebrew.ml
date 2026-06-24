(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_ci
module CI = Cacio.Shared
open Changesets

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

(* This CIAO job is used as the base for the release pipeline's
   [job_build_homebrew_release] in [release_tag.ml], which adds artifacts to it. *)
let job_create_homebrew_formula : tezos_job =
  make_job_create_homebrew_formula ()

let job_build_homebrew_formula =
  CI.job
    "oc.build-homebrew-formula"
    ~__POS__
    ~stage:Test
    ~description:
      "Tests if the formula created by oc.create-homebrew-formula can be \
       compiled and installed"
    ~arch:Amd64
    ~cpu:Very_high
    ~allow_failure:No
    ~image
    ~needs_legacy:[(Job, job_create_homebrew_formula)]
    ~only_if_changed:(Tezos_ci.Changeset.encode changeset_homebrew)
    ~variables:
      [
        ("DUNE_BUILD_JOBS", "-j 12");
        ("HOMEBREW_KISSCACHE", "http://kisscache.kisscache.svc.cluster.local");
        ("HOMEBREW_OPAMFETCH", "scripts/kiss-fetch.sh");
        ("CARGO_NET_OFFLINE", "false");
      ]
    ~script:
      [
        "cp images/ci/.cargo/config.toml .cargo/";
        "./scripts/packaging/test_homebrew_install.sh";
      ]

let job_build_homebrew_formula_macosx =
  CI.job
    "oc.build-homebrew-formula-macosx"
    ~__POS__
    ~stage:Test
    ~description:"Run the homebrew installation on MacOSX"
    ~image:(Image.mk_external ~image_path:"$MACOS_IMAGE")
    ~allow_failure:Yes
    ~needs_legacy:[(Job, job_create_homebrew_formula)]
    ~only_if_changed:(Tezos_ci.Changeset.encode changeset_homebrew)
    ~parallel:
      (Matrix [[("MACOS_IMAGE", ["macos-15-xcode-16"; "macos-26-xcode-26"])]])
    ~tag:Dynamic
    ~variables:[("TAGS", "saas-macos-large-m2pro")]
    ~script:
      [
        "./scripts/packaging/homebrew_install.sh";
        "eval $(/opt/homebrew/bin/brew shellenv)";
        "./scripts/packaging/test_homebrew_install.sh";
      ]

let jobs = [Tezos_ci.job_datadog_pipeline_trace; job_create_homebrew_formula]

let () =
  Cacio.register_merge_request_jobs
    [
      (Cacio.Auto, job_build_homebrew_formula);
      (Cacio.Auto, job_build_homebrew_formula_macosx);
    ] ;
  Cacio.register_jobs
    Cacio.Homebrew_daily
    [
      (Cacio.Auto, job_build_homebrew_formula);
      (Cacio.Auto, job_build_homebrew_formula_macosx);
    ]
