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

let job_create_homebrew_formula =
  CI.job
    "oc.create-homebrew-formula"
    ~__POS__
    ~stage:Build
    ~description:
      "Create a Homebrew formula from a template using the homebrew_release.sh \
       script"
    ~arch:Amd64
    ~image
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~expire_in:(Duration (Days 1))
         ["public/homebrew/*"])
    ~only_if_changed:(Tezos_ci.Changeset.encode changeset_homebrew)
    ~script:["./scripts/packaging/homebrew_release.sh"]

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
    ~needs:[(Job, job_create_homebrew_formula)]
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
    ~needs:[(Job, job_create_homebrew_formula)]
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
