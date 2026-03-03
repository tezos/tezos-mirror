(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines build jobs that are not component-specific.
   As such, the jobs are defined in the [Shared] component. *)

module CI = Cacio.Shared

(* This job is [~forced] to appear in all pipelines because there does not appear
   to be an obvious changeset that would work. Looking at the history,
   this job was triggered in some MRs that didn't modify [changeset_octez].
   TODO: it could be a good idea to require a label to be added to the MR instead. *)
let build_octez_source =
  CI.job
    "build_octez_source"
    ~__POS__
    ~description:"Check compilation of the Octez tarball."
    ~stage:Test
    ~force:true
    ~image:Tezos_ci.Images.CI.build
    ~cpu:Very_high
    ~storage:Ramfs
    ~variables:[("DUNE_BUILD_JOBS", "-j 12")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/restrict_export_to_octez_source.sh";
      "./scripts/ci/create_octez_tarball.sh octez";
      "mv octez.tar.bz2 ../";
      "cd ../";
      "tar xf octez.tar.bz2";
      "cd octez/";
      "eval $(opam env)";
      "make octez";
    ]

let job_build_x86_64_released =
  CI.job
    "oc.build_x86_64-released"
    ~__POS__
    ~description:"Build the set of released executables for Octez, for amd64."
    ~stage:Build
    ~arch:Amd64
    ~cpu:Very_high
    ~storage:Ramfs
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:
      ((* TODO: why "or_doc"? *)
       Tezos_ci.Changeset.encode
         Changesets.changeset_octez_or_doc)
    ~variables:[("EXECUTABLE_FILES", "script-inputs/released-executables")]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:"build-x86_64-$CI_COMMIT_REF_SLUG"
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         ["octez-*"; "src/proto_*/parameters/*.json"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ~policy:Pull_push ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/build_full_unreleased.sh";
    ]

let register () =
  (* Since [build_octez_source] is manual, we do not add it to [merge_train] pipelines,
     only to [before_merging] pipelines. *)
  CI.register_before_merging_jobs [(Manual, build_octez_source)] ;
  (* Even though the build jobs are automatically added by Cacio as dependencies
     of test jobs, we explicitly want to make sure that the build jobs run
     even if the tests need not be run. *)
  CI.register_merge_request_jobs [(Auto, job_build_x86_64_released)] ;
  CI.register_schedule_extended_test_jobs
    [(Auto, build_octez_source); (Auto, job_build_x86_64_released)] ;
  ()
