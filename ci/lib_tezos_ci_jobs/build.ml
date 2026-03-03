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

(* Common features of build jobs that use [build_full_unreleased.sh]. *)
let build_job ~__POS__ ~arch ?storage ~executable_files ?(extra = []) ~artifacts
    ?dune_cache name =
  CI.job
    name
    ~__POS__
    ~stage:Build
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | _ -> None)
    ?storage
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:
      ((* TODO: why "or_doc"? *)
       Tezos_ci.Changeset.encode
         Changesets.changeset_octez_or_doc)
    ~variables:
      (("EXECUTABLE_FILES", executable_files)
      ::
      (match extra with
      | [] -> []
      | _ :: _ -> [("BUILD_EXTRA", String.concat " " extra)]))
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ~name:
           (sf
              "build-%s-$CI_COMMIT_REF_SLUG"
              (Tezos_ci.Runner.Arch.show_easy_to_distinguish arch))
         ~when_:On_success
         ~expire_in:(Duration (Days 1))
         artifacts)
    ?dune_cache
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ~policy:Pull_push ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/build_full_unreleased.sh";
    ]

let job_build_released =
  Cacio.parameterize @@ fun arch ->
  build_job
    (sf
       "oc.build_%s-released"
       (Tezos_ci.Runner.Arch.show_easy_to_distinguish arch))
    ~__POS__
    ~description:"Build the set of released executables for Octez, for amd64."
    ~arch
    ~storage:Ramfs
    ~executable_files:"script-inputs/released-executables"
    ~artifacts:["octez-*"; "src/proto_*/parameters/*.json"]

let job_build_extra_dev =
  Cacio.parameterize @@ fun arch ->
  build_job
    (sf
       "oc.build_%s-extra-dev"
       (Tezos_ci.Runner.Arch.show_easy_to_distinguish arch))
    ~__POS__
    ~description:
      "Build the set of developer executables, as well as the TPS evaluation \
       tool, Octogram, the main Tezt executable, and the Octez injector \
       server."
    ~arch
    ~executable_files:"script-inputs/dev-executables"
    ~extra:
      ([
         "src/bin_tps_evaluation/main_tps_evaluation.exe";
         "src/bin_octogram/octogram_main.exe";
         "tezt/tests/main.exe";
       ]
      @
      match arch with
      | Amd64 -> ["contrib/octez_injector_server/octez_injector_server.exe"]
      | _ -> [])
    ~artifacts:
      [
        "octez-*";
        "octez-teztale-*";
        "src/proto_*/parameters/*.json";
        "_build/default/src/lib_protocol_compiler/bin/main_native.exe";
        "_build/default/tezt/tests/main.exe";
        "_build/default/contrib/octez_injector_server/octez_injector_server.exe";
        "etherlink-governance-observer";
        "fa-bridge-watchtower";
      ]
    ~dune_cache:(match arch with Amd64 -> true | Arm64 -> false)

let job_build_exp =
  Cacio.parameterize @@ fun arch ->
  build_job
    (sf "oc.build_%s-exp" (Tezos_ci.Runner.Arch.show_easy_to_distinguish arch))
    ~__POS__
    ~description:"Build the set of experimental executables."
    ~arch
    ~executable_files:"script-inputs/experimental-executables"
    ~artifacts:
      [
        (* TODO: clean up this list, which was originally shared with
           [job_build_x86_64_extra_dev] but with no good reason since the two jobs
           do not build the same set of executables. *)
        "octez-*";
        "octez-teztale-*";
        "src/proto_*/parameters/*.json";
        "_build/default/src/lib_protocol_compiler/bin/main_native.exe";
        "_build/default/tezt/tests/main.exe";
        "_build/default/contrib/octez_injector_server/octez_injector_server.exe";
        "etherlink-governance-observer";
        "fa-bridge-watchtower";
      ]
    ~dune_cache:true

let register () =
  (* We do not add manual jobs to [merge_train] pipelines,
     only to [before_merging] pipelines. *)
  CI.register_before_merging_jobs
    [
      (Manual, build_octez_source);
      (Manual, job_build_released Arm64);
      (Manual, job_build_extra_dev Arm64);
    ] ;
  (* Even though the build jobs are automatically added by Cacio as dependencies
     of test jobs, we explicitly want to make sure that the build jobs run
     even if the tests need not be run. *)
  CI.register_merge_request_jobs
    [
      (Auto, job_build_released Amd64);
      (Auto, job_build_extra_dev Amd64);
      (Auto, job_build_exp Amd64);
    ] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, build_octez_source);
      (Auto, job_build_released Amd64);
      (Auto, job_build_extra_dev Amd64);
      (Auto, job_build_exp Amd64);
      (Auto, job_build_released Arm64);
      (Auto, job_build_extra_dev Arm64);
    ] ;
  CI.register_master_jobs
    [(Manual, job_build_released Arm64); (Manual, job_build_extra_dev Arm64)] ;
  ()
