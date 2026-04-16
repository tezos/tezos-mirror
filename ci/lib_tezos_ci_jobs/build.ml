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
let build_job ~__POS__ ~arch ?storage ~executable_files ?(extra = [])
    ?(extra_artifacts = []) ?dune_cache name =
  CI.job
    name
    ~__POS__
    ~stage:Build
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | _ -> None)
    ?storage
    ~image:Tezos_ci.Images.CI.build
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
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
         (read_lines_from_file executable_files
         @ List.map (( // ) "_build/default") extra
         @ extra_artifacts))
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
    ~extra_artifacts:["src/proto_*/parameters/*.json"]

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
    ~extra_artifacts:
      [
        "src/proto_*/parameters/*.json";
        (* TODO: the protocol compiler executable is already at the root
           as octez-protocol-compiler, do we actually need the following line? *)
        "_build/default/src/lib_protocol_compiler/bin/main_native.exe";
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
    ~extra_artifacts:["src/proto_*/parameters/*.json"]
    ~dune_cache:(match arch with Amd64 -> true | Arm64 -> false)

let job_build_layer1_profiling =
  let profiled_binaries =
    ["octez-node"; "octez-dal-node"; "octez-baker"; "octez-client"]
  in
  let binaries =
    List.map
      (Filename.concat "./octez-binaries/x86_64/profiler/")
      profiled_binaries
    @ List.map
        (Filename.concat "./octez-binaries/x86_64/telemetry/")
        profiled_binaries
  in
  let profiled_binaries_string = String.concat " " profiled_binaries in
  let octez_executables =
    "OCTEZ_EXECUTABLES?=\"" ^ profiled_binaries_string ^ "\""
  in
  CI.job
    "build-layer1-profiling"
    ~__POS__
    ~description:
      "Build some layer1 executables with the profiler PPX enabled, to check \
       that it can still be built."
    ~stage:Test
    ~image:Tezos_ci.Images.CI.build
    ~cpu:Very_high
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~artifacts:
      (Gitlab_ci.Util.artifacts ~expire_in:(Duration (Days 1)) binaries)
    ~variables:[("PROFILE", "static")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "scripts/slim-mode.sh on";
      (* turn on -opaque for all subsequent builds *)
      "scripts/custom-flags.sh set -opaque";
      (* 1) compile with PPX profiling *)
      "TEZOS_PPX_PROFILER=profiling make build " ^ octez_executables;
      "mkdir -p octez-binaries/x86_64/profiler";
      "mv " ^ profiled_binaries_string ^ " octez-binaries/x86_64/profiler";
      (* 2) compile with OpenTelemetry PPX (overwrites binaries) *)
      "TEZOS_PPX_PROFILER=opentelemetry make build " ^ octez_executables;
      "mkdir -p octez-binaries/x86_64/telemetry";
      "mv " ^ profiled_binaries_string ^ " octez-binaries/x86_64/telemetry";
    ]

let job_build_static_linux_released_binaries =
  Cacio.parameterize @@ fun arch ->
  Cacio.parameterize @@ fun mode ->
  let arch_string = Tezos_ci.Runner.Arch.show_easy_to_distinguish arch in
  CI.job
    ("oc.build:static-" ^ arch_string ^ "-linux-released-binaries")
    ~__POS__
    ~description:
      ("Build the static Octez released binaries for " ^ arch_string ^ ".")
    ~stage:Build
    ?allow_failure:(match mode with `test -> None | `release -> Some No)
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | _ -> None)
    ~storage:Ramfs
    ~image:Tezos_ci.Images.CI.build
    ~variables:
      [
        ("ARCH", arch_string);
        ("EXECUTABLE_FILES", "script-inputs/octez-released-executables");
        ("DUNE_BUILD_JOBS", "-j 12");
      ]
    ~artifacts:
      (Gitlab_ci.Util.artifacts
         ?expire_in:
           (match mode with
           | `test -> None
           | `release -> Some (Duration (Days 90)))
         ["octez-binaries/$ARCH/*"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      "eval $(opam env)";
      "./scripts/ci/build_static_binaries.sh";
    ]

let job_build_static_linux_experimental_binaries =
  Cacio.parameterize @@ fun arch ->
  let arch_string = Tezos_ci.Runner.Arch.show_easy_to_distinguish arch in
  CI.job
    ("oc.build:static-" ^ arch_string ^ "-linux-experimental-binaries")
    ~__POS__
    ~description:
      ("Build the static Octez experimental binaries for " ^ arch_string ^ ".")
    ~stage:Build
    ~only_if_changed:(Tezos_ci.Changeset.encode Changesets.changeset_octez)
    ~arch
    ?cpu:(match arch with Amd64 -> Some Very_high | _ -> None)
    ~storage:Ramfs
    ~image:Tezos_ci.Images.CI.build
    ~variables:
      [
        ("ARCH", arch_string);
        ("EXECUTABLE_FILES", "script-inputs/octez-experimental-executables");
        ("VERSION_EXECUTABLE", "octez-baker-alpha");
        ("DUNE_BUILD_JOBS", "-j 12");
      ]
    ~artifacts:(Gitlab_ci.Util.artifacts ["octez-binaries/$ARCH/*"])
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      "eval $(opam env)";
      "./scripts/ci/build_static_binaries.sh";
    ]

let register () =
  (* We do not add manual jobs to [merge_train] pipelines,
     only to [before_merging] pipelines. *)
  Cacio.register_jobs
    Before_merging
    [
      (Manual, build_octez_source);
      (Manual, job_build_released Arm64);
      (Manual, job_build_extra_dev Arm64);
      (Manual, job_build_exp Arm64);
      (Manual, job_build_static_linux_released_binaries Arm64 `test);
      (Manual, job_build_static_linux_experimental_binaries Arm64);
    ] ;
  (* Even though the build jobs are automatically added by Cacio as dependencies
     of test jobs, we explicitly want to make sure that the build jobs run
     even if the tests need not be run. *)
  Cacio.register_merge_request_jobs
    [
      (Auto, job_build_released Amd64);
      (Auto, job_build_extra_dev Amd64);
      (Auto, job_build_exp Amd64);
      (Auto, job_build_layer1_profiling);
      (Auto, job_build_static_linux_released_binaries Amd64 `test);
      (Auto, job_build_static_linux_experimental_binaries Amd64);
    ] ;
  Cacio.register_jobs
    Schedule_extended_test
    [
      (Auto, build_octez_source);
      (Auto, job_build_released Amd64);
      (Auto, job_build_extra_dev Amd64);
      (Auto, job_build_exp Amd64);
      (Auto, job_build_released Arm64);
      (Auto, job_build_extra_dev Arm64);
      (Auto, job_build_exp Arm64);
      (Auto, job_build_layer1_profiling);
      (Auto, job_build_static_linux_released_binaries Arm64 `test);
      (Auto, job_build_static_linux_experimental_binaries Arm64);
    ] ;
  Cacio.register_jobs
    Master
    [
      (Manual, job_build_released Arm64);
      (Manual, job_build_extra_dev Arm64);
      (Manual, job_build_exp Arm64);
      (Auto, job_build_static_linux_released_binaries Amd64 `test);
      (Auto, job_build_static_linux_released_binaries Arm64 `test);
      (Auto, job_build_static_linux_experimental_binaries Amd64);
      (Auto, job_build_static_linux_experimental_binaries Arm64);
    ] ;
  ()
