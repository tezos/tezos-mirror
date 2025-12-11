(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024-2025 TriliTech <contact@trili.tech>                    *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [code_verification] pipeline.

   This pipeline comes in two variants:

   - The [before_merging] pipeline runs on merge requests. Jobs in
   this pipeline are conditional on the set of [~changes] in the merge
   request. The goal is to only run those jobs whose outcome is
   affected by the merge request.

   - The [schedule_extended_test] pipeline runs daily on the [master]
   branch. It contains a set of jobs that are too slow to run in merge
   request pipelines and a large subset of the [before_merging]
   pipeline in addition. This subset excludes jobs that are irrelevant
   in a non-merge request context, like the commit title check. Jobs
   in this pipeline should run [Always] -- unless they depend on
   artifacts of another job in which case they should run
   [On_success]. The goal of this pipeline is to catch any breaking
   changes that might've slipped through the [before_merging]
   pipeline.

   When adding new jobs to the [code_verification] pipeline, make sure
   that it appears in both variants as applicable, with the
   appropriate rules. *)

open Gitlab_ci
open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache
open Common.Docker
open Common.Build
open Common.Helpers
open Changesets

(** Variants of the code verification pipeline.

    Encodes the conditional [before_merging] pipeline, the [merge_train]
    and its unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline =
  | Before_merging
  | Schedule_extended_test
  | Merge_train

let code_verification_pipeline_name = function
  | Before_merging -> "before_merging"
  | Schedule_extended_test -> "schedule_extended_test"
  | Merge_train -> "merge_train"

(** Configuration of manual jobs for [make_rules] *)
type manual =
  | No  (** Do not add rule for manual start. *)
  | Yes  (** Add rule for manual start. *)
  | On_changes of Changeset.t  (** Add manual start on certain [changes:] *)

(* [make_rules] makes rules for jobs that are:
   - automatic in scheduled pipelines;
   - conditional in [before_merging] pipelines.

   If a job has non-optional dependencies, then [dependent] must be
   set to [true] to ensure that we only run the job in case previous
   jobs succeeded (setting [when: on_success]).

   If [label] is set, add rule that selects the job in
   [Before_merging] pipelines for merge requests with the given
   label. Rules for manual start can be configured using [manual].

   If [label], [changes] and [manual] are omitted, then rules will
   enable the job [On_success] in the [before_merging] pipeline. This
   is safe, but prefer specifying a [changes] clause if possible.

   If [final_pipeline_disable] is set to true (default false), this job is
   disabled in final [Before_merging] pipelines. *)
let make_rules ~pipeline_type ?label ?changes ?(manual = No)
    ?(dependent = false) ?(final_pipeline_disable = false) () =
  match pipeline_type with
  | Schedule_extended_test ->
      (* The scheduled pipeline always runs all jobs unconditionally
           -- unless they are dependent on a previous job (which is
           not [job_start] defined below), in the pipeline. *)
      [job_rule ~when_:(if dependent then On_success else Always) ()]
  | Before_merging | Merge_train -> (
      (* MR labels can be used to force tests to run. *)
      (if final_pipeline_disable then
         [job_rule ~if_:Rules.is_final_pipeline ~when_:Never ()]
       else [])
      @ (match label with
        | Some label ->
            [job_rule ~if_:Rules.(has_mr_label label) ~when_:On_success ()]
        | None -> [])
      (* Modifying some files can force tests to run. *)
      @ (match changes with
        | None -> []
        | Some changes ->
            [job_rule ~changes:(Changeset.encode changes) ~when_:On_success ()])
      (* It can be relevant to start some jobs manually. *)
      @
      match manual with
      | No -> []
      | Yes -> [job_rule ~when_:Manual ()]
      | On_changes changes ->
          [job_rule ~when_:Manual ~changes:(Changeset.encode changes) ()])

(* Define the [start] job.

   The purpose of this job is to implement a manual trigger
   for [Before_merging] pipelines, instead of running it on
   each update to the merge request. *)
let job_start =
  job
    ~__POS__
    ~image:Images.datadog_ci
    ~stage:Stages.start
    ~rules:
      [
        job_rule
          ~if_:(If.not Rules.is_final_pipeline)
          ~allow_failure:No
          ~when_:Manual
          ();
        job_rule ~when_:Always ();
      ]
    ~timeout:(Minutes 10)
    ~name:"trigger"
    [
      "echo 'Trigger pipeline!'";
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

(* Short-cut for jobs that have no dependencies except [job_start] on
   [Before_merging] pipelines. *)
let dependencies_needs_start pipeline_type =
  match pipeline_type with
  | Before_merging | Merge_train -> Dependent [Job job_start]
  | Schedule_extended_test -> Dependent []

(* Use this function to define jobs that depend on the pipeline type.
   Without this function, you risk defining the same job multiple times
   for the same pipeline type. *)
let depending_on_pipeline_type :
    (code_verification_pipeline -> 'a) -> code_verification_pipeline -> 'a =
  (* Same as [Cacio.parameterize]: this is just a memoization function.
     We just specialize its type to make it more clear what we are doing. *)
  Cacio.parameterize

(* The build_x86_64 jobs are split in two to keep the artifact size
   under the 1GB hard limit set by GitLab. *)
(* [job_build_x86_64_release] builds the released executables. *)
let job_build_x86_64_release =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_released_binaries
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~storage:Ramfs
    ~dependencies:(dependencies_needs_start pipeline_type)
    ~rules:(make_rules ~pipeline_type ~changes:changeset_octez_or_doc ())
    ()

(* 'oc.build_x86_64-exp-dev-extra' builds the developer and experimental
   executables, as well as the tezt test suite used by the subsequent
   'tezt' jobs and TPS evaluation tool. *)
let job_build_x86_64_extra_dev =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_dynamic_binaries
    ~name:"oc.build_amd64-extra-dev"
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~dependencies:(dependencies_needs_start pipeline_type)
    ~rules:(make_rules ~pipeline_type ~changes:changeset_octez_or_doc ())
    ~extra:true
    "script-inputs/dev-executables"
  |> enable_dune_cache ~key:Pipeline ~policy:Push

let job_build_x86_64_exp =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_dynamic_binaries
    ~name:"oc.build_amd64-exp"
    ~__POS__
    ~arch:Amd64
    ~cpu:Very_high
    ~dependencies:(dependencies_needs_start pipeline_type)
    ~rules:(make_rules ~pipeline_type ~changes:changeset_octez_or_doc ())
    "script-inputs/experimental-executables"
  |> enable_dune_cache ~key:Pipeline ~policy:Push

let build_arm_rules ~pipeline_type =
  make_rules ~pipeline_type ~label:"ci--arm64" ~manual:Yes ()

let job_build_arm64_release =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_arm64_release ~rules:(build_arm_rules ~pipeline_type) ()

let job_build_arm64_extra_dev =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_arm64_extra_dev ~rules:(build_arm_rules ~pipeline_type) ()

let job_build_arm64_exp =
  depending_on_pipeline_type @@ fun pipeline_type ->
  job_build_arm64_exp ~rules:(build_arm_rules ~pipeline_type) ()

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
  let make_rules = make_rules ~pipeline_type in
  (* Stages *)
  let start_stage =
    match pipeline_type with
    | Schedule_extended_test -> [job_datadog_pipeline_trace]
    | Before_merging | Merge_train -> [job_start]
  in

  (* Used in trigger job definitions. For code verification pipelines,
     add type of parent pipeline. This definition shadows
     [Tezos_ci.trigger_job]. *)
  let trigger_job ~__POS__ ?rules ?dependencies ?description ?variables
      child_pipeline_path =
    trigger_job
      ~__POS__
      ?rules
      ?dependencies
      ?description
      ?variables
      ~parent_pipeline_name:(code_verification_pipeline_name pipeline_type)
      child_pipeline_path
  in
  (* Sanity jobs *)
  let sanity =
    let stage = Stages.sanity in
    let dependencies = Dependent [] in
    (* Necromantic nix-related rites. *)
    let job_nix : tezos_job =
      job
        ~__POS__
        ~name:"nix"
        ~image:Images.nix
        ~stage
        ~dependencies
        ~artifacts:(artifacts ~when_:On_failure ["flake.lock"])
        ~rules:
          (make_rules
             ~changes:
               (Changeset.make ["**/*.nix"; "flake.lock"; "scripts/version.sh"])
             ())
        ~before_script:
          [
            "mkdir -p ~/.config/nix";
            "echo 'extra-experimental-features = flakes nix-command' > \
             ~/.config/nix/nix.conf";
          ]
        ["nix run .#ci-check-version-sh-lock"]
        ~cache:[cache ~key:"nix-store" ["/nix/store"]]
    in
    let job_semgrep : tezos_job =
      job
        ~__POS__
        ~name:"oc.semgrep"
        ~image:Images.semgrep_agent
        ~stage
        ~dependencies
        ~rules:(make_rules ~changes:changeset_semgrep_files ())
        [
          "echo \"OCaml code linting. For information on how to reproduce \
           locally, check out scripts/semgrep/README.md\"";
          "sh ./scripts/semgrep/lint-all-ocaml-sources.sh";
        ]
    in
    let job_oc_misc_checks : tezos_job =
      job
        ~__POS__
        ~name:"oc.misc_checks"
        ~image:Images.CI.test_master
        ~stage
        ~dependencies
        ~rules:(make_rules ~changes:changeset_lint_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             ~init_python_venv:true
             [])
        ([
           "./scripts/ci/lint_misc_check.sh";
           "scripts/check_wasm_pvm_regressions.sh check";
           "etherlink/scripts/check_evm_store_migrations.sh check";
           "./scripts/check_rollup_node_sql_migrations.sh check";
           "./src/lib_dal_node/scripts/check_dal_store_migrations.sh check";
         ]
        @
        (* The license check only applies to new files (in the sense
           of [git add]), so can only run in [before_merging]
           pipelines. *)
        match pipeline_type with
        | Before_merging | Merge_train ->
            ["./scripts/ci/lint_check_licenses.sh"]
        | Schedule_extended_test -> [])
    in
    let job_check_jsonnet =
      (* Note: this job's script includes a copy-paste of the script of [grafazos.build]. *)
      job
        ~__POS__
        ~name:"check_jsonnet"
        ~image:Images.jsonnet_master
        ~stage
        ~dependencies
        ~rules:
          (make_rules ~dependent:true ~changes:changeset_jsonnet_fmt_files ())
        ~before_script:
          [
            "cd grafazos/";
            (* For security, we explicitly install v11.1.0
               which corresponds to commit [1ce5aec]. *)
            "jb install \
             github.com/grafana/grafonnet/gen/grafonnet-v11.1.0@1ce5aec";
            "cd ../";
          ]
        [
          "scripts/lint.sh --check-jsonnet-format";
          "scripts/lint.sh --check-jsonnet-lint";
        ]
    in
    let job_check_rust_fmt : tezos_job =
      job
        ~__POS__
        ~name:"check_rust_fmt"
        ~image:Images.rust_toolchain_master
        ~stage
        ~dependencies
        ~rules:(make_rules ~dependent:true ~changes:changeset_rust_fmt_files ())
        ["scripts/check-format-rust.sh"]
    in
    let job_commit_titles : tezos_job =
      let allow_failure : allow_failure_job =
        match pipeline_type with Merge_train -> No | _ -> With_exit_codes [65]
      in
      job
        ~__POS__
        ~name:"commit_titles"
        ~image:Images.CI.prebuild_master
        ~stage
        ~dependencies
        (* ./scripts/ci/check_commit_messages.sh exits with code 65 when a git history contains
           invalid commits titles in situations where that is allowed. *)
        (script_propagate_exit_code "./scripts/ci/check_commit_messages.sh")
        ~allow_failure
    in
    let mr_only_jobs =
      match pipeline_type with
      | Before_merging | Merge_train ->
          [
            (* This job shall only run in pipelines for MRs because it's not
               sensitive to changes in time. *)
            job_nix;
            (* It makes no sense to test commit titles in scheduled
               pipelines (they run on master, where commit titles are
               unmutable) *)
            job_commit_titles;
          ]
      | Schedule_extended_test -> []
    in
    [
      job_semgrep;
      job_oc_misc_checks;
      job_check_jsonnet;
      job_check_rust_fmt;
    ]
    @ mr_only_jobs
  in
  let dependencies_needs_start = dependencies_needs_start pipeline_type in
  let job_build_x86_64_release = job_build_x86_64_release pipeline_type in
  let job_build_x86_64_extra_dev = job_build_x86_64_extra_dev pipeline_type in
  let job_build_x86_64_exp = job_build_x86_64_exp pipeline_type in

  let job_build_arm64_release = job_build_arm64_release pipeline_type in
  let job_build_arm64_extra_dev = job_build_arm64_extra_dev pipeline_type in
  let job_build_arm64_exp = job_build_arm64_exp pipeline_type in

  (* Octez static binaries *)
  let job_static_x86_64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~storage:Ramfs
        (* Even though not many tests depend on static executables, some
           of those that do are limiting factors in the total duration
           of pipelines. So we start this job as early as possible,
           without waiting for sanity_ci. *)
      ~dependencies:dependencies_needs_start
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
  in
  let job_static_arm64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Arm64
      ~storage:Ramfs
      ~dependencies:dependencies_needs_start (* See rationale above *)
      ~rules:(make_rules ~manual:(On_changes changeset_octez) ())
      ()
  in

  (* Build jobs *)
  let build =
    let stage = Stages.build in
    (* TODO: The code is a bit convoluted here because these jobs are
       either in the build or in the manual stage depending on the
       pipeline type. However, we can put them in the build stage on
       [before_merging] pipelines as long as we're careful to put
       [allow_failure: true]. *)
    let bin_packages_jobs =
      match pipeline_type with
      | Schedule_extended_test | Before_merging | Merge_train -> []
    in
    let build_octez_source =
      (* We check compilation of the octez tarball on scheduled
         pipelines because it's not worth testing it for every merge
         request pipeline. The job is manual on regular MR
         pipelines. *)
      job
        ~__POS__
        ~stage
        ~image:Images.CI.build
        ~cpu:Very_high
        ~storage:Ramfs
        ~rules:(make_rules ~manual:Yes ())
        ~variables:[("DUNE_BUILD_JOBS", "-j 12")]
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ~name:"build_octez_source"
        [
          "./scripts/ci/restrict_export_to_octez_source.sh";
          "./scripts/ci/create_octez_tarball.sh octez";
          "mv octez.tar.bz2 ../";
          "cd ../";
          "tar xf octez.tar.bz2";
          "cd octez/";
          "eval $(opam env)";
          "make octez";
        ]
      |> enable_cargo_cache |> enable_sccache
    in
    [
      job_build_arm64_release;
      job_build_arm64_extra_dev;
      job_build_arm64_exp;
      job_static_x86_64_experimental;
      job_static_arm64_experimental;
      job_build_x86_64_release;
      job_build_x86_64_extra_dev;
      job_build_x86_64_exp;
      build_octez_source;
      job_build_layer1_profiling
        ~rules:(make_rules ~changes:changeset_octez ())
        ();
    ]
    @ bin_packages_jobs
  in

  (* Test jobs*)
  let test =
    (* This job triggers the debian child pipeline automatically if any
       files in the changeset is modified. It's the same as
       job_debian_repository_trigger that can be run manually.
        We want both:
       - to trigger the debian repository test automatically when relevant
          files change, if the whole pipeline was triggered;
       - and to be able to trigger the debian repository test manually,
          whether relevant files changed or not, without having to trigger the
          whole pipeline.
         To achieve that we need to duplicate the job, because
         it needs two different sets of dependencies. *)
    let job_debian_repository_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_debian_packages ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Debian_repository.child_pipeline_partial_auto
    in

    (* rpm counterpart of the debian tests *)
    let job_rpm_repository_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_rpm_packages ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Rpm_repository.child_pipeline_partial_auto
    in

    let job_homebrew_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_homebrew ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Homebrew.child_pipeline_full_auto
    in

    (* The set of installation test jobs *)
    let jobs_install_octez : tezos_job list =
      let compile_octez_rules =
        make_rules
          ~changes:(Changeset.make ["docs/introduction/compile-sources.sh"])
          ~manual:Yes
          ()
      in
      let job_install_opam_noble : tezos_job =
        job
          ~__POS__
          ~name:"oc.install_opam_noble"
          ~cpu:Very_high
          ~image:
            (Image.mk_external
               ~image_path:
                 (Images.Base_images.path_prefix ^ "/build-ubuntu-noble:master"))
          ~rules:(make_rules ~manual:Yes ())
          ~allow_failure:Yes
          ~stage:Stages.test
            (* As this job is long, we override the default timeout to
               2 hours. *)
          ~timeout:(Hours 2)
          ~before_script:["apt update"; "apt install -y sudo"]
          ["./docs/introduction/install-opam.sh"]
        |> enable_networked_cargo
      in
      let job_compile_sources_build ~__POS__ ~name ~matrix ?retry () =
        job
          ~__POS__
          ~name
          ~cpu:Very_high
          ~image:
            (Image.mk_external
               ~image_path:(Images.Base_images.path_prefix ^ "/$IMAGE"))
          ~parallel:(Matrix matrix)
          ?retry
          ~dependencies:dependencies_needs_start
          ~rules:compile_octez_rules
          ~stage:Stages.test
            (* This job uses a CARGO_HOME different from
               {!Common.cargo_home}. That CARGO_HOME used is outside the
               CI_PROJECT_DIR, and is thus uncachable. *)
          ~variables:[("CARGO_HOME", "/home/opam/.cargo")]
          [sf "./docs/introduction/compile-sources-setup.sh"]
        |> enable_networked_cargo
      in
      let job_compile_sources ~__POS__ ~name ~matrix ~project ~branch ?retry ()
          =
        job
          ~__POS__
          ~name
          ~cpu:Very_high
          ~storage:Ramfs
          ~image:
            (Image.mk_external
               ~image_path:(Images.Base_images.path_prefix ^ "/$IMAGE"))
          ~parallel:(Matrix matrix)
          ?retry
          ~dependencies:dependencies_needs_start
          ~rules:compile_octez_rules
          ~stage:Stages.test
            (* This job uses a CARGO_HOME different from
               {!Common.cargo_home}. That CARGO_HOME used is outside the
               CI_PROJECT_DIR, and is thus uncachable. *)
          ~variables:[("CARGO_HOME", "/home/opam/.cargo")]
          [sf "./docs/introduction/compile-sources.sh %s %s" project branch]
        |> enable_networked_cargo
      in

      [(* Test installing through opam *) job_install_opam_noble]
      @
      match pipeline_type with
      (* These tests make sure that the compilation instructions
         in master are still valid for the latest-release branch *)
      | Schedule_extended_test ->
          [
            job_compile_sources_build
              ~__POS__
              ~name:"oc.compile_sources_doc_deps"
              ~matrix:[[("IMAGE", ["debian:bookworm"; "ubuntu:noble"])]]
              ();
            job_compile_sources
              ~__POS__
              ~name:"oc.compile_sources_doc"
              ~project:"tezos/tezos"
              ~branch:"latest-release"
              ~matrix:
                [
                  [
                    ( "IMAGE",
                      [
                        "build-debian-bookworm:master";
                        "build-ubuntu-noble:master";
                      ] );
                  ];
                ]
              ();
          ]
      (* Test compiling the [master] branch on Bookworm, to make sure
         that the compilation instructions in this branch are still
         valid.
      *)
      | _ ->
          [
            job_compile_sources
              ~__POS__
              ~name:"oc.compile_sources_doc_master"
              ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
              ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}"
              ~matrix:[[("IMAGE", ["build-debian-bookworm:master"])]]
              ();
          ]
    in
    let jobs_sdk_rust : tezos_job list =
      let job_test_sdk_rust =
        job
          ~__POS__
          ~name:"test_sdk_rust"
          ~image:Images.rust_toolchain
          ~stage:Stages.test
          ~dependencies:dependencies_needs_start
          ~rules:
            (make_rules ~dependent:true ~changes:changeset_test_sdk_rust ())
          ["make -C sdk/rust check"; "make -C sdk/rust test"]
        |> enable_cargo_cache
        |> enable_sccache ~policy:Pull_push
      in
      [job_test_sdk_rust]
    in
    let jobs_sdk_bindings : tezos_job list =
      let job_test_sdk_bindings =
        Sdk_bindings_ci.job_test
          ~dependencies:dependencies_needs_start
          ~rules:
            (job_rule ~allow_failure:Yes ()
            :: make_rules
                 ~dependent:true
                 ~changes:changeset_test_sdk_bindings
                 ())
          ()
      in
      [job_test_sdk_bindings]
    in

    let jobs_packaging =
      match pipeline_type with
      | Before_merging | Merge_train ->
          [
            job_debian_repository_trigger_auto;
            job_rpm_repository_trigger_auto;
            job_homebrew_trigger_auto;
          ]
      | Schedule_extended_test -> []
    in
    jobs_packaging @ jobs_sdk_rust @ jobs_sdk_bindings @ jobs_install_octez
  in

  (* Manual jobs *)
  let manual =
    (* On scheduled pipelines we build and test the full packages test matrix.
       On [Before_merging] pipelines only a subset of the packages are built
       and tested. There is a similar job job_debian_repository_trigger_auto
       in the test stage that is started automatically if any files related to
       packaging is changed. *)
    let job_debian_repository_trigger_partial : tezos_job =
      (* Same as [job_debian_repository_trigger_auto] but manual,
         so that one can trigger it without triggering the whole main pipeline.
         See comment near the definition of [job_debian_repository_trigger_auto]. *)
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~dependencies:(Dependent [])
        ~stage:Stages.manual
        Debian_repository.child_pipeline_partial
    in
    let job_rpm_repository_trigger_partial : tezos_job =
      (* Same as [job_rpm_repository_trigger_auto] but manual,
         so that one can trigger it without triggering the whole main pipeline.
         See comment near the definition of [job_rpm_repository_trigger_auto]. *)
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~dependencies:(Dependent [])
        ~stage:Stages.manual
        Rpm_repository.child_pipeline_partial
    in
    let job_homebrew_repository_trigger : tezos_job =
      (* We leave the possibility to run this pipeline manually, in particular
         to generate the formula on scheduled pipelines *)
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~dependencies:(Dependent [])
        ~stage:Stages.manual
        Homebrew.child_pipeline_full
    in
    let job_base_images_trigger =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~stage:Stages.manual
        ~variables:[("DOCKER_FORCE_BUILD", "true")]
        ~dependencies:(Dependent [])
        Base_images.child_pipeline
    in
    let security_scan_trigger =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:Yes ())
        ~stage:Stages.manual
        ~dependencies:(Dependent [])
        Security_scans.child_pipeline
    in

    match pipeline_type with
    | Before_merging | Merge_train ->
        (* Note: manual jobs in stage [manual] (which is the final
           stage) in [Before_merging] pipelines should be [Dependent]
           by default, and in particular [Dependent []] if they have
           no need for artifacts from other jobs. Making these
           dependent on [job_start] is redundant since they are
           already manual, and what's more, puts the pipeline in a
           confusing "pending state" with a yellow "pause" icon on the
           [manual] stage. *)
        let job_docker_amd64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~arch:Amd64
            ~dependencies:(Dependent [])
            ~rules:(make_rules ~changes:changeset_docker_files ~manual:Yes ())
            Test_manual
        in
        let job_docker_arm64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~arch:Arm64
            ~storage:Ramfs
            ~dependencies:(Dependent [])
            ~rules:(make_rules ~changes:changeset_docker_files ~manual:Yes ())
            Test_manual
        in
        let job_docker_verify_test_amd64 : tezos_job =
          job_docker_authenticated
            ~__POS__
            ~name:"oc.script.docker_verify_image_amd64"
            ~stage:Stages.manual
            ~variables:[("IMAGE_ARCH_PREFIX", "amd64_")]
            ~rules:(make_rules ~manual:Yes ())
            ~dependencies:(Dependent [Job job_docker_amd64_test_manual])
            ["./scripts/ci/docker_verify_signature.sh"]
        in
        let job_docker_verify_test_arm64 : tezos_job =
          job_docker_authenticated
            ~__POS__
            ~name:"oc.script.docker_verify_image_arm64"
            ~stage:Stages.manual
            ~variables:[("IMAGE_ARCH_PREFIX", "arm64_")]
            ~rules:(make_rules ~manual:Yes ())
            ~dependencies:(Dependent [Job job_docker_arm64_test_manual])
            ["./scripts/ci/docker_verify_signature.sh"]
        in
        let jobs =
          [
            job_docker_amd64_test_manual;
            job_docker_arm64_test_manual;
            job_docker_verify_test_arm64;
            job_docker_verify_test_amd64;
          ]
        in
        if pipeline_type = Merge_train then jobs
        else
          [
            job_homebrew_repository_trigger;
            job_rpm_repository_trigger_partial;
            job_debian_repository_trigger_partial;
            job_base_images_trigger;
            security_scan_trigger;
          ]
          @ jobs
    (* No manual jobs on the scheduled pipeline *)
    | Schedule_extended_test -> []
  in
  start_stage @ sanity @ build @ test @ manual
