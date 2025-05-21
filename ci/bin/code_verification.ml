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
open Common

(** Variants of the code verification pipeline.

    Encodes the conditional [before_merging] pipeline, the [merge_train]
    and its unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline =
  | Before_merging
  | Schedule_extended_test
  | Merge_train

(** Jobs testing the installability of the Octez opam packages.

    The opam packages are split into two {i groups}: those installing
    executables and the other jobs. The former group is tested more
    often (being typically the leaves the dependency graph of opam
    packages).

    Due to the large number of packages, the opam testing jobs are
    launched in a series of {i batches}. Jobs are grouped into batches as
    per their expected duration. The expected duration is based on the
    depth of it's dependency tree. Thus, jobs testing packages with a
    deeper dependency tree are launched in the first batch (the
    [batch_index] of all those jobs is 1).

    The set of opam packages, their group, and the index of its batch,
    is defined by manifest and written to the TSV file
    [script-inputs/ci-opam-package-tests]. *)
module Opam = struct
  (** Opam package group.

      Opam jobs are split into two groups:
      - [Executable]: those that install an executable, e.g. [octez-client].
      - [All]: remaining packages, e.g. [octez-libs]. *)
  type opam_package_group = Executable | All

  (** Opam package meta-data.

      An opam testing job is characterized by the package name, its
      group and the index of the batch in which it will execute. *)
  type opam_package = {
    name : string;
    group : opam_package_group;
    batch_index : int;
  }

  (** Opam packages and their meta data.

      This is read from the file [script-inputs/ci-opam-package-tests],
      which is written by manifest. *)
  let opam_packages =
    let ci_opam_package_tests = "script-inputs/ci-opam-package-tests" in
    Fun.flip List.filter_map (read_lines_from_file ci_opam_package_tests)
    @@ fun line ->
    let fail () =
      failwith
        (sf "failed to parse %S: invalid line: %S" ci_opam_package_tests line)
    in
    if line = "" then None
    else
      match String.split_on_char '\t' line with
      | [name; group; batch_index] ->
          let batch_index =
            match int_of_string_opt batch_index with
            | Some i -> i
            | None -> fail ()
          in
          let group =
            match group with
            | "exec" -> Executable
            | "all" -> All
            | _ -> fail ()
          in
          Some {name; group; batch_index}
      | _ -> fail ()

  (** Opam job rules.

      These rules define when the opam job runs, and implements the
      delay used to run opam jobs in batches. *)
  let opam_rules pipeline_type ~only_final_pipeline ~batch_index () =
    let when_ = Delayed (Minutes batch_index) in
    match pipeline_type with
    | Schedule_extended_test -> [job_rule ~when_ ()]
    | Before_merging | Merge_train ->
        [
          job_rule ~if_:(Rules.has_mr_label "ci--opam") ~when_ ();
          job_rule
            ?if_:
              (if only_final_pipeline then Some Rules.is_final_pipeline
               else None)
            ~changes:(Changeset.encode changeset_opam_jobs)
            ~when_
            ();
        ]

  (** Constructs the opam package job for a given group and batch.

      Separate packages with the same group and batch are tested in
      separate jobs, which is implemented through parallel-matrix
      jobs. *)
  let job_opam_packages ?dependencies pipeline_type group batch_index packages :
      tezos_job =
    job
      ?dependencies
      ~__POS__
      ~name:
        ("opam:"
        ^ (match group with All -> "all" | Executable -> "exec")
        ^ "_" ^ string_of_int batch_index)
      ~image:Images.CI.prebuild
      ~stage:Stages.packaging
        (* FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/663
           FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/664
           At the time of writing, the opam tests were quite flaky.
           Therefore, a retry was added. This should be removed once the
           underlying tests have been fixed. *)
      ~retry:{max = 2; when_ = []}
      ~timeout:(Minutes 90)
      ~rules:
        (opam_rules
           pipeline_type
           ~only_final_pipeline:(group = All)
           ~batch_index
           ())
      ~variables:
        (* See [variables] in [main.ml] for details on [RUNTEZTALIAS] *)
        [("RUNTEZTALIAS", "true")]
      ~parallel:(Matrix [[("package", packages)]])
      ~before_script:
        (before_script ~eval_opam:true ["mkdir -p $CI_PROJECT_DIR/opam_logs"])
      [
        "opam remote add dev-repo ./_opam-repo-for-release";
        "opam install --yes ${package}.dev";
        "opam reinstall --yes --with-test ${package}.dev";
      ]
      (* Stores logs in opam_logs for artifacts and outputs an excerpt on
         failure. [after_script] runs in a separate shell and so requires
         a second opam environment initialization. *)
      ~after_script:
        [
          "eval $(opam env)";
          "OPAM_LOGS=opam_logs ./scripts/ci/opam_handle_output.sh";
        ]
      ~artifacts:
        (artifacts ~expire_in:(Duration (Weeks 1)) ~when_:Always ["opam_logs/"])
    |> (* We store caches in [_build] for two reasons: (1) the [_build]
          folder is excluded from opam's rsync. (2) gitlab ci cache
          requires that cached files are in a sub-folder of the checkout. *)
    enable_sccache
      ~key:"opam-sccache"
      ~error_log:"$CI_PROJECT_DIR/opam_logs/sccache.log"
      ~idle_timeout:"0"
      ~path:"$CI_PROJECT_DIR/_build/_sccache"
    |> enable_cargo_cache

  let jobs_opam_packages ?dependencies pipeline_type : tezos_job list =
    let package_by_group_index = Hashtbl.create 5 in
    List.iter
      (fun pkg ->
        let key = (pkg.group, pkg.batch_index) in
        let packages =
          Option.value ~default:[]
          @@ Hashtbl.find_opt package_by_group_index key
        in
        Hashtbl.replace package_by_group_index key (pkg.name :: packages))
      opam_packages ;
    (* The [opam:prepare] job creates a local opam-repository from
       which installability is tested. This repository is transferred
       as an artifact to the opam package jobs.

       Note: this preliminary step is very quick and could be folded
       into the opam package jobs. *)
    let job_prepare =
      job
        ~__POS__
        ~name:"opam:prepare"
        ~image:Images.CI.prebuild
        ~stage:Stages.packaging
        ?dependencies
        ~before_script:(before_script ~eval_opam:true [])
        ~artifacts:(artifacts ["_opam-repo-for-release/"])
        ~rules:
          (opam_rules
             pipeline_type
             ~only_final_pipeline:false
             ~batch_index:1
             ())
        [
          "git init _opam-repo-for-release";
          "./scripts/opam-prepare-repo.sh dev ./ ./_opam-repo-for-release";
          "git -C _opam-repo-for-release add packages";
          "git -C _opam-repo-for-release commit -m \"tezos packages\"";
        ]
    in
    job_prepare
    :: Hashtbl.fold
         (fun (group, index) packages jobs ->
           let dependencies = Dependent [Artifacts job_prepare] in
           job_opam_packages ~dependencies pipeline_type group index packages
           :: jobs)
         package_by_group_index
         []
end

(** Configuration of manual jobs for [make_rules] *)
type manual =
  | No  (** Do not add rule for manual start. *)
  | Yes  (** Add rule for manual start. *)
  | On_changes of Changeset.t  (** Add manual start on certain [changes:] *)

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
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
  let make_rules ?label ?changes ?(manual = No) ?(dependent = false)
      ?(final_pipeline_disable = false) () =
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
              [
                job_rule ~changes:(Changeset.encode changes) ~when_:On_success ();
              ])
        (* It can be relevant to start some jobs manually. *)
        @
        match manual with
        | No -> []
        | Yes -> [job_rule ~when_:Manual ()]
        | On_changes changes ->
            [job_rule ~when_:Manual ~changes:(Changeset.encode changes) ()])
  in
  (* Collect coverage trace producing jobs *)
  let jobs_with_coverage_output = ref [] in
  (* Add variables for bisect_ppx output and store the traces as an
     artifact.

     This function should be applied to test jobs that produce coverage. *)
  let enable_coverage_output_artifact ?(expire_in = Duration (Days 1)) tezos_job
      : tezos_job =
    jobs_with_coverage_output := tezos_job :: !jobs_with_coverage_output ;
    tezos_job |> enable_coverage_location
    |> append_script ["./scripts/ci/merge_coverage.sh"]
    |> add_artifacts
         ~expire_in
         ~name:"coverage-files-$CI_JOB_ID"
         ~when_:On_success
         (* Store merged .coverage files or [.corrupt.json] files. *)
         ["$BISECT_FILE/$CI_JOB_NAME_SLUG.*"]
  in

  (* Stages *)
  let start_stage, make_dependencies =
    match pipeline_type with
    | Schedule_extended_test ->
        let make_dependencies ~before_merging:_ ~schedule_extended_test =
          schedule_extended_test ()
        in
        ([job_datadog_pipeline_trace], make_dependencies)
    | Before_merging | Merge_train ->
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
              "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline \
               --tags pipeline_type:$PIPELINE_TYPE --tags \
               mr_number:$CI_MERGE_REQUEST_IID";
            ]
        in
        let make_dependencies ~before_merging ~schedule_extended_test:_ =
          before_merging job_start
        in
        ([job_start], make_dependencies)
  in
  (* Sanity jobs *)
  let sanity =
    let stage = Stages.sanity in
    let dependencies =
      make_dependencies
        ~before_merging:(fun job_start -> Dependent [Job job_start])
        ~schedule_extended_test:(fun () -> Staged [])
    in
    let job_sanity_ci : tezos_job =
      (* Quick, CI-related sanity checks.

         Verifies that manifest and CIAO are up to date. *)
      job
        ~__POS__
        ~name:"sanity_ci"
        ~image:Images.CI.build
        ~stage
        ~dependencies
        ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
        [
          "make --silent -C manifest check";
          (* Check that .gitlab-ci.yml is up to date. *)
          "make --silent -C ci check";
        ]
    in
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
    let job_docker_hadolint =
      job
        ~rules:(make_rules ~changes:changeset_docker_files ())
        ~__POS__
        ~name:"docker:hadolint"
        ~dependencies
        ~image:Images.hadolint
        ~stage
        ["hadolint build.Dockerfile"; "hadolint Dockerfile"]
    in
    let job_oc_ocaml_fmt : tezos_job =
      job
        ~__POS__
        ~name:"oc.ocaml_fmt"
        ~image:Images.CI.build
        ~stage
        ~dependencies
        ~rules:(make_rules ~changes:changeset_ocaml_fmt_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        [
          (* Check .ocamlformat files. *)
          "scripts/lint.sh --check-ocamlformat";
          (* Check actual formatting. *)
          "dune build --profile=dev @fmt";
        ]
      |> enable_dune_cache
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
        ~image:Images.CI.test
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
      job
        ~__POS__
        ~name:"check_jsonnet"
        ~image:Images.jsonnet
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
        ~image:Images.rust_toolchain
        ~stage
        ~dependencies
        ~rules:(make_rules ~dependent:true ~changes:changeset_rust_fmt_files ())
        ["scripts/check-format-rust.sh"]
    in
    let job_check_rst =
      job
        ~__POS__
        ~name:"documentation:rst-check"
        ~image:Images.CI.test
        ~stage
        ~rules:(make_rules ~changes:changeset_octez_docs_rst ())
        ~before_script:(before_script ~init_python_venv:true [])
        ["make --silent -C docs sphinx-check"]
    in
    let job_commit_titles : tezos_job =
      let allow_failure : allow_failure_job =
        match pipeline_type with Merge_train -> No | _ -> With_exit_codes [65]
      in
      job
        ~__POS__
        ~name:"commit_titles"
        ~image:Images.CI.prebuild
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
      job_sanity_ci;
      job_docker_hadolint;
      job_oc_ocaml_fmt;
      job_semgrep;
      job_oc_misc_checks;
      job_check_jsonnet;
      job_check_rust_fmt;
      job_check_rst;
    ]
    @ mr_only_jobs
  in
  (* Short-cut for jobs that have no dependencies except [job_start] on
     [Before_merging] pipelines. These jobs must also depend on all
     jobs in the stage [sanity], such that they do not run if this
     stage does not succeed. Since some sanity jobs are conditional,
     we make these dependencies optional. *)
  let dependencies_needs_start =
    make_dependencies
      ~before_merging:(fun job_start ->
        Dependent ([Job job_start] @ List.map (fun job -> Optional job) sanity))
      ~schedule_extended_test:(fun () -> Staged [])
  in
  (* Job [documentation:manuals] requires the build jobs, because it needs
     to run Octez executables to generate the man pages.
     So the build jobs need to be included if the documentation changes. *)
  let changeset_octez_or_doc =
    Changeset.(changeset_octez @ changeset_octez_docs)
  in
  let changeset_octez_or_kernels_or_doc =
    Changeset.(changeset_octez_or_kernels @ changeset_octez_docs)
  in
  (* The build_x86_64 jobs are split in two to keep the artifact size
     under the 1GB hard limit set by GitLab. *)
  (* [job_build_x86_64_release] builds the released executables. *)
  let job_build_x86_64_release =
    job_build_dynamic_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
      ~dependencies:dependencies_needs_start
      ~release:true
      ~rules:(make_rules ~changes:changeset_octez_or_doc ())
      ()
  in
  (* 'oc.build_x86_64-exp-dev-extra' builds the developer and experimental
     executables, as well as the tezt test suite used by the subsequent
     'tezt' jobs and TPS evaluation tool. *)
  let build_cache_key =
    "dune-build-cache-" ^ Gitlab_ci.Predefined_vars.(show ci_pipeline_id)
  in
  let job_build_x86_64_exp_dev_extra =
    job_build_dynamic_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
      ~dependencies:dependencies_needs_start
      ~release:false
      ~rules:(make_rules ~changes:changeset_octez_or_doc ())
      ()
    |> enable_dune_cache ~key:build_cache_key ~policy:Push
  in

  let build_arm_rules = make_rules ~label:"ci--arm64" ~manual:Yes () in
  let job_build_arm64_release : Tezos_ci.tezos_job =
    job_build_arm64_release ~rules:build_arm_rules ()
  in
  let job_build_arm64_exp_dev_extra : Tezos_ci.tezos_job =
    job_build_arm64_exp_dev_extra ~rules:build_arm_rules ()
  in
  let job_build_kernels =
    job_build_kernels
      ~rules:
        (make_rules
           ~changes:changeset_octez_or_kernels_or_doc
           ~dependent:true
           ())
      ()
  in
  let job_build_dsn_node =
    job_build_dsn_node
      ~rules:
        (make_rules
           ~changes:changeset_octez_or_kernels_or_doc
           ~dependent:true
           ())
      ()
  in
  let job_tezt_fetch_records =
    Tezt.job_tezt_fetch_records
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
  in
  let job_select_tezts =
    (* Scheduled pipelines execute all tezt tests: no need for [job_select_tezts]. *)
    match pipeline_type with
    | Before_merging | Merge_train ->
        Some
          (Tezt.job_select_tezts
             ~rules:(make_rules ~changes:changeset_octez ())
             ())
    | Schedule_extended_test -> None
  in
  (* Octez static binaries *)
  let job_static_x86_64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
      ~cpu:Very_high
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
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
      ~dependencies:dependencies_needs_start (* See rationale above *)
      ~rules:(make_rules ~manual:(On_changes changeset_octez) ())
      ()
  in

  (* EVM static binaires *)
  let job_evm_static_x86_64_experimental =
    job
      ~__POS__
      ~arch:Amd64
      ~name:("etherlink.build:static-" ^ arch_to_string Amd64)
      ~image:Images.CI.build
      ~stage:Stages.build
      ~rules:(make_rules ~manual:(On_changes changeset_etherlink) ())
      ~artifacts:
        (artifacts
           ~name:"evm-binaries"
           ~when_:On_success
           ["octez-evm-*"; "etherlink-*"])
      ~cpu:Very_high
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
      ~before_script:
        [
          "./scripts/ci/take_ownership.sh";
          ". ./scripts/version.sh";
          "eval $(opam env)";
        ]
      ["make evm-node-static"]
    |> enable_cargo_cache
    |> enable_sccache ~cache_size:"2G"
    |> enable_cargo_target_caches
  in

  let job_evm_static_arm64_experimental =
    job
      ~__POS__
      ~arch:Arm64
      ~name:("etherlink.build:static-" ^ arch_to_string Arm64)
      ~image:Images.CI.build
      ~stage:Stages.build
      ~rules:(make_rules ~manual:(On_changes changeset_etherlink) ())
      ~artifacts:
        (artifacts
           ~name:"evm-binaries"
           ~when_:On_success
           ["octez-evm-*"; "etherlink-*"])
      ~before_script:
        [
          "./scripts/ci/take_ownership.sh";
          ". ./scripts/version.sh";
          "eval $(opam env)";
        ]
      ["make evm-node-static"]
    |> enable_cargo_cache
    |> enable_sccache ~cache_size:"2G"
    |> enable_cargo_target_caches
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
    let wasm_runtime_check : tezos_job =
      job
        ~__POS__
        ~name:"wasm-runtime-check"
        ~image:Images.CI.build
        ~stage
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_wasm_runtime_check_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ["etherlink/lib_wasm_runtime/lint.sh"]
      |> enable_cargo_cache |> enable_sccache
    in
    let job_ocaml_check : tezos_job =
      job
        ~__POS__
        ~name:"ocaml-check"
        ~cpu:Very_high
        ~image:Images.CI.build
        ~stage
        ~retry:
          {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_ocaml_check_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        (* Stops on first error for easier detection of problems in
           the log and to reduce time to merge of other MRs further
           down the merge train. *)
        ["dune build @check --stop-on-first-error"]
      |> enable_cargo_cache |> enable_sccache |> enable_dune_cache
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
        ~rules:(make_rules ~manual:Yes ())
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
    let job_build_grafazos =
      match pipeline_type with
      | Merge_train | Before_merging ->
          Grafazos_ci.Common.job_build_grafazos
            ~rules:
              [
                job_rule
                  ~when_:Always
                  ~changes:
                    (Changeset.encode Grafazos_ci.Common.changeset_grafazos)
                  ();
                job_rule ~when_:Manual ();
              ]
            ()
      | Schedule_extended_test ->
          Grafazos_ci.Common.job_build_grafazos
            ~rules:[job_rule ~when_:Always ()]
            ()
    in
    let job_build_teztale ~arch =
      Teztale.Common.job_build
        ~arch
        ~rules:(make_rules ~manual:Yes ~changes:Teztale.Common.changeset ())
        ()
    in
    [
      job_build_arm64_release;
      job_build_arm64_exp_dev_extra;
      job_static_x86_64_experimental;
      job_static_arm64_experimental;
      job_build_x86_64_release;
      job_build_x86_64_exp_dev_extra;
      wasm_runtime_check;
      job_ocaml_check;
      job_build_kernels;
      job_build_dsn_node;
      job_tezt_fetch_records;
      build_octez_source;
      job_build_grafazos;
      job_build_teztale ~arch:Amd64;
      job_build_teztale ~arch:Arm64;
      job_evm_static_x86_64_experimental;
      job_evm_static_arm64_experimental;
      job_build_layer1_profiling ();
    ]
    @ Option.to_list job_select_tezts
    @ bin_packages_jobs
  in

  (* Packaging jobs *)
  let packaging =
    match pipeline_type with
    | Merge_train | Before_merging -> []
    | Schedule_extended_test ->
        Opam.jobs_opam_packages
          ~dependencies:dependencies_needs_start
          pipeline_type
  in
  (* Dependencies for jobs that should run immediately after jobs
     [job_build_x86_64] in [Before_merging] if they are present
     (otherwise, they run immediately after [job_start]). In
     [Scheduled_extended_test] we are not in a hurry and we let them
     be [Staged []]. *)
  let order_after_build =
    make_dependencies
      ~before_merging:(fun job_start ->
        Dependent
          (Job job_start
          :: [
               Optional job_build_x86_64_release;
               Optional job_build_x86_64_exp_dev_extra;
             ]))
      ~schedule_extended_test:(fun () -> Staged [])
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
    let job_debian_repository_trigger_full : tezos_job =
      trigger_job
        ~__POS__
        ~dependencies:(Dependent [])
        ~stage:Stages.test
        Debian_repository.child_pipeline_full
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
    let job_rpm_repository_trigger_full : tezos_job =
      trigger_job
        ~__POS__
        ~dependencies:(Dependent [])
        ~stage:Stages.test
        Rpm_repository.child_pipeline_full
    in

    let job_homebrew_trigger_auto =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_homebrew ())
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        Homebrew.child_pipeline_full_auto
    in
    let job_homebrew_trigger_full =
      trigger_job
        ~__POS__
        ~rules:(make_rules ~manual:No ~changes:changeset_homebrew ())
        ~stage:Stages.test
        ~dependencies:(Dependent [])
        Homebrew.child_pipeline_full
    in
    (* check that ksy files are still up-to-date with octez *)
    let job_kaitai_checks : tezos_job =
      job
        ~__POS__
        ~name:"kaitai_checks"
        ~image:Images.CI.build
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_kaitai_e2e_files ())
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        [
          "make -C ${CI_PROJECT_DIR} check-kaitai-struct-files || (echo 'Octez \
           encodings and Kaitai files seem to be out of sync. You might need \
           to run `make check-kaitai-struct-files` and commit the resulting \
           diff.' ; false)";
        ]
        ~artifacts:
          (artifacts
             ~expire_in:(Duration (Hours 1))
             ~when_:On_success
             ["_build/default/client-libs/bin_codec_kaitai/codec.exe"])
      |> enable_cargo_cache |> enable_sccache
    in
    let job_kaitai_e2e_checks =
      job
        ~__POS__
        ~name:"kaitai_e2e_checks"
        ~image:Images.client_libs_dependencies
        ~stage:Stages.test
        ~dependencies:(Dependent [Artifacts job_kaitai_checks])
        ~rules:
          (make_rules ~changes:changeset_kaitai_e2e_files ~dependent:true ())
        ~before_script:
          (before_script
             ~source_version:true
               (* TODO: https://gitlab.com/tezos/tezos/-/issues/5026
                  As observed for the `unit:js_components` running `npm i`
                  everytime we run a job is inefficient.

                  The benefit of this approach is that we specify node version
                  and npm dependencies (package.json) in one place, and that the local
                  environment is then the same as CI environment. *)
             ~install_js_deps:true
             [])
        [
          "./client-libs/kaitai-struct-files/scripts/kaitai_e2e.sh \
           client-libs/kaitai-struct-files/files 2>/dev/null";
        ]
    in
    let job_oc_check_lift_limits_patch =
      job
        ~__POS__
        ~name:"oc.check_lift_limits_patch"
        ~image:Images.CI.build
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_lift_limits_patch ())
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        [
          (* Check that the patch only modifies the
             src/proto_alpha/lib_protocol. If not, the rules above have to be
             updated. *)
          "[ $(git apply --numstat src/bin_tps_evaluation/lift_limits.patch | \
           cut -f3) = \"src/proto_alpha/lib_protocol/main.ml\" ]";
          "git apply src/bin_tps_evaluation/lift_limits.patch";
          "dune build @src/proto_alpha/lib_protocol/check";
        ]
      |> enable_cargo_cache |> enable_sccache
    in
    let job_oc_python_check : tezos_job =
      job
        ~__POS__
        ~name:"oc.python_check"
        ~image:Images.CI.test
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_python_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~init_python_venv:true
             [])
        ["./scripts/ci/lint_misc_python_check.sh"]
    in
    let jobs_unit : tezos_job list =
      let build_dependencies = function
        | Amd64 ->
            Dependent
              [Job job_build_x86_64_release; Job job_build_x86_64_exp_dev_extra]
        | Arm64 ->
            Dependent
              [Job job_build_arm64_release; Job job_build_arm64_exp_dev_extra]
      in
      let rules =
        (* TODO: Note that all jobs defined here are
           [dependent] in the sense that they all have
           [dependencies:]. However, AFAICT, these dependencies are all
           dummy dependencies for ordering -- not for artifacts. This
           means that these unit tests can very well run even if their
           dependencies failed. Moreover, the ordering serves no purpose
           on scheduled pipelines, so we might even remove them for that
           [pipeline_type]. *)
        make_rules ~changes:changeset_octez ~dependent:true ()
      in
      let job_unit_test ~__POS__ ?(image = Images.CI.build) ?timeout
          ?parallel_vector ?(rules = rules) ~arch ?(cpu = Normal) ~name
          ~make_targets () : tezos_job =
        let arch_string = arch_to_string arch in
        let script = ["make $MAKE_TARGETS"] in
        let dependencies = build_dependencies arch in
        let variables =
          [
            ("ARCH", arch_string);
            ("MAKE_TARGETS", String.concat " " make_targets);
          ]
        in

        let variables, parallel =
          (* When parallel_vector is set to non-zero (translating to the
             [parallel_vector:] clause), set the variable
             [DISTRIBUTE_TESTS_TO_PARALLELS] to [true], so that
             [scripts/test_wrapper.sh] partitions the set of @runtest
             targets to build. *)
          match parallel_vector with
          | Some n ->
              ( variables @ [("DISTRIBUTE_TESTS_TO_PARALLELS", "true")],
                Some (Vector n) )
          | None -> (variables, None)
        in
        let job =
          job
            ?timeout
            ?parallel
            ~__POS__
            ~retry:Gitlab_ci.Types.{max = 2; when_ = []}
            ~name
            ~stage:Stages.test
            ~image
            ~arch
            ~cpu
            ~dependencies
            ~rules
            ~variables
            ~artifacts:
              (artifacts
                 ~name:"$CI_JOB_NAME-$CI_COMMIT_SHA-${ARCH}"
                 ["test_results"]
                 ~reports:(reports ~junit:"test_results/*.xml" ())
                 ~expire_in:(Duration (Days 1))
                 ~when_:Always)
            ~before_script:
              (before_script ~source_version:true ~eval_opam:true [])
            script
          |> enable_cargo_cache |> enable_sccache
        in
        if arch = Amd64 then
          enable_dune_cache ~key:build_cache_key ~policy:Pull job
        else job
      in
      let oc_unit_non_proto_x86_64 =
        job_unit_test
          ~__POS__
          ~name:"oc.unit:non-proto-x86_64"
          ~arch:Amd64 (* The [lib_benchmark] unit tests require Python *)
          ~image:Images.CI.test
          ~make_targets:["test-nonproto-unit"]
          ()
        |> enable_coverage_instrumentation |> enable_coverage_output_artifact
      in
      let oc_unit_etherlink_x86_64 =
        job_unit_test
          ~__POS__
          ~name:"oc.unit:etherlink-x86_64"
          ~arch:Amd64
          ~rules:(make_rules ~changes:changeset_etherlink ~dependent:true ())
          ~make_targets:["test-etherlink-unit"]
          ()
        |> enable_coverage_instrumentation |> enable_coverage_output_artifact
      in
      let oc_unit_other_x86_64 =
        (* Runs unit tests for contrib. *)
        job_unit_test
          ~__POS__
          ~name:"oc.unit:other-x86_64"
          ~arch:Amd64
          ~cpu:High
          ~make_targets:["test-other-unit"]
          ()
        |> enable_coverage_instrumentation |> enable_coverage_output_artifact
      in
      let oc_unit_proto_x86_64 =
        (* Runs unit tests for protocol. *)
        job_unit_test
          ~__POS__
          ~name:"oc.unit:proto-x86_64"
          ~arch:Amd64
          ~cpu:Very_high
          ~make_targets:["test-proto-unit"]
          ()
        |> enable_coverage_instrumentation |> enable_coverage_output_artifact
      in
      let oc_unit_non_proto_arm64 =
        (* No coverage for arm64 jobs -- the code they test is a
           subset of that tested by x86_64 unit tests. *)
        job_unit_test
          ~__POS__
          ~name:"oc.unit:non-proto-arm64"
          ~parallel_vector:2
          ~arch:Arm64 (* The [lib_benchmark] unit tests require Python *)
          ~image:Images.CI.test
          ~make_targets:["test-nonproto-unit"; "test-webassembly"]
          ()
      in
      let oc_unit_webassembly_x86_64 =
        job
          ~__POS__
          ~name:"oc.unit:webassembly-x86_64"
          ~arch:Amd64 (* The wasm tests are written in Python *)
          ~image:Images.CI.test
          ~stage:Stages.test
          ~dependencies:(build_dependencies Amd64)
          ~rules
          ~before_script:(before_script ~source_version:true ~eval_opam:true [])
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/4663
               This test takes around 2 to 4min to complete, but it sometimes
               hangs. We use a timeout to retry the test in this case. The
               underlying issue should be fixed eventually, turning this timeout
               unnecessary. *)
          ~timeout:(Minutes 20)
          ["make test-webassembly"]
      in
      let oc_unit_protocol_compiles =
        job
          ~__POS__
          ~name:"oc.unit:protocol_compiles"
          ~arch:Amd64
          ~cpu:Very_high
          ~retry:
            {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
          ~image:Images.CI.build
          ~stage:Stages.test
          ~dependencies:(build_dependencies Amd64)
          ~rules
          ~before_script:(before_script ~source_version:true ~eval_opam:true [])
          ["dune build @runtest_compile_protocol"]
        |> enable_cargo_cache |> enable_sccache
      in
      (* "de" stands for data-encoding, since data-encoding is considered
         to be a separate product. *)
      let de_unit arch =
        job
          ~__POS__
          ~name:("de.unit:" ^ arch_to_string arch)
          ~arch
          ~image:Images.CI.test
          ~stage:Stages.test
          ~rules:
            (make_rules
               ~changes:
                 (Changeset.union
                    changeset_base
                    (Changeset.make ["data-encoding/**"]))
               ())
          ~dependencies:(build_dependencies arch)
          ~before_script:(before_script ~eval_opam:true [])
          ["dune runtest data-encoding"]
      in
      let resto_unit arch =
        job
          ~__POS__
          ~name:("resto.unit:" ^ arch_to_string arch)
          ~arch
          ~image:Images.CI.test
          ~stage:Stages.test
          ~rules:
            (make_rules
               ~changes:
                 (Changeset.union changeset_base (Changeset.make ["resto/**"]))
               ())
          ~dependencies:(build_dependencies arch)
          ~before_script:(before_script ~eval_opam:true [])
          ["dune runtest resto"]
      in
      [
        oc_unit_non_proto_x86_64;
        oc_unit_etherlink_x86_64;
        oc_unit_other_x86_64;
        oc_unit_proto_x86_64;
        oc_unit_non_proto_arm64;
        oc_unit_webassembly_x86_64;
        oc_unit_protocol_compiles;
        de_unit Amd64;
        de_unit Arm64;
        resto_unit Amd64;
        resto_unit Arm64;
      ]
    in
    let job_oc_integration_compiler_rejections : tezos_job =
      job
        ~__POS__
        ~name:"oc.integration:compiler-rejections"
        ~stage:Stages.test
        ~image:Images.CI.build
        ~rules:(make_rules ~changes:changeset_octez ())
        ~dependencies:
          (Dependent
             [Job job_build_x86_64_release; Job job_build_x86_64_exp_dev_extra])
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        ["dune build @runtest_rejections"]
      |> enable_cargo_cache |> enable_sccache
    in
    let job_oc_script_test_gen_genesis : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:test-gen-genesis"
        ~stage:Stages.test
        ~image:Images.CI.build
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_octez ())
        ~before_script:
          (before_script ~eval_opam:true ["cd scripts/gen-genesis"])
        ["dune build gen_genesis.exe"]
    in
    let job_oc_script_snapshot_alpha_and_link : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:snapshot_alpha_and_link"
        ~stage:Stages.test
        ~image:Images.CI.build
        ~cpu:Very_high
        ~retry:
          {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
        ~dependencies:order_after_build
          (* Since the above dependencies are only for ordering, we do not set [dependent] *)
        ~rules:(make_rules ~changes:changeset_script_snapshot_alpha_and_link ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ["./scripts/ci/script:snapshot_alpha_and_link.sh"]
      |> enable_cargo_cache |> enable_sccache
      |> enable_dune_cache ~key:build_cache_key ~policy:Pull
    in
    let job_oc_script_test_release_versions : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:test_octez_release_versions"
        ~stage:Stages.test
        ~image:Images.CI.build
        ~dependencies:
          (Dependent
             [Job job_build_x86_64_release; Job job_build_x86_64_exp_dev_extra])
          (* Since the above dependencies are only for ordering, we do not set [dependent] *)
        ~rules:(make_rules ~changes:changeset_octez ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ["./scripts/test_octez_release_version.sh"]
    in
    let job_oc_script_b58_prefix =
      job
        ~__POS__
        ~name:"oc.script:b58_prefix"
        ~stage:Stages.test
          (* Requires Python. Can be changed to a python image, but using
             the build docker image to keep in sync with the python
             version used for the tests *)
        ~image:Images.CI.test
        ~rules:(make_rules ~changes:changeset_script_b58_prefix ())
        ~dependencies:dependencies_needs_start
        ~before_script:
          (before_script ~source_version:true ~init_python_venv:true [])
        [
          "poetry run pylint scripts/b58_prefix/b58_prefix.py \
           --disable=missing-docstring --disable=invalid-name";
          "poetry run pytest scripts/b58_prefix/test_b58_prefix.py";
        ]
    in
    let job_oc_test_liquidity_baking_scripts : tezos_job =
      job
        ~__POS__
        ~name:"oc.test-liquidity-baking-scripts"
        ~stage:Stages.test
        ~image:Images.CI.build
        ~dependencies:
          (Dependent
             [
               Artifacts job_build_x86_64_release;
               Artifacts job_build_x86_64_exp_dev_extra;
             ])
        ~rules:
          (make_rules
             ~dependent:true
             ~changes:changeset_test_liquidity_baking_scripts
             ())
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        ["./scripts/ci/test_liquidity_baking_scripts.sh"]
    in
    let job_test_release_versions =
      job
        ~__POS__
        ~image:Images.CI.prebuild
        ~stage:Stages.test
        ~name:"oc:scripts:release_script_values"
        ~dependencies:dependencies_needs_start
        ["scripts/ci/test_release_values.sh"]
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
          ~image:Images.opam_ubuntu_noble
          ~dependencies:dependencies_needs_start
          ~rules:(make_rules ~manual:Yes ())
          ~allow_failure:Yes
          ~stage:Stages.test
            (* The default behavior of opam is to use `nproc` to determine its level of
               parallelism. This returns the number of CPU of the "host" CI runner
               instead of the number of cores a single CI job can reasonably use. *)
          ~variables:[("OPAMJOBS", "4")]
            (* As this job is long, we override the default timeout to
               2 hours. *)
          ~timeout:(Hours 2)
          ["./docs/introduction/install-opam.sh"]
      in
      let job_compile_sources ~__POS__ ~name ~image ~project ~branch ?cpu ?retry
          () =
        job
          ~__POS__
          ~name
          ~image
          ?cpu
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
            job_compile_sources
              ~__POS__
              ~name:"oc.compile_sources_doc_bookworm"
              ~image:Images.opam_debian_bookworm
              ~project:"tezos/tezos"
              ~branch:"latest-release"
              ~cpu:Very_high
              ~retry:
                {
                  max = 2;
                  when_ = [Stuck_or_timeout_failure; Runner_system_failure];
                }
              ();
            job_compile_sources
              ~__POS__
              ~name:"oc.compile_sources_doc_oracular"
              ~image:Images.opam_ubuntu_oracular
              ~project:"tezos/tezos"
              ~branch:"latest-release"
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
              ~name:"oc.compile_sources_doc_bookworm"
              ~image:Images.opam_debian_bookworm
              ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
              ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}"
              ~cpu:Very_high
              ~retry:
                {
                  max = 2;
                  when_ = [Stuck_or_timeout_failure; Runner_system_failure];
                }
              ();
          ]
    in
    (* Tezt jobs.

       The Tezt jobs are split into a set of special-purpose jobs running the
       tests of the corresponding tag:
       - [tezt-time_sensitive]: runs the jobs with tag [time_sensitive];
       - [tezt-slow]: runs the jobs with tag [slow];
       - [tezt-flaky]: runs the jobs with tag [flaky] and
          none of the tags above;

       and a job [tezt] that runs all remaining tests (excepting those
       that are tagged [ci_disabled], that are disabled in the CI.)

       There is an implicit rule that the Tezt tags [time_sensitive],
       [slow] and [cloud] are mutually exclusive.
       The [flaky] tag is not exclusive to these tags.
       If e.g. a test has both tags [slow] and [flaky], it will run in
       [tezt-slow], to prevent flaky tests to run in the [tezt-flaky]
       job if they also have another special tag. Tests tagged [cloud] are
       meant to be used with Tezt cloud (see [tezt/lib_cloud/README.md]) and
       do not run in the CI.

       For more information on tags, see [src/lib_test/tag.mli].

       Important: the [Custom_test_extended_pipeline.jobs] function
       declares a set of jobs that must match the ones defined
       below. Please update the jobs accordingly.
    *)
    let jobs_tezt =
      let dependencies =
        Dependent
          [
            Artifacts job_build_x86_64_release;
            Artifacts job_build_x86_64_exp_dev_extra;
            Artifacts job_build_kernels;
            Artifacts job_build_dsn_node;
            Artifacts job_tezt_fetch_records;
          ]
      in
      (* Rules for tezt jobs that are started automatically.

         Tezt jobs are selected whenever a file in [changeset_octez] is modified.
         The tezt jobs are dependent (they need the binaries and such). *)
      let rules = make_rules ~dependent:true ~changes:changeset_octez () in
      (* Rules for tezt jobs that are started manually.

         These jobs can only be manually started on [before_merging]
         pipelines when its artifact dependencies exist, which they do
         when [changeset_octez] is changed. *)
      let rules_manual =
        make_rules ~dependent:true ~manual:(On_changes changeset_octez) ()
      in
      let coverage_expiry = Duration (Days 3) in
      let tezt : tezos_job =
        Tezt.job
          ~__POS__
          ~name:"tezt"
            (* Exclude all tests with tags in [tezt_tags_always_disable] or
               [tezt_tags_exclusive_tags]. *)
          ~tezt_tests:(Tezt.tests_tag_selector [Not (Has_tag "flaky")])
          ~tezt_parallel:6
          ~parallel:(Vector 50)
          ~timeout:(Minutes 40)
          ~rules
          ~dependencies
          ?job_select_tezts
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_time_sensitive : tezos_job =
        (* the following tests are executed with [~tezt_parallel:1] to ensure
           that other tests do not affect their executions. However, these
           tests are not particularly cpu/memory-intensive hence they do not
           need to run on a particular machine contrary to performance
           regression tests. *)
        Tezt.job
          ~__POS__
          ~name:"tezt-time-sensitive"
          ~tezt_tests:(Tezt.tests_tag_selector ~time_sensitive:true [])
          ~tezt_variant:"-time_sensitive"
          ~dependencies
          ?job_select_tezts
          ~rules
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_slow : tezos_job =
        Tezt.job
          ~__POS__
          ~name:"tezt-slow"
          ~rules:rules_manual
          ~tezt_tests:
            (Tezt.tests_tag_selector
               ~slow:true
               (* TODO: https://gitlab.com/tezos/tezos/-/issues/7063
                  The deselection of Paris [test_adaptive_issuance_launch.ml]
                  should be removed once the fixes to its slowness has been
                  snapshotted from Alpha. *)
               [
                 Not
                   (String_predicate
                      ( File,
                        Is
                          "src/proto_019_PtParisA/lib_protocol/test/integration/test_adaptive_issuance_launch.ml"
                      ));
               ])
          ~tezt_variant:"-slow"
          ~retry:2
          ~tezt_parallel:3
          ~parallel:(Vector 20)
          ~dependencies
          ?job_select_tezts
          ()
      in
      let tezt_flaky : tezos_job =
        (* Runs tests tagged "flaky" [Tag.flaky].

           These tests only run on scheduled pipelines. They run with
           higher retries (both GitLab CI job retries, and tezt
           retries). They also run with [~parallel:1] to increase
           stability. *)
        Tezt.job
          ~__POS__
          ~name:"tezt-flaky"
          ~tezt_tests:(Tezt.tests_tag_selector [Has_tag "flaky"])
          ~tezt_variant:"-flaky"
            (* To handle flakiness, consider tweaking [~tezt_parallel] (passed to
               Tezt's '--job-count'), and [~tezt_retry] (passed to Tezt's
               '--retry') *)
          ~retry:2
          ~tezt_retry:3
          ~tezt_parallel:1
          ~dependencies
          ?job_select_tezts
          ~rules:rules_manual
          ~allow_failure:Yes
          ()
        |> enable_coverage_output_artifact
      in
      let tezt_static_binaries : tezos_job =
        Tezt.job
          ~__POS__
          ~tag:Gcp
          ~name:"tezt:static-binaries"
          ~tezt_tests:
            (Tezt.tests_tag_selector [Has_tag "cli"; Not (Has_tag "flaky")])
          ~tezt_parallel:3
          ~retry:0
          ~dependencies:
            (Dependent
               [
                 Artifacts job_build_x86_64_exp_dev_extra;
                 Artifacts job_static_x86_64_experimental;
                 Artifacts job_tezt_fetch_records;
               ])
          ~rules
          ?job_select_tezts
          ~before_script:(before_script ["mv octez-binaries/x86_64/octez-* ."])
          ()
      in
      [tezt; tezt_time_sensitive; tezt_slow; tezt_flaky; tezt_static_binaries]
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
        |> enable_cargo_cache |> enable_sccache
      in
      [job_test_sdk_rust]
    in
    let jobs_sdk_bindings : tezos_job list =
      let job_test_sdk_bindings =
        job
          ~__POS__
          ~name:"test_sdk_bindings"
          ~image:Images.rust_sdk_bindings
          ~stage:Stages.test
          ~dependencies:dependencies_needs_start
          ~before_script:(before_script ~init_python_venv:true [])
          ~rules:
            (make_rules ~dependent:true ~changes:changeset_test_sdk_bindings ())
          [
            "make -C contrib/sdk-bindings check";
            "make -C contrib/sdk-bindings test";
          ]
        |> enable_cargo_cache |> enable_sccache
      in
      [job_test_sdk_bindings]
    in
    let jobs_kernels : tezos_job list =
      let make_job_kernel ?dependencies ?(stage = Stages.test) ~__POS__ ~name
          ~changes script =
        job
          ?dependencies
          ~__POS__
          ~name
          ~image:Images.rust_toolchain
          ~stage
          ~rules:(make_rules ~dependent:true ~changes ())
          script
        |> enable_kernels |> enable_cargo_cache |> enable_sccache
      in
      let job_test_kernels : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_kernels"
          ~changes:changeset_test_kernels
          ~dependencies:(Dependent [Job job_build_kernels])
          ["make -f kernels.mk check"; "make -f kernels.mk test"]
      in
      let job_test_etherlink_kernel : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_etherlink_kernel"
          ~changes:changeset_test_etherlink_kernel
          ~dependencies:(Dependent [Job job_build_kernels])
          ["make -f etherlink.mk check"; "make -f etherlink.mk test"]
      in
      let job_test_etherlink_firehose : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_etherlink_firehose"
          ~changes:changeset_test_etherlink_firehose
          ~dependencies:(Dependent [Job job_build_kernels])
          ["make -C etherlink/firehose check"]
      in
      let job_audit_riscv_deps : tezos_job =
        make_job_kernel
          ~stage:Stages.sanity
          ~__POS__
          ~name:"audit_riscv_deps"
          ~changes:changeset_riscv_kernels
          ["make -C src/riscv audit"]
      in
      let riscv_ci_flags =
        (* These flags ensure we don't need Ocaml installed in the check and test jobs *)
        "--no-default-features --features ci"
      in
      let job_check_riscv_kernels : tezos_job =
        make_job_kernel
          ~stage:Stages.build
          ~__POS__
          ~name:"check_riscv_kernels"
          ~changes:changeset_riscv_kernels
          ~dependencies:(Dependent [Job job_audit_riscv_deps])
          [
            Format.asprintf
              "make -C src/riscv CHECK_FLAGS= EXTRA_FLAGS='%s' check"
              riscv_ci_flags;
          ]
      in
      let job_test_riscv_kernels : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_riscv_kernels"
          ~changes:changeset_riscv_kernels
          ~dependencies:(Dependent [Job job_check_riscv_kernels])
          [
            Format.asprintf
              "make -C src/riscv EXTRA_FLAGS='%s' test"
              riscv_ci_flags;
          ]
      in
      let job_test_long_riscv_kernels : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_long_riscv_kernels"
          ~changes:changeset_riscv_kernels
          ~dependencies:(Dependent [Job job_check_riscv_kernels])
          [
            Format.asprintf
              "make -C src/riscv EXTRA_FLAGS='%s' test-long"
              riscv_ci_flags;
          ]
      in
      let job_test_miri_riscv_kernels : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_miri_riscv_kernels"
          ~changes:changeset_riscv_kernels
          ~dependencies:(Dependent [Job job_check_riscv_kernels])
          [
            Format.asprintf
              "make -C src/riscv EXTRA_FLAGS='%s' test-miri"
              riscv_ci_flags;
          ]
      in
      let job_test_evm_compatibility : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_evm_compatibility"
          ~changes:changeset_test_evm_compatibility
          ~dependencies:(Dependent [Job job_build_kernels])
          [
            "make -f etherlink.mk EVM_EVALUATION_FEATURES=disable-file-logs \
             evm-evaluation-assessor";
            "git clone --depth 1 --branch v14.1@etherlink \
             https://github.com/functori/tests ethereum_tests";
            "./evm-evaluation-assessor --eth-tests ./ethereum_tests/ \
             --resources ./etherlink/kernel_latest/evm_evaluation/resources/ \
             -c";
          ]
      in
      [
        job_test_kernels;
        job_test_etherlink_kernel;
        job_test_etherlink_firehose;
        job_audit_riscv_deps;
        job_check_riscv_kernels;
        job_test_riscv_kernels;
        job_test_long_riscv_kernels;
        job_test_miri_riscv_kernels;
        job_test_evm_compatibility;
      ]
    in
    let job_mir_unit =
      job
        ~__POS__
        ~name:"mir_unit"
        ~image:Images.CI.test
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_mir ())
        ["cargo test --manifest-path contrib/mir/Cargo.toml"]
      |> enable_cargo_cache
    in
    let job_mir_tzt =
      job
        ~__POS__
        ~name:"mir_tzt"
        ~image:Images.CI.test
        ~stage:Stages.test
        ~dependencies:dependencies_needs_start
        ~rules:(make_rules ~changes:changeset_mir_tzt ())
        [
          "cargo run --manifest-path contrib/mir/Cargo.toml --bin tzt_runner \
           tzt_reference_test_suite/*.tzt";
        ]
      |> enable_cargo_cache
    in
    let jobs_misc =
      [
        job_kaitai_checks;
        job_kaitai_e2e_checks;
        job_oc_check_lift_limits_patch;
        job_oc_python_check;
        job_oc_integration_compiler_rejections;
        job_oc_script_test_gen_genesis;
        job_oc_script_snapshot_alpha_and_link;
        job_oc_script_test_release_versions;
        job_oc_script_b58_prefix;
        job_oc_test_liquidity_baking_scripts;
        job_test_release_versions;
        job_mir_unit;
        job_mir_tzt;
      ]
    in
    let jobs_debian =
      match pipeline_type with
      | Before_merging | Merge_train ->
          [
            job_debian_repository_trigger_auto;
            job_rpm_repository_trigger_auto;
            job_homebrew_trigger_auto;
          ]
      | Schedule_extended_test ->
          [
            job_debian_repository_trigger_full;
            job_rpm_repository_trigger_full;
            job_homebrew_trigger_full;
          ]
    in
    jobs_debian @ jobs_misc @ jobs_sdk_rust @ jobs_sdk_bindings @ jobs_kernels
    @ jobs_unit @ jobs_install_octez @ jobs_tezt
  in

  (*Coverage jobs *)
  let coverage =
    match pipeline_type with
    | Before_merging | Merge_train ->
        (* Write the name of each job that produces coverage as input for other scripts.
           Only includes the stem of the name: parallel jobs only appear once.
           E.g. as [tezt], not [tezt X/Y]. *)
        Base.write_file
          "script-inputs/ci-coverage-producing-jobs"
          ~contents:
            (String.concat
               "\n"
               (List.map Tezos_ci.name_of_tezos_job !jobs_with_coverage_output)
            ^ "\n") ;
        (* This job fetches coverage files by precedent test stage. It creates
           the html, summary and cobertura reports. It also provide a coverage %
           for the merge request. *)
        let job_unified_coverage : tezos_job =
          let dependencies = List.rev !jobs_with_coverage_output in
          job
            ~__POS__
            ~image:Images.CI.e2etest
            ~name:"oc.unified_coverage"
            ~stage:Stages.test_coverage
            ~coverage:"/Coverage: ([^%]+%)/"
            ~rules:
              (make_rules
                 ~final_pipeline_disable:true
                 ~changes:changeset_octez
                 ())
            ~variables:
              [
                (* This inhibits the Makefile's opam version check, which
                   this job's opam-less image
                   ([e2etest]) cannot pass. *)
                ("TEZOS_WITHOUT_OPAM", "true");
              ]
            ~dependencies:(Staged dependencies)
            (* On the development branches, we compute coverage.
               TODO: https://gitlab.com/tezos/tezos/-/issues/6173
               We propagate the exit code to temporarily allow corrupted coverage files. *)
            (script_propagate_exit_code "./scripts/ci/report_coverage.sh")
            ~allow_failure:(With_exit_codes [64])
          |> enable_coverage_location |> enable_coverage_report
        in
        [job_unified_coverage]
    | Schedule_extended_test -> []
  in

  (*Doc jobs*)
  let doc =
    let jobs_install_python =
      (* Creates a job that tests installation of the python environment in [image] *)
      let job_install_python ~__POS__ ~name ~image ~project ~branch =
        job
          ~__POS__
          ~name
          ~image
          ~stage:Stages.doc
          ~dependencies:dependencies_needs_start
          ~rules:
            (make_rules
               ~changes:
                 (Changeset.make
                    [
                      "docs/developer/install-python-debian-ubuntu.sh";
                      "pyproject.toml";
                      "poetry.lock";
                    ])
               ~manual:Yes
               ~label:"ci--docs"
               ())
          [
            sf
              "./docs/developer/install-python-debian-ubuntu.sh %s %s"
              project
              branch;
          ]
      in
      (* The set of python installation test jobs. Since python is
         today less used, we do the bulk of the tests in scheduled pipelines
         and we only test debian_bookworm in a merge pipeline *)
      match pipeline_type with
      | Schedule_extended_test ->
          [
            job_install_python
              ~__POS__
              ~name:"oc.install_python_noble"
              ~image:Images.ubuntu_noble
              ~project:"tezos/tezos"
              ~branch:"master";
            job_install_python
              ~__POS__
              ~name:"oc.install_python_jammy"
              ~image:Images.ubuntu_jammy
              ~project:"tezos/tezos"
              ~branch:"master";
            job_install_python
              ~__POS__
              ~name:"oc.install_python_bookworm"
              ~image:Images.debian_bookworm
              ~project:"tezos/tezos"
              ~branch:"master";
          ]
      | Before_merging | Merge_train ->
          [
            job_install_python
              ~__POS__
              ~name:"oc.install_python_bookworm"
              ~image:Images.debian_bookworm
              ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
              ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}";
          ]
    in
    let jobs_documentation : tezos_job list =
      let rules =
        make_rules ~changes:changeset_octez_docs ~label:"ci--docs" ()
      in
      let dependencies = dependencies_needs_start in
      let job_odoc =
        Documentation.job_odoc ~rules ~dependencies ~lite:true ()
      in
      let job_manuals =
        Documentation.job_manuals
          ~rules
          ~use_static_executables:false
          ~dependencies:
            (Dependent
               [
                 Artifacts job_build_x86_64_release;
                 Artifacts job_build_x86_64_exp_dev_extra;
                 Artifacts job_build_kernels;
                 Artifacts job_build_dsn_node;
               ])
          ()
      in
      let job_docgen = Documentation.job_docgen ~rules ~dependencies () in
      let job_build_all =
        Documentation.job_build_all
          ~job_odoc
          ~job_manuals
          ~job_docgen
          ~rules:
            (make_rules
               ~dependent:true
               ~changes:changeset_octez_docs
               ~label:"ci--docs"
               ())
          ()
      in
      let job_documentation_linkcheck : tezos_job =
        Documentation.job_linkcheck
          ~job_manuals
          ~job_docgen
          ~job_build_all
          ~rules:
            (make_rules
               ~dependent:true
               ~label:"ci--docs"
               ~manual:(On_changes changeset_octez_docs)
               ())
          ()
      in
      [
        job_odoc;
        job_manuals;
        job_docgen;
        job_build_all;
        job_documentation_linkcheck;
      ]
    in
    jobs_install_python @ jobs_documentation
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
          [job_docker_amd64_test_manual; job_docker_arm64_test_manual]
          @ [job_docker_verify_test_arm64; job_docker_verify_test_amd64]
        in
        if pipeline_type = Merge_train then jobs
        else
          job_homebrew_repository_trigger :: job_rpm_repository_trigger_partial
          :: job_debian_repository_trigger_partial :: jobs
    (* No manual jobs on the scheduled pipeline *)
    | Schedule_extended_test -> []
  in
  start_stage @ sanity @ build @ packaging @ test @ coverage @ doc @ manual
