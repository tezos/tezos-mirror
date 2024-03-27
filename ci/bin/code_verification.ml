(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* Encodes the conditional [before_merging] pipeline and its
   unconditional variant [schedule_extended_test]. *)
type code_verification_pipeline = Before_merging | Schedule_extended_test

(** Manual trigger configuration for [make_rules] *)
type manual =
  | No  (** Do not add rule for manual trigger. *)
  | Yes  (** Add rule for manual trigger. *)
  | On_changes of Changeset.t  (** Add manual trigger on certain [changes:] *)

(* [make_rules] makes rules for jobs that are:
     - automatic in scheduled pipelines;
     - conditional in [before_merging] pipelines.

     If a job has non-optional dependencies, then [dependent] must be
     set to [true] to ensure that we only run the job in case previous
     jobs succeeded (setting [when: on_success]).

     If [label] is set, add rule that selects the job in
     [Before_merging] pipelines for merge requests with the given
     label. Rules for manual triggers can be configured using
     [manual].

     If [label], [changes] and [manual] are omitted, then rules will
     enable the job [On_success] in the [before_merging]
     pipeline. This is safe, but prefer specifying a [changes] clause
     if possible. *)
let make_rules ?label ?changes ?(manual = No) ?(dependent = false) pipeline_type
    =
  match pipeline_type with
  | Schedule_extended_test ->
      (* The scheduled pipeline always runs all jobs unconditionally
         -- unless they are dependent on a previous, non-trigger job, in the
         pipeline. *)
      [job_rule ~when_:(if dependent then On_success else Always) ()]
  | Before_merging -> (
      (* MR labels can be used to force tests to run. *)
      (match label with
      | Some label ->
          [job_rule ~if_:Rules.(has_mr_label label) ~when_:On_success ()]
      | None -> [])
      (* Modifying some files can force tests to run. *)
      @ (match changes with
        | None -> []
        | Some changes ->
            [job_rule ~changes:(Changeset.encode changes) ~when_:On_success ()])
      (* For some tests, it can be relevant to have a manual trigger. *)
      @
      match manual with
      | No -> []
      | Yes -> [job_rule ~when_:Manual ()]
      | On_changes changes ->
          [job_rule ~when_:Manual ~changes:(Changeset.encode changes) ()])

type opam_package_group = Executable | All

type opam_package = {
  name : string;
  group : opam_package_group;
  batch_index : int;
}

let opam_rules ~only_marge_bot ?batch_index () =
  let when_ =
    match batch_index with
    | Some batch_index -> Delayed (Minutes batch_index)
    | None -> On_success
  in
  [
    job_rule ~if_:Rules.schedule_extended_tests ~when_ ();
    job_rule ~if_:(Rules.has_mr_label "ci--opam") ~when_ ();
    job_rule
      ~if_:
        (if only_marge_bot then
         If.(Rules.merge_request && Rules.triggered_by_marge_bot)
        else Rules.merge_request)
      ~changes:(Changeset.encode changeset_opam_jobs)
      ~when_
      ();
    job_rule ~when_:Never ();
  ]

let job_opam_package ?dependencies {name; group; batch_index} : tezos_job =
  job
    ?dependencies
    ~__POS__
    ~name:("opam:" ^ name)
    ~image:Images.runtime_prebuild_dependencies
    ~stage:Stages.packaging
      (* FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/663
         FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/664
         At the time of writing, the opam tests were quite flaky.
         Therefore, a retry was added. This should be removed once the
         underlying tests have been fixed. *)
    ~retry:2
    ~rules:(opam_rules ~only_marge_bot:(group = All) ~batch_index ())
    ~variables:
      [
        (* See [.gitlab-ci.yml] for details on [RUNTEZTALIAS] *)
        ("RUNTEZTALIAS", "true");
        ("package", name);
      ]
    ~before_script:
      (before_script
         ~eval_opam:true
         [
           "mkdir -p $CI_PROJECT_DIR/opam_logs";
           ". ./scripts/ci/sccache-start.sh";
         ])
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
        "sccache --stop-server || true";
        "eval $(opam env)";
        "OPAM_LOGS=opam_logs ./scripts/ci/opam_handle_output.sh";
      ]
    ~artifacts:
      (artifacts ~expire_in:(Duration (Weeks 1)) ~when_:Always ["opam_logs/"])
    ~cache:[{key = "opam-sccache"; paths = ["_build/_sccache"]}]
  |> (* We store caches in [_build] for two reasons: (1) the [_build]
        folder is excluded from opam's rsync. (2) gitlab ci cache
        requires that cached files are in a sub-folder of the checkout. *)
  enable_sccache
    ~error_log:"$CI_PROJECT_DIR/opam_logs/sccache.log"
    ~idle_timeout:"0"
    ~log:"debug"
    ~dir:"$CI_PROJECT_DIR/_build/_sccache"

let ci_opam_package_tests = "script-inputs/ci-opam-package-tests"

let read_opam_packages =
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
          match group with "exec" -> Executable | "all" -> All | _ -> fail ()
        in
        Some {name; group; batch_index}
    | _ -> fail ()

let jobs_unit_tests ~job_build_x86_64_release ~job_build_x86_64_exp_dev_extra
    ~job_build_arm64_release ~job_build_arm64_exp_dev_extra pipeline_type :
    tezos_job list =
  let build_dependencies = function
    | Amd64 ->
        Dependent
          [Job job_build_x86_64_release; Job job_build_x86_64_exp_dev_extra]
    | Arm64 ->
        Dependent
          [Job job_build_arm64_release; Job job_build_arm64_exp_dev_extra]
  in
  let rules =
    (* TODO: Note that all jobs defined in this function are
       [dependent] in the sense that they all have
       [dependencies:]. However, AFAICT, these dependencies are all
       dummy dependencies for ordering -- not for artifacts. This
       means that these unit tests can very well run even if their
       dependencies failed. Moreover, the ordering serves no purpose
       on scheduled pipelines, so we might even remove them for that
       [pipeline_type]. *)
    make_rules ~changes:changeset_octez ~dependent:true pipeline_type
  in
  let job_unit_test ~__POS__ ?(image = Images.runtime_build_dependencies)
      ?timeout ?parallel_vector ~arch ~name ?(enable_coverage = true)
      ~make_targets () : tezos_job =
    let arch_string = arch_to_string arch in
    let script =
      ["make $MAKE_TARGETS"]
      @ if enable_coverage then ["./scripts/ci/merge_coverage.sh"] else []
    in
    let dependencies = build_dependencies arch in
    let variables =
      [("ARCH", arch_string); ("MAKE_TARGETS", String.concat " " make_targets)]
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
        ~retry:2
        ~name
        ~stage:Stages.test
        ~image
        ~arch
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
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        script
    in
    if enable_coverage then
      job |> enable_coverage_instrumentation |> enable_coverage_output_artifact
    else job
  in
  let oc_unit_non_proto_x86_64 =
    job_unit_test
      ~__POS__
      ~name:"oc.unit:non-proto-x86_64"
      ~arch:Amd64 (* The [lib_benchmark] unit tests require Python *)
      ~image:Images.runtime_build_test_dependencies
      ~make_targets:["test-nonproto-unit"]
      ()
  in
  let oc_unit_other_x86_64 =
    (* Runs unit tests for contrib. *)
    job_unit_test
      ~__POS__
      ~name:"oc.unit:other-x86_64"
      ~arch:Amd64
      ~make_targets:["test-other-unit"]
      ()
  in
  let oc_unit_proto_x86_64 =
    (* Runs unit tests for protocol. *)
    job_unit_test
      ~__POS__
      ~name:"oc.unit:proto-x86_64"
      ~arch:Amd64
      ~make_targets:["test-proto-unit"]
      ()
  in
  let oc_unit_non_proto_arm64 =
    job_unit_test
      ~__POS__
      ~name:"oc.unit:non-proto-arm64"
      ~parallel_vector:2
      ~arch:Arm64 (* The [lib_benchmark] unit tests require Python *)
      ~image:Images.runtime_build_test_dependencies
      ~make_targets:["test-nonproto-unit"; "test-webassembly"]
        (* No coverage for arm64 jobs -- the code they test is a
           subset of that tested by x86_64 unit tests. *)
      ~enable_coverage:false
      ()
  in
  let oc_unit_webassembly_x86_64 =
    job
      ~__POS__
      ~name:"oc.unit:webassembly-x86_64"
      ~arch:Amd64 (* The wasm tests are written in Python *)
      ~image:Images.runtime_build_test_dependencies
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
  let oc_unit_js_components =
    job
      ~__POS__
      ~name:"oc.unit:js_components"
      ~arch:Amd64
      ~image:Images.runtime_build_test_dependencies
      ~stage:Stages.test
      ~dependencies:(build_dependencies Amd64)
      ~rules
      ~retry:2
      ~variables:[("RUNTEZTALIAS", "true")]
      ~before_script:
        (before_script
           ~take_ownership:true
           ~source_version:true
           ~eval_opam:true
           ~install_js_deps:true
           [])
      ["make test-js"]
  in
  let oc_unit_protocol_compiles =
    job
      ~__POS__
      ~name:"oc.unit:protocol_compiles"
      ~arch:Amd64
      ~image:Images.runtime_build_dependencies
      ~stage:Stages.test
      ~dependencies:(build_dependencies Amd64)
      ~rules
      ~before_script:(before_script ~source_version:true ~eval_opam:true [])
      ["dune build @runtest_compile_protocol"]
  in
  [
    oc_unit_non_proto_x86_64;
    oc_unit_other_x86_64;
    oc_unit_proto_x86_64;
    oc_unit_non_proto_arm64;
    oc_unit_webassembly_x86_64;
    oc_unit_js_components;
    oc_unit_protocol_compiles;
  ]

type install_octez_distribution = Ubuntu_focal | Ubuntu_jammy | Fedora_37

let image_of_distribution = function
  | Ubuntu_focal -> Images.ubuntu_focal
  | Ubuntu_jammy -> Images.ubuntu_jammy
  | Fedora_37 -> Images.fedora_37

let job_tezt ~__POS__ ?rules ?parallel ?(tags = ["gcp_tezt"]) ~name
    ~(tezt_tests : Tezt_core.TSL_AST.t) ?(retry = 2) ?(tezt_retry = 1)
    ?(tezt_parallel = 1) ?(tezt_variant = "")
    ?(before_script = before_script ~source_version:true ~eval_opam:true [])
    ~dependencies () : tezos_job =
  let variables =
    [
      ("JUNIT", "tezt-junit.xml");
      ("TEZT_VARIANT", tezt_variant);
      ("TESTS", Tezt_core.TSL.show tezt_tests);
      ("TEZT_RETRY", string_of_int tezt_retry);
      ("TEZT_PARALLEL", string_of_int tezt_parallel);
    ]
  in
  let artifacts =
    artifacts
      ~reports:(reports ~junit:"$JUNIT" ())
      [
        "selected_tezts.tsv";
        "tezt.log";
        "tezt-*.log";
        "tezt-results-${CI_NODE_INDEX:-1}${TEZT_VARIANT}.json";
        "$JUNIT";
      ]
      (* The record artifacts [tezt-results-$CI_NODE_INDEX.json]
         should be stored for as long as a given commit on master is
         expected to be HEAD in order to support auto-balancing. At
         the time of writing, we have approximately 6 merges per day,
         so 1 day should more than enough. However, we set it to 3
         days to keep records over the weekend. The tezt artifacts
         (including records and coverage) take up roughly 2MB /
         job. Total artifact storage becomes [N*P*T*W] where [N] is
         the days of retention (3 atm), [P] the number of pipelines
         per day (~200 atm), [T] the number of Tezt jobs per pipeline
         (60) and [W] the artifact size per tezt job (2MB). This makes
         35GB which is less than 0.5% than our
         {{:https://gitlab.com/tezos/tezos/-/artifacts}total artifact
         usage}. *)
      ~expire_in:(Duration (Days 7))
      ~when_:Always
  in
  let print_variables =
    [
      "TESTS";
      "JUNIT";
      "CI_NODE_INDEX";
      "CI_NODE_TOTAL";
      "TEZT_PARALLEL";
      "TEZT_VARIANT";
    ]
  in
  let retry = if retry = 0 then None else Some retry in
  job
    ~__POS__
    ~image:Images.runtime_e2etest_dependencies
    ~name
    ?parallel
    ~tags
    ~stage:Stages.test
    ?rules
    ~artifacts
    ~variables
    ~dependencies
    ?retry
    ~before_script
    [
      (* Print [print_variables] in a shell-friendly manner for easier debugging *)
      "echo \""
      ^ String.concat
          " "
          (List.map (fun var -> sf {|%s=\"${%s}\"|} var var) print_variables)
      ^ "\"";
      (* Store the list of tests that have been scheduled for execution for later debugging.
         It is imperative this this first call to tezt receives any flags passed to the
         second call that affect test selection.Note that TESTS must be quoted (here and below)
         since it will contain e.g. '&&' which we want to interpreted as TSL and not shell
         syntax. *)
      "./scripts/ci/tezt.sh \"${TESTS}\" --from-record tezt/records --job \
       ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --list-tsv > selected_tezts.tsv";
      (* For Tezt tests, there are multiple timeouts:
         - --global-timeout is the internal timeout of Tezt, which only works if tests
           are cooperative;
         - the "timeout" command, which we set to send SIGTERM to Tezt 60s after --global-timeout
           in case tests are not cooperative;
         - the "timeout" command also sends SIGKILL 60s after having sent SIGTERM in case
           Tezt is still stuck;
         - the CI timeout.
         The use of the "timeout" command is to make sure that Tezt eventually exits,
         because if the CI timeout is reached, there are no artefacts,
         and thus no logs to investigate.
         See also: https://gitlab.com/gitlab-org/gitlab/-/issues/19818 *)
      "./scripts/ci/exit_code.sh timeout -k 60 1860 ./scripts/ci/tezt.sh \
       \"${TESTS}\" --color --log-buffer-size 5000 --log-file tezt.log \
       --global-timeout 1800 --on-unknown-regression-files fail --junit \
       ${JUNIT} --from-record tezt/records --job \
       ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --record \
       tezt-results-${CI_NODE_INDEX:-1}${TEZT_VARIANT}.json --job-count \
       ${TEZT_PARALLEL} --retry ${TEZT_RETRY}";
      "if [ -n \"${BISECT_FILE:-}\" ]; then ./scripts/ci/merge_coverage.sh; fi";
    ]

(** Tezt tag selector string.

    It returns a TSL expression that:
    - always deselects tags with [ci_disabled];
    - selects, respectively deselects, the tests with the tags
      [memory_3k], [memory_4k], [time_sensitive], [slow] or [cloud],
      depending on the value of the corresponding function
      argument. These arguments all default to false.

    See [src/lib_test/tag.mli] for a description of the above tags.

    The list of TSL expressions [and_] are appended to the final
    selector, allowing to modify the selection further. *)
let tezt_tests ?(memory_3k = false) ?(memory_4k = false)
    ?(time_sensitive = false) ?(slow = false) ?(cloud = false)
    (and_ : Tezt_core.TSL_AST.t list) : Tezt_core.TSL_AST.t =
  let tags =
    [
      (false, "ci_disabled");
      (memory_3k, "memory_3k");
      (memory_4k, "memory_4k");
      (time_sensitive, "time_sensitive");
      (slow, "slow");
      (cloud, "cloud");
    ]
  in
  let positive, negative = List.partition fst tags in
  let positive = List.map snd positive in
  let negative = List.map snd negative in
  Tezt_core.(
    TSL.conjunction
    @@ List.map (fun tag -> TSL_AST.Has_tag tag) positive
    @ List.map (fun tag -> TSL_AST.Not (Has_tag tag)) negative
    @ and_)

(* Encodes the conditional [before_merging] pipeline and its unconditional variant
   [schedule_extended_test]. *)
let jobs pipeline_type =
  let make_rules ?label ?changes ?manual ?dependent () =
    make_rules ?label ?changes ?manual ?dependent pipeline_type
  in
  (* Externalization *)
  let job_external_split ?(before_merging_suffix = "before_merging")
      ?(scheduled_suffix = "scheduled_extended_test") job =
    job_external
      ~filename_suffix:
        (match pipeline_type with
        | Before_merging -> before_merging_suffix
        | Schedule_extended_test -> scheduled_suffix)
      job
  in
  let jobs_external_split ?(before_merging_suffix = "before_merging")
      ?(scheduled_suffix = "scheduled_extended_test") ~path jobs =
    let path =
      sf
        "%s-%s.yml"
        path
        (match pipeline_type with
        | Before_merging -> before_merging_suffix
        | Schedule_extended_test -> scheduled_suffix)
    in
    jobs_external ~path jobs
  in
  (* Used to externalize jobs that are the same on both pipelines. They're only written once.
     Beware: there is no check that the two jobs are actually identical. *)
  let job_external_once job =
    match pipeline_type with
    | Before_merging -> job_external job
    | Schedule_extended_test -> job
  in
  (* as [job_external_once] but for sets of jobs *)
  let jobs_external_once ~path jobs =
    match pipeline_type with
    | Before_merging -> jobs_external ~path jobs
    | Schedule_extended_test -> jobs
  in
  (* Common GitLab CI caches *)
  let cache_kernels = {key = "kernels"; paths = ["cargo/"]} in
  (* Stages *)
  (* All stages should be empty, as explained below, until the full pipeline is generated. *)
  let trigger_stage, make_dependencies =
    match pipeline_type with
    | Schedule_extended_test ->
        let make_dependencies ~before_merging:_ ~schedule_extended_test =
          schedule_extended_test ()
        in
        ([], make_dependencies)
    | Before_merging ->
        (* Define the [trigger] job

           §1: The purpose of this job is to launch the CI manually in certain cases.
           The objective is not to run computing when it is not
           necessary and the decision to do so belongs to the developer

           §2: We also perform some fast sanity checks. *)
        let job_trigger =
          job
            ~__POS__
            ~image:Images.alpine
            ~stage:Stages.trigger
            ~allow_failure:No
            ~rules:
              [
                job_rule
                  ~if_:(If.not Rules.assigned_to_marge_bot)
                  ~allow_failure:No
                  ~when_:Manual
                  ();
                job_rule ~when_:Always ();
              ]
            ~timeout:(Minutes 10)
            ~name:"trigger"
            [
              "echo 'Trigger pipeline!'";
              (* Check that [.gitlab-ci.yml]'s [build_deps_image_version] and
                 [scripts/version.sh]'s [opam_repository_tag] are the same. *)
              "./scripts/ci/check_opam_repository_tag.sh";
              (* Check that the Alpine version of the trigger job's image
                 corresponds to the value in scripts/version.sh. *)
              "./scripts/ci/check_alpine_version.sh";
            ]
          |> job_external
        in
        let make_dependencies ~before_merging ~schedule_extended_test:_ =
          before_merging job_trigger
        in
        ([job_trigger], make_dependencies)
  in
  (* Short-cut for jobs that has no dependencies except [job_trigger]
     on [Before_merging] pipelines. *)
  let dependencies_needs_trigger =
    make_dependencies
      ~before_merging:(fun job_trigger -> Dependent [Job job_trigger])
      ~schedule_extended_test:(fun () -> Staged [])
  in
  let sanity =
    let job_sanity_ci : tezos_job =
      job
        ~__POS__
        ~name:"sanity_ci"
        ~image:Images.runtime_build_dependencies
        ~stage:Stages.sanity
        ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
        [
          "make -C manifest check";
          "./scripts/lint.sh --check-gitlab-ci-yml";
          (* Check that the opam-repo images' Alpine version corresponds to
             the value in scripts/version.sh. *)
          "./scripts/ci/check_alpine_version.sh";
          (* Check that .gitlab-ci.yml is up to date. *)
          "make -C ci check";
        ]
      |> job_external_once
    in
    let job_docker_hadolint =
      job
        ~rules:(make_rules ~changes:changeset_hadolint_docker_files ())
        ~__POS__
        ~name:
          (* TODO: I'm not sure it makes sense to have different names for the same job *)
          ("docker:hadolint-"
          ^
          match pipeline_type with
          | Before_merging -> "before_merging"
          | Schedule_extended_test -> "schedule_extended_test")
        ~image:Images.hadolint
        ~stage:Stages.sanity
        ["hadolint build.Dockerfile"; "hadolint Dockerfile"]
      |> job_external
    in
    [job_sanity_ci; job_docker_hadolint]
  in
  let job_docker_rust_toolchain =
    job_docker_rust_toolchain
      ~__POS__
      ~rules:(make_rules ~changes:changeset_octez_or_kernels ~manual:Yes ())
      ~dependencies:dependencies_needs_trigger
      ()
    |> job_external_split
  in
  let job_docker_client_libs_dependencies =
    job_docker_authenticated
      ~__POS__
      ~rules:(make_rules ~changes:changeset_kaitai_e2e_files ())
      ~stage:Stages.build
      ~name:"oc.docker:client-libs-dependencies"
        (* These image are not built for external use. *)
      ~ci_docker_hub:false
        (* Handle docker initialization, if necessary, in [./scripts/ci/docker_client_libs_dependencies_build.sh]. *)
      ~skip_docker_initialization:true
      ["./scripts/ci/docker_client_libs_dependencies_build.sh"]
      ~artifacts:
        (artifacts
           ~reports:
             (reports ~dotenv:"client_libs_dependencies_image_tag.env" ())
           [])
    |> job_external_split
  in
  (* The build_x86_64 jobs are split in two to keep the artifact size
     under the 1GB hard limit set by GitLab. *)
  (* [job_build_x86_64_release] builds the released executables. *)
  let job_build_x86_64_release =
    job_build_dynamic_binaries
      ~__POS__
      ~arch:Amd64
      ~dependencies:dependencies_needs_trigger
      ~release:true
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
    |> job_external_split
  in
  (* 'oc.build_x86_64-exp-dev-extra' builds the developer and experimental
     executables, as well as the tezt test suite used by the subsequent
     'tezt' jobs and TPS evaluation tool. *)
  let job_build_x86_64_exp_dev_extra =
    job_build_dynamic_binaries
      ~__POS__
      ~arch:Amd64
      ~dependencies:dependencies_needs_trigger
      ~release:false
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
    |> job_external_split
  in
  let build_arm_rules = make_rules ~label:"ci--arm64" ~manual:Yes () in
  let job_build_arm64_release : Tezos_ci.tezos_job =
    job_build_arm64_release ~rules:build_arm_rules () |> job_external_split
  in
  let job_build_arm64_exp_dev_extra : Tezos_ci.tezos_job =
    job_build_arm64_exp_dev_extra ~rules:build_arm_rules ()
    |> job_external_split
  in
  (* Used in [before_merging] and [schedule_extended_tests].

     Fetch records for Tezt generated on the last merge request pipeline
     on the most recently merged MR and makes them available in artifacts
     for future merge request pipelines. *)
  let job_select_tezts : tezos_job =
    job
      ~__POS__
      ~name:"select_tezts"
        (* We need:
           - Git (to run git diff)
           - ocamlyacc, ocamllex and ocamlc (to build manifest/manifest) *)
      ~image:Images.runtime_prebuild_dependencies
      ~stage:Stages.build
      ~before_script:(before_script ~take_ownership:true ~eval_opam:true [])
      (script_propagate_exit_code "scripts/ci/select_tezts.sh")
      ~allow_failure:(With_exit_codes [17])
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Days 3))
           ~when_:Always
           ["selected_tezts.tsl"])
    |> job_external_once
  in
  let job_build_kernels : tezos_job =
    job
      ~__POS__
      ~name:"oc.build_kernels"
      ~image:Images.rust_toolchain
      ~stage:Stages.build
      ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
      ~rules:(make_rules ~changes:changeset_octez_or_kernels ~dependent:true ())
      [
        "make -f kernels.mk build";
        "make -f etherlink.mk evm_kernel.wasm";
        "make -C src/risc_v risc-v-sandbox risc-v-dummy.elf";
        "make -C src/risc_v/tests/ build";
      ]
      ~artifacts:
        (artifacts
           ~name:"build-kernels-$CI_COMMIT_REF_SLUG"
           ~expire_in:(Duration (Days 1))
           ~when_:On_success
           [
             "evm_kernel.wasm";
             "smart-rollup-installer";
             "sequenced_kernel.wasm";
             "tx_kernel.wasm";
             "tx_kernel_dal.wasm";
             "dal_echo_kernel.wasm";
             "src/risc_v/risc-v-sandbox";
             "src/risc_v/risc-v-dummy.elf";
             "src/risc_v/tests/inline_asm/rv64-inline-asm-tests";
           ])
      ~cache:[cache_kernels; {key = "kernels-sccache"; paths = ["_sccache"]}]
    |> enable_kernels |> enable_sccache |> job_external_split
  in
  (* Fetch records for Tezt generated on the last merge request pipeline
         on the most recently merged MR and makes them available in artifacts
         for future merge request pipelines. *)
  let job_tezt_fetch_records : tezos_job =
    job
      ~__POS__
      ~name:"oc.tezt:fetch-records"
      ~image:Images.runtime_build_dependencies
      ~stage:Stages.build
      ~before_script:
        (before_script
           ~take_ownership:true
           ~source_version:true
           ~eval_opam:true
           [])
      ~rules:(make_rules ~changes:changeset_octez ())
      [
        "dune exec scripts/ci/update_records/update.exe -- --log-file \
         tezt-fetch-records.log --from last-successful-schedule-extended-test \
         --info";
      ]
      ~after_script:["./scripts/ci/filter_corrupted_records.sh"]
        (* Allow failure of this job, since Tezt can use the records
           stored in the repo as backup for balancing. *)
      ~allow_failure:Yes
      ~artifacts:
        (artifacts
           ~expire_in:(Duration (Hours 4))
           ~when_:Always
           [
             "tezt-fetch-records.log";
             "tezt/records/*.json";
             (* Keep broken records for debugging *)
             "tezt/records/*.json.broken";
           ])
    |> job_external_split
  in
  let job_static_x86_64_experimental =
    job_build_static_binaries
      ~__POS__
      ~arch:Amd64
        (* Even though not many tests depend on static executables, some
           of those that do are limiting factors in the total duration
           of pipelines. So we start this job as early as possible,
           without waiting for sanity_ci. *)
      ~dependencies:dependencies_needs_trigger
      ~rules:(make_rules ~changes:changeset_octez ())
      ()
    |> job_external_split
  in
  let build =
    (* TODO: The code is a bit convulted here because these jobs are
       either in the build or in the manual stage depending on the
       pipeline type. However, we can put them in the build stage on
       [before_merging] pipelines as long as we're careful to put
       [allow_failure: true]. *)
    let bin_packages_jobs =
      match pipeline_type with
      | Schedule_extended_test ->
          let job_build_dpkg_amd64 = job_build_dpkg_amd64 () |> job_external in
          let job_build_rpm_amd64 = job_build_rpm_amd64 () |> job_external in
          [job_build_dpkg_amd64; job_build_rpm_amd64]
      | Before_merging -> []
    in
    let job_ocaml_check : tezos_job =
      job
        ~__POS__
        ~name:"ocaml-check"
        ~image:Images.runtime_build_dependencies
        ~stage:Stages.build
        ~dependencies:dependencies_needs_trigger
        ~rules:(make_rules ~changes:changeset_ocaml_files ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ["dune build @check"]
      |> job_external_split
    in
    [
      job_docker_rust_toolchain;
      job_docker_client_libs_dependencies;
      job_build_arm64_release;
      job_build_arm64_exp_dev_extra;
      job_static_x86_64_experimental;
      job_build_x86_64_release;
      job_build_x86_64_exp_dev_extra;
      job_ocaml_check;
      job_build_kernels;
      job_tezt_fetch_records;
      job_select_tezts;
    ]
    @ bin_packages_jobs
  in
  let packaging =
    let job_opam_prepare : tezos_job =
      job
        ~__POS__
        ~name:"opam:prepare"
        ~image:Images.runtime_prebuild_dependencies
        ~stage:Stages.packaging
        ~dependencies:dependencies_needs_trigger
        ~before_script:(before_script ~eval_opam:true [])
        ~artifacts:(artifacts ["_opam-repo-for-release/"])
        ~rules:(opam_rules ~only_marge_bot:false ~batch_index:1 ())
        [
          "git init _opam-repo-for-release";
          "./scripts/opam-prepare-repo.sh dev ./ ./_opam-repo-for-release";
          "git -C _opam-repo-for-release add packages";
          "git -C _opam-repo-for-release commit -m \"tezos packages\"";
        ]
      |> job_external_split
    in
    let (jobs_opam_packages : tezos_job list) =
      read_opam_packages
      |> List.map
           (job_opam_package
              ~dependencies:(Dependent [Artifacts job_opam_prepare]))
      |> jobs_external_once ~path:"packaging/opam_package.yml"
    in
    job_opam_prepare :: jobs_opam_packages
  in
  (* Dependencies for jobs that should run immediately after jobs
     [job_build_x86_64] in [Before_merging] if they are present
     (otherwise, they run immediately after [job_trigger]). In
     [Scheduled_extended_test] we are not in a hurry and we let them
     be [Staged []]. *)
  let order_after_build =
    make_dependencies
      ~before_merging:(fun job_trigger ->
        Dependent
          (Job job_trigger
          :: [
               Optional job_build_x86_64_release;
               Optional job_build_x86_64_exp_dev_extra;
             ]))
      ~schedule_extended_test:(fun () -> Staged [])
  in
  let test =
    (* check that ksy files are still up-to-date with octez *)
    let job_kaitai_checks : tezos_job =
      job
        ~__POS__
        ~name:"kaitai_checks"
        ~image:Images.runtime_build_dependencies
        ~stage:Stages.test
        ~dependencies:dependencies_needs_trigger
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
      |> job_external_split
    in
    let job_kaitai_e2e_checks =
      job
        ~__POS__
        ~name:"kaitai_e2e_checks"
        ~image:Images.client_libs_dependencies
        ~stage:Stages.test
        ~dependencies:
          (Dependent
             [
               Artifacts job_docker_client_libs_dependencies;
               Artifacts job_kaitai_checks;
             ])
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
      |> job_external_split
    in
    let job_oc_check_lift_limits_patch =
      job
        ~__POS__
        ~name:"oc.check_lift_limits_patch"
        ~image:Images.runtime_build_dependencies
        ~stage:Stages.test
        ~dependencies:dependencies_needs_trigger
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
      |> job_external_split
    in
    let job_oc_misc_checks : tezos_job =
      job
        ~__POS__
        ~name:"oc.misc_checks"
        ~image:Images.runtime_build_test_dependencies
        ~stage:Stages.test
        ~dependencies:dependencies_needs_trigger
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
         ]
        @
        (* The license check only applies to new files (in the sense
           of [git add]), so can only run in [before_merging]
           pipelines. *)
        if pipeline_type = Before_merging then
          ["./scripts/ci/lint_check_licenses.sh"]
        else [])
      |> job_external_split
    in
    let job_misc_opam_checks : tezos_job =
      job
        ~__POS__
        ~name:"misc_opam_checks"
        ~image:Images.runtime_build_dependencies
        ~stage:Stages.test
        ~retry:2
        ~dependencies:dependencies_needs_trigger
        ~rules:(make_rules ~changes:changeset_octez ())
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        [
          (* checks that all deps of opam packages are already installed *)
          "./scripts/opam-check.sh";
        ]
      |> job_external_split
    in
    let job_semgrep : tezos_job =
      job
        ~__POS__
        ~name:"oc.semgrep"
        ~image:Images.semgrep_agent
        ~stage:Stages.test
        ~dependencies:dependencies_needs_trigger
        ~rules:(make_rules ~changes:changeset_semgrep_files ())
        [
          "echo \"OCaml code linting. For information on how to reproduce \
           locally, check out scripts/semgrep/README.md\"";
          "sh ./scripts/semgrep/lint-all-ocaml-sources.sh";
        ]
      |> job_external_split
    in
    let jobs_unit : tezos_job list =
      jobs_unit_tests
        ~job_build_x86_64_release
        ~job_build_x86_64_exp_dev_extra
        ~job_build_arm64_release
        ~job_build_arm64_exp_dev_extra
        pipeline_type
      |> jobs_external_split ~path:"test/oc.unit"
    in
    let job_oc_integration_compiler_rejections : tezos_job =
      job
        ~__POS__
        ~name:"oc.integration:compiler-rejections"
        ~stage:Stages.test
        ~image:Images.runtime_build_dependencies
        ~rules:(make_rules ~changes:changeset_octez ())
        ~dependencies:
          (Dependent
             [Job job_build_x86_64_release; Job job_build_x86_64_exp_dev_extra])
        ~before_script:(before_script ~source_version:true ~eval_opam:true [])
        ["dune build @runtest_rejections"]
      |> job_external_split
    in
    let job_oc_script_test_gen_genesis : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:test-gen-genesis"
        ~stage:Stages.test
        ~image:Images.runtime_build_dependencies
        ~dependencies:dependencies_needs_trigger
        ~rules:(make_rules ~changes:changeset_octez ())
        ~before_script:
          (before_script ~eval_opam:true ["cd scripts/gen-genesis"])
        ["dune build gen_genesis.exe"]
      |> job_external_split
    in
    let job_oc_script_snapshot_alpha_and_link : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:snapshot_alpha_and_link"
        ~stage:Stages.test
        ~image:Images.runtime_build_dependencies
        ~dependencies:order_after_build
          (* Since the above dependencies are only for ordering, we do not set [dependent] *)
        ~rules:(make_rules ~changes:changeset_script_snapshot_alpha_and_link ())
        ~before_script:
          (before_script
             ~take_ownership:true
             ~source_version:true
             ~eval_opam:true
             [])
        ["./.gitlab/ci/jobs/test/script:snapshot_alpha_and_link.sh"]
      |> job_external_split
    in
    let job_oc_script_test_release_versions : tezos_job =
      job
        ~__POS__
        ~name:"oc.script:test_octez_release_versions"
        ~stage:Stages.test
        ~image:Images.runtime_build_dependencies
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
      |> job_external_split
    in
    let job_oc_script_b58_prefix =
      job
        ~__POS__
        ~name:"oc.script:b58_prefix"
        ~stage:Stages.test
          (* Requires Python. Can be changed to a python image, but using
             the build docker image to keep in sync with the python
             version used for the tests *)
        ~image:Images.runtime_build_test_dependencies
        ~rules:(make_rules ~changes:changeset_script_b58_prefix ())
        ~dependencies:dependencies_needs_trigger
        ~before_script:
          (before_script ~source_version:true ~init_python_venv:true [])
        [
          "poetry run pylint scripts/b58_prefix/b58_prefix.py \
           --disable=missing-docstring --disable=invalid-name";
          "poetry run pytest scripts/b58_prefix/test_b58_prefix.py";
        ]
      |> job_external_split
    in
    let job_oc_test_liquidity_baking_scripts : tezos_job =
      job
        ~__POS__
        ~name:"oc.test-liquidity-baking-scripts"
        ~stage:Stages.test
        ~image:Images.runtime_build_dependencies
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
      |> job_external_split
    in
    (* The set of installation test jobs *)
    let jobs_install_octez : tezos_job list =
      let changeset_install_jobs =
        Changeset.make
          ["docs/introduction/install*.sh"; "docs/introduction/compile*.sh"]
      in
      let install_octez_rules =
        make_rules ~changes:changeset_install_jobs ~manual:Yes ()
      in
      let job_install_bin ~__POS__ ~name ?allow_failure ?(rc = false)
          distribution =
        let distribution_string =
          match distribution with
          | Ubuntu_focal | Ubuntu_jammy -> "ubuntu"
          | Fedora_37 -> "fedora"
        in
        let script =
          sf "./docs/introduction/install-bin-%s.sh" distribution_string
          ^ if rc then " rc" else ""
        in
        job
          ?allow_failure
          ~__POS__
          ~name
          ~image:(image_of_distribution distribution)
          ~dependencies:dependencies_needs_trigger
          ~rules:install_octez_rules
          ~stage:Stages.test
          [script]
      in
      let job_install_opam_focal : tezos_job =
        job
          ~__POS__
          ~name:"oc.install_opam_focal"
          ~image:Images.opam_ubuntu_focal
          ~dependencies:dependencies_needs_trigger
          ~rules:(make_rules ~manual:Yes ())
          ~allow_failure:Yes
          ~stage:Stages.test
            (* The default behavior of opam is to use `nproc` to determine its level of
               parallelism. This returns the number of CPU of the "host" CI runner
               instead of the number of cores a single CI job can reasonably use. *)
          ~variables:[("OPAMJOBS", "4")]
          ["./docs/introduction/install-opam.sh"]
      in
      let job_compile_sources ~__POS__ ~name ~image ~project ~branch =
        job
          ~__POS__
          ~name
          ~image
          ~dependencies:dependencies_needs_trigger
          ~rules:install_octez_rules
          ~stage:Stages.test
          [sf "./docs/introduction/compile-sources.sh %s %s" project branch]
      in
      [
        (* Test installing binary / binary RC distributions in all distributions *)
        job_install_bin ~__POS__ ~name:"oc.install_bin_fedora_37" Fedora_37;
        job_install_bin
          ~__POS__
          ~name:"oc.install_bin_rc_fedora_37"
          ~rc:true
          Fedora_37;
        (* The Ubuntu jobs currently fail because the last rc packages can't be installed anymore.
           See https://gitlab.com/tezos/tezos/-/issues/6902.
           TODO: https://gitlab.com/tezos/tezos/-/issues/6915
           This should be removed after the next release candidate. *)
        job_install_bin
          ~__POS__
          ~name:"oc.install_bin_ubuntu_focal"
          ~allow_failure:Yes
          Ubuntu_focal;
        job_install_bin
          ~__POS__
          ~name:"oc.install_bin_ubuntu_jammy"
          ~allow_failure:Yes
          Ubuntu_jammy;
        job_install_bin
          ~__POS__
          ~name:"oc.install_bin_rc_ubuntu_focal"
          ~allow_failure:Yes
          ~rc:true
          Ubuntu_focal;
        job_install_bin
          ~__POS__
          ~name:"oc.install_bin_rc_ubuntu_jammy"
          ~allow_failure:Yes
          ~rc:true
          Ubuntu_jammy;
        (* Test installing through opam *)
        job_install_opam_focal;
        (* Test compiling the [latest-release] branch on Bullseye *)
        job_compile_sources
          ~__POS__
          ~name:"oc.compile_release_sources_bullseye"
          ~image:Images.opam_debian_bullseye
          ~project:"tezos/tezos"
          ~branch:"latest-release";
        (* Test compiling the [master] branch on Bullseye *)
        job_compile_sources
          ~__POS__
          ~name:"oc.compile_sources_bullseye"
          ~image:Images.opam_debian_bullseye
          ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
          ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}";
        job_compile_sources
          ~__POS__
          ~name:"oc.compile_sources_mantic"
          ~image:Images.opam_ubuntu_mantic
          ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
          ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}";
      ]
      |> jobs_external_split ~path:"test/install_octez"
    in
    (* Tezt jobs.

       The tezt jobs are split into a set of special-purpose jobs running the
       tests of the corresponding tag:
        - [tezt-memory-3k]: runs the jobs with tag [memory_3k],
        - [tezt-memory-4k]: runs the jobs with tag [memory_4k],
        - [tezt-time_sensitive]: runs the jobs with tag [time-sensitive],
        - [tezt-slow]: runs the jobs with tag [slow].
        - [tezt-flaky]: runs the jobs with tag [flaky] and
          none of the tags above.

       and a job [tezt] that runs all remaining tests (excepting those
       that are tagged [ci_disabled], that are disabled in the CI.)

       There is an implicit rule that the Tezt tags [memory_3k],
       [memory_4k], [time_sensitive], [slow] and [cloud] are mutually
       exclusive. The [flaky] tag is not exclusive to these tags. If
       e.g. a test has both tags [slow] and [flaky], it will run in
       [tezt-slow], to prevent flaky tests to run in the [tezt-flaky]
       job if they also have another special tag. Tests tagged [cloud] are
       meant to be used with Tezt cloud (see [tezt/lib_cloud/README.md]) and
       do not run in the CI.

       For more information on tags, see [src/lib_test/tag.mli]. *)
    let tezt_dependencies =
      Dependent
        [
          Artifacts job_select_tezts;
          Artifacts job_build_x86_64_release;
          Artifacts job_build_x86_64_exp_dev_extra;
          Artifacts job_build_kernels;
          Artifacts job_tezt_fetch_records;
        ]
    in
    let job_tezt_flaky : tezos_job =
      job_tezt
        ~__POS__
        ~name:"tezt-flaky"
        ~tezt_tests:(tezt_tests [Has_tag "flaky"])
        ~tezt_variant:"-flaky"
          (* To handle flakiness, consider tweaking [~tezt_parallel] (passed to
             Tezt's '--job-count'), and [~tezt_retry] (passed to Tezt's
             '--retry') *)
        ~retry:2
        ~tezt_retry:3
        ~tezt_parallel:1
        ~dependencies:tezt_dependencies
        ~rules:
          (* This job can only be manually triggered when it's
             artifact dependencies exists, which they do when
             [changeset_octez] is changed. *)
          (make_rules ~dependent:true ~manual:(On_changes changeset_octez) ())
        ()
      |> enable_coverage_output_artifact |> job_external_split
    in
    let job_tezt_slow : tezos_job =
      job_tezt
        ~__POS__
        ~name:"tezt-slow"
        ~rules:
          (* See comment for [job_tezt_flaky] *)
          (make_rules ~dependent:true ~manual:(On_changes changeset_octez) ())
        ~tezt_tests:
          (tezt_tests
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
        ~parallel:(Vector 10)
        ~dependencies:tezt_dependencies
        ()
      |> job_external_split
    in
    let jobs_tezt =
      let rules = make_rules ~dependent:true ~changes:changeset_octez () in
      let coverage_expiry = Duration (Days 3) in
      let tezt : tezos_job =
        job_tezt
          ~__POS__
          ~name:"tezt"
            (* Exclude all tests with tags in [tezt_tags_always_disable] or
               [tezt_tags_exclusive_tags]. *)
          ~tezt_tests:(tezt_tests [Not (Has_tag "flaky")])
          ~tezt_parallel:3
          ~parallel:(Vector 60)
          ~rules
          ~dependencies:tezt_dependencies
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_memory_4k : tezos_job =
        job_tezt
          ~__POS__
          ~name:"tezt-memory-4k"
          ~tezt_tests:(tezt_tests ~memory_4k:true [])
          ~tezt_variant:"-memory_4k"
          ~parallel:(Vector 4)
          ~dependencies:tezt_dependencies
          ~rules
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_memory_3k : tezos_job =
        job_tezt
          ~__POS__
          ~name:"tezt-memory-3k"
          ~tezt_tests:(tezt_tests ~memory_3k:true [])
          ~tezt_variant:"-memory_3k"
          ~dependencies:tezt_dependencies
          ~rules
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_time_sensitive : tezos_job =
        (* the following tests are executed with [~tezt_parallel:1] to ensure
           that other tests do not affect their executions. However, these
           tests are not particularly cpu/memory-intensive hence they do not
           need to run on a particular machine contrary to performance
           regression tests. *)
        job_tezt
          ~__POS__
          ~name:"tezt-time-sensitive"
          ~tezt_tests:(tezt_tests ~time_sensitive:true [])
          ~tezt_variant:"-time_sensitive"
          ~dependencies:tezt_dependencies
          ~rules
          ()
        |> enable_coverage_output_artifact ~expire_in:coverage_expiry
      in
      let tezt_static_binaries : tezos_job =
        job_tezt
          ~__POS__
          ~tags:["gcp"]
          ~name:"tezt:static-binaries"
          ~tezt_tests:(tezt_tests [Has_tag "cli"; Not (Has_tag "flaky")])
          ~tezt_parallel:3
          ~retry:0
          ~dependencies:
            (Dependent
               [
                 Artifacts job_select_tezts;
                 Artifacts job_build_x86_64_exp_dev_extra;
                 Artifacts job_static_x86_64_experimental;
                 Artifacts job_tezt_fetch_records;
               ])
          ~rules
          ~before_script:(before_script ["mv octez-binaries/x86_64/octez-* ."])
          ()
      in
      [
        tezt;
        tezt_memory_4k;
        tezt_memory_3k;
        tezt_time_sensitive;
        tezt_static_binaries;
      ]
      |> jobs_external_split ~path:"test/tezt"
    in
    let jobs_kernels : tezos_job list =
      let make_job_kernel ~__POS__ ~name ~changes script =
        job
          ~__POS__
          ~name
          ~image:Images.rust_toolchain
          ~stage:Stages.test
          ~dependencies:(Dependent [Artifacts job_docker_rust_toolchain])
          ~rules:(make_rules ~dependent:true ~changes ())
          script
          ~cache:[cache_kernels]
        |> enable_kernels
      in
      let job_test_kernels : tezos_job =
        make_job_kernel
          ~__POS__
          ~name:"test_kernels"
          ~changes:changeset_test_kernels
          ["make -f kernels.mk check"; "make -f kernels.mk test"]
        |> job_external_split
      in
      [job_test_kernels]
    in
    [
      job_kaitai_checks;
      job_kaitai_e2e_checks;
      job_oc_check_lift_limits_patch;
      job_oc_misc_checks;
      job_misc_opam_checks;
      job_semgrep;
      job_oc_integration_compiler_rejections;
      job_oc_script_test_gen_genesis;
      job_oc_script_snapshot_alpha_and_link;
      job_oc_script_test_release_versions;
      job_oc_script_b58_prefix;
      job_oc_test_liquidity_baking_scripts;
      job_tezt_flaky;
      job_tezt_slow;
    ]
    @ jobs_kernels @ jobs_unit @ jobs_install_octez @ jobs_tezt
    @
    match pipeline_type with
    | Before_merging ->
        let job_commit_titles : tezos_job =
          job
            ~__POS__
            ~name:"commit_titles"
            ~image:Images.runtime_prebuild_dependencies
            ~stage:Stages.test
            ~dependencies:dependencies_needs_trigger
            (* ./scripts/ci/check_commit_messages.sh exits with code 65 when a git history contains
               invalid commits titles in situations where that is allowed. *)
            (script_propagate_exit_code "./scripts/ci/check_commit_messages.sh")
            ~allow_failure:(With_exit_codes [65])
          |> job_external
        in
        [job_commit_titles]
    | Schedule_extended_test -> []
  in
  let doc = [] in
  let manual =
    match pipeline_type with
    | Before_merging ->
        (* Note: manual jobs in stage [manual] (which is the final
           stage) in [Before_merging] pipelines should be [Dependent]
           by default, and in particular [Dependent []] if they have
           no need for artifacts from other jobs. Making these
           dependent on [job_trigger] is redundant since they are
           already manual, and what's more, puts the pipeline in a
           confusing "pending state" with a yellow "pause" icon on the
           [manual] stage. *)
        let job_docker_amd64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~external_:true
            ~arch:Amd64
            ~dependencies:(Dependent [])
            Test_manual
        in
        let job_docker_arm64_test_manual : Tezos_ci.tezos_job =
          job_docker_build
            ~__POS__
            ~external_:true
            ~arch:Arm64
            ~dependencies:(Dependent [])
            Test_manual
        in
        let job_build_dpkg_amd64_manual =
          job_build_bin_package
            ~__POS__
            ~name:"oc.build:dpkg:amd64"
            ~target:Dpkg
            ~arch:Tezos_ci.Amd64
            ~rules:[job_rule ~when_:Manual ()]
            ~dependencies:(Dependent [])
            ~stage:Stages.manual
            ()
          |> job_external ~directory:"build" ~filename_suffix:"manual"
        in
        let job_build_rpm_amd64_manual =
          job_build_bin_package
            ~__POS__
            ~name:"oc.build:rpm:amd64"
            ~target:Rpm
            ~arch:Tezos_ci.Amd64
            ~rules:[job_rule ~when_:Manual ()]
            ~dependencies:(Dependent [])
            ~stage:Stages.manual
            ()
          |> job_external ~directory:"build" ~filename_suffix:"manual"
        in
        [
          job_docker_amd64_test_manual;
          job_docker_arm64_test_manual;
          job_build_dpkg_amd64_manual;
          job_build_rpm_amd64_manual;
        ]
    (* No manual jobs on the scheduled pipeline *)
    | Schedule_extended_test -> []
  in
  (* Empty placeholder: this has the effect of not overwriting the pipeline file in question.
     Once all the jobs in these pipelines are defined, we will return them here which
     will cause the pipeline files to contain the definition of all those jobs.

     Until that time, all the jobs are written ot external files
     (using {!job_external} or {!jobs_external}) and included by hand
     in the files [.gitlab/ci/pipelines/before_merging.yml] and
     [.gitlab/ci/pipelines/schedule_extended_test.yml]. *)
  ignore (trigger_stage @ sanity @ build @ packaging @ test @ doc @ manual) ;
  []
