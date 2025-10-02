(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Util
open Tezos_ci

(** Create a tezt job.

      To enable tezt selection via manifest, pass a job as constructed
      by {!job_select_tezts} as [~job_select_tezts]. Then the
      constructed tezt job will only run the tezts selected by the
      selection job. *)
let job ~__POS__ ?rules ?parallel ?(tag = Runner.Tag.Gcp_tezt) ~variant
    ~(tezt_tests : Tezt_core.TSL_AST.t) ?(retry = 2) ?(tezt_retry = 1)
    ?(tezt_parallel = 1)
    ?(before_script =
      Common.before_script ~source_version:true ~eval_opam:false []) ?timeout
    ?(disable_test_timeout = false) ?job_select_tezts ~dependencies
    ?allow_failure ?(keep_going = false) () : tezos_job =
  let variables =
    [
      ("JUNIT", "tezt-junit.xml");
      ("TEZT_VARIANT", if variant = "" then "" else "-" ^ variant);
      ("TESTS", Tezt_core.TSL.show tezt_tests);
      ("TEZT_RETRY", string_of_int tezt_retry);
      ("TEZT_PARALLEL", string_of_int tezt_parallel);
      ("TEZT_NO_NPX", "true");
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
           the days of retention (7 atm), [P] the number of pipelines
           per day (~200 atm), [T] the number of Tezt jobs per pipeline
           (100) and [W] the artifact size per tezt job (2MB). This
           makes 280GB which is ~4% of our
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
  let with_or_without_select_tezts, dependencies =
    match job_select_tezts with
    | Some job_select_tezts ->
        let dependencies =
          Tezos_ci.dependencies_add_artifact_dependency
            dependencies
            job_select_tezts
        in
        ("--with-select-tezts", dependencies)
    | None -> ("--without-select-tezts", dependencies)
  in
  let retry =
    if retry = 0 then None else Some {Gitlab_ci.Types.max = retry; when_ = []}
  in
  let junit_tags =
    (* List of tags to include in JUnit reports that we send to DataDog. *)
    [
      (* Tags that change the job that runs the test.
           Useful to be able to filter on only a specific job type in DataDog. *)
      "flaky";
      "time_sensitive";
      "slow";
      "extra";
      (* Tags that denote the owner of tests.
           Useful in case we want to send alerts to specific teams. *)
      "infrastructure";
      "layer1";
      "tezos2";
      "etherlink";
      (* Tags that change alert thresholds. *)
      "memory_hungry";
    ]
  in
  let junit_tags =
    junit_tags
    |> List.map (fun tag ->
           Printf.sprintf " --junit-tag 'dd_tags[tezt-tag.%s]=%s'" tag tag)
    |> String.concat ""
  in
  let keep_going_opt = if keep_going then " --keep-going" else "" in
  job
    ?timeout
    ~__POS__
    ~image:Images.CI.e2etest
    ~name:(if variant = "" then "tezt" else "tezt-" ^ variant)
    ?parallel
    ~tag
    ~stage:Stages.test
    ?rules
    ~artifacts
    ~variables
    ~dependencies
    ?retry
    ~before_script
    ?allow_failure
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
      "./scripts/ci/tezt.sh " ^ with_or_without_select_tezts
      ^ " \"${TESTS}\" --from-record tezt/records"
      ^ (if variant = "" then "" else "/" ^ variant)
      ^ " --job ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --list-tsv > \
         selected_tezts.tsv";
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
      (* To observe memory usage of tests, we use the following options:
           - --record-mem-peak causes Tezt to measure memory usage
             (it is implied by --mem-warn so we could omit it);
           - --junit-mem-peak tells Tezt to store peak memory usage
             in a <property> named dd_tags[memory.peak] in JUnit reports,
             which makes DataDog aware of it
             (see https://docs.datadoghq.com/tests/setup/junit_xml/?tab=linux#providing-metadata-through-property-elements);
           - --mem-warn causes Tezt to warn if a test uses more than the specified
             amount of memory (in bytes). We set the threshold to 5 GB. *)
      "./scripts/ci/exit_code.sh timeout -k 60 1860 ./scripts/ci/tezt.sh \
       --send-junit " ^ with_or_without_select_tezts
      ^ " \"${TESTS}\" --color --log-buffer-size 5000 --log-file tezt.log \
         --global-timeout 1800"
      ^ (if disable_test_timeout then "" else " --test-timeout 540")
      ^ " --on-unknown-regression-files fail --junit ${JUNIT} --junit-mem-peak \
         'dd_tags[memory.peak]' --from-record tezt/records"
      ^ (if variant = "" then "" else "/" ^ variant)
      ^ " --job ${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1} --record \
         tezt-results-${CI_NODE_INDEX:-1}${TEZT_VARIANT}.json --job-count \
         ${TEZT_PARALLEL} --retry ${TEZT_RETRY} --record-mem-peak --mem-warn \
         5_000_000_000" ^ keep_going_opt ^ junit_tags;
    ]

(** Tezt tag selector string.

    It returns a TSL expression that:
    - always deselects tags with [ci_disabled];
    - selects, respectively deselects, the tests with the tags
      [time_sensitive], [slow], [extra] or [cloud],
      depending on the value of the corresponding function argument.
      These arguments all default to false.

    See [src/lib_test/tag.mli] for a description of the above tags.

    The list of TSL expressions [and_] are appended to the final
    selector, allowing to modify the selection further. *)
let tests_tag_selector ?(time_sensitive = false) ?(slow = false)
    ?(extra = false) ?(cloud = false) (and_ : Tezt_core.TSL_AST.t list) :
    Tezt_core.TSL_AST.t =
  let tags =
    [
      (false, "ci_disabled");
      (time_sensitive, "time_sensitive");
      (slow, "slow");
      (extra, "extra");
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

(** Selects tezt tests based on merge request diff.

      This job only makes sense in merge request pipelines. To enable
      tezt selection, it should be passed to the [~job_select_tezts]
      argument to {!Tezt.job}. *)
let job_select_tezts ?rules () : tezos_job =
  Tezos_ci.job
    ~__POS__
    ~name:"select_tezts"
      (* We need:
           - Git (to run git diff)
           - ocamlyacc, ocamllex and ocamlc (to build manifest/manifest) *)
    ?rules
    ~image:Images.CI.prebuild
    ~stage:Stages.build
    ~before_script:
      (Common.before_script ~take_ownership:true ~eval_opam:true [])
    (script_propagate_exit_code "scripts/ci/select_tezts.sh")
    ~allow_failure:(With_exit_codes [17])
    ~artifacts:
      (artifacts
         ~expire_in:(Duration (Days 3))
         ~when_:Always
         ["selected_tezts.tsl"])

(* Fetch records for Tezt generated on the last merge request
     pipeline on the most recently merged MR and makes them available
     in artifacts for future merge request pipelines. *)
let job_tezt_fetch_records ?rules () : tezos_job =
  Tezos_ci.job
    ~__POS__
    ~name:"oc.tezt:fetch-records"
    ~image:Images.CI.build
    ~stage:Stages.build
    ~before_script:
      (Common.before_script
         ~take_ownership:true
         ~source_version:true
         ~eval_opam:true
         [])
    ?rules
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
