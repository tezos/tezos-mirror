(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint of CI-in-OCaml.

   Here we register the set of pipelines, stages and images and
   generate the GitLab CI configuration file. *)

open Gitlab_ci
open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

let () = Tezos_ci.Cli.init ()

(* Sets up the [default:] top-level configuration element. *)
let default = default ~interruptible:true ()

(* Top-level [variables:] *)
let variables : variables =
  [
    (* /!\ [GCP_PUBLIC_REGISTRY] contains the name of the PUBLIC
       registry to and from which Docker images are produced and
       consumed. This variable is defined at the tezos-group level and
       always contains the path to the unprotected Docker registry
       (unlike [GCP_REGISTRY], see below). This is used to locate the
       CI images, which are always pushed to the public repository. *)
    ("ci_image_name", "${GCP_REGISTRY}/${CI_PROJECT_PATH}/ci");
    ( "ci_image_name_protected",
      "${GCP_PROTECTED_REGISTRY}/${CI_PROJECT_PATH}/ci" );
    (* /!\ GCP_REGISTRY is the variable containing the name of the registry to and from
       which docker images are produced and consumed. This variable is defined at tezos
       level with the value unprotected registry and at tezos/tezos level in its protected
       version. This mechanism allows pipelines from a protected tezos/tezos branch to
       read the protected variable from tezos/tezos and for others to not have access to
       the variable tezos/tezos but tezos. *)
    ( "rust_toolchain_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/rust-toolchain" );
    ( "rust_sdk_bindings_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/rust-sdk-bindings" );
    ("jsonnet_image_name", "${GCP_REGISTRY}/${CI_PROJECT_PATH}/jsonnet");
    ( "client_libs_dependencies_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/client-libs-dependencies" );
    ("GIT_STRATEGY", "fetch");
    ("GIT_DEPTH", "1");
    ("GET_SOURCES_ATTEMPTS", "2");
    ("ARTIFACT_DOWNLOAD_ATTEMPTS", "2");
    (* Sets the number of tries before failing opam downloads. *)
    ("OPAMRETRIES", "5");
    (* An addition to working around a bug in gitlab-runner's default
       unzipping implementation
       (https://gitlab.com/gitlab-org/gitlab-runner/-/issues/27496),
       this setting cuts cache creation time. *)
    ("FF_USE_FASTZIP", "true");
    (* If RUNTEZTALIAS is true, then Tezt tests are included in the
       @runtest alias. We set it to false to deactivate these tests in
       the unit test jobs, as they already run in the Tezt jobs. It is
       set to true in the opam jobs where we want to run the tests
       --with-test. *)
    ("RUNTEZTALIAS", "false");
    ("CARGO_HOME", Common.cargo_home);
    (* To avoid Cargo accessing the network in jobs without caching (see
       {!Common.enable_cargo_cache}), we turn of net access by default. *)
    ("CARGO_NET_OFFLINE", "true");
    (* Reduce the verbosity of Cargo. *)
    ("CARGO_TERM_QUIET", "true");
    (* Enable timestamps for each line in job logs.

       https://docs.gitlab.com/ee/ci/yaml/ci_job_log_timestamps.html *)
    ("FF_TIMESTAMPS", "true");
  ]

(** {2 Pipeline types} *)

(* Register pipelines types. Pipelines types are used to generate
   workflow rules and includes of the files where the jobs of the
   pipeline is defined.

   Please add a [~description] to each pipeline.

   The first sentence of the description should be short (<=80
   characters), and be terminated by two new-lines. It should
   describe _what_ the pipeline does.

   The remainder of the description should detail:
   - what the pipeline does;
   - why we do it;
   - when it happens;
   - how;
   - and by whom it is triggered (a developer? a release manager? some automated system?). *)

(** {3 General pipelines} *)

let () =
  let open Rules in
  let open Pipeline in
  register
    "before_merging"
    If.(on_tezos_namespace && merge_request && not merge_train)
    ~jobs:(Code_verification.jobs Before_merging)
    ~description:
      "Lints code in merge requests, checks that it compiles and runs tests.\n\n\
       This pipeline is created on each push to a branch with an associated \
       open merge request, typically by the developer. It runs sanity checks, \
       linters and checks that code of the MR compiles and that the tests \
       pass. Must be manually started through the job 'trigger'." ;
  register
    "merge_train"
    ~auto_cancel:{on_job_failure = true; on_new_commit = false}
    If.(on_tezos_namespace && merge_request && merge_train)
    ~jobs:(Code_verification.jobs Merge_train)
    ~description:
      "A merge-train-specific version of 'before_merging'.\n\n\
       This pipeline contains the same set of jobs as 'before_merging' but \
       with auto-cancelling enabled on job failures. That is, if one job in \
       the pipeline fails, the full pipeline is cancelled. This ensures that \
       pipelines running in a merge train, that are bound to fail (due to some \
       failing job), does so as early as possible. This prevents unneccessary \
       delays in merging MRs further down the train.\n\n\
       The merge train pipeline is created by GitLab when a merge request is \
       added to the merge train (typically by marge-bot)." ;
  register
    "master_branch"
    If.(on_tezos_namespace && push && on_branch "master")
    ~jobs:Master_branch.jobs
    ~description:
      "Publishes artifacts (docs, static binaries) from master on each merge.\n\n\
       This pipeline publishes the documentation at tezos.gitlab.io, builds \
       static binaries, and the 'master' tag of the Octez Docker distribution. \
       This pipeline is created automatically by GitLab on each push, \
       typically resulting from the merge of a merge request, to the 'master' \
       branch on tezos/tezos."

(** {3 Release pipelines} *)

let () =
  (* Matches Octez release tags, e.g. [octez-v1.2] or [octez-v1.2-rc4]. *)
  let octez_release_tag_re = "/^octez-v\\d+\\.\\d+(?:\\-rc\\d+)?$/" in
  (* Matches Octez beta release tags, e.g. [octez-v1.2-beta5]. *)
  let octez_beta_release_tag_re = "/^octez-v\\d+\\.\\d+\\-beta\\d*$/" in
  (* Matches Etherlink release tags, e.g. [etherlink-v1.2] or [etherlink-v1.2-rc4]. *)
  let octez_evm_node_release_tag_re =
    "/^octez-evm-node-v\\d+\\.\\d+(?:\\-rc\\d+)?$/"
  in
  (* Matches Grafazos release tags, e.g. [grafazos-v1.2]. *)
  let grafazos_release_tag_re = "/^grafazos-v\\d+\\.\\d+$/" in
  (* Matches Teztale release tags, e.g. [teztale-v1.2]. *)
  let teztale_release_tag_re = "/^teztale-v\\d+\\.\\d+$/" in
  let open Rules in
  let open Pipeline in
  (* Matches either Octez release tags or Octez beta release tags,
     e.g. [octez-v1.2], [octez-v1.2-rc4] or [octez-v1.2-beta5]. *)
  let has_any_octez_release_tag =
    If.(
      has_tag_match octez_release_tag_re
      || has_tag_match octez_beta_release_tag_re)
  in
  let has_non_release_tag =
    If.(
      Predefined_vars.ci_commit_tag != null
      && (not has_any_octez_release_tag)
      && (not (has_tag_match octez_evm_node_release_tag_re))
      && (not (has_tag_match grafazos_release_tag_re))
      && not (has_tag_match teztale_release_tag_re))
  in
  let release_description =
    "\n\n\
     For more information on Octez' release system, see: \
     https://tezos.gitlab.io/releases/releases.html"
  in
  (* TODO: rename 'octez_docker_latest_release' ?? *)
  register
    "octez_latest_release"
    ~jobs:(Octez_latest_release.jobs ())
    If.(on_tezos_namespace && push && on_branch "latest-release")
    ~description:
      ("Updates 'latest' tag of the Octez Docker distribution on Docker Hub.\n\n\
        This pipeline is created on each push to the 'latest-release' branch \
        of 'tezos/tezos', typically performed by the release manager. On each \
        release, the 'latest-release' branch is updated to point to the git \
        tag of the release. This resulting pipeline then updates the Docker \
        tag 'latest' of the Octez Docker distribution published to Docker hub \
        (https://hub.docker.com/r/tezos/tezos) to point to the Docker release \
        associated with the git tag pushed to the 'latest-release' branch."
     ^ release_description) ;
  register
    "octez_latest_release_test"
    If.(not_on_tezos_namespace && push && on_branch "latest-release-test")
    ~jobs:(Octez_latest_release.jobs ~test:true ())
    ~description:
      "Dry-run pipeline for 'octez_latest_release' pipelines.\n\n\
       This pipeline is used to dry run the 'octez_latest_release' pipeline, \
       checking that it works as intended, without updating any Docker tags. \
       Developers or release managers trigger it manually by pushing to the \
       branch 'latest-release-test' of a fork of 'tezos/tezos', e.g. to the \
       'nomadic-labs/tezos' project." ;
  (* TODO: simplify dry run pipelines by having them all be on tezos/tezos? *)
  register
    "octez_release_tag"
    If.(on_tezos_namespace && push && has_tag_match octez_release_tag_re)
    ~jobs:(Release_tag.octez_jobs Release_tag)
    ~description:
      ("Release tag pipelines for Octez.\n\n\
        This pipeline is created when the release manager pushes a tag in the \
        format octez-vX.Y(-rcN). It creates and publishes release on GitLab \
        with the associated artifacts (static binaries, Docker images, .deb \
        packages, etc.). It also prepares for a new opam package release by \
        updating https://github.com/tezos/opam-repository."
     ^ release_description) ;
  register
    "octez_beta_release_tag"
    If.(on_tezos_namespace && push && has_tag_match octez_beta_release_tag_re)
    ~jobs:(Release_tag.octez_jobs Beta_release_tag)
    ~description:
      ("Beta release tag pipelines for Octez.\n\n\
        This pipeline is created when the release manager pushes a tag in the \
        format octez-vX.Y(-betaN). It is as Octez release tag pipelines, but \
        does not publish to opam." ^ release_description) ;
  register
    "octez_release_tag_test"
    If.(not_on_tezos_namespace && push && has_any_octez_release_tag)
    ~jobs:(Release_tag.octez_jobs ~test:true Release_tag)
    ~description:
      "Dry-run pipeline for 'octez_release_tag'.\n\n\
       This pipeline checks that 'octez_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project." ;
  (* TODO: We should be able to register this pipeline in [grafazos/ci]. *)
  register
    "grafazos_release_tag_test"
    If.(not_on_tezos_namespace && push && has_tag_match grafazos_release_tag_re)
    ~jobs:(Grafazos_ci.Release.jobs ~test:true ())
    ~description:
      "Test release pipeline for Grafazos.\n\n\
       This pipeline checks that 'grafazos_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project." ;
  (* TODO: We should be able to register this pipeline in [teztale/ci]. *)
  register
    "teztale_release_tag_test"
    If.(not_on_tezos_namespace && push && has_tag_match teztale_release_tag_re)
    ~jobs:(Teztale.Release.jobs ~test:true ())
    ~description:
      "Test release pipeline for Teztale.\n\n\
       This pipeline checks that 'teztale_release_tag' pipelines work as \
       intended, without publishing any release. Developers or release \
       managers can create this pipeline by pushing a tag to a fork of \
       'tezos/tezos', e.g. to the 'nomadic-labs/tezos' project." ;
  register
    "octez_evm_node_release_tag"
    If.(push && has_tag_match octez_evm_node_release_tag_re)
    ~jobs:(Release_tag.octez_evm_node_jobs ())
    ~description:
      ("Release tag pipelines for Etherlink.\n\n\
        Created when the release manager pushes a tag in the format \
        octez-evm-node-vX.Y(-rcN). Creates and publishes a release on GitLab \
        with associated etherlink artifacts (static binaries and Docker \
        image)." ^ release_description) ;
  register
    "non_release_tag"
    If.(on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.octez_jobs Non_release_tag)
    ~description:
      ("Tag pipeline for non-release tags.\n\n\
        Created on each push of a tag that does not match e.g. \
        octez(-evm-node)-vX.Y(-rcN). This pipeline creates a release on GitLab \
        and associated artifacts, like 'octez_release_tag' pipelines, but does \
        not publish it." ^ release_description) ;
  register
    "non_release_tag_test"
    If.(not_on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.octez_jobs ~test:true Non_release_tag)
    ~description:
      "Dry-run pipeline for 'non_release_tag'.\n\n\
       This pipeline checks that 'non_release_tag' pipelines work as intended, \
       without publishing any release. Developers, or release managers, can \
       create this pipeline by pushing a tag to a fork of 'tezos/tezos', e.g. \
       to the 'nomadic-labs/tezos' project."

(** {3 Scheduled pipelines} *)

(* All jobs in scheduled pipelines have "interruptible: false"
   to prevent them from being canceled after a push to master.
   Instead of modifying the definition of each job, we override the value
   with [with_interruptible]. *)
let () =
  let open Pipeline in
  let open Rules in
  register
    "schedule_extended_test"
    schedule_extended_tests
    ~jobs:
      (Code_verification.jobs Schedule_extended_test
      |> List.map (with_interruptible false))
    ~description:
      "Scheduled, full version of 'before_merging', daily on 'master'.\n\n\
       This pipeline unconditionally executes all jobs in 'before_merging' \
       pipelines, daily on the 'master_branch'. Regular 'before_merging' \
       pipelines run only subset of all jobs depending on files modified by \
       the MR. This \"safety net\"-pipeline ensures that all jobs run at least \
       daily." ;
  register
    "schedule_extended_rpc_test"
    schedule_extended_rpc_tests
    ~jobs:
      (Custom_extended_test_pipeline.jobs |> List.map (with_interruptible false))
    ~description:
      "Scheduled run of all tezt tests with external RPC servers, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use external RPC servers." ;
  register
    "schedule_extended_validation_test"
    schedule_extended_validation_tests
    ~jobs:
      (Custom_extended_test_pipeline.jobs |> List.map (with_interruptible false))
    ~description:
      "Scheduled run of all tezt tests with single-process validation, weekly \
       on 'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use single-process validation." ;
  register
    "schedule_extended_baker_remote_mode_test"
    schedule_extended_baker_remote_mode_tests
    ~jobs:
      (Custom_extended_test_pipeline.jobs |> List.map (with_interruptible false))
    ~description:
      "Scheduled run of all tezt tests with baker using remote node, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites." ;
  register
    "schedule_master_test_release"
    schedule_test_release
    ~jobs:(Release_tag.octez_jobs ~test:true Schedule_test)
    ~description:
      "Scheduled pipeline that runs a test release pipeline for master. The \
       jobs are the same as a release pipeline but run in dry-mode." ;
  register
    "schedule_container_scanning_master"
    schedule_container_scanning_master
    ~jobs:(Container_scanning.jobs "tezos/tezos:master")
    ~description:
      "Scheduled pipeline for scanning vulnerabilities in tezos/tezos:master \
       Docker image" ;
  register
    "schedule_container_scanning_octez_releases"
    schedule_container_scanning_octez_releases
    ~jobs:(Container_scanning.jobs "tezos/tezos:latest")
    ~description:
      "Scheduled pipeline for scanning vulnerabilities in tezos/tezos:latest \
       Docker image" ;
  register
    "schedule_container_scanning_evm_node_releases"
    schedule_container_scanning_evm_node_releases
    ~jobs:(Container_scanning.jobs "tezos/tezos:octez-evm-node-v0.27")
    ~description:
      "Scheduled pipeline for scanning vulnerabilities in latest \
       tezos/tezos:octez-evm-node-vX.Y Docker image" ;
  register
    "schedule_container_scanning_octez_rc"
    schedule_container_scanning_octez_rc
    ~jobs:(Container_scanning.jobs "tezos/tezos:octez-v22.0-rc3")
    ~description:
      "Scheduled pipeline for scanning vulnerabilities in the Docker image for \
       the latest release candidate of Octez" ;
  register
    "schedule_documentation"
    schedule_documentation
    ~jobs:
      (Common.job_datadog_pipeline_trace :: Master_branch.job_static_x86_64
       :: Master_branch.jobs_documentation
      |> List.map (with_interruptible false))
    ~description:
      "Scheduled pipeline that updates the octez.com/docs documentation \
       without being interrupted."

(** {Manual pipelines} *)

let () =
  let open Pipeline in
  let open Rules in
  register
    "publish_test_release_page"
    If.(api_release_page && not_on_tezos_namespace)
    ~jobs:[Release_tag.job_release_page ~test:true ()]
    ~description:"Pipeline that updates and publishes the test release page." ;
  register
    "publish_release_page"
    If.(api_release_page && on_tezos_namespace)
    ~jobs:
      [
        Common.job_datadog_pipeline_trace;
        Release_tag.job_release_page ~test:false ();
      ]
    ~description:"Pipeline that updates and publishes the release page."

(** {2 Entry point of the generator binary} *)

let () =
  (* If argument --verbose is set, then log generation info.
     If argument --inline-source, then print generation info in yml files. *)
  match Cli.config.action with
  | Write ->
      Pipeline.write ~default ~variables ~filename:".gitlab-ci.yml" () ;
      Tezos_ci.check_files ~remove_extra_files:Cli.config.remove_extra_files ()
  | List_pipelines -> Pipeline.list_pipelines ()
  | Overview_pipelines -> Pipeline.overview_pipelines ()
  | Describe_pipeline {name} -> Pipeline.describe_pipeline name
