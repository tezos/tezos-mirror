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
    (* /!\ GCP_REGISTRY is the variable containing the name of the registry to and from
       which docker images are produced and consumed. This variable is defined at tezos
       level with the value unprotected registry and at tezos/tezos level in its protected
       version. This mechanism allows pipelines from a protected tezos/tezos branch to
       read the protected variable from tezos/tezos and for others to not have access to
       the variable tezos/tezos but tezos. *)
    ( "rust_toolchain_image_name",
      "${GCP_REGISTRY}/${CI_PROJECT_PATH}/rust-toolchain" );
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
  (* Matches Octez release tags, e.g. [octez-v1.2.3] or [octez-v1.2.3-rc4]. *)
  let octez_release_tag_re = "/^octez-v\\d+\\.\\d+(?:\\-rc\\d+)?$/" in
  (* Matches Octez beta release tags, e.g. [octez-v1.2.3-beta5]. *)
  let octez_beta_release_tag_re = "/^octez-v\\d+\\.\\d+\\-beta\\d*$/" in
  (* Matches Etherlink release tags, e.g. [etherlink-v1.2.3] or [etherlink-v1.2.3-rc4]. *)
  let octez_evm_node_release_tag_re =
    "/^octez-evm-node-v\\d+\\.\\d+(?:\\-rc\\d+)?$/"
  in
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
      && not (has_tag_match octez_evm_node_release_tag_re))
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

let () =
  let open Pipeline in
  let open Rules in
  register
    "schedule_extended_test"
    schedule_extended_tests
    ~jobs:(Code_verification.jobs Schedule_extended_test)
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
    ~jobs:Custom_extended_test_pipeline.jobs
    ~description:
      "Scheduled run of all tezt tests with external RPC servers, weekly on \
       'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use external RPC servers." ;
  register
    "schedule_extended_validation_test"
    schedule_extended_validation_tests
    ~jobs:Custom_extended_test_pipeline.jobs
    ~description:
      "Scheduled run of all tezt tests with single-process validation, weekly \
       on 'master'.\n\n\
       This scheduled pipeline exercices the full tezt tests suites, but with \
       Octez nodes configured to use single-process validation."

(** {2 Entry point of the generator binary} *)

let () =
  (* If argument --verbose is set, then log generation info.
     If argument --inline-source, then print generation info in yml files. *)
  let filename = ".gitlab-ci.yml" in
  match Cli.config.action with
  | Write ->
      Pipeline.write ~default ~variables ~filename () ;
      Tezos_ci.check_files ~remove_extra_files:Cli.config.remove_extra_files ()
  | List_pipelines -> Pipeline.list_pipelines ()
