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

(* Register pipelines types. Pipelines types are used to generate
   workflow rules and includes of the files where the jobs of the
   pipeline is defined. *)
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
     e.g. [octez-v1.2.3], [octez-v1.2.3-rc4] or [octez-v1.2.3-beta5]. *)
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
  register
    "before_merging"
    If.(on_tezos_namespace && merge_request && not merge_train)
    ~jobs:(Code_verification.jobs Before_merging) ;
  (* The [merge_train] pipeline has the exact same jobs as the
     [Before_merging] pipeline, but it auto-cancels on job failure. *)
  register
    "merge_train"
    ~auto_cancel:{on_job_failure = true; on_new_commit = false}
    If.(on_tezos_namespace && merge_request && merge_train)
    ~jobs:(Code_verification.jobs Before_merging) ;
  register
    "octez_latest_release"
    ~jobs:(Octez_latest_release.jobs ())
    If.(on_tezos_namespace && push && on_branch "latest-release") ;
  register
    "octez_latest_release_test"
    If.(not_on_tezos_namespace && push && on_branch "latest-release-test")
    ~jobs:(Octez_latest_release.jobs ~test:true ()) ;
  register
    "master_branch"
    If.(on_tezos_namespace && push && on_branch "master")
    ~jobs:Master_branch.jobs ;
  register
    "octez_release_tag"
    If.(on_tezos_namespace && push && has_tag_match octez_release_tag_re)
    ~jobs:(Release_tag.octez_jobs Release_tag) ;
  register
    "octez_beta_release_tag"
    If.(on_tezos_namespace && push && has_tag_match octez_beta_release_tag_re)
    ~jobs:(Release_tag.octez_jobs Beta_release_tag) ;
  register
    "octez_release_tag_test"
    If.(not_on_tezos_namespace && push && has_any_octez_release_tag)
    ~jobs:(Release_tag.octez_jobs ~test:true Release_tag) ;
  (* To test this type of release, push a tag to a fork of [tezos/tezos]
     e.g. [nomadic-labs/tezos] project. *)
  register
    "octez_evm_node_release_tag"
    If.(push && has_tag_match octez_evm_node_release_tag_re)
    ~jobs:(Release_tag.octez_evm_node_jobs ()) ;
  register
    "non_release_tag"
    If.(on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.octez_jobs Non_release_tag) ;
  register
    "non_release_tag_test"
    If.(not_on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.octez_jobs ~test:true Non_release_tag) ;
  register
    "schedule_extended_test"
    schedule_extended_tests
    ~jobs:(Code_verification.jobs Schedule_extended_test) ;
  register
    "schedule_extended_rpc_test"
    schedule_extended_rpc_tests
    ~jobs:Custom_extended_test_pipeline.jobs ;
  register
    "schedule_extended_validation_test"
    schedule_extended_validation_tests
    ~jobs:Custom_extended_test_pipeline.jobs

let () =
  (* If argument --verbose is set, then log generation info.
     If argument --inline-source, then print generation info in yml files. *)
  let filename = ".gitlab-ci.yml" in
  match Cli.config.action with
  | Write ->
      Pipeline.write ~default ~variables ~filename () ;
      Tezos_ci.check_files ~remove_extra_files:Cli.config.remove_extra_files ()
  | List_pipelines -> Pipeline.list_pipelines ()
