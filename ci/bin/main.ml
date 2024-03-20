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
    (* /!\ CI_REGISTRY is overriden to use a private Docker registry mirror in AWS ECR
       in GitLab namespaces `nomadic-labs` and `tezos`
       /!\ This value MUST be the same as `opam_repository_tag` in `scripts/version.sh` *)
    ("build_deps_image_version", Common.build_deps_image_version);
    ("build_deps_image_name", "${GCP_REGISTRY}/tezos/opam-repository");
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
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6764
       "false" is the GitLab default but we've overridden it in the runner settings.
       This should be fixed at the runner level but we reset it to the
       default here in the meantime. *)
    ("FF_KUBERNETES_HONOR_ENTRYPOINT", "false");
  ]

(* Dummy job.

   This fixes the "configuration must contain at least one
   visible job" error in GitLab when using includes.

   For more info, see: https://gitlab.com/gitlab-org/gitlab/-/issues/341693 *)
let job_dummy : job =
  Util.job
    ~rules:
      [job_rule ~if_:If.(var "foo" == str "bar" && var "foo" != str "bar") ()]
    ~name:"dummy_job"
    ~script:[{|echo "This job will never execute"|}]
    ()

(* Register pipelines types. Pipelines types are used to generate
   workflow rules and includes of the files where the jobs of the
   pipeline is defined. At the moment, all these pipelines are defined
   manually in .yml, but will eventually be generated. *)
let () =
  (* Matches release tags, e.g. [v1.2.3] or [v1.2.3-rc4]. *)
  let octez_release_tag_re = "/^octez-v\\d+\\.\\d+(?:\\-rc\\d+)?$/" in
  (* Matches beta release tags, e.g. [v1.2.3-beta5]. *)
  let octez_beta_release_tag_re = "/^octez-v\\d+\\.\\d+\\-beta\\d*$/" in
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
    If.(Predefined_vars.ci_commit_tag != null && not has_any_octez_release_tag)
  in
  register
    "before_merging"
    If.(on_tezos_namespace && merge_request)
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
    ~jobs:(Release_tag.jobs Release_tag) ;
  register
    "octez_beta_release_tag"
    If.(on_tezos_namespace && push && has_tag_match octez_beta_release_tag_re)
    ~jobs:(Release_tag.jobs Beta_release_tag) ;
  register
    "octez_release_tag_test"
    If.(not_on_tezos_namespace && push && has_any_octez_release_tag)
    ~jobs:(Release_tag.jobs ~test:true Release_tag) ;
  register
    "non_release_tag"
    If.(on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.jobs Non_release_tag) ;
  register
    "non_release_tag_test"
    If.(not_on_tezos_namespace && push && has_non_release_tag)
    ~jobs:(Release_tag.jobs ~test:true Non_release_tag) ;
  register
    "schedule_extended_test"
    schedule_extended_tests
    ~jobs:(Code_verification.jobs Schedule_extended_test)

(* Split pipelines and writes image templates *)
let config () =
  (* Split pipelines types into workflow and includes *)
  let workflow, includes = Pipeline.workflow_includes () in
  (* Write image templates.

     This is a temporary stop-gap and only necessary for jobs that are
     not define in OCaml. Once all jobs have been migrated, this can
     be removed. *)
  let image_templates_include =
    let filename = ".gitlab/ci/jobs/shared/images.yml" in
    let image_template (name, image_path) : string * Yaml.value =
      let name = ".image_template__" ^ name in
      (name, `O [("image", `String (Image.name image_path))])
    in
    let config : Yaml.value = `O (List.map image_template (Image.all ())) in
    Base.write_yaml ~header:Tezos_ci.header filename config ;
    {local = filename; rules = []}
  in
  let includes =
    image_templates_include
    :: {local = ".gitlab/ci/jobs/shared/templates.yml"; rules = []}
    :: includes
  in
  Pipeline.write () ;
  [
    Workflow workflow;
    Default default;
    Variables variables;
    Stages (Stage.to_string_list ());
    Job job_dummy;
    Include includes;
  ]

let () =
  (* If argument --verbose is set, then log generation info.
     If argument --inline-source, then print generation info in yml files. *)
  let filename = ".gitlab-ci.yml" in
  Tezos_ci.to_file ~filename (config ()) ;
  (* Paths to exclude from generation check. As files are translated
     to CI-in-OCaml, they should be removed from this function *)
  let exclude = function
    | ".gitlab/ci/jobs/coverage/common.yml"
    | ".gitlab/ci/jobs/coverage/oc.unified_coverage-before_merging.yml"
    | ".gitlab/ci/jobs/doc/documentation.yml"
    | ".gitlab/ci/jobs/doc/documentation:linkcheck.yml"
    | ".gitlab/ci/jobs/doc/oc.install_python.yml"
    | ".gitlab/ci/jobs/packaging/debian_repository.yml"
    | ".gitlab/ci/jobs/shared/images.yml"
    | ".gitlab/ci/jobs/shared/templates.yml" | ".gitlab/ci/jobs/test/common.yml"
    | ".gitlab/ci/jobs/test/install_octez.yml"
    | ".gitlab/ci/jobs/test/oc.integration:compiler-rejections.yml"
    | ".gitlab/ci/jobs/test/oc.script:b58_prefix.yml"
    | ".gitlab/ci/jobs/test/oc.script:snapshot_alpha_and_link.yml"
    | ".gitlab/ci/jobs/test/oc.script:test-gen-genesis.yml"
    | ".gitlab/ci/jobs/test/oc.script:test_octez_release_versions.yml"
    | ".gitlab/ci/jobs/test/oc.test-liquidity-baking-scripts.yml"
    | ".gitlab/ci/jobs/test/oc.unit.yml"
    | ".gitlab/ci/jobs/test/test_etherlink_kernel-before_merging.yml"
    | ".gitlab/ci/jobs/test/test_etherlink_kernel-schedule_extended_test.yml"
    | ".gitlab/ci/jobs/test/test_evm_compatibility.yml"
    | ".gitlab/ci/jobs/test/test_kernels.yml"
    | ".gitlab/ci/jobs/test/test_risc_v_kernels-before_merging.yml"
    | ".gitlab/ci/jobs/test/test_risc_v_kernels-schedule_extended_test.yml"
    | ".gitlab/ci/jobs/test/tezt-flaky-before_merging.yml"
    | ".gitlab/ci/jobs/test/tezt-flaky-schedule_extended_test.yml"
    | ".gitlab/ci/jobs/test/tezt-flaky.yml"
    | ".gitlab/ci/jobs/test/tezt-slow-before_merging.yml"
    | ".gitlab/ci/jobs/test/tezt-slow-schedule_extended_test.yml"
    | ".gitlab/ci/jobs/test/tezt-slow.yml" | ".gitlab/ci/jobs/test/tezt.yml"
    | ".gitlab/ci/pipelines/before_merging.yml"
    | ".gitlab/ci/pipelines/schedule_extended_test.yml" ->
        true
    | _ -> false
  in
  Tezos_ci.check_files
    ~remove_extra_files:Cli.config.remove_extra_files
    ~exclude
    ()
