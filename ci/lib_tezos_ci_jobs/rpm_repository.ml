(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [rockylinux_repository] child
   pipeline.

   This pipeline builds the Fedora (and Rocky Linux) packages. *)

open Gitlab_ci.Types
open Tezos_ci
open Common.Helpers
open Common.Packaging

let tag_amd64 ~ramfs =
  if ramfs then Runner.Tag.show Gcp_very_high_cpu_ramfs
  else Runner.Tag.show Gcp_very_high_cpu

let tag_arm64 = Runner.Tag.show Gcp_arm64

(** These are the set of Rocky Linux release-architecture combinations for
    which we build rpm packages in the job
    [job_build_rockylinux_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let rockylinux_package_release_matrix ?(ramfs = false) = function
  | Partial -> [[("RELEASE", ["9.3"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full | Release ->
      [[("RELEASE", ["9.3"]); ("TAGS", [tag_amd64 ~ramfs; tag_arm64])]]

(** These are the set of Fedora release-architecture combinations for
    which we build rpm packages in the job
    [job_build_fedora_package]. See {!rockylinux_package_release_matrix}
    for more information.

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let fedora_package_release_matrix ?(ramfs = false) = function
  | Partial -> [[("RELEASE", ["39"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full | Release ->
      [[("RELEASE", ["39"; "42"]); ("TAGS", [tag_amd64 ~ramfs; tag_arm64])]]

(* Push .rpm artifacts to storagecloud rpm repository. *)
let make_job_repo ?rules ~__POS__ ~name ?(stage = Stages.publish)
    ?(prefix = false) ?dependencies ~variables ?id_tokens ~image ~before_script
    ?tag ?retry script : tezos_job =
  let variables =
    variables
    @ [("GNUPGHOME", "$CI_PROJECT_DIR/.gnupg")]
    @ if prefix then [("PREFIX", "")] else []
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage
    ~name
    ?id_tokens
    ~image
    ~before_script
    ~variables
    ?tag
    ?retry
    script

(* The entire RPM packages pipeline. When [pipeline_type] is [Before_merging]
   we test only on Rockylinux. Returns a triplet, the first element is
   the list of all jobs, the second is the job building fedora packages artifats
   and the third rockylinux packages artifacts *)
let jobs ?(limit_dune_build_jobs = false) pipeline_type =
  let make_job_docker_systemd_tests =
    make_job_docker_systemd_tests
      ~base_image:
        (Images.Base_images.path_prefix ^ "/${DISTRIBUTION}:${RELEASE}")
      ~script:
        [
          "./scripts/ci/build-packages-dependencies.sh \
           images/packages/rpm-systemd-tests.Dockerfile";
        ]
  in
  let job_docker_systemd_test_rpm_rockylinux_dependencies : tezos_job =
    make_job_docker_systemd_tests
      ~__POS__
      ~name:"oc.docker-systemd-tests-rpm-rockylinux"
      ~distribution:"rockylinux"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
  in
  let job_docker_systemd_test_rpm_fedora_dependencies : tezos_job =
    make_job_docker_systemd_tests
      ~__POS__
      ~name:"oc.docker-systemd-tests-rpm-fedora"
      ~distribution:"fedora"
      ~matrix:(fedora_package_release_matrix pipeline_type)
  in
  let make_job_docker_build_dependencies =
    make_docker_build_dependencies
      ~base_image:
        (Images.Base_images.path_prefix ^ "/${DISTRIBUTION}:${RELEASE}")
      ~script:
        [
          "./scripts/ci/build-packages-dependencies.sh \
           images/packages/rpm-deps-build.Dockerfile";
        ]
  in
  let job_docker_build_rockylinux_dependencies : tezos_job =
    make_job_docker_build_dependencies
      ~__POS__
      ~name:"oc.docker-build-rockylinux-dependencies"
      ~distribution:"rockylinux"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
      ()
  in
  let job_docker_build_fedora_dependencies : tezos_job =
    make_job_docker_build_dependencies
      ~__POS__
      ~name:"oc.docker-build-fedora-dependencies"
      ~distribution:"fedora"
      ~matrix:(fedora_package_release_matrix pipeline_type)
      ()
  in
  (* These jobs build the packages in a matrix using the
     build dependencies images *)
  let job_build_rockylinux_package : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-rockylinux"
      ~distribution:"rockylinux"
      ~dependencies:(Dependent [Job job_docker_build_rockylinux_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh binaries"
      ~matrix:(rockylinux_package_release_matrix ~ramfs:true pipeline_type)
      ~limit_dune_build_jobs
      ()
  in
  let job_build_fedora_package : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-fedora"
      ~distribution:"fedora"
      ~dependencies:(Dependent [Job job_docker_build_fedora_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh binaries"
      ~matrix:(fedora_package_release_matrix ~ramfs:true pipeline_type)
      ~limit_dune_build_jobs
      ()
  in
  let job_build_rockylinux_package_data : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-rockylinux-data"
      ~distribution:"rockylinux"
      ~dependencies:(Dependent [Job job_docker_build_rockylinux_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh zcash"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
      ()
  in
  let job_build_fedora_package_data : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-fedora-data"
      ~distribution:"fedora"
      ~dependencies:(Dependent [Job job_docker_build_fedora_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh zcash"
      ~matrix:(fedora_package_release_matrix pipeline_type)
      ()
  in

  (* These jobs create the rpm repository for the packages *)
  let job_rpm_repo_rockylinux =
    make_job_repo
      ~__POS__
      ~name:"rpm_repo_rockylinux"
      ~prefix:true
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_rockylinux_package;
             Artifacts job_build_rockylinux_package_data;
           ])
      ~variables:(archs_variables pipeline_type)
      ~id_tokens:Tezos_ci.id_tokens
      ~image:Images.Base_images.rockylinux_9_3
      ~before_script:
        (before_script
           ~source_version:true
           ["./scripts/ci/prepare-rpm-repo.sh"])
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ["./scripts/ci/create_rpm_repo.sh rockylinux 9.3"]
      ~tag:Gcp_not_interruptible
  in
  let job_rpm_repo_fedora =
    make_job_repo
      ~__POS__
      ~name:"rpm_repo_fedora"
      ~prefix:true
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_fedora_package;
             Artifacts job_build_fedora_package_data;
           ])
      ~variables:(archs_variables pipeline_type)
      ~id_tokens:Tezos_ci.id_tokens
      ~image:Images.Base_images.fedora_39
      ~before_script:
        (before_script
           ~source_version:true
           ["./scripts/ci/prepare-rpm-repo.sh"])
      ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
      ["./scripts/ci/create_rpm_repo.sh fedora 39 42"]
      ~tag:Gcp_not_interruptible
  in
  (* These test the installability *)
  let job_install_bin ~__POS__ ~name ~dependencies ~image ?(variables = [])
      ?allow_failure script =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~variables
      ~stage:Stages.publishing_tests
      script
  in
  (* These test the upgrade *)
  let _job_upgrade_bin ~__POS__ ~name ~dependencies ~image ?allow_failure script
      =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~stage:Stages.publishing_tests
      script
  in
  let job_install_systemd_bin ~__POS__ ~name ~dependencies ?(variables = [])
      ?allow_failure script =
    job_docker_authenticated
      ?allow_failure
      ~__POS__
      ~name
      ~dependencies
      ~variables
      ~stage:Stages.publishing_tests
      script
  in
  let test_fedora_packages_jobs =
    [
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_fedora_39.doc"
        ~dependencies:(Dependent [Job job_rpm_repo_fedora])
        ~image:Images.Base_images.fedora_39
        ["./docs/introduction/install-bin-rpm.sh fedora 39"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_fedora_39_systemd"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_rpm_fedora_dependencies;
               Job job_rpm_repo_fedora;
             ])
        ~variables:
          (Common.Packaging.make_variables
             ~kind:"systemd-tests"
             [("DISTRIBUTION", "fedora"); ("RELEASE", "39")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/rpm/rpm-install.sh \
           images/packages/rpm-systemd-tests.Dockerfile";
        ];
    ]
  in
  let test_rockylinux_packages_jobs =
    [
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_rockylinux_9.3.doc"
        ~dependencies:(Dependent [Job job_rpm_repo_rockylinux])
        ~image:Images.Base_images.rockylinux_9_3
        ["./docs/introduction/install-bin-rpm.sh rockylinux 9.3"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_rockylinux_93_systemd"
        ~allow_failure:Yes
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_rpm_rockylinux_dependencies;
               Job job_rpm_repo_rockylinux;
             ])
        ~variables:
          (Common.Packaging.make_variables
             ~kind:"systemd-tests"
             [("DISTRIBUTION", "rockylinux"); ("RELEASE", "9.3")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/rpm/rpm-install.sh \
           images/packages/rpm-systemd-tests.Dockerfile";
        ];
    ]
  in
  let rockylinux_jobs =
    [
      job_docker_build_rockylinux_dependencies;
      job_docker_systemd_test_rpm_rockylinux_dependencies;
      job_build_rockylinux_package;
      job_build_rockylinux_package_data;
      job_rpm_repo_rockylinux;
    ]
  in
  let fedora_jobs =
    [
      job_docker_build_fedora_dependencies;
      job_docker_systemd_test_rpm_fedora_dependencies;
      job_build_fedora_package;
      job_build_fedora_package_data;
      job_rpm_repo_fedora;
    ]
  in
  match pipeline_type with
  | Partial -> rockylinux_jobs @ test_rockylinux_packages_jobs
  | Full ->
      rockylinux_jobs @ fedora_jobs @ test_fedora_packages_jobs
      @ test_rockylinux_packages_jobs
  | Release -> rockylinux_jobs @ fedora_jobs

let register ~auto ~description pipeline_type =
  let pipeline_name =
    match (pipeline_type, auto) with
    | Partial, false -> "rpm_repository_partial"
    | Partial, true -> "rpm_repository_partial_auto"
    | Full, _ -> "rpm_repository_full"
    | Release, _ -> "rpm_repository_release"
  in
  let jobs = jobs pipeline_type in
  Pipeline.register_child
    pipeline_name
    ~description
    ~jobs:(job_datadog_pipeline_trace :: jobs)

let child_pipeline_partial =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Rocky Linux 9.3 .rpm packages."
    ~auto:false
    Partial

let child_pipeline_partial_auto =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Rockylinux 9.3 .rpm packages. Starts automatically on certain \
       conditions."
    ~auto:true
    Partial
