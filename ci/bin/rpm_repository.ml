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
open Common

(* This is a dynamic image declaration that is going to be instantiated using
   the values set in the parallel/matrix contruct *)
let generic_packages_image =
  Image.mk_external
    ~image_path:"$DEP_IMAGE:${CI_COMMIT_REF_SLUG}-${CI_COMMIT_SHORT_SHA}"

(** These are the set of Rocky Linux release-architecture combinations for
    which we build rpm packages in the job
    [job_build_rockylinux_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let rockylinux_package_release_matrix = function
  | Partial -> [[("RELEASE", ["9.3"]); ("TAGS", ["gcp"])]]
  | Full | Release -> [[("RELEASE", ["9.3"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

(** These are the set of Fedora release-architecture combinations for
    which we build rpm packages in the job
    [job_build_fedora_package]. See {!rockylinux_package_release_matrix}
    for more information.

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let fedora_package_release_matrix = function
  | Partial -> [[("RELEASE", ["39"]); ("TAGS", ["gcp"])]]
  | Full | Release ->
      [[("RELEASE", ["39"; "42"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

(* Push .rpm artifacts to storagecloud rpm repository. *)
let make_job_repo ?rules ~__POS__ ~name ?(stage = Stages.publishing)
    ?(prefix = false) ?dependencies ~variables ?id_tokens ~image ~before_script
    script : tezos_job =
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
    ~retry:{max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
    ~variables
    script

(* The entire RPM packages pipeline. When [pipeline_type] is [Before_merging]
   we test only on Rockylinux. Returns a triplet, the first element is
   the list of all jobs, the second is the job building fedora packages artifats
   and the third rockylinux packages artifacts *)
let jobs pipeline_type =
  let variables ?(kind = "build") add =
    ("FLAVOUR", kind)
    :: ( "DEP_IMAGE",
         "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/$FLAVOUR-$DISTRIBUTION-$RELEASE"
       )
       (* This second variable is for a read only registry and we want it to be
          tezos/tezos *)
    :: ( "DEP_IMAGE_PROTECTED",
         "${GCP_PROTECTED_REGISTRY}/tezos/tezos/$FLAVOUR-$DISTRIBUTION-$RELEASE"
       )
    :: add
  in
  let make_job_docker_systemd_tests ~__POS__ ~name ~matrix ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:
        (variables ~kind:"systemd-tests" [("DISTRIBUTION", distribution)])
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
      [
        "./scripts/ci/build-packages-dependencies.sh \
         images/packages/rpm-systemd-tests.Dockerfile";
      ]
  in
  let job_docker_systemd_test_rpm_dependencies : tezos_job =
    make_job_docker_systemd_tests
      ~__POS__
      ~name:"oc.docker-systemd_tests-rpm"
      ~distribution:"rockylinux"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
  in

  let make_job_docker_build_dependencies ~__POS__ ~name ~matrix ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:(variables [("DISTRIBUTION", distribution)])
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
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
  in
  let job_docker_build_fedora_dependencies : tezos_job =
    make_job_docker_build_dependencies
      ~__POS__
      ~name:"oc.docker-build-fedora-dependencies"
      ~distribution:"fedora"
      ~matrix:(fedora_package_release_matrix pipeline_type)
  in
  (* These jobs build the packages in a matrix using the
     build dependencies images *)
  let job_build_rockylinux_package : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-rockylinux"
      ~dependencies:(Dependent [Job job_docker_build_rockylinux_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh binaries"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
      ~variables:(variables [("DISTRIBUTION", "rockylinux")])
      ~image:generic_packages_image
  in
  let job_build_fedora_package : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-fedora"
      ~dependencies:(Dependent [Job job_docker_build_fedora_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh binaries"
      ~matrix:(fedora_package_release_matrix pipeline_type)
      ~variables:(variables [("DISTRIBUTION", "fedora")])
      ~image:generic_packages_image
  in
  let job_build_rockylinux_package_data : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-rockylinux-data"
      ~dependencies:(Dependent [Job job_docker_build_rockylinux_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh zcash"
      ~matrix:(rockylinux_package_release_matrix pipeline_type)
      ~variables:(variables [("DISTRIBUTION", "rockylinux")])
      ~image:generic_packages_image
  in
  let job_build_fedora_package_data : tezos_job =
    make_job_build_packages
      ~__POS__
      ~name:"oc.build-fedora-data"
      ~dependencies:(Dependent [Job job_docker_build_fedora_dependencies])
      ~script:"./scripts/ci/build-rpm-packages.sh zcash"
      ~matrix:(fedora_package_release_matrix pipeline_type)
      ~variables:(variables [("DISTRIBUTION", "fedora")])
      ~image:generic_packages_image
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
      ~image:Images.rockylinux_93
      ~before_script:
        (before_script
           ~source_version:true
           ["./scripts/ci/prepare-rpm-repo.sh"])
      ["./scripts/ci/create_rpm_repo.sh rockylinux 9.3"]
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
      ~image:Images.fedora_39
      ~before_script:
        (before_script
           ~source_version:true
           ["./scripts/ci/prepare-rpm-repo.sh"])
      ["./scripts/ci/create_rpm_repo.sh fedora 39 42"]
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
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
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
      ~retry:
        {max = 2; when_ = [Stuck_or_timeout_failure; Runner_system_failure]}
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
        ~name:"oc.install_bin_fedora_39"
        ~dependencies:(Dependent [Job job_rpm_repo_fedora])
        ~image:Images.fedora_39
        ["./scripts/packaging/tests/rpm/rpm-install.sh fedora 39"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_fedora_39.doc"
        ~dependencies:(Dependent [Job job_rpm_repo_fedora])
        ~image:Images.fedora_39
        ["./docs/introduction/install-bin-rpm.sh fedora 39"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_fedora_39_systemd"
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_rpm_dependencies;
               Job job_rpm_repo_fedora;
             ])
        ~variables:
          (variables
             ~kind:"systemd-tests"
             [("DISTRIBUTION", "fedora"); ("RELEASE", "39")])
        [
          "./scripts/ci/systemd-packages-test.sh \
           docs/introduction/install-bin-rpm.sh \
           images/packages/rpm-systemd-tests.Dockerfile";
        ];
    ]
  in
  let test_rockylinux_packages_jobs =
    [
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_rockylinux_9.3"
        ~dependencies:(Dependent [Job job_rpm_repo_rockylinux])
        ~image:Images.rockylinux_93
        ["./scripts/packaging/tests/rpm/rpm-install.sh rockylinux 9.3"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_rockylinux_9.3.doc"
        ~dependencies:(Dependent [Job job_rpm_repo_rockylinux])
        ~image:Images.rockylinux_93
        ["./docs/introduction/install-bin-rpm.sh rockylinux 9.3"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_rockylinux_93_systemd"
        ~dependencies:
          (Dependent
             [
               Job job_docker_systemd_test_rpm_dependencies;
               Job job_rpm_repo_rockylinux;
             ])
        ~variables:
          (variables
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
      job_docker_systemd_test_rpm_dependencies;
      job_build_rockylinux_package;
      job_build_rockylinux_package_data;
      job_rpm_repo_rockylinux;
    ]
  in
  let fedora_jobs =
    [
      job_docker_build_fedora_dependencies;
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

let child_pipeline_full =
  register
    ~description:
      "A child pipeline of 'schedule_extended_test' testing the build of all \
       .rpm packages."
    ~auto:false
    Full

let child_pipeline_partial_auto =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Rockylinux 9.3 .rpm packages. Starts automatically on certain \
       conditions."
    ~auto:true
    Partial
