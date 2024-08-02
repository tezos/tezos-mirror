(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [debian_repository] child
   pipeline.

   This pipeline builds the current and next Debian (and Ubuntu)
   packages. *)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Common

let build_debian_packages_image =
  Image.mk_external ~image_path:"$DEP_IMAGE:${CI_COMMIT_REF_SLUG}"

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job
    [job_build_debian_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let debian_package_release_matrix = function
  | Partial -> [[("RELEASE", ["bookworm"]); ("TAGS", ["gcp"])]]
  | Full | Release ->
      [[("RELEASE", ["unstable"; "bookworm"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

(** These are the set of Ubuntu release-architecture combinations for
    which we build deb packages in the job
    [job_build_ubuntu_package]. See {!debian_package_release_matrix}
    for more information.

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture. *)
let ubuntu_package_release_matrix = function
  | Partial -> [[("RELEASE", ["jammy"]); ("TAGS", ["gcp"])]]
  | Full | Release ->
      [[("RELEASE", ["noble"; "jammy"]); ("TAGS", ["gcp"; "gcp_arm64"])]]

(* Push .deb artifacts to storagecloud apt repository. *)
let job_apt_repo ?rules ~__POS__ ~name ?(stage = Stages.publishing)
    ?(prefix = false) ?dependencies ?(archs = [Amd64]) ~image script : tezos_job
    =
  let variables =
    [
      ( "ARCHITECTURES",
        String.concat " " (List.map Tezos_ci.arch_to_string_alt archs) );
      ("GNUPGHOME", "$CI_PROJECT_DIR/.gnupg");
    ]
    @ if prefix then [("PREFIX", "next")] else []
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage
    ~name
    ~image
    ~before_script:
      (before_script ~source_version:true ["./scripts/ci/prepare-apt-repo.sh"])
    ~variables
    script

(* The entire Debian packages pipeline. When [pipeline_type] is [Before_merging]
   we test only on Debian stable. Returns a triplet, the first element is
   the list of all jobs, the second is the job building ubuntu packages artifats
   and the third debian packages artifacts *)
let jobs pipeline_type =
  let variables add =
    ( "DEP_IMAGE",
      "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/build-$DISTRIBUTION-$RELEASE"
    )
    :: add
  in
  let make_job_docker_build_debian_dependencies ~__POS__ ~name ~matrix
      ~distribution =
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables:(variables [("DISTRIBUTION", distribution)])
      ~parallel:(Matrix matrix)
      ~tag:Dynamic
      ["./scripts/ci/build-debian-packages-dependencies.sh"]
  in
  let job_docker_build_debian_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-debian-dependencies"
      ~distribution:"debian"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_docker_build_ubuntu_dependencies : tezos_job =
    make_job_docker_build_debian_dependencies
      ~__POS__
      ~name:"oc.docker-build-ubuntu-dependencies"
      ~distribution:"ubuntu"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in
  let make_job_build_debian_packages ~__POS__ ~name ~matrix ~distribution
      ~script ~dependencies =
    job
      ~__POS__
      ~name
      ~image:build_debian_packages_image
      ~stage:Stages.build
      ~variables:(variables [("DISTRIBUTION", distribution)])
      ~parallel:(Matrix matrix)
      ~dependencies
      ~tag:Dynamic
      ~artifacts:(artifacts ["packages/$DISTRIBUTION/$RELEASE"])
      [
        (* This is an hack to enable Cargo networking for jobs in child pipelines.

           There is an weird gotcha with how variables are passed to
           child pipelines. Global variables of the parent pipeline
           are passed to the child pipeline. Inside the child
           pipeline, variables received from the parent pipeline take
           precedence over job-level variables. It's bit strange. So
           to override the default [CARGO_NET_OFFLINE=true], we cannot
           just set it in the job-level variables of this job.

           See
           {{:https://docs.gitlab.com/ee/ci/variables/index.html#cicd-variable-precedence}here}
           for more info. *)
        "export CARGO_NET_OFFLINE=false";
        script;
      ]
  in

  (* These jobs build the current packages in a matrix using the
     build dependencies images *)
  let job_build_debian_package_current_a : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-debian-current_a"
      ~distribution:"debian"
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~script:"./scripts/ci/build-debian-packages_current.sh A"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_build_ubuntu_package_current_a : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-ubuntu-current_a"
      ~distribution:"ubuntu"
      ~dependencies:(Dependent [Job job_docker_build_ubuntu_dependencies])
      ~script:"./scripts/ci/build-debian-packages_current.sh A"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in

  let job_build_debian_package_current_b : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-debian-current_b"
      ~distribution:"debian"
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~script:"./scripts/ci/build-debian-packages_current.sh B"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_build_ubuntu_package_current_b : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-ubuntu-current_b"
      ~distribution:"ubuntu"
      ~dependencies:(Dependent [Job job_docker_build_ubuntu_dependencies])
      ~script:"./scripts/ci/build-debian-packages_current.sh B"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in

  (* These jobs build the next packages in a matrix using the
     build dependencies images *)
  let job_build_data_packages : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-data_packages"
      ~distribution:"debian"
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~script:"./scripts/ci/build-debian-packages.sh zcash"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_build_debian_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-debian"
      ~distribution:"debian"
      ~dependencies:(Dependent [Job job_docker_build_debian_dependencies])
      ~script:"./scripts/ci/build-debian-packages.sh binaries"
      ~matrix:(debian_package_release_matrix pipeline_type)
  in
  let job_build_ubuntu_package : tezos_job =
    make_job_build_debian_packages
      ~__POS__
      ~name:"oc.build-ubuntu"
      ~distribution:"ubuntu"
      ~dependencies:(Dependent [Job job_docker_build_ubuntu_dependencies])
      ~script:"./scripts/ci/build-debian-packages.sh binaries"
      ~matrix:(ubuntu_package_release_matrix pipeline_type)
  in

  (* These jobs create the apt repository for the current packages *)
  let job_apt_repo_debian_current =
    job_apt_repo
      ~__POS__
      ~name:"apt_repo_debian_current"
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_debian_package_current_a;
             Artifacts job_build_debian_package_current_b;
           ])
      ~image:Images.debian_bookworm
      ["./scripts/ci/create_debian_repo.sh debian bookworm"]
  in
  let job_apt_repo_ubuntu_current =
    job_apt_repo
      ~__POS__
      ~name:"apt_repo_ubuntu_current"
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_ubuntu_package_current_a;
             Artifacts job_build_ubuntu_package_current_b;
           ])
      ~image:Images.ubuntu_noble
      ["./scripts/ci/create_debian_repo.sh ubuntu noble jammy"]
  in
  (* These jobs create the apt repository for the next packages *)
  let job_apt_repo_debian =
    job_apt_repo
      ~__POS__
      ~name:"apt_repo_debian"
      ~prefix:true
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_debian_package;
             Artifacts job_build_data_packages;
           ])
      ~image:Images.debian_bookworm
      ["./scripts/ci/create_debian_repo.sh debian bookworm"]
  in
  let job_apt_repo_ubuntu =
    job_apt_repo
      ~__POS__
      ~name:"apt_repo_ubuntu"
      ~prefix:true
      ~dependencies:
        (Dependent
           [
             Artifacts job_build_ubuntu_package;
             Artifacts job_build_data_packages;
           ])
      ~image:Images.ubuntu_noble
      ["./scripts/ci/create_debian_repo.sh ubuntu noble jammy"]
  in
  (* These test the installability of the current packages *)
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
  let job_lintian ~__POS__ ~name ~dependencies ~image ?allow_failure script =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~stage:Stages.publishing_tests
      ~before_script:
        (before_script
           ~source_version:true
           [
             "export DEBIAN_FRONTEND=noninteractive";
             "apt-get update";
             "apt-get install lintian -y";
           ])
      script
  in
  let test_current_ubuntu_packages_jobs =
    (* in merge pipelines we tests only debian. release pipelines
       test the entire matrix *)
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_ubuntu"
        ~dependencies:(Dependent [Artifacts job_build_ubuntu_package])
        ~image:Images.ubuntu_noble
        ["./scripts/ci/lintian_debian_packages.sh ubuntu jammy noble"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_noble_current"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu_current])
        ~image:Images.ubuntu_noble
        ["./docs/introduction/install-bin-deb.sh ubuntu noble"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_jammy_current"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu_current])
        ~image:Images.ubuntu_jammy
        ["./docs/introduction/install-bin-deb.sh ubuntu jammy"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_noble"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu])
        ~variables:[("PREFIX", "next")]
        ~image:Images.ubuntu_noble
        ["./docs/introduction/install-bin-deb.sh ubuntu noble"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_jammy"
        ~dependencies:(Dependent [Job job_apt_repo_ubuntu])
        ~variables:[("PREFIX", "next")]
        ~image:Images.ubuntu_jammy
        ["./docs/introduction/install-bin-deb.sh ubuntu jammy"];
    ]
  in
  let test_current_debian_packages_jobs =
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_debian"
        ~dependencies:(Dependent [Artifacts job_build_debian_package])
        ~image:Images.debian_bookworm
        ["./scripts/ci/lintian_debian_packages.sh debian bookworm"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm_current"
        ~dependencies:(Dependent [Job job_apt_repo_debian_current])
        ~image:Images.debian_bookworm
        ["./docs/introduction/install-bin-deb.sh debian bookworm"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm"
        ~dependencies:(Dependent [Job job_apt_repo_debian])
        ~variables:[("PREFIX", "next")]
        ~image:Images.debian_bookworm
        ["./docs/introduction/install-bin-deb.sh debian bookworm"];
    ]
  in
  let debian_jobs =
    [
      job_docker_build_debian_dependencies;
      job_build_debian_package;
      job_build_debian_package_current_a;
      job_build_debian_package_current_b;
      job_build_data_packages;
      job_apt_repo_debian_current;
      job_apt_repo_debian;
    ]
  in
  let ubuntu_jobs =
    [
      job_docker_build_ubuntu_dependencies;
      job_build_ubuntu_package;
      job_build_ubuntu_package_current_a;
      job_build_ubuntu_package_current_b;
      job_apt_repo_ubuntu_current;
      job_apt_repo_ubuntu;
    ]
  in
  match pipeline_type with
  | Partial ->
      ( debian_jobs @ test_current_debian_packages_jobs,
        job_build_ubuntu_package_current_a,
        job_build_debian_package_current_a,
        job_build_ubuntu_package_current_b,
        job_build_debian_package_current_b )
  | Full ->
      ( debian_jobs @ ubuntu_jobs @ test_current_debian_packages_jobs
        @ test_current_ubuntu_packages_jobs,
        job_build_ubuntu_package_current_a,
        job_build_debian_package_current_a,
        job_build_ubuntu_package_current_b,
        job_build_debian_package_current_b )
  | Release ->
      ( debian_jobs @ ubuntu_jobs,
        job_build_ubuntu_package_current_a,
        job_build_debian_package_current_a,
        job_build_ubuntu_package_current_b,
        job_build_debian_package_current_b )

let register pipeline_type =
  let pipeline_name =
    match pipeline_type with
    | Partial -> "debian_repository_partial"
    | Full -> "debian_repository_full"
    | Release -> "debian_repository_release"
  in
  let jobs, _, _, _, _ = jobs pipeline_type in
  Pipeline.register_child pipeline_name ~jobs

let child_pipeline_partial = register Partial

let child_pipeline_full = register Full
