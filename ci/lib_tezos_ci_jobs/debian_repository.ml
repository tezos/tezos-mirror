(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines the jobs of the [debian_repository] child
   pipeline.

   This pipeline builds the old and old Debian (and Ubuntu)
   packages. *)

open Tezos_ci

let tag_amd64 ~ramfs =
  if ramfs then Runner.Tag.show Gcp_very_high_cpu_ramfs
  else Runner.Tag.show Gcp_very_high_cpu

let tag_arm64 = Runner.Tag.show Gcp_arm64

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job
    [job_build_debian_package]. A dependency image will be built once
    for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture.

    Specify [ramfs] to select the specific runner for amd64.

    Set [arm64] to false to exclude from the matrix arm64 architecture.
    *)
let debian_package_release_matrix ?(ramfs = false) ?(arm64 = true) = function
  | Common.Packaging.Partial ->
      [[("RELEASE", ["bookworm"; "trixie"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full ->
      [
        [
          ("RELEASE", ["unstable"; "bookworm"; "trixie"]);
          ("TAGS", tag_amd64 ~ramfs :: (if arm64 then [tag_arm64] else []));
        ];
      ]
  | Release ->
      [
        [
          ("RELEASE", ["bookworm"; "trixie"]);
          ("TAGS", tag_amd64 ~ramfs :: (if arm64 then [tag_arm64] else []));
        ];
      ]

(* Points to [(debian|ubuntu)-build] static images. *)
let build_dependency_image =
  Image.mk_external
    ~image_path:
      (sf
         "${GCP_PROTECTED_REGISTRY}/tezos/tezos/$DISTRIBUTION-build:$RELEASE-%s"
         Tezos_ci.Images.Base_images.debian_version)

let make_job_build_packages ~__POS__ ?(limit_dune_build_jobs = false) ~name
    ~matrix ~distribution ~script ~dependencies ?(manual = false) () =
  job
    ~__POS__
    ~name
    ~image:build_dependency_image
    ~stage:Stages.build
    ?rules:
      (if manual then Some [Gitlab_ci.Util.job_rule ~when_:Manual ()] else None)
    ~variables:
      (("DISTRIBUTION", distribution)
      :: (if limit_dune_build_jobs then [("DUNE_BUILD_JOBS", "-j 12")] else [])
      )
    ~parallel:(Matrix matrix)
    ~dependencies
    ~tag:Dynamic
    ~artifacts:(Gitlab_ci.Util.artifacts ["packages/$DISTRIBUTION/$RELEASE"])
    [
      (* This is a hack to enable Cargo networking for jobs in child pipelines.

         Global variables of the parent pipeline
         are passed to the child pipeline. Inside the child
         pipeline, variables received from the parent pipeline take
         precedence over job-level variables. It's bit strange. So
         to override the default [CARGO_NET_OFFLINE=true], we cannot
         just set it in the job-level variables of this job.

         [enable_sccache] adds the cache directive for [$CI_PROJECT_DIR/_sccache].

         See
         {{:https://docs.gitlab.com/ee/ci/variables/index.html#cicd-variable-precedence}here}
         for more info. *)
      "export CARGO_NET_OFFLINE=false";
      script;
    ]
  |> Tezos_ci.Cache.enable_sccache

let make_debian_variables distribution image_kind release version =
  ( "DEP_IMAGE",
    sf
      "${GCP_PROTECTED_REGISTRY}/tezos/tezos/%s-%s:%s-%s"
      distribution
      image_kind
      release
      version )
  :: [("PREFIX", ""); ("DISTRIBUTION", distribution); ("RELEASE", release)]

(** These are the set of Ubuntu release-architecture combinations for
    which we build deb packages in the job
    [job_build_ubuntu_package]. See {!debian_package_release_matrix}
    for more information.

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture.

    Specify [ramfs] to select the specific runner for amd64.

    Set [arm64] to false to exclude from the matrix arm64 architecture.
    *)
let ubuntu_package_release_matrix ?(ramfs = false) ?(arm64 = true) = function
  | Common.Packaging.Partial ->
      [[("RELEASE", ["22.04"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full | Release ->
      [
        [
          ("RELEASE", ["22.04"; "24.04"]);
          ("TAGS", tag_amd64 ~ramfs :: (if arm64 then [tag_arm64] else []));
        ];
      ]

(* Push .deb artifacts to storagecloud apt repository. *)
let make_job_apt_repo ?rules ~__POS__ ~name ?(stage = Stages.publish)
    ?dependencies ~prefix ~variables ?retry ~image script : tezos_job =
  let variables =
    variables @ [("GNUPGHOME", "$CI_PROJECT_DIR/.gnupg")] @ [("PREFIX", prefix)]
  in
  job
    ?rules
    ?dependencies
    ~__POS__
    ~stage
    ~name
    ~id_tokens:Tezos_ci.id_tokens
    ~image
    ~tag:Gcp_not_interruptible
    ?retry
    ~before_script:
      (Common.Helpers.before_script
         ~source_version:true
         ["apt-get install -y --update apt-utils debsigs"])
    ~variables
    script

(* data packages. we build them once *)
let job_build_data_packages ~manual : tezos_job =
  job
    ~__POS__
    ~name:"oc.build-data_packages"
    ~image:build_dependency_image
    ~stage:Stages.build
    ?rules:
      (if manual then Some [Gitlab_ci.Util.job_rule ~when_:Manual ()] else None)
    ~variables:
      [("DISTRIBUTION", "debian"); ("RELEASE", "trixie"); ("TAGS", "gcp")]
    ~dependencies:(Dependent [])
    ~tag:Dynamic
    ~artifacts:(Gitlab_ci.Util.artifacts ["packages/$DISTRIBUTION/$RELEASE"])
    [
      "export CARGO_NET_OFFLINE=false";
      "./scripts/ci/build-debian-packages.sh zcash";
    ]

(* These jobs build the packages in a matrix using the
   build dependencies images *)
let job_build_debian_package ~limit_dune_build_jobs ~manual pipeline_type :
    tezos_job =
  make_job_build_packages
    ~__POS__
    ~name:"oc.build-debian"
    ~distribution:"debian"
    ~dependencies:(Dependent [])
    ~script:"./scripts/ci/build-debian-packages.sh binaries"
    ~matrix:(debian_package_release_matrix ~ramfs:true pipeline_type)
    ~limit_dune_build_jobs
    ~manual
    ()

let job_build_ubuntu_package ~limit_dune_build_jobs ~manual pipeline_type :
    tezos_job =
  make_job_build_packages
    ~__POS__
    ~name:"oc.build-ubuntu"
    ~distribution:"ubuntu"
    ~dependencies:(Dependent [])
    ~script:"./scripts/ci/build-debian-packages.sh binaries"
    ~matrix:(ubuntu_package_release_matrix ~ramfs:true pipeline_type)
    ~limit_dune_build_jobs
    ~manual
    ()

(* These jobs create the apt repository for the packages *)
let job_apt_repo_debian ~limit_dune_build_jobs ~manual pipeline_type =
  make_job_apt_repo
    ~__POS__
    ~name:"apt_repo_debian"
    ~prefix:""
    ~dependencies:
      (Dependent
         [
           Artifacts
             (job_build_debian_package
                ~limit_dune_build_jobs
                ~manual
                pipeline_type);
           Artifacts (job_build_data_packages ~manual);
         ])
    ~variables:(Common.Packaging.archs_variables pipeline_type)
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~image:Images.Base_images.debian_trixie
    ["./scripts/ci/create_debian_repo.sh debian bookworm trixie"]

let job_apt_repo_ubuntu ~limit_dune_build_jobs ~manual pipeline_type =
  make_job_apt_repo
    ~__POS__
    ~name:"apt_repo_ubuntu"
    ~prefix:""
    ~dependencies:
      (Dependent
         [
           Artifacts
             (job_build_ubuntu_package
                ~limit_dune_build_jobs
                ~manual
                pipeline_type);
           Artifacts (job_build_data_packages ~manual);
         ])
    ~variables:(Common.Packaging.archs_variables pipeline_type)
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~image:Images.Base_images.ubuntu_24_04
    ["./scripts/ci/create_debian_repo.sh ubuntu 22.04 24.04"]

(* These test the installability of the old packages *)
let job_install_bin ~__POS__ ~name ~dependencies ~image ?(variables = [])
    ?allow_failure ?before_script script =
  job
    ?allow_failure
    ~__POS__
    ~name
    ~image
    ~dependencies
    ~variables
    ~stage:Stages.publishing_tests
    ?before_script
    script

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

(* The entire Debian packages pipeline. When [pipeline_type] is [Before_merging]
   we test only on Debian stable. Returns a triplet, the first element is
   the list of all jobs, the second is the job building ubuntu packages artifats
   and the third debian packages artifacts *)
let jobs ?(limit_dune_build_jobs = false) ?(manual = false) pipeline_type =
  let job_lintian ~__POS__ ~name ~dependencies ?(variables = []) ~image
      ?allow_failure script =
    job
      ?allow_failure
      ~__POS__
      ~name
      ~image
      ~dependencies
      ~stage:Stages.publishing_tests
      ~variables
      ~before_script:
        (Common.Helpers.before_script
           ~source_version:true
           [
             "export DEBIAN_FRONTEND=noninteractive";
             "apt-get update";
             "apt-get install lintian parallel -y";
           ])
      script
  in
  let test_ubuntu_packages_jobs =
    (* in merge pipelines we tests only debian. ubuntu packages
       are built and tested in the scheduled pipelines*)
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_ubuntu"
        ~dependencies:
          (Dependent
             [
               Artifacts
                 (job_build_ubuntu_package
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~image:Images.Base_images.ubuntu_24_04
        ["./scripts/ci/lintian_debian_packages.sh ubuntu 22.04 24.04"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_22_04"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_ubuntu
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.ubuntu_22_04
        ["./docs/introduction/install-bin-deb.sh ubuntu 22.04"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_24_04"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_ubuntu
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.ubuntu_24_04
        ["./docs/introduction/install-bin-deb.sh ubuntu 24.04"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_ubuntu_24_04_systemd"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_ubuntu
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:
          (make_debian_variables
             "ubuntu"
             "systemd"
             "24.04"
             Tezos_ci.Images.Base_images.debian_version)
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/install-bin-deb.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_ubuntu_22_04_systemd"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_ubuntu
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:
          (make_debian_variables
             "ubuntu"
             "systemd"
             "22.04"
             Tezos_ci.Images.Base_images.debian_version)
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_ubuntu_24_04_systemd"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_ubuntu
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:
          (make_debian_variables
             "ubuntu"
             "systemd"
             "24.04"
             Tezos_ci.Images.Base_images.debian_version)
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
    ]
  in
  let test_debian_packages_jobs =
    [
      job_lintian
        ~__POS__
        ~name:"oc.lintian_debian"
        ~dependencies:
          (Dependent
             [
               Artifacts
                 (job_build_debian_package
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~image:Images.Base_images.debian_bookworm
        ["./scripts/ci/lintian_debian_packages.sh debian bookworm"];
      job_install_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_debian
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:[("PREFIX", "")]
        ~image:Images.Base_images.debian_bookworm
        ["./docs/introduction/install-bin-deb.sh debian bookworm"];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.install_bin_debian_bookworm_systemd"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_debian
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:
          (make_debian_variables
             "debian"
             "systemd"
             "bookworm"
             Tezos_ci.Images.Base_images.debian_version)
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/install-bin-deb.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
      job_install_systemd_bin
        ~__POS__
        ~name:"oc.upgrade_bin_debian_bookworm-systemd"
        ~dependencies:
          (Dependent
             [
               Job
                 (job_apt_repo_debian
                    ~limit_dune_build_jobs
                    ~manual
                    pipeline_type);
             ])
        ~variables:
          (make_debian_variables
             "debian"
             "systemd"
             "bookworm"
             Tezos_ci.Images.Base_images.debian_version)
        [
          "./scripts/ci/systemd-packages-test.sh \
           scripts/packaging/tests/deb/upgrade-systemd-test.sh \
           images/packages/debian-systemd-tests.Dockerfile";
        ];
    ]
  in
  let debian_jobs =
    [
      job_build_debian_package ~limit_dune_build_jobs ~manual pipeline_type;
      job_build_data_packages ~manual;
      job_apt_repo_debian ~limit_dune_build_jobs ~manual pipeline_type;
    ]
  in
  let ubuntu_jobs =
    [
      job_build_ubuntu_package ~limit_dune_build_jobs ~manual pipeline_type;
      job_apt_repo_ubuntu ~limit_dune_build_jobs ~manual pipeline_type;
    ]
  in
  match pipeline_type with
  | Partial -> debian_jobs @ test_debian_packages_jobs
  | Full ->
      debian_jobs @ ubuntu_jobs @ test_debian_packages_jobs
      @ test_ubuntu_packages_jobs
  | Release -> debian_jobs @ ubuntu_jobs

let register ~auto ~description pipeline_type =
  let pipeline_name =
    match (pipeline_type, auto) with
    | Common.Packaging.Partial, false -> "debian_repository_partial"
    | Partial, true -> "debian_repository_partial_auto"
    | Full, _ -> "debian_repository_full"
    | Release, _ -> "debian_repository_release"
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
       Debian stable .deb packages."
    ~auto:false
    Partial

let child_pipeline_partial_auto =
  register
    ~description:
      "A child pipeline of 'before_merging' (and thus 'merge_train') building \
       Debian stable .deb packages. Starts automatically on certain \
       conditions."
    ~auto:true
    Partial
