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
module CI = Cacio.Shared

let tag_amd64 ~ramfs =
  if ramfs then Runner.Tag.show Gcp_very_high_cpu_ramfs
  else Runner.Tag.show Gcp_very_high_cpu

let tag_arm64 = Runner.Tag.show Gcp_arm64

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job [oc.build-debian].
    A dependency image will be built once for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture.

    Specify [ramfs] to select the specific runner for amd64.

    Set [arm64] to false to exclude from the matrix arm64 architecture.
    *)
let debian_package_release_matrix ?(ramfs = false) ?(arm64 = true) = function
  | Common.Packaging.Partial ->
      [[("RELEASE", ["bookworm"; "trixie"]); ("TAGS", [tag_amd64 ~ramfs])]]
  | Full | Release ->
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
          ("RELEASE", ["22.04"; "24.04"; "26.04"]);
          ("TAGS", tag_amd64 ~ramfs :: (if arm64 then [tag_arm64] else []));
        ];
      ]

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
let cargo_network_hack = "export CARGO_NET_OFFLINE=false"

let make_package_build_job ~target =
  CI.job
    ~image:build_dependency_image
    ~stage:Build
    ~tag:Dynamic
    ~artifacts:(Gitlab_ci.Util.artifacts ["packages/$DISTRIBUTION/$RELEASE"])
    ~script:
      [cargo_network_hack; "./scripts/ci/build-debian-packages.sh " ^ target]

(* data packages. we build them once *)
let job_build_data_packages =
  make_package_build_job
    "oc.build-data_packages"
    ~__POS__
    ~description:"Build the Debian packages that contain Tezos data."
    ~variables:
      [("DISTRIBUTION", "debian"); ("RELEASE", "trixie"); ("TAGS", "gcp")]
    ~target:"zcash"
    []

(* These jobs build the packages in a matrix using the
   build dependencies images *)
let job_build_debian =
  Cacio.parameterize @@ fun pipeline_type ->
  make_package_build_job
    "oc.build-debian"
    ~__POS__
    ~description:"Build the Debian packages for Debian."
    ~variables:[("DISTRIBUTION", "debian"); ("DUNE_BUILD_JOBS", "-j 12")]
    ~parallel:(Matrix (debian_package_release_matrix ~ramfs:true pipeline_type))
    ~sccache:(Cacio.sccache ())
    ~target:"binaries"
    []

let job_build_ubuntu =
  Cacio.parameterize @@ fun pipeline_type ->
  make_package_build_job
    "oc.build-ubuntu"
    ~__POS__
    ~description:"Build the Debian packages for Ubuntu."
    ~variables:[("DISTRIBUTION", "ubuntu"); ("DUNE_BUILD_JOBS", "-j 12")]
    ~parallel:(Matrix (ubuntu_package_release_matrix ~ramfs:true pipeline_type))
    ~sccache:(Cacio.sccache ())
    ~target:"binaries"
    []

let make_apt_repo_job ~pipeline_type ~build_job ~distribution ~releases =
  CI.job
    ~description:
      (sf "Create the apt repository for %s packages and sign it." distribution)
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_data_packages);
        (Artifacts, build_job pipeline_type);
      ]
    ~variables:
      (Common.Packaging.archs_variables pipeline_type
      @ [("GNUPGHOME", "$CI_PROJECT_DIR/.gnupg"); ("PREFIX", "")])
    ~retry:Gitlab_ci.Types.{max = 0; when_ = []}
    ~id_tokens:Tezos_ci.id_tokens
    ~script:
      [
        ". ./scripts/version.sh";
        "apt-get install -y --update apt-utils debsigs";
        "./scripts/ci/create_debian_repo.sh "
        ^ String.concat " " (distribution :: releases);
      ]

let job_apt_repo_debian =
  Cacio.parameterize @@ fun pipeline_type ->
  make_apt_repo_job
    "apt_repo_debian"
    ~__POS__
    ~pipeline_type
    ~build_job:job_build_debian
    ~image:Images.Base_images.debian_trixie
    ~distribution:"debian"
    ~releases:["bookworm"; "trixie"]
    []

let job_apt_repo_ubuntu =
  Cacio.parameterize @@ fun pipeline_type ->
  make_apt_repo_job
    "apt_repo_ubuntu"
    ~__POS__
    ~pipeline_type
    ~build_job:job_build_ubuntu
    ~image:Images.Base_images.ubuntu_24_04
    ~distribution:"ubuntu"
    ~releases:["22.04"; "24.04"; "26.04"]
    []

let make_lintian_job ~distribution ~releases =
  CI.job
    ~stage:Test_publication
    ~description:(sf "Run lintian on %s packages." distribution)
    ~script:
      [
        ". ./scripts/version.sh";
        "export DEBIAN_FRONTEND=noninteractive";
        "apt-get update";
        "apt-get install lintian parallel -y";
        "./scripts/ci/lintian_debian_packages.sh "
        ^ String.concat " " (distribution :: releases);
      ]

let job_lintian_ubuntu =
  Cacio.parameterize @@ fun pipeline_type ->
  make_lintian_job
    "oc.lintian_ubuntu"
    ~__POS__
    ~needs:[(Artifacts, job_build_ubuntu pipeline_type)]
    ~image:Images.Base_images.ubuntu_24_04
    ~distribution:"ubuntu"
    ~releases:["22.04"; "24.04"; "26.04"]
    []

let job_lintian_debian =
  Cacio.parameterize @@ fun pipeline_type ->
  make_lintian_job
    "oc.lintian_debian"
    ~__POS__
    ~needs:[(Artifacts, job_build_debian pipeline_type)]
    ~image:Images.Base_images.debian_bookworm
    ~distribution:"debian"
    ~releases:["bookworm"]
    []

let make_install_bin_job ~distribution ~release =
  CI.job
    ~stage:Test_publication
    ~description:(sf "Check that %s packages can be installed." distribution)
    ~variables:[("PREFIX", "")]
    ~script:
      ["./docs/introduction/install-bin-deb.sh " ^ distribution ^ " " ^ release]

let job_install_bin_ubuntu_22_04 =
  Cacio.parameterize @@ fun pipeline_type ->
  make_install_bin_job
    "oc.install_bin_ubuntu_22_04"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~image:Images.Base_images.ubuntu_22_04
    ~distribution:"ubuntu"
    ~release:"22.04"
    []

let job_install_bin_ubuntu_24_04 =
  Cacio.parameterize @@ fun pipeline_type ->
  make_install_bin_job
    "oc.install_bin_ubuntu_24_04"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~image:Images.Base_images.ubuntu_24_04
    ~distribution:"ubuntu"
    ~release:"24.04"
    []

let job_install_bin_debian_bookworm =
  Cacio.parameterize @@ fun pipeline_type ->
  make_install_bin_job
    "oc.install_bin_debian_bookworm"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~image:Images.Base_images.debian_bookworm
    ~distribution:"debian"
    ~release:"bookworm"
    []

let make_systemd_test_job ~script ~distribution ~release =
  CI.job
    ~stage:Test_publication
    ~image:Images_external.docker
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~variables:
      ([("DOCKER_VERSION", Docker.version)]
      @ make_debian_variables
          distribution
          "systemd"
          release
          Tezos_ci.Images.Base_images.debian_version)
    ~script:
      [
        "./scripts/ci/docker_initialize.sh";
        "./scripts/ci/systemd-packages-test.sh " ^ script
        ^ " images/packages/debian-systemd-tests.Dockerfile";
      ]

let make_systemd_install_job =
  make_systemd_test_job
    ~description:"Check that packages that use systemd can be installed."
    ~script:"scripts/packaging/tests/deb/install-bin-deb.sh"

let make_systemd_upgrade_job =
  make_systemd_test_job
    ~description:"Check the upgrade process in a systemd enabled Docker image."
    ~script:"scripts/packaging/tests/deb/upgrade-systemd-test.sh"

let job_install_bin_ubuntu_24_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_ubuntu_24_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"24.04"
    []

let job_upgrade_bin_ubuntu_22_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_ubuntu_22_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"22.04"
    []

let job_upgrade_bin_ubuntu_24_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_ubuntu_24_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"24.04"
    []

let job_install_bin_debian_bookworm_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_debian_bookworm_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"bookworm"
    []

(* Note: this job is in the publish stage because it depends on a job
   that is in the publish stage, but it is a test.
   Ideally we would build the images in the build stage, test them in the test stage,
   and only then publish them in the publish stage. *)
let job_upgrade_bin_debian_bookworm_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_debian_bookworm-systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"bookworm"
    []

let () =
  (* In merge pipelines we tests only Debian.
     Ubuntu packages are built and tested in the scheduled pipelines. *)
  Cacio.register_jobs
    Debian_partial
    [
      (Auto, job_apt_repo_debian Partial);
      (Auto, job_lintian_debian Partial);
      (Auto, job_install_bin_debian_bookworm Partial);
      (Auto, job_install_bin_debian_bookworm_systemd Partial);
      (Auto, job_upgrade_bin_debian_bookworm_systemd Partial);
    ] ;
  Cacio.register_jobs
    Debian_daily
    [
      (Auto, job_apt_repo_debian Full);
      (Auto, job_apt_repo_ubuntu Full);
      (Auto, job_lintian_ubuntu Full);
      (Auto, job_lintian_debian Full);
      (Auto, job_install_bin_ubuntu_22_04 Full);
      (Auto, job_install_bin_ubuntu_24_04 Full);
      (Auto, job_install_bin_ubuntu_24_04_systemd Full);
      (Auto, job_upgrade_bin_ubuntu_22_04_systemd Full);
      (Auto, job_upgrade_bin_ubuntu_24_04_systemd Full);
      (Auto, job_install_bin_debian_bookworm Full);
      (Auto, job_install_bin_debian_bookworm_systemd Full);
      (Auto, job_upgrade_bin_debian_bookworm_systemd Full);
    ] ;
  ()

let register ~auto ~description pipeline_type =
  let pipeline_name =
    match (pipeline_type, auto) with
    | Common.Packaging.Partial, false -> "debian_repository_partial"
    | Partial, true -> "debian_repository_partial_auto"
    | Full, _ -> "debian_repository_full"
    | Release, _ -> "debian_repository_release"
  in
  Pipeline.register_child
    pipeline_name
    ~description
    ~jobs:(job_datadog_pipeline_trace :: Cacio.get_jobs Debian_partial)

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
