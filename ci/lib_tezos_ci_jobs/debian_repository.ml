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

(* Types for the repository pipelines.
   - Release: we run all the release jobs, but no tests
   - Partial: we run only a subset of the tests jobs
   - Full: we run the complete test matrix *)
type repository_pipeline = Full | Partial | Release

(** Return a tuple (ARCHITECTURES, <archs>) based on the type
    of repository pipeline. *)
let archs_variables pipeline =
  let archs : Runner.Arch.t list =
    match pipeline with Partial -> [Amd64] | Full | Release -> [Amd64; Arm64]
  in
  [
    ( "ARCHITECTURES",
      String.concat " " (List.map Runner.Arch.show_uniform archs) );
  ]

let tag_amd64 ~ramfs =
  if ramfs then Runner.Tag.show Gcp_very_high_cpu_ramfs
  else Runner.Tag.show Gcp_very_high_cpu

let tag_arm64 = Runner.Tag.show Gcp_arm64

(** Debian releases tested per pipeline type.

    Partial pipelines (merge requests) only test the current Debian
    stable; Full and Release pipelines test both supported releases. *)
let debian_releases : repository_pipeline -> string list = function
  | Partial -> ["trixie"]
  | Full | Release -> ["bookworm"; "trixie"]

(* Job names use [_] instead of [.] in releases (e.g. [22_04] for
   Ubuntu [22.04]). Debian codenames have no dots so this is a no-op
   for them. *)
let release_token release = String.map (function '.' -> '_' | c -> c) release

(** These are the set of Debian release-architecture combinations for
    which we build deb packages in the job [oc.build-debian].
    A dependency image will be built once for each combination of [RELEASE] and [TAGS].

    If [release_pipeline] is false, we only tests a subset of the matrix,
    one release, and one architecture.

    Specify [ramfs] to select the specific runner for amd64.

    Set [arm64] to false to exclude from the matrix arm64 architecture.
    *)
let debian_package_release_matrix ?(ramfs = false) ?(arm64 = true) pipeline_type
    =
  let tags =
    match pipeline_type with
    | Partial -> [tag_amd64 ~ramfs]
    | Full | Release -> tag_amd64 ~ramfs :: (if arm64 then [tag_arm64] else [])
  in
  [[("RELEASE", debian_releases pipeline_type); ("TAGS", tags)]]

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
  | Partial -> [[("RELEASE", ["22.04"]); ("TAGS", [tag_amd64 ~ramfs])]]
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

(* keyring package. Architecture: all, built once. *)
let job_build_keyring_package =
  make_package_build_job
    "oc.build-keyring_package"
    ~__POS__
    ~description:"Build the octez-archive-keyring Debian package."
    ~variables:
      [("DISTRIBUTION", "debian"); ("RELEASE", "trixie"); ("TAGS", "gcp")]
      (* The build sources repository-keys.sh, which fetches the production
         public signing key(s) from GCP on protected branches. That fetch
         authenticates to GCP via Workload Identity Federation, which needs the
         GCP_ID_TOKEN OIDC token generated by this id_tokens section. *)
    ~id_tokens:Tezos_ci.id_tokens
    ~target:"keyring"

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

let make_apt_repo_job ~pipeline_type ~build_job ~distribution ~releases =
  CI.job
    ~only_if_changed:
      (Tezos_ci.Changeset.encode Changesets.changeset_debian_packages)
    ~description:
      (sf "Create the apt repository for %s packages and sign it." distribution)
    ~stage:Publish
    ~needs:
      [
        (Artifacts, job_build_data_packages);
        (Artifacts, job_build_keyring_package);
        (Artifacts, build_job pipeline_type);
      ]
    ~variables:
      (archs_variables pipeline_type
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
    ~releases:(debian_releases pipeline_type)

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

let make_lintian_job ~distribution ~releases =
  CI.job
    ~only_if_changed:
      (Tezos_ci.Changeset.encode Changesets.changeset_debian_packages)
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

let job_lintian_debian =
  Cacio.parameterize @@ fun pipeline_type ->
  make_lintian_job
    "oc.lintian_debian"
    ~__POS__
    ~needs:[(Artifacts, job_build_debian pipeline_type)]
    ~image:Images.Base_images.debian_trixie
    ~distribution:"debian"
    ~releases:(debian_releases pipeline_type)

(* Rebuild the Debian binary, data and keyring packages for trixie/amd64 and
   check, with diffoscope, that they are byte-for-byte identical to the packages
   already built by [job_build_debian], [job_build_data_packages] and
   [job_build_keyring_package] (pulled in as artifacts). Reusing the existing
   builds as the reference means we only pay for one extra build.

   This job only ever runs in the scheduled [Debian_daily] pipeline (registered
   with [Full] below). It checks a single distribution/architecture pair, pinned
   by the DISTRIBUTION and RELEASE variables (debian/trixie) and the amd64 tag;
   reproducibility of that pair is taken as representative of the other
   combinations. The [pipeline_type] parameter only exists to thread the right
   matrix variant into the [job_build_debian] dependency. *)
let job_reproducibility_debian =
  Cacio.parameterize @@ fun pipeline_type ->
  CI.job
    "oc.reproducibility_debian"
    ~__POS__
    ~description:
      "Rebuild the Debian packages and check they are byte-for-byte identical \
       to the first build (reproducible builds)."
    ~image:build_dependency_image
    ~stage:Test_publication
    ~tag:Dynamic
    ~needs:
      [
        (Artifacts, job_build_debian pipeline_type);
        (Artifacts, job_build_data_packages);
        (Artifacts, job_build_keyring_package);
      ]
    ~variables:
      [
        ("DISTRIBUTION", "debian");
        ("RELEASE", "trixie");
        ("TAGS", tag_amd64 ~ramfs:true);
        ("DUNE_BUILD_JOBS", "-j 12");
      ]
    ~sccache:(Cacio.sccache ())
    ~script:[cargo_network_hack; "./scripts/ci/test-debian-reproducibility.sh"]

let make_install_bin_job ~distribution ~release =
  CI.job
    ~only_if_changed:
      (Tezos_ci.Changeset.encode Changesets.changeset_debian_packages)
    ~stage:Test_publication
    ~description:(sf "Check that %s packages can be installed." distribution)
    ~variables:[("PREFIX", "")]
    ~script:
      ["./docs/introduction/install-bin-deb.sh " ^ distribution ^ " " ^ release]

let job_install_bin ~distribution ~release ~image ~apt_repo =
  Cacio.parameterize @@ fun pipeline_type ->
  make_install_bin_job
    (sf "oc.install_bin_%s_%s" distribution (release_token release))
    ~__POS__
    ~needs:[(Job, apt_repo pipeline_type)]
    ~image
    ~distribution
    ~release

let job_install_bin_ubuntu_22_04 =
  job_install_bin
    ~distribution:"ubuntu"
    ~release:"22.04"
    ~image:Images.Base_images.ubuntu_22_04
    ~apt_repo:job_apt_repo_ubuntu

let job_install_bin_ubuntu_24_04 =
  job_install_bin
    ~distribution:"ubuntu"
    ~release:"24.04"
    ~image:Images.Base_images.ubuntu_24_04
    ~apt_repo:job_apt_repo_ubuntu

let job_install_bin_ubuntu_26_04 =
  job_install_bin
    ~distribution:"ubuntu"
    ~release:"26.04"
    ~image:Images.Base_images.ubuntu_26_04
    ~apt_repo:job_apt_repo_ubuntu

let job_install_bin_debian_bookworm =
  job_install_bin
    ~distribution:"debian"
    ~release:"bookworm"
    ~image:Images.Base_images.debian_bookworm
    ~apt_repo:job_apt_repo_debian

let job_install_bin_debian_trixie =
  job_install_bin
    ~distribution:"debian"
    ~release:"trixie"
    ~image:Images.Base_images.debian_trixie
    ~apt_repo:job_apt_repo_debian

let make_systemd_test_job ~script ~distribution ~release =
  CI.job
    ~only_if_changed:
      (Tezos_ci.Changeset.encode Changesets.changeset_debian_packages)
    ~stage:Test_publication
    ~image:Images.Base_images.alpine_docker_ci
    ~services:[{name = Images.Base_images.dind_service}]
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

let make_systemd_keyring_test_job =
  make_systemd_test_job
    ~description:"Check that the keyring package works for APT authentication."
    ~script:"scripts/packaging/tests/deb/test-keyring.sh"

let job_install_bin_ubuntu_24_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_ubuntu_24_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"24.04"

let job_install_bin_ubuntu_26_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_ubuntu_26_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"26.04"

let job_upgrade_bin_ubuntu_22_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_ubuntu_22_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"22.04"

let job_upgrade_bin_ubuntu_24_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_ubuntu_24_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"24.04"

let job_upgrade_bin_ubuntu_26_04_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_ubuntu_26_04_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"26.04"

let job_install_bin_debian_bookworm_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_debian_bookworm_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"bookworm"

let job_install_bin_debian_trixie_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_install_job
    "oc.install_bin_debian_trixie_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"trixie"

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

let job_test_keyring_debian_bookworm =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_keyring_test_job
    "oc.test_keyring_debian_bookworm"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"bookworm"

let job_test_keyring_debian_trixie =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_keyring_test_job
    "oc.test_keyring_debian_trixie"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"trixie"

let job_upgrade_bin_debian_trixie_systemd =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_upgrade_job
    "oc.upgrade_bin_debian_trixie_systemd"
    ~__POS__
    ~needs:[(Job, job_apt_repo_debian pipeline_type)]
    ~distribution:"debian"
    ~release:"trixie"

let job_test_keyring_ubuntu_22_04 =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_keyring_test_job
    "oc.test_keyring_ubuntu_22_04"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"22.04"

let job_test_keyring_ubuntu_24_04 =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_keyring_test_job
    "oc.test_keyring_ubuntu_24_04"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"24.04"

let job_test_keyring_ubuntu_26_04 =
  Cacio.parameterize @@ fun pipeline_type ->
  make_systemd_keyring_test_job
    "oc.test_keyring_ubuntu_26_04"
    ~__POS__
    ~needs:[(Job, job_apt_repo_ubuntu pipeline_type)]
    ~distribution:"ubuntu"
    ~release:"26.04"

let () =
  (* Register the Debian partial jobs directly into before_merging and
     merge_train pipelines with only_if_changed so they run automatically
     only when relevant files change. *)
  Cacio.register_merge_request_jobs
    [
      (Auto, job_apt_repo_debian Partial);
      (Auto, job_lintian_debian Partial);
      (Auto, job_install_bin_debian_bookworm Partial);
      (Auto, job_install_bin_debian_bookworm_systemd Partial);
      (Auto, job_upgrade_bin_debian_bookworm_systemd Partial);
      (Auto, job_test_keyring_debian_bookworm Partial);
    ] ;
  (* In merge pipelines we tests only Debian.
     Ubuntu packages are built and tested in the scheduled pipelines. *)
  Cacio.register_jobs
    Debian_partial
    [
      (Auto, job_apt_repo_debian Partial);
      (Auto, job_lintian_debian Partial);
      (Auto, job_install_bin_debian_trixie Partial);
      (Auto, job_install_bin_debian_trixie_systemd Partial);
      (Auto, job_upgrade_bin_debian_trixie_systemd Partial);
      (Auto, job_test_keyring_debian_trixie Partial);
    ] ;
  Cacio.register_jobs
    Debian_daily
    [
      (Auto, job_apt_repo_debian Full);
      (Auto, job_apt_repo_ubuntu Full);
      (Auto, job_reproducibility_debian Full);
      (Auto, job_lintian_ubuntu Full);
      (Auto, job_lintian_debian Full);
      (Auto, job_install_bin_ubuntu_22_04 Full);
      (Auto, job_install_bin_ubuntu_24_04 Full);
      (Auto, job_install_bin_ubuntu_26_04 Full);
      (Auto, job_install_bin_ubuntu_24_04_systemd Full);
      (Auto, job_install_bin_ubuntu_26_04_systemd Full);
      (Auto, job_upgrade_bin_ubuntu_22_04_systemd Full);
      (Auto, job_upgrade_bin_ubuntu_24_04_systemd Full);
      (Auto, job_upgrade_bin_ubuntu_26_04_systemd Full);
      (Auto, job_install_bin_debian_bookworm Full);
      (Auto, job_install_bin_debian_trixie Full);
      (Auto, job_install_bin_debian_bookworm_systemd Full);
      (Auto, job_install_bin_debian_trixie_systemd Full);
      (Auto, job_upgrade_bin_debian_bookworm_systemd Full);
      (Auto, job_upgrade_bin_debian_trixie_systemd Full);
      (Auto, job_test_keyring_debian_bookworm Full);
      (Auto, job_test_keyring_debian_trixie Full);
      (Auto, job_test_keyring_ubuntu_22_04 Full);
      (Auto, job_test_keyring_ubuntu_24_04 Full);
      (Auto, job_test_keyring_ubuntu_26_04 Full);
    ] ;
  ()

let register ~auto ~description pipeline_type =
  let pipeline_name =
    match (pipeline_type, auto) with
    | Partial, false -> "debian_repository_partial"
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
