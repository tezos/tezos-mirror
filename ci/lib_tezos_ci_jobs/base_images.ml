(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
module CI = Cacio.Shared

(* Helper function to standardise the path of a base image built in the same
   pipeline. Used to build more complex base images and avoid code duplications *)
let base_dep_img_name image =
  let prefix = "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos" in
  let tag = "${RELEASE}-${CI_COMMIT_REF_SLUG}" in
  Format.sprintf "%s/%s:%s" prefix image tag

type compilation =
  | Amd64_only (* Built on amd64 runner *)
  | Arm64_only
    (* Built on arm64 runner. The image will be suffixed with -arm64 *)
  | Emulated
    (* Built on amd64 runners. Arm64 image is built using qumu.
       Use 1 runner / 1 job to create one multi-arch image *)
  | Native
(* Both amd64 and arm64 images are built on their respective native architectures
   Use 3 runners. 2 jobs for building the images, one job for merging the manifest
   must be added to create one multi-arch image. Both images are suffixed,
   repesctively, with -amd64 or -arm64 *)

type upstream_image = Pipeline_dep of string | Upstream of string
(* Datatype to differenciate between variable passed with IMAGE_PATH to the
   dockerfile via --build-arg IMAGE=IMAGE_PATH.
   Upstream is the name of an upstream image ( eg. debian:trixie )
   Pipeline_dep is the name of an image generate in this pipeline ( eg.
   ${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/debian:trixie-$COMMIT_REF_SLUG )
*)

module Files = struct
  let build_script = ["scripts/ci/build-base-images.sh"]

  (* Used in jobs merging manifests of natively build Docker images. *)
  let merge_script = ["scripts/ci/docker-merge-base-images.sh"]

  (* Direct changesets of jobs, i.e. files directly used by the corresponding jobs.

     The full changeset of a job should also contain the files indirectly used.
     - Example: [image A] is built on top of [image B], then the
     changeset of the image A build ([job A])job should contain the
     files needed to build B.


     Also, the full changeset of a job should contain the changeset of the jobs that depend on it.
     - Example: [image A] is built on top of [image B] so [job A] depends on
     [job B]. If [job A] is in the pipeline then [job B] should be too. So, the
     full changeset of [job B] needs to include the changeset of [job A].
     NB: This is enforced by Cacio, so this part will be simplified if the jobs are migrated.

   *)

  let rpm_base =
    [
      "images/base-images/Dockerfile.rpm";
      (* scripts used in Dockerfile *)
      "scripts/kiss-fetch.sh";
      "images/scripts/configure_rpm_proxy.sh";
      "images/scripts/install_datadog_static.sh";
      "images/scripts/install-gcloud-dnf.sh";
    ]
    @ build_script

  let debian_base =
    [
      "images/base-images/Dockerfile.debian";
      (* scripts in Dockerfile *)
      "scripts/kiss-fetch.sh";
      "images/scripts/install_datadog_static.sh";
      "images/scripts/install-gcloud.sh";
    ]
    @ build_script

  let debian_homebrew =
    [
      "images/base-images/Dockerfile.debian-homebrew";
      (* script used in Dockerfile *)
      "scripts/packaging/homebrew_install.sh";
    ]
    @ build_script

  let debian_build =
    [
      "images/base-images/Dockerfile.debian-build";
      (* files copied in Dockerfile except Makefile as many changes to
         it should be irrelevant to debian packages *)
      "images/scripts/install_sccache_static.sh";
      "images/scripts/install_opam_static.sh";
      "scripts/kiss-fetch.sh";
      "scripts/kiss-logs.sh";
      "scripts/version.sh";
      "scripts/install_build_deps.sh";
      "scripts/install_build_deps.rust.sh";
      "opam/virtual/octez-deps.opam.locked";
      "opam/virtual/release-tools-deps.opam.locked";
      "opam/virtual/dream-httpaf.opam.locked";
      "opam/virtual/dream.opam.locked";
    ]
    @ build_script

  let debian_systemd =
    [
      "images/base-images/Dockerfile.debian-systemd";
      (* scripts in Dockerfile *)
      "scripts/ci/systemd-entrypoint.service";
      "scripts/ci/systemd-entrypoint.sh";
      "scripts/ci/octez-packages-version.sh";
      "scripts/packaging/tests/tests-common.inc.sh";
      "script-inputs/active_protocol_versions_without_number";
    ]
    @ build_script

  let debian_jsonnet =
    ["images/base-images/Dockerfile.debian-jsonnet"] @ build_script

  let debian_rust_build =
    [
      "images/base-images/Dockerfile.rust";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
    ]
    @ build_script

  let rust_sdk_bindings =
    [
      "images/base-images/Dockerfile.rust-sdk-bindings";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
    ]
    @ build_script

  let alpine_docker_ci =
    [
      "images/base-images/Dockerfile.alpine-docker-ci";
      "images/scripts/install-gcloud-apk.sh";
      "scripts/ci/docker_initialize.sh";
      "images/scripts/install_datadog_static.sh";
    ]
    @ build_script

  let ci_releases =
    [
      "scripts/kiss-fetch.sh";
      "images/scripts/install-release-cli.sh";
      "images/scripts/install_datadog_static.sh";
      "images/scripts/install-gcloud.sh";
      "images/base-images/Dockerfile.debian-release";
    ]
end

module Distribution = struct
  type t = Debian | Ubuntu | Fedora | Rockylinux

  let name = function
    | Debian -> "debian"
    | Ubuntu -> "ubuntu"
    | Fedora -> "fedora"
    | Rockylinux -> "rockylinux"

  let releases = function
    | Debian -> ["bookworm"; "trixie"]
    | Ubuntu -> ["22.04"; "24.04"; "26.04"]
    | Fedora -> ["39"; "42"]
    | Rockylinux -> ["9"; "10"]

  let release_matrix distro = [("RELEASE", releases distro)]

  let dockerfile = function
    | Debian | Ubuntu -> "images/base-images/Dockerfile.debian"
    | Fedora | Rockylinux -> "images/base-images/Dockerfile.rpm"

  let base_changeset = function
    | Debian | Ubuntu -> Files.debian_base
    | Fedora | Rockylinux -> Files.rpm_base
end

(* Set to [true] to include RPM-based distribution images (Fedora, Rockylinux)
   in the pipeline. Set to [false] to deprecate them.
   See https://gitlab.com/tezos/tezos/-/work_items/8205 *)
let enable_rpm_images = false

(* ── Cacio helpers ───────────────────────────────────────────────────────── *)

(* Partial application of [CI.job] that fixes the docker-auth settings:
   alpine_docker_ci image, dind service, DOCKER_VERSION variable, and
   docker_initialize.sh prepended to the script (Cacio has no before_script).
   All other [CI.job] arguments are left for the caller. *)
let docker_job ?(extra_variables = []) ~script =
  CI.job
    ~stage:Cacio.Build
    ~image:Images.Base_images.alpine_docker_ci
    ~services:[{name = Images.Base_images.dind_service}]
    ~variables:
      (("DOCKER_VERSION", Images.Base_images.docker_version) :: extra_variables)
    ~script:("./scripts/ci/docker_initialize.sh" :: script)

(* Partial application of [docker_job] that fixes the image-build variables
   (DISTRIBUTION, IMAGE_PATH, PLATFORM) and the compilation strategy
   (tag and matrix). All other arguments are left for the caller. *)
let base_image_job ~image_name ?(base_name = Upstream image_name) ~matrix
    ~compilation ?(extra_variables = []) dockerfile =
  let platform, extra_tags =
    match compilation with
    | Amd64_only -> ("linux/amd64", [])
    | Arm64_only -> ("", [("TAGS", [Runner.Tag.show Gcp_arm64])])
    | Emulated -> ("linux/amd64,linux/arm64", [])
    | Native ->
        ( "",
          [
            ( "TAGS",
              [Runner.Tag.show Gcp_very_high_cpu; Runner.Tag.show Gcp_arm64] );
          ] )
  in
  let emulated = extra_tags = [] in
  docker_job
    ~extra_variables:
      ([
         ("DISTRIBUTION", image_name);
         ( "IMAGE_PATH",
           match base_name with
           | Upstream name -> Format.asprintf "%s:$RELEASE" name
           | Pipeline_dep name -> base_dep_img_name name );
         ("PLATFORM", platform);
       ]
      @ extra_variables)
    ~script:[Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile]
    ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
    ~parallel:(Matrix [matrix @ extra_tags])

(* ── CIAO helpers (used until all jobs are migrated) ─────────────────────── *)

(* This function can build docker images both in an emulated environment using
   qemu or natively. The advantage of choosing emulated vs native depends on
   the build time associated with the image. Small images are more efficiently
   built in an emulated environment, while larger images are better build
   natively.

   [name] is the name of the job.

   [matrix] is a parallel/matrix gitlab construct. Here we use it with the
   RELEASE variable to build multiple images for the same distribution, but
   different releases. This parameter is optional: Some images do no need this.

   [image_name] is the name of the final docker image.

   [base_name] is the name of the image upon the newly created image is FROM.
   It can be either the upstream image, or an image generated in this pipeline.

   [compilation] determines the type of the image.
   If [compilation] parameter is either set to [Emulated] or omitted then we
   build for two different architectures using qemu. This handles both build
   and merging the manifest of the images.

   If [compilation] is either [ Amd64_only ] or [ Arm64_only ] we build the
   images natively, but the arm64 image is going to be suffixed with "-arm64".

   If [compilation] is set to [Native] we build for both architectures using
   a native runner. In this case we also must add a merge manifest job.

   [start_job] adds a dependency to [trigger] in [before_merging] pipelines.
   [changeset] should be [true] for [before_merging]/[merge_train] only. *)
let make_job_base_images ?start_job ?(changeset = false) ~__POS__ ?(matrix = [])
    ~image_name ?(base_name = Upstream image_name)
    ?(changes = Changeset.make []) ?(compilation = Emulated) ?(variables = [])
    ?dependencies dockerfile =
  let script = Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile in
  (* if provided, add [start_job] to the dependencies. *)
  let dependencies =
    match start_job with
    | None -> dependencies
    | Some job -> (
        match dependencies with
        | None -> Some (Dependent [Job job])
        | Some (Dependent lst) -> Some (Dependent (Job job :: lst))
        | Some (Staged _) ->
            failwith
              "Only job dependencies in base_image jobs. Stage dependencies \
               are not allowed.")
  in
  (* cf. [scripts/ci/build-base-images.sh] for more details on the coupling between $PLATFORM and $TAGS *)
  let platform, tags =
    match compilation with
    | Amd64_only -> ("linux/amd64", [])
    | Arm64_only -> ("", [("TAGS", [Runner.Tag.show Gcp_arm64])])
    | Emulated -> ("linux/amd64,linux/arm64", []) (* default *)
    | Native ->
        ( "",
          [
            ( "TAGS",
              [Runner.Tag.show Gcp_very_high_cpu; Runner.Tag.show Gcp_arm64] );
          ] )
  in
  let emulated = tags = [] in
  let variables =
    [
      ("DISTRIBUTION", image_name);
      (* if the base name is passed explicitely, then we assume is a
         fully qualified image, otherwise we add the release component
         to the image name *)
      ( "IMAGE_PATH",
        match base_name with
        | Upstream name -> Format.asprintf "%s:$RELEASE" name
        | Pipeline_dep name -> base_dep_img_name name );
      ("PLATFORM", platform);
    ]
    @ variables
  in
  job_docker_authenticated
    ~__POS__
    ~name:("images." ^ image_name)
    ~stage:Stages.build
    ~variables
    ~rules:
      (if changeset then
         [job_rule ~changes:(Changeset.encode changes) ~when_:On_success ()]
       (* To force the run of the job. A bit hackish but simpler
          than to have no rule and consistent with what is done in
          [code_verification]. Will be done cleanly when migrated to
          Cacio. *)
         else [job_rule ~when_:On_success ()])
    ~parallel:(Matrix [matrix @ tags])
    ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
    ?dependencies
    [script]

(* Specialisation of [make_job_base_images] for distribution images.
   - [base_name] used only for Rockylinux.
   - if [changes] is not provided, we use the base changeset of the
   distribution. This applies to base images that are not a
   dependency of other images.  *)
let make_job_base_image_distribution ?start_job ?(changeset = false) ?base_name
    ?changes distro =
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~matrix:(Distribution.release_matrix distro)
    ~image_name:(Distribution.name distro)
    ?base_name
    ~changes:
      (* except for Debian, job changeset is the [Distribution.base_changeset] *)
      (match changes with
      | None -> Changeset.make @@ Distribution.base_changeset distro
      | Some changes -> changes)
    (Distribution.dockerfile distro)

(* ── Base distribution jobs ─────────────────────────────────────────────── *)

let make_job_debian_based_images ?start_job ?(changeset = false) () =
  let changes =
    (* we need the [debian] base job if we have jobs of
       images based on top of it *)
    Changeset.make
      (Files.debian_base @ Files.debian_homebrew @ Files.debian_rust_build
     @ Files.merge_script @ Files.debian_build @ Files.merge_script
     @ Files.debian_systemd @ Files.debian_jsonnet @ Files.rust_sdk_bindings)
  in
  make_job_base_image_distribution
    ?start_job
    ~changeset
    ~changes
    Distribution.Debian

let make_job_ubuntu_based_images ?start_job ?(changeset = false) () =
  let changes =
    Changeset.make
      (Files.debian_base @ Files.debian_build @ Files.merge_script
     @ Files.debian_systemd)
  in
  make_job_base_image_distribution
    ?start_job
    ~changeset
    ~changes
    Distribution.Ubuntu

let make_job_fedora_based_images ?start_job ?(changeset = false) () =
  make_job_base_image_distribution ?start_job ~changeset Distribution.Fedora

let make_job_rockylinux_based_images ?start_job ?(changeset = false) () =
  make_job_base_image_distribution
    ?start_job
    ~changeset
    ~base_name:(Upstream "rockylinux/rockylinux")
    Distribution.Rockylinux

(* ── Standalone jobs (no job dependencies within this file) ─────────────── *)

(* ── Cacio: ci-release ───────────────────────────────────────────────────── *)

let job_ci_release_based_images =
  base_image_job
    ~image_name:"ci-release"
    ~base_name:(Upstream "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    "images/base-images/Dockerfile.debian-release"
    ~__POS__
    ~description:"Build ci-release base images"
    ~only_if_changed:Files.(ci_releases @ debian_base)
    "images.ci-release"

(* This base image differs from the others in its build process: it is
   bootstrapped directly from the upstream Docker Hub [docker:<version>]
   image (declared in {!Images_external.upstream_docker}) rather than from
   another internal base image (e.g. [debian:<release>]).
   The resulting image includes the Docker daemon, gcloud CLI, hadolint
   and regctl — it is the standard Docker-in-Docker image for CI jobs.
   The changeset is also different to ensure this image is always up-to-date *)
let make_job_docker_ci_based_images ?start_job ?(changeset = false) () =
  let docker_version = Images.Base_images.docker_version in
  let variables =
    [
      ("RELEASE", docker_version);
      ("DISTRIBUTION", "alpine-docker-ci");
      ("IMAGE_PATH", "");
      ("GCLOUD_VERSION", "543.0.0");
      ("HADOLINT_VERSION", "2.10.0");
      ("DOCKER_VERSION", docker_version);
      ("REGCTL_VERSION", "v0.4.3");
      ("PLATFORM", "linux/amd64,linux/arm64");
      ("CI_DOCKER_HUB", "false");
    ]
  in
  job
    ~rules:
      (if changeset then
         [
           job_rule
             ~changes:Changeset.(encode (make Files.alpine_docker_ci))
             ~when_:On_success
             ();
         ]
       else [job_rule ~when_:On_success ()])
    ~tag:Gcp_very_high_cpu
    ~__POS__
    ~image:Images.upstream_docker
    ~variables
    ~services:[{name = Images.Base_images.dind_service}]
    ~stage:Stages.build
    ?dependencies:(Option.map (fun j -> Dependent [Job j]) start_job)
    ~name:"images.alpine-docker-ci"
    [
      (* minimal set of tools needed to bootstrap the docker-ci image *)
      "images/scripts/install-gcloud-apk.sh";
      "export PATH=$PATH:/google-cloud-sdk/bin";
      "scripts/ci/docker_initialize.sh";
      "scripts/ci/build-base-images.sh \
       images/base-images/Dockerfile.alpine-docker-ci";
    ]

(* ── Rust family ────────────────────────────────────────────────────────── *)

(* debian-rust: based on [debian:trixie]. *)
let make_job_rust_based_images ?start_job ?(changeset = false) () =
  (* dep is called with defaults just to get the job name for the dependency reference;
     CIAO only uses the name *)
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-rust"
    ~base_name:(Pipeline_dep "debian")
    ~matrix:[("RELEASE", ["trixie"])]
    ~dependencies:(Dependent [Job dep_debian])
    ~compilation:Native
    ~changes:
      (Changeset.make
         (Files.debian_rust_build
        (* Adding the changeset of debian job as we want to test the
           build of [debian-rust] if [debian] is rebuild. *)
        @ Files.debian_base
         (* If we run [debian-rust] merge job, we need [debian-rust] build job *)
         @ Files.merge_script))
    "images/base-images/Dockerfile.rust"

(* dedicated merge job exist because QEMU compilation takes too much
   time. Build job reached timeout. *)
let make_job_rust_based_images_merge ?(changeset = false) () =
  (* dep is called with defaults just to get the job name for the dependency reference *)
  let dep_rust = make_job_rust_based_images () in
  job_docker_authenticated
    ~__POS__
    ~name:"images.debian-rust.merge"
    ~stage:Stages.build
    ~dependencies:(Dependent [Job dep_rust])
    ~rules:
      (if changeset then
         [
           job_rule
             ~changes:
               (Changeset.encode
                  (Changeset.make
                     (Files.merge_script
                    (* Adding changesets of [debian] and
                      [debian-rust] build jobs as if we rebuild one
                      of these images, we want to test the
                      [debian-rust] merge job *)
                    @ Files.debian_rust_build
                     @ Files.debian_base)))
             ~when_:On_success
             ();
         ]
       (* To force the run of the job. Similar to
          [make_job_base_images] and cf. comment above.
          Migration to Cacio will clean this hack. *)
         else [job_rule ~when_:On_success ()])
    ~variables:
      [
        ("RELEASE", "trixie");
        ("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-rust");
      ]
    ["scripts/ci/docker-merge-base-images.sh"]

(* ── debian-homebrew ────────────────────────────────────────────────────── *)

(* debian-homebrew: based on [debian:trixie] *)
let make_job_debian_homebrew_base_images ?start_job ?(changeset = false) () =
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-homebrew"
    ~base_name:(Pipeline_dep "debian")
    ~dependencies:(Dependent [Job dep_debian])
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
      (* Adding the changeset of [debian] job as we want to test the
       build of [debian-homebrew] if [debian] is rebuild. *)
    ~changes:(Changeset.make (Files.debian_homebrew @ Files.debian_base))
    "images/base-images/Dockerfile.debian-homebrew"

(* ── debian-build and ubuntu-build families ─────────────────────────────── *)

let make_job_debian_build_base_images ?start_job ?(changeset = false) () =
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-build"
    ~base_name:(Pipeline_dep "debian")
    ~dependencies:(Dependent [Job dep_debian])
    ~matrix:Distribution.(release_matrix Debian)
    ~compilation:Native
    ~changes:
      (Changeset.make
         (Files.debian_build @ Files.debian_base @ Files.merge_script))
    "images/base-images/Dockerfile.debian-build"

let make_job_debian_build_base_images_merge ?(changeset = false) () =
  let dep_build = make_job_debian_build_base_images () in
  job_docker_authenticated
    ~__POS__
    ~name:"images.debian-build.merge"
    ~stage:Stages.build
    ~dependencies:(Dependent [Job dep_build])
    ~rules:
      (if changeset then
         [
           job_rule
             ~changes:
               (Changeset.encode
                  (Changeset.make
                     (Files.merge_script @ Files.debian_build
                    @ Files.debian_base)))
             ~when_:On_success
             ();
         ]
       else [job_rule ~when_:On_success ()])
    ~parallel:(Matrix [Distribution.(release_matrix Debian)])
    ~variables:[("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-build")]
    ["scripts/ci/docker-merge-base-images.sh"]

let make_job_ubuntu_build_base_images ?start_job ?(changeset = false) () =
  let dep_ubuntu = make_job_ubuntu_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"ubuntu-build"
    ~base_name:(Pipeline_dep "ubuntu")
    ~dependencies:(Dependent [Job dep_ubuntu])
    ~matrix:Distribution.(release_matrix Ubuntu)
    ~compilation:Native
    ~changes:
      (Changeset.make
         (Files.debian_build @ Files.debian_base @ Files.merge_script))
    "images/base-images/Dockerfile.debian-build"

let make_job_ubuntu_build_base_images_merge ?(changeset = false) () =
  let dep_build = make_job_ubuntu_build_base_images () in
  job_docker_authenticated
    ~__POS__
    ~name:"images.ubuntu-build.merge"
    ~stage:Stages.build
    ~dependencies:(Dependent [Job dep_build])
    ~rules:
      (if changeset then
         [
           job_rule
             ~changes:
               (Changeset.encode
                  (Changeset.make
                     (Files.merge_script @ Files.debian_build
                    @ Files.debian_base)))
             ~when_:On_success
             ();
         ]
       else [job_rule ~when_:On_success ()])
    ~parallel:(Matrix [Distribution.(release_matrix Ubuntu)])
    ~variables:[("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/ubuntu-build")]
    ["scripts/ci/docker-merge-base-images.sh"]

(* ── Systemd images ──────────────────────────────────────────────────────── *)

let make_job_debian_systemd_base_images ?start_job ?(changeset = false) () =
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-systemd"
    ~base_name:(Pipeline_dep "debian")
    ~dependencies:(Dependent [Job dep_debian])
    ~matrix:Distribution.(release_matrix Debian)
    ~compilation:Amd64_only
    ~changes:(Changeset.make (Files.debian_systemd @ Files.debian_base))
    "images/base-images/Dockerfile.debian-systemd"

let make_job_ubuntu_systemd_base_images ?start_job ?(changeset = false) () =
  let dep_ubuntu = make_job_ubuntu_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"ubuntu-systemd"
    ~base_name:(Pipeline_dep "ubuntu")
    ~dependencies:(Dependent [Job dep_ubuntu])
    ~matrix:Distribution.(release_matrix Ubuntu)
    ~compilation:Amd64_only
    ~changes:(Changeset.make (Files.debian_systemd @ Files.debian_base))
    "images/base-images/Dockerfile.debian-systemd"

(* ── debian-jsonnet ──────────────────────────────────────────────────────── *)

(* debian-jsonnet: based on [debian:trixie] *)
let make_job_jsonnet_base_images ?start_job ?(changeset = false) () =
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-jsonnet"
    ~base_name:(Pipeline_dep "debian")
    ~dependencies:(Dependent [Job dep_debian])
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    ~changes:(Changeset.make (Files.debian_jsonnet @ Files.debian_base))
    "images/base-images/Dockerfile.debian-jsonnet"

(* ── rust-sdk-bindings ───────────────────────────────────────────────────── *)

(* rust-sdk-bindings: based on [debian:trixie] *)
let make_job_rust_sdk_bindings_base_images ?start_job ?(changeset = false) () =
  let dep_debian = make_job_debian_based_images () in
  make_job_base_images
    ?start_job
    ~changeset
    ~__POS__
    ~image_name:"debian-rust-sdk-bindings"
    ~base_name:(Pipeline_dep "debian")
    ~dependencies:(Dependent [Job dep_debian])
    ~matrix:[("RELEASE", ["trixie"])]
    ~compilation:Amd64_only
    ~changes:(Changeset.make (Files.rust_sdk_bindings @ Files.debian_base))
    "images/base-images/Dockerfile.rust-sdk-bindings"

(* ── Assembly ────────────────────────────────────────────────────────────── *)

(* [start_job] adds a dependency to [trigger] in [before_merging] pipelines.
   [changeset] should be set to [true] for [before_merging]/[merge_train]
   parent pipelines only. Changesets should be ignored for other pipelines:
   - branch pipelines such as [base_images.daily] because they are then
     ignored by GitLab;
   - in the manually triggered base images child pipeline as we may want to
     trigger all base images jobs. *)
let jobs ?start_job ?(changeset = false) () =
  [
    make_job_debian_based_images ?start_job ~changeset ();
    make_job_ubuntu_based_images ?start_job ~changeset ();
    make_job_rust_based_images ?start_job ~changeset ();
    make_job_rust_based_images_merge ~changeset ();
    make_job_debian_homebrew_base_images ?start_job ~changeset ();
    make_job_docker_ci_based_images ?start_job ~changeset ();
    make_job_debian_build_base_images ?start_job ~changeset ();
    make_job_debian_build_base_images_merge ~changeset ();
    make_job_ubuntu_build_base_images ?start_job ~changeset ();
    make_job_ubuntu_build_base_images_merge ~changeset ();
    make_job_debian_systemd_base_images ?start_job ~changeset ();
    make_job_ubuntu_systemd_base_images ?start_job ~changeset ();
    make_job_jsonnet_base_images ?start_job ~changeset ();
    make_job_rust_sdk_bindings_base_images ?start_job ~changeset ();
  ]
  @
  if enable_rpm_images then
    [
      make_job_fedora_based_images ?start_job ~changeset ();
      make_job_rockylinux_based_images ?start_job ~changeset ();
    ]
  else []

(* ── Cacio pipeline registrations ───────────────────────────────────────── *)

let () =
  let jobs = [(Cacio.Auto, job_ci_release_based_images)] in
  Cacio.register_merge_request_jobs jobs ;
  Cacio.register_jobs Cacio.Base_images_daily jobs

let child_pipeline =
  Pipeline.register_child
    "base_images"
    ~description:"Build CI base images"
    ~jobs:
      (job_datadog_pipeline_trace
      :: (jobs () @ Cacio.get_jobs Cacio.Base_images_daily))
