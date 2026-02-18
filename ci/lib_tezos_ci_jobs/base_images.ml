(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

let debian_releases = ["unstable"; "bookworm"; "trixie"]

let debian_matrix = [("RELEASE", debian_releases)]

let ubuntu_releases = ["22.04"; "24.04"; "25.10"]

let ubuntu_matrix = [("RELEASE", ubuntu_releases)]

let rockylinux_releases = ["9"; "10"]

let rockylinux_matrix = [("RELEASE", rockylinux_releases)]

let fedora_releases = ["39"; "42"]

let fedora_matrix = [("RELEASE", fedora_releases)]

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

(* FIXME: remove changesets from [base_images.daily] which is a branch
   pipeline cf. https://gitlab.com/tezos/tezos/-/issues/8221 *)
module Files = struct
  let build_script = ["scripts/ci/build-base-images.sh"]

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
      "images/scripts/install-gcloud-apt.sh";
    ]
    @ build_script

  let debian_homebrew =
    [
      "images/base-images/Dockerfile.debian-homebrew";
      (* script used in Dockerfile *)
      "scripts/packaging/homebrew_install.sh";
    ]
    @ build_script

  let debian_rust_build =
    [
      "images/base-images/Dockerfile.rust";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
    ]
    @ build_script

  let debian_rust_merge =
    [
      "images/base-images/Dockerfile.rust";
      (* script used in Dockerfile *)
      "images/scripts/install_sccache_static.sh";
      (* job script *)
      "scripts/ci/docker-merge-base-images.sh";
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
    | Debian -> ["unstable"; "bookworm"; "trixie"]
    | Ubuntu -> ["22.04"; "24.04"; "25.10"]
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

(* [start_job] used to add dependency to [trigger] in [before_merging] pipelines.

   [changeset] should be set to [true] for [before_merging]/[merge_train] parent pipelines only.
   Changesets should be ignored for other pipelines:
   - branch pipelines such as [base_images.daily] because they are then ignored by GitLab
   - in the manually triggered base images child pipeline as we may want to trigger all base images jobs.
 *)
let jobs ?start_job ?(changeset = false) () =
  (* This function can build docker images both in an emulated environment using
     qemu or natively. The advantage of choosing emulated vs native depends on
     the build time associated with the image. Small images are more efficiently
     built in an emulated environment, while larger images are better build
     natively.

     [name] is the name of the job.

     [matrix] is a parallel/matrix gitlab construct. Here we use it with the RELEASE
     variable to build multiple images for the same distribution, but different releases.

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
     *)
  let make_job_base_images ~__POS__ ~matrix ~image_name
      ?(base_name = Upstream image_name) ?(changes = Changeset.make [])
      ?(compilation = Emulated) ?dependencies dockerfile =
    let script =
      Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile
    in
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
                [Runner.Tag.show Gcp_very_high_cpu; Runner.Tag.show Gcp_arm64]
              );
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
           else [job_rule ~when_:Always ()])
      ~parallel:(Matrix [matrix @ tags])
      ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
      ?dependencies
      [script]
  in

  (* specialisation of [make_job_base_images] for distribution images.
     - [base_name] used only for Rockylinux.
     - if [changes] is not provided, we use the base changeset of the
     distribution. This applies to base images that are not a
     dependency of other images.  *)
  let _make_job_base_image_distribution ?base_name ?changes distro =
    make_job_base_images
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
  in

  (* base images: deb and rpm distros *)
  let job_debian_based_images =
    let changes =
      (* we need the [debian] base job if we have [debian-homebrew] or
         [debian-rust] jobs *)
      Changeset.make
        (Files.debian_base @ Files.debian_homebrew @ Files.debian_rust_build
       @ Files.debian_rust_merge)
    in
    make_job_base_images
      ~__POS__
      ~image_name:"debian"
      ~matrix:debian_matrix
      ~changes
      "images/base-images/Dockerfile.debian"
  in
  let job_ubuntu_based_images =
    make_job_base_images
      ~__POS__
      ~image_name:"ubuntu"
      ~matrix:ubuntu_matrix
      ~changes:(Changeset.make Files.debian_base)
      "images/base-images/Dockerfile.debian"
  in
  let job_fedora_based_images =
    make_job_base_images
      ~__POS__
      ~image_name:"fedora"
      ~matrix:fedora_matrix
      ~changes:(Changeset.make Files.rpm_base)
      "images/base-images/Dockerfile.rpm"
  in
  let job_rockylinux_based_images =
    make_job_base_images
      ~__POS__
      ~image_name:"rockylinux"
      ~base_name:(Upstream "rockylinux/rockylinux")
      ~matrix:rockylinux_matrix
      ~changes:(Changeset.make Files.rpm_base)
      "images/base-images/Dockerfile.rpm"
  in
  (* debian-rust: based on [debian:trixie]. *)
  let job_rust_based_images =
    make_job_base_images
      ~__POS__
      ~image_name:"debian-rust"
      ~base_name:(Pipeline_dep "debian")
      ~matrix:[("RELEASE", ["trixie"])]
      ~dependencies:(Dependent [Job job_debian_based_images])
      ~compilation:Native
      ~changes:
        (Changeset.make
           (Files.debian_rust_build
          (* Adding the changeset of debian job as we want to test the
             build of [debian-rust] if [debian] is rebuild. *)
          @ Files.debian_base
           (* If we run [debian-rust] merge job, we need [debian-rust] build job *)
           @ Files.debian_rust_merge))
      "images/base-images/Dockerfile.rust"
  in
  (* dedicated merge job exist because QEMU compilation takes too much
     time. Build job reached timeout. *)
  let job_rust_based_images_merge =
    job_docker_authenticated
      ~__POS__
      ~name:"images.debian-rust.merge"
      ~stage:Stages.build
      ~dependencies:(Dependent [Job job_rust_based_images])
      ~rules:
        (if changeset then
           [
             job_rule
               ~changes:
                 (Changeset.encode
                    (Changeset.make
                       (Files.debian_rust_merge
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
           else [job_rule ~when_:Always ()])
      ~variables:
        [
          ("RELEASE", "trixie");
          ("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-rust");
        ]
      ["scripts/ci/docker-merge-base-images.sh"]
  in
  (* debian-homebrew: based on [debian:trixie] *)
  let job_debian_homebrew_base_images =
    make_job_base_images
      ~__POS__
      ~image_name:"debian-homebrew"
      ~base_name:(Pipeline_dep "debian")
      ~dependencies:(Dependent [Job job_debian_based_images])
      ~matrix:[("RELEASE", ["trixie"])]
      ~compilation:Amd64_only
        (* Adding the changeset of [debian] job as we want to test the
         build of [debian-homebrew] if [debian] is rebuild. *)
      ~changes:(Changeset.make (Files.debian_homebrew @ Files.debian_base))
      "images/base-images/Dockerfile.debian-homebrew"
  in
  [
    job_debian_based_images;
    job_ubuntu_based_images;
    job_fedora_based_images;
    job_rockylinux_based_images;
    job_rust_based_images;
    job_rust_based_images_merge;
    job_debian_homebrew_base_images;
  ]

let child_pipeline =
  Pipeline.register_child
    "base_images"
    ~description:"Build CI base images"
    ~jobs:(job_datadog_pipeline_trace :: jobs ())
