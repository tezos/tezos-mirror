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

let ubuntu_releases = ["noble"; "jammy"; "plucky"]

let ubuntu_matrix = [("RELEASE", ubuntu_releases)]

let rockylinux_releases = ["9.3"; "9.6"; "10.0"; "9"; "10"]

let rockylinux_matrix = [("RELEASE", rockylinux_releases)]

let fedora_releases = ["39"; "41"; "42"]

let fedora_matrix = [("RELEASE", fedora_releases)]

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

let jobs =
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

     [compilation] determines the type of the image.
     If [compilation] parameter is either set to [Emulated] or omitted then we
     build for two different architectures using qemu. This handles both build
     and merging the manifest of the images.

     If [compilation] is either [ Amd64_only ] or [ Arm64_only ] we build the
     images natively, but the arm64 image is going to be suffixed with "-arm64".

     If [compilation] is set to [Native] we build for both architectures using
     a native runner. In this case we also must add a merge manifest job.
     *)
  let make_job_base_images ~__POS__ ~name ~matrix ~image_name ?base_name
      ?(changes = Changeset.make []) ?(compilation = Emulated) dockerfile =
    let script =
      Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile
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
        ( "IMAGE_PATH",
          if Option.is_none base_name then image_name else Option.get base_name
        );
        ("PLATFORM", platform);
      ]
    in
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables
      ~rules:
        [
          job_rule ~changes:(Changeset.encode changes) ~when_:On_success ();
          job_rule ~if_:Rules.force_rebuild ~when_:On_success ();
        ]
      ~parallel:(Matrix [matrix @ tags])
      ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
      [script]
  in
  let job_debian_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.debian"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.debian"
      ~image_name:"debian"
      ~matrix:debian_matrix
      ~changes
      "images/base-images/Dockerfile.debian"
  in
  let job_ubuntu_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.debian"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.ubuntu"
      ~image_name:"ubuntu"
      ~matrix:ubuntu_matrix
      ~changes
      "images/base-images/Dockerfile.debian"
  in
  let job_fedora_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.rpm"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.fedora"
      ~image_name:"fedora"
      ~matrix:fedora_matrix
      ~changes
      "images/base-images/Dockerfile.rpm"
  in
  let job_rockylinux_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.rpm"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.rockylinux"
      ~image_name:"rockylinux"
      ~base_name:"rockylinux/rockylinux"
      ~matrix:rockylinux_matrix
      ~changes
      "images/base-images/Dockerfile.rpm"
  in
  let job_rust_based_images, job_rust_based_images_merge =
    let images =
      make_job_base_images
        ~__POS__
        ~name:"oc.base-images.rust"
        ~image_name:"debian-rust"
        ~base_name:"debian"
        ~matrix:[("RELEASE", ["trixie"])]
        ~compilation:Native
        "images/base-images/Dockerfile.rust"
    in
    let merge =
      job_docker_authenticated
        ~__POS__
        ~name:"oc.base-images.rust.merge"
        ~stage:Stages.images
        ~dependencies:(Dependent [Job images])
        ~variables:
          [
            ("RELEASE", "trixie");
            ("IMAGE_NAME", "${GCP_REGISTRY}/tezos/tezos/debian-rust");
          ]
        ["scripts/ci/docker-merge-base-images.sh"]
    in
    (images, merge)
  in
  let job_debian_homebrew_base_images =
    let changes =
      Changeset.make ["images/base-images/Dockerfile.debian-homebrew"]
    in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.debian-homebrew"
      ~image_name:"debian-homebrew"
      ~base_name:"debian"
      ~matrix:[("RELEASE", ["trixie"])]
      ~compilation:Amd64_only
      ~changes
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
    ~jobs:(job_datadog_pipeline_trace :: jobs)
