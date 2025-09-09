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

let rockylinux_releases = ["9.3"; "9.6"; "10.0"]

let rockylinux_matrix = [("RELEASE", rockylinux_releases)]

let fedora_releases = ["39"; "41"; "42"]

let fedora_matrix = [("RELEASE", fedora_releases)]

let jobs =
  let make_job_base_images ~__POS__ ~name ~matrix ~distribution ?image_path
      ?(changes = Changeset.make []) ?tags dockerfile =
    (* if [tags] is omitted then we build for two different architectures
     using emulation. If [tags] is not empty ( we assume a matrix ), then
     we build using a native runner setting the tag accordingly. In the former
     case, we must also run a merge manifest job *)
    let script =
      Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile
    in
    let emulated = Option.is_none tags in
    let variables =
      [
        ("DISTRIBUTION", distribution);
        ( "IMAGE_PATH",
          if Option.is_none image_path then distribution
          else Option.get image_path );
        ("PLATFORM", if emulated then "linux/amd64,linux/arm64" else "");
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
      ~parallel:(Matrix [matrix @ Option.value ~default:[] tags])
      ~tag:(if emulated then Gcp_very_high_cpu else Dynamic)
      [script]
  in
  let job_debian_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.debian"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.debian"
      ~distribution:"debian"
      ~matrix:debian_matrix
      ~changes
      "images/base-images/Dockerfile.debian"
  in
  let job_ubuntu_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.debian"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.ubuntu"
      ~distribution:"ubuntu"
      ~matrix:ubuntu_matrix
      ~changes
      "images/base-images/Dockerfile.debian"
  in
  let job_fedora_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.rpm"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.fedora"
      ~distribution:"fedora"
      ~matrix:fedora_matrix
      ~changes
      "images/base-images/Dockerfile.rpm"
  in
  let job_rockylinux_based_images =
    let changes = Changeset.make ["images/base-images/Dockerfile.rpm"] in
    make_job_base_images
      ~__POS__
      ~name:"oc.base-images.rockylinux"
      ~distribution:"rockylinux"
      ~image_path:"rockylinux/rockylinux"
      ~matrix:rockylinux_matrix
      ~changes
      "images/base-images/Dockerfile.rpm"
  in
  [
    job_debian_based_images;
    job_ubuntu_based_images;
    job_fedora_based_images;
    job_rockylinux_based_images;
  ]

let child_pipeline =
  Pipeline.register_child
    "base_images"
    ~description:"Build CI base images"
    ~jobs:(job_datadog_pipeline_trace :: jobs)
