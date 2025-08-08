(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci

let debian_matrix = [[("RELEASE", ["unstable"; "bookworm"])]]

let ubuntu_matrix = [[("RELEASE", ["noble"; "jammy"])]]

let rockylinux_matrix = [[("RELEASE", ["9.6"])]]

let fedora_matrix = [[("RELEASE", ["39"; "42"])]]

let jobs =
  let make_job_base_images ~__POS__ ~name ~matrix ~distribution ?image_path
      ?(changes = Changeset.make []) dockerfile =
    let script =
      Printf.sprintf "scripts/ci/build-base-images.sh %s" dockerfile
    in
    let variables =
      if Option.is_none image_path then
        [("DISTRIBUTION", distribution); ("IMAGE_PATH", distribution)]
      else
        [("DISTRIBUTION", distribution); ("IMAGE_PATH", Option.get image_path)]
    in
    job_docker_authenticated
      ~__POS__
      ~name
      ~stage:Stages.images
      ~variables
      ~rules:[job_rule ~changes:(Changeset.encode changes) ~when_:On_success ()]
      ~parallel:(Matrix matrix)
      ~tag:Gcp_very_high_cpu
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
