(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines jobs that were migrated to Cacio,
   but which are not yet part of a component.

   As such, the jobs are defined in the [Shared] component,
   and are added to the [scheduled_extended_test] pipeline. *)

module CI = Cacio.Shared

let job_install_opam_noble =
  CI.job
    "oc.install_opam_noble"
    ~__POS__
    ~description:"Check that Octez can be installed via opam."
    ~stage:Test
    ~cpu:Very_high
    ~image:
      (Tezos_ci.Image.mk_external
         ~image_path:
           (* this is not a base image, but the dependency image
              from the packages pipelines *)
           (Tezos_ci.Images.Base_images.path_prefix ^ "/build-ubuntu:24.04"))
    ~allow_failure:Yes
      (* As this job is long, we override the default timeout to 2 hours. *)
    ~timeout:(Hours 2)
    ~variables:[("CARGO_NET_OFFLINE", "false")]
    ~force:true
    ["apt update"; "apt install -y sudo"; "./docs/introduction/install-opam.sh"]

let compile_sources_doc_deps_job ~name_suffix ~image =
  CI.job
    ("oc.compile_sources_doc_deps" ^ name_suffix)
    ~__POS__
    ~description:
      "Check that the instructions to install dependencies, from the \
       documentation, still work (on image %s)."
    ~stage:Test
    ~cpu:Very_high
    ~image
    ~variables:[("CARGO_NET_OFFLINE", "false")]
    ~only_if_changed:["docs/introduction/compile-sources-setup.sh"]
    [sf "./docs/introduction/compile-sources-setup.sh"]

let job_compile_sources_doc_deps_trixie =
  compile_sources_doc_deps_job
    ~name_suffix:"_trixie"
    ~image:Tezos_ci.Images.Base_images.debian_trixie

let job_compile_sources_doc_deps_noble =
  compile_sources_doc_deps_job
    ~name_suffix:"_noble"
    ~image:Tezos_ci.Images.Base_images.ubuntu_24_04

let compile_sources_doc_job ~name_suffix ~project ~branch ~image_name =
  CI.job
    ("oc.compile_sources_doc" ^ name_suffix)
    ~__POS__
    ~description:
      (sf
         "Compile Octez from source (branch %s) according to the instructions \
          in the documentation, to check that those instructions still work \
          (on image %s)."
         branch
         image_name)
    ~stage:Test
    ~cpu:Very_high
    ~storage:Ramfs
    ~only_if_changed:["docs/introduction/compile-sources.sh"]
    ~image:
      (Tezos_ci.Image.mk_external
         ~image_path:(Tezos_ci.Images.Base_images.path_prefix ^ "/" ^ image_name))
    ~variables:[("DUNE_BUILD_JOBS", "-j 12"); ("CARGO_NET_OFFLINE", "false")]
    ~sccache:(Cacio.sccache ())
    [sf "./docs/introduction/compile-sources.sh %s %s" project branch]

let job_compile_sources_doc_latest_trixie =
  compile_sources_doc_job
    ~name_suffix:"_trixie"
    ~project:"tezos/tezos"
    ~branch:"latest-release"
    ~image_name:"build-debian:trixie"

let job_compile_sources_doc_latest_noble =
  compile_sources_doc_job
    ~name_suffix:"_noble"
    ~project:"tezos/tezos"
    ~branch:"latest-release"
    ~image_name:"build-ubuntu:24.04"

let job_compile_sources_doc =
  compile_sources_doc_job
    ~name_suffix:""
    ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
    ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}"
    ~image_name:"build-debian:trixie"

let register () =
  CI.register_merge_request_jobs
    [(Manual, job_install_opam_noble); (Auto, job_compile_sources_doc)] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_install_opam_noble);
      (Auto, job_compile_sources_doc_deps_trixie);
      (Auto, job_compile_sources_doc_deps_noble);
      (Auto, job_compile_sources_doc_latest_trixie);
      (Auto, job_compile_sources_doc_latest_noble);
    ] ;
  ()
