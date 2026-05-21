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
    ~script:[sf "./docs/introduction/compile-sources-setup.sh"]

let job_compile_sources_doc_deps_debian =
  compile_sources_doc_deps_job
    ~name_suffix:"_debian"
    ~image:Tezos_ci.Images.Base_images.debian_trixie

let job_compile_sources_doc_deps_ubuntu =
  compile_sources_doc_deps_job
    ~name_suffix:"_ubuntu"
    ~image:Tezos_ci.Images.Base_images.ubuntu_24_04

let compile_sources_doc_job ~name_suffix ~project ~branch ~image =
  CI.job
    ("oc.compile_sources_doc" ^ name_suffix)
    ~__POS__
    ~description:
      (sf
         "Compile Octez from source (branch %s) according to the instructions \
          in the documentation, to check that those instructions still work \
          (on image %a)."
         branch
         Tezos_ci.Image.pp
         image)
    ~stage:Test
    ~cpu:Very_high
    ~storage:Ramfs
    ~only_if_changed:["docs/introduction/compile-sources.sh"]
    ~image
    ~variables:[("DUNE_BUILD_JOBS", "-j 12"); ("CARGO_NET_OFFLINE", "false")]
    ~sccache:(Cacio.sccache ())
    ~script:[sf "./docs/introduction/compile-sources.sh %s %s" project branch]

let job_compile_sources_doc_latest_debian =
  compile_sources_doc_job
    ~name_suffix:"_debian"
    ~project:"tezos/tezos"
    ~branch:"latest-release"
    ~image:Tezos_ci.Images.Base_images.debian_build_trixie

let job_compile_sources_doc_latest_ubuntu =
  compile_sources_doc_job
    ~name_suffix:"_ubuntu"
    ~project:"tezos/tezos"
    ~branch:"latest-release"
    ~image:Tezos_ci.Images.Base_images.ubuntu_build_24_04

let job_compile_sources_doc =
  compile_sources_doc_job
    ~name_suffix:""
    ~project:"${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos}"
    ~branch:"${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}"
    ~image:Tezos_ci.Images.Base_images.debian_build_trixie

let register () =
  Cacio.register_merge_request_jobs [(Auto, job_compile_sources_doc)] ;
  Cacio.register_jobs
    Schedule_extended_test
    [
      (Auto, job_compile_sources_doc_deps_debian);
      (Auto, job_compile_sources_doc_deps_ubuntu);
      (Auto, job_compile_sources_doc_latest_debian);
      (Auto, job_compile_sources_doc_latest_ubuntu);
    ] ;
  ()
