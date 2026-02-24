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

let job_compile_sources_doc_trixie =
  CI.job
    "oc.compile_sources_doc_trixie"
    ~__POS__
    ~description:
      "Compile Octez from source (branch latest-release) according to the \
       instructions in the documentation, to check that those instructions \
       still work."
    ~stage:Test
    ~cpu:Very_high
    ~storage:Ramfs
    ~only_if_changed:["docs/introduction/compile-sources.sh"]
    ~image:
      (Tezos_ci.Image.mk_external
         ~image_path:
           (Tezos_ci.Images.Base_images.path_prefix ^ "/build-debian:trixie"))
    ~variables:[("DUNE_BUILD_JOBS", "-j 12"); ("CARGO_NET_OFFLINE", "false")]
    ~sccache:(Cacio.sccache ())
    ["./docs/introduction/compile-sources.sh tezos/tezos latest-release"]

let job_compile_sources_doc_noble =
  CI.job
    "oc.compile_sources_doc_noble"
    ~__POS__
    ~description:
      "Compile Octez from source (branch latest-release) according to the \
       instructions in the documentation, to check that those instructions \
       still work."
    ~stage:Test
    ~cpu:Very_high
    ~storage:Ramfs
    ~only_if_changed:["docs/introduction/compile-sources.sh"]
    ~image:
      (Tezos_ci.Image.mk_external
         ~image_path:
           (Tezos_ci.Images.Base_images.path_prefix ^ "/build-ubuntu:24.04"))
    ~variables:[("DUNE_BUILD_JOBS", "-j 12"); ("CARGO_NET_OFFLINE", "false")]
    ~sccache:(Cacio.sccache ())
    ["./docs/introduction/compile-sources.sh tezos/tezos latest-release"]

let job_compile_sources_doc_master =
  CI.job
    "oc.compile_sources_doc_master"
    ~__POS__
    ~description:
      "Compile Octez from source (branch master) according to the instructions \
       in the documentation, to check that those instructions still work."
    ~stage:Test
    ~cpu:Very_high
    ~storage:Ramfs
    ~only_if_changed:["docs/introduction/compile-sources.sh"]
    ~image:
      (Tezos_ci.Image.mk_external
         ~image_path:
           (Tezos_ci.Images.Base_images.path_prefix ^ "/build-debian:trixie"))
    ~variables:[("CARGO_NET_OFFLINE", "false")]
    ~sccache:(Cacio.sccache ())
    [
      "./docs/introduction/compile-sources.sh \
       ${CI_MERGE_REQUEST_SOURCE_PROJECT_PATH:-tezos/tezos} \
       ${CI_MERGE_REQUEST_SOURCE_BRANCH_NAME:-master}";
    ]

let register () =
  CI.register_merge_request_jobs [(Auto, job_compile_sources_doc_master)] ;
  CI.register_schedule_extended_test_jobs
    [
      (Auto, job_compile_sources_doc_trixie);
      (Auto, job_compile_sources_doc_noble);
    ] ;
  ()
