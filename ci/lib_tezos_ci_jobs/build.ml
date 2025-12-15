(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file defines build jobs that are not component-specific.
   As such, the jobs are defined in the [Shared] component. *)

module CI = Cacio.Shared

(* This job is [~forced] to appear in all pipelines because there does not appear
   to be an obvious changeset that would work. Looking at the history,
   this job was triggered in some MRs that didn't modify [changeset_octez].
   TODO: it could be a good idea to require a label to be added to the MR instead. *)
let build_octez_source =
  CI.job
    "build_octez_source"
    ~__POS__
    ~description:"Check compilation of the Octez tarball."
    ~stage:Test
    ~force:true
    ~image:Tezos_ci.Images.CI.build
    ~cpu:Very_high
    ~storage:Ramfs
    ~variables:[("DUNE_BUILD_JOBS", "-j 12")]
    ~cargo_cache:true
    ~sccache:(Cacio.sccache ())
    [
      "./scripts/ci/take_ownership.sh";
      ". ./scripts/version.sh";
      "eval $(opam env)";
      "./scripts/ci/restrict_export_to_octez_source.sh";
      "./scripts/ci/create_octez_tarball.sh octez";
      "mv octez.tar.bz2 ../";
      "cd ../";
      "tar xf octez.tar.bz2";
      "cd octez/";
      "eval $(opam env)";
      "make octez";
    ]

let register () =
  CI.register_before_merging_jobs [(Manual, build_octez_source)] ;
  CI.register_schedule_extended_test_jobs [(Auto, build_octez_source)] ;
  ()
