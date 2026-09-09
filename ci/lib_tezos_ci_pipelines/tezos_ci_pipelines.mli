(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Global pipelines of this repository.

    Add jobs to those pipelines with [Cacio.register_jobs].
    They are registered with CIAO by [Cacio.close]. *)

(** Pipeline that updates and publishes the test release page. *)
val publish_test_release_page : Cacio.global_pipeline

(** Pipeline that updates and publishes the release page. *)
val publish_release_page : Cacio.global_pipeline
