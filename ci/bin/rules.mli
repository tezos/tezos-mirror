(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci

(** A set of commonly used rules used for defining pipeline types and their inclusion.

    For more info, refer to
    {{:https://docs.gitlab.com/ee/ci/variables/predefined_variables.html}Predefined
    variables reference}. *)

(** A rule that is true if [CI_PIPELINE_SOURCE] is [merge_request_event]. *)
val merge_request : If.t

(** A rule that is true if [CI_PIPELINE_SOURCE] is [push]. *)
val push : If.t

(** A rule that is true if [CI_PIPELINE_SOURCE] is [scheduled]. *)
val scheduled : If.t

(** A rule that is true for scheduled extended test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_TESTS]. *)
val schedule_extended_tests : If.t

(** A rule that is true if [CI_COMMIT_BRANCH] is a given branch. *)
val on_branch : string -> If.t

(** A rule that is true if [CI_COMMIT_BRANCH] is [master]. *)
val on_master : If.t

(** A rule that is true if [CI_PROJECT_NAMESPACE] is [tezos]. *)
val on_tezos_namespace : If.t

(** A rule that is true if [CI_PROJECT_NAMESPACE] is not [tezos]. *)
val not_on_tezos_namespace : If.t

(** A rule that is true if [CI_COMMIT_TAG] matches the given regexp. *)
val has_tag_match : string -> If.t

(** A rule that is true if the comma-separated list [CI_MERGE_REQUEST_LABELS] contains a given label. *)
val has_mr_label : string -> If.t

(** A rule that is true if [CI_MERGE_REQUEST_ASSIGNEES] contains [nomadic-margebot]. *)
val assigned_to_marge_bot : If.t

(** A rule that is true if [CI_USER_LOGIN] equals [nomadic-margebot]. *)
val triggered_by_marge_bot : If.t
