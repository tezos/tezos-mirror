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

(** A rule that is true if [CI_MERGE_REQUEST_EVENT_TYPE] is [detached].

    Merge request pipelines are {i detached} when:
     - merge result pipelines are disabled in GitLab CI settings;
     - merge result pipelines are enabled {i but} the MR has
       conflicts with the target branch and cannot be rebased. *)
val detached : If.t

(** A rule that is true if [CI_MERGE_REQUEST_EVENT_TYPE] is [merged_result].

    Merge request pipelines are {i merged result} pipelines when:
     - merge result pipelines are enabled in GitLab CI settings; {b and}
     - MR rebases cleanly on the target branch; {b and}
     - the pipeline is not running in a merge train. *)
val merged_result : If.t

(** A rule that is true if [CI_MERGE_REQUEST_EVENT_TYPE] is [merge_train].

    Merge request pipelines are {i merge train} pipelines when merge
    trains are activated (and thus necessarily merged result
    pipelines), and when the pipeline is triggered from a merge train. *)
val merge_train : If.t

(** A rule that is true for scheduled extended test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_TESTS]. *)
val schedule_extended_tests : If.t

(** A rule that is true for scheduled extended RPC test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_RPC_TESTS]. *)
val schedule_extended_rpc_tests : If.t

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

(** A rule that is true if this is a final pipeline before merging.

    This rule is true if and only if:
     - the pipeline is started by marge-bot; or
     - the merge request is assigned to marge-bot; or
     - the pipeline is in a merge train.

    We require the second case since there are cases where marge-bot
    will just trigger an existing pipeline instead of creating one.

    Use this rule to add or exclude jobs from final pipelines. *)
val is_final_pipeline : If.t

(** A rule that is never true. *)
val never : If.t

(** A rule that is always true. *)
val always : If.t
