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

(** A rule that is true if [CI_PIPELINE_SOURCE] is [api]. *)
val api : If.t

(** A rule that is true for created via API release page publish pipeline.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [api] and
    [TZ_API_KIND] set to [RELEASE_PAGE]. *)
val api_release_page : If.t

val api_docker : If.t

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

(** A rule that is true for daily debian test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [debian.daily]. *)
val debian_daily : If.t

(** A rule that is true for daily RPM test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [rpm.daily]. *)
val rpm_daily : If.t

(** A rule that is true for daily Homebrew test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [homebrew.daily]. *)
val homebrew_daily : If.t

(** A rule that is true for daily Base Images test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [base_images.daily]. *)
val base_images_daily : If.t

(** A rule that is true for daily OPAM test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [opam.daily]. *)
val opam_daily : If.t

(** A rule that is true for scheduled extended RPC test pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_RPC_TESTS].

    NB: Tests will run differently with this value of [TZ_SCHEDULE_KIND]:
    the node's external RPC mode will be enabled.
    Cf. [tezt/lib_tezos/node.mli]. *)
val schedule_extended_rpc_tests : If.t

(** A rule that is true for scheduled extended validation test
    pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_VALIDATION_TESTS].

    NB: Tests will run differently with this value of [TZ_SCHEDULE_KIND]:
    the node's single-process mode will be enabled.
    Cf. [tezt/lib_tezos/node.mli]. *)
val schedule_extended_validation_tests : If.t

(** A rule that is true for scheduled extended baker with remote node test
    pipelines.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_BAKER_REMOTE_MODE_TESTS].

    NB: Tests will run differently with this value of [TZ_SCHEDULE_KIND]:
    the remote mode of the baker will be enabled.
    Cf. [tezt/lib_tezos/agnostic_baker.ml]. *)
val schedule_extended_baker_remote_mode_tests : If.t

(** A rule that is true for scheduled extended baker with baker dal
    commands.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [EXTENDED_DAL_USE_BAKER].

    NB: Tests will run differently with this value of [TZ_SCHEDULE_KIND]:
    the baker will start the DAL node.
    Cf. [tezt/lib_tezos/dal_node.ml]. *)
val schedule_extended_dal_use_baker : If.t

(** A rule that is true for scheduled release tests.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [TEST_RELEASE]. *)
val schedule_test_release : If.t

(** A rule that is true for scheduled pipelines that trigger security
    scans of various Docker images: `tezos/tezos:master`,
    `tezos/tezos:latest`, `tezos/tezos:octez-evm-node-latest`.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [SECURITY_SCANS]. *)
val schedule_security_scans : If.t

(** A rule that is true for scheduled pipelines that scan
    `tezos/tezos:master` Docker image.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [CONTAINER_SCANNING_MASTER]. *)
val schedule_container_scanning_master : If.t

(** A rule that is true for scheduled pipelines that scan
    `tezos/tezos:latest` Docker image.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [CONTAINER_SCANNING_OCTEZ_RELEASES]. *)
val schedule_container_scanning_octez_releases : If.t

(** A rule that is true for scheduled pipelines that scan
    `tezos/tezos:octez-vX.Y-rcN` Docker image.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [CONTAINER_SCANNING_OCTEZ_RC]. *)
val schedule_container_scanning_octez_rc : If.t

(** A rule that is true for scheduled pipelines that scan
    `tezos/tezos:octez-evm-node-vX.Y` Docker image.

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [CONTAINER_SCANNING_EVM_NODE_RELEASES]. *)
val schedule_container_scanning_evm_node_releases : If.t

(** A rule that is true for scheduled documentation pipelines.

     Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [DOCUMENTATION]. *)
val schedule_documentation : If.t

(** A rule that is true for scheduled pipelines that build
    fresh Docker images (latest available Alpine packages).

    Such pipelines have [CI_PIPELINE_SOURCE] set to [scheduled] and
    [TZ_SCHEDULE_KIND] set to [DOCKER_FRESH_IMAGE_BUILD]. *)
val schedule_docker_build : If.t

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

(** A rule that is true if the variable DOCKER_FORCE_REBUILD = True *)
val force_rebuild : If.t
