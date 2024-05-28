(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* This file is generated.

   See [make_predefined_vars.sh] in this directory for more information. *)

open Var

let show = encode

let chat_channel = make "CHAT_CHANNEL"

let chat_input = make "CHAT_INPUT"

let chat_user_id = make "CHAT_USER_ID"

let ci = make "CI"

let ci_api_v4_url = make "CI_API_V4_URL"

let ci_api_graphql_url = make "CI_API_GRAPHQL_URL"

let ci_builds_dir = make "CI_BUILDS_DIR"

let ci_commit_author = make "CI_COMMIT_AUTHOR"

let ci_commit_before_sha = make "CI_COMMIT_BEFORE_SHA"

let ci_commit_branch = make "CI_COMMIT_BRANCH"

let ci_commit_description = make "CI_COMMIT_DESCRIPTION"

let ci_commit_message = make "CI_COMMIT_MESSAGE"

let ci_commit_ref_name = make "CI_COMMIT_REF_NAME"

let ci_commit_ref_protected = make "CI_COMMIT_REF_PROTECTED"

let ci_commit_ref_slug = make "CI_COMMIT_REF_SLUG"

let ci_commit_sha = make "CI_COMMIT_SHA"

let ci_commit_short_sha = make "CI_COMMIT_SHORT_SHA"

let ci_commit_tag = make "CI_COMMIT_TAG"

let ci_commit_tag_message = make "CI_COMMIT_TAG_MESSAGE"

let ci_commit_timestamp = make "CI_COMMIT_TIMESTAMP"

let ci_commit_title = make "CI_COMMIT_TITLE"

let ci_concurrent_id = make "CI_CONCURRENT_ID"

let ci_concurrent_project_id = make "CI_CONCURRENT_PROJECT_ID"

let ci_config_path = make "CI_CONFIG_PATH"

let ci_debug_trace = make "CI_DEBUG_TRACE"

let ci_debug_services = make "CI_DEBUG_SERVICES"

let ci_default_branch = make "CI_DEFAULT_BRANCH"

let ci_dependency_proxy_direct_group_image_prefix =
  make "CI_DEPENDENCY_PROXY_DIRECT_GROUP_IMAGE_PREFIX"

let ci_dependency_proxy_group_image_prefix =
  make "CI_DEPENDENCY_PROXY_GROUP_IMAGE_PREFIX"

let ci_dependency_proxy_password = make "CI_DEPENDENCY_PROXY_PASSWORD"

let ci_dependency_proxy_server = make "CI_DEPENDENCY_PROXY_SERVER"

let ci_dependency_proxy_user = make "CI_DEPENDENCY_PROXY_USER"

let ci_deploy_freeze = make "CI_DEPLOY_FREEZE"

let ci_deploy_password = make "CI_DEPLOY_PASSWORD"

let ci_deploy_user = make "CI_DEPLOY_USER"

let ci_disposable_environment = make "CI_DISPOSABLE_ENVIRONMENT"

let ci_environment_name = make "CI_ENVIRONMENT_NAME"

let ci_environment_slug = make "CI_ENVIRONMENT_SLUG"

let ci_environment_url = make "CI_ENVIRONMENT_URL"

let ci_environment_action = make "CI_ENVIRONMENT_ACTION"

let ci_environment_tier = make "CI_ENVIRONMENT_TIER"

let ci_release_description = make "CI_RELEASE_DESCRIPTION"

let ci_gitlab_fips_mode = make "CI_GITLAB_FIPS_MODE"

let ci_has_open_requirements = make "CI_HAS_OPEN_REQUIREMENTS"

let ci_job_id = make "CI_JOB_ID"

let ci_job_image = make "CI_JOB_IMAGE"

let ci_job_manual = make "CI_JOB_MANUAL"

let ci_job_name = make "CI_JOB_NAME"

let ci_job_name_slug = make "CI_JOB_NAME_SLUG"

let ci_job_stage = make "CI_JOB_STAGE"

let ci_job_status = make "CI_JOB_STATUS"

let ci_job_timeout = make "CI_JOB_TIMEOUT"

let ci_job_token = make "CI_JOB_TOKEN"

let ci_job_url = make "CI_JOB_URL"

let ci_job_started_at = make "CI_JOB_STARTED_AT"

let ci_kubernetes_active = make "CI_KUBERNETES_ACTIVE"

let ci_node_index = make "CI_NODE_INDEX"

let ci_node_total = make "CI_NODE_TOTAL"

let ci_open_merge_requests = make "CI_OPEN_MERGE_REQUESTS"

let ci_pages_domain = make "CI_PAGES_DOMAIN"

let ci_pages_url = make "CI_PAGES_URL"

let ci_pipeline_id = make "CI_PIPELINE_ID"

let ci_pipeline_iid = make "CI_PIPELINE_IID"

let ci_pipeline_source = make "CI_PIPELINE_SOURCE"

let ci_pipeline_triggered = make "CI_PIPELINE_TRIGGERED"

let ci_pipeline_url = make "CI_PIPELINE_URL"

let ci_pipeline_created_at = make "CI_PIPELINE_CREATED_AT"

let ci_pipeline_name = make "CI_PIPELINE_NAME"

let ci_project_dir = make "CI_PROJECT_DIR"

let ci_project_id = make "CI_PROJECT_ID"

let ci_project_name = make "CI_PROJECT_NAME"

let ci_project_namespace = make "CI_PROJECT_NAMESPACE"

let ci_project_namespace_id = make "CI_PROJECT_NAMESPACE_ID"

let ci_project_path_slug = make "CI_PROJECT_PATH_SLUG"

let ci_project_path = make "CI_PROJECT_PATH"

let ci_project_repository_languages = make "CI_PROJECT_REPOSITORY_LANGUAGES"

let ci_project_root_namespace = make "CI_PROJECT_ROOT_NAMESPACE"

let ci_project_title = make "CI_PROJECT_TITLE"

let ci_project_description = make "CI_PROJECT_DESCRIPTION"

let ci_project_url = make "CI_PROJECT_URL"

let ci_project_visibility = make "CI_PROJECT_VISIBILITY"

let ci_project_classification_label = make "CI_PROJECT_CLASSIFICATION_LABEL"

let ci_registry = make "CI_REGISTRY"

let ci_registry_image = make "CI_REGISTRY_IMAGE"

let ci_registry_password = make "CI_REGISTRY_PASSWORD"

let ci_registry_user = make "CI_REGISTRY_USER"

let ci_repository_url = make "CI_REPOSITORY_URL"

let ci_runner_description = make "CI_RUNNER_DESCRIPTION"

let ci_runner_executable_arch = make "CI_RUNNER_EXECUTABLE_ARCH"

let ci_runner_id = make "CI_RUNNER_ID"

let ci_runner_revision = make "CI_RUNNER_REVISION"

let ci_runner_short_token = make "CI_RUNNER_SHORT_TOKEN"

let ci_runner_tags = make "CI_RUNNER_TAGS"

let ci_runner_version = make "CI_RUNNER_VERSION"

let ci_server_host = make "CI_SERVER_HOST"

let ci_server_name = make "CI_SERVER_NAME"

let ci_server_port = make "CI_SERVER_PORT"

let ci_server_protocol = make "CI_SERVER_PROTOCOL"

let ci_server_shell_ssh_host = make "CI_SERVER_SHELL_SSH_HOST"

let ci_server_shell_ssh_port = make "CI_SERVER_SHELL_SSH_PORT"

let ci_server_revision = make "CI_SERVER_REVISION"

let ci_server_tls_ca_file = make "CI_SERVER_TLS_CA_FILE"

let ci_server_tls_cert_file = make "CI_SERVER_TLS_CERT_FILE"

let ci_server_tls_key_file = make "CI_SERVER_TLS_KEY_FILE"

let ci_server_url = make "CI_SERVER_URL"

let ci_server_version_major = make "CI_SERVER_VERSION_MAJOR"

let ci_server_version_minor = make "CI_SERVER_VERSION_MINOR"

let ci_server_version_patch = make "CI_SERVER_VERSION_PATCH"

let ci_server_version = make "CI_SERVER_VERSION"

let ci_server = make "CI_SERVER"

let ci_shared_environment = make "CI_SHARED_ENVIRONMENT"

let ci_template_registry_host = make "CI_TEMPLATE_REGISTRY_HOST"

let gitlab_ci = make "GITLAB_CI"

let gitlab_features = make "GITLAB_FEATURES"

let gitlab_user_email = make "GITLAB_USER_EMAIL"

let gitlab_user_id = make "GITLAB_USER_ID"

let gitlab_user_login = make "GITLAB_USER_LOGIN"

let gitlab_user_name = make "GITLAB_USER_NAME"

let kubeconfig = make "KUBECONFIG"

let trigger_payload = make "TRIGGER_PAYLOAD"

let ci_merge_request_approved = make "CI_MERGE_REQUEST_APPROVED"

let ci_merge_request_assignees = make "CI_MERGE_REQUEST_ASSIGNEES"

let ci_merge_request_diff_base_sha = make "CI_MERGE_REQUEST_DIFF_BASE_SHA"

let ci_merge_request_diff_id = make "CI_MERGE_REQUEST_DIFF_ID"

let ci_merge_request_event_type = make "CI_MERGE_REQUEST_EVENT_TYPE"

let ci_merge_request_description = make "CI_MERGE_REQUEST_DESCRIPTION"

let ci_merge_request_description_is_truncated =
  make "CI_MERGE_REQUEST_DESCRIPTION_IS_TRUNCATED"

let ci_merge_request_id = make "CI_MERGE_REQUEST_ID"

let ci_merge_request_iid = make "CI_MERGE_REQUEST_IID"

let ci_merge_request_labels = make "CI_MERGE_REQUEST_LABELS"

let ci_merge_request_milestone = make "CI_MERGE_REQUEST_MILESTONE"

let ci_merge_request_project_id = make "CI_MERGE_REQUEST_PROJECT_ID"

let ci_merge_request_project_path = make "CI_MERGE_REQUEST_PROJECT_PATH"

let ci_merge_request_project_url = make "CI_MERGE_REQUEST_PROJECT_URL"

let ci_merge_request_ref_path = make "CI_MERGE_REQUEST_REF_PATH"

let ci_merge_request_source_branch_name =
  make "CI_MERGE_REQUEST_SOURCE_BRANCH_NAME"

let ci_merge_request_source_branch_protected =
  make "CI_MERGE_REQUEST_SOURCE_BRANCH_PROTECTED"

let ci_merge_request_source_branch_sha =
  make "CI_MERGE_REQUEST_SOURCE_BRANCH_SHA"

let ci_merge_request_source_project_id =
  make "CI_MERGE_REQUEST_SOURCE_PROJECT_ID"

let ci_merge_request_source_project_path =
  make "CI_MERGE_REQUEST_SOURCE_PROJECT_PATH"

let ci_merge_request_source_project_url =
  make "CI_MERGE_REQUEST_SOURCE_PROJECT_URL"

let ci_merge_request_squash_on_merge = make "CI_MERGE_REQUEST_SQUASH_ON_MERGE"

let ci_merge_request_target_branch_name =
  make "CI_MERGE_REQUEST_TARGET_BRANCH_NAME"

let ci_merge_request_target_branch_protected =
  make "CI_MERGE_REQUEST_TARGET_BRANCH_PROTECTED"

let ci_merge_request_target_branch_sha =
  make "CI_MERGE_REQUEST_TARGET_BRANCH_SHA"

let ci_merge_request_title = make "CI_MERGE_REQUEST_TITLE"
