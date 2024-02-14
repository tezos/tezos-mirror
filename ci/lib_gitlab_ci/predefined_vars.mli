(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Predefined CI/CD variables in all pipelines.

    This contains a subset of the
    {{:https://docs.gitlab.com/ee/ci/variables/predefined_variables.html}
    predefined variables}. *)

(** String representation of a variable with the sigil-sign.

    A handy alias of {!Var.encode}. *)
val show : Var.t -> string

(** Corresponds to [CHAT_CHANNEL].

    The Source chat channel that triggered the ChatOps command.

    Context: [Pipeline]. Available since GitLab [10.6].
    Available in [all] runners. *)
val chat_channel : Var.t

(** Corresponds to [CHAT_INPUT].

    The additional arguments passed with the ChatOps command.

    Context: [Pipeline]. Available since GitLab [10.6].
    Available in [all] runners. *)
val chat_input : Var.t

(** Corresponds to [CHAT_USER_ID].

    The chat service’s user ID of the user who triggered the ChatOps command.

    Context: [Pipeline]. Available since GitLab [14.4].
    Available in [all] runners. *)
val chat_user_id : Var.t

(** Corresponds to [CI].

    Available for all jobs executed in CI/CD. ["true"] when available.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [0.4] runners. *)
val ci : Var.t

(** Corresponds to [CI_API_V4_URL].

    The GitLab API v4 root URL.

    Context: [Pipeline]. Available since GitLab [11.7].
    Available in [all] runners. *)
val ci_api_v4_url : Var.t

(** Corresponds to [CI_API_GRAPHQL_URL].

    The GitLab API GraphQL root URL.

    Context: [Pipeline]. Available since GitLab [15.11].
    Available in [all] runners. *)
val ci_api_graphql_url : Var.t

(** Corresponds to [CI_BUILDS_DIR].

    The top-level directory where builds are executed.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [11.10] runners. *)
val ci_builds_dir : Var.t

(** Corresponds to [CI_COMMIT_AUTHOR].

    The author of the commit in Name <email> format.

    Context: [Pipeline]. Available since GitLab [13.11].
    Available in [all] runners. *)
val ci_commit_author : Var.t

(** Corresponds to [CI_COMMIT_BEFORE_SHA].

    The previous latest commit present on a branch or tag. Is always
    0000000000000000000000000000000000000000 for merge request pipelines, the
    first commit in pipelines for branches or tags, or when manually running a
    pipeline.

    Context: [Pipeline]. Available since GitLab [11.2].
    Available in [all] runners. *)
val ci_commit_before_sha : Var.t

(** Corresponds to [CI_COMMIT_BRANCH].

    The commit branch name. Available in branch pipelines, including pipelines
    for the default branch. Not available in merge request pipelines or tag
    pipelines.

    Context: [Pipeline]. Available since GitLab [12.6].
    Available in [0.5] runners. *)
val ci_commit_branch : Var.t

(** Corresponds to [CI_COMMIT_DESCRIPTION].

    The description of the commit. If the title is shorter than 100 characters,
    the message without the first line.

    Context: [Pipeline]. Available since GitLab [10.8].
    Available in [all] runners. *)
val ci_commit_description : Var.t

(** Corresponds to [CI_COMMIT_MESSAGE].

    The full commit message.

    Context: [Pipeline]. Available since GitLab [10.8].
    Available in [all] runners. *)
val ci_commit_message : Var.t

(** Corresponds to [CI_COMMIT_REF_NAME].

    The branch or tag name for which project is built.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_commit_ref_name : Var.t

(** Corresponds to [CI_COMMIT_REF_PROTECTED].

    ["true"] if the job is running for a protected reference, ["false"] otherwise.

    Context: [Pipeline]. Available since GitLab [11.11].
    Available in [all] runners. *)
val ci_commit_ref_protected : Var.t

(** Corresponds to [CI_COMMIT_REF_SLUG].

    CI_COMMIT_REF_NAME in lowercase, shortened to 63 bytes, and with everything
    except 0-9 and a-z replaced with -. No leading / trailing -. Use in URLs,
    host names and domain names.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_commit_ref_slug : Var.t

(** Corresponds to [CI_COMMIT_SHA].

    The commit revision the project is built for.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_commit_sha : Var.t

(** Corresponds to [CI_COMMIT_SHORT_SHA].

    The first eight characters of CI_COMMIT_SHA.

    Context: [Pipeline]. Available since GitLab [11.7].
    Available in [all] runners. *)
val ci_commit_short_sha : Var.t

(** Corresponds to [CI_COMMIT_TAG].

    The commit tag name. Available only in pipelines for tags.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [0.5] runners. *)
val ci_commit_tag : Var.t

(** Corresponds to [CI_COMMIT_TAG_MESSAGE].

    The commit tag message. Available only in pipelines for tags.

    Context: [Pipeline]. Available since GitLab [15.5].
    Available in [all] runners. *)
val ci_commit_tag_message : Var.t

(** Corresponds to [CI_COMMIT_TIMESTAMP].

    The timestamp of the commit in the ISO 8601 format. For example,
    2022-01-31T16:47:55Z.

    Context: [Pipeline]. Available since GitLab [13.4].
    Available in [all] runners. *)
val ci_commit_timestamp : Var.t

(** Corresponds to [CI_COMMIT_TITLE].

    The title of the commit. The full first line of the message.

    Context: [Pipeline]. Available since GitLab [10.8].
    Available in [all] runners. *)
val ci_commit_title : Var.t

(** Corresponds to [CI_CONCURRENT_ID].

    The unique ID of build execution in a single executor.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [11.10] runners. *)
val ci_concurrent_id : Var.t

(** Corresponds to [CI_CONCURRENT_PROJECT_ID].

    The unique ID of build execution in a single executor and project.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [11.10] runners. *)
val ci_concurrent_project_id : Var.t

(** Corresponds to [CI_CONFIG_PATH].

    The path to the CI/CD configuration file. Defaults to .gitlab-ci.yml.
    Read-only inside a running pipeline.

    Context: [Pipeline]. Available since GitLab [9.4].
    Available in [0.5] runners. *)
val ci_config_path : Var.t

(** Corresponds to [CI_DEBUG_TRACE].

    ["true"] if debug logging (tracing) is enabled.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [1.7] runners. *)
val ci_debug_trace : Var.t

(** Corresponds to [CI_DEBUG_SERVICES].

    ["true"] if service container logging is enabled.

    Context: [Pipeline]. Available since GitLab [15.7].
    Available in [15.7] runners. *)
val ci_debug_services : Var.t

(** Corresponds to [CI_DEFAULT_BRANCH].

    The name of the project’s default branch.

    Context: [Pipeline]. Available since GitLab [12.4].
    Available in [all] runners. *)
val ci_default_branch : Var.t

(** Corresponds to [CI_DEPENDENCY_PROXY_DIRECT_GROUP_IMAGE_PREFIX].

    The direct group image prefix for pulling images through the Dependency
    Proxy.

    Context: [Pipeline]. Available since GitLab [14.3].
    Available in [all] runners. *)
val ci_dependency_proxy_direct_group_image_prefix : Var.t

(** Corresponds to [CI_DEPENDENCY_PROXY_GROUP_IMAGE_PREFIX].

    The top-level group image prefix for pulling images through the Dependency
    Proxy.

    Context: [Pipeline]. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_dependency_proxy_group_image_prefix : Var.t

(** Corresponds to [CI_DEPENDENCY_PROXY_PASSWORD].

    The password to pull images through the Dependency Proxy.

    Context: [Pipeline]. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_dependency_proxy_password : Var.t

(** Corresponds to [CI_DEPENDENCY_PROXY_SERVER].

    The server for logging in to the Dependency Proxy. This is equivalent to
    $CI_SERVER_HOST:$CI_SERVER_PORT.

    Context: [Pipeline]. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_dependency_proxy_server : Var.t

(** Corresponds to [CI_DEPENDENCY_PROXY_USER].

    The username to pull images through the Dependency Proxy.

    Context: [Pipeline]. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_dependency_proxy_user : Var.t

(** Corresponds to [CI_DEPLOY_FREEZE].

    Only available if the pipeline runs during a deploy freeze window. ["true"]
    when available.

    Context: [Pipeline]. Available since GitLab [13.2].
    Available in [all] runners. *)
val ci_deploy_freeze : Var.t

(** Corresponds to [CI_DEPLOY_PASSWORD].

    The authentication password of the GitLab Deploy Token, if the project has
    one.

    Context: [Jobs only]. Available since GitLab [10.8].
    Available in [all] runners. *)
val ci_deploy_password : Var.t

(** Corresponds to [CI_DEPLOY_USER].

    The authentication username of the GitLab Deploy Token, if the project has
    one.

    Context: [Jobs only]. Available since GitLab [10.8].
    Available in [all] runners. *)
val ci_deploy_user : Var.t

(** Corresponds to [CI_DISPOSABLE_ENVIRONMENT].

    Only available if the job is executed in a disposable environment
    (something that is created only for this job and disposed of/destroyed
    after the execution - all executors except shell and ssh). ["true"] when
    available.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [10.1] runners. *)
val ci_disposable_environment : Var.t

(** Corresponds to [CI_ENVIRONMENT_NAME].

    The name of the environment for this job. Available if environment:name is
    set.

    Context: [Pipeline]. Available since GitLab [8.15].
    Available in [all] runners. *)
val ci_environment_name : Var.t

(** Corresponds to [CI_ENVIRONMENT_SLUG].

    The simplified version of the environment name, suitable for inclusion in
    DNS, URLs, Kubernetes labels, and so on. Available if environment:name is
    set. The slug is truncated to 24 characters. A random suffix is
    automatically added to uppercase environment names.

    Context: [Pipeline]. Available since GitLab [8.15].
    Available in [all] runners. *)
val ci_environment_slug : Var.t

(** Corresponds to [CI_ENVIRONMENT_URL].

    The URL of the environment for this job. Available if environment:url is
    set.

    Context: [Pipeline]. Available since GitLab [9.3].
    Available in [all] runners. *)
val ci_environment_url : Var.t

(** Corresponds to [CI_ENVIRONMENT_ACTION].

    The action annotation specified for this job’s environment. Available if
    environment:action is set. Can be start, prepare, or stop.

    Context: [Pipeline]. Available since GitLab [13.11].
    Available in [all] runners. *)
val ci_environment_action : Var.t

(** Corresponds to [CI_ENVIRONMENT_TIER].

    The deployment tier of the environment for this job.

    Context: [Pipeline]. Available since GitLab [14.0].
    Available in [all] runners. *)
val ci_environment_tier : Var.t

(** Corresponds to [CI_RELEASE_DESCRIPTION].

    The description of the release. Available only on pipelines for tags.
    Description length is limited to first 1024 characters.

    Context: [Pipeline]. Available since GitLab [15.5].
    Available in [all] runners. *)
val ci_release_description : Var.t

(** Corresponds to [CI_GITLAB_FIPS_MODE].

    Only available if FIPS mode is enabled in the GitLab instance. ["true"] when
    available.

    Context: [Pipeline]. Available since GitLab [14.10].
    Available in [all] runners. *)
val ci_gitlab_fips_mode : Var.t

(** Corresponds to [CI_HAS_OPEN_REQUIREMENTS].

    Only available if the pipeline’s project has an open requirement. ["true"]
    when available.

    Context: [Pipeline]. Available since GitLab [13.1].
    Available in [all] runners. *)
val ci_has_open_requirements : Var.t

(** Corresponds to [CI_JOB_ID].

    The internal ID of the job, unique across all jobs in the GitLab instance.

    Context: [Jobs only]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_job_id : Var.t

(** Corresponds to [CI_JOB_IMAGE].

    The name of the Docker image running the job.

    Context: [Pipeline]. Available since GitLab [12.9].
    Available in [12.9] runners. *)
val ci_job_image : Var.t

(** Corresponds to [CI_JOB_MANUAL].

    Only available if the job was started manually. ["true"] when available.

    Context: [Pipeline]. Available since GitLab [8.12].
    Available in [all] runners. *)
val ci_job_manual : Var.t

(** Corresponds to [CI_JOB_NAME].

    The name of the job.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [0.5] runners. *)
val ci_job_name : Var.t

(** Corresponds to [CI_JOB_NAME_SLUG].

    CI_JOB_NAME in lowercase, shortened to 63 bytes, and with everything except
    0-9 and a-z replaced with -. No leading / trailing -. Use in paths.

    Context: [Pipeline]. Available since GitLab [15.4].
    Available in [all] runners. *)
val ci_job_name_slug : Var.t

(** Corresponds to [CI_JOB_STAGE].

    The name of the job’s stage.

    Context: [Pipeline]. Available since GitLab [9.0].
    Available in [0.5] runners. *)
val ci_job_stage : Var.t

(** Corresponds to [CI_JOB_STATUS].

    The status of the job as each runner stage is executed. Use with
    after_script. Can be success, failed, or canceled.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [13.5] runners. *)
val ci_job_status : Var.t

(** Corresponds to [CI_JOB_TIMEOUT].

    The job timeout, in seconds.

    Context: [Jobs only]. Available since GitLab [15.7].
    Available in [15.7] runners. *)
val ci_job_timeout : Var.t

(** Corresponds to [CI_JOB_TOKEN].

    A token to authenticate with certain API endpoints. The token is valid as
    long as the job is running.

    Context: [Jobs only]. Available since GitLab [9.0].
    Available in [1.2] runners. *)
val ci_job_token : Var.t

(** Corresponds to [CI_JOB_URL].

    The job details URL.

    Context: [Jobs only]. Available since GitLab [11.1].
    Available in [0.5] runners. *)
val ci_job_url : Var.t

(** Corresponds to [CI_JOB_STARTED_AT].

    The UTC datetime when a job started, in ISO 8601 format. For example,
    2022-01-31T16:47:55Z.

    Context: [Jobs only]. Available since GitLab [13.10].
    Available in [all] runners. *)
val ci_job_started_at : Var.t

(** Corresponds to [CI_KUBERNETES_ACTIVE].

    Only available if the pipeline has a Kubernetes cluster available for
    deployments. ["true"] when available.

    Context: [Pipeline]. Available since GitLab [13.0].
    Available in [all] runners. *)
val ci_kubernetes_active : Var.t

(** Corresponds to [CI_NODE_INDEX].

    The index of the job in the job set. Only available if the job uses
    parallel.

    Context: [Pipeline]. Available since GitLab [11.5].
    Available in [all] runners. *)
val ci_node_index : Var.t

(** Corresponds to [CI_NODE_TOTAL].

    The total number of instances of this job running in parallel. Set to 1 if
    the job does not use parallel.

    Context: [Pipeline]. Available since GitLab [11.5].
    Available in [all] runners. *)
val ci_node_total : Var.t

(** Corresponds to [CI_OPEN_MERGE_REQUESTS].

    A comma-separated list of up to four merge requests that use the current
    branch and project as the merge request source. Only available in branch
    and merge request pipelines if the branch has an associated merge request.
    For example, gitlab-org/gitlab!333,gitlab-org/gitlab-foss!11.

    Context: [Pipeline]. Available since GitLab [13.8].
    Available in [all] runners. *)
val ci_open_merge_requests : Var.t

(** Corresponds to [CI_PAGES_DOMAIN].

    The configured domain that hosts GitLab Pages.

    Context: [Pipeline]. Available since GitLab [11.8].
    Available in [all] runners. *)
val ci_pages_domain : Var.t

(** Corresponds to [CI_PAGES_URL].

    The URL for a GitLab Pages site. Always a subdomain of CI_PAGES_DOMAIN.

    Context: [Pipeline]. Available since GitLab [11.8].
    Available in [all] runners. *)
val ci_pages_url : Var.t

(** Corresponds to [CI_PIPELINE_ID].

    The instance-level ID of the current pipeline. This ID is unique across all
    projects on the GitLab instance.

    Context: [Jobs only]. Available since GitLab [8.10].
    Available in [all] runners. *)
val ci_pipeline_id : Var.t

(** Corresponds to [CI_PIPELINE_IID].

    The project-level IID (internal ID) of the current pipeline. This ID is
    unique only within the current project.

    Context: [Pipeline]. Available since GitLab [11.0].
    Available in [all] runners. *)
val ci_pipeline_iid : Var.t

(** Corresponds to [CI_PIPELINE_SOURCE].

    How the pipeline was triggered. Can be push, web, schedule, api, external,
    chat, webide, merge_request_event, external_pull_request_event,
    parent_pipeline, trigger, or pipeline. For a description of each value, see
    Common if clauses for rules, which uses this variable to control when jobs
    run.

    Context: [Pipeline]. Available since GitLab [10.0].
    Available in [all] runners. *)
val ci_pipeline_source : Var.t

(** Corresponds to [CI_PIPELINE_TRIGGERED].

    ["true"] if the job was triggered.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_pipeline_triggered : Var.t

(** Corresponds to [CI_PIPELINE_URL].

    The URL for the pipeline details.

    Context: [Jobs only]. Available since GitLab [11.1].
    Available in [0.5] runners. *)
val ci_pipeline_url : Var.t

(** Corresponds to [CI_PIPELINE_CREATED_AT].

    The UTC datetime when the pipeline was created, in ISO 8601 format. For
    example, 2022-01-31T16:47:55Z.

    Context: [Pipeline]. Available since GitLab [13.10].
    Available in [all] runners. *)
val ci_pipeline_created_at : Var.t

(** Corresponds to [CI_PIPELINE_NAME].

    The pipeline name defined in workflow:name

    Context: [Pipeline]. Available since GitLab [16.3].
    Available in [all] runners. *)
val ci_pipeline_name : Var.t

(** Corresponds to [CI_PROJECT_DIR].

    The full path the repository is cloned to, and where the job runs from. If
    the GitLab Runner builds_dir parameter is set, this variable is set
    relative to the value of builds_dir. For more information, see the Advanced
    GitLab Runner configuration.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_project_dir : Var.t

(** Corresponds to [CI_PROJECT_ID].

    The ID of the current project. This ID is unique across all projects on the
    GitLab instance.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_project_id : Var.t

(** Corresponds to [CI_PROJECT_NAME].

    The name of the directory for the project. For example if the project URL
    is gitlab.example.com/group-name/project-1, CI_PROJECT_NAME is project-1.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_project_name : Var.t

(** Corresponds to [CI_PROJECT_NAMESPACE].

    The project namespace (username or group name) of the job.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_project_namespace : Var.t

(** Corresponds to [CI_PROJECT_NAMESPACE_ID].

    The project namespace ID of the job.

    Context: [Pipeline]. Available since GitLab [15.7].
    Available in [0.5] runners. *)
val ci_project_namespace_id : Var.t

(** Corresponds to [CI_PROJECT_PATH_SLUG].

    $CI_PROJECT_PATH in lowercase with characters that are not a-z or 0-9
    replaced with - and shortened to 63 bytes. Use in URLs and domain names.

    Context: [Pipeline]. Available since GitLab [9.3].
    Available in [all] runners. *)
val ci_project_path_slug : Var.t

(** Corresponds to [CI_PROJECT_PATH].

    The project namespace with the project name included.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_project_path : Var.t

(** Corresponds to [CI_PROJECT_REPOSITORY_LANGUAGES].

    A comma-separated, lowercase list of the languages used in the repository.
    For example ruby,javascript,html,css. The maximum number of languages is
    limited to 5. An issue proposes to increase the limit.

    Context: [Pipeline]. Available since GitLab [12.3].
    Available in [all] runners. *)
val ci_project_repository_languages : Var.t

(** Corresponds to [CI_PROJECT_ROOT_NAMESPACE].

    The root project namespace (username or group name) of the job. For
    example, if CI_PROJECT_NAMESPACE is
    root-group/child-group/grandchild-group, CI_PROJECT_ROOT_NAMESPACE is
    root-group.

    Context: [Pipeline]. Available since GitLab [13.2].
    Available in [0.5] runners. *)
val ci_project_root_namespace : Var.t

(** Corresponds to [CI_PROJECT_TITLE].

    The human-readable project name as displayed in the GitLab web interface.

    Context: [Pipeline]. Available since GitLab [12.4].
    Available in [all] runners. *)
val ci_project_title : Var.t

(** Corresponds to [CI_PROJECT_DESCRIPTION].

    The project description as displayed in the GitLab web interface.

    Context: [Pipeline]. Available since GitLab [15.1].
    Available in [all] runners. *)
val ci_project_description : Var.t

(** Corresponds to [CI_PROJECT_URL].

    The HTTP(S) address of the project.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_project_url : Var.t

(** Corresponds to [CI_PROJECT_VISIBILITY].

    The project visibility. Can be internal, private, or public.

    Context: [Pipeline]. Available since GitLab [10.3].
    Available in [all] runners. *)
val ci_project_visibility : Var.t

(** Corresponds to [CI_PROJECT_CLASSIFICATION_LABEL].

    The project external authorization classification label.

    Context: [Pipeline]. Available since GitLab [14.2].
    Available in [all] runners. *)
val ci_project_classification_label : Var.t

(** Corresponds to [CI_REGISTRY].

    Address of the container registry server, formatted as <host>[:<port>]. For
    example: registry.gitlab.example.com. Only available if the container
    registry is enabled for the GitLab instance.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_registry : Var.t

(** Corresponds to [CI_REGISTRY_IMAGE].

    Base address for the container registry to push, pull, or tag project’s
    images, formatted as <host>[:<port>]/<project_full_path>. For example:
    registry.gitlab.example.com/my_group/my_project. Image names must follow
    the container registry naming convention. Only available if the container
    registry is enabled for the project.

    Context: [Pipeline]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_registry_image : Var.t

(** Corresponds to [CI_REGISTRY_PASSWORD].

    The password to push containers to the GitLab project’s container
    registry. Only available if the container registry is enabled for the
    project. This password value is the same as the CI_JOB_TOKEN and is valid
    only as long as the job is running. Use the CI_DEPLOY_PASSWORD for
    long-lived access to the registry

    Context: [Jobs only]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_registry_password : Var.t

(** Corresponds to [CI_REGISTRY_USER].

    The username to push containers to the project’s GitLab container
    registry. Only available if the container registry is enabled for the
    project.

    Context: [Jobs only]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_registry_user : Var.t

(** Corresponds to [CI_REPOSITORY_URL].

    The full path to Git clone (HTTP) the repository with a CI/CD job token, in
    the format
    https://gitlab-ci-token:$CI_JOB_TOKEN@gitlab.example.com/my-group/my-project
    .git.

    Context: [Jobs only]. Available since GitLab [9.0].
    Available in [all] runners. *)
val ci_repository_url : Var.t

(** Corresponds to [CI_RUNNER_DESCRIPTION].

    The description of the runner.

    Context: [Jobs only]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_runner_description : Var.t

(** Corresponds to [CI_RUNNER_EXECUTABLE_ARCH].

    The OS/architecture of the GitLab Runner executable. Might not be the same
    as the environment of the executor.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [10.6] runners. *)
val ci_runner_executable_arch : Var.t

(** Corresponds to [CI_RUNNER_ID].

    The unique ID of the runner being used.

    Context: [Jobs only]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_runner_id : Var.t

(** Corresponds to [CI_RUNNER_REVISION].

    The revision of the runner running the job.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [10.6] runners. *)
val ci_runner_revision : Var.t

(** Corresponds to [CI_RUNNER_SHORT_TOKEN].

    The runner’s unique ID, used to authenticate new job requests. In GitLab
    14.9 and later, the token contains a prefix, and the first 17 characters
    are used. Prior to 14.9, the first eight characters are used.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [12.3] runners. *)
val ci_runner_short_token : Var.t

(** Corresponds to [CI_RUNNER_TAGS].

    A comma-separated list of the runner tags.

    Context: [Jobs only]. Available since GitLab [8.10].
    Available in [0.5] runners. *)
val ci_runner_tags : Var.t

(** Corresponds to [CI_RUNNER_VERSION].

    The version of the GitLab Runner running the job.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [10.6] runners. *)
val ci_runner_version : Var.t

(** Corresponds to [CI_SERVER_HOST].

    The host of the GitLab instance URL, without protocol or port. For example
    gitlab.example.com.

    Context: [Pipeline]. Available since GitLab [12.1].
    Available in [all] runners. *)
val ci_server_host : Var.t

(** Corresponds to [CI_SERVER_NAME].

    The name of CI/CD server that coordinates jobs.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_name : Var.t

(** Corresponds to [CI_SERVER_PORT].

    The port of the GitLab instance URL, without host or protocol. For example
    8080.

    Context: [Pipeline]. Available since GitLab [12.8].
    Available in [all] runners. *)
val ci_server_port : Var.t

(** Corresponds to [CI_SERVER_PROTOCOL].

    The protocol of the GitLab instance URL, without host or port. For example
    https.

    Context: [Pipeline]. Available since GitLab [12.8].
    Available in [all] runners. *)
val ci_server_protocol : Var.t

(** Corresponds to [CI_SERVER_SHELL_SSH_HOST].

    The SSH host of the GitLab instance, used for access to Git repositories
    via SSH. For example gitlab.com.

    Context: [Pipeline]. Available since GitLab [15.11].
    Available in [all] runners. *)
val ci_server_shell_ssh_host : Var.t

(** Corresponds to [CI_SERVER_SHELL_SSH_PORT].

    The SSH port of the GitLab instance, used for access to Git repositories
    via SSH. For example 22.

    Context: [Pipeline]. Available since GitLab [15.11].
    Available in [all] runners. *)
val ci_server_shell_ssh_port : Var.t

(** Corresponds to [CI_SERVER_REVISION].

    GitLab revision that schedules jobs.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_revision : Var.t

(** Corresponds to [CI_SERVER_TLS_CA_FILE].

    File containing the TLS CA certificate to verify the GitLab server when
    tls-ca-file set in runner settings.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_tls_ca_file : Var.t

(** Corresponds to [CI_SERVER_TLS_CERT_FILE].

    File containing the TLS certificate to verify the GitLab server when
    tls-cert-file set in runner settings.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_tls_cert_file : Var.t

(** Corresponds to [CI_SERVER_TLS_KEY_FILE].

    File containing the TLS key to verify the GitLab server when tls-key-file
    set in runner settings.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_tls_key_file : Var.t

(** Corresponds to [CI_SERVER_URL].

    The base URL of the GitLab instance, including protocol and port. For
    example https://gitlab.example.com:8080.

    Context: [Pipeline]. Available since GitLab [12.7].
    Available in [all] runners. *)
val ci_server_url : Var.t

(** Corresponds to [CI_SERVER_VERSION_MAJOR].

    The major version of the GitLab instance. For example, if the GitLab
    version is 13.6.1, the CI_SERVER_VERSION_MAJOR is 13.

    Context: [Pipeline]. Available since GitLab [11.4].
    Available in [all] runners. *)
val ci_server_version_major : Var.t

(** Corresponds to [CI_SERVER_VERSION_MINOR].

    The minor version of the GitLab instance. For example, if the GitLab
    version is 13.6.1, the CI_SERVER_VERSION_MINOR is 6.

    Context: [Pipeline]. Available since GitLab [11.4].
    Available in [all] runners. *)
val ci_server_version_minor : Var.t

(** Corresponds to [CI_SERVER_VERSION_PATCH].

    The patch version of the GitLab instance. For example, if the GitLab
    version is 13.6.1, the CI_SERVER_VERSION_PATCH is 1.

    Context: [Pipeline]. Available since GitLab [11.4].
    Available in [all] runners. *)
val ci_server_version_patch : Var.t

(** Corresponds to [CI_SERVER_VERSION].

    The full version of the GitLab instance.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server_version : Var.t

(** Corresponds to [CI_SERVER].

    Available for all jobs executed in CI/CD. yes when available.

    Context: [Jobs only]. Available since GitLab [all].
    Available in [all] runners. *)
val ci_server : Var.t

(** Corresponds to [CI_SHARED_ENVIRONMENT].

    Only available if the job is executed in a shared environment (something
    that is persisted across CI/CD invocations, like the shell or ssh
    executor). ["true"] when available.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [10.1] runners. *)
val ci_shared_environment : Var.t

(** Corresponds to [CI_TEMPLATE_REGISTRY_HOST].

    The host of the registry used by CI/CD templates. Defaults to
    registry.gitlab.com.

    Context: [Pipeline]. Available since GitLab [15.3].
    Available in [all] runners. *)
val ci_template_registry_host : Var.t

(** Corresponds to [GITLAB_CI].

    Available for all jobs executed in CI/CD. ["true"] when available.

    Context: [Pipeline]. Available since GitLab [all].
    Available in [all] runners. *)
val gitlab_ci : Var.t

(** Corresponds to [GITLAB_FEATURES].

    The comma-separated list of licensed features available for the GitLab
    instance and license.

    Context: [Pipeline]. Available since GitLab [10.6].
    Available in [all] runners. *)
val gitlab_features : Var.t

(** Corresponds to [GITLAB_USER_EMAIL].

    The email of the user who started the pipeline, unless the job is a manual
    job. In manual jobs, the value is the email of the user who started the job.

    Context: [Pipeline]. Available since GitLab [8.12].
    Available in [all] runners. *)
val gitlab_user_email : Var.t

(** Corresponds to [GITLAB_USER_ID].

    The numeric ID of the user who started the pipeline, unless the job is a
    manual job. In manual jobs, the value is the ID of the user who started the
    job.

    Context: [Pipeline]. Available since GitLab [8.12].
    Available in [all] runners. *)
val gitlab_user_id : Var.t

(** Corresponds to [GITLAB_USER_LOGIN].

    The username of the user who started the pipeline, unless the job is a
    manual job. In manual jobs, the value is the username of the user who
    started the job.

    Context: [Pipeline]. Available since GitLab [10.0].
    Available in [all] runners. *)
val gitlab_user_login : Var.t

(** Corresponds to [GITLAB_USER_NAME].

    The display name of the user who started the pipeline, unless the job is a
    manual job. In manual jobs, the value is the name of the user who started
    the job.

    Context: [Pipeline]. Available since GitLab [10.0].
    Available in [all] runners. *)
val gitlab_user_name : Var.t

(** Corresponds to [KUBECONFIG].

    The path to the kubeconfig file with contexts for every shared agent
    connection. Only available when a GitLab agent is authorized to access the
    project.

    Context: [Pipeline]. Available since GitLab [14.2].
    Available in [all] runners. *)
val kubeconfig : Var.t

(** Corresponds to [TRIGGER_PAYLOAD].

    The webhook payload. Only available when a pipeline is triggered with a
    webhook.

    Context: [Pipeline]. Available since GitLab [13.9].
    Available in [all] runners. *)
val trigger_payload : Var.t

(** {2 Predefined variables for merge request pipelines} *)

(** Corresponds to [CI_MERGE_REQUEST_APPROVED].

    Approval status of the merge request. ["true"] when merge request approvals is
    available and the merge request has been approved.

    Context: merge requests. Available since GitLab [14.1].
    Available in [all] runners. *)
val ci_merge_request_approved : Var.t

(** Corresponds to [CI_MERGE_REQUEST_ASSIGNEES].

    Comma-separated list of usernames of assignees for the merge request.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_assignees : Var.t

(** Corresponds to [CI_MERGE_REQUEST_DIFF_BASE_SHA].

    The base SHA of the merge request diff.

    Context: merge requests. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_merge_request_diff_base_sha : Var.t

(** Corresponds to [CI_MERGE_REQUEST_DIFF_ID].

    The version of the merge request diff.

    Context: merge requests. Available since GitLab [13.7].
    Available in [all] runners. *)
val ci_merge_request_diff_id : Var.t

(** Corresponds to [CI_MERGE_REQUEST_EVENT_TYPE].

    The event type of the merge request. Can be detached, merged_result or
    merge_train.

    Context: merge requests. Available since GitLab [12.3].
    Available in [all] runners. *)
val ci_merge_request_event_type : Var.t

(** Corresponds to [CI_MERGE_REQUEST_DESCRIPTION].

    The description of the merge request. If the description is more than 2700
    characters long, only the first 2700 characters are stored in the variable.

    Context: merge requests. Available since GitLab [16.7].
    Available in [all] runners. *)
val ci_merge_request_description : Var.t

(** Corresponds to [CI_MERGE_REQUEST_DESCRIPTION_IS_TRUNCATED].

    ["true"] if CI_MERGE_REQUEST_DESCRIPTION is truncated down to 2700 characters
    because the description of the merge request is too long.

    Context: merge requests. Available since GitLab [16.8].
    Available in [all] runners. *)
val ci_merge_request_description_is_truncated : Var.t

(** Corresponds to [CI_MERGE_REQUEST_ID].

    The instance-level ID of the merge request. This is a unique ID across all
    projects on the GitLab instance.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_id : Var.t

(** Corresponds to [CI_MERGE_REQUEST_IID].

    The project-level IID (internal ID) of the merge request. This ID is unique
    for the current project, and is the number used in the merge request URL,
    page title, and other visible locations.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_iid : Var.t

(** Corresponds to [CI_MERGE_REQUEST_LABELS].

    Comma-separated label names of the merge request.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_labels : Var.t

(** Corresponds to [CI_MERGE_REQUEST_MILESTONE].

    The milestone title of the merge request.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_milestone : Var.t

(** Corresponds to [CI_MERGE_REQUEST_PROJECT_ID].

    The ID of the project of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_project_id : Var.t

(** Corresponds to [CI_MERGE_REQUEST_PROJECT_PATH].

    The path of the project of the merge request. For example
    namespace/awesome-project.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_project_path : Var.t

(** Corresponds to [CI_MERGE_REQUEST_PROJECT_URL].

    The URL of the project of the merge request. For example,
    http://192.168.10.15:3000/namespace/awesome-project.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_project_url : Var.t

(** Corresponds to [CI_MERGE_REQUEST_REF_PATH].

    The ref path of the merge request. For example, refs/merge-requests/1/head.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_ref_path : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_BRANCH_NAME].

    The source branch name of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_source_branch_name : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_BRANCH_PROTECTED].

    ["true"] when the source branch of the merge request is protected.

    Context: merge requests. Available since GitLab [16.4].
    Available in [all] runners. *)
val ci_merge_request_source_branch_protected : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_BRANCH_SHA].

    The HEAD SHA of the source branch of the merge request. The variable is
    empty in merge request pipelines. The SHA is present only in merged results
    pipelines.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_source_branch_sha : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_PROJECT_ID].

    The ID of the source project of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_source_project_id : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_PROJECT_PATH].

    The path of the source project of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_source_project_path : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SOURCE_PROJECT_URL].

    The URL of the source project of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_source_project_url : Var.t

(** Corresponds to [CI_MERGE_REQUEST_SQUASH_ON_MERGE].

    ["true"] when the squash on merge option is set.

    Context: merge requests. Available since GitLab [16.4].
    Available in [all] runners. *)
val ci_merge_request_squash_on_merge : Var.t

(** Corresponds to [CI_MERGE_REQUEST_TARGET_BRANCH_NAME].

    The target branch name of the merge request.

    Context: merge requests. Available since GitLab [11.6].
    Available in [all] runners. *)
val ci_merge_request_target_branch_name : Var.t

(** Corresponds to [CI_MERGE_REQUEST_TARGET_BRANCH_PROTECTED].

    ["true"] when the target branch of the merge request is protected.

    Context: merge requests. Available since GitLab [15.2].
    Available in [all] runners. *)
val ci_merge_request_target_branch_protected : Var.t

(** Corresponds to [CI_MERGE_REQUEST_TARGET_BRANCH_SHA].

    The HEAD SHA of the target branch of the merge request. The variable is
    empty in merge request pipelines. The SHA is present only in merged results
    pipelines.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_target_branch_sha : Var.t

(** Corresponds to [CI_MERGE_REQUEST_TITLE].

    The title of the merge request.

    Context: merge requests. Available since GitLab [11.9].
    Available in [all] runners. *)
val ci_merge_request_title : Var.t
