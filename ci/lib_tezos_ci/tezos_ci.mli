(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A GitLab CI job annotated with Octez-specific meta-data. *)
type tezos_job

module Rules = Rules
module Runner = Runner

(** The name of a {!tezos_job} as given to [~name] of {!job}.

    This returns the stem of the job name. Parallel jobs produce
    multiple job instances ([JOB N/M] for a {!Vector}-parallel job and
    [JOB: [foo, bar, ...]] for {!Matrix}-parallel jobs) -- the stem of such
    jobs is [JOB]. For non-parallel jobs, the argument given to
    [~name] and the stem is equivalent. *)
val name_of_tezos_job : tezos_job -> string

(** A string that should be prepended to all generated files.

    Warns not to modify the generated files, and refers to the generator. *)
val header : string

(** Write a CI configuration to a file *)
val to_file : filename:string -> Gitlab_ci.Types.config -> unit

(** Checks that should be performed after {!Pipeline.write}.

    Checks that the file [.gitlab-ci.yml] and all the [.yml] files in [.gitlab]
    are either generated or excluded. It is an error if a generated file is excluded.
    You can use [exclude] to specify which files should be excluded.
    [exclude] is given a path relative to the [root] directory
    and shall return [true] for excluded paths.

    In case of errors, errors are printed and the process exits with exit code 1. *)
val check_files :
  remove_extra_files:bool -> ?exclude:(string -> bool) -> unit -> unit

(** Run-time configuration and command-line processing. *)
module Cli : sig
  (** Action the binary should perform. *)
  type action =
    | Write  (** Write the CI configuration *)
    | Overview_pipelines  (** Print pipelines as table. *)
    | List_pipelines  (** List registered pipelines. *)
    | Describe_pipeline of {name : string}
        (** Describe a registered pipeline. *)

  (** Type of the command-line configuration for the generator binary. *)
  type config = {
    mutable verbose : bool;
        (** Enable [verbose] output, including the source of generated jobs. *)
    mutable inline_source_info : bool;
        (** Enable the emission of source information in generated configuration. *)
    mutable remove_extra_files : bool;
        (** Remove files that are neither generated nor excluded. *)
    mutable action : action;  (** Action to perform *)
  }

  (** Populate  {!config} from command-line arguments.

      Terminates the program with usage help if invalid arguments, or
      [--help] is passed. *)
  val init : unit -> unit

  (** The current command-line configuration, as populated by {!init}. *)
  val config : config
end

(** A facility for registering pipeline stages. *)
module Stage : sig
  (* Represents a pipeline stage *)
  type t

  (** Register a stage.

      Fails if a stage of the same name has already been registered. *)
  val register : string -> t
end

(** A facility for registering pipelines. *)
module Pipeline : sig
  (** Default pipeline configuration.

      It is included in [.gitlab-ci.yml] and thus all pipelines inherit it,
      with the exception of child pipelines. You can specify different defaults
      for child pipelines, with the default defaults being [default_config]. *)
  val default_config : Gitlab_ci.Types.default

  (** Register a pipeline.

      [register ~description ?variables ?default name rule] will register a pipeline [name]
      that runs when [rule] is true.

      If [variables] is set, then these variables will be added to the
      [workflow:] clause for this pipeline in the top-level [.gitlab-ci.yml].
      Similarly, an [auto_cancel] clause can be specified.

      If [default] is specified, it will be written to the pipeline's YAML
      as a [default:] section, providing default job configurations for all
      jobs in the pipeline (such as retry policies, interruptible settings, etc).

      The [description] is printed in {!list_pipelines}.

      The [jobs] of the pipeline are generated to the file
      [.gitlab/ci/pipelines/NAME.yml] when {!write} is called. *)
  val register :
    ?variables:Gitlab_ci.Types.variables ->
    ?auto_cancel:Gitlab_ci.Types.auto_cancel ->
    description:string ->
    jobs:tezos_job list ->
    string ->
    Gitlab_ci.If.t ->
    unit

  (** A child pipeline.

      See {!register_child} and {!trigger_job} for more information. *)
  type child_pipeline

  (** Register a child pipeline.

      [register_child name] will register a child pipeline called [name].

      The [jobs] of the pipeline are generated to the file
      [.gitlab/ci/pipelines/NAME.yml] when {!write} is called. See
      {!register} for info on [auto_cancel] and [default].

      The [description] is printed in {!list_pipelines}.

      Child pipelines cannot be launched without a trigger job that is
      included in a regular pipeline (see {!trigger_job}). *)
  val register_child :
    ?auto_cancel:Gitlab_ci.Types.auto_cancel ->
    ?inherit_:Gitlab_ci.Types.inherit_ ->
    ?default:Gitlab_ci.Types.default ->
    description:string ->
    jobs:tezos_job list ->
    string ->
    child_pipeline

  (** Writes the set of registered pipelines.

      A top-level configuration is generated to [filename]. It
      contains a [workflow:] section that enables pipeline execution
      for each registered, non-child pipeline and an [include:]
      section that includes the set of jobs for the given pipeline. If
      specified, [default:] and [variables:] sections are also written
      to the top-level configuration. The [stages:] section
      automatically contains all the stages registered with
      {!Stage.register} that are used in the given pipeline.

      A [dummy_job] is written to the top-level configuration. This
      job is never enabled, but it works around a GitLab CI issue that
      occurs when the top-level configuration contains no visible
      jobs.

      Then, each registered pipeline is written to
      [.gitlab/ci/pipelines/NAME.yml].

      The string {!header} will be prepended to each written file. *)
  val write :
    ?default:Gitlab_ci.Types.default ->
    ?variables:Gitlab_ci.Types.variables ->
    filename:string ->
    unit ->
    unit

  (** Outputs the set of registered pipelines with their description. *)
  val list_pipelines : unit -> unit

  (** Pretty prints the set of registered pipelines as a table. *)
  val overview_pipelines : unit -> unit

  (** Describe the registered pipeline of a given name.

      If no such pipeline is registered, [exit 1] is called and an
      error message is printed. *)
  val describe_pipeline : string -> unit
end

module Cache : sig
  (* Possible values for [~key] arguments.

     - [Job]: share the cache between all instances of the job, across all pipelines.
     - [Pipeline]: share the cache between all jobs of each pipeline, but not across pipelines. *)
  type cache_key = Job | Pipeline

  (** Add variable enabling dune cache.

    This function can be applied to jobs that run dune.

    - [key] specifies which jobs the cache should be shared with.
      See {!cache_key}.

    - [policy] specifies whether to pull the cache when the job starts,
      and whether to push the cache when the job finishes.

    *)

  val enable_dune_cache :
    ?key:cache_key ->
    ?policy:Gitlab_ci.Types.cache_policy ->
    tezos_job ->
    tezos_job

  (** Add variable enabling sccache.

    This function should be applied to jobs that build rust files and
    which has a configured sccache Gitlab CI cache.

    - [error_log] and [log] sets the environment
    variables [SCCACHE_ERROR_LOG] and [SCCACHE_LOG] respectively.
    See the sccache documentation for more information on these variables. *)
  val enable_sccache :
    ?error_log:string ->
    ?log:string ->
    ?policy:Gitlab_ci.Types.cache_policy ->
    tezos_job ->
    tezos_job

  (** Value of [CARGO_HOME] *)
  val cargo_home : string

  (** Allow cargo to access the network by setting [CARGO_NET_OFFLINE=false].

    This function should only be applied to jobs that have a GitLab CI
    cache for [CARGO_HOME], as enabled through [enable_cache_cargo] (that
    function calls this function, so there is no need to apply both).
    Exceptions can be made for jobs that must have CARGO_HOME set to
    something different than {!cargo_home}. *)
  val enable_networked_cargo : tezos_job -> tezos_job

  (** Adds a GitLab CI cache for the CARGO_HOME folder.

    More precisely, we only cache the non-SCM dependencies in the
    sub-directory [registry/cache]. *)
  val enable_cargo_cache : tezos_job -> tezos_job
end

(** A facility for registering images for [image:] keywords.

    Images can be registered in two manners:
     - {i External} images are built outside the [tezos/tezos] CI and are
       registered with their path (e.g. [alpine:3.18]).

     - {i Internal} images are built in the pipeline they are
       used. They are registered an [image_path] and an
       [image_builder]. The [script:] of an [image_builder]
       must build the image and push it to [image_path].

    Pipelines with jobs using internal images automatically include
    corresponding image builder jobs with appropriate dependencies. *)
module Image : sig
  (** A registered Docker image in which a job can execute. *)
  type t

  (** Register an external image of the given [image_path]. *)
  val mk_external : image_path:string -> t

  (** Register internal image for [image_path] built by [image_builder_amd64].

      Optionally, a builder for an arm64 version of the image can be
      registered by supplying [image_builder_arm64]. If
      [image_builder_arm64] is supplied, then it must have a distinct
      name from [image_builder_amd64].

      Note: the name of the image builder(s) must uniquely identify the
      job definition. If two image builders with the same name but
      differing job definitions (as per polymorphic comparison of the
      underlying {!Gitlab_ci.Types.job}) are registered, then this
      function throws a run-time error. *)
  val mk_internal :
    ?image_builder_arm64:tezos_job ->
    image_builder_amd64:tezos_job ->
    image_path:string ->
    unit ->
    t
end

(** Changesets are used to specify [changes:] clauses in rules.

    Note: Operations over changesets do not preserve order nor
    duplicates. Ordering and duplicates in [changes:] clauses have no
    semantic impact. *)
module Changeset : sig
  (** A changeset. *)
  type t

  (** Create a changeset from a list of strings. *)
  val make : string list -> t

  (** Encode a changeset as a alphabetically sorted list of strings. *)
  val encode : t -> string list

  (** Combine two changesets. *)
  val union : t -> t -> t

  (** Operator for {!union}. *)
  val ( @ ) : t -> t -> t
end

(** A job dependency.

    - A job that depends on [Job j] will not start until [j] finishes.

    - A job that depends on [Optional j] will not start until [j]
    finishes, if it is present in the pipeline. For more information,
    see
    {{:https://docs.gitlab.com/ee/ci/yaml/#needsoptional}needs:optional}.

    - A job that depends on [Artefacts j] will not start until [j] finishes
      and will also have the artefacts of [j] available. *)
type dependency =
  | Job of tezos_job
  | Optional of tezos_job
  | Artifacts of tezos_job

(** Job dependency sets.

    - A [Staged artifact_deps] job implements the default GitLab CI behavior of
      running once all jobs in the previous stage have terminated. Artifacts are
      downloaded from the list of jobs in [artifact_deps] (by default, no
      artifacts are downloaded).
    - An [Dependent deps] job runs once all the jobs in [deps] have terminated.
      To have a job run immediately, set [Dependent []]

    In practice, prefer using [Dependent]. Only use [Staged
    artifact_deps] when the number of dependencies exceed the GitLab
    imposed limit of 50 [needs:] per job. *)
type dependencies = Staged of tezos_job list | Dependent of dependency list

(** Add the artifacts of [tezos_job] to a [dependencies] set. *)
val dependencies_add_artifact_dependency :
  dependencies -> tezos_job -> dependencies

(** Values for the [GIT_STRATEGY] variable.

    This can be used to specify whether a job should [Fetch] or [Clone]
    the git repository, or not get it at all with [No_strategy].

    For more information, see
   {{:https://docs.gitlab.com/ee/ci/runners/configure_runners.html#git-strategy}GIT_STRATEGY} *)
type git_strategy =
  | Fetch  (** Translates to [fetch]. *)
  | Clone  (** Translates to [clone]. *)
  | No_strategy
      (** Translates to [none].

          Renamed to avoid clashes with {!Option.None}. *)

(** GitLab CI/CD YAML representation of [git_strategy].

    Translates {!git_strategy} to values of accepted by the GitLab
    CI/CD YAML variable [GIT_STRATEGY]. *)
val enc_git_strategy : git_strategy -> string

(** Define a job.

    This is a smart constructor for {!Gitlab_ci.Types.job}.

    This function handles dependencies as follows.

    - Translates each {!dependency} to [needs:] and [dependencies:]
      keywords as detailed in the documentation of {!dependency}.

    - [image_dependencies] is a list of internal !{Image.t}s that this
      job uses indirectly, i.e. not in it's [image:] field. For
      instance, this can be used by a job that builds a Docker image
      with an internal image as input.  A run-time error will be thrown
      if this list includes an external image.

    - If both a [template] and an [image] are provided, then a
      run-time error is raised to prevent overriding the image defined
      in the GitLab template.

    - If the [image] used is {!Internal} and [tag] is set to
      {!Dynamic} then a run-time error is generated as the required
      architecture for the internal image cannot be statically
      deduced.

    This function accepts the following arguments to specify requirements
    on the runners that are to run the job.

    - [dev_infra] specifies the cloud provider account to use.
      [false] is [GCP] or [AWS], [true] is [GCP_dev].
      If omitted, any account may be used.

    - [arch] specifies a required CPU architecture.
      If omitted, any architecture may be used.

    - [cpu] specifies the required CPU power.
      If omitted, any CPU may be used.

    - [storage] specifies the required storage method.
      If omitted, any storage method may be used.

    - [interruptible_runner] specifies whether it is acceptable that the runner
      may be interrupted, for instance because of the spot instance mechanism.
      If omitted, either interruptible and non-interruptible runners may be used.

    From these requirements, the function selects a runner tag, using {!Runner.Tag.choose}.
    This takes the first tag that is compatible with your requirements,
    from a list of tags ordered in a priority list located in [runner.ml].
    You may also specify the runner [tag] explicitly instead,
    in which case the function simply checks that your requirements are compatible. *)
val job :
  ?arch:Runner.Arch.t ->
  ?after_script:string list ->
  ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
  ?artifacts:Gitlab_ci.Types.artifacts ->
  ?before_script:string list ->
  ?cache:Gitlab_ci.Types.cache list ->
  ?id_tokens:Gitlab_ci.Types.id_tokens ->
  ?interruptible:bool ->
  ?dependencies:dependencies ->
  ?image_dependencies:Image.t list ->
  ?services:Gitlab_ci.Types.service list ->
  ?variables:Gitlab_ci.Types.variables ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?timeout:Gitlab_ci.Types.time_interval ->
  ?tag:Runner.Tag.t ->
  ?cpu:Runner.CPU.t ->
  ?storage:Runner.Storage.t ->
  ?interruptible_runner:bool ->
  ?git_strategy:git_strategy ->
  ?retry:Gitlab_ci.Types.retry ->
  ?parallel:Gitlab_ci.Types.parallel ->
  ?description:string ->
  ?dev_infra:bool ->
  __POS__:string * int * int * int ->
  ?image:Image.t ->
  ?template:Gitlab_ci.Types.template ->
  ?datadog:bool ->
  stage:Stage.t ->
  name:string ->
  string list ->
  tezos_job

(** Define a trigger job for a child pipeline.

    The trigger job will be named [trigger:CHILD_PIPELINE_NAME].

    We may provide the [parent_pipeline_name] to include it in the
    PIPELINE_TYPE variable. This helps observability of pipelines
    containing child pipelines. *)
val trigger_job :
  ?dependencies:dependencies ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?description:string ->
  ?variables:(string * string) list ->
  __POS__:string * int * int * int ->
  stage:Stage.t ->
  ?parent_pipeline_name:string ->
  Pipeline.child_pipeline ->
  tezos_job

(** Adds artifacts to a job without overriding, if possible, existing artifacts.

    Throws error when applied to {!trigger_job}s.

    - If the job already has an artifact with [old_name] and [name] is given, then
      [name] is used.
    - If the job already has an artifact exposed as [old_exposed_as] and
      [exposed_as] is given, then [exposed_as] is used.
    - If the job already has an artifact added [old_when] and
      [when_] is given, the new artifact will be added:
        - at [old_when] if [old_when = when_]
        - at [Always] if [old_when <> when_]
    - If the job already has an artifact that expires in [old_expires_in] and
      [expires_in] is given, then the largest of the two durations is used. Note
      that for the purpose of this comparison, we consider that a minute is 60
      seconds, that an hour is 60 minutes, that a week is 7 days, that a
      month is 31 days, and that a year is 365 days.
    - Individual fields of a {!report} cannot be combined: a run-time error is
      raised if [reports] contains a report that is already set in the job.
    - [paths] are appended to the set of paths in [job]. *)
val add_artifacts :
  ?name:string ->
  ?expose_as:string ->
  ?reports:Gitlab_ci.Types.reports ->
  ?expire_in:Gitlab_ci.Types.expiration ->
  ?when_:Gitlab_ci.Types.when_artifact ->
  string list ->
  tezos_job ->
  tezos_job

(** Append the variables [variables] to the variables of [job].

    Throws error when applied to {!trigger_job}s.

    Raises [Failure] if any of the [variables] is already defined for
    [job], unless [allow_overwrite] is true (default is [false]). *)
val append_variables :
  ?allow_overwrite:bool -> Gitlab_ci.Types.variables -> tezos_job -> tezos_job

(** Append to the [script:] section of a job.

    Throws error when applied to {!trigger_job}s. *)
val append_script : string list -> tezos_job -> tezos_job

(** Append to the [cache:] section of a job.

    Throws error when applied to {!trigger_job}s. *)
val append_cache : Gitlab_ci.Types.cache -> tezos_job -> tezos_job

(** Append to the [before_script:] section of a job.

    Throws error when applied to {!trigger_job}s. *)
val append_before_script : string list -> tezos_job -> tezos_job

(** Append to the [after_script:] section of a job.

    Throws error when applied to {!trigger_job}s. *)
val append_after_script : string list -> tezos_job -> tezos_job

(** Override the [interruptible:] flag of a job.

    Has no effect on {!trigger_job}s. *)
val with_interruptible : bool -> tezos_job -> tezos_job

(** A [script:] that executes a given command and propagates its exit code.

    This might seem like a noop but is in fact necessary to please his majesty GitLab.

    For more info, see:
     - https://gitlab.com/tezos/tezos/-/merge_requests/9923#note_1538894754;
     - https://gitlab.com/tezos/tezos/-/merge_requests/12141; and
     - https://gitlab.com/groups/gitlab-org/-/epics/6074

   TODO: replace this with [FF_USE_NEW_BASH_EVAL_STRATEGY=true], see
   {{:https://docs.gitlab.com/runner/configuration/feature-flags.html}GitLab
   Runner feature flags}. *)
val script_propagate_exit_code : string -> string list

val job_docker_authenticated :
  ?skip_docker_initialization:bool ->
  ?ci_docker_hub:bool ->
  ?artifacts:Gitlab_ci.Types.artifacts ->
  ?variables:(string * string) list ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?dependencies:dependencies ->
  ?image_dependencies:Image.t list ->
  ?arch:Runner.Arch.t ->
  ?storage:Runner.Storage.t ->
  ?tag:Runner.Tag.t ->
  ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
  ?parallel:Gitlab_ci.Types.parallel ->
  ?timeout:Gitlab_ci.Types.time_interval ->
  ?retry:Gitlab_ci.Types.retry ->
  ?description:string ->
  ?dev_infra:bool ->
  __POS__:string * int * int * int ->
  stage:Stage.t ->
  name:string ->
  string list ->
  tezos_job

module Stages : sig
  val start : Stage.t

  val images : Stage.t

  val sanity : Stage.t

  val build : Stage.t

  val test : Stage.t

  val packaging : Stage.t

  val publish : Stage.t

  val publishing_tests : Stage.t

  val manual : Stage.t
end

module Images_external : sig
  val nix : Image.t

  val docker : Image.t

  val datadog_ci : Image.t

  val ci_release : Image.t

  val hadolint : Image.t

  val semgrep_agent : Image.t
end

module Images : sig
  val nix : Image.t

  val docker : Image.t

  val datadog_ci : Image.t

  val ci_release : Image.t

  val hadolint : Image.t

  val semgrep_agent : Image.t

  val macosx_15 : Image.t

  val client_libs_dependencies : Image.t

  val rust_toolchain : Image.t

  val rust_toolchain_master : Image.t

  val rust_sdk_bindings : Image.t

  val trivy : Image.t

  val jsonnet : Image.t

  val jsonnet_master : Image.t

  module CI : sig
    val job_docker_ci :
      Runner.Arch.t -> ?storage:Runner.Storage.t -> unit -> tezos_job

    val runtime : Image.t

    val monitoring : Image.t

    val prebuild : Image.t

    val prebuild_master : Image.t

    val build : Image.t

    val build_master : Image.t

    val test : Image.t

    val test_master : Image.t

    val e2etest : Image.t

    val release_page : Image.t
  end

  module Base_images : sig
    type version = {major : int; minor : int}

    val version_pp : Format.formatter -> version -> unit

    (** The prefix of the path of the base images. *)
    val path_prefix : string

    val debian_version : version

    val rpm_version : version

    val homebrew_version : version

    val rust_toolchain_version : version

    val debian_unstable : Image.t

    val debian_bookworm : Image.t

    val debian_trixie : Image.t

    val ubuntu_noble : Image.t

    val ubuntu_jammy : Image.t

    val ubuntu_plucky : Image.t

    val rockylinux_9_3 : Image.t

    val rockylinux_9_6 : Image.t

    val rockylinux_10_0 : Image.t

    val fedora_39 : Image.t

    val fedora_41 : Image.t

    val fedora_42 : Image.t

    val homebrew : Image.t

    val rust_toolchain_trixie : Image.t
  end
end

(* OIDC tokens to be generated by GitLab *)
val id_tokens : Gitlab_ci.Types.id_tokens

val job_datadog_pipeline_trace : tezos_job

(** Add common variables used by jobs compiling kernels *)
val enable_kernels : tezos_job -> tezos_job

(** Get the number of times the [job] function was called. *)
val get_number_of_declared_jobs : unit -> int
