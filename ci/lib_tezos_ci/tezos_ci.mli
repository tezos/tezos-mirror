(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A GitLab CI job annotated with Octez-specific meta-data. *)
type tezos_job

module Rules : module type of Rules

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
  (** Register a pipeline.

      [register ~description ?variables name rule] will register a pipeline [name]
      that runs when [rule] is true.

      If [variables] is set, then these variables will be added to the
      [workflow:] clause for this pipeline in the top-level [.gitlab-ci.yml].
      Similarly, an [auto_cancel] clause can be specified.

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
      {!register} for info on [auto_cancel].

      The [description] is printed in {!list_pipelines}.

      Child pipelines cannot be launched without a trigger job that is
      included in a regular pipeline (see {!trigger_job}). *)
  val register_child :
    ?auto_cancel:Gitlab_ci.Types.auto_cancel ->
    ?inherit_:Gitlab_ci.Types.inherit_ ->
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

(** Add variable enabling sccache.

    This function should be applied to jobs that build rust files and
    which has a configured sccache Gitlab CI cache.

    - [key] and [path] configure the key under which the cache is
    stored, and the path that will be cached. By default, the [key]
    contains the name of the job, thus scoping the cache to all
    instances of that job. By default, [path] is the folder
    ["$CI_PROJECT_DIR/_sccache"], and this function also sets the
    environment dir [SCCACHE_DIR] such that sccache stores its caches
    there.

    - [cache_size] sets the environment variable [SCCACHE_CACHE_SIZE]
    that configures the maximum size of the cache.

    - [error_log], [idle_timeout] and [log] sets the environment
    variables [SCCACHE_ERROR_LOG], [SCCACHE_IDLE_TIMEOUT] and
    [SCCACHE_LOG] respectively. See the sccache documentation for more
    information on these variables. *)
val enable_sccache :
  ?key:string ->
  ?error_log:string ->
  ?idle_timeout:string ->
  ?log:string ->
  ?path:string ->
  ?cache_size:string ->
  tezos_job ->
  tezos_job

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

(** Represents architectures. *)
type arch = Amd64 | Arm64

(** String representation of architectures ([Amd64] is ["x86_64"]) *)
val arch_to_string : arch -> string

(** Alternative string representation of architectures ([Amd64] is ["amd64"]) *)
val arch_to_string_alt : arch -> string

(** The list of available runner tags. *)
type tag =
  | Gcp  (** GCP prod AMD64 runner, general purpose. *)
  | Gcp_arm64  (** GCP prod ARM64 runner, general purpose. *)
  | Gcp_dev  (** GCP dev AMD64 runner, general purpose. *)
  | Gcp_dev_arm64  (** GCP dev ARM64 runner, general purpose. *)
  | Gcp_tezt
      (** GCP prod AMD64 runner, suitable for tezt jobs (more RAM and CPU) *)
  | Gcp_tezt_dev
      (** GCP dev AMD64 runner, suitable for tezt jobs (more RAM and CPU) *)
  | Gcp_high_cpu
      (** GCP prod AMD64 runner, suitable for jobs needing high CPU. *)
  | Gcp_high_cpu_dev
      (** GCP dev AMD64 runner, suitable for jobs needing high CPU. *)
  | Gcp_very_high_cpu
      (** GCP prod AMD64 runner, suitable for jobs needing very high CPU. *)
  | Gcp_very_high_cpu_dev
      (** GCP dev AMD64 runner, suitable for jobs needing very high CPU. *)
  | Aws_specific
      (** AWS runners, in cases where a CI is legacy or not suitable for GCP. *)
  | Dynamic
      (** The runner is dynamically set through the CI variable {!dynamic_tag_var}. *)

(** The variable to set enabling dynamic runner selection.

    To dynamically set the runner of a job through a CI/CD variable,
    assign to this variable using [variables:] or [parallel:matrix:]. *)
val dynamic_tag_var : Gitlab_ci.Var.t

(** The architecture of the runner associated to a tag if statically known. *)
val arch_of_tag : tag -> arch option

(** The string representation of a tag. *)
val string_of_tag : tag -> string

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

type cpu =
  | Normal  (** Target default Gitlab runner pool. *)
  | High  (** Target GCP high runner pool. *)
  | Very_high  (** Target GCP very high runner pool. *)

(** Define a job.

    This smart constructor for {!Gitlab_ci.Types.job} additionally:

    - Translates each {!dependency} to [needs:] and [dependencies:]
    keywords as detailed in the documentation of {!dependency}.
    - Adds [tag:] based on [arch] and [tag]:

      - If only [tag] is set, then it is passed as is to the job's [tags:]
        field. The runners of the tezos/tezos CI all use singleton tags,
        hence we only allow one tag per job.
      - Setting both [arch] and [tag] throws an error.
      - Omitting both [arch] and [tag] is equivalent to setting
        [~tag:Gcp] or, equivalently, omitting tag and setting
        [~arch:Amd64].

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

    - The [cpu] parameter specifies the CPU allocation for the job,
      allowing it to run on a GCP GitLab runner with normal, high,
      or very high CPU capacity.

    - The [dev_infra] parameter allows to run the job on the dev infrastructure.
      This parameter is used for tests only and should not be merged in
      production.*)

val job :
  ?arch:arch ->
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
  ?tag:tag ->
  ?cpu:cpu ->
  ?git_strategy:git_strategy ->
  ?coverage:string ->
  ?retry:Gitlab_ci.Types.retry ->
  ?parallel:Gitlab_ci.Types.parallel ->
  ?description:string ->
  ?dev_infra:bool ->
  __POS__:string * int * int * int ->
  ?image:Image.t ->
  ?template:Gitlab_ci.Types.template ->
  stage:Stage.t ->
  name:string ->
  string list ->
  tezos_job

(** Define a trigger job for a child pipeline.

    The trigger job will be named [trigger:CHILD_PIPELINE_NAME]. *)
val trigger_job :
  ?dependencies:dependencies ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?description:string ->
  __POS__:string * int * int * int ->
  stage:Stage.t ->
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

val job_docker_authenticated :
  ?skip_docker_initialization:bool ->
  ?ci_docker_hub:bool ->
  ?artifacts:Gitlab_ci.Types.artifacts ->
  ?variables:(string * string) list ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?dependencies:dependencies ->
  ?image_dependencies:Image.t list ->
  ?arch:arch ->
  ?tag:tag ->
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

  val test_coverage : Stage.t

  val packaging : Stage.t

  val publishing : Stage.t

  val publishing_tests : Stage.t

  val doc : Stage.t

  (* Scanning vulnerabilities of Docker images. *)
  val scan : Stage.t

  val prepare_release : Stage.t

  val publish_release_gitlab : Stage.t

  val publish_release : Stage.t

  val publish_package_gitlab : Stage.t

  val manual : Stage.t
end

module Images_external : sig
  val nix : Image.t

  val docker : Image.t

  val datadog_ci : Image.t

  val debian_bookworm : Image.t

  val ubuntu_noble : Image.t

  val ubuntu_jammy : Image.t

  val ubuntu_oracular : Image.t

  val fedora_37 : Image.t

  val fedora_39 : Image.t

  val rockylinux_93 : Image.t

  val opam_ubuntu_oracular : Image.t

  val opam_ubuntu_noble : Image.t

  val opam_debian_bookworm : Image.t

  val ci_release : Image.t

  val hadolint : Image.t

  val semgrep_agent : Image.t
end

module Images : sig
  val nix : Image.t

  val docker : Image.t

  val datadog_ci : Image.t

  val debian_bookworm : Image.t

  val ubuntu_noble : Image.t

  val ubuntu_jammy : Image.t

  val ubuntu_oracular : Image.t

  val fedora_37 : Image.t

  val fedora_39 : Image.t

  val rockylinux_93 : Image.t

  val opam_ubuntu_oracular : Image.t

  val opam_ubuntu_noble : Image.t

  val opam_debian_bookworm : Image.t

  val ci_release : Image.t

  val hadolint : Image.t

  val semgrep_agent : Image.t

  val macosx_14 : Image.t

  val stage : Stage.t

  val client_libs_dependencies : Image.t

  val rust_toolchain : Image.t

  val rust_sdk_bindings : Image.t

  val jsonnet : Image.t

  module CI : sig
    val job_docker_ci : arch -> tezos_job

    val mk_ci_image : image_path:string -> Image.t

    val runtime : Image.t

    val prebuild : Image.t

    val build : Image.t

    val test : Image.t

    val e2etest : Image.t
  end
end

(* OIDC tokens to be generated by GitLab *)
val id_tokens : Gitlab_ci.Types.id_tokens
