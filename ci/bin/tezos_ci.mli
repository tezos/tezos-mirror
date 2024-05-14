(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A GitLab CI job annotated with Octez-specific meta-data. *)
type tezos_job

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
  (** Type of the command-line configuration for the generator binary. *)
  type config = {
    mutable verbose : bool;
        (** Enable [verbose] output, including the source of generated jobs. *)
    mutable inline_source_info : bool;
        (** Enable the emission of source information in generated configuration. *)
    mutable remove_extra_files : bool;
        (** Remove files that are neither generated nor excluded. *)
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

      [register ?variables name rule] will register a pipeline [name]
      that runs when [rule] is true.

      If [variables] is set, then these variables will be added to the
      [workflow:] clause for this pipeline in the top-level [.gitlab-ci.yml].

      The [jobs] of the pipeline are generated to the file
      [.gitlab/ci/pipelines/NAME.yml] when {!write} is called. *)
  val register :
    ?variables:Gitlab_ci.Types.variables ->
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
      [.gitlab/ci/pipelines/NAME.yml] when {!write} is called.

      Child pipelines cannot be launched without a trigger job that is
      included in a regular pipeline (see {!trigger_job}). *)
  val register_child : jobs:tezos_job list -> string -> child_pipeline

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
end

(** A facility for registering images for [image:] keywords. *)
module Image : sig
  (** Represents an image *)
  type t = Gitlab_ci.Types.image

  (** Register an image of the given [name] and [image_path]. *)
  val register : name:string -> image_path:string -> t

  (** The [name] of an image *)
  val name : t -> string

  (** Returns the set of registered images as [name, image_path] tuples. *)
  val all : unit -> (string * t) list
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

(** Job dependencies.

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

    This smart constructor for {!Gitlab_ci.Types.job} additionally:

    - Translates each {!dependency} to [needs:] and [dependencies:]
    keywords as detailed in the documentation of {!dependency}.
    - Adds [tags:] based on [arch] and [tags]:

      - If only [arch] is set to [Amd64] (resp. [Arm64]) then the tag
        ["gcp"] (resp ["gcp_arm64"]) is set.
      - If only [tags] is set, then it is passed as is to the job's [tags:]
        field.
      - Setting both [arch] and [tags] throws an error.
      - Omitting both [arch] and [tags] is equivalent to setting
        [~arch:Amd64] and omitting [tags].

    - Throws a run-time error if both [rules] and [when_] are passed. A
     [when_] field can always be represented by [rules] instead, so use
     the latter for more complex conditions. *)
val job :
  ?arch:arch ->
  ?after_script:string list ->
  ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
  ?artifacts:Gitlab_ci.Types.artifacts ->
  ?before_script:string list ->
  ?cache:Gitlab_ci.Types.cache list ->
  ?interruptible:bool ->
  ?dependencies:dependencies ->
  ?services:Gitlab_ci.Types.service list ->
  ?variables:Gitlab_ci.Types.variables ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  ?timeout:Gitlab_ci.Types.time_interval ->
  ?tags:string list ->
  ?git_strategy:git_strategy ->
  ?when_:Gitlab_ci.Types.when_job ->
  ?coverage:string ->
  ?retry:int ->
  ?parallel:Gitlab_ci.Types.parallel ->
  __POS__:string * int * int * int ->
  image:Image.t ->
  stage:Stage.t ->
  name:string ->
  string list ->
  tezos_job

(** Define a trigger job for a child pipeline.

    The trigger job will be named [trigger:CHILD_PIPELINE_NAME]. *)
val trigger_job :
  ?dependencies:dependencies ->
  ?rules:Gitlab_ci.Types.job_rule list ->
  __POS__:string * int * int * int ->
  stage:Stage.t ->
  Pipeline.child_pipeline ->
  tezos_job

(** Adds artifacts to a job without overriding, if possible, existing artifacts.

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

    Raises [Failure] if any of the [variables] is already defined for
    [job], unless [allow_overwrite] is true (default is [false]). *)
val append_variables :
  ?allow_overwrite:bool -> Gitlab_ci.Types.variables -> tezos_job -> tezos_job

(** Append to the [script:] section of a job. *)
val append_script : string list -> tezos_job -> tezos_job
