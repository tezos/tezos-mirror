(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A GitLab CI job annotated with Octez-specific meta-data. *)
type tezos_job

(** A string that should be prepended to all generated files.

    Warns not to modify the generated files, and refers to the generator. *)
val header : string

(** A facility for registering pipeline stages. *)
module Stage : sig
  (* Represents a pipeline stage *)
  type t

  (** Register a stage.

      Fails if a stage of the same name has already been registered. *)
  val register : string -> t

  (** Name of a stage *)
  val name : t -> string

  (** Returns the list of registered stages, in order of registration, as a list of strings.

      This is appropriate to use with the [Stages] constructor of
      {!Gitlab_ci.Types.config_element} generating a [stages:]
      element. *)
  val to_string_list : unit -> string list
end

(** A facility for registering pipelines. *)
module Pipeline : sig
  (* Register a pipeline.

     [register ?variables name rule] will register a pipeline [name]
     that runs when [rule] is true.

     If [variables] is set, then these variables will be added to the
     [workflow:] clause for this pipeline in the top-level [.gitlab-ci.yml].

     If [jobs] is not set, then the pipeline is a legacy, hand-written
     .yml file, expected to be defined in
     [.gitlab/ci/pipelines/NAME.yml]. If [jobs] is set, then the those
     jobs will be generated to the same file when {!write} is
     called. In both cases, this file will be included from the
     top-level [.gitlab-ci.yml]. *)
  val register :
    ?variables:Gitlab_ci.Types.variables ->
    ?jobs:tezos_job list ->
    string ->
    Gitlab_ci.If.t ->
    unit

  (** Splits the set of registered pipelines into workflow rules and includes.

      The result of this function is used in the top-level
      [.gitlab-ci.yml] to filter pipelines (using [workflow:] rules)
      and to include the select pipeline (using [include:]). *)
  val workflow_includes :
    unit -> Gitlab_ci.Types.workflow * Gitlab_ci.Types.include_ list

  (** Writes the set of non-legacy registered pipelines.

      The string {!header} will be prepended to each written file. *)
  val write : unit -> unit
end

(** A facility for registering images for [image:] keywords.

    During the transition from hand-written [.gitlab-ci.yml] to
    CI-in-OCaml, we write a set of templates corresponding to the
    registered images, to make them available for hand-written jobs. *)
module Image : sig
  (** Represents an image *)
  type t = Gitlab_ci.Types.image

  (** Register an image of the given [name] and [image_path]. *)
  val register : name:string -> image_path:string -> t

  (** The name of an image *)
  val name : t -> string

  (** Returns the set of registered images as [name, image] tuples. *)
  val all : unit -> (string * t) list
end

(** Represents architectures. *)
type arch = Amd64 | Arm64

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
      (** Translates to [].

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
  ?parallel:int ->
  image:Image.t ->
  stage:Stage.t ->
  name:string ->
  string list ->
  tezos_job

(** Generates a job to an external file.

    This function is meant to be used in the transition to CI-in-OCaml.
    It writes {!header} and the given job to the destination path
    [.gitlab/ci/jobs/DIRECTORY/NAME(-FILENAME_SUFFIX).yml].
    Directory defaults to the stage name if not set.

    This allows migrating all the jobs of a given pipeline, and
    including the generated definition of those jobs in other
    pipelines where it appears.

    Raises [Failure] if [.gitlab/ci/jobs/DIRECTORY] is not an existing
    directory. Also [Failure] if destination path has already been
    used to write another job. *)
val job_external :
  ?directory:string -> ?filename_suffix:string -> tezos_job -> tezos_job
