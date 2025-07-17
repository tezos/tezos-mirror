(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Pipeline stages.

    The purpose of [Build] jobs is to produce artifacts for other jobs.
    They can depend on other build jobs.
    If there is no job that uses the artifacts of a build job,
    the build job should not be included.
    If you intend to include a build job as a test,
    consider having it in the [Test] stage instead,
    or add some actual tests that use the artifacts from the build job.

    The purpose of [Test] jobs is to make sure that pipelines fail if something looks wrong.
    They typically use artifacts from build jobs.
    Test jobs should only depend on build jobs that build the artifacts that they are testing.
    Test jobs usually do not depend on other test jobs,
    with the exception of jobs that aggregate debug artifacts.
    Test jobs typically produce debug artifacts such as reports and logs,
    but not artifacts that are used by other jobs,
    except jobs that aggregate debug artifacts.

    The purpose of [Publish] jobs is to publish artifacts from build jobs
    somewhere more convenient than GitLab artifacts.
    Publish jobs should only depend on build jobs that build the artifacts
    that they are publishing. They can also depend on test jobs to prevent
    publishing broken artifacts, and on other publish jobs if order matters.

    There is no need to include [Build] jobs explicitly in pipelines.
    One should only list [Test] and [Publish] jobs and let Cacio automatically
    include the relevant build jobs that they depend on.
    This reflects the fact that the purpose of build jobs is only to
    make artifacts for other jobs. *)
type stage = Build | Test | Publish

(** Dependency relationships.

    - [Job]: do not download artifacts of the dependency.
      A typical use case is to prevent [Publish] jobs from running
      before some [Test] jobs succeed, without losing time downloading test reports.

    - [Artifacts]: download artifacts of the dependency. *)
type need = Job | Artifacts

(** Pipeline jobs.

    Jobs are basically code to run in a given pipeline stage. *)
type job

(** How a job is triggered.

    - [Auto]: the job is triggered automatically once all of its dependencies succeed.
      This includes the [trigger] job for pipelines that have one.
    - [Manual]: the job can be triggered manually once all of its dependencies succeed.

    If a job [dep] is needed by a job with trigger [Auto],
    the trigger of [dep] is automatically forced to [Auto].

    If a job [dep] is added to a pipeline automatically
    because it is the dependency of other jobs,
    and if all those other jobs have trigger [Manual],
    [dep] is added with trigger [Manual].
    Otherwise it is added with trigger [Auto]. *)
type trigger = Auto | Manual

(** Memoize a function.

    The intended use case is to parameterize jobs without duplicating them by mistake.
    For instance:
    {[
      let my_job =
        parameterize @@ fun stage ->
        parameterize @@ fun image ->
        job ~stage ~image ...
    ]}

    [my_job] always returns the same job when given the same parameters.
    This means that you can apply [my_job] in multiple places with the same parameters
    without duplicating the job in the CI.

    If you instead defined [my_job] as a function directly, like this:
    {[
      let my_job stage image =
        job ~stage ~image ...
    ]}
    then the risk of duplicating jobs by mistake would be really high.
    For instance, if you wrote [~needs: [my_job Build fedora_37]]
    in two places to include this job as a dependency of two jobs in the same pipeline,
    this dependency would be included twice, since each application of [my_job]
    would return a different job (even if those different jobs had the same name).

    Note that [parameterize] does not automatically parameterize a job's name.
    If you intend to include multiple instances of [my_job] in the same pipeline,
    you should compute a name that differs according to the parameters.
    [parameterize] knows nothing about jobs.

    The parameter type ['a] must be hashable by [Hashtbl]. *)
val parameterize : ('a -> 'b) -> 'a -> 'b

module type COMPONENT = sig
  (** Description of a component (argument of the {!Make} functor). *)

  (** Component name.

      This is added as a prefix to jobs and pipelines of this component.
      It should thus be short.

      Release tags are prefixed by the lowercase version of [name].
      So names must be valid identifiers. *)
  val name : string

  (** Files that belong to the component.

      This is added as [changes] clauses for jobs of this component
      in the [before_merging] pipeline. *)
  val paths : string list
end

module type COMPONENT_API = sig
  (** Functions that components can use to define their CI (result of the {!Make} functor). *)

  (** Define a job.

      This function must only be called once per job. See {!parameterize}.

      Usage:
      {[
        job name
          ~__POS__
          ~stage: ...
          ~description: ...
          ~image: ...
          [
            (* script *)
          ]
      ]}

      This defines a job named [name] prefixed by the name of the component.
      The job can then be added to multiple pipelines using other functions
      from this module.

      Jobs themselves do not carry their [!trigger].
      Instead, each pipeline can decide to add jobs with a different trigger.

      When added to the [before_merging] pipeline,
      jobs are given a [changes] clause which is the union of:
      - the component [paths];
      - the [changes] clauses of reverse dependencies.

      The job will not start before all [needs] and [needs_legacy] jobs succeed.
      Additionally, [needs] are automatically added to pipelines in which the job
      is present if they are not already present.
      [needs_legacy] allows to make an incremental transition to Cacio,
      but [needs_legacy] jobs are not automatically added to pipelines,
      contrary to [needs] jobs.

      See {!Tezos_ci.job} for information about other arguments. *)
  val job :
    __POS__:string * int * int * int ->
    stage:stage ->
    description:string ->
    image:Tezos_ci.Image.t ->
    ?needs:(need * job) list ->
    ?needs_legacy:(need * Tezos_ci.tezos_job) list ->
    ?variables:Gitlab_ci.Types.variables ->
    ?artifacts:Gitlab_ci.Types.artifacts ->
    string ->
    string list ->
    job

  (** Register jobs to be included in [before_merging] and [merge_train] pipelines. *)
  val register_before_merging_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in [master_branch] pipelines. *)
  val register_master_jobs : (trigger * job) list -> unit

  (** Register a scheduled pipeline for this component.

      Usage: [register_scheduled_pipeline ~description name jobs]

      [name] can typically be ["daily"] or ["weekly"].
      The actual name of the pipeline will automatically be prefixed
      by the name of the component.

      This does not actually register the pipeline in GitLab.
      The pipeline must manually be set up in GitLab
      to run with variable [TZ_SCHEDULE_KIND] equal to the name of the pipeline
      (including the prefix, which is the component's name). *)
  val register_scheduled_pipeline :
    description:string -> string -> (trigger * job) list -> unit

  (** {2 Releases} *)

  (** This section contains functions to define jobs for release pipelines.
      In the future, these functions may be replaced by something more abstract.
      (See the future work section at the end of the file.) *)

  (** Register jobs to be included in the global release pipeline.

      This pipeline is for major releases and includes all components.
      It runs in [tezos/tezos]. *)
  val register_global_release_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in the global test release pipeline.

      This pipeline is supposed to be very close to the global release pipeline.
      It is not run in [tezos/tezos] but in forks. *)
  val register_global_test_release_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in the global scheduled test release pipeline.

      This pipeline is supposed to be close to the global release pipeline,
      except that it should not actually create the release.
      It runs in [tezos/tezos]. *)
  val register_global_scheduled_test_release_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in the release pipeline of the current component.

      This pipeline is for releasing this component separately.
      It runs in [tezos/tezos].

      This function must be called only once per component. *)
  val register_dedicated_release_pipeline : (trigger * job) list -> unit

  (** Register jobs to be included in the test release pipeline of the current component.

      This pipeline is for testing the release of this component separately.
      It runs in [tezos/tezos].

      This function must be called only once per component. *)
  val register_dedicated_test_release_pipeline : (trigger * job) list -> unit
end

(** The main functor of Cacio. *)
module Make (_ : COMPONENT) : COMPONENT_API

(** {2 Future work} *)

(** One idea would be to have Cacio provide a dedicated function to create Tezt jobs.
    This would automatically cause the relevant pipelines with those jobs
    to have a [select_tezts] job.
    It could assume that the [main.ml] is located in [component/tezt]
    (although this requires to change [COMPONENT] to be able to identify the main path). *)

(** Ideally, all components would only have a single path (their toplevel directory).
    For now, we choose to allow multiple paths, so that:
    - one can migrate old components more easily;
    - one can continue to run tests of a component if one of its dependencies is modified
      (by including the paths of these dependencies in the list of paths for
      this component). *)

(** Instead of having one [register_*_release_jobs] function per release pipeline,
    it would be better if components only declared WHAT they want to release, not HOW.
    Components could give a list of build jobs, and for each of them they could specify
    which artifacts are meant to be published, as well as which Docker images.
    It should be possible to generate everything else just from this information.
    This abstraction could guarantee important invariants like:
    - a test release must not create a GitLab release on [tezos/tezos];
    - there is only one job that creates a GitLab release in each pipeline
      (unless we actually want one per component as well, in which case
      the invariant becomes: there is exactly one job per component, plus a global one);
    - our release page is only updated by actual release pipelines
      (other pipelines can update the test release page instead);
    - Docker images are only pushed to Docker Hub in actual release pipelines,
      not test release pipelines (those can use the GitLab registry instead for instance);
    - ... *)
