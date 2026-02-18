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

(** Configuration of sccache. *)
type sccache_config

(** Make an sccache configuration.

    See {!Tezos_ci.Cache.enable_sccache}. *)
val sccache :
  ?error_log:string ->
  ?log:string ->
  ?policy:Gitlab_ci.Types.cache_policy ->
  unit ->
  sccache_config

(** Pipeline jobs.

    Jobs are basically code to run in a given pipeline stage. *)
type job

(** How a job is triggered.

    - [Auto]: the job is triggered automatically once all of its dependencies succeed.
      This includes the [trigger] job for pipelines that have one.
    - [Immediate]: same as [Auto], but do not wait for the [trigger] job.
    - [Manual]: the job can be triggered manually once all of its dependencies succeed.

    If a job [dep] with trigger [Manual]
    is needed by a job with trigger [Auto],
    the trigger of [dep] is automatically forced to [Auto].

    If a job [dep] with trigger [Manual] or [Auto]
    is needed by a job with trigger [Immediate],
    the trigger of [dep] is automatically forced to [Immediate].

    If a job [dep] is added to a pipeline automatically
    because it is the dependency of other jobs,
    and if all those other jobs have trigger [Manual],
    [dep] is added with trigger [Manual].
    Otherwise it is added with trigger [Auto]. *)
type trigger = Auto | Immediate | Manual

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

      When added to the [before_merging] or [merge_train] pipeline,
      jobs are only included to the pipeline if
      any file listed in [only_if_changed] has been modified,
      or if the merge request has any label listed in [force_if_label],
      or if [force] is set to [true].
      [only_if_changed] can include glob patterns such as [dir/**/*.ml].
      Its default value is the [paths] of the current component.

      The job will not start before all [needs] and [needs_legacy] jobs succeed.
      Additionally, [needs] are automatically added to pipelines in which the job
      is present if they are not already present.
      [needs_legacy] allows to make an incremental transition to Cacio,
      but [needs_legacy] jobs are not automatically added to pipelines,
      contrary to [needs] jobs.

      If [cargo_cache] is [true], the resulting job is modified with
      {!Tezos_ci.Cache.enable_cargo_cache}. Default is [false].

      If [sccache] is specified, the resulting job is modified with
      {!Tezos_ci.Cache.enable_sccache}.

      If [dune_cache] is [true], the resulting job is modified with
      {!Tezos_ci.Cache.enable_dune_cache}. Default is [false].

      [?image_dependencies] is temporary,
      it will be removed once the image feature is implemented in Cacio.
      See the Future work section for more details.

      See {!Tezos_ci.job} for information about other arguments.

      The default number of retries is 0 for [Publish] jobs.
      Other jobs use the value defined in [.gitlab-ci.yml].
      You can force a specific value with [~retry]. *)
  val job :
    __POS__:string * int * int * int ->
    stage:stage ->
    description:string ->
    ?provider:Tezos_ci.Runner.Provider.t ->
    ?arch:Tezos_ci.Runner.Arch.t ->
    ?cpu:Tezos_ci.Runner.CPU.t ->
    ?storage:Tezos_ci.Runner.Storage.t ->
    image:Tezos_ci.Image.t ->
    ?only_if_changed:string list ->
    ?force:bool ->
    ?force_if_label:string list ->
    ?needs:(need * job) list ->
    ?needs_legacy:(need * Tezos_ci.tezos_job) list ->
    ?parallel:Gitlab_ci.Types.parallel ->
    ?variables:Gitlab_ci.Types.variables ->
    ?artifacts:Gitlab_ci.Types.artifacts ->
    ?cache:Gitlab_ci.Types.cache list ->
    ?cargo_cache:bool ->
    ?sccache:sccache_config ->
    ?dune_cache:bool ->
    ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
    ?retry:Gitlab_ci.Types.retry ->
    ?timeout:Gitlab_ci.Types.time_interval ->
    ?image_dependencies:Tezos_ci.Image.t list ->
    ?services:Gitlab_ci.Types.service list ->
    string ->
    string list ->
    job

  (** Time interval for timeouts passed to {!tezt_job}. *)
  type tezt_timeout = No_timeout | Minutes of int

  (** Define a job that runs Tezt tests.

      Just like {!job}, this function must only be called once per job. See {!parameterize}.

      The main [string] argument is the name of the job.
      It will be prefixed by the name of the component.

      General parameters:
      - [tezt_exe] is the path to the Tezt executable, relative to [_build/default/].
        It defaults to [tezt/tests/main.exe]. This executable is assumed to have been
        built by another job, which you must specify as a dependency with an [Artifacts]
        [~needs] or [~needs_legacy].
      - [before_script] is prepended to the job's script.

      Timeouts:
      - [global_timeout] is Tezt's [--global-timeout].
        It defaults to 30 minutes.
        Do not set it to [No_timeout] unless you have a VERY good reason.
      - If [global_timeout] is specified,
        the main Tezt call is wrapped under the [timeout] command,
        causing [SIGTERM] to be sent to Tezt after [global_timeout] plus 1 minute,
        and [SIGKILL] to be sent 1 more minute after that.
      - If [global_timeout] is specified,
        the CI job itself also gets a timeout of [global_timeout] plus 10 minutes.
      - [test_timeout] is Tezt's [--test-timeout].
        It defaults to 9 minutes.

      Parallel execution:
      - [parallel_jobs] is GitLab's [parallel:] field.
        It is the number of CI jobs that are spawned to run in parallel.
        It defaults to 1.
      - [parallel_tests] is Tezt's [--job-count].
        It is the maximum number of tests that Tezt shall run in parallel in a given CI job.
        It defaults to 6.

      Error handling:
      - [retry_jobs] is GitLab's [retry:] field.
        It allows GitLab to re-launch jobs that failed.
        By default, there is no retry.
      - [retry_tests] is Tezt's [--retry].
        It allows Tezt to re-run tests that failed.
        By default, there is no retry.
      - Tezt's [--keep-going] is set if, and only if [pipeline] is [`scheduled].

      Test selection:
      - Manifezt is active if, and only if [pipeline] is [`merge_request].
        When active, it causes the job to depend on the [select_tezts] job,
        which is automatically added to the pipeline if necessary.
      - [test_selection] is a TSL expression.
        TSL is the Test Selection Language fo Tezt.
        It allows to select a subset of tests to run.
        It defaults to [True], i.e. "run all tests".

      For other arguments, see the documentation of the [job] function above. *)
  val tezt_job :
    __POS__:string * int * int * int ->
    pipeline:[`merge_request | `scheduled] ->
    description:string ->
    ?provider:Tezos_ci.Runner.Provider.t ->
    ?arch:Tezos_ci.Runner.Arch.t ->
    ?cpu:Tezos_ci.Runner.CPU.t ->
    ?storage:Tezos_ci.Runner.Storage.t ->
    ?only_if_changed:string list ->
    ?needs:(need * job) list ->
    ?needs_legacy:(need * Tezos_ci.tezos_job) list ->
    ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
    ?tezt_exe:string ->
    ?global_timeout:tezt_timeout ->
    ?test_timeout:tezt_timeout ->
    ?parallel_jobs:int ->
    ?parallel_tests:int ->
    ?retry_jobs:int ->
    ?retry_tests:int ->
    ?test_selection:Tezt_core.TSL_AST.t ->
    ?before_script:string list ->
    string ->
    job

  (** Register jobs to be included in [before_merging]. *)
  val register_before_merging_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in [merge_train] pipelines. *)
  val register_merge_train_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in [before_merging] and [merge_train] pipelines.

      This is equivalent to registering the job with both
      [register_before_merging_jobs] and [register_merge_train_jobs]. *)
  val register_merge_request_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in [schedule_extended_test] pipelines.

      Only available in the [Shared] component. *)
  val register_schedule_extended_test_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in [custom_extended_test] pipelines.

      Only available in the [Shared] component. *)
  val register_custom_extended_test_jobs : (trigger * job) list -> unit

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
      (including the prefix, which is the component's name).

      Use [legacy_jobs] to include jobs that were not migrated to Cacio yet.
      Cacio does not add them automatically for you even if they are dependencies
      of Cacio jobs. *)
  val register_scheduled_pipeline :
    description:string ->
    ?legacy_jobs:Tezos_ci.tezos_job list ->
    string ->
    (trigger * job) list ->
    unit

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

  (** Register jobs to be included in the publish release pages pipeline.

      This pipeline is for publishing the updated release pages. *)
  val register_global_publish_release_page_jobs : (trigger * job) list -> unit

  (** Register jobs to be included in the publish release pages test pipeline.

      This pipeline is for publishing the updated release pages on the test web page. *)
  val register_global_test_publish_release_page_jobs :
    (trigger * job) list -> unit

  (** Register jobs to be included in the release pipeline of the current component.

      This pipeline is for releasing this component separately.
      It runs in [tezos/tezos].

      This function must be called only once per component.

      [tag_rex] allows to specify a custom tag regular expression.
      It is registered to be returned by {!get_release_tag_rexes}.

      Not implemented for the [Shared] component.

      Use [legacy_jobs] to include jobs that were not migrated to Cacio yet.
      Cacio does not add them automatically for you even if they are dependencies
      of Cacio jobs. *)
  val register_dedicated_release_pipeline :
    ?tag_rex:string ->
    ?legacy_jobs:Tezos_ci.tezos_job list ->
    (trigger * job) list ->
    unit

  (** Register jobs to be included in the test release pipeline of the current component.

      This pipeline is for testing the release of this component separately.
      It runs in [tezos/tezos].

      This function must be called only once per component.

      [tag_rex] allows to specify a custom tag regular expression.
      It is registered to be returned by {!get_release_tag_rexes}.

      Not implemented for the [Shared] component. *)
  val register_dedicated_test_release_pipeline :
    ?tag_rex:string -> (trigger * job) list -> unit
end

(** The main functor of Cacio. *)
module Make (_ : COMPONENT) : COMPONENT_API

(** An instance of [Make] for jobs with no component.

    Only use this if you have a good reason, as jobs should usually belong to components.
    Add a comment next to your job definitions to justify this reason. *)
module Shared : COMPONENT_API

(** {2 Listing registered jobs and more} *)

(** Only call these functions after all jobs are registered.
    They are meant to be called from [ci/bin/main.ml]
    when registering the pipelines with CIAO. *)

(** Jobs for [before_merging]. *)
val get_before_merging_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for [merge_train]. *)
val get_merge_train_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for [schedule_extended_test]. *)
val get_schedule_extended_test_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for [custom_extended_test]. *)
val get_custom_extended_test_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for [master]. *)
val get_master_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for the global release pipeline. *)
val get_global_release_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for the global test release pipeline. *)
val get_global_test_release_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for the global scheduled test release pipeline. *)
val get_global_scheduled_test_release_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for the publish release pages pipeline. *)
val get_global_publish_release_page_jobs : unit -> Tezos_ci.tezos_job list

(** Jobs for the publish release pages test pipeline. *)
val get_global_test_publish_release_page_jobs : unit -> Tezos_ci.tezos_job list

(** Regular expressions that match release tags.

    To be used in [ci/bin/main.ml] to define the [non_release_tag]
    and [non_release_tag_test] pipelines. *)
val get_release_tag_rexes : unit -> string list

(** Get the number of times the [job] function was called. *)
val get_number_of_declared_jobs : unit -> int

(** {2 Other} *)

(** Output the list of Tezt jobs, as well as some of their parameters, to a file.

    The file can be read by [tezt/stats]. *)
val output_tezt_job_list : string -> unit

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

(** Another idea would be to have the default ~force_if_label be ["ci--" ^ component_name].
    Or to automatically add this label to the list. *)

(** The function [job] currently takes [?image_dependencies] as an optional argument.
    This is temporary and should be replaced by ~needs:[job_to_build_image]
    once all the images have been migrated to Cacio.
    In that case, Cacio would add the [job_to_build_image] in the pipeline, as a dependency. *)
