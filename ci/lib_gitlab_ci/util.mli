(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Smart constructors for values of {!Types} *)

open Types

(** Constructs a [default:] configuration element. *)
val default : ?image:image -> ?interruptible:bool -> unit -> default

(** Constructs a job rule.

    [when_] defaults to [On_success].

    If [when_] is set to [Manual] then the default [allow_failure]
    will be overwritten to allow failure, to avoid blocking jobs.
    This typically makes sense unless you truly want a job to block
    the execution of all later jobs (i.e. for a trigger-type job).  If
    you want to make such a job, set [~allow_failure] explicitly:
    [job_rule ~when_:Manual ~allow_failure:false ()]. *)
val job_rule :
  ?changes:string list ->
  ?if_:If.t ->
  ?variables:variables ->
  ?when_:when_ ->
  ?allow_failure:allow_failure_rule ->
  unit ->
  job_rule

(** Constructs a workflow rule.

    [when_] defaults to [Always]. *)
val workflow_rule :
  ?changes:string list ->
  ?if_:If.t ->
  ?variables:variables ->
  ?when_:when_workflow ->
  unit ->
  workflow_rule

(** Constructs an include rule.

    Include rules do not permit [variables] and there is consequently
    no such parameter.

    [when_] defaults to [Always]. *)
val include_rule :
  ?changes:string list ->
  ?if_:If.t ->
  ?when_:when_workflow ->
  unit ->
  include_rule

(** Constructs a job. *)
val job :
  ?after_script:string list ->
  ?allow_failure:allow_failure_job ->
  ?artifacts:artifacts ->
  ?before_script:string list ->
  ?cache:cache list ->
  ?image:image ->
  ?interruptible:bool ->
  ?needs:need list ->
  ?dependencies:string list ->
  ?rules:job_rule list ->
  ?services:service list ->
  ?stage:string ->
  ?variables:variables ->
  ?timeout:time_interval ->
  ?tags:string list ->
  ?when_:when_job ->
  ?coverage:string ->
  ?retry:int ->
  ?parallel:int ->
  name:string ->
  script:string list ->
  unit ->
  job

(** [artifacts paths] Construct an [artifacts:] clause storing [paths].

    - [expire_in:] is omitted if [expire_in] is [None].
    - [reports:] is omitted if [reports] is [None].
    - [when:] is omitted if [when_] is [None].
    - [expose_as:] is omitted if [expose_as] is [None].

    At least one of [paths] or [reports] must be non-empty. *)
val artifacts :
  ?expire_in:time_interval ->
  ?reports:reports ->
  ?when_:when_artifact ->
  ?expose_as:string ->
  ?name:string ->
  string list ->
  artifacts

(** Construct an [reports:] clause for [artifacts:].

    - [dotenv:] is omitted if [dotenv] is [None].
    - [junit:] is omitted if [junit] is [None].
    - [coverage_report:] is omitted if [coverage_report] is [None]. *)
val reports :
  ?dotenv:string ->
  ?junit:string ->
  ?coverage_report:coverage_report ->
  unit ->
  reports
