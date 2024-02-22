(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The AST of GitLab CI configurations.

    For reference, see GitLab's {{:https://docs.gitlab.com/ee/ci/yaml/} CI/CD YAML syntax reference}. *)

type variables = (string * string) list

type time_interval =
  | Seconds of int
  | Minutes of int
  | Hours of int
  | Days of int
  | Weeks of int
  | Months of int
  | Years of int

(** Represents values of the [when:] field in job rules. *)
type when_ = Always | Never | On_success | Manual | Delayed of time_interval

(** Represents values of the [when:] field of jobs. *)
type when_job = Always | On_success | Manual

(** Represents values of the [when:] field in [workflow:] and [include:] rules. *)
type when_workflow = Always | Never

(** Represents values of the [job:allow_failure:] field of rules. *)
type allow_failure_job = Yes | No | With_exit_codes of int list

(** Represents values of the [rules:allow_failure:] field of rules.

    GitLab does not enable allowing a set of exit codes in rules,
    as is possible in a job's [allow_failure] field. *)
type allow_failure_rule = Yes | No

(** Represents a job rule. *)
type job_rule = {
  changes : string list option;
  if_ : If.t option;
  variables : variables option;
  when_ : when_;
  allow_failure : allow_failure_rule option;
}

(** Represents a workflow rule. *)
type workflow_rule = {
  changes : string list option;
  if_ : If.t option;
  variables : variables option;
  when_ : when_workflow;
}

(** Represents an include rule. *)
type include_rule = {
  changes : string list option;
  if_ : If.t option;
  when_ : when_workflow;
}

type coverage_format = Cobertura

type coverage_report = {coverage_format : coverage_format; path : string}

type reports = {
  dotenv : string option;
  junit : string option;
  coverage_report : coverage_report option;
}

type image = Image of string

type when_artifact = Always | On_success | On_failure

type artifacts = {
  expire_in : time_interval option;
  paths : string list option;
  reports : reports option;
  when_ : when_artifact option;
  expose_as : string option;
  name : string option;
}

type default = {image : image option; interruptible : bool option}

type cache = {key : string; paths : string list}

type service = {name : string}

type need = {job : string; optional : bool}

type job = {
  name : string;
      (** Note that [name] does not translate to a field in a job, but
          instead to the key in the top-level that identifies the job. *)
  after_script : string list option;
  allow_failure : allow_failure_job option;
  artifacts : artifacts option;
  before_script : string list option;
  cache : cache list option;
  image : image option;
  interruptible : bool option;
  needs : need list option;
  dependencies : string list option;
  rules : job_rule list option;
  script : string list;
  services : service list option;
  stage : string option;
  variables : variables option;
  timeout : time_interval option;
  tags : string list option;
  when_ : when_job option;
  coverage : string option;
      (** Note: the job field [coverage] is not to be confused with
          {!coverage_report}.
          {{:https://docs.gitlab.com/ee/ci/yaml/#coverage}This
          coverage field} is used to specify a regular expression that
          can be used to capture coverage information from the job's
          trace.  On the other hand, {!coverage_report} is used to
          expose the captured coverage information as a report in a
          job's artifacts
          ({{:https://docs.gitlab.com/ee/ci/yaml/artifacts_reports.html#artifactsreportscoverage_report}ref}). *)
  retry : int option;
  parallel : int option;
}

type workflow = {rules : workflow_rule list; name : string option}

type include_ = {local : string; rules : include_rule list}

type config_element =
  | Workflow of workflow  (** Corresponds to a [workflow:] key. *)
  | Stages of string list  (** Corresponds to a [stages:] key. *)
  | Variables of variables  (** Corresponds to a [variables:] key. *)
  | Default of default  (** Corresponds to a [default:] key. *)
  | Job of job  (** Corresponds to a job, identified by it's key. *)
  | Include of include_ list  (** Corresponds to a [include:] key *)

(** A GitLab CI/CD configuration.

    Note that a configuration can consists of a sequence of
    [config_element]s. The same element can occur multiple times, and
    their order has semantic significance (for instance, with [include:]). *)
type config = config_element list
