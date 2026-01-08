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

type expiration = Duration of time_interval | Never

(** Represents values of the [when:] field in job rules.

    Note: [Delayed] jobs only run if their dependencies (or subsequent
    stages for jobs that have no [needs:]) passed. *)
type when_ = Always | Never | On_success | Manual | Delayed of time_interval

(** Represents values of the [when:] field of jobs. *)
type when_job = Always | On_success | Manual

(** Represents values of the [when:] field in [workflow:] and [include:] rules. *)
type when_workflow = Always | Never

(** Represents values of the [when:] field of trigger jobs. *)
type when_trigger_job = Always | On_success | On_failure

(** Represents values of the [job:allow_failure:] field of rules. *)
type allow_failure_job = Yes | No | With_exit_codes of int list

(** Represents values of the [rules:allow_failure:] field of rules.

    GitLab does not enable allowing a set of exit codes in rules,
    as is possible in a job's [allow_failure] field. *)
type allow_failure_rule = Yes | No

(** Represents auto cancel configurations.

    Auto-cancel can be used to auto-cancel pipelines on certain conditions.

    Auto-cancel can be configured globally for the full configuration
    (by adding it to the {!workflow}), or configured for a given
    workflow rule {!workflow_rule}. See
    {{:https://docs.gitlab.com/ee/ci/yaml/#workflowauto_cancelon_job_failure}
    workflow:auto_cancel:on_job_failure} for more info. *)
type auto_cancel = {on_new_commit : bool; on_job_failure : bool}

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
  auto_cancel : auto_cancel option;
      (** Auto-cancel for this workflow rule.

      See
      {{:https://docs.gitlab.com/ee/ci/yaml/#workflowrulesauto_cancel}
      workflow:rules:auto_cancel} for more info. *)
}

(** Represents an include rule. See {{:https://docs.gitlab.com/ee/ci/yaml/#includerules}include:rules} for more info. *)
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
  container_scanning : string option;
}

type image = Image of string

type when_artifact = Always | On_success | On_failure

type artifacts = {
  expire_in : expiration option;
  paths : string list option;
  reports : reports option;
  when_ : when_artifact option;
  expose_as : string option;
  name : string option;
}

type failure_type =
  | Unknown_failure
  | Script_failure
  | Api_failure
  | Stuck_or_timeout_failure
  | Runner_system_failure
  | Runner_unsupported
  | Stale_schedule
  | Job_execution_timeout
  | Archived_failure
  | Unmet_prerequisites
  | Scheduler_failure
  | Data_integrity_failure

(** Retry configuration.

    Retry at most [max] times (can be 0, 1, or 2).

    If [when_] is non-empty, only retry for those specific error causes.

    See {{:https://docs.gitlab.com/ee/ci/yaml/#retry}retry} in the GitLab
    YAML keyword reference for more information. *)
type retry = {max : int; when_ : failure_type list}

type default = {
  image : image option;
  interruptible : bool option;
  retry : retry option;
}

(** Policy for caches.

    - A [Pull_push] cache is pulled at the start of a job's execution and pushed at its termination. This is the default.
    - A [Push] cache is not pulled at the start of a job's execution, but is pushed at its termination,
    - A [Pull] cache is pulled at the start of a job's execution, but is not pushed at its termination.

    That is, a jobs with [Pull_push] cache produce and consume
    caches. Jobs with [Push] caches only produce the caches. Jobs with
    [Pull] caches only consumes them. *)
type cache_policy = Pull_push | Push | Pull

type cache = {
  key : string;
  paths : string list;
  policy : cache_policy;
  fallback_keys : string list option;
}

type service = {name : string}

type need = {job : string; optional : bool}

(** A matrix parallel job configuration.

    The matrix type allows to express how to run a job multiple times in
    parallel in a single pipeline, but with different variable values for each
    instance of the job. For example, a value of

    [[
      ("A", ["v1"; "v2"]);
      ("B"; ["v3"; "v4"]))
    ]]

    can be used to generate the following GitLab YAML:

    {{
    parallel:
      matrix:
        - A: ["v1","v2"]
          B: ["v3", "v4"]
    }}

    This will run 4 jobs with the associated variables [( v1,v3 ; v1,v4 ; v2;v3 ; v2;v4 )].

    See {{:https://docs.gitlab.com/ee/ci/yaml/#parallelmatrix}parallel:matrix} in the
    GitLab YAML keyword reference for more information. *)
type matrix = (string * string list) list list

(** Parallelism configuration of a job.

    - [Vector n] will create [n] copies of the job.
    - [Matrix envs] will create one job per element in the
      cartesian product of the variable values domains for each
      environment. See {!matrix} for more details.

    See {{:https://docs.gitlab.com/ee/ci/yaml/#parallel}parallel} in the GitLab
    YAML keyword reference for more information. *)
type parallel = Vector of int | Matrix of matrix

(** Represents an inherit rule.

    See https://docs.gitlab.com/ee/ci/yaml/index.html#inherit *)
type inherit_ = Variable_list of string list | Variable_bool of bool

(** Represents audience(s) used in id_tokens.

    The aud sub-keyword is used to configure the aud claim for the JWT
    in id_tokens.

    See {{:https://docs.gitlab.com/ee/ci/yaml/#id_tokens}id_tokens} in
    the GitLab YAML keyword reference for more information. *)
type aud = Aud_string of string | Aud_list of string list

(** Represents an id_tokens.

    Use id_tokens to create JSON web tokens (JWT) to authenticate with third
    party services. All JWTs created this way support OIDC authentication.
    For example, a value of

    [[
      ("ID_TOKEN_1", Gitlab_ci.Types.Aud_string "https://vault.example.com");
      ("ID_TOKEN_2", Gitlab_ci.Types.Aud_list [
          "https://gcp.com";
          "https://aws.com";
        ])
    ]]

    can be used to generate the following GitLab YAML:

    {{
    id_tokens:
      ID_TOKEN_1:
        aud: https://vault.example.com
      ID_TOKEN_2:
        aud:
          - https://gcp.com
          - https://aws.com
    }}


    See {{:https://docs.gitlab.com/ee/ci/yaml/#id_tokens}id_tokens} in
    the GitLab YAML keyword reference for more information. *)
type id_tokens = (string * aud) list

type job = {
  name : string;
      (** Note that [name] does not translate to a field in a job, but
          instead to the key in the top-level that identifies the job. *)
  after_script : string list option;
  allow_failure : allow_failure_job option;
  artifacts : artifacts option;
  before_script : string list option;
  cache : cache list option;
  id_tokens : id_tokens option;
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
  retry : retry option;
  parallel : parallel option;
}

(** Represents a trigger rule.

    Implements [trigger:include] and [trigger:strategy].
    [trigger:include] should be a path to the definition of the child-pipeline.
*)
type trigger = {include_ : string; strategy_depend : bool}

(** Parent-child downstream pipeline trigger.

    {{:https://docs.gitlab.com/ee/ci/pipelines/downstream_pipelines.html#parent-child-pipelines}
    Parent-child downstream pipelines} execute in the same project as
    the parent pipeline. They inherit the global variables of the
    parent pipeline.

    Be aware that the child pipeline must define a workflow that
    enables the jobs therein. Notably, the default workflow does not
    enable merge request pipelines, and thus the workflow for a child
    pipeline spawned by a merge request pipeline must include
    [workflow:] section with rules that also enables merge request
    pipelines.

    The fields of a trigger job is
    a subset of those of a normal {!job} and share the same
    semantics. The fields supported by GitLab for trigger jobs are:

    - [allow_failure].
    - [extends].
    - [needs], but not [needs:project].
    - [only] and [except].
    - [inherit]
    - [rules].
    - [stage].
    - [trigger].
    - [variables].
    - [when] (only with a value of [on_success], [on_failure], or [always]).
    - [resource_group].
    - [environment].

    We currently only implement a subset of these, but more can be
    added as need arises. See
    {{:https://docs.gitlab.com/ee/ci/yaml/#trigger}CI/CD YAML syntax
    reference} for more information. *)
type trigger_job = {
  name : string;
  stage : string option;
  variables : variables option;
  when_ : when_trigger_job option;
  inherit_ : inherit_ option;
  rules : job_rule list option;
  needs : need list option;
  trigger : trigger;
}

type generic_job = Job of job | Trigger_job of trigger_job

(** Represents a workflow configuration.

    See {{:https://docs.gitlab.com/ee/ci/yaml/#workflow}workflow} for more info. *)
type workflow = {
  rules : workflow_rule list;
  name : string option;
  auto_cancel : auto_cancel option;
      (** Default auto-cancel for the configuration.

      See {{:https://docs.gitlab.com/ee/ci/yaml/#workflowauto_cancelon_new_commit} workflow:auto_cancel} for more info. *)
}

(** GitLab templates provided for vulnerability scanners.  For more
    information, see
    {{:https://docs.gitlab.com/ee/user/application_security/#vulnerability-scanner-maintenance}Vulnerability
    scanners}. *)
type template =
  | Jobs_container_scanning
      (** See {{:https://docs.gitlab.com/ee/user/application_security/container_scanning/index.html}Container Scanning} *)

let path_of_template template =
  match template with
  | Jobs_container_scanning -> "Jobs/Container-Scanning.gitlab-ci.yml"

(* We only use the Container Scanning tool at the moment but more
   scanners should be added in the future. *)

(** Represents an include configuration. Only [include:local] [include:template] are supported, as other subkeys are not used in our CI.

    See
    - {{:https://docs.gitlab.com/ee/ci/yaml/#include}include},
    - {{:https://docs.gitlab.com/ee/ci/yaml/#includelocal}include:local}
    - and {{:https://docs.gitlab.com/ee/ci/yaml/#includetemplate}include:template}
    for more info. *)

type include_subkey = Local of string | Template of template

type include_ = {subkey : include_subkey; rules : include_rule list}

type config_element =
  | Workflow of workflow  (** Corresponds to a [workflow:] key. *)
  | Stages of string list  (** Corresponds to a [stages:] key. *)
  | Variables of variables  (** Corresponds to a [variables:] key. *)
  | Default of default  (** Corresponds to a [default:] key. *)
  | Generic_job of generic_job
      (** Corresponds to a job, identified by it's key. *)
  | Include of include_ list  (** Corresponds to a [include:] key *)
  | Comment of string  (** Corresponds to a top-level YAML comment. *)

(** A GitLab CI/CD configuration.

    Note that a configuration can consists of a sequence of
    [config_element]s. The same element can occur multiple times, and
    their order has semantic significance (for instance, with [include:]). *)
type config = config_element list
