(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Yaml
open Yaml.Util
open Types

(* Helpers *)

let opt name f = function Some v -> [(name, f v)] | None -> []

let obj_flatten fields = `O (List.concat fields)

let key name f value : (string * value) list = [(name, f value)]

let array f values = `A (List.map f values)

(* Equivalent to [array f values] unless [values] is a singleton [x],
   in which case it is encoded as [f x].

   This is useful for more succint encoding of fields like [cache:]
   that can either take an array of values, or a single value. *)
let array1 f values =
  match values with [value] -> f value | _ -> array f values

let strings ss : value = array string ss

let int i = float (float_of_int i)

(* Translation elements *)

let enc_if expr = string @@ If.encode expr

let enc_variables (vars : variables) : value =
  `O (List.map (fun (name, value) -> (name, `String value)) vars)

let enc_when : when_ -> value = function
  | Always -> `String "always"
  | Never -> `String "never"
  | On_success -> `String "on_success"
  | Manual -> `String "manual"
  | Delayed _ -> `String "delayed"

let enc_when_workflow : when_workflow -> value = function
  | Always -> `String "always"
  | Never -> `String "never"

let enc_when_artifact : when_artifact -> value = function
  | Always -> `String "always"
  | On_failure -> `String "on_failure"
  | On_success -> `String "on_success"

let enc_when_job : when_job -> value = function
  | Always -> `String "always"
  | On_success -> `String "on_success"
  | Manual -> `String "manual"

let enc_workflow_rule : workflow_rule -> value =
 fun {changes; if_; variables; when_} ->
  obj_flatten
    [
      opt "changes" strings changes;
      opt "if" enc_if if_;
      opt "variables" enc_variables variables;
      key "when" enc_when_workflow when_;
    ]

let enc_allow_failure_rule (allow_failure : allow_failure_rule) : value =
  match allow_failure with Yes -> `Bool true | No -> `Bool false

let enc_time_interval interval =
  `String
    (match interval with
    | Seconds 1 -> "1 second"
    | Seconds x -> string_of_int x ^ " seconds"
    | Minutes 1 -> "1 minute"
    | Minutes x -> string_of_int x ^ " minutes"
    | Hours 1 -> "1 hour"
    | Hours x -> string_of_int x ^ " hours"
    | Days 1 -> "1 day"
    | Days x -> string_of_int x ^ " days"
    | Weeks 1 -> "1 week"
    | Weeks x -> string_of_int x ^ " weeks"
    | Months 1 -> "1 month"
    | Months x -> string_of_int x ^ " months"
    | Years 1 -> "1 year"
    | Years x -> string_of_int x ^ " years")

let enc_job_rule : job_rule -> value =
 fun {changes; if_; variables; when_; allow_failure} ->
  let start_in =
    match when_ with Delayed start_in -> Some start_in | _ -> None
  in
  obj_flatten
    [
      opt "changes" strings changes;
      opt "if" enc_if if_;
      opt "variables" enc_variables variables;
      key "when" enc_when when_;
      opt "allow_failure" enc_allow_failure_rule allow_failure;
      opt "start_in" enc_time_interval start_in;
    ]

let enc_include_rule : include_rule -> value =
 fun {changes; if_; when_} ->
  obj_flatten
    [
      opt "changes" strings changes;
      opt "if" enc_if if_;
      key "when" enc_when_workflow when_;
    ]

let enc_workflow_rules : workflow_rule list -> value = array enc_workflow_rule

let enc_job_rules : job_rule list -> value = array enc_job_rule

let enc_include_rules : include_rule list -> value = array enc_include_rule

let enc_workflow : workflow -> value = function
  | {name; rules} ->
      obj_flatten [opt "name" string name; key "rules" enc_workflow_rules rules]

let enc_stages stages : value = strings stages

let enc_image (Image image) = string image

let enc_default ({image; interruptible} : default) : value =
  obj_flatten
    [opt "image" enc_image image; opt "interruptible" bool interruptible]

let enc_coverage : coverage_report -> value =
 fun {coverage_format; path} ->
  obj_flatten
    [
      key
        "coverage_format"
        (function Cobertura -> `String "cobertura")
        coverage_format;
      key "path" string path;
    ]

let enc_report : reports -> value =
 fun {dotenv; junit; coverage_report} ->
  obj_flatten
    [
      opt "dotenv" string dotenv;
      opt "junit" string junit;
      opt "coverage_report" enc_coverage coverage_report;
    ]

let enc_artifacts : artifacts -> value =
 fun {expire_in; paths; reports; when_; expose_as; name} ->
  obj_flatten
    [
      opt "name" string name;
      opt "expire_in" enc_time_interval expire_in;
      key "paths" strings paths;
      opt "reports" enc_report reports;
      opt "when" enc_when_artifact when_;
      opt "expose_as" string expose_as;
    ]

let enc_cache : cache -> value =
 fun {key = k; paths} ->
  obj_flatten [key "key" string k; key "paths" strings paths]

let enc_service ({name} : service) : value = `String name

let enc_services (ss : service list) : value = array enc_service ss

let enc_allow_failure_job (allow_failure : allow_failure_job) : value =
  match allow_failure with
  | Yes -> `Bool true
  | No -> `Bool false
  | With_exit_codes codes -> `O [("exit_codes", array1 int codes)]

let enc_needs (needs : need list) : value =
  (* Use terse encoding unless optional is set to true for at least one need *)
  let enc_need =
    if List.for_all (fun {job = _; optional} -> optional = false) needs then
      fun {job; optional = _} -> `String job
    else fun {job; optional} ->
      `O
        ([("job", `String job)]
        @ if optional then [("optional", `Bool true)] else [])
  in
  array enc_need needs

let enc_job : job -> value =
 fun {
       name = _;
       after_script;
       allow_failure;
       artifacts;
       before_script;
       cache;
       image;
       interruptible;
       needs;
       dependencies;
       rules;
       script;
       services;
       stage;
       variables;
       timeout;
       tags;
       when_;
       coverage;
       retry;
       parallel;
     } ->
  obj_flatten
    [
      opt "image" enc_image image;
      opt "stage" string stage;
      opt "tags" (array string) tags;
      opt "rules" enc_job_rules rules;
      opt "needs" enc_needs needs;
      opt "dependencies" strings dependencies;
      opt "allow_failure" enc_allow_failure_job allow_failure;
      opt "timeout" enc_time_interval timeout;
      opt "cache" (array1 enc_cache) cache;
      opt "interruptible" bool interruptible;
      opt "before_script" strings before_script;
      key "script" strings script;
      opt "after_script" strings after_script;
      opt "services" enc_services services;
      opt "variables" enc_variables variables;
      opt "artifacts" enc_artifacts artifacts;
      opt "when" enc_when_job when_;
      opt "coverage" string coverage;
      opt "retry" int retry;
      opt "parallel" int parallel;
    ]

let enc_includes : include_ list -> value =
 fun includes ->
  let enc_includes ({local; rules} : include_) =
    match rules with
    | [] -> `String local
    | _ :: _ ->
        `O [("local", `String local); ("rules", enc_include_rules rules)]
  in
  match includes with
  | [] -> failwith "empty includes"
  | [{local; rules = []}] -> `String local
  | inc -> array enc_includes inc

let config_element : config_element -> string * value = function
  | Workflow wf -> ("workflow", enc_workflow wf)
  | Stages ss -> ("stages", enc_stages ss)
  | Variables vars -> ("variables", enc_variables vars)
  | Default def -> ("default", enc_default def)
  | Job j -> (j.name, enc_job j)
  | Include i -> ("include", enc_includes i)

let to_yaml (config : config) : value = `O (List.map config_element config)

let to_file ?header ~filename config =
  Base.write_yaml ?header filename (to_yaml config)
