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

let enc_when_trigger_job : when_trigger_job -> value = function
  | Always -> `String "always"
  | On_success -> `String "on_success"
  | On_failure -> `String "on_failure"

let enc_auto_cancel {on_new_commit; on_job_failure} =
  `O
    ((if on_new_commit then [("on_new_commit", `String "all")] else [])
    @ if on_job_failure then [("on_job_failure", `String "all")] else [])

let enc_workflow_rule : workflow_rule -> value =
 fun {changes; if_; variables; when_; auto_cancel} ->
  obj_flatten
    [
      opt "changes" strings changes;
      opt "if" enc_if if_;
      opt "variables" enc_variables variables;
      key "when" enc_when_workflow when_;
      opt "auto_cancel" enc_auto_cancel auto_cancel;
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

let enc_expiration = function
  | Duration interval -> enc_time_interval interval
  | Never -> `String "never"

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
  | {name; rules; auto_cancel} ->
      obj_flatten
        [
          opt "name" string name;
          key "rules" enc_workflow_rules rules;
          opt "auto_cancel" enc_auto_cancel auto_cancel;
        ]

let enc_stages stages : value = strings stages

let enc_id_tokens (id_tokens : id_tokens) : value =
  `O
    (List.map
       (fun (id_token_name, aud) ->
         ( id_token_name,
           match aud with
           | Aud_string aud_str -> `O [("aud", `String aud_str)]
           | Aud_list aud_list ->
               obj_flatten [key "aud" (array string) aud_list] ))
       id_tokens)

let enc_image (Image image) = string image

let enc_retry : retry -> value =
  let enc_failure_type failure_type =
    let failure_type =
      match failure_type with
      | Unknown_failure -> "unknown_failure"
      | Script_failure -> "script_failure"
      | Api_failure -> "api_failure"
      | Stuck_or_timeout_failure -> "stuck_or_timeout_failure"
      | Runner_system_failure -> "runner_system_failure"
      | Runner_unsupported -> "runner_unsupported"
      | Stale_schedule -> "stale_schedule"
      | Job_execution_timeout -> "job_execution_timeout"
      | Archived_failure -> "archived_failure"
      | Unmet_prerequisites -> "unmet_prerequisites"
      | Scheduler_failure -> "scheduler_failure"
      | Data_integrity_failure -> "data_integrity_failure"
    in
    `String failure_type
  in
  fun {max; when_} ->
    if when_ = [] then int max
    else `O [("max", int max); ("when", array enc_failure_type when_)]

let enc_default ({image; interruptible; retry} : default) : value =
  obj_flatten
    [
      opt "image" enc_image image;
      opt "interruptible" bool interruptible;
      opt "retry" enc_retry retry;
    ]

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
 fun {dotenv; junit; coverage_report; container_scanning} ->
  obj_flatten
    [
      opt "dotenv" string dotenv;
      opt "junit" string junit;
      opt "coverage_report" enc_coverage coverage_report;
      opt "container_scanning" string container_scanning;
    ]

let enc_artifacts : artifacts -> value =
 fun {expire_in; paths; reports; when_; expose_as; name} ->
  obj_flatten
    [
      opt "name" string name;
      opt "expire_in" enc_expiration expire_in;
      opt "paths" strings paths;
      opt "reports" enc_report reports;
      opt "when" enc_when_artifact when_;
      opt "expose_as" string expose_as;
    ]

let enc_cache : cache -> value =
 fun {key = k; paths; policy; fallback_keys} ->
  obj_flatten
    [
      key "key" string k;
      key "paths" strings paths;
      opt "fallback_keys" strings fallback_keys;
      key
        "policy"
        (fun policy ->
          `String
            (match policy with
            | Pull -> "pull"
            | Push -> "push"
            | Pull_push -> "pull-push"))
        policy;
    ]

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

let enc_parallel (parallel : parallel) : value =
  match parallel with
  | Vector n -> int n
  | Matrix environments ->
      let matrix =
        Fun.flip list environments @@ fun environment ->
        `O
          (List.map
             (fun (name, values) -> (name, list string values))
             environment)
      in
      `O [("matrix", matrix)]

let enc_job : job -> value =
 fun {
       name = _;
       after_script;
       allow_failure;
       artifacts;
       before_script;
       cache;
       id_tokens;
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
      opt "id_tokens" enc_id_tokens id_tokens;
      opt "when" enc_when_job when_;
      opt "coverage" string coverage;
      opt "retry" enc_retry retry;
      opt "parallel" enc_parallel parallel;
    ]

let enc_inherit : inherit_ -> value = function
  | Variable_bool b -> `O (key "variables" bool b)
  | Variable_list v -> `O (key "variables" (list string) v)

let enc_trigger_job : trigger_job -> value =
  let enc_trigger trigger =
    obj_flatten
      [
        key "include" string trigger.include_;
        opt
          "strategy"
          string
          (if trigger.strategy_depend then Some "depend" else None);
      ]
  in
  fun {name = _; stage; variables; when_; inherit_; rules; needs; trigger} ->
    obj_flatten
      [
        opt "stage" string stage;
        opt "variables" enc_variables variables;
        opt "inherit" enc_inherit inherit_;
        opt "rules" enc_job_rules rules;
        opt "needs" enc_needs needs;
        opt "when" enc_when_trigger_job when_;
        key "trigger" enc_trigger trigger;
      ]

let enc_includes : include_ list -> value =
  let enc_subkey subkey : string * value =
    match subkey with
    | Local path -> ("local", `String path)
    | Template template -> ("template", `String (path_of_template template))
  in

  fun includes ->
    let enc_includes ({subkey; rules} : include_) =
      match rules with
      | [] -> `O [enc_subkey subkey]
      | _ :: _ -> `O [enc_subkey subkey; ("rules", enc_include_rules rules)]
    in
    match includes with
    | [] -> failwith "enc_includes: empty includes"
    | [{subkey; rules = []}] -> `O [enc_subkey subkey]
    | inc -> array enc_includes inc

let config_element : config_element -> (string * value) option = function
  | Workflow wf -> Some ("workflow", enc_workflow wf)
  | Stages ss -> Some ("stages", enc_stages ss)
  | Variables vars -> Some ("variables", enc_variables vars)
  | Default def -> Some ("default", enc_default def)
  | Generic_job (Job j) -> Some (j.name, enc_job j)
  | Generic_job (Trigger_job j) -> Some (j.name, enc_trigger_job j)
  | Include i -> Some ("include", enc_includes i)
  | Comment _ -> None

let to_yaml (config : config) : value =
  `O (List.filter_map config_element config)

let to_file ?header ~filename config =
  Base.with_open_out filename @@ fun ch ->
  let out = output_string ch in
  Option.iter out header ;
  let rec loop =
    let print element =
      (match element with
      | Comment s ->
          let lines = String.split_on_char '\n' s in
          let lines = List.map (fun s -> String.trim ("# " ^ s)) lines in
          out (String.concat "\n" lines)
      | _ -> ()) ;
      Option.iter
        (fun field -> out (Base.yaml_to_string (`O [field])))
        (config_element element)
    in
    function
    | [] -> ()
    | [x] -> print x
    | x :: y :: ys ->
        print x ;
        out "\n" ;
        loop (y :: ys)
  in
  loop config
