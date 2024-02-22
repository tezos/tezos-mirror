(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Types

let default ?image ?interruptible () : default = {image; interruptible}

let job_rule ?changes ?if_ ?variables ?(when_ : when_ = On_success)
    ?allow_failure () : job_rule =
  (* Swap the
     {{:https://docs.gitlab.com/ee/ci/yaml/#allow_failure}default} of
     [allow_failure] for manual rules. This makes the default case
     non-blocking, and blocking rules have to be demanded
     explicitly. *)
  let allow_failure =
    match (when_, allow_failure) with
    | Manual, None -> Some Yes
    | _ -> allow_failure
  in
  {changes; if_; variables; when_; allow_failure}

let workflow_rule ?changes ?if_ ?variables ?(when_ : when_workflow = Always) ()
    : workflow_rule =
  {changes; if_; variables; when_}

let include_rule ?changes ?if_ ?(when_ : when_workflow = Always) () :
    include_rule =
  {changes; if_; when_}

let job ?after_script ?allow_failure ?artifacts ?before_script ?cache ?image
    ?interruptible ?needs ?dependencies ?rules ?services ?stage ?variables
    ?timeout ?tags ?when_ ?coverage ?retry ?parallel ~name ~script () =
  {
    name;
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
  }

let artifacts ?expire_in ?reports ?when_ ?expose_as ?name paths =
  (match (reports, paths) with
  | Some {dotenv = None; junit = None; coverage_report = None}, [] ->
      failwith
        "Attempted to register an artifact with no reports or paths -- this \
         doesn't make any sense"
  | _ -> ()) ;
  {
    expire_in;
    paths = (if paths = [] then None else Some paths);
    reports;
    when_;
    expose_as;
    name;
  }

let reports ?dotenv ?junit ?coverage_report () =
  (match (dotenv, junit, coverage_report) with
  | None, None, None ->
      failwith
        "Attempted to register a empty [reports] -- this doesn't make any sense"
  | _ -> ()) ;
  {dotenv; junit; coverage_report}
