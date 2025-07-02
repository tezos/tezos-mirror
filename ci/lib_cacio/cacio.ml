(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Gitlab_ci.Base

type stage = Build | Test | Publish

(* Should actually be equivalent to [Stdlib.compare]
   if stages are defined in the right order.
   But this function is used to check job dependencies,
   so the order cannot be arbitrary.
   It is thus safer to be explicit instead of relying on the compiler's internals.
   Also, if one adds a stage, the compiler will attract the attention to this function,
   and thus this comment. *)
let compare_stages a b =
  match (a, b) with
  | Build, Build -> 0
  | Build, (Test | Publish) -> -1
  | Test, Build -> 1
  | Test, Test -> 0
  | Test, Publish -> -1
  | Publish, (Build | Test) -> 1
  | Publish, Publish -> 0

let show_stage = function
  | Build -> "build"
  | Test -> "test"
  | Publish -> "publish"

type need = Job | Artifacts

type job = {
  uid : int;
  source_location : string * int * int * int;
  name : string;
  stage : stage;
  description : string;
  image : Tezos_ci.Image.t;
  needs : (need * job) list;
  needs_legacy : (need * Tezos_ci.tezos_job) list;
  only_if_changed : Tezos_ci.Changeset.t;
  variables : Gitlab_ci.Types.variables option;
  script : string list;
  artifacts : Gitlab_ci.Types.artifacts option;
}

type trigger = Auto | Manual

let fresh_uid =
  let last = ref (-1) in
  fun () ->
    incr last ;
    !last

module UID_set = Set.Make (Int)
module UID_map = Map.Make (Int)

(* GRAPH TRANSFORMATIONS

   Step 1: the user specifies a list of jobs, as well as how they want those jobs
   to be triggered. This list thus have type [(trigger * job) list].

   Step 2: the [make_graph] function converts this list into a graph, of type [job_graph].
   The main differences with the original list are:
   - a [job_graph] is not a list, but a map from job UID,
     making it more efficient to work with;
   - a [job_graph] is transitively closed, i.e. all dependencies are explicitly part
     of the graph even if they were not explicitly given in the original list.
   Additionally, [make_graph] checks that the same job does not appear twice
   in the original list with different triggers.

   Step 3: the [fix_graph] function converts the [job_graph] into a [fixed_job_graph].
   This function fixes triggers and changesets.
   By "fix" we mean not only that we set them to corrected values,
   but also that these corrected values are a fixpoint.
   More precisely:
   - triggers are fixed so that if a job has trigger [Auto],
     then all of its dependencies also have trigger [Auto];
   - changesets are fixed so that if changing a file causes a job to be included,
     then changing this file also causes all dependencies of this job to be included.

   At this point, we took the *intent* of the user (given in step 1),
   and corrected it to something that actually makes sense.
   In particular this prevents some cases of invalid pipelines.

   Step 4: the [convert_graph] function converts the [fixed_job_graph]
   into a [tezos_job_graph]. This just converts all jobs to [Tezos_ci.tezos_job],
   that can be used by CIAO.

   Those successive transformations are driven by the [convert_jobs] function,
   which is used by all functions that take Cacio jobs and register them as CIAO jobs. *)

type job_graph_node = {
  job : job;
  trigger : trigger option;
  rev_deps : UID_set.t; (* UIDs of jobs that directly depend on [job]. *)
}

type job_graph = job_graph_node UID_map.t

let error_s (file, line, start_char, end_char) message =
  (* We start with [\n] because Dune's progress bar messes with the output. *)
  Printf.eprintf
    "\nFile %S, line %d, characters %d-%d:\nError: %s\n%!"
    file
    line
    start_char
    end_char
    message ;
  exit 1

let error pos x = Printf.ksprintf (error_s pos) x

(* Compute the transitive closure of [jobs] as a [job_graph].
   Also makes sure that the same job is not requested twice with different triggers.
   See GRAPH TRANSFORMATIONS above (step 2). *)
let make_graph (jobs : (trigger * job) list) : job_graph =
  (* [add_job] adds [job] to [acc], as well as its dependencies, recursively.
     [acc] is the resulting graph being built.
     [add_job] also adds [rev_deps] to the list of reverse dependencies of [job].
     [trigger] is the trigger to use for [job]; it can be:
     - [Some _] if the trigger was specified by the user, from the [jobs] list;
     - [None] if the job was added automatically as a dependency.*)
  let rec add_job ~trigger ~rev_deps acc job =
    (* Add dependencies of [job], with at least [job] as reverse dependency. *)
    let acc : job_graph_node UID_map.t =
      List.fold_left
        (add_job ~trigger:None ~rev_deps:(UID_set.singleton job.uid))
        acc
        (List.map snd job.needs)
    in
    (* Add [job], with at least [rev_deps] as reverse dependency. *)
    let update = function
      | None ->
          (* [job] is not in the graph yet, just add it. *)
          Some {job; trigger; rev_deps}
      | Some {job; trigger = old_trigger; rev_deps = old_rev_deps} ->
          (* [job] is already in the graph, update it. *)
          (* Make sure the triggers are compatible. *)
          let trigger =
            match (old_trigger, trigger) with
            | x, None | None, x ->
                (* If both triggers are [None], it means the job has been added twice
                   as a dependency; we don't know the triggers of its reverse dependencies
                   yet so we keep [None].
                   If one of the trigger is [None] and the other is [Some _],
                   it means the job was both specified by the user (with a trigger),
                   and added automatically. In this case we keep the trigger specified
                   by the user. *)
                x
            | Some old_trigger, Some trigger ->
                (* The job was specified twice in [jobs].
                   This is suspicious but not a problem
                   as long as the triggers are the same. *)
                if old_trigger <> trigger then
                  error
                    job.source_location
                    "job %S is listed twice in the same pipeline but with \
                     different triggers"
                    job.name ;
                Some trigger
          in
          (* A job can be added multiple times because it is the dependency
             of multiple reverse dependencies.
             Make sure all those reverse dependencies are stored. *)
          let rev_deps = UID_set.union rev_deps old_rev_deps in
          Some {job; trigger; rev_deps}
    in
    UID_map.update job.uid update acc
  in
  (* Start from the empty graph and use [add_jobs] to add all [jobs]. *)
  List.fold_left
    (fun acc (trigger, job) ->
      add_job ~trigger:(Some trigger) ~rev_deps:UID_set.empty acc job)
    UID_map.empty
    jobs

type fixed_job_graph_node = {
  job : job;
  trigger : trigger;
  only_if_changed : Tezos_ci.Changeset.t;
}

type fixed_job_graph = fixed_job_graph_node UID_map.t

(* Compute the final values for [only_if_changed] and [trigger]s.
   Assumes there are no cycles.
   See GRAPH TRANSFORMATIONS above (step 3). *)
let fix_graph (graph : job_graph) : fixed_job_graph =
  (* To build the graph, we take all jobs from [graph], fix them,
     and add them to [result].
     But before we fix a job, we need the [trigger] and [only_if_changed]
     of its reverse dependencies.
     So we need to fix those reverse dependencies first.
     [fix_uid] takes the UID of a job, fixes and adds its reverse dependencies recursively,
     then fixes this job and adds it to [result]. *)
  let result : fixed_job_graph ref = ref UID_map.empty in
  let rec fix_uid uid =
    match UID_map.find_opt uid !result with
    | Some result_node ->
        (* Job already visited as dependency of another job. *)
        result_node
    | None ->
        let result_node =
          match UID_map.find_opt uid graph with
          | None ->
              (* Not supposed to happen. *)
              assert false
          | Some {job; trigger; rev_deps} ->
              (* Fix and add reverse dependencies recursively. *)
              let rev_deps = rev_deps |> UID_set.elements |> List.map fix_uid in
              (* Fix [trigger]. *)
              let trigger =
                let merge_triggers a b =
                  match (a, b) with
                  | Manual, Manual -> Manual
                  | Auto, (Auto | Manual) | Manual, Auto ->
                      (* If a job is supposed to run automatically,
                         its dependencies must run automatically as well. *)
                      Auto
                in
                let initial_trigger =
                  match trigger with
                  | None ->
                      (* We don't know yet.
                         [Manual] will be upgraded to [Auto] if necessary. *)
                      Manual
                  | Some trigger -> trigger
                in
                rev_deps
                |> List.map (fun node -> node.trigger)
                |> List.fold_left merge_triggers initial_trigger
              in
              (* Fix the changeset, i.e. add the union of the changesets
                 of reverse dependencies. *)
              let only_if_changed =
                rev_deps
                |> List.map (fun node -> node.only_if_changed)
                |> List.fold_left Tezos_ci.Changeset.union job.only_if_changed
              in
              {job; trigger; only_if_changed}
        in
        result := UID_map.add uid result_node !result ;
        result_node
  in
  (* Make sure all jobs from the input [graph] are visited at least once. *)
  UID_map.iter
    (fun uid _ ->
      let _ : fixed_job_graph_node = fix_uid uid in
      ())
    graph ;
  !result

let convert_stage (stage : stage) : Tezos_ci.Stage.t =
  match stage with
  | Build -> Tezos_ci.Stages.build
  | Test -> Tezos_ci.Stages.test
  | Publish -> Tezos_ci.Stages.publish

type tezos_job_graph = Tezos_ci.tezos_job UID_map.t

(* Convert jobs to [Tezos_ci] jobs.
   See GRAPH TRANSFORMATIONS above (step 4).

   If [with_changes] is [true], the job [rules] will contain a [changes] clause.
   If it is [false], [only_if_changed] is ignored.
   [changes] clauses are typically only used in merge request pipelines
   such as [before_merging]. *)
let convert_graph ~with_changes (graph : fixed_job_graph) : tezos_job_graph =
  (* To build the graph, we take all jobs from [graph], convert them,
     and add them to [result].
     But before we convert a job, we need the converted version of its dependencies.
     So we need to fix those dependencies first.
     [convert_uid] takes the UID of a job, converts its dependencies recursively,
     then converts this job and adds it to [result]. *)
  let result = ref UID_map.empty in
  let rec convert_uid uid =
    match UID_map.find_opt uid !result with
    | Some result_node ->
        (* Job already visited as dependency of another job. *)
        result_node
    | None ->
        let result_node =
          match UID_map.find_opt uid graph with
          | None ->
              (* Not supposed to happen. *)
              assert false
          | Some
              {
                job =
                  {
                    uid = _;
                    source_location;
                    name;
                    stage;
                    description;
                    image;
                    needs;
                    needs_legacy;
                    only_if_changed = _;
                    variables;
                    script;
                    artifacts;
                  };
                trigger;
                only_if_changed;
              } ->
              (* Convert dependencies recursively. *)
              let dependencies =
                let needs =
                  needs_legacy
                  @ List.map
                      (fun (need, dep) -> (need, convert_uid dep.uid))
                      needs
                in
                Fun.flip List.map needs @@ fun (need, dep) ->
                match need with
                | Job -> Tezos_ci.Job dep
                | Artifacts -> Tezos_ci.Artifacts dep
              in
              (* Compute [rules] from on the job's fixed changeset,
                 whether we actually want the [changes] clause ([with_changes]),
                 and the [trigger]. *)
              let rules =
                if with_changes then
                  [
                    Gitlab_ci.Util.job_rule
                      ~changes:(Tezos_ci.Changeset.encode only_if_changed)
                      ~when_:
                        (match trigger with
                        | Auto -> On_success
                        | Manual -> Manual)
                      ();
                  ]
                else
                  match trigger with
                  | Auto -> []
                  | Manual -> [Gitlab_ci.Util.job_rule ~when_:Manual ()]
              in
              let interruptible =
                match stage with Build | Test -> true | Publish -> false
              in
              Tezos_ci.job
                ~__POS__:source_location
                ~name
                ~stage:(convert_stage stage)
                ~description
                ~image
                ~dependencies:(Dependent dependencies)
                ~rules
                ~interruptible
                ?variables
                ?artifacts
                script
        in
        result := UID_map.add uid result_node !result ;
        result_node
  in
  (* Make sure all jobs from the input [graph] are visited at least once. *)
  UID_map.iter
    (fun uid _ ->
      let _ : Tezos_ci.tezos_job = convert_uid uid in
      ())
    graph ;
  !result

(* Convert user-specified jobs into [Tezos_ci] jobs.
   See GRAPH TRANSFORMATIONS. *)
let convert_jobs ~with_changes (jobs : (trigger * job) list) :
    Tezos_ci.tezos_job list =
  jobs |> make_graph |> fix_graph
  |> convert_graph ~with_changes
  |> UID_map.bindings |> List.map snd

let parameterize make =
  let table = Hashtbl.create 8 in
  fun value ->
    match Hashtbl.find_opt table value with
    | None ->
        let result = make value in
        Hashtbl.add table value result ;
        result
    | Some result -> result

module type COMPONENT = sig
  val name : string

  val paths : string list
end

module type COMPONENT_API = sig
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

  val register_before_merging_jobs : (trigger * job) list -> unit

  val register_master_jobs : (trigger * job) list -> unit

  val register_scheduled_pipeline :
    description:string -> string -> (trigger * job) list -> unit

  val register_global_release_jobs : (trigger * job) list -> unit

  val register_global_test_release_jobs : (trigger * job) list -> unit

  val register_global_scheduled_test_release_jobs : (trigger * job) list -> unit

  val register_dedicated_release_pipeline : (trigger * job) list -> unit

  val register_dedicated_test_release_pipeline : (trigger * job) list -> unit
end

(* We could avoid using a functor if we required the user of this module
   to pass the component's [name] and [paths] to the functions that need them,
   but in practice this would be less convenient since all functions need at least
   one of them. *)
module Make (Component : COMPONENT) : COMPONENT_API = struct
  let only_if_changed = Tezos_ci.Changeset.make Component.paths

  let job ~__POS__:source_location ~stage ~description ~image ?(needs = [])
      ?(needs_legacy = []) ?variables ?artifacts name script =
    let name = Component.name ^ "." ^ name in
    (* Check that no dependency is in an ulterior stage. *)
    ( Fun.flip List.iter needs @@ fun (_, dep) ->
      if compare_stages dep.stage stage > 0 then
        error
          source_location
          "job %s, which is in stage '%s', cannot depend on %s, which is in \
           ulterior stage '%s'"
          name
          (show_stage stage)
          dep.name
          (show_stage dep.stage) ) ;
    {
      uid = fresh_uid ();
      source_location;
      name;
      stage;
      description;
      image;
      needs;
      needs_legacy;
      only_if_changed;
      variables;
      script;
      artifacts;
    }

  let register_before_merging_jobs jobs =
    (* Add [trigger] as a dependency of all [jobs]. *)
    let jobs =
      (* The actual [trigger] job is defined deep inside [code_verification.ml]
         (look for [job_start]). CIAO only really cares about the name of the job,
         so we hackishly redefine it here. *)
      let job_trigger =
        Tezos_ci.job ~__POS__ ~stage:Tezos_ci.Stages.start ~name:"trigger" []
      in
      (* Here we re-allocate the jobs with the same UID to override [needs_legacy].
         But only these re-allocated jobs will be in the pipeline,
         so there is no risk of actually duplicating them. *)
      Fun.flip List.map jobs @@ fun (need, job) ->
      let job =
        {job with needs_legacy = (Job, job_trigger) :: job.needs_legacy}
      in
      (need, job)
    in
    let jobs = convert_jobs ~with_changes:true jobs in
    Tezos_ci.Hooks.before_merging := jobs @ !Tezos_ci.Hooks.before_merging

  let register_master_jobs jobs =
    let jobs = convert_jobs ~with_changes:false jobs in
    Tezos_ci.Hooks.master := jobs @ !Tezos_ci.Hooks.master

  let full_pipeline_name name = sf "%s.%s" Component.name name

  (* Helper for other functions below, that is not exposed to users of this module.
     It is responsible for:
     - prefixing the name of the pipeline with the name of the component;
     - including the DataDog job.
     Returns the pipeline name. *)
  let register_pipeline ~description ~jobs name rules =
    Tezos_ci.Pipeline.register
      ~description
      ~jobs:(Tezos_ci.job_datadog_pipeline_trace :: jobs)
      (full_pipeline_name name)
      rules

  let register_scheduled_pipeline ~description name jobs =
    register_pipeline
      name
      ~description
      ~jobs:(convert_jobs ~with_changes:false jobs)
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          scheduled && var "TZ_SCHEDULE_KIND" == str (full_pipeline_name name)))

  let register_global_release_jobs jobs =
    let jobs = convert_jobs ~with_changes:false jobs in
    Tezos_ci.Hooks.global_release := jobs @ !Tezos_ci.Hooks.global_release

  let register_global_test_release_jobs jobs =
    let jobs = convert_jobs ~with_changes:false jobs in
    Tezos_ci.Hooks.global_test_release :=
      jobs @ !Tezos_ci.Hooks.global_test_release

  let register_global_scheduled_test_release_jobs jobs =
    let jobs = convert_jobs ~with_changes:false jobs in
    Tezos_ci.Hooks.global_scheduled_test_release :=
      jobs @ !Tezos_ci.Hooks.global_scheduled_test_release

  (* Use this function to get the release tag regular expression,
     as it makes sure that it is registered with [Hooks.release_tags] only once. *)
  let get_release_tag_rex =
    let tag = ref None in
    fun () ->
      match !tag with
      | Some tag -> tag
      | None ->
          let result =
            "/^" ^ String.lowercase_ascii Component.name ^ "-v\\d+\\.\\d+$/"
          in
          tag := Some result ;
          Tezos_ci.Hooks.release_tags := result :: !Tezos_ci.Hooks.release_tags ;
          result

  (* Wrap a function with this function to make sure it is called only once. *)
  let only_once name f =
    let already_called = ref false in
    fun x ->
      if !already_called then
        error __POS__ "component %s called %s twice" Component.name name ;
      already_called := true ;
      f x

  let register_dedicated_release_pipeline =
    only_once "register_dedicated_release_pipeline" @@ fun jobs ->
    let release_tag_rex = get_release_tag_rex () in
    register_pipeline
      "release"
      ~description:(sf "Release %s." Component.name)
      ~jobs:(convert_jobs ~with_changes:false jobs)
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          on_tezos_namespace && push && has_tag_match release_tag_rex))

  let register_dedicated_test_release_pipeline =
    only_once "register_dedicated_test_release_pipeline" @@ fun jobs ->
    let release_tag_rex = get_release_tag_rex () in
    register_pipeline
      "test_release"
      ~description:(sf "Release %s (test)." Component.name)
      ~jobs:(convert_jobs ~with_changes:false jobs)
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          not_on_tezos_namespace && push && has_tag_match release_tag_rex))
end
