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

type sccache_config = {
  key : string option;
  error_log : string option;
  idle_timeout : string option;
  log : string option;
  path : string option;
  cache_size : string option;
}

let sccache ?key ?error_log ?idle_timeout ?log ?path ?cache_size () =
  {key; error_log; idle_timeout; log; path; cache_size}

type dune_cache_config = {
  key : string option;
  path : string option;
  cache_size : string option;
  copy_mode : bool option;
  policy : Gitlab_ci.Types.cache_policy option;
}

let dune_cache ?key ?path ?cache_size ?copy_mode ?policy () =
  {key; path; cache_size; copy_mode; policy}

(* Conditions are disjunctions: the job is included in the pipeline if
   ANY file in [changed] changed, or if the merge request has ANY of the [label]s. *)
type condition = {changed : Tezos_ci.Changeset.t; label : String_set.t}

let merge_conditions {changed = changed1; label = label1}
    {changed = changed2; label = label2} =
  {
    changed = Tezos_ci.Changeset.union changed1 changed2;
    label = String_set.union label1 label2;
  }

type job = {
  uid : int;
  source_location : string * int * int * int;
  name : string;
  stage : stage;
  description : string;
  provider : Tezos_ci.Runner.Provider.t option;
  arch : Tezos_ci.Runner.Arch.t option;
  cpu : Tezos_ci.Runner.CPU.t option;
  storage : Tezos_ci.Runner.Storage.t option;
  image : Tezos_ci.Image.t;
  needs : (need * job) list;
  needs_legacy : (need * Tezos_ci.tezos_job) list;
  parallel : Gitlab_ci.Types.parallel option;
  only_if : condition;
  variables : Gitlab_ci.Types.variables option;
  script : string list;
  artifacts : Gitlab_ci.Types.artifacts option;
  cargo_cache : bool;
  cargo_target_caches : bool;
  sccache : sccache_config option;
  dune_cache : dune_cache_config option;
  test_coverage : bool;
  allow_failure : Gitlab_ci.Types.allow_failure_job option;
  retry : Gitlab_ci.Types.retry option;
  timeout : Gitlab_ci.Types.time_interval option;
  image_dependencies : Tezos_ci.Image.t list;
  services : Gitlab_ci.Types.service list option;
}

type trigger = Auto | Immediate | Manual

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
   This function fixes triggers and conditions.
   By "fix" we mean not only that we set them to corrected values,
   but also that these corrected values are a fixpoint.
   More precisely:
   - triggers are fixed so that if a job has trigger [Auto],
     then all of its dependencies also have trigger [Auto];
   - conditions are fixed so that if a condition causes a job to be included,
     then this condition also causes all dependencies of this job to be included.

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

type fixed_job_graph_node = {job : job; trigger : trigger; only_if : condition}

type fixed_job_graph = fixed_job_graph_node UID_map.t

(* Compute the final values for conditions and [trigger]s.
   Assumes there are no cycles.
   See GRAPH TRANSFORMATIONS above (step 3). *)
let fix_graph (graph : job_graph) : fixed_job_graph =
  (* To build the graph, we take all jobs from [graph], fix them,
     and add them to [result].
     But before we fix a job, we need the [trigger] and [only_if]
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
                  | Immediate, (Auto | Immediate | Manual)
                  | (Auto | Manual), Immediate ->
                      Immediate
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
              let only_if =
                rev_deps
                |> List.map (fun node -> node.only_if)
                |> List.fold_left merge_conditions job.only_if
              in
              {job; trigger; only_if}
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

let convert_stage (trigger : trigger) (stage : stage) : Tezos_ci.Stage.t =
  match (stage, trigger) with
  | Build, _ -> Tezos_ci.Stages.build
  | Test, Immediate ->
      (* In the future, we plan to remove the sanity stage.
         Hence why we do not expose a [Sanity] constructor in type [stage].
         But for now, immediate test jobs are still supposed to be in the sanity stage. *)
      Tezos_ci.Stages.sanity
  | Test, _ -> Tezos_ci.Stages.test
  | Publish, _ -> Tezos_ci.Stages.publish

type tezos_job_graph = Tezos_ci.tezos_job UID_map.t

(* Convert jobs to [Tezos_ci] jobs.
   See GRAPH TRANSFORMATIONS above (step 4).

   By default, [Publish] jobs are non-interruptible and other jobs are interruptible.
   Setting [interruptible_pipeline] to [false] makes all jobs non-interruptible.

   [Publish] jobs use non-interruptible runners.
   Other jobs may use any runner (interruptible or not).
   [interruptible_pipeline] has no impact on the choice of runners.

   If [with_condition] is [true], the job [rules] will include its conditions.
   If it is [false], its conditions are ignored.
   Conditions are typically only used in merge request pipelines such as [before_merging]. *)
let convert_graph ?(interruptible_pipeline = true) ~with_condition
    (graph : fixed_job_graph) : tezos_job_graph =
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
                    provider;
                    arch;
                    cpu;
                    storage;
                    image;
                    needs;
                    needs_legacy;
                    parallel;
                    only_if = _;
                    variables;
                    script;
                    artifacts;
                    cargo_cache;
                    cargo_target_caches;
                    sccache;
                    dune_cache;
                    test_coverage;
                    allow_failure;
                    retry;
                    timeout;
                    image_dependencies;
                    services;
                  };
                trigger;
                only_if;
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
                 whether we actually want the condition ([with_condition]),
                 and the [trigger]. *)
              let rules =
                if with_condition then
                  let when_ : Gitlab_ci.Types.when_ =
                    match trigger with
                    | Auto | Immediate -> On_success
                    | Manual -> Manual
                  in
                  let labels =
                    match String_set.elements only_if.label with
                    | [] -> []
                    | first_label :: other_labels ->
                        [
                          Gitlab_ci.Util.job_rule
                            ~if_:
                              (List.fold_left
                                 (fun acc label ->
                                   Gitlab_ci.If.(
                                     acc || Tezos_ci.Rules.has_mr_label label))
                                 (Tezos_ci.Rules.has_mr_label first_label)
                                 other_labels)
                            ~when_
                            ();
                        ]
                  in
                  let changes =
                    [
                      Gitlab_ci.Util.job_rule
                        ~changes:(Tezos_ci.Changeset.encode only_if.changed)
                        ~when_
                        ();
                    ]
                  in
                  Some (labels @ changes)
                else
                  match trigger with
                  | Auto | Immediate -> None
                  | Manual -> Some [Gitlab_ci.Util.job_rule ~when_:Manual ()]
              in
              let interruptible_stage =
                match stage with Build | Test -> true | Publish -> false
              in
              let retry : Gitlab_ci.Types.retry option =
                match retry with
                | Some _ -> retry
                | None -> (
                    match stage with
                    | Build | Test -> None
                    | Publish -> Some {max = 0; when_ = []})
              in
              let dev_infra =
                match provider with
                | None | Some GCP | Some AWS -> false
                | Some GCP_dev -> true
              in
              let maybe_enable_cargo_cache job =
                if cargo_cache then Tezos_ci.Cache.enable_cargo_cache job
                else job
              in
              let maybe_enable_cargo_target_caches job =
                if cargo_target_caches then
                  Tezos_ci.Cache.enable_cargo_target_caches job
                else job
              in
              let maybe_enable_sccache job =
                match sccache with
                | None -> job
                | Some {key; error_log; idle_timeout; log; path; cache_size} ->
                    Tezos_ci.Cache.enable_sccache
                      ?key
                      ?error_log
                      ?idle_timeout
                      ?log
                      ?path
                      ?cache_size
                      job
              in
              let maybe_enable_dune_cache job =
                match dune_cache with
                | None -> job
                | Some {key; path; cache_size; copy_mode; policy} ->
                    Tezos_ci.Cache.enable_dune_cache
                      ?key
                      ?path
                      ?cache_size
                      ?copy_mode
                      ?policy
                      job
              in
              let maybe_enable_test_coverage job =
                if test_coverage then
                  job |> Tezos_ci.Coverage.enable_instrumentation
                  |> Tezos_ci.Coverage.enable_output_artifact
                else job
              in
              Tezos_ci.job
                ~__POS__:source_location
                ~name
                ~stage:(convert_stage trigger stage)
                ~description
                ~dev_infra
                ?arch
                ?cpu
                ?storage
                ~image
                ~image_dependencies
                ~dependencies:(Dependent dependencies)
                ?parallel
                ?rules
                ?services
                ~interruptible:(interruptible_stage && interruptible_pipeline)
                ?interruptible_runner:
                  (if interruptible_stage then
                     (* Can be interruptible, or not. *)
                     None
                   else (* Cannot be interruptible. *)
                     Some false)
                ?retry
                ?timeout
                ?variables
                ?artifacts
                ?allow_failure
                script
              |> maybe_enable_cargo_cache |> maybe_enable_cargo_target_caches
              |> maybe_enable_sccache |> maybe_enable_dune_cache
              |> maybe_enable_test_coverage
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
let convert_jobs ?interruptible_pipeline ~with_condition
    (jobs : (trigger * job) list) : Tezos_ci.tezos_job list =
  jobs |> make_graph |> fix_graph
  |> convert_graph ?interruptible_pipeline ~with_condition
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
    ?provider:Tezos_ci.Runner.Provider.t ->
    ?arch:Tezos_ci.Runner.Arch.t ->
    ?cpu:Tezos_ci.Runner.CPU.t ->
    ?storage:Tezos_ci.Runner.Storage.t ->
    image:Tezos_ci.Image.t ->
    ?only_if_changed:string list ->
    ?force_if_label:string list ->
    ?needs:(need * job) list ->
    ?needs_legacy:(need * Tezos_ci.tezos_job) list ->
    ?parallel:Gitlab_ci.Types.parallel ->
    ?variables:Gitlab_ci.Types.variables ->
    ?artifacts:Gitlab_ci.Types.artifacts ->
    ?cargo_cache:bool ->
    ?cargo_target_caches:bool ->
    ?sccache:sccache_config ->
    ?dune_cache:dune_cache_config ->
    ?test_coverage:bool ->
    ?allow_failure:Gitlab_ci.Types.allow_failure_job ->
    ?retry:Gitlab_ci.Types.retry ->
    ?timeout:Gitlab_ci.Types.time_interval ->
    ?image_dependencies:Tezos_ci.Image.t list ->
    ?services:Gitlab_ci.Types.service list ->
    string ->
    string list ->
    job

  val register_before_merging_jobs : (trigger * job) list -> unit

  val register_schedule_extended_test_jobs : (trigger * job) list -> unit

  val register_custom_extended_test_jobs : (trigger * job) list -> unit

  val register_master_jobs : (trigger * job) list -> unit

  val register_scheduled_pipeline :
    description:string ->
    ?legacy_jobs:Tezos_ci.tezos_job list ->
    string ->
    (trigger * job) list ->
    unit

  val register_global_release_jobs : (trigger * job) list -> unit

  val register_global_test_release_jobs : (trigger * job) list -> unit

  val register_global_scheduled_test_release_jobs : (trigger * job) list -> unit

  val register_global_publish_release_page_jobs : (trigger * job) list -> unit

  val register_global_test_publish_release_page_jobs :
    (trigger * job) list -> unit

  val register_dedicated_release_pipeline : (trigger * job) list -> unit

  val register_dedicated_test_release_pipeline : (trigger * job) list -> unit
end

(* We could avoid using a functor if we required the user of this module
   to pass the component's [name] and [paths] to the functions that need them,
   but in practice this would be less convenient since all functions need at least
   one of them. *)
module Make (Component : COMPONENT) : COMPONENT_API = struct
  let default_only_if_changed = Tezos_ci.Changeset.make Component.paths

  (* Users of Cacio just specify a [string] for the [name];
     we don't want them to provide a [string option].
     But in this module, we want the type-checker to force us to check
     whether the name is empty (for the [Shared] module). *)
  module Component = struct
    include Component

    let name = if name = "" then None else Some name
  end

  let make_name name =
    match Component.name with
    | None -> name
    | Some component -> component ^ "." ^ name

  let job ~__POS__:source_location ~stage ~description ?provider ?arch ?cpu
      ?storage ~image ?only_if_changed ?(force_if_label = []) ?(needs = [])
      ?(needs_legacy = []) ?parallel ?variables ?artifacts
      ?(cargo_cache = false) ?(cargo_target_caches = false) ?sccache ?dune_cache
      ?(test_coverage = false) ?allow_failure ?retry ?timeout
      ?(image_dependencies = []) ?services name script =
    let name = make_name name in
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
      provider;
      arch;
      cpu;
      storage;
      image;
      needs;
      needs_legacy;
      parallel;
      only_if =
        {
          changed =
            (match only_if_changed with
            | None -> default_only_if_changed
            | Some list -> Tezos_ci.Changeset.make list);
          label = String_set.of_list force_if_label;
        };
      variables;
      script;
      artifacts;
      cargo_cache;
      cargo_target_caches;
      sccache;
      dune_cache;
      test_coverage;
      allow_failure;
      retry;
      timeout;
      image_dependencies;
      services;
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
      Fun.flip List.map jobs @@ fun (trigger, job) ->
      match trigger with
      | Immediate -> (trigger, job)
      | Auto | Manual ->
          let job =
            {job with needs_legacy = (Job, job_trigger) :: job.needs_legacy}
          in
          (trigger, job)
    in
    let jobs = convert_jobs ~with_condition:true jobs in
    Tezos_ci.Hooks.before_merging := jobs @ !Tezos_ci.Hooks.before_merging

  let register_schedule_extended_test_jobs jobs =
    match Component.name with
    | Some _ ->
        failwith
          "register_schedule_extended_test_jobs can only be used from the \
           Shared component; regular components should define their own \
           schedule pipelines with register_scheduled_pipeline"
    | None ->
        let jobs = convert_jobs ~with_condition:false jobs in
        Tezos_ci.Hooks.schedule_extended_test :=
          jobs @ !Tezos_ci.Hooks.schedule_extended_test

  let register_custom_extended_test_jobs jobs =
    match Component.name with
    | Some _ ->
        failwith
          "register_custom_extended_test_jobs can only be used from the Shared \
           component to migrate old custom extended test jobs"
    | None ->
        let jobs = convert_jobs ~with_condition:false jobs in
        Tezos_ci.Hooks.custom_extended_test :=
          jobs @ !Tezos_ci.Hooks.custom_extended_test

  let register_master_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.master := jobs @ !Tezos_ci.Hooks.master

  (* Helper for other functions below, that is not exposed to users of this module.
     It is responsible for:
     - prefixing the name of the pipeline with the name of the component;
     - including the DataDog job.
     Returns the pipeline name. *)
  let register_pipeline ~description ~jobs name rules =
    Tezos_ci.Pipeline.register
      ~description
      ~jobs:(Tezos_ci.job_datadog_pipeline_trace :: jobs)
      (make_name name)
      rules

  let register_scheduled_pipeline ~description ?(legacy_jobs = []) name jobs =
    (* Scheduled pipelines are non-interruptible:
       we don't want them to be canceled just because
       a new commit was merged into master. *)
    register_pipeline
      name
      ~description
      ~jobs:
        (legacy_jobs
        @ convert_jobs ~interruptible_pipeline:false ~with_condition:false jobs
        )
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          scheduled && var "TZ_SCHEDULE_KIND" == str (make_name name)))

  let register_global_release_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.global_release := jobs @ !Tezos_ci.Hooks.global_release

  let register_global_test_release_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.global_test_release :=
      jobs @ !Tezos_ci.Hooks.global_test_release

  let register_global_scheduled_test_release_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.global_scheduled_test_release :=
      jobs @ !Tezos_ci.Hooks.global_scheduled_test_release

  let register_global_publish_release_page_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.global_publish_release_page :=
      jobs @ !Tezos_ci.Hooks.global_publish_release_page

  let register_global_test_publish_release_page_jobs jobs =
    let jobs = convert_jobs ~with_condition:false jobs in
    Tezos_ci.Hooks.global_test_publish_release_page :=
      jobs @ !Tezos_ci.Hooks.global_test_publish_release_page

  (* Use this function to get the release tag regular expression,
     as it makes sure that it is registered with [Hooks.release_tags] only once. *)
  let get_release_tag_rex =
    let tag = ref None in
    fun component_name ->
      match !tag with
      | Some tag -> tag
      | None ->
          let result =
            "/^" ^ String.lowercase_ascii component_name ^ "-v\\d+\\.\\d+$/"
          in
          tag := Some result ;
          Tezos_ci.Hooks.release_tags := result :: !Tezos_ci.Hooks.release_tags ;
          result

  (* Wrap a function with this function to make sure
     the component is an actual component, not [Shared]. *)
  let component_must_not_be_shared name f =
    match Component.name with
    | None -> failwith (name ^ ": not implemented for the Shared component")
    | Some name -> f name

  (* Wrap a function with this function to make sure it is called only once. *)
  let only_once name f =
    let already_called = ref false in
    fun x ->
      if !already_called then
        error
          __POS__
          "component %s called %s twice"
          (match Component.name with None -> "Shared" | Some name -> name)
          name ;
      already_called := true ;
      f x

  let register_dedicated_release_pipeline =
    only_once "register_dedicated_release_pipeline" @@ fun jobs ->
    component_must_not_be_shared "register_dedicated_release_pipeline"
    @@ fun component_name ->
    let release_tag_rex = get_release_tag_rex component_name in
    register_pipeline
      "release"
      ~description:(sf "Release %s." component_name)
      ~jobs:(convert_jobs ~with_condition:false jobs)
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          on_tezos_namespace && push && has_tag_match release_tag_rex))

  let register_dedicated_test_release_pipeline =
    only_once "register_dedicated_test_release_pipeline" @@ fun jobs ->
    component_must_not_be_shared "register_dedicated_release_pipeline"
    @@ fun component_name ->
    let release_tag_rex = get_release_tag_rex component_name in
    register_pipeline
      "test_release"
      ~description:(sf "Release %s (test)." component_name)
      ~jobs:(convert_jobs ~with_condition:false jobs)
      Tezos_ci.Rules.(
        Gitlab_ci.If.(
          not_on_tezos_namespace && push && has_tag_match release_tag_rex))
end

module Shared = Make (struct
  (* The empty name causes common jobs to not be prefixed by a component name. *)
  let name = ""

  let paths = []
end)
