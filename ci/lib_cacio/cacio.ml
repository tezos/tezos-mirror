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
}

let sccache ?key ?error_log ?idle_timeout ?log () =
  {key; error_log; idle_timeout; log}

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
                | Some {key; error_log; idle_timeout; log} ->
                    Tezos_ci.Cache.enable_sccache
                      ?key
                      ?error_log
                      ?idle_timeout
                      ?log
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

  type tezt_timeout = No_timeout | Minutes of int

  val tezt_job :
    pipeline:[`merge_request | `scheduled] ->
    description:string ->
    ?provider:Tezos_ci.Runner.Provider.t ->
    ?arch:Tezos_ci.Runner.Arch.t ->
    ?cpu:Tezos_ci.Runner.CPU.t ->
    ?storage:Tezos_ci.Runner.Storage.t ->
    ?fetch_records_from:string ->
    ?only_if_changed:string list ->
    ?needs:(need * job) list ->
    ?needs_legacy:(need * Tezos_ci.tezos_job) list ->
    ?test_coverage:bool ->
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

(* Some jobs are to be added to shared pipelines.
   We must only call [convert_jobs] once we know all of them,
   otherwise some jobs (such as [select_tezts]) could be added twice. *)
let before_merging_jobs = ref []

let get_before_merging_jobs () =
  convert_jobs ~with_condition:true !before_merging_jobs

let schedule_extended_test_jobs = ref []

let get_schedule_extended_test_jobs () =
  convert_jobs
    ~interruptible_pipeline:false
    ~with_condition:false
    !schedule_extended_test_jobs

let custom_extended_test_jobs = ref []

let get_custom_extended_test_jobs () =
  convert_jobs ~with_condition:false !custom_extended_test_jobs

let master_jobs = ref []

let get_master_jobs () = convert_jobs ~with_condition:false !master_jobs

let global_release_jobs = ref []

let get_global_release_jobs () =
  convert_jobs ~with_condition:false !global_release_jobs

let global_test_release_jobs = ref []

let get_global_test_release_jobs () =
  convert_jobs ~with_condition:false !global_test_release_jobs

let global_scheduled_test_release_jobs = ref []

let get_global_scheduled_test_release_jobs () =
  convert_jobs ~with_condition:false !global_scheduled_test_release_jobs

let global_publish_release_page_jobs = ref []

let get_global_publish_release_page_jobs () =
  convert_jobs ~with_condition:false !global_publish_release_page_jobs

let global_test_publish_release_page_jobs = ref []

let get_global_test_publish_release_page_jobs () =
  convert_jobs ~with_condition:false !global_test_publish_release_page_jobs

let release_tag_rexes = ref []

let get_release_tag_rexes () = !release_tag_rexes

(* [job_select_tezts] will be initialized further down. *)
let job_select_tezts = ref None

(* We could avoid using a functor if we required the user of this module
   to pass the component's [name] and [paths] to the functions that need them,
   but in practice this would be less convenient since all functions need at least
   one of them. *)
module Make (Component : COMPONENT) : COMPONENT_API = struct
  let default_only_if_changed = Tezos_ci.Changeset.make Component.paths

  (* Users of Cacio just specify a [string] for the [name];
     we don't want them to provide a [string option].
     But in this module, we want the type-checker to force us to check
     whether the name is empty (for the [Shared] module).

     Another solution would be to define two functors:
     - one where the name is optional;
     - a second one where the name is mandatory;
     and to only expose the second one.
     This would encode, in types, the fact that there is a [Shared] component.
     But it would be heavier, and types would still not exactly capture
     the intent, since the empty name would become an invalid value. *)
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

  (* This job is allowed to fail because Tezt can still run without records,
     or with records that are committed in the repository. *)
  let job_tezt_fetch_records =
    parameterize @@ fun from_pipeline ->
    job
      ("teztrec_" ^ from_pipeline)
      ~__POS__
      ~description:
        "Fetch Tezt records so that jobs that run Tezt tests can use them for \
         auto-balancing."
      ~image:Tezos_ci.Images.CI.build
      ~stage:Build
      ~allow_failure:Yes
      ~artifacts:
        (Gitlab_ci.Util.artifacts
           ~expire_in:(Duration (Hours 4))
           ~when_:Always
           [
             "tezt-fetch-records.log";
             "tezt/records/**.json";
             (* Keep broken records for debugging *)
             "tezt/records/**.json.broken";
           ])
      [
        "./scripts/ci/take_ownership.sh";
        ". ./scripts/version.sh";
        "eval $(opam env)";
        "dune exec scripts/ci/update_records/update.exe -- --info --log-file \
         tezt-fetch-records.log --from last-successful-scheduled:"
        ^ from_pipeline;
        "./scripts/ci/filter_corrupted_records.sh";
      ]

  module SH = struct
    (* Mini-module to help craft large shell commands. *)

    (* [arguments] is a list of list.
       For instance, it can be [["--verbose"]; ["--log-file"; "logs.txt"]].
       Using a list of list instead of a list allows to
       group related arguments together without ocamlformat ungrouping them.
       It also allows to easily insert optional arguments.

       This does not quote because we sometimes want to insert variables.
       So, use with caution. *)
    let command executable arguments =
      String.concat " " (executable :: List.flatten arguments)

    let redirect_stdout_to path command = command ^ " > " ^ path

    let wrap_with executable arguments cmd =
      command executable arguments ^ " " ^ cmd

    let quote = Filename.quote
  end

  type tezt_timeout = No_timeout | Minutes of int

  let tezt_job ~pipeline ~description ?provider ?arch
      ?(cpu = Tezos_ci.Runner.CPU.Tezt) ?storage ?fetch_records_from
      ?only_if_changed ?(needs = []) ?needs_legacy ?test_coverage ?allow_failure
      ?tezt_exe ?(global_timeout = Minutes 30) ?(test_timeout = Minutes 9)
      ?(parallel_jobs = 1) ?(parallel_tests = 1) ?retry_jobs ?(retry_tests = 0)
      ?(test_selection = Tezt_core.TSL_AST.True) ?(before_script = []) variant =
    let select_tezts =
      match pipeline with `merge_request -> true | `scheduled -> false
    in
    let record_dir =
      let dir =
        match Component.name with
        | None -> "tezt/records"
        | Some name -> "tezt/records" // name
      in
      if variant = "" then dir else dir // variant
    in
    let variables =
      [
        (* The following variables are only used in the YAML.
           We can inline them later but for now the diff with the old Tezt.job function
           is easier to review with them. *)
        ("JUNIT", "tezt-junit.xml");
        ("TEZT_VARIANT", if variant = "" then "" else "-" ^ variant);
        ("TESTS", Tezt_core.TSL.show test_selection);
        ("TEZT_RETRY", string_of_int retry_tests);
        ("TEZT_PARALLEL", string_of_int parallel_tests);
        (* The following variable must be an environment variable
           because it is not only used in the YAML. *)
        ("TEZT_NO_NPX", "true");
      ]
    in
    let variables_to_echo =
      (* Later this could be computed from [variables], but for now we try to reduce
         the diff as much as possible to ease reviewing. *)
      [
        "TESTS";
        "JUNIT";
        "CI_NODE_INDEX";
        "CI_NODE_TOTAL";
        "TEZT_PARALLEL";
        "TEZT_VARIANT";
      ]
    in
    let cmd_echo_variables =
      let message =
        variables_to_echo
        |> List.map (fun var -> sf {|%s=\"${%s}\"|} var var)
        |> String.concat " "
      in
      "echo \"" ^ message ^ "\""
    in
    (* It would be a good idea to define the arguments that control test selection
       only once, to share the code for both [tezt.sh] invocations,
       as it is important that exactly the same tests are selected.
       But this makes the diff for the merge request that introduces this function
       harder to read, so for now we don't. *)
    let cmd_store_list_of_selected_tests =
      SH.command
        "./scripts/ci/tezt.sh"
        [
          (match tezt_exe with
          | None -> []
          | Some path -> ["--tezt-exe"; SH.quote path]);
          [
            (if select_tezts then "--with-select-tezts"
             else "--without-select-tezts");
          ];
          ["--"];
          ["\"${TESTS}\""];
          [
            "--from-record";
            (* Ideally we woud use SH.quote here.
               For now we don't because it makes the diff harder to read. *)
            record_dir;
          ];
          ["--job"; "${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1}"];
          ["--list-tsv"];
        ]
      |> SH.redirect_stdout_to "selected_tezts.tsv"
    in
    let cmd_run_tests =
      let junit_tags =
        (* List of tags to include in JUnit reports that we send to DataDog. *)
        [
          (* Tags that change the job that runs the test.
             Useful to be able to filter on only a specific job type in DataDog. *)
          "flaky";
          "time_sensitive";
          "slow";
          "extra";
          (* Tags that denote the owner of tests.
             Useful in case we want to send alerts to specific teams. *)
          "infrastructure";
          "layer1";
          "tezos2";
          "etherlink";
          (* Tags that change alert thresholds. *)
          "memory_hungry";
        ]
      in
      SH.command
        "./scripts/ci/tezt.sh"
        [
          ["--send-junit"; "${JUNIT}"];
          (match tezt_exe with
          | None -> []
          | Some path -> ["--tezt-exe"; SH.quote path]);
          [
            (if select_tezts then "--with-select-tezts"
             else "--without-select-tezts");
          ];
          ["--"];
          ["\"${TESTS}\""];
          ["--color"];
          ["--log-buffer-size"; "5000"];
          ["--log-file"; "tezt.log"];
          (match global_timeout with
          | No_timeout -> []
          | Minutes m -> ["--global-timeout"; string_of_int (60 * m)]);
          (match test_timeout with
          | No_timeout -> []
          | Minutes m -> ["--test-timeout"; string_of_int (60 * m)]);
          ["--on-unknown-regression-files"; "fail"];
          ["--junit"; "${JUNIT}"];
          ["--junit-mem-peak"; SH.quote "dd_tags[memory.peak]"];
          [
            "--from-record";
            (* Ideally we woud use SH.quote here.
               For now we don't because it makes the diff harder to read. *)
            record_dir;
          ];
          ["--job"; "${CI_NODE_INDEX:-1}/${CI_NODE_TOTAL:-1}"];
          ["--record"; "tezt-results.json"];
          ["--job-count"; "${TEZT_PARALLEL}"];
          ["--retry"; "${TEZT_RETRY}"];
          ["--record-mem-peak"];
          ["--mem-warn"; "5_000_000_000"];
          (match pipeline with
          | `scheduled -> ["--keep-going"]
          | `merge_request -> []);
          ( Fun.flip List.concat_map junit_tags @@ fun tag ->
            ["--junit-tag"; Printf.sprintf "'dd_tags[tezt-tag.%s]=%s'" tag tag]
          );
        ]
    in
    let wrap_with_timeout command =
      (* Wrapping the Tezt call with the [timeout] command allows to avoid GitLab timeouts.
         The latter cause artifacts to be lost.
         See https://gitlab.com/gitlab-org/gitlab/-/issues/19818. *)
      match global_timeout with
      | No_timeout -> command
      | Minutes m ->
          SH.wrap_with
            "timeout"
            [["-k"; "60"]; [string_of_int (60 * (m + 1))]]
            command
    in
    let wrap_with_exit_code = SH.wrap_with "./scripts/ci/exit_code.sh" [] in
    let needs =
      if select_tezts then
        match !job_select_tezts with
        | None -> failwith "job_select_tezts has not been initialized"
        | Some job -> (Artifacts, job) :: needs
      else needs
    in
    let needs =
      match fetch_records_from with
      | None -> needs
      | Some pipeline_name ->
          (Artifacts, job_tezt_fetch_records pipeline_name) :: needs
    in
    job
      (if variant = "" then "tezt" else "tezt-" ^ variant)
      ~__POS__
      ~stage:Test
      ~description
      ?provider
      ?arch
      ~cpu
      ?storage
      ~image:Tezos_ci.Images.CI.e2etest
      ?only_if_changed
      ~needs
      ?needs_legacy
      ?parallel:
        (if parallel_jobs > 1 then Some (Vector parallel_jobs) else None)
      ~variables
      ~artifacts:
        (Gitlab_ci.Util.artifacts
           ~reports:(Gitlab_ci.Util.reports ~junit:"$JUNIT" ())
           [
             "selected_tezts.tsv";
             "tezt.log";
             "tezt-*.log";
             "tezt-results.json";
             "$JUNIT";
           ]
           ~expire_in:(Duration (Days 7))
           ~when_:Always)
      ?test_coverage
      ?allow_failure
      ?retry:
        (match retry_jobs with
        | None -> None
        | Some max -> Some {max; when_ = []})
      ?timeout:
        (match global_timeout with
        | No_timeout -> None
        | Minutes m -> Some (Minutes (m + 10)))
      (before_script
      @ [
          ". ./scripts/version.sh";
          cmd_echo_variables;
          cmd_store_list_of_selected_tests;
          wrap_with_exit_code (wrap_with_timeout cmd_run_tests);
        ])

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
    before_merging_jobs := jobs @ !before_merging_jobs

  let register_schedule_extended_test_jobs jobs =
    match Component.name with
    | Some _ ->
        failwith
          "register_schedule_extended_test_jobs can only be used from the \
           Shared component; regular components should define their own \
           schedule pipelines with register_scheduled_pipeline"
    | None -> schedule_extended_test_jobs := jobs @ !schedule_extended_test_jobs

  let register_custom_extended_test_jobs jobs =
    match Component.name with
    | Some _ ->
        failwith
          "register_custom_extended_test_jobs can only be used from the Shared \
           component to migrate old custom extended test jobs"
    | None -> custom_extended_test_jobs := jobs @ !custom_extended_test_jobs

  let register_master_jobs jobs = master_jobs := jobs @ !master_jobs

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
    global_release_jobs := jobs @ !global_release_jobs

  let register_global_test_release_jobs jobs =
    global_test_release_jobs := jobs @ !global_test_release_jobs

  let register_global_scheduled_test_release_jobs jobs =
    global_scheduled_test_release_jobs :=
      jobs @ !global_scheduled_test_release_jobs

  let register_global_publish_release_page_jobs jobs =
    global_publish_release_page_jobs := jobs @ !global_publish_release_page_jobs

  let register_global_test_publish_release_page_jobs jobs =
    global_test_publish_release_page_jobs :=
      jobs @ !global_test_publish_release_page_jobs

  (* Use this function to get the release tag regular expression,
     as it makes sure that it is registered only once. *)
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
          release_tag_rexes := result :: !release_tag_rexes ;
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

(* Initialize [job_select_tezts]. *)
let () =
  (* This job needs the following executables from its [~image]:
     - Git (to run git diff)
     - ocamlyacc, ocamllex and ocamlc (to build manifest/manifest) *)
  job_select_tezts :=
    Some
      (Shared.job
         "select_tezts"
         ~__POS__
         ~description:"Run Manifezt to select the set of Tezt tests to run."
         ~image:Tezos_ci.Images.CI.prebuild
         ~stage:Build
         ~artifacts:
           (Gitlab_ci.Util.artifacts
              ~expire_in:(Duration (Days 3))
              ~when_:Always
              ["selected_tezts.tsl"])
         ~allow_failure:(With_exit_codes [17])
         [
           "./scripts/ci/take_ownership.sh";
           "eval $(opam env)";
           "scripts/ci/select_tezts.sh || exit $?";
         ])
