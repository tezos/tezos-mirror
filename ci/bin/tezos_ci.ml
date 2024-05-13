open Gitlab_ci.Util

module Cli = struct
  type config = {
    mutable verbose : bool;
    mutable inline_source_info : bool;
    mutable remove_extra_files : bool;
  }

  let config =
    {verbose = false; inline_source_info = false; remove_extra_files = false}

  let verbose fmt =
    Format.kasprintf (if config.verbose then print_endline else fun _ -> ()) fmt

  let has_error = ref false

  let error fmt =
    Format.kasprintf
      (fun s ->
        has_error := true ;
        prerr_endline s)
      fmt

  let info fmt = Format.kasprintf print_endline fmt

  let init () =
    let speclist =
      Arg.align
        [
          ( "--verbose",
            Arg.Unit (fun () -> config.verbose <- true),
            " Show debug output, including the location of each generated job.."
          );
          ( "--inline-source-info",
            Arg.Unit (fun () -> config.inline_source_info <- true),
            " Comment each generated job with source information." );
          ( "--remove-extra-files",
            Arg.Unit (fun () -> config.remove_extra_files <- true),
            " Remove files that are neither generated nor excluded." );
        ]
    in
    Arg.parse
      speclist
      (fun s ->
        Arg.(usage speclist (sf "No anonymous arguments (got %s)" s)) ;
        exit 1)
      (sf "Usage: %s [options]\n\nOptions are:" Sys.argv.(0))
end

type tezos_job = {
  job : Gitlab_ci.Types.job;
  source_position : string * int * int * int;
}

let name_of_tezos_job tezos_job = tezos_job.job.name

let map_job (tezos_job : tezos_job)
    (f : Gitlab_ci.Types.job -> Gitlab_ci.Types.job) : tezos_job =
  {tezos_job with job = f tezos_job.job}

let tezos_job_to_config_elements (j : tezos_job) =
  let source_comment =
    if Cli.config.inline_source_info then
      let file, line, _, _ = j.source_position in
      let source_info = sf "(generated from %s:%d)" file line in
      [Gitlab_ci.Types.Comment source_info]
    else []
  in
  source_comment @ [Gitlab_ci.Types.Job j.job]

let failwith fmt = Format.kasprintf (fun s -> failwith s) fmt

let header =
  {|# This file was automatically generated, do not edit.
# Edit file ci/bin/main.ml instead.

|}

let generated_files = ref String_set.empty

let to_file ~filename config =
  if String_set.mem filename !generated_files then
    failwith "Attempted to write file %s twice." filename
  else (
    generated_files := String_set.add filename !generated_files ;
    Gitlab_ci.To_yaml.to_file ~header ~filename config)

let () = Printexc.register_printer @@ function Failure s -> Some s | _ -> None

module Stage = struct
  type t = Stage of string

  let stages : t list ref = ref []

  let register name =
    let stage = Stage name in
    if List.mem stage !stages then
      failwith "[Stage.register] attempted to register stage %S twice" name
    else (
      stages := stage :: !stages ;
      stage)

  let name (Stage name) = name

  let to_string_list () = List.map name (List.rev !stages)
end

module Pipeline = struct
  type pipeline = {
    name : string;
    if_ : Gitlab_ci.If.t;
    variables : Gitlab_ci.Types.variables option;
    jobs : tezos_job list;
  }

  type child_pipeline = {name : string; jobs : tezos_job list}

  type t = Pipeline of pipeline | Child_pipeline of child_pipeline

  let pipelines : t list ref = ref []

  let name = function Pipeline {name; _} | Child_pipeline {name; _} -> name

  let jobs = function
    | Child_pipeline {jobs; _} -> jobs
    | Pipeline {jobs; _} -> jobs

  let path : name:string -> string =
   fun ~name -> sf ".gitlab/ci/pipelines/%s.yml" name

  let register_raw pipeline =
    if List.exists (fun pipeline' -> name pipeline = name pipeline') !pipelines
    then
      failwith
        "[Pipeline.register] attempted to register pipeline %S twice"
        (name pipeline)
    else pipelines := pipeline :: !pipelines

  let register ?variables ~jobs name if_ =
    register_raw (Pipeline {variables; if_; name; jobs})

  let register_child ~jobs name =
    let child_pipeline = {name; jobs} in
    register_raw (Child_pipeline child_pipeline) ;
    child_pipeline

  let all () = List.rev !pipelines

  (* Perform a set of static checks on the full pipeline before writing it. *)
  let precheck pipeline =
    let pipeline_name = name pipeline in
    let jobs = jobs pipeline in
    (* TODO: Have to figure out:
       - is it possible to have a [needs:] on a trigger job?
       - is it possible to get artifacts from a trigger job (i.e. like from it's child pipeline?) *)
    let job_by_name : (string, Gitlab_ci.Types.job) Hashtbl.t =
      Hashtbl.create 5
    in
    (* Populate [job_by_name] and check that no two different jobs have the same name. *)
    List.iter
      (fun ({job; _} : tezos_job) ->
        match Hashtbl.find_opt job_by_name job.name with
        | None -> Hashtbl.add job_by_name job.name job
        | Some _ ->
            failwith
              "[%s] the job '%s' is included twice"
              pipeline_name
              job.name)
      jobs ;
    (* Check usage of [needs:] & [depends:] *)
    Fun.flip List.iter jobs @@ fun {job; _} ->
    (* Get the [needs:] / [dependencies:] of job *)
    let needs =
      (* The mandatory set of needs *)
      match job.needs with
      | Some needs ->
          Some
            (needs
            |> List.filter_map (fun Gitlab_ci.Types.{job; optional} ->
                   if not optional then Some job else None)
            |> String_set.of_list)
      | None -> None
    in
    let dependencies =
      String_set.of_list (Option.value ~default:[] job.dependencies)
    in
    (* Check that jobs in [needs:]/[dependencies:] jobs are defined *)
    (let dep_needs =
       String_set.union
         dependencies
         (Option.value ~default:String_set.empty needs)
     in
     Fun.flip String_set.iter dep_needs @@ fun need ->
     match Hashtbl.find_opt job_by_name need with
     | Some _needed_job ->
         (* TODO: https://gitlab.com/tezos/tezos/-/issues/7015
            check rule implication *)
         ()
     | None ->
         failwith
           "[%s] job '%s' has a need on '%s' which is not defined in this \
            pipeline."
           pipeline_name
           job.name
           need) ;
    (* Check that dependencies are a subset of needs.
       Note: this is already enforced by the smart constructor {!job}
       defined below. Is it redundant? Nothing enforces the usage of
       this smart constructor at this point.*)
    (match needs with
    | Some needs ->
        String_set.iter
          (fun dependency ->
            if not (String_set.mem dependency needs) then
              failwith
                "[%s] the job '%s' has a [dependency:] on '%s' which is not \
                 included in it's [need:]"
                pipeline_name
                job.name
                dependency)
          dependencies
    | None ->
        (* If the job has no needs, then it suffices that the dependency is in
           an anterior stage. *)
        let stage_index =
          let stage_names = Stage.to_string_list () in
          fun stage_opt ->
            let stage = Option.value ~default:"test" stage_opt in
            List.assoc_opt
              stage
              (List.combine stage_names (range 1 (List.length stage_names)))
        in
        String_set.iter
          (fun dependency ->
            (* We use [find] instead of [find_opt] *)
            let dependency_job = Hashtbl.find job_by_name dependency in
            if stage_index dependency_job.stage >= stage_index job.stage then
              failwith
                "[%s] the job '%s' has a [dependency:] on '%s' which is not in \
                 a anterior stage."
                pipeline_name
                job.name
                dependency)
          dependencies) ;
    (* Check that all [dependencies:] are on jobs that produce artifacts *)
    ( Fun.flip String_set.iter dependencies @@ fun dependency ->
      match Hashtbl.find_opt job_by_name dependency with
      | Some {artifacts = Some {paths = Some (_ :: _); _}; _}
      | Some {artifacts = Some {reports = Some {dotenv = Some _; _}; _}; _} ->
          (* This is fine: we depend on a job that define non-report artifacts, or a dotenv file. *)
          ()
      | Some _ ->
          failwith
            "[%s] the job '%s' has a [dependency:] on '%s' which produces \
             neither regular, [paths:] artifacts or a dotenv report."
            pipeline_name
            job.name
            dependency
      | None ->
          (* This case is precluded by the dependency analysis above. *)
          assert false ) ;

    ()

  (* Splits the set of registered non-child pipelines into workflow
     rules and includes. Used by the function [write]. *)
  let workflow_includes () :
      Gitlab_ci.Types.workflow * Gitlab_ci.Types.include_ list =
    let workflow_rule_of_pipeline = function
      | {name; if_; variables; _} ->
          (* Add [PIPELINE_TYPE] to the variables of the workflow rules, so
             that it can be added to the pipeline [name] *)
          let variables =
            ("PIPELINE_TYPE", name) :: Option.value ~default:[] variables
          in
          workflow_rule ~if_ ~variables ~when_:Always ()
    in
    let include_of_pipeline = function
      | {name; if_; _} ->
          (* Note that variables associated to the pipeline are not
             set in the include rule, they are set in the workflow
             rule *)
          let rule = include_rule ~if_ ~when_:Always () in
          Gitlab_ci.Types.{local = path ~name; rules = [rule]}
    in
    let pipelines =
      all ()
      |> List.filter_map (function
             | Pipeline pipeline -> Some pipeline
             | Child_pipeline _ -> None)
    in
    let workflow =
      let rules = List.map workflow_rule_of_pipeline pipelines in
      Gitlab_ci.Types.{rules; name = Some "[$PIPELINE_TYPE] $CI_COMMIT_TITLE"}
    in
    let includes = List.map include_of_pipeline pipelines in
    (workflow, includes)

  let write ?default ?variables ~stages ~filename () =
    (* Write all registered the pipelines *)
    ( Fun.flip List.iter (all ()) @@ fun pipeline ->
      let jobs = jobs pipeline in
      let name = name pipeline in
      if not (Sys.getenv_opt "CI_DISABLE_PRECHECK" = Some "true") then
        precheck pipeline ;
      if jobs = [] then
        failwith "[Pipeline.write] pipeline '%s' contains no jobs!" name ;
      let filename = path ~name in
      List.iter
        (fun tezos_job ->
          let source_file, source_line, _, _ = tezos_job.source_position in
          Cli.verbose
            "%s:%d: generates '%s' for pipeline '%s' in %s"
            source_file
            source_line
            tezos_job.job.name
            name
            filename)
        jobs ;
      let prepend_config =
        match pipeline with
        | Pipeline _ -> []
        | Child_pipeline _ ->
            Gitlab_ci.Types.
              [
                Workflow
                  {
                    rules = [Gitlab_ci.Util.workflow_rule ~if_:Rules.always ()];
                    name = None;
                  };
                Stages (Stage.to_string_list ());
              ]
      in
      let config =
        prepend_config @ List.concat_map tezos_job_to_config_elements jobs
      in
      to_file ~filename config ) ;
    (* Write top-level configuration. *)
    let workflow, includes = workflow_includes () in
    let config =
      let open Gitlab_ci.Types in
      (* Dummy job.

         This fixes the "configuration must contain at least one
         visible job" error in GitLab when using includes.

         For more info, see: https://gitlab.com/gitlab-org/gitlab/-/issues/341693 *)
      let job_dummy : job =
        Gitlab_ci.Util.job
          ~rules:[job_rule ~if_:Rules.never ()]
          ~name:"dummy_job"
          ~script:[{|echo "This job will never execute"|}]
          ()
      in
      [Workflow workflow]
      @ Option.fold ~none:[] ~some:(fun default -> [Default default]) default
      @ Option.fold
          ~none:[]
          ~some:(fun variables -> [Variables variables])
          variables
      @ [Stages stages; Job job_dummy; Include includes]
    in
    to_file ~filename config ;
    ()
end

module Image = struct
  type t = Gitlab_ci.Types.image

  let images : t String_map.t ref = ref String_map.empty

  let register ~name ~image_path =
    let image : t = Image image_path in
    if String_map.mem name !images then
      failwith "[Image.register] attempted to register image %S twice" name
    else (
      images := String_map.add name image !images ;
      image)

  let name (Gitlab_ci.Types.Image name) = name

  let all () = String_map.bindings !images
end

module Changeset = struct
  type t = String_set.t

  let make = String_set.of_list

  let encode changeset =
    changeset |> String_set.elements |> List.sort String.compare

  let union = String_set.union

  let ( @ ) = union
end

type arch = Amd64 | Arm64

let arch_to_string = function Amd64 -> "x86_64" | Arm64 -> "arm64"

let arch_to_string_alt = function Amd64 -> "amd64" | Arm64 -> "arm64"

type dependency =
  | Job of tezos_job
  | Optional of tezos_job
  | Artifacts of tezos_job

type dependencies = Staged of tezos_job list | Dependent of dependency list

type git_strategy = Fetch | Clone | No_strategy

let enc_git_strategy = function
  | Fetch -> "fetch"
  | Clone -> "clone"
  | No_strategy -> "none"

let job ?arch ?after_script ?allow_failure ?artifacts ?before_script ?cache
    ?interruptible ?(dependencies = Staged []) ?services ?variables ?rules
    ?timeout ?tags ?git_strategy ?when_ ?coverage ?retry ?parallel ~__POS__
    ~image ~stage ~name script : tezos_job =
  (match (rules, when_) with
  | Some _, Some _ ->
      failwith
        "[job] do not use [when_] and [rules] at the same time in job '%s' -- \
         it's confusing."
        name
  | _ -> ()) ;
  let tags =
    Some
      (match (arch, tags) with
      | Some arch, None ->
          [(match arch with Amd64 -> "gcp" | Arm64 -> "gcp_arm64")]
      | None, Some tags -> tags
      | None, None ->
          (* By default, we assume Amd64 runners as given by the [gcp] tag. *)
          ["gcp"]
      | Some _, Some _ ->
          failwith
            "[job] cannot specify both [arch] and [tags] at the same time in \
             job '%s'."
            name)
  in
  let stage = Some (Stage.name stage) in
  (match (parallel : Gitlab_ci.Types.parallel option) with
  | Some (Vector n) when n < 2 ->
      failwith
        "[job] the argument [parallel] must be at least 2 or omitted, in job \
         '%s'."
        name
  | Some (Matrix []) ->
      failwith
        "[job] the argument [matrix] must be at a non empty list of variables, \
         in job '%s'."
        name
  | _ -> ()) ;
  let needs, dependencies =
    let name {job = {name; _}; _} = name in
    match dependencies with
    | Staged dependencies -> (None, List.map name dependencies)
    | Dependent dependencies ->
        let rec loop (needs, dependencies) = function
          | dep :: deps ->
              let dep_name =
                match dep with Job j | Optional j | Artifacts j -> name j
              in
              let needs ~optional =
                Gitlab_ci.Types.{job = dep_name; optional} :: needs
              in
              let needs, dependencies =
                match dep with
                | Job _ -> (needs ~optional:false, dependencies)
                | Optional _ -> (needs ~optional:true, dependencies)
                | Artifacts _ ->
                    (needs ~optional:false, dep_name :: dependencies)
              in
              loop (needs, dependencies) deps
          | [] -> (Some (List.rev needs), List.rev dependencies)
        in
        loop ([], []) dependencies
  in
  (* https://docs.gitlab.com/ee/ci/yaml/#needs *)
  (match needs with
  | Some needs when List.length needs > 50 ->
      failwith
        "[job] attempted to add %d [needs] to the job '%s' -- GitLab imposes a \
         limit of 50."
        (List.length needs)
        name
  | _ -> ()) ;
  let variables =
    match git_strategy with
    | Some strategy ->
        Some
          (("GIT_STRATEGY", enc_git_strategy strategy)
          :: Option.value ~default:[] variables)
    | None -> variables
  in
  (match retry with
  | Some retry when retry < 0 || retry > 2 ->
      failwith
        "Invalid [retry] value '%d' for job '%s': must be 0, 1 or 2."
        retry
        name
  | _ -> ()) ;
  let job : Gitlab_ci.Types.job =
    {
      name;
      after_script;
      allow_failure;
      artifacts;
      before_script;
      cache;
      image = Some image;
      interruptible;
      needs;
      (* Note that [dependencies] is always filled, because we want to
         fetch no dependencies by default ([dependencies = Some
         []]), whereas the absence of [dependencies = None] would
         fetch all the dependencies of the preceding jobs. *)
      dependencies = Some dependencies;
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
  in
  {job; source_position = __POS__}

let add_artifacts ?name ?expose_as ?reports ?expire_in ?when_ paths
    (tezos_job : tezos_job) =
  map_job tezos_job @@ fun (job : Gitlab_ci.Types.job) ->
  match job.artifacts with
  | None ->
      {
        job with
        artifacts = Some (artifacts ?expose_as ?reports ?expire_in ?when_ paths);
      }
  | Some artifacts ->
      let opt_merge_right o1 o2 =
        match (o1, o2) with
        | _, Some value -> Some value
        | Some value, None -> Some value
        | None, None -> None
      in
      let opt_combine o1 o2 f =
        match (o1, o2) with
        | None, opt | opt, None -> opt
        | Some value1, Some value2 -> Some (f value1 value2)
      in
      let name = opt_merge_right artifacts.name name in
      let expose_as = opt_merge_right artifacts.expose_as expose_as in
      let expire_in =
        opt_combine artifacts.expire_in expire_in
        @@ fun expiration1 expiration2 ->
        (* This function gives a measure of the size of a duration in seconds.
           It is only used to compare durations. *)
        match (expiration1, expiration2) with
        | Never, _ | _, Never -> Never
        | Duration interval1, Duration interval2 ->
            let interval_to_seconds = function
              | Gitlab_ci.Types.Seconds n -> n
              | Minutes n -> n * 60
              | Hours n -> n * 3600
              | Days n -> n * 24 * 3600
              | Weeks n -> n * 7 * 24 * 3600
              | Months n -> n * 31 * 7 * 24 * 3600
              | Years n -> n * 365 * 7 * 24 * 3600
            in
            if interval_to_seconds interval1 > interval_to_seconds interval2
            then Duration interval1
            else Duration interval2
      in
      let when_ =
        opt_combine artifacts.when_ when_ @@ fun when1 when2 ->
        if when1 = when2 then when1 else Always
      in
      let paths =
        match paths with
        | [] -> artifacts.paths
        | _ -> Some (Option.value ~default:[] artifacts.paths @ paths)
      in
      let reports =
        opt_combine artifacts.reports reports @@ fun reports1 reports2 ->
        let opt_combine_fail field o1 o2 =
          opt_combine o1 o2 @@ fun _v1 _v2 ->
          failwith
            "[add_artifacts] attempted to override existing %s report"
            field
        in
        {
          dotenv = opt_combine_fail "dotenv" reports1.dotenv reports2.dotenv;
          junit = opt_combine_fail "junit" reports1.junit reports2.junit;
          coverage_report =
            opt_combine_fail
              "coverage_report"
              reports1.coverage_report
              reports2.coverage_report;
        }
      in
      {
        job with
        artifacts = Some {name; expose_as; expire_in; when_; paths; reports};
      }

let append_variables ?(allow_overwrite = false) new_variables
    (tezos_job : tezos_job) : tezos_job =
  map_job tezos_job @@ fun job ->
  let variables =
    let old_variables, new_variables =
      List.fold_left
        (fun (old_variables, new_variables) (name, value) ->
          let old_variables =
            match List.assoc_opt name old_variables with
            | Some old_value ->
                if not allow_overwrite then
                  failwith
                    "[Tezos_ci.append_variables] attempted to overwrite the \
                     variable '%s' (old value: '%s', new value: '%s')"
                    name
                    old_value
                    value ;
                List.remove_assoc name old_variables
            | None -> old_variables
          in
          (old_variables, (name, value) :: new_variables))
        (Option.value ~default:[] job.variables, [])
        new_variables
    in
    old_variables @ List.rev new_variables
  in
  {job with variables = Some variables}

let check_files ~remove_extra_files ?(exclude = fun _ -> false) () =
  let all_files =
    let root = "." in
    let rec loop acc dir =
      let dir_contents = Sys.readdir (root // dir) in
      let add_item acc filename =
        let full_filename = dir // filename in
        if Filename.extension filename = ".yml" then
          String_set.add full_filename acc
        else if filename.[0] = '.' || filename.[0] = '_' then acc
        else if
          try Sys.is_directory (root // dir // filename)
          with Sys_error _ -> false
        then loop acc full_filename
        else acc
      in
      Array.fold_left add_item acc dir_contents
    in
    loop (String_set.singleton ".gitlab-ci.yml") ".gitlab"
  in
  let all_non_excluded_files =
    String_set.filter (fun x -> not (exclude x)) all_files
  in
  let error_generated_and_excluded =
    String_set.filter exclude !generated_files
  in
  String_set.iter
    (Cli.error "%s: generated but is excluded")
    error_generated_and_excluded ;
  let error_not_generated =
    String_set.diff all_non_excluded_files !generated_files
  in
  String_set.iter
    (fun file ->
      if remove_extra_files then (
        Cli.info "%s: exists but was not generated, removing it." file ;
        Sys.remove file)
      else Cli.error "%s: exists but was not generated" file)
    error_not_generated ;
  if
    not
      ((remove_extra_files || String_set.is_empty error_not_generated)
      && String_set.is_empty error_generated_and_excluded)
  then (
    Cli.error
      "Please modify ci/bin/main.ml to either generate the above file(s)\n\
       or declare them in the 'exclude' function (but not both)." ;
    if not (remove_extra_files || String_set.is_empty error_not_generated) then
      Cli.error
        "If this file is a leftover from some previous work on the CI\n\
         system then simply remove with 'make -C ci remove-extra-files' or \
         with:\n\n\
        \  rm %s"
        (error_not_generated |> String_set.elements |> String.concat " ")) ;
  if !Cli.has_error then exit 1

let append_script script tezos_job =
  map_job tezos_job @@ fun job -> {job with script = job.script @ script}
