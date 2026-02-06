(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Rules = Rules
module Runner = Runner
open Gitlab_ci.Util

let failwith fmt = Format.kasprintf (fun s -> failwith s) fmt

module Cli = struct
  type action =
    | Write
    | Overview_pipelines
    | List_pipelines
    | Describe_pipeline of {name : string}

  type config = {
    mutable verbose : bool;
    mutable inline_source_info : bool;
    mutable remove_extra_files : bool;
    mutable action : action;
  }

  let config =
    {
      verbose = false;
      inline_source_info = false;
      remove_extra_files = false;
      action = Write;
    }

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
            " Show debug output, including the location of each generated job."
          );
          ( "--inline-source-info",
            Arg.Unit (fun () -> config.inline_source_info <- true),
            " Comment each generated job with source information." );
          ( "--remove-extra-files",
            Arg.Unit (fun () -> config.remove_extra_files <- true),
            " Remove files that are neither generated nor excluded." );
          ( "--overview-pipelines",
            Arg.Unit (fun () -> config.action <- Overview_pipelines),
            " List registered pipelines." );
          ( "--list-pipelines",
            Arg.Unit (fun () -> config.action <- List_pipelines),
            " List registered pipelines." );
          ( "--describe-pipeline",
            Arg.String (fun name -> config.action <- Describe_pipeline {name}),
            "NAME Describe a pipeline." );
        ]
    in
    Arg.parse
      speclist
      (fun s ->
        Arg.(usage speclist (sf "No anonymous arguments (got %s)" s)) ;
        exit 1)
      (sf "Usage: %s [options]\n\nOptions are:" Sys.argv.(0))
end

module Stage = struct
  type t = Stage of {name : string; index : int}

  let stages : t list ref = ref []

  let register name =
    let stage = Stage {name; index = List.length !stages} in
    if
      List.exists
        (fun (Stage {name = name'; index = _}) -> name' = name)
        !stages
    then failwith "[Stage.register] attempted to register stage %S twice" name
    else (
      stages := stage :: !stages ;
      stage)

  let name (Stage {name; index = _}) = name

  let index (Stage {name = _; index}) = index
end

let templates_to_config (templates : Gitlab_ci.Types.template list) =
  match templates with
  | [] -> [] (* empty includes are forbidden *)
  | _ :: _ ->
      [
        Gitlab_ci.Types.Include
          (List.map
             (fun template ->
               {Gitlab_ci.Types.subkey = Template template; rules = []})
             templates);
      ]

type tezos_job = {
  job : Gitlab_ci.Types.generic_job;
  source_position : string * int * int * int;
  description : string option;
  stage : Stage.t;
  template : Gitlab_ci.Types.template option;
  image_builders : tezos_job list;
}

type tezos_image =
  | Internal of {
      image : Gitlab_ci.Types.image;
      builder_amd64 : tezos_job;
      builder_arm64 : tezos_job option;
    }
  | External of Gitlab_ci.Types.image

let name_of_generic_job (generic_job : Gitlab_ci.Types.generic_job) =
  match generic_job with Job {name; _} | Trigger_job {name; _} -> name

let name_of_tezos_job tezos_job = name_of_generic_job tezos_job.job

let tezos_job_to_config_elements (j : tezos_job) =
  let source_comment =
    if Cli.config.inline_source_info then
      let file, line, _, _ = j.source_position in
      let source_info = sf "(generated from %s:%d)" file line in
      [Gitlab_ci.Types.Comment source_info]
    else []
  in
  source_comment @ [Gitlab_ci.Types.Generic_job j.job]

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

module Pipeline = struct
  type pipeline = {
    name : string;
    description : string;
    if_ : Gitlab_ci.If.t;
    variables : Gitlab_ci.Types.variables option;
    auto_cancel : Gitlab_ci.Types.auto_cancel option;
    jobs : tezos_job list;
  }

  type child_pipeline = {
    name : string;
    description : string;
    auto_cancel : Gitlab_ci.Types.auto_cancel option;
    inherit_ : Gitlab_ci.Types.inherit_ option;
    default : Gitlab_ci.Types.default;
    jobs : tezos_job list;
  }

  type t = Pipeline of pipeline | Child_pipeline of child_pipeline

  let pipelines : t list ref = ref []

  let name = function Pipeline {name; _} | Child_pipeline {name; _} -> name

  let description = function
    | Pipeline {description; _} | Child_pipeline {description; _} -> description

  let jobs = function
    | Child_pipeline {jobs; _} -> jobs
    | Pipeline {jobs; _} -> jobs

  let default_config =
    Gitlab_ci.Types.
      {
        image = None;
        interruptible = Some true;
        (* Check https://docs.gitlab.com/ci/yaml/#retry for more details *)
        retry =
          Some
            {
              max = 2;
              when_ =
                [
                  Stuck_or_timeout_failure;
                  Runner_system_failure;
                  Api_failure;
                  Unknown_failure;
                  Scheduler_failure;
                ];
            };
      }

  let default = function
    | Pipeline _ -> default_config
    | Child_pipeline {default; _} -> default

  let set_jobs jobs = function
    | Child_pipeline pl -> Child_pipeline {pl with jobs}
    | Pipeline pl -> Pipeline {pl with jobs}

  let path : name:string -> string =
   fun ~name -> sf ".gitlab/ci/pipelines/%s.yml" name

  let register_raw pipeline =
    if List.exists (fun pipeline' -> name pipeline = name pipeline') !pipelines
    then
      failwith
        "[Pipeline.register] attempted to register pipeline %S twice"
        (name pipeline)
    else pipelines := pipeline :: !pipelines

  let register ?variables ?auto_cancel ~description ~jobs name if_ =
    register_raw
      (Pipeline {variables; if_; name; jobs; auto_cancel; description})

  let register_child ?auto_cancel ?inherit_ ?(default = default_config)
      ~description ~jobs name =
    let child_pipeline =
      {name; inherit_; jobs; auto_cancel; default; description}
    in
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
    let job_by_name : (string, tezos_job) Hashtbl.t = Hashtbl.create 5 in
    (* Populate [job_by_name] and check that no two different jobs have the same name. *)
    List.iter
      (fun (job : tezos_job) ->
        let name = name_of_tezos_job job in
        match Hashtbl.find_opt job_by_name name with
        | None -> Hashtbl.add job_by_name name job
        | Some _ ->
            failwith "[%s] the job '%s' is included twice" pipeline_name name)
      jobs ;
    (* Check usage of [needs:] & [depends:] *)
    Fun.flip List.iter jobs @@ fun tezos_job ->
    let job = tezos_job.job in
    let job_name = name_of_generic_job job in
    (* Get the [needs:] / [dependencies:] of job *)
    let needs =
      let needs =
        match job with
        | Job {needs; _} -> needs
        | Trigger_job {needs; _} -> needs
      in
      (* The mandatory set of needs *)
      match needs with
      | Some needs ->
          Some
            (needs
            |> List.filter_map (fun Gitlab_ci.Types.{job; optional} ->
                   if not optional then Some job else None)
            |> String_set.of_list)
      | None -> None
    in
    let dependencies =
      match job with
      | Job {dependencies; _} ->
          String_set.of_list @@ Option.value ~default:[] dependencies
      | Trigger_job _ -> String_set.empty
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
           job_name
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
                job_name
                dependency)
          dependencies
    | None ->
        (* If the job has no needs, then it suffices that the dependency is in
           an anterior stage. *)
        String_set.iter
          (fun dependency ->
            (* We use [find] instead of [find_opt] *)
            let dependency_job = Hashtbl.find job_by_name dependency in
            if Stage.(index dependency_job.stage >= index tezos_job.stage) then
              failwith
                "[%s] the job '%s' has a [dependency:] on '%s' which is not in \
                 an anterior stage."
                pipeline_name
                job_name
                dependency)
          dependencies) ;
    (* Check that all [dependencies:] are on jobs that produce artifacts *)
    ( Fun.flip String_set.iter dependencies @@ fun dependency ->
      let job =
        match Hashtbl.find_opt job_by_name dependency with
        | Some {job; _} -> job
        | None ->
            (* This case is precluded by the dependency analysis above. *)
            assert false
      in
      match job with
      | Job {artifacts = Some {paths = Some (_ :: _); _}; _}
      | Job {artifacts = Some {reports = Some {dotenv = Some _; _}; _}; _} ->
          (* This is fine: we depend on a job that define non-report artifacts, or a dotenv file. *)
          ()
      | Job _ ->
          failwith
            "[%s] the job '%s' has a [dependency:] on a job '%s' which \
             produces neither regular, [paths:] artifacts or a dotenv report."
            pipeline_name
            job_name
            dependency
      | Trigger_job _ ->
          failwith
            "[%s] the job '%s' has a [dependency:] on a trigger job '%s', but \
             trigger jobs cannot produce artifacts."
            pipeline_name
            job_name
            dependency ) ;
    ()

  (* Pre-process the pipeline:

     - Find usage of internal images. For each internal image
       dependency, add the appropriate builder job to the pipeline. *)
  let add_image_builders (pipeline : t) =
    (* Note: one job can create many different image paths.

       If two different image paths are required that are built by the
       same job, then make sure it is only include once. *)
    let image_builders =
      (* TODO: we currently do not support image_builders that use internal images *)
      (* [image_builder_jobs] maps image builder job names to the actual job. *)
      let image_builder_jobs : (string, tezos_job) Hashtbl.t =
        Hashtbl.create 5
      in
      let image_dependencies =
        List.concat_map
          (fun {image_builders; _} -> image_builders)
          (jobs pipeline)
      in
      ( Fun.flip List.iter image_dependencies @@ fun builder ->
        let builder_name = name_of_tezos_job builder in
        Hashtbl.replace image_builder_jobs builder_name builder ) ;
      image_builder_jobs |> Hashtbl.to_seq_values |> List.of_seq
    in
    set_jobs (image_builders @ jobs pipeline) pipeline

  (* Splits the set of registered non-child pipelines into workflow
     rules and includes. Used by the function [write]. *)
  let workflow_includes () :
      Gitlab_ci.Types.workflow * Gitlab_ci.Types.include_ list =
    let workflow_rule_of_pipeline = function
      | {name; if_; variables; auto_cancel; _} ->
          (* Add [PIPELINE_TYPE] to the variables of the workflow
             rules, so that it can be added to the pipeline [name] *)
          let variables =
            ("PIPELINE_TYPE", name) :: Option.value ~default:[] variables
          in
          workflow_rule ?auto_cancel ~if_ ~variables ~when_:Always ()
    in
    let include_of_pipeline = function
      | {name; if_; _} ->
          (* Note that variables associated to the pipeline are not
             set in the include rule, they are set in the workflow
             rule *)
          let rule = include_rule ~if_ ~when_:Always () in
          Gitlab_ci.Types.{subkey = Local (path ~name); rules = [rule]}
    in
    let pipelines =
      all ()
      |> List.filter_map (function
           | Pipeline pipeline -> Some pipeline
           | Child_pipeline _ -> None)
    in
    let workflow =
      let rules = List.map workflow_rule_of_pipeline pipelines in
      Gitlab_ci.Types.
        {
          rules;
          name = Some "[$PIPELINE_TYPE] $CI_COMMIT_TITLE";
          auto_cancel = None;
        }
    in
    let includes = List.map include_of_pipeline pipelines in
    (workflow, includes)

  let stages pipeline =
    let jobs = jobs pipeline in
    let stages : (string, int) Hashtbl.t = Hashtbl.create 5 in
    List.iter
      (fun job ->
        Hashtbl.replace stages (Stage.name job.stage) (Stage.index job.stage))
      jobs ;
    Hashtbl.to_seq stages |> List.of_seq
    |> List.sort (fun (_n1, idx1) (_n2, idx2) -> Int.compare idx1 idx2)
    |> List.map fst

  let templates pipeline =
    let jobs = jobs pipeline in
    let templates : (Gitlab_ci.Types.template, unit) Hashtbl.t =
      Hashtbl.create 5
    in
    let replace_template job =
      match job.template with
      | Some template -> Hashtbl.replace templates template ()
      | None -> ()
    in
    List.iter replace_template jobs ;
    Hashtbl.to_seq_keys templates |> List.of_seq

  let write_registered_pipeline pipeline =
    let pipeline = add_image_builders pipeline in
    let jobs = jobs pipeline in
    let name = name pipeline in
    if not (Sys.getenv_opt "CI_DISABLE_PRECHECK" = Some "true") then
      precheck pipeline ;
    if jobs = [] then
      failwith "[Pipeline.write] pipeline '%s' contains no jobs!" name ;
    let filename = path ~name in
    List.iter
      (fun tezos_job ->
        let job_name = name_of_tezos_job tezos_job in
        let source_file, source_line, _, _ = tezos_job.source_position in
        Cli.verbose
          "%s:%d: generates '%s' for pipeline '%s' in %s"
          source_file
          source_line
          job_name
          name
          filename)
      jobs ;
    let prepend_config =
      templates_to_config (templates pipeline)
      @ (match pipeline with
        | Pipeline _ -> []
        | Child_pipeline _ -> [Gitlab_ci.Types.Default (default pipeline)])
      @ (match pipeline with
        | Pipeline _ -> []
        | Child_pipeline _ ->
            Gitlab_ci.Types.
              [
                Workflow
                  {
                    rules = [Gitlab_ci.Util.workflow_rule ~if_:Rules.always ()];
                    name = None;
                    auto_cancel = None;
                  };
              ])
      @ [Stages (stages pipeline)]
    in
    let config =
      prepend_config @ List.concat_map tezos_job_to_config_elements jobs
    in
    to_file ~filename config

  let write_top_level_pipeline ?default ?variables ~filename () =
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
            (* The dummy job must be included in all pipelines. However,
               we do not know which stages are included in a given
               pipeline. The default stage ["test"] might not
               exist. Therefore we put it in the special [.pre] stage
               which always exists
               ({{:https://docs.gitlab.com/ee/ci/yaml/#stage-pre}ref}). *)
          ~stage:".pre"
          ~script:[{|echo "This job will never execute"|}]
          ()
      in
      [Workflow workflow]
      @ Option.fold ~none:[] ~some:(fun default -> [Default default]) default
      @ Option.fold
          ~none:[]
          ~some:(fun variables -> [Variables variables])
          variables
      @ [Generic_job (Job job_dummy); Include includes]
    in
    to_file ~filename config

  let write ?default ?variables ~filename () =
    (* Write all registered pipelines *)
    List.iter write_registered_pipeline (all ()) ;
    (* Write top-level configuration. *)
    write_top_level_pipeline ?default ?variables ~filename () ;
    ()

  let list_pipelines () =
    let pipelines = all () in
    let open Format in
    open_vbox 0 ;
    Printf.printf "Registered pipeline types:\n\n" ;
    ( Fun.flip List.iter pipelines @@ fun pipeline ->
      let pipeline = add_image_builders pipeline in
      let jobs = jobs pipeline in
      let name = name pipeline in
      let filename = path ~name in
      printf " - '%s' (%s): %d job(s)@," name filename (List.length jobs) ;
      printf "@,@[<2>  %a@]@,@," pp_print_text (description pipeline) ) ;
    close_box () ;
    print_flush ()

  let markdown_table fmt ~headers ~rows =
    let cells = List.length headers in
    ( Fun.flip List.iteri rows @@ fun idx row ->
      if List.length row <> cells then
        raise
          (Invalid_argument
             (sf
                "Row %d should have exactly as many cells as the header row: \
                 %d, but has %d."
                (idx + 1)
                (List.length row)
                cells)) ) ;
    let cell_sizes =
      List.fold_left
        (fun max_sizes row ->
          let row_sizes = List.map String.length row in
          List.map2 Int.max max_sizes row_sizes)
        (List.map String.length headers)
        rows
    in
    let open Printf in
    let print_row row =
      fprintf fmt "|" ;
      ( Fun.flip List.iteri row @@ fun idx cell ->
        fprintf
          fmt
          " %s%s |"
          cell
          (String.make (List.nth cell_sizes idx - String.length cell) ' ') ) ;
      fprintf fmt "\n"
    in
    (* make header *)
    print_row headers ;
    (* make separator between header and rows *)
    print_row (Fun.flip List.map cell_sizes @@ fun size -> String.make size '-') ;
    (* print rows *)
    Fun.flip List.iter rows print_row

  let shorten_description description =
    description |> String.split_on_char '\n' |> function
    | [] -> description
    | l :: _ -> l

  let overview_pipelines () =
    markdown_table
      stdout
      ~headers:["PIPELINE"; "DESCRIPTION"; "JOBS"]
      ~rows:
        ( Fun.flip List.map (all ()) @@ fun pipeline ->
          let description = shorten_description @@ description pipeline in
          [
            name pipeline;
            description;
            List.length (jobs pipeline) |> string_of_int;
          ] )

  let describe_pipeline pipeline_name =
    let pipelines = all () in
    match
      List.find_opt (fun pipeline -> name pipeline = pipeline_name) pipelines
    with
    | Some pipeline ->
        let pipeline = add_image_builders pipeline in
        let jobs = jobs pipeline in
        let name = name pipeline in
        let filename = path ~name in
        let open Format in
        open_vbox 0 ;
        printf "%s (%s): %d jobs@," name filename (List.length jobs) ;
        printf "@,@[%a@]@,@," pp_print_text (description pipeline) ;
        close_box () ;
        print_flush () ;
        print_endline "Jobs in this pipeline:\n" ;
        markdown_table
          stdout
          ~headers:["STAGE"; "JOB"; "DESCRIPTION"]
          ~rows:
            ( Fun.flip List.concat_map (stages pipeline) @@ fun stage ->
              let jobs_of_stage =
                List.filter (fun job -> Stage.name job.stage = stage) jobs
              in
              Fun.flip List.map jobs_of_stage @@ fun job ->
              let description =
                match job.description with
                | None -> ""
                | Some d -> shorten_description d
              in
              [stage; name_of_tezos_job job; description] )
    | None ->
        Printf.eprintf
          "Pipeline '%s' does not exist.\n\nTry one of:\n"
          pipeline_name ;
        List.iter
          (fun pipeline -> Printf.eprintf "- %s\n" (name pipeline))
          pipelines ;
        exit 1
end

module Image = struct
  type t = tezos_image

  (* Track image builders to avoid name collisions. *)
  let image_builders : (string, tezos_job) Hashtbl.t = Hashtbl.create 5

  let mk_external ~image_path : t = External (Image image_path)

  let mk_internal ?image_builder_arm64 ~image_builder_amd64 ~image_path () : t =
    let image =
      Internal
        {
          builder_amd64 = image_builder_amd64;
          builder_arm64 = image_builder_arm64;
          image = Image image_path;
        }
    in
    let register_builder image_builder =
      let builder_name = name_of_tezos_job image_builder in
      match Hashtbl.find_opt image_builders builder_name with
      | None -> Hashtbl.add image_builders builder_name image_builder
      | Some image_builder' when image_builder = image_builder' -> ()
      | Some _ ->
          failwith
            "The image builder '%s' for image '%s' was registered twice with \
             differing definitions."
            builder_name
            image_path
    in
    (match
       ( name_of_tezos_job image_builder_amd64,
         Option.map name_of_tezos_job image_builder_arm64 )
     with
    | name, Some name' when name = name' ->
        failwith
          "The the image builders for multi-arch image '%s' must have distinct \
           names (both are called '%s')."
          image_path
          name
    | _ -> ()) ;
    register_builder image_builder_amd64 ;
    Option.iter register_builder image_builder_arm64 ;
    image

  let image = function Internal {image; _} | External image -> image

  let pp ppf img =
    match image img with
    | Image image_name -> Format.fprintf ppf "%s" image_name
end

module Changeset = struct
  type t = String_set.t

  let make = String_set.of_list

  (* Remove paths that are implied by other paths.
     For instance, [a/b/c] is implied by [a/**/*]. *)
  let remove_implied changeset =
    let implies a b =
      if String.ends_with ~suffix:"/**/*" a then
        let a_without_suffix =
          (* Remove "**/*" (do NOT remove the first "/"). *)
          String.sub a 0 (String.length a - 4)
        in
        String.starts_with ~prefix:a_without_suffix b
      else false
    in
    let add acc path =
      if String_set.exists (fun previous -> implies previous path) acc then acc
      else String_set.add path acc
    in
    (* Add all paths one by one, starting from the smallest ones so that
       the next ones can see if they are implied by smaller prefixes before them. *)
    changeset |> String_set.to_list
    |> List.sort (fun a b -> Int.compare (String.length a) (String.length b))
    |> List.fold_left add String_set.empty

  let encode changeset =
    changeset |> remove_implied |> String_set.elements
    |> List.sort String.compare

  let union = String_set.union

  let ( @ ) = union
end

type dependency =
  | Job of tezos_job
  | Optional of tezos_job
  | Artifacts of tezos_job

type dependencies = Staged of tezos_job list | Dependent of dependency list

let dependencies_add_artifact_dependency dependencies tezos_job =
  match dependencies with
  | Staged jobs -> Staged (tezos_job :: jobs)
  | Dependent dependencies -> Dependent (Artifacts tezos_job :: dependencies)

(* Resolve {!dependencies} into a pair of [needs:] and [dependencies:] *)
let resolve_dependencies job_name dependencies =
  let needs, dependencies =
    let name = name_of_tezos_job in
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
        job_name
  | _ -> ()) ;
  (needs, dependencies)

type git_strategy = Fetch | Clone | No_strategy

let enc_git_strategy = function
  | Fetch -> "fetch"
  | Clone -> "clone"
  | No_strategy -> "none"

let number_of_declared_jobs = ref 0

let get_number_of_declared_jobs () = !number_of_declared_jobs

let job ?(arch : Runner.Arch.t option) ?(after_script = []) ?allow_failure
    ?artifacts ?(before_script = []) ?cache ?id_tokens ?interruptible
    ?(dependencies = Dependent []) ?(image_dependencies = []) ?services
    ?variables ?(rules : Gitlab_ci.Types.job_rule list option)
    ?(timeout = Gitlab_ci.Types.Minutes 60) ?(tag : Runner.Tag.t option)
    ?(cpu : Runner.CPU.t option) ?(storage : Runner.Storage.t option)
    ?interruptible_runner ?git_strategy ?retry ?parallel ?description
    ?(dev_infra = false) ~__POS__ ?image ?template ?(datadog = true) ~stage
    ~name script : tezos_job =
  incr number_of_declared_jobs ;
  (* The tezos/tezos CI uses singleton tags for its runners. *)
  let tag : Runner.Tag.t =
    let provider : Runner.Provider.t = if dev_infra then GCP_dev else GCP in
    let show show_fun = function
      | None -> "(unspecified)"
      | Some x -> show_fun x
    in
    let provider_string = Runner.Provider.show provider in
    let arch_string = show Runner.Arch.show_uniform arch in
    let cpu_string = show Runner.CPU.show cpu in
    let storage_string = show Runner.Storage.show storage in
    let interruptible_runner_string =
      show string_of_bool interruptible_runner
    in
    match tag with
    | None -> (
        match
          Runner.Tag.choose
            ~provider
            ?arch
            ?cpu
            ?storage
            ?interruptible:interruptible_runner
            ()
        with
        | None ->
            failwith
              "job %S: no suitable runner tag found for provider = %s, arch = \
               %s, cpu = %s, storage = %s, interruptible_runner = %s"
              name
              provider_string
              arch_string
              cpu_string
              storage_string
              interruptible_runner_string
        | Some tag -> tag)
    | Some Dynamic ->
        (* Cannot check, assume the user knows what they are doing. *)
        Dynamic
    | Some tag ->
        if
          not
            (Runner.Tag.has
               ~provider
               ?arch
               ?cpu
               ?storage
               ?interruptible:interruptible_runner
               tag)
        then
          failwith
            "job %S: requested tag %s is not compatible with provider = %s, \
             arch = %s, cpu = %s, storage = %s, interruptible_runner = %s"
            name
            (Runner.Tag.show tag)
            provider_string
            arch_string
            cpu_string
            storage_string
            interruptible_runner_string ;
        tag
  in
  (* Check [rules]. *)
  (match rules with
  | None -> ()
  | Some [] ->
      failwith
        "In job '%s', ~rules is the empty list. Either specify a non-empty \
         list, or do not specify ~rules."
        name
  | Some rules ->
      Fun.flip List.iter rules @@ fun {changes; _} ->
      let changes_count =
        match changes with None -> 0 | Some list -> List.length list
      in
      if changes_count >= 50 then
        (* See https://docs.gitlab.com/ci/yaml/#ruleschanges *)
        failwith
          "In job '%s', one of the ~rules has more than 50 entries in its \
           changeset. This is not allowed by GitLab."
          name) ;
  let arch = if Option.is_some arch then arch else Runner.Tag.arch tag in
  (match (image, arch) with
  | Some (Internal {image = Image image_path; _}), None ->
      failwith
        "[job] the job '%s' has tag '%s' whose architecture is not statically \
         known cannot use the image '%s' since it is Internal. Set a static \
         tag or use an external image."
        name
        (Runner.Tag.show tag)
        image_path
  | _ -> ()) ;
  let tags = Some [Runner.Tag.show tag] in
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
  let image_dependencies =
    (match image with
    | Some (Internal image) -> [Internal image]
    | Some (External _) | None -> [])
    @ image_dependencies
  in
  (match (template, image) with
  | Some _, Some _ ->
      failwith
        "[job] cannot specify both [image] and [template] in job '%s' as it \
         would override the image defined in the template."
        name
  | None, _ | _, None -> ()) ;
  let dependencies, image_builders =
    ( Fun.flip List.iter image_dependencies @@ function
      | External (Image image_path) ->
          failwith
            "It doesn't make any sense for job %s to depend on the external \
             image %s"
            name
            image_path
      | Internal _ -> () ) ;
    let add_builder (dependencies, image_builders) = function
      | External _ -> (dependencies, image_builders)
      | Internal {builder_amd64; builder_arm64; _} ->
          let builder =
            match (arch, builder_arm64) with
            | Some Amd64, _ -> builder_amd64
            | Some Arm64, Some builder_arm64 -> builder_arm64
            | Some Arm64, None ->
                failwith "requested arm64 image for which there is no builder"
            | None, _ -> failwith "unknown arch"
          in
          let image_builders = builder :: image_builders in
          let dependencies =
            match dependencies with
            | Staged artifact_dependencies ->
                Staged (builder :: artifact_dependencies)
            | Dependent dependencies ->
                Dependent (Artifacts builder :: dependencies)
          in
          (dependencies, image_builders)
    in
    List.fold_left add_builder (dependencies, []) image_dependencies
  in
  let needs, dependencies = resolve_dependencies name dependencies in
  let variables =
    match git_strategy with
    | Some strategy ->
        Some
          (("GIT_STRATEGY", enc_git_strategy strategy)
          :: Option.value ~default:[] variables)
    | None -> variables
  in
  (match retry with
  | Some Gitlab_ci.Types.{max; when_ = _} when max < 0 || max > 2 ->
      failwith
        "Invalid [retry:max] value '%d' for job '%s': must be 0, 1 or 2."
        max
        name
  | _ -> ()) ;
  (match
     (Sys.getenv_opt Gitlab_ci.Predefined_vars.(show gitlab_user_login), tag)
   with
  | ( Some "nomadic-margebot",
      ( Gcp_dev | Gcp_dev_arm64 | Gcp_not_interruptible_dev | Gcp_tezt_dev
      | Gcp_high_cpu_dev | Gcp_very_high_cpu_dev | Gcp_very_high_cpu_ramfs_dev
        ) ) ->
      failwith
        "[job] Attempting to merge a CI configuration using development \
         runners (job: %s)"
        name
  | _ -> ()) ;
  let job : Gitlab_ci.Types.job =
    {
      name;
      after_script =
        Some
          (". ./scripts/ci/datadog_send_job_cache_info.sh 'after'"
         :: after_script);
      allow_failure;
      artifacts;
      (* Sending job-level info to Datadog is done first. This step should never fail, even if [datadog-ci] is not installed in the image running the job. *)
      before_script =
        Some
          (if datadog then
             "SCRIPT_STEP_BEGIN=$(date +%s)"
             :: ". ./scripts/ci/datadog_send_job_info.sh" :: before_script
             @ [". ./scripts/ci/datadog_send_job_cache_info.sh 'before'"]
           else before_script);
      cache;
      id_tokens;
      image = Option.map Image.image image;
      interruptible;
      needs;
      (* Note that [dependencies] is always filled, because we want to
         fetch no dependencies by default ([dependencies = Some
         []]), whereas the absence of [dependencies = None] would
         fetch all the dependencies of the preceding jobs. *)
      dependencies = Some dependencies;
      rules;
      script =
        script @ [". ./scripts/ci/datadog_send_job_script_step_time.sh || true"];
      services;
      stage = Some (Stage.name stage);
      variables;
      timeout = Some timeout;
      tags;
      when_ = None;
      coverage = None;
      retry;
      parallel;
    }
  in
  {
    job = Job job;
    source_position = __POS__;
    description;
    stage;
    image_builders;
    template;
  }

(* helper function to merge two list of variables, giving the precedence
   to the values in [variables] over the [default_variables] *)
let merge_variables ~default_variables variables =
  let module StringMap = Map.Make (String) in
  let m1 = List.to_seq default_variables |> StringMap.of_seq in
  let m2 = List.to_seq variables |> StringMap.of_seq in
  StringMap.union (fun _ _ v2 -> Some v2) m1 m2
  |> StringMap.to_seq |> List.of_seq |> List.rev

let trigger_job ?(dependencies = Staged []) ?rules ?description
    ?(variables = []) ~__POS__ ~stage ?parent_pipeline_name
    Pipeline.
      {
        name = child_pipeline_name;
        inherit_;
        jobs = _;
        auto_cancel = _;
        description = _;
        default = _;
      } : tezos_job =
  let job_name = "trigger:" ^ child_pipeline_name in
  let needs, dependencies = resolve_dependencies job_name dependencies in
  if dependencies != [] then
    failwith
      "[trigger_job] trigger job '%s' has artifact-dependencies, which is not \
       allowed by GitLab CI."
      job_name ;
  let pipeline_type =
    match parent_pipeline_name with
    | None -> child_pipeline_name
    | Some parent_name -> parent_name ^ "-" ^ child_pipeline_name
  in
  let trigger_job =
    Gitlab_ci.Util.trigger_job
      ?needs
      ?inherit_
      ?rules
      ~stage:(Stage.name stage)
      ~variables:
        (merge_variables
           ~default_variables:
             [
               ("PIPELINE_TYPE", pipeline_type);
               ("DOCKER_FORCE_BUILD", "$DOCKER_FORCE_BUILD");
             ]
           variables)
      ~name:job_name
      (Pipeline.path ~name:child_pipeline_name)
  in
  {
    job = Trigger_job trigger_job;
    description;
    source_position = __POS__;
    stage;
    image_builders = [];
    template = None;
  }

let map_non_trigger_job ?error_on_trigger (tezos_job : tezos_job)
    (f : Gitlab_ci.Types.job -> Gitlab_ci.Types.job) : tezos_job =
  match tezos_job.job with
  | Job job -> {tezos_job with job = Job (f job)}
  | _ -> (
      match error_on_trigger with
      | None -> tezos_job
      | Some error_on_trigger -> failwith "%s" error_on_trigger)

let add_artifacts ?name ?expose_as ?reports ?expire_in ?when_ paths
    (tezos_job : tezos_job) =
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[add_artifacts] attempting to add artifact to trigger job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun (job : Gitlab_ci.Types.job) ->
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
          container_scanning =
            opt_combine_fail
              "container_scanning"
              reports1.container_scanning
              reports2.container_scanning;
        }
      in
      {
        job with
        artifacts = Some {name; expose_as; expire_in; when_; paths; reports};
      }

let append_variables ?(allow_overwrite = false) new_variables
    (tezos_job : tezos_job) : tezos_job =
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[append_variables] attempting to append variables to trigger job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun job ->
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
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[append_script] attempting to append script to trigger job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun job -> {job with script = job.script @ script}

let append_cache cache tezos_job =
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[append_cache] attempting to append a cache to trigger job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun job ->
  let caches = Option.value ~default:[] job.cache in
  {job with cache = Some (caches @ [cache])}

let append_before_script script tezos_job =
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[append_before_script] attempting to append before_script to trigger \
          job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun job ->
  let before_script = Option.value ~default:[] job.before_script in
  {job with before_script = Some (before_script @ script)}

let append_after_script script tezos_job =
  map_non_trigger_job
    ~error_on_trigger:
      (sf
         "[append_after_script] attempting to append after_script to trigger \
          job '%s'"
         (name_of_tezos_job tezos_job))
    tezos_job
  @@ fun job ->
  let after_script = Option.value ~default:[] job.after_script in
  {job with after_script = Some (after_script @ script)}

(* The reason we don't use [error_on_trigger] here is that this is intended to be
   used with [List.map] on a whole pipeline, and it's just more convenient to
   not have to filter out the trigger job to then put it back. *)
let with_interruptible value tezos_job =
  map_non_trigger_job tezos_job @@ fun job ->
  {job with interruptible = Some value}

(* Define [stages:]

   The "manual" stage exists to fix a UI problem that occurs when mixing
   manual and non-manual jobs. *)
module Stages = struct
  let start = Stage.register "start"

  (* All automatic image creation is done in the stage [images]. *)
  let images = Stage.register "images"

  let sanity = Stage.register "sanity"

  let build = Stage.register "build"

  let test = Stage.register "test"

  let packaging = Stage.register "packaging"

  let publish = Stage.register "publish"

  let publishing_tests = Stage.register "publishing_tests"

  let manual = Stage.register "manual"
end

(* Register external images.

   Use this module to register images that are as built outside the
   [tezos/tezos] CI. *)
module Images_external = struct
  let nix = Image.mk_external ~image_path:"nixos/nix:2.22.1"

  (* Match GitLab executors version and directly use the Docker socket
     The Docker daemon is already configured, experimental features are enabled
     The following environment variables are already set:
     - [BUILDKIT_PROGRESS]
     - [DOCKER_DRIVER]
     - [DOCKER_VERSION]
     For more info, see {{:https://docs.gitlab.com/ee/ci/docker/using_docker_build.html#use-docker-socket-binding}} here.

     This image is defined in {{:https://gitlab.com/tezos/docker-images/ci-docker}tezos/docker-images/ci-docker}. *)
  let docker =
    Image.mk_external
      ~image_path:
        "${GCP_RELEASE_REGISTRY}/tezos/docker-images/ci-docker:v1.13.0"

  (* Image used in initial pipeline job that sends to Datadog useful
     info for CI visibility.

     The [datadog-ci] version should be consistent across all CI
     images that use it. At the moment it is installed in the
     external image below and the internal image [e2etest]. *)
  let datadog_ci = Image.mk_external ~image_path:"datadog/ci:v4.1.0"

  let ci_release =
    Image.mk_external
      ~image_path:
        "${GCP_RELEASE_REGISTRY}/tezos/docker-images/ci-release:v1.8.0"

  let hadolint = Image.mk_external ~image_path:"hadolint/hadolint:2.12.0-alpine"

  (* We specify the semgrep image by hash to avoid flakiness. Indeed, if we took the
     latest release, then an update in the parser or analyser could result in new
     errors being found even if the code doesn't change. This would place the
     burden for fixing the code on the wrong dev (the devs who happen to open an
     MR coinciding with the semgrep update rather than the dev who wrote the
     infringing code in the first place).
     Update the hash in scripts/semgrep/README.md too when updating it here
     Last update: 2022-01-03 *)
  let semgrep_agent =
    Image.mk_external ~image_path:"returntocorp/semgrep-agent:sha-c6cd7cf"

  (* Image provided by GitLab. More details in the doc:
     - https://docs.gitlab.com/ee/ci/runners/hosted_runners/macos.html#supported-macos-images
     - https://gitlab-org.gitlab.io/ci-cd/shared-runners/images/macos-image-inventory/macos-15-xcode-16/ *)
  let macosx_15 = Image.mk_external ~image_path:"macos-15-xcode-16"

  (* Image used in [schedule_security_scans] pipeline. Trivy scans
     Docker images and codebase for CVEs and SBOMs. *)
  let trivy = Image.mk_external ~image_path:"aquasec/trivy:latest"
end

let opt_var name f = function Some value -> [(name, f value)] | None -> []

(** {2 Job makers} *)

(** Helper to create jobs that uses the Docker daemon.

    It sets the appropriate image. Furthermore, unless
    [skip_docker_initialization] is [true], it:
    - activates the Docker daemon as a service;
    - sets up authentification with Docker registries
    in the job's [before_script] section.

    If [ci_docker_hub] is set to [true], then the job will
    authenticate with Docker Hub provided the environment variable
    [CI_DOCKER_AUTH] contains the appropriate credentials. *)
let job_docker_authenticated ?(skip_docker_initialization = false)
    ?ci_docker_hub ?artifacts ?(variables = []) ?rules
    ?(dependencies = Staged []) ?image_dependencies ?arch ?storage ?tag
    ?allow_failure ?parallel ?timeout ?retry ?description ?dev_infra ~__POS__
    ~stage ~name script : tezos_job =
  let docker_version = "24.0.7" in
  job
    ?rules
    ~dependencies
    ?image_dependencies
    ?artifacts
    ?arch
    ?storage
    ?tag
    ?allow_failure
    ?parallel
    ?timeout
    ?retry
    ?description
    ?dev_infra
    ~__POS__
    ~image:Images_external.docker
    ~variables:
      ([("DOCKER_VERSION", docker_version)]
      @ opt_var "CI_DOCKER_HUB" Bool.to_string ci_docker_hub
      @ variables)
    ~before_script:
      (if not skip_docker_initialization then
         ["./scripts/ci/docker_initialize.sh"]
       else [])
    ~services:[{name = "docker:${DOCKER_VERSION}-dind"}]
    ~stage
    ~name
    script

(** {2 Caches} *)
module Cache = struct
  let enable_dune_cache job =
    let path = "$CI_PROJECT_DIR/_dune_cache" in
    job
    |> append_variables
         [
           ("DUNE_CACHE", "enabled");
           ("DUNE_CACHE_STORAGE_MODE", "hardlink");
           ("DUNE_CACHE_ROOT", path);
         ]
    |> append_cache
         (cache
            ~policy:Gitlab_ci.Types.Pull_push
            ~key:
              ("dune_cache-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
            [path])
    |> append_after_script ["eval $(opam env)"; "dune cache trim --size=5GB"]

  let enable_sccache ?error_log ?log ?(policy = Gitlab_ci.Types.Pull) job =
    let rw_mode =
      match policy with Pull -> "READ_ONLY" | Pull_push | Push -> "READ_WRITE"
    in
    job
    |> append_variables
         ([
            (* force incremental build in cargo

              see https://github.com/mozilla/sccache?tab=readme-ov-file#known-caveats *)
            ("CARGO_INCREMENTAL", "0");
            (* we use GCP backend in r/w mode *)
            ("SCCACHE_GCS_BUCKET", "$GCP_SCCACHE_BUCKET");
            ("SCCACHE_GCS_RW_MODE", rw_mode);
            ("SCCACHE_GCS_KEY_PREFIX", "sccache");
            (* if network error, fail over local rust compiler instead of stopping *)
            ("SCCACHE_IGNORE_SERVER_IO_ERROR", "1");
            (* daemon does not stop if no client request *)
            ("SCCACHE_IDLE_TIMEOUT", "0");
          ]
         @ opt_var "SCCACHE_ERROR_LOG" Fun.id error_log
         @ opt_var "SCCACHE_LOG" Fun.id log)
    (* Starts sccache and sets [RUSTC_WRAPPER] *)
    |> append_before_script [". ./scripts/ci/sccache-start.sh"]
    |> append_after_script ["./scripts/ci/sccache-stop.sh"]

  let cargo_home =
    (* Note:
       - We want [CARGO_HOME] to be in a sub-folder of
         {!ci_project_dir} to enable GitLab CI caching.
       - We want [CARGO_HOME] to be hidden from dune
         (thus the dot-prefix). *)
    Gitlab_ci.Predefined_vars.(show ci_project_dir) // ".cargo"

  let enable_networked_cargo = append_variables [("CARGO_NET_OFFLINE", "false")]

  let enable_cargo_cache job =
    job
    |> append_cache
         (cache
            ~key:("cargo-" ^ Gitlab_ci.Predefined_vars.(show ci_job_name_slug))
            [cargo_home // "registry/cache"])
    (* Allow Cargo to access the network *)
    |> enable_networked_cargo
end

(** A set of internally and externally built images.

    Use this module to register images built in the CI of
    [tezos/tezos] that are also used in the same pipelines. See
    {!Images_external} for external images.

    To make the distinction between internal and external images
    transparent to job definitions, this module also includes
    {!Images_external}.

    For documentation on the [CI] images and the [rust_toolchain]
    images, refer to [images/README.md]. *)
module Images = struct
  (* Include external images here for convenience. *)
  include Images_external

  module Base_images = struct
    let path_prefix = "${GCP_PROTECTED_REGISTRY}/tezos/tezos"

    let make_img distro version =
      Image.mk_external ~image_path:(sf "%s/%s-%s" path_prefix distro version)

    (* Version created by https://gitlab.com/tezos/tezos/-/pipelines/2300621337 *)
    let common_version = "master-03271731"

    let debian_version = common_version

    let debian_bookworm = make_img "debian:bookworm" debian_version

    let debian_trixie = make_img "debian:trixie" debian_version

    let debian_unstable = make_img "debian:unstable" debian_version

    let ubuntu_noble = make_img "ubuntu:noble" debian_version

    let ubuntu_jammy = make_img "ubuntu:jammy" debian_version

    let ubuntu_plucky = make_img "ubuntu:plucky" debian_version

    let ubuntu_22_04 = make_img "ubuntu:22.04" debian_version

    let ubuntu_24_04 = make_img "ubuntu:24.04" debian_version

    let ubuntu_25_10 = make_img "ubuntu:25.10" debian_version

    let rpm_version = common_version

    let rockylinux_9 = make_img "rockylinux:9.6" rpm_version

    let rockylinux_10 = make_img "rockylinux:10.0" rpm_version

    let fedora_39 = make_img "fedora:39" rpm_version

    let fedora_41 = make_img "fedora:41" rpm_version

    let fedora_42 = make_img "fedora:42" rpm_version

    let homebrew_version = common_version

    let homebrew = make_img "debian-homebrew:trixie" homebrew_version

    let rust_toolchain_version = common_version

    let rust_toolchain_trixie =
      make_img "debian-rust:trixie" rust_toolchain_version

    let pp = Image.pp
  end

  (* Internal images are built in the stage {!Stages.images}. *)
  let stage = Stages.images

  let client_libs_dependencies =
    let image_builder_amd64 =
      job_docker_authenticated
        ~__POS__
        ~stage
        ~name:"oc.docker:client-libs-dependencies"
        ~description:"Build internal client-libs-dependencies images"
          (* This image is not built for external use. *)
        ~ci_docker_hub:false
          (* Handle docker initialization, if necessary, in [./scripts/ci/docker_client_libs_dependencies_build.sh]. *)
        ~skip_docker_initialization:true
        ["./scripts/ci/docker_client_libs_dependencies_build.sh"]
        ~artifacts:
          (artifacts
             ~reports:
               (reports ~dotenv:"client_libs_dependencies_image_tag.env" ())
             [])
    in
    let image_path =
      "${client_libs_dependencies_image_name}:${client_libs_dependencies_image_tag}"
    in
    Image.mk_internal ~image_builder_amd64 ~image_path ()

  (** The rust toolchain image *)
  let rust_toolchain =
    (* The job that builds the rust_toolchain image.
       This job is automatically included in any pipeline that uses this image. *)
    let image_builder arch ?storage () =
      job_docker_authenticated
        ~__POS__
        ~arch
        ?storage
        ~skip_docker_initialization:true
        ~stage
        ~name:("oc.docker:rust-toolchain:" ^ Runner.Arch.show_uniform arch)
        ~description:
          ("Build internal rust-toolchain images for "
          ^ Runner.Arch.show_uniform arch)
        ~ci_docker_hub:false
        ~variables:
          [
            ( "IMAGE",
              Base_images.(Format.asprintf "%a" pp rust_toolchain_trixie) );
          ]
        ~artifacts:
          (artifacts
             ~reports:(reports ~dotenv:"rust_toolchain_image_tag.env" ())
             [])
        ["./scripts/ci/docker_rust_toolchain_build.sh"]
    in
    let image_path =
      "${rust_toolchain_image_name}:${rust_toolchain_image_tag}"
    in
    Image.mk_internal
      ~image_builder_amd64:(image_builder Amd64 ())
      ~image_builder_arm64:(image_builder Arm64 ~storage:Ramfs ())
      ~image_path
      ()

  (** The image containing all dependencies required for Rust SDK bindings *)
  let rust_sdk_bindings =
    (* The job that builds the rust-sdk-bindings image.
       This job is automatically included in any pipeline that uses this image. *)
    let image_builder arch ?storage () =
      job_docker_authenticated
        ~__POS__
        ~arch
        ?storage
        ~stage
        ~name:("oc.docker:rust-sdk-bindings:" ^ Runner.Arch.show_uniform arch)
        ~description:
          ("Build internal rust-sdk-bindings images for "
          ^ Runner.Arch.show_uniform arch)
        ~ci_docker_hub:false
        ~artifacts:
          (artifacts
             ~reports:(reports ~dotenv:"rust_sdk_bindings_image_tag.env" ())
             [])
        ["./scripts/ci/docker_rust_sdk_bindings_build.sh"]
    in
    let image_path =
      "${rust_sdk_bindings_image_name}:${rust_sdk_bindings_image_tag}"
    in
    Image.mk_internal
      ~image_builder_amd64:(image_builder Amd64 ())
      ~image_builder_arm64:(image_builder Arm64 ~storage:Ramfs ())
      ~image_path
      ()

  (** The jsonnet image *)
  let jsonnet =
    let image_builder arch =
      job_docker_authenticated
        ~__POS__
        ~arch
        ~stage
        ~name:("oc.docker:jsonnet:" ^ Runner.Arch.show_uniform arch)
        ~ci_docker_hub:false
        ~artifacts:
          (artifacts ~reports:(reports ~dotenv:"jsonnet_image_tag.env" ()) [])
        ["./scripts/ci/docker_jsonnet_build.sh"]
    in
    let image_path = "${jsonnet_image_name}:${jsonnet_image_tag}" in
    Image.mk_internal ~image_builder_amd64:(image_builder Amd64) ~image_path ()

  (** The jsonnet image (static tag [master]) *)
  let jsonnet_master =
    let image_path = "${jsonnet_image_name_protected}:master" in
    Image.mk_external ~image_path

  module CI = struct
    (* The job that builds the CI images.
       This job is automatically included in any pipeline that uses any of these images. *)
    let job_docker_ci arch ?storage () =
      let variables =
        Some
          [
            ("ARCH", Runner.Arch.show_uniform arch);
            ("GCLOUD_VERSION", "543.0.0");
          ]
      in
      let retry =
        match arch with
        | Amd64 -> None
        (* We're currently seeing flakiness in the arm64 jobs *)
        | Arm64 ->
            Some {Gitlab_ci.Types.max = 1; when_ = [Runner_system_failure]}
      in
      job_docker_authenticated
        ?variables
        ?retry
        ~__POS__
        ~arch
        ?storage
        ~skip_docker_initialization:true
        ~stage
        ~timeout:(Minutes 90)
        ~name:("oc.docker:ci:" ^ Runner.Arch.show_uniform arch)
        ~description:
          ("Build internal CI images for " ^ Runner.Arch.show_uniform arch)
        ~ci_docker_hub:false
        ~artifacts:
          (artifacts ~reports:(reports ~dotenv:"ci_image_tag.env" ()) [])
        ["./images/ci_create_ci_images.sh"]

    let mk_ci_image ~image_path =
      Image.mk_internal
        ~image_builder_amd64:(job_docker_ci Amd64 ())
        ~image_builder_arm64:(job_docker_ci Arm64 ~storage:Ramfs ())
        ~image_path
        ()

    (* To use static images from the protected registry. *)
    let mk_ci_image_master name =
      Image.mk_external
        ~image_path:("${ci_image_name_protected}/" ^ name ^ ":amd64--master")

    (* Reuse the same image_builder job [job_docker_ci] for all
       the below images, since they're all produced in that same job.

       Depending on any of these images ensures that the job
       [job_docker_ci] is included exactly once in the pipeline. *)
    let runtime =
      mk_ci_image ~image_path:"${ci_image_name}/runtime:${ci_image_tag}"

    let monitoring =
      mk_ci_image ~image_path:"${ci_image_name}/monitoring:${ci_image_tag}"

    let prebuild =
      mk_ci_image ~image_path:"${ci_image_name}/prebuild:${ci_image_tag}"

    let build = mk_ci_image ~image_path:"${ci_image_name}/build:${ci_image_tag}"

    let test = mk_ci_image ~image_path:"${ci_image_name}/test:${ci_image_tag}"

    let e2etest =
      mk_ci_image ~image_path:"${ci_image_name}/e2etest:${ci_image_tag}"

    let release_page =
      mk_ci_image ~image_path:"${ci_image_name}/release-page:${ci_image_tag}"

    (* Corresponding CI images from the protected registry using the [master] tags. We only define those used in [sanity] jobs. *)
    let prebuild_master = mk_ci_image_master "prebuild"

    let build_master = mk_ci_image_master "build"

    let test_master = mk_ci_image_master "test"
  end
end

(* OIDC tokens to be generated by GitLab *)
let id_tokens =
  [
    (* To get secrets from GCP Secret Manager, an OIDC token must be generated
       by GitLab and needs to setup an OIDC audience based on the target,
       here GCP Workload Identitfy Federation (WIF) *)
    ( "GCP_ID_TOKEN",
      Gitlab_ci.Types.Aud_string
        "https://iam.googleapis.com/projects/${GCP_WORKLOAD_IDENTITY_FEDERATION_PROJECT_ID}/locations/global/workloadIdentityPools/${GCP_WORKLOAD_IDENTITY_FEDERATION_POOL_ID}/providers/${GCP_WORKLOAD_IDENTITY_FEDERATION_PROVIDER_ID}"
    );
  ]

let job_datadog_pipeline_trace : tezos_job =
  job
    ~__POS__
    ~allow_failure:Yes
    ~name:"datadog_pipeline_trace"
    ~image:Images.datadog_ci
    ~stage:Stages.start
    [
      "CI_MERGE_REQUEST_IID=${CI_MERGE_REQUEST_IID:-none}";
      "DATADOG_SITE=datadoghq.eu datadog-ci tag --level pipeline --tags \
       pipeline_type:$PIPELINE_TYPE --tags mr_number:$CI_MERGE_REQUEST_IID";
    ]

let enable_kernels =
  append_variables
    [("CC", "clang"); ("NATIVE_TARGET", "x86_64-unknown-linux-musl")]
