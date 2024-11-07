open Gitlab_ci.Util

let failwith fmt = Format.kasprintf (fun s -> failwith s) fmt

module Cli = struct
  type action = Write | List_pipelines

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
          ( "--list-pipelines",
            Arg.Unit (fun () -> config.action <- List_pipelines),
            " List registered pipelines." );
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

type tezos_image =
  | Internal of {
      image : Gitlab_ci.Types.image;
      builder_amd64 : tezos_job;
      builder_arm64 : tezos_job option;
    }
  | External of Gitlab_ci.Types.image

and tezos_job = {
  job : Gitlab_ci.Types.generic_job;
  source_position : string * int * int * int;
  stage : Stage.t;
  image_builders : tezos_job list;
}

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

  let register_child ?auto_cancel ~description ~jobs name =
    let child_pipeline = {name; jobs; auto_cancel; description} in
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
      Gitlab_ci.Types.
        {
          rules;
          name = Some "[$PIPELINE_TYPE] $CI_COMMIT_TITLE";
          auto_cancel = None;
        }
    in
    let includes = List.map include_of_pipeline pipelines in
    (workflow, includes)

  let write ?default ?variables ~filename () =
    (* Write all registered pipelines *)
    ( Fun.flip List.iter (all ()) @@ fun pipeline ->
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
      let stages =
        let stages : (string, int) Hashtbl.t = Hashtbl.create 5 in
        List.iter
          (fun job ->
            Hashtbl.replace
              stages
              (Stage.name job.stage)
              (Stage.index job.stage))
          jobs ;
        Hashtbl.to_seq stages |> List.of_seq
        |> List.sort (fun (_n1, idx1) (_n2, idx2) -> Int.compare idx1 idx2)
        |> List.map fst
      in
      let prepend_config =
        (match pipeline with
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
                Variables [("PIPELINE_TYPE", name)];
              ])
        @ [Stages stages]
      in
      let config =
        prepend_config @ List.concat_map tezos_job_to_config_elements jobs
      in
      to_file ~filename config ) ;
    (* Write top-level configuration. *)
    let workflow, includes = workflow_includes () in
    (* temporary manual addition *)
    let ci_docker_branch = "ci-docker-latest-release" in
    let ci_docker_if_expr =
      Gitlab_ci.If.(
        var "CI_PROJECT_NAMESPACE" == str "tezos"
        && var "CI_PIPELINE_SOURCE" == str "push"
        && var "CI_COMMIT_BRANCH" == str ci_docker_branch)
    in
    let workflow =
      let ci_docker_workflow_rule : Gitlab_ci.Types.workflow_rule =
        {
          changes = None;
          if_ = Some ci_docker_if_expr;
          variables = Some [("PIPELINE_TYPE", "ci_docker_release")];
          when_ = Always;
          auto_cancel = None;
        }
      in
      {workflow with rules = ci_docker_workflow_rule :: workflow.rules}
    in
    let includes =
      let ci_docker_include_rule : Gitlab_ci.Types.include_ =
        {
          local = "images_base/ci-docker/.gitlab-ci.yml";
          rules =
            [{changes = None; if_ = Some ci_docker_if_expr; when_ = Always}];
        }
      in
      ci_docker_include_rule :: includes
    in
    (* end of temporary manual addition *)
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
    to_file ~filename config ;
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

let dynamic_tag_var = Gitlab_ci.Var.make "TAGS"

type tag =
  | Gcp
  | Gcp_arm64
  | Gcp_dev
  | Gcp_dev_arm64
  | Gcp_tezt
  | Gcp_tezt_dev
  | Gcp_tezt_memory_3k
  | Gcp_tezt_memory_3k_dev
  | Gcp_tezt_memory_4k
  | Gcp_tezt_memory_4k_dev
  | Aws_specific
  | Dynamic

let string_of_tag = function
  | Gcp -> "gcp"
  | Gcp_arm64 -> "gcp_arm64"
  | Gcp_dev -> "gcp_dev"
  | Gcp_dev_arm64 -> "gcp_dev_arm64"
  | Gcp_tezt -> "gcp_tezt"
  | Gcp_tezt_dev -> "gcp_tezt_dev"
  | Gcp_tezt_memory_3k -> "gcp_tezt_memory_3k"
  | Gcp_tezt_memory_3k_dev -> "gcp_tezt_memory_3k_dev"
  | Gcp_tezt_memory_4k -> "gcp_tezt_memory_4k"
  | Gcp_tezt_memory_4k_dev -> "gcp_tezt_memory_4k_dev"
  | Aws_specific -> "aws_specific"
  | Dynamic -> Gitlab_ci.Var.encode dynamic_tag_var

(** The architecture of the runner associated to a tag . *)
let arch_of_tag = function
  | Gcp_arm64 | Gcp_dev_arm64 -> Some Arm64
  | Gcp | Gcp_dev | Gcp_tezt | Gcp_tezt_dev | Gcp_tezt_memory_3k
  | Gcp_tezt_memory_3k_dev | Gcp_tezt_memory_4k | Gcp_tezt_memory_4k_dev
  | Aws_specific ->
      Some Amd64
  | Dynamic -> None

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

let job ?arch ?after_script ?allow_failure ?artifacts ?before_script ?cache
    ?interruptible ?(dependencies = Staged []) ?(image_dependencies = [])
    ?services ?variables ?rules ?(timeout = Gitlab_ci.Types.Minutes 60) ?tag
    ?git_strategy ?coverage ?retry ?parallel ~__POS__ ~image ~stage ~name script
    : tezos_job =
  (* The tezos/tezos CI uses singleton tags for its runners. *)
  let tag =
    match (arch, tag) with
    | Some arch, None -> ( match arch with Amd64 -> Gcp | Arm64 -> Gcp_arm64)
    | None, Some tag -> tag
    | None, None ->
        (* By default, we assume Amd64 runners as given by the [gcp] tag. *)
        Gcp
    | Some _, Some _ ->
        failwith
          "[job] cannot specify both [arch] and [tags] at the same time in job \
           '%s'."
          name
  in
  if rules == Some [] then
    failwith "The job '%s' cannot have empty [rules]." name ;
  let arch = if Option.is_some arch then arch else arch_of_tag tag in
  (match (image, arch) with
  | Internal {image = Image image_path; _}, None ->
      failwith
        "[job] the job '%s' has tag '%s' whose architecture is not statically \
         known cannot use the image '%s' since it is Internal. Set a static \
         tag or use an external image."
        name
        (string_of_tag tag)
        image_path
  | _ -> ()) ;
  let tags = Some [string_of_tag tag] in
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
    (match image with Internal _ -> [image] | External _ -> [])
    @ image_dependencies
  in
  let dependencies, image_builders =
    (Fun.flip List.iter image_dependencies @@ function
     | External (Image image_path) ->
         failwith
           "It doesn't make any sense for job %s to depend on the external \
            image %s"
           name
           image_path
     | Internal _ -> ()) ;
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
  | Some "nomadic-margebot", (Gcp_dev | Gcp_dev_arm64) ->
      failwith
        "[job] Attempting to merge a CI configuration using development \
         runners (job: %s)"
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
      image = Some (Image.image image);
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
      stage = Some (Stage.name stage);
      variables;
      timeout = Some timeout;
      tags;
      when_ = None;
      coverage;
      retry;
      parallel;
    }
  in
  {job = Job job; source_position = __POS__; stage; image_builders}

let trigger_job ?(dependencies = Staged []) ?rules ~__POS__ ~stage
    Pipeline.
      {name = child_pipeline_name; jobs = _; auto_cancel = _; description = _} :
    tezos_job =
  let job_name = "trigger:" ^ child_pipeline_name in
  let needs, dependencies = resolve_dependencies job_name dependencies in
  if dependencies != [] then
    failwith
      "[trigger_job] trigger job '%s' has artifact-dependencies, which is not \
       allowed by GitLab CI."
      job_name ;
  let trigger_job =
    Gitlab_ci.Util.trigger_job
      ?needs
      ?rules
      ~stage:(Stage.name stage)
      ~name:job_name
      (Pipeline.path ~name:child_pipeline_name)
  in
  {
    job = Trigger_job trigger_job;
    source_position = __POS__;
    stage;
    image_builders = [];
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
