open Gitlab_ci.Util

let header =
  {|# This file was automatically generated, do not edit.
# Edit file ci/bin/main.ml instead.

|}

let () = Printexc.register_printer @@ function Failure s -> Some s | _ -> None

let failwith fmt = Format.kasprintf (fun s -> failwith s) fmt

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
  type t = {
    name : string;
    if_ : Gitlab_ci.If.t;
    variables : Gitlab_ci.Types.variables option;
    jobs : Gitlab_ci.Types.job list;
  }

  let pipelines : t list ref = ref []

  let filename : name:string -> string =
   fun ~name -> sf ".gitlab/ci/pipelines/%s.yml" name

  let register ?variables ?(jobs = []) name if_ =
    let pipeline : t = {variables; if_; name; jobs} in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7015
       check that stages have not been crossed. *)
    if List.exists (fun {name = name'; _} -> name' = name) !pipelines then
      failwith
        "[Pipeline.register] attempted to register pipeline %S twice"
        name
    else pipelines := pipeline :: !pipelines

  let all () = List.rev !pipelines

  (* Perform a set of static checks on the full pipeline before writing it. *)
  let precheck {name = pipeline_name; jobs; _} =
    let job_by_name : (string, Gitlab_ci.Types.job) Hashtbl.t =
      Hashtbl.create 5
    in
    (* Populate [job_by_name] and check that no two different jobs have the same name. *)
    List.iter
      (fun (job : Gitlab_ci.Types.job) ->
        match Hashtbl.find_opt job_by_name job.name with
        | None -> Hashtbl.add job_by_name job.name job
        | Some _ ->
            failwith
              "[%s] the job '%s' is included twice"
              pipeline_name
              job.name)
      jobs ;
    (* Check usage of [needs:] & [depends:] *)
    Fun.flip List.iter jobs @@ fun job ->
    (* Get the [needs:] / [dependencies:] of job *)
    let opt_set l = String_set.of_list (Option.value ~default:[] l) in
    let needs =
      (* The mandatory set of needs *)
      job.needs |> Option.value ~default:[]
      |> List.filter_map (fun Gitlab_ci.Types.{job; optional} ->
             if not optional then Some job else None)
      |> String_set.of_list
    in
    let dependencies = opt_set job.dependencies in
    (* Check that dependencies are a subset of needs.
       Note: this is already enforced by the smart constructor {!job}
       defined below. Is it redundant? Nothing enforces the usage of
       this smart constructor at this point.*)
    String_set.iter
      (fun dependency ->
        if not (String_set.mem dependency needs) then
          failwith
            "[%s] the job '%s' has a [dependency:] on '%s' which is not \
             included in it's [need:]"
            pipeline_name
            job.name
            dependency)
      dependencies ;
    (* Check that needed jobs (which thus includes dependencies) are defined *)
    ( Fun.flip String_set.iter needs @@ fun need ->
      match Hashtbl.find_opt job_by_name need with
      | Some _needed_job ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/7015
             check rule implication *)
          ()
      | None ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/7015
             handle optional needs *)
          failwith
            "[%s] job '%s' has a need on '%s' which is not defined in this \
             pipeline."
            pipeline_name
            job.name
            need ) ;
    (* Check that all [dependencies:] are on jobs that produce artifacts *)
    ( Fun.flip String_set.iter dependencies @@ fun dependency ->
      match Hashtbl.find_opt job_by_name dependency with
      | Some {artifacts = Some {paths = _ :: _; _}; _}
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

  let write () =
    all ()
    |> List.iter @@ fun ({name; jobs; _} as pipeline) ->
       if not (Sys.getenv_opt "CI_DISABLE_PRECHECK" = Some "true") then
         precheck pipeline ;
       match jobs with
       | [] -> ()
       | _ :: _ ->
           let filename = filename ~name in
           let config = List.map (fun j -> Gitlab_ci.Types.Job j) jobs in
           Gitlab_ci.To_yaml.to_file ~header ~filename config

  let workflow_includes () :
      Gitlab_ci.Types.workflow * Gitlab_ci.Types.include_ list =
    let workflow_rule_of_pipeline = function
      | {name; if_; variables; jobs = _} ->
          (* Add [PIPELINE_TYPE] to the variables of the workflow rules, so
             that it can be added to the pipeline [name] *)
          let variables =
            ("PIPELINE_TYPE", name) :: Option.value ~default:[] variables
          in
          workflow_rule ~if_ ~variables ~when_:Always ()
    in
    let include_of_pipeline = function
      | {name; if_; variables = _; jobs = _} ->
          (* Note that variables associated to the pipeline are not
             set in the include rule, they are set in the workflow
             rule *)
          let rule = include_rule ~if_ ~when_:Always () in
          Gitlab_ci.Types.{local = filename ~name; rules = [rule]}
    in
    let pipelines = all () in
    let workflow =
      let rules = List.map workflow_rule_of_pipeline pipelines in
      Gitlab_ci.Types.{rules; name = Some "[$PIPELINE_TYPE] $CI_COMMIT_TITLE"}
    in
    let includes = List.map include_of_pipeline pipelines in
    (workflow, includes)
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

type arch = Amd64 | Arm64

type dependency =
  | Job of Gitlab_ci.Types.job
  | Optional of Gitlab_ci.Types.job
  | Artifacts of Gitlab_ci.Types.job

type dependencies =
  | Staged of Gitlab_ci.Types.job list
  | Dependent of dependency list

type git_strategy = Fetch | Clone | No_strategy

let enc_git_strategy = function
  | Fetch -> "fetch"
  | Clone -> "clone"
  | No_strategy -> "none"

let job ?arch ?after_script ?allow_failure ?artifacts ?before_script ?cache
    ?interruptible ?(dependencies = Staged []) ?services ?variables ?rules
    ?timeout ?tags ?git_strategy ?when_ ?coverage ?retry ?parallel ~image ~stage
    ~name script : Gitlab_ci.Types.job =
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
  (match parallel with
  | Some n when n < 2 ->
      failwith
        "[job] the argument [parallel] must be at least 2 or omitted, in job \
         '%s'."
        name
  | _ -> ()) ;
  let needs, dependencies =
    let expand_job = function
      | Gitlab_ci.Types.{name; parallel; _} -> (
          match parallel with
          | None -> [name]
          | Some n -> List.map (fun i -> sf "%s %d/%d" name i n) (range 1 n))
    in
    match dependencies with
    | Staged dependencies -> (None, List.concat_map expand_job dependencies)
    | Dependent dependencies ->
        let rec loop (needs, dependencies) = function
          | dep :: deps ->
              let job_expanded =
                match dep with
                | Job j | Optional j | Artifacts j -> List.rev (expand_job j)
              in
              let needs ~optional =
                List.map
                  (fun name -> Gitlab_ci.Types.{job = name; optional})
                  job_expanded
                @ needs
              in
              let needs, dependencies =
                match dep with
                | Job _ -> (needs ~optional:false, dependencies)
                | Optional _ -> (needs ~optional:true, dependencies)
                | Artifacts _ ->
                    (needs ~optional:false, job_expanded @ dependencies)
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
