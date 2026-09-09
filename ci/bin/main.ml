(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint of CI-in-OCaml.

   Here we register the set of pipelines, stages and images and
   generate the GitLab CI configuration file. *)

open Gitlab_ci
open Gitlab_ci.Types
open Tezos_ci

let () = Tezos_ci.Cli.init ()

(* Top-level [variables:] *)
let variables : variables =
  [
    (* /!\ [GCP_PUBLIC_REGISTRY] contains the name of the PUBLIC
       registry to and from which Docker images are produced and
       consumed. This variable is defined at the tezos-group level and
       always contains the path to the unprotected Docker registry
       (unlike [GCP_REGISTRY], see below). This is used to locate the
       CI images, which are always pushed to the public repository. *)
    (* /!\ GCP_REGISTRY is the variable containing the name of the registry to and from
       which docker images are produced and consumed. This variable is defined at tezos
       level with the value unprotected registry and at tezos/tezos level in its protected
       version. This mechanism allows pipelines from a protected tezos/tezos branch to
       read the protected variable from tezos/tezos and for others to not have access to
       the variable tezos/tezos but tezos. *)
    ("ci_image_name", "${GCP_REGISTRY}/${CI_PROJECT_PATH}/ci");
    ( "ci_image_name_protected",
      "${GCP_PROTECTED_REGISTRY}/${CI_PROJECT_PATH}/ci" );
    ("GIT_STRATEGY", "fetch");
    ("GIT_DEPTH", "1");
    ("GET_SOURCES_ATTEMPTS", "2");
    ("ARTIFACT_DOWNLOAD_ATTEMPTS", "2");
    (* Sets the number of tries before failing opam downloads. *)
    ("OPAMRETRIES", "5");
    (* An addition to working around a bug in gitlab-runner's default
       unzipping implementation
       (https://gitlab.com/gitlab-org/gitlab-runner/-/issues/27496),
       this setting cuts cache creation time. *)
    ("FF_USE_FASTZIP", "true");
    (* If RUNTEZTALIAS is true, then Tezt tests are included in the
       @runtest alias. We set it to false to deactivate these tests in
       the unit test jobs, as they already run in the Tezt jobs. It is
       set to true in the opam jobs where we want to run the tests
       --with-test. *)
    ("RUNTEZTALIAS", "false");
    ("CARGO_HOME", Cargo.home);
    (* To avoid Cargo accessing the network in jobs without caching (see
       {!Common.enable_cargo_cache}), we turn off net access by default. *)
    ("CARGO_NET_OFFLINE", "true");
    (* Reduce the verbosity of Cargo. *)
    ("CARGO_TERM_QUIET", "true");
    (* Enable timestamps for each line in job logs.

       https://docs.gitlab.com/ee/ci/yaml/ci_job_log_timestamps.html *)
    ("FF_TIMESTAMPS", "true");
  ]

(** {2 Pipeline types} *)

(* Register pipelines types. Pipelines types are used to generate
   workflow rules and includes of the files where the jobs of the
   pipeline is defined.

   Please add a [~description] to each pipeline.

   The first sentence of the description should be short (<=80
   characters), and be terminated by two new-lines. It should
   describe _what_ the pipeline does.

   The remainder of the description should detail:
   - what the pipeline does;
   - why we do it;
   - when it happens;
   - how;
   - and by whom it is triggered (a developer? a release manager? some automated system?). *)

(** {3 Components} *)

(* This must be done before registering shared pipelines. *)

let () = Release_site_ci.register ()

let () = Grafazos_ci.register ()

let () = Teztale_ci.register ()

let () = Rollup_node_ci.register ()

let () = Documentation_ci.register ()

let () = Etherlink_ci.register ()

let () = Tezos_ci_jobs.Sanity.register ()

let () = Tezos_ci_jobs.Build.register ()

let () = Tezos_ci_jobs.Misc.register ()

let () = Tezos_ci_jobs.Kernels.register ()

let () = Tezos_ci_jobs.Tezos_x.register ()

let () = Tezos_ci_jobs.Tezt.register ()

let () = Tezos_ci_jobs.Installation.register ()

let () = Tezos_ci_jobs.Docker.register ()

let () = Tezos_ci_jobs.Security_scans.register ()

let () = Release_tag.register ()

let () = Sdk_bindings_ci.register ()

(** {3 General pipelines} *)

let () =
  let open Rules in
  let open Pipeline in
  register
    "before_merging"
    If.(merge_request && not merge_train)
    ~jobs:(Cacio.get_jobs Before_merging)
    ~description:
      "Lints code in merge requests, checks that it compiles and runs tests.\n\n\
       This pipeline is created on each push to a branch with an associated \
       open merge request, typically by the developer. It runs sanity checks, \
       linters and checks that code of the MR compiles and that the tests \
       pass. Must be manually started through the job 'trigger'." ;
  register
    "merge_train"
    ~auto_cancel:{on_job_failure = true; on_new_commit = false}
    If.(on_tezos_namespace && merge_request && merge_train)
    ~jobs:(Cacio.get_jobs Merge_train)
    ~description:
      "A merge-train-specific version of 'before_merging'.\n\n\
       This pipeline contains the same set of jobs as 'before_merging' but \
       with auto-cancelling enabled on job failures. That is, if one job in \
       the pipeline fails, the full pipeline is cancelled. This ensures that \
       pipelines running in a merge train, that are bound to fail (due to some \
       failing job), does so as early as possible. This prevents unneccessary \
       delays in merging MRs further down the train.\n\n\
       The merge train pipeline is created by GitLab when a merge request is \
       added to the merge train (typically by marge-bot)."

(** {2 Closing the set of pipelines} *)

(* Register the pipelines that are defined with [Cacio.new_global_pipeline].
   No job and no pipeline can be registered after this point. *)
let () = Cacio.close ()

(** {2 Entry point of the generator binary} *)

let () =
  (* If argument --verbose is set, then log generation info.
     If argument --inline-source, then print generation info in yml files. *)
  match Cli.config.action with
  | Write ->
      Pipeline.write
        ~default:Pipeline.default_config
        ~variables
        ~filename:".gitlab-ci.yml"
        () ;
      Tezos_ci.check_files ~remove_extra_files:Cli.config.remove_extra_files () ;
      if Cli.config.verbose then (
        let ciao_jobs = Tezos_ci.get_declared_jobs () in
        let cacio_jobs = Cacio.get_declared_jobs () in
        let non_migrated_jobs =
          (* Remove [cacio_jobs] from [ciao_jobs] to get [non_migrated_jobs]. *)
          String_map.merge
            (fun _ a b ->
              match (a, b) with
              | None, _ | _, Some _ -> None
              | (Some _ as x), None -> x)
            ciao_jobs
            cacio_jobs
        in
        print_endline "CACIO MIGRATION" ;
        String_map.iter
          (fun name (file, line, _, _) ->
            Printf.printf "%s:%d: not migrated: %s\n" file line name)
          non_migrated_jobs ;
        (* Note: [Tezos_ci] jobs include [Cacio] jobs, since [Cacio] registers
           jobs using [Tezos_ci.job]. *)
        Printf.printf
          "%d/%d jobs were defined using Cacio.\n%!"
          (String_map.cardinal cacio_jobs)
          (String_map.cardinal ciao_jobs))
  | List_pipelines -> Pipeline.list_pipelines ()
  | Overview_pipelines -> Pipeline.overview_pipelines ()
  | Describe_pipeline {name} -> Pipeline.describe_pipeline name

let () = Cacio.output_tezt_job_list "script-inputs/cacio-tezt-jobs"
