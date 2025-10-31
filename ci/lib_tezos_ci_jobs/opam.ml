(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Jobs testing the installability of the Octez opam packages.

    The opam packages are split into two {i groups}: those installing
    executables and the other jobs. The former group is tested more
    often (being typically the leaves the dependency graph of opam
    packages).

    Due to the large number of packages, the opam testing jobs are
    launched in a series of {i batches}. Jobs are grouped into batches as
    per their expected duration. The expected duration is based on the
    depth of it's dependency tree. Thus, jobs testing packages with a
    deeper dependency tree are launched in the first batch (the
    [batch_index] of all those jobs is 1).

    The set of opam packages, their group, and the index of its batch,
    is defined by manifest and written to the TSV file
    [script-inputs/ci-opam-package-tests]. *)

open Gitlab_ci.Types
open Gitlab_ci.Util
open Tezos_ci
open Tezos_ci.Cache
open Common.Helpers

(** Opam package group.

      Opam jobs are split into two groups:
      - [Executable]: those that install an executable, e.g. [octez-client].
      - [All]: remaining packages, e.g. [octez-libs]. *)
type opam_package_group = Executable | All

(** Opam package meta-data.

      An opam testing job is characterized by the package name, its
      group and the index of the batch in which it will execute. *)
type opam_package = {
  name : string;
  group : opam_package_group;
  batch_index : int;
}

(** Opam packages and their meta data.

      This is read from the file [script-inputs/ci-opam-package-tests],
      which is written by manifest. *)
let opam_packages =
  let ci_opam_package_tests = "script-inputs/ci-opam-package-tests" in
  Fun.flip List.filter_map (read_lines_from_file ci_opam_package_tests)
  @@ fun line ->
  let fail () =
    failwith
      (sf "failed to parse %S: invalid line: %S" ci_opam_package_tests line)
  in
  if line = "" then None
  else
    match String.split_on_char '\t' line with
    | [name; group; batch_index] ->
        let batch_index =
          match int_of_string_opt batch_index with
          | Some i -> i
          | None -> fail ()
        in
        let group =
          match group with "exec" -> Executable | "all" -> All | _ -> fail ()
        in
        Some {name; group; batch_index}
    | _ -> fail ()

(** Constructs the opam package job for a given group and batch.

      Separate packages with the same group and batch are tested in
      separate jobs, which is implemented through parallel-matrix
      jobs. *)
let job_opam_packages ?dependencies group batch_index packages : tezos_job =
  job
    ?dependencies
    ~__POS__
    ~name:
      ("opam:"
      ^ (match group with All -> "all" | Executable -> "exec")
      ^ "_" ^ string_of_int batch_index)
    ~image:Images.CI.prebuild
    ~stage:Stages.packaging
      (* FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/663
           FIXME: https://gitlab.com/nomadic-labs/tezos/-/issues/664
           At the time of writing, the opam tests were quite flaky.
           Therefore, a retry was added. This should be removed once the
           underlying tests have been fixed. *)
    ~timeout:(Minutes 90)
    ~rules:[job_rule ~when_:(Delayed (Minutes batch_index)) ()]
    ~variables:
      (* See [variables] in [main.ml] for details on [RUNTEZTALIAS] *)
      [("RUNTEZTALIAS", "true")]
    ~parallel:(Matrix [[("package", packages)]])
    ~before_script:
      (before_script ~eval_opam:true ["mkdir -p $CI_PROJECT_DIR/opam_logs"])
    [
      "opam remote add dev-repo ./_opam-repo-for-release";
      "opam install --yes ${package}.dev";
      "opam reinstall --yes --with-test ${package}.dev";
    ]
    (* Stores logs in opam_logs for artifacts and outputs an excerpt on
         failure. [after_script] runs in a separate shell and so requires
         a second opam environment initialization. *)
    ~after_script:
      [
        "eval $(opam env)";
        "OPAM_LOGS=opam_logs ./scripts/ci/opam_handle_output.sh";
      ]
    ~artifacts:
      (artifacts ~expire_in:(Duration (Weeks 1)) ~when_:Always ["opam_logs/"])
  |>
  (* We store caches in [_build] for two reasons: (1) the [_build]
          folder is excluded from opam's rsync. (2) gitlab ci cache
          requires that cached files are in a sub-folder of the checkout. *)
  enable_sccache
    ~key:"opam-sccache"
    ~error_log:"$CI_PROJECT_DIR/opam_logs/sccache.log"
    ~idle_timeout:"0"
    ~path:"$CI_PROJECT_DIR/_build/_sccache"
  |> enable_cargo_cache

let jobs_opam_packages ?dependencies () : tezos_job list =
  let package_by_group_index = Hashtbl.create 5 in
  List.iter
    (fun pkg ->
      let key = (pkg.group, pkg.batch_index) in
      let packages =
        Option.value ~default:[] @@ Hashtbl.find_opt package_by_group_index key
      in
      Hashtbl.replace package_by_group_index key (pkg.name :: packages))
    opam_packages ;
  (* The [opam:prepare] job creates a local opam-repository from
       which installability is tested. This repository is transferred
       as an artifact to the opam package jobs.

       Note: this preliminary step is very quick and could be folded
       into the opam package jobs. *)
  let job_prepare =
    job
      ~__POS__
      ~name:"opam:prepare"
      ~image:Images.CI.prebuild
      ~stage:Stages.packaging
      ?dependencies
      ~before_script:(before_script ~eval_opam:true [])
      ~artifacts:(artifacts ["_opam-repo-for-release/"])
      ~rules:[job_rule ~when_:(Delayed (Minutes 1)) ()]
      [
        "git init _opam-repo-for-release";
        "./scripts/opam-prepare-repo.sh dev ./ ./_opam-repo-for-release";
        "git -C _opam-repo-for-release add packages";
        "git -C _opam-repo-for-release commit -m \"tezos packages\"";
      ]
  in
  job_prepare
  :: Hashtbl.fold
       (fun (group, index) packages jobs ->
         let dependencies = Dependent [Artifacts job_prepare] in
         job_opam_packages ~dependencies group index packages :: jobs)
       package_by_group_index
       []
