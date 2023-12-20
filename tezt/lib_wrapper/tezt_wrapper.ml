(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Tezt
open Base

type error_mode = Ignore | Warn | Fail

let error_mode_for_missing_use = ref Fail

let error_mode_for_useless_use = ref Warn

module Uses = struct
  type t = {tag : string; path : string}

  (* Filled by [make] and read by [lookup]. *)
  let known_paths : t String_map.t ref = ref String_map.empty

  let canonicalize_path path =
    String.split_on_char '/' path
    |> List.filter (function "" | "." -> false | _ -> true)
    |> String.concat "/"

  let add_to_known_paths path uses =
    let path = canonicalize_path path in
    known_paths :=
      String_map.update
        path
        (function None -> Some uses | Some _ as x -> x)
        !known_paths

  let make ~tag ~path =
    let uses = {tag; path} in
    add_to_known_paths path uses ;
    uses

  let path_handler : (t -> unit) ref = ref (fun _ -> ())

  let path uses =
    !path_handler uses ;
    uses.path

  let tag uses = uses.tag

  let lookup path = String_map.find_opt (canonicalize_path path) !known_paths

  let octez_node = make ~tag:"node" ~path:"./octez-node"

  let octez_client = make ~tag:"client" ~path:"./octez-client"

  let octez_admin_client = make ~tag:"admin_client" ~path:"./octez-admin-client"

  (* The following test generates:

       tezt/lib_wrapper/expected/tezt_wrapper.ml/runtime-dependency-tags.out

     This file can be used by the manifest to deduce which tests to run.
     By having this be a regression test, we guarantee that this list is always up-to-date
     and that there is only one source of truth.

     To regenerate the file, run:

       dune exec tezt/tests/main.exe -- \
         --reset-regressions -t 'meta: list runtime dependencies'
  *)
  let () =
    Regression.register
      ~__FILE__
      ~title:"meta: list runtime dependencies"
      ~file:"runtime-dependency-tags"
      ~tags:["meta"; "uses"]
    @@ fun () ->
    ( Fun.flip String_map.iter !known_paths @@ fun path {tag; _} ->
      Regression.capture @@ sf "%s: %s" tag path ) ;
    unit
end

let error mode =
  Printf.ksprintf @@ fun message ->
  match mode with
  | Ignore -> ()
  | Warn -> Log.warn "%s" message
  | Fail -> Test.fail "%s" message

(* Prepare parameters of a [Test.register]-like function. *)
let wrap ~file ~title ~tags ?(uses = []) ?(uses_node = true)
    ?(uses_client = true) ?(uses_admin_client = true) ~run_test () =
  (* Add default uses. *)
  let uses = if uses_node then Uses.octez_node :: uses else uses in
  let uses = if uses_client then Uses.octez_client :: uses else uses in
  let uses =
    if uses_admin_client then Uses.octez_admin_client :: uses else uses
  in
  (* Add [uses] into [tags]. *)
  let uses_tags =
    String_set.of_list (List.map (fun (uses : Uses.t) -> uses.tag) uses)
  in
  let all_tags = String_set.union (String_set.of_list tags) uses_tags in
  (* Wrap [run_test] to check tags.
     In the future, we could use [Unix.chroot] to implement a sandbox
     that prevents using files that were not declared in [uses].
     Or even just [Unix.chdir], if we assume that we only use relative paths,
     in which case we need to override [Tezt.Base.project_root] though.
     [chdir] would let tests use [/tmp] and system executables more easily. *)
  let unused_uses_tags = ref uses_tags in
  let run_test () =
    (* Set hook so that tests can only call [Uses.path] on allowed paths. *)
    (Uses.path_handler :=
       fun uses ->
         (* Do *not* use [Uses.path uses] here; use [uses.path].
            Otherwise you'll get a stack overflow as the hook will trigger itself. *)
         unused_uses_tags := String_set.remove uses.tag !unused_uses_tags ;
         if not (String_set.mem uses.tag uses_tags) then
           error
             !error_mode_for_missing_use
             "In %S, test %S is not allowed to use %S. Try to add '%s' to its \
              ~uses."
             file
             title
             uses.path
             uses.tag) ;
    (* Actually run the test. *)
    let* () = run_test () in
    (* Check for unused tags. *)
    String_set.iter
      (error
         !error_mode_for_useless_use
         "In %S, test %S was declared with '%s' in its ~uses but did not call \
          Uses.path on it."
         file
         title)
      !unused_uses_tags ;
    unit
  in
  (* Return information that will be needed to register tests. *)
  (String_set.elements all_tags, run_test)

module Test = struct
  include Test

  let register ~__FILE__:file ~title ~tags ?uses ?uses_node ?uses_client
      ?uses_admin_client ?seed run_test =
    let tags, run_test =
      wrap
        ~file
        ~title
        ~tags
        ?uses
        ?uses_node
        ?uses_client
        ?uses_admin_client
        ~run_test
        ()
    in
    Test.register ~__FILE__:file ~title ~tags ?seed run_test
end

module Regression = struct
  include Regression

  let register ~__FILE__:file ~title ~tags ?uses ?uses_node ?uses_client
      ?uses_admin_client ?file:output_file run_test =
    let tags, run_test =
      wrap
        ~file
        ~title
        ~tags
        ?uses
        ?uses_node
        ?uses_client
        ?uses_admin_client
        ~run_test
        ()
    in
    Regression.register ~__FILE__:file ~title ~tags ?file:output_file run_test
end
