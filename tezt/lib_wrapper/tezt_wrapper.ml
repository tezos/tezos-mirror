(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Tezt
open Base

module Uses = struct
  type t = {tag : string; path : string}

  let make ~tag ~path = {tag; path}

  let path_handler : (t -> unit) ref = ref (fun _ -> ())

  let path uses =
    !path_handler uses ;
    uses.path

  let tag uses = uses.tag
end

(* Prepare parameters of a [Test.register]-like function. *)
let wrap ~file ~title ~tags ~uses ~run_test =
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
           Log.warn
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
      (Log.warn
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

  let register ~__FILE__:file ~title ~tags ?(uses = []) ?seed run_test =
    let tags, run_test = wrap ~file ~title ~tags ~uses ~run_test in
    Test.register ~__FILE__:file ~title ~tags ?seed run_test
end

module Regression = struct
  include Regression

  let register ~__FILE__:file ~title ~tags ?(uses = []) ?file:output_file
      run_test =
    let tags, run_test = wrap ~file ~title ~tags ~uses ~run_test in
    Regression.register ~__FILE__:file ~title ~tags ?file:output_file run_test
end
