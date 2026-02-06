(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Smart rollup node
    Invocation:   dune exec src/lib_smart_rollup_node/test/main.exe
    Subject:      Test garbage collection for the smart rollup node context
*)

module Irmin_context = struct
  include Irmin_context

  let load ~cache_size path = load ~cache_size path
end

let commit_new_state context key =
  let open Lwt_syntax in
  let* state =
    Context.Internal_for_tests.get_a_tree (module Irmin_context) key
  in
  let* () = Context.PVMState.set context state in
  let* hash = Context.commit context in
  Lwt.return (context, hash)

(* Create a mock context, commit 3 states, and trigger garbage collection
 * for the first 2 hashes. Check that the first 2 hashes have been deleted
 * and that the third hash can be retrieved. *)
let test_gc data_dir =
  let open Lwt_result_syntax in
  let* context =
    Context.load
      (module Irmin_context)
      ~cache_size:Configuration.default_irmin_cache_size
      Read_write
      (Configuration.default_context_dir data_dir)
  in
  let context = Context.empty context in
  let*! context, hash1 = commit_new_state context "tree1" in
  let*! context, hash2 = commit_new_state context "tree2" in
  let*! context, hash3 = commit_new_state context "tree3" in
  let index = Context.index context in

  let*! b = Context.checkout index hash1 in
  assert (Option.is_some b) ;
  let*! b = Context.checkout index hash2 in
  assert (Option.is_some b) ;
  let*! b = Context.checkout index hash3 in
  assert (Option.is_some b) ;

  let*! () = Context.gc index hash3 in
  let*! () = Context.wait_gc_completion index in
  assert (Context.is_gc_finished index) ;

  let*! b = Context.checkout index hash1 in
  assert (Option.is_none b) ;
  let*! b = Context.checkout index hash2 in
  assert (Option.is_none b) ;
  let*! b = Context.checkout index hash3 in
  assert (Option.is_some b) ;
  Lwt.return_ok ()

(* adapted from lib_store/unix/test/test_locator.ml *)
let wrap n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      Lwt_utils_unix.with_tempdir "context_gc_test_" (fun dir ->
          let open Lwt_syntax in
          let* r = f dir in
          match r with
          | Ok () -> Lwt.return_unit
          | Error error ->
              Format.kasprintf Stdlib.failwith "%a" pp_print_trace error))

let tests = [wrap "Garbage collection" test_gc]

let () =
  Alcotest_lwt.run ~__FILE__ "lib_smart_rollup_node" [("Context", tests)]
  |> Lwt_main.run
