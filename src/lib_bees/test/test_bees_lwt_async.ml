(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_bees async_lwt
    Invocation:   dune exec src/lib_bees/test/main.exe \
                  -- --file test_bees_lwt_async.ml
    Subject:      Unit tests for [Lib_bees] async_lwt promise processing
*)

let test_async_lwt_promise_processing () =
  let open Result_syntax in
  (* Test that async_lwt processes promises.
     This test schedules multiple async Lwt tasks and verifies they all complete. *)
  let results = ref 0 in
  let num_tasks = 5 in
  (* Schedule multiple Lwt tasks *)
  for _ = 1 to num_tasks do
    Tezos_bees.Hive.async_lwt (fun () ->
        incr results ;
        Lwt.return_unit)
  done ;
  (* Give the lwt scheduler time to process all tasks *)
  let env = Tezos_base_unix.Event_loop.env_exn () in
  Eio.Time.sleep env#clock 1. ;
  if !results = num_tasks then return_unit
  else
    Alcotest.failf
      "async_lwt task results mismatch: got %d, expected %d"
      !results
      num_tasks

let tests_async_lwt =
  ( "Async Lwt",
    [
      Alcotezt_process.test_case
        "Promise processing (eio handlers)"
        `Quick
        test_async_lwt_promise_processing;
    ] )

let () = Alcotezt_process.run ~__FILE__ "Bees async_lwt" [tests_async_lwt]
