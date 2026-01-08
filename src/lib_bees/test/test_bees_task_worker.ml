(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_bees task workers
    Invocation:   dune exec src/lib_bees/test/main.exe \
                  -- --file test_bees_task_worker.ml
    Subject:      Unit tests for [Lib_bees] (task worker)
*)

(** Because our tests might be ran in the same process as other tests,
    we need to run our tests in a child process or next tests ran
    will fail if using [Unix.fork].
*)
let tztest label fn = Alcotezt_process.test_case_lwt label `Quick fn

let tests_fibonacci =
  let test_fibonacci domains =
    let fib () =
      let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2) in
      let input = Stdlib.List.init 5 (fun i -> i + 10) in
      let expected = List.map fib input in
      let output =
        Tezos_bees.Task_worker.launch_tasks_and_wait "fib" fib input
      in
      let output = List.filter_map Result.to_option output in
      Assert.equal_list expected output ;
      Lwt.return_ok ()
    in
    let name = Printf.sprintf "Fibonacci[11;12;13;14;15] %d domains" domains in
    tztest name fib
  in
  ("Fibonacci", [test_fibonacci 1; test_fibonacci 2; test_fibonacci 3])

let tests_reuse =
  (* Check that the same task worker can be used with totally different tasks and types. *)
  let test =
    tztest "reuse" @@ fun () ->
    let int_input = 0 in
    let int_expected = succ int_input in
    let int_output =
      Tezos_bees.Task_worker.launch_task_and_wait "succ(int)" succ int_input
    in
    let str_input = "0" in
    let succ s = int_of_string s |> succ |> string_of_int in
    let str_expected = succ str_input in
    let str_output =
      Tezos_bees.Task_worker.launch_task_and_wait "succ(str)" succ str_input
    in
    let int_output = Eio.Promise.await int_output in
    let str_output = Eio.Promise.await str_output in
    Assert.equal (Ok int_expected) int_output ;
    Assert.equal (Ok str_expected) str_output ;
    Lwt.return_ok ()
  in
  ("Reuse a worker for a different kind of computation", [test])

let tests_on_completion_callback =
  let test =
    tztest "on_completion_handler" @@ fun () ->
    let r = ref 0 in
    let noop () = () in
    let _ = Tezos_bees.Task_worker.launch_task_and_wait "callback" noop () in
    Assert.equal !r 0 ;
    let on_completion () = incr r in
    let tasks = 2 in
    let _ =
      Tezos_bees.Task_worker.launch_tasks_and_wait
        ~on_completion
        "callback"
        noop
        (Stdlib.List.init tasks (fun _ -> ()))
    in
    Assert.equal !r tasks ;
    Lwt.return_ok ()
  in
  ("Run on_completion callback", [test])

let () =
  Alcotezt_process.run
    ~__FILE__
    "Task worker"
    [tests_fibonacci; tests_reuse; tests_on_completion_callback]
