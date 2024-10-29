(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Base, Unix
   Invocation:   dune exec src/lib_base/unix/test/main.exe -- --file test_simple_profiler.ml
                 Use --verbose flag for detailed logs.
   Subject:      Testing simple profiler
*)
open Tezt
open Tezos_base.TzPervasives
open Profiler
module WithExceptions = Tezos_error_monad.TzLwtreslib.WithExceptions
open Lwt_syntax

let replace_variables string =
  let replacements =
    [
      ("  +\\d{1,3}\\.\\d{3}ms +\\d{1,3}% +\\+\\d{1,3}\\.\\d{3}ms", "[PROF]");
      ("  +\\+?\\d{1,3}\\.\\d{3}ms *\\d*%?", "[PROF]");
      ("20\\d{2}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}", "[TIMESTAMP]");
    ]
  in
  List.fold_left
    (fun string (replace, by) ->
      replace_string ~all:true (rex replace) ~by string)
    string
    replacements

let check_file_content file_path =
  let rec read_and_print file =
    try
      let line = input_line file in
      Log.info "%s" line ;
      Regression.capture (replace_variables line) ;
      read_and_print file
    with
    | End_of_file -> close_in file
    | ex ->
        close_in file ;
        raise ex
  in
  try
    let file = open_in file_path in
    read_and_print file
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg ;
    Sys.remove file_path

let noop profiler =
  Profiler.record profiler "noop" ;
  Profiler.stop profiler

let sleep10ms profiler =
  let tag = "sleep10ms" in
  Profiler.record profiler tag ;
  Log.info "%s" tag ;
  Unix.sleepf 0.01 ;
  noop profiler ;
  Profiler.stop profiler

let sleep70ms profiler =
  let tag = "sleep70" in
  Profiler.record profiler tag ;
  Log.info "%s" tag ;
  Unix.sleepf 0.07 ;
  Profiler.stop profiler

let foo profiler =
  Profiler.record profiler "foo" ;
  sleep10ms profiler ;
  sleep70ms profiler ;
  sleep10ms profiler ;
  Profiler.stop profiler

let bar profiler =
  Profiler.record profiler "bar" ;
  sleep70ms profiler ;
  sleep10ms profiler ;
  foo profiler ;
  foo profiler ;
  Profiler.stop profiler

let test70 profiler =
  Log.info "\nTEST: test 70\n" ;
  Profiler.record profiler "test70" ;
  bar profiler ;
  foo profiler ;
  Profiler.stop profiler

let sleep_until_next_second () =
  let current_time = Unix.gettimeofday () in
  let time_in_seconds = int_of_float current_time in
  let next_second = time_in_seconds + 1 in
  let remaining_time = float_of_int next_second -. current_time in
  Lwt_unix.sleep remaining_time

let slp tag profiler =
  let* () =
    Profiler.record_s profiler ("slp " ^ tag) (fun () ->
        Log.info "sleep in %s" tag ;
        let* () = Lwt_unix.sleep 0.01 in
        Log.info "sleep done" ;
        Lwt.return_unit)
  in
  return_unit

let sequences profiler =
  let a_seq = "a_seq" in
  let b_seq = "b_seq" in
  let b_a_seq = "b_a_seq" in
  let b_b_seq = "b_b_seq" in
  let* () = Profiler.record_s profiler a_seq (fun () -> slp a_seq profiler) in
  let* () =
    Profiler.record_s profiler b_seq (fun () ->
        let* () =
          Profiler.record_s profiler b_a_seq (fun () -> slp b_a_seq profiler)
        in
        let* () =
          Profiler.record_s profiler b_b_seq (fun () -> slp b_b_seq profiler)
        in
        slp b_seq profiler)
  in
  return_unit

let aggregates profiler =
  let a_aggr = "a_aggr" in
  let b_aggr = "b_aggr" in
  let a_a_aggr = "a_a_aggr" in
  let a_b_aggr = "a_b_aggr" in
  let a_c_aggr = "a_c_aggr" in
  let a_a_a_aggr = "a_a_a_aggr" in
  let* () =
    Profiler.aggregate_s profiler a_aggr (fun () -> slp a_aggr profiler)
  in
  let* () =
    Profiler.aggregate_s profiler b_aggr (fun () -> slp b_aggr profiler)
  in
  let* () =
    Profiler.aggregate_s profiler a_aggr (fun () ->
        let* () =
          Profiler.aggregate_s profiler a_a_aggr (fun () ->
              slp a_a_aggr profiler)
        in
        let* () =
          Profiler.aggregate_s profiler a_b_aggr (fun () ->
              slp a_b_aggr profiler)
        in
        let* () =
          Profiler.aggregate_s profiler a_c_aggr (fun () ->
              slp a_c_aggr profiler)
        in
        slp a_aggr profiler)
  in
  let* () =
    Profiler.aggregate_s profiler a_aggr (fun () ->
        let* () =
          Profiler.aggregate_s profiler a_a_aggr (fun () ->
              Profiler.aggregate_s profiler a_a_a_aggr (fun () ->
                  slp a_a_a_aggr profiler))
        in
        slp a_aggr profiler)
  in
  return_unit

let buggy_aggregates profiler =
  let bug_aggr_a = "bug_aggr_a" in
  let bug_aggr_b = "bug_aggr_b" in
  let bug_aggr_b_a = "bug_aggr_b_a" in
  let bug_aggr_b_b = "bug_aggr_b_b" in

  let t1 =
    Profiler.aggregate_s profiler bug_aggr_a (fun () -> slp bug_aggr_a profiler)
  in
  let t2 =
    Profiler.aggregate_s profiler bug_aggr_b (fun () ->
        let* () = slp bug_aggr_b_a profiler in
        slp bug_aggr_b_b profiler)
  in
  Lwt.join [t1; t2]

let spans profiler =
  let span1 = "span1" in
  let span2 = "span2" in
  let inner1 = "inner1" in
  let inner2 = "inner2" in
  let inner11 = "inner1/1" in
  let inner12 = "inner1/2" in
  let inner21 = "inner2/1" in
  let inner22 = "inner2/2" in
  let t1 = Profiler.span_s profiler [span1] (fun () -> slp span1 profiler) in
  let t2 =
    Profiler.span_s profiler [span2; inner1] (fun () ->
        let* () = slp (span2 ^ ", " ^ inner11) profiler in
        slp (span2 ^ ", " ^ inner12) profiler)
  in
  let t3 =
    Profiler.span_s profiler [span2; inner2] (fun () ->
        let* () = slp (span2 ^ ", " ^ inner21) profiler in
        slp (span2 ^ ", " ^ inner22) profiler)
  in
  Lwt.join [t1; t2; t3]

let sequences_with_mark_and_stamp profiler =
  let a_seq = "a_seq" in
  let b_seq = "b_seq" in
  let b_a_seq = "b_a_seq" in
  let b_b_seq = "b_b_seq" in
  let mark = "mark" in
  let stamp = "stamp" in
  let* () = Profiler.record_s profiler a_seq (fun () -> slp a_seq profiler) in
  let* () =
    Profiler.record_s profiler b_seq (fun () ->
        let* () =
          Profiler.record_s profiler b_a_seq (fun () -> slp b_a_seq profiler)
        in
        let () = List.iter (fun _ -> Profiler.mark profiler [mark]) (1 -- 10) in
        let () = Profiler.stamp profiler stamp in
        let* () =
          Profiler.record_s profiler b_b_seq (fun () -> slp b_b_seq profiler)
        in
        let () = List.iter (fun _ -> Profiler.mark profiler [mark]) (1 -- 10) in
        let () = Profiler.stamp profiler stamp in
        slp b_seq profiler)
  in
  return_unit

let test_profiler_actions profiler =
  Log.info "\nTEST: test_profiler_actions\n" ;
  let main () =
    let* () = sleep_until_next_second () in
    Profiler.record_s profiler "main" (fun () ->
        let* () = sequences profiler in
        let* () = aggregates profiler in
        let* () = buggy_aggregates profiler in
        let* () = spans profiler in
        let* () = sequences_with_mark_and_stamp profiler in
        Lwt.return_unit)
  in
  Lwt_main.run (main ())

let get_profiler file_name =
  let profiler = unplugged () in
  let test_profiler_instance =
    Profiler.instance
      (* The default driver is a text driver writing to a file
         without suffixing it *)
      Tezos_base_unix.Simple_profiler.auto_write_as_txt_to_file
      (file_name, Profiler.Info)
  in
  plug profiler test_profiler_instance ;
  profiler

let run_test_with_profiler test_name test_fn =
  let file_name = Temp.file ("output_test_simple_profiling_" ^ test_name) in
  let profiler = get_profiler file_name in

  test_fn profiler ;
  Log.info "\nProfiling result for %s" test_name ;
  Log.info "==================================" ;
  check_file_content file_name

let () =
  Tezt_core.Regression.register
    ~__FILE__
    ~title:"Simple profiler: basic profiler test"
    ~tags:["unix"; "profiler"]
  @@ fun () ->
  run_test_with_profiler "test70" test70 ;
  Tezt_core.Base.unit

let () =
  Tezt_core.Regression.register
    ~__FILE__
    ~title:"Simple profiler: all profiler actions"
    ~tags:["unix"; "profiler"]
  @@ fun () ->
  run_test_with_profiler "test_actions" test_profiler_actions ;

  Tezt_core.Base.unit
