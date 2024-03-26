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

let check_line_matches line (pattern: string) =
  if not (Re.Str.string_match (Re.Str.regexp pattern) line 0) then
    Test.fail ~__LOC__ "No match: pattern={%s}\n%s" pattern line

let check_file_matches (file_path : string) (patterns : (int * string) list)
    =
  let get_line file =
    try input_line file with
    | End_of_file ->
        close_in file ;
        Test.fail ~__LOC__ "The file is shorter than expected: %s" file_path
    | ex ->
        close_in file ;
        raise ex
  in
  let check_eof file =
    try
      let line = input_line file in
      close_in file ;
      Test.fail ~__LOC__ "The file is longer than expected: \n%s (%s)" line file_path
    with
    | End_of_file -> close_in file
    | ex ->
        close_in file ;
        raise ex
  in
  let rec read_and_match file patterns_left =
    match patterns_left with
    | [] -> check_eof file
    | (count, pattern) :: rest -> (
        match count with
        | 0 -> read_and_match file rest
        | n ->
            let line = get_line file in
            Log.info "%s" line ;
            check_line_matches line pattern ;
            read_and_match file ((n - 1, pattern) :: rest))
  in
  try
    let file = open_in file_path in
    read_and_match file patterns
  with Sys_error msg -> Printf.eprintf "Error [%s] %s\n" file_path msg

let noop profiler =
  Profiler.record profiler "noop" ;
  Profiler.stop profiler

let sleep10ms profiler =
  Profiler.record profiler "sleep10ms" ;
  Log.info "---sleep10---" ;
  Unix.sleepf 0.01 ;
  noop profiler ;
  Profiler.stop profiler

let sleep70ms profiler =
  Profiler.record profiler "sleep70ms" ;
  Log.info "---sleep70---" ;
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
        Log.info "sleep done";
        Lwt.return_unit)
  in
  return_unit

let sequences profiler =
  let* () =
    Profiler.record_s profiler "a_seq" (fun () -> slp "a_seq" profiler)
  in
  let* () =
    Profiler.record_s profiler "b_seq" (fun () ->
        let* () =
          Profiler.record_s profiler "b_a_seq" (fun () ->
              slp "b_a_seq" profiler)
        in
        let* () =
          Profiler.record_s profiler "b_b_seq" (fun () ->
              slp "b_b_seq" profiler)
        in
        slp "b_seq" profiler)
  in
  return_unit

let aggregates profiler =
  let* () =
    Profiler.aggregate_s profiler "a_aggr" (fun () -> slp "a_aggr" profiler)
  in
  let* () =
    Profiler.aggregate_s profiler "b_aggr" (fun () -> slp "b_aggr" profiler)
  in
  let* () =
    Profiler.aggregate_s profiler "a_aggr" (fun () ->
        let* () =
          Profiler.aggregate_s profiler "a_a_aggr" (fun () ->
              slp "a_a_aggr" profiler)
        in
        let* () =
          Profiler.aggregate_s profiler "a_b_aggr" (fun () ->
              slp "a_b_aggr" profiler)
        in
        let* () =
          Profiler.aggregate_s profiler "a_c_aggr" (fun () ->
              slp "a_c_aggr" profiler)
        in
        slp "a_aggr1" profiler)
  in
  let* () =
    Profiler.aggregate_s profiler "a_aggr" (fun () ->
        let* () =
          Profiler.aggregate_s profiler "a_a_aggr" (fun () ->
              Profiler.aggregate_s profiler "a_a_a_aggr" (fun () ->
                  slp "a_a_a_aggr" profiler))
        in
        slp "a_aggr2" profiler)
  in
  return_unit

let buggy_aggregates profiler =
  let t1 =
    Profiler.aggregate_s profiler "bug_aggr1" (fun () ->
        slp "bug_aggr1" profiler)
  in
  let t2 =
    Profiler.aggregate_s profiler "bug_aggr2" (fun () ->
        let* () = slp "bug_aggr2-1" profiler in
        slp "bug_aggr2-2" profiler)
  in
  Lwt.join [t1; t2]

let spans profiler =
  let t1 =
    Profiler.span_s profiler ["span1"] (fun () -> slp "span1" profiler)
  in
  let t2 =
    Profiler.span_s profiler ["span2"; "inner1"] (fun () ->
        let* () = slp "span2, inner1/1" profiler in
        slp "span2, inner1/2" profiler)
  in
  let t3 =
    Profiler.span_s profiler ["span2"; "inner2"] (fun () ->
        let* () = slp "span2, inner2/1" profiler in
        slp "span2, inner2/2" profiler)
  in
  Lwt.join [t1; t2; t3]

let sequences_with_mark_and_stamp profiler =
  let* () =
    Profiler.record_s profiler "a_seq" (fun () -> slp "a_seq" profiler)
  in
  let* () =
    Profiler.record_s profiler "b_seq" (fun () ->
        let* () =
          Profiler.record_s profiler "b_a_seq" (fun () ->
              slp "b_a_seq" profiler)
        in
        let () =
          List.iter (fun _ -> Profiler.mark profiler ["mark"]) (1 -- 10)
        in
        let () = Profiler.stamp profiler "stamp" in
        let* () =
          Profiler.record_s profiler "b_b_seq" (fun () ->
              slp "b_b_seq" profiler)
        in
        let () =
          List.iter (fun _ -> Profiler.mark profiler ["mark"]) (1 -- 10)
        in
        let () = Profiler.stamp profiler "stamp" in
        slp "b_seq" profiler)
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

let ms_pattern = {| +[0-9]+\.[0-9][0-9][0-9]ms +[0-9]+%.*|}
let test70_patterns : ((int * string) list) = 
  [
    (1, {|20[0-9\-]+T[0-9:\.]+-00:00|});  (* Timestamp pattern *)
    (1, {|test70 \.+ 1|} ^ ms_pattern);
    (22, {| +[a-z0-9_ ]+ \.+ [0-9]+|} ^ ms_pattern);
  ]
let test_actions_patterns : ((int * string) list) = 
  [
    (1, {|20[0-9\-]+T[0-9:\.]+-00:00|});  (* Timestamp pattern *)
    (1, {|main \.+ 1|} ^ ms_pattern);
    (1, {|  a_aggr \.+ 3|} ^ ms_pattern);
    (1, {|    a_a_aggr \.+ 2|} ^ ms_pattern);
    (17, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
    (1, {|  span2 \.+ 0|});
    (10, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
    (1, {|    span2 \.+ 0|});
    (9, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
    (1, {|    mark \.+ 20|});
    (2, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
    (1, {|    stamp \.+ 1.*|});
    (2, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
    (1, {|    stamp \.+ 1.*|});
    (1, {| +[a-z0-9_\- ]+ \.+ 1|} ^ ms_pattern);
  ]
  
let get_profiler file_name=
  let profiler = unplugged () in
  let test_profiler_instance =
    Profiler.instance
      Tezos_base_unix.Simple_profiler.auto_write_to_txt_file ( file_name, Profiler.Detailed )
  in
  plug profiler test_profiler_instance ;
  profiler

let run_test_with_profiler test_name test_fn test_patterns =
  let file_name = 
    (* "/tmp/output_test_simple_profiling_" ^ test_name ^ ".txt" in *)
    Temp.file ("output_test_simple_profiling_" ^ test_name ^ ".txt") in
  let profiler = get_profiler file_name in
    
  test_fn profiler ;
  Log.info "\nProfiling result for %s" test_name ;
  Log.info "==================================" ;
  check_file_matches file_name test_patterns

let () =
  Tezt_core.Regression.register
    ~__FILE__
    ~title:"Simple profiler: basic profiler test"
    ~tags:["unix"; "profiler"]
  @@ fun () ->
  run_test_with_profiler "test70" test70 test70_patterns;
  Tezt_core.Base.unit

let () =
  Tezt_core.Regression.register
    ~__FILE__
    ~title:"Simple profiler: all profiler actions"
    ~tags:["unix"; "profiler"]
  @@ fun () ->
  run_test_with_profiler "test_actions" test_profiler_actions test_actions_patterns;

  Tezt_core.Base.unit
