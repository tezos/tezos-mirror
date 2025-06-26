(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Base, Unix
   Invocation:   dune exec src/lib_profiler/backends/test/main.exe -- --file test_simple_profiler.ml
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
      (* Special case for [stamp] operation that may run in less
          than 0.001ms and be printed without a time associated. *)
      ( "stamp \\.{70} 1 {24}$",
        "stamp \
         ...................................................................... \
         1            0.000ms   0%" );
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
  record ~cpu:None profiler Info ("noop", []) ;
  stop profiler

let sleep10ms profiler =
  let tag = "sleep10ms" in
  record ~cpu:None profiler Info (tag, []) ;
  Log.info "%s" tag ;
  Unix.sleepf 0.01 ;
  noop profiler ;
  stop profiler

let sleep70ms profiler =
  let tag = "sleep70" in
  record ~cpu:None profiler Info (tag, []) ;
  Log.info "%s" tag ;
  Unix.sleepf 0.07 ;
  stop profiler

let foo profiler =
  record ~cpu:None profiler Info ("foo", []) ;
  sleep10ms profiler ;
  sleep70ms profiler ;
  sleep10ms profiler ;
  stop profiler

let bar profiler =
  record ~cpu:None profiler Info ("bar", []) ;
  sleep70ms profiler ;
  sleep10ms profiler ;
  foo profiler ;
  foo profiler ;
  stop profiler

let test70 profiler =
  Log.info "\nTEST: test 70\n" ;
  record ~cpu:None profiler Info ("test70", []) ;
  bar profiler ;
  foo profiler ;
  stop profiler

let sleep_until_next_second () =
  let current_time = Unix.gettimeofday () in
  let time_in_seconds = int_of_float current_time in
  let next_second = time_in_seconds + 1 in
  let remaining_time = float_of_int next_second -. current_time in
  Lwt_unix.sleep remaining_time

let slp tag profiler =
  record_s
    ~cpu:None
    profiler
    Info
    ("slp " ^ tag, [])
    (fun () ->
      Log.info "sleep in %s" tag ;
      let* () = Lwt_unix.sleep 0.01 in
      Log.info "sleep done" ;
      Lwt.return_unit)

let sequences profiler =
  let* () =
    record_s ~cpu:None profiler Info ("a_seq", []) (fun () ->
        slp "a_seq" profiler)
  in
  record_s ~cpu:None profiler Info ("b_seq", []) (fun () ->
      let* () =
        record_s ~cpu:None profiler Info ("b_a_seq", []) (fun () ->
            slp "b_a_seq" profiler)
      in
      let* () =
        record_s ~cpu:None profiler Info ("b_b_seq", []) (fun () ->
            slp "b_b_seq" profiler)
      in
      slp "b_seq" profiler)

let aggregates profiler =
  let* () =
    aggregate_s ~cpu:None profiler Info ("a_aggr", []) (fun () ->
        slp "a_aggr" profiler)
  in
  let* () =
    aggregate_s ~cpu:None profiler Info ("b_aggr", []) (fun () ->
        slp "b_aggr" profiler)
  in
  let* () =
    aggregate_s ~cpu:None profiler Info ("a_aggr", []) (fun () ->
        let* () =
          aggregate_s ~cpu:None profiler Info ("a_a_aggr", []) (fun () ->
              slp "a_a_aggr" profiler)
        in
        let* () =
          aggregate_s ~cpu:None profiler Info ("a_b_aggr", []) (fun () ->
              slp "a_b_aggr" profiler)
        in
        let* () =
          aggregate_s ~cpu:None profiler Info ("a_c_aggr", []) (fun () ->
              slp "a_c_aggr" profiler)
        in
        slp "a_aggr" profiler)
  in
  aggregate_s ~cpu:None profiler Info ("a_aggr", []) (fun () ->
      let* () =
        aggregate_s ~cpu:None profiler Info ("a_a_aggr", []) (fun () ->
            aggregate_s ~cpu:None profiler Info ("a_a_a_aggr", []) (fun () ->
                slp "a_a_a_aggr" profiler))
      in
      slp "a_aggr" profiler)

let buggy_aggregates profiler =
  let t1 =
    aggregate_s ~cpu:None profiler Info ("bug_aggr_a", []) (fun () ->
        slp "bug_aggr_a" profiler)
  in
  let t2 =
    aggregate_s ~cpu:None profiler Info ("bug_aggr_b", []) (fun () ->
        let* () = slp "bug_aggr_b_a" profiler in
        slp "bug_aggr_b_b" profiler)
  in
  Lwt.join [t1; t2]

let spans profiler =
  let t1 =
    span_s ~cpu:None profiler Info (["span1"], []) (fun () ->
        slp "span1" profiler)
  in
  let t2 =
    span_s
      ~cpu:None
      profiler
      Info
      (["span2"; "inner1"], [])
      (fun () ->
        let* () = slp "span2, inner1/1" profiler in
        slp "span2, inner1/2" profiler)
  in
  let t3 =
    span_s
      ~cpu:None
      profiler
      Info
      (["span2"; "inner2"], [])
      (fun () ->
        let* () = slp "span2, inner2/1" profiler in
        slp "span2, inner2/2" profiler)
  in
  Lwt.join [t1; t2; t3]

let sequences_with_mark_and_stamp profiler =
  let* () =
    record_s ~cpu:None profiler Info ("a_seq", []) (fun () ->
        slp "a_seq" profiler)
  in
  record_s ~cpu:None profiler Info ("b_seq", []) (fun () ->
      let* () =
        record_s ~cpu:None profiler Info ("b_a_seq", []) (fun () ->
            slp "b_a_seq" profiler)
      in
      let () =
        List.iter (fun _ -> mark profiler Info (["mark"], [])) (1 -- 10)
      in
      stamp ~cpu:None profiler Info ("stamp", []) ;
      let* () =
        record_s ~cpu:None profiler Info ("b_b_seq", []) (fun () ->
            slp "b_b_seq" profiler)
      in
      let () =
        List.iter (fun _ -> mark profiler Info (["mark"], [])) (1 -- 10)
      in
      stamp ~cpu:None profiler Info ("stamp", []) ;
      slp "b_seq" profiler)

let test_profiler_actions profiler =
  Log.info "\nTEST: test_profiler_actions\n" ;
  let main () =
    let* () = sleep_until_next_second () in
    record_s ~cpu:None profiler Info ("main", []) (fun () ->
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
    instance
      (* The default driver is a text driver writing to a file
         without suffixing it *)
      Tezos_profiler_backends.Simple_profiler.auto_write_as_txt_to_file
      (file_name, Info)
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
