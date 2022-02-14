(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Base

let reset_functions = ref []

let declare_reset_function f = reset_functions := f :: !reset_functions

let before_test_run_functions = ref []

let before_test_run f =
  before_test_run_functions := f :: !before_test_run_functions

(* Prepare a promise that will resolve on SIGINT
   (e.g. when the user presses Ctrl+C).

   We need a new promise everytime because they get canceled. *)
let sigint =
  let received_sigint = ref false in
  fun () ->
    if !received_sigint then unit
    else
      let (promise, resolver) = Lwt.task () in
      Sys.(set_signal sigint)
        (Signal_handle
           (fun _ ->
             (* If the user presses Ctrl+C again, let the program die immediately. *)
             received_sigint := true ;
             Sys.(set_signal sigint) Signal_default ;
             Lwt.wakeup_later resolver ())) ;
      promise

exception Failed of string

let () =
  Printexc.register_printer @@ function
  | Failed message -> Some message
  | _ -> None

let fail ?__LOC__ x =
  Format.kasprintf
    (fun message ->
      let message =
        match __LOC__ with
        | None -> message
        | Some loc -> sf "%s: %s" loc message
      in
      raise (Failed message))
    x

let global_starting_time = Unix.gettimeofday ()

module Summed_durations : sig
  type t

  val zero : t

  val single_seconds : float -> t

  val ( + ) : t -> t -> t

  val total_seconds : t -> float

  val total_nanoseconds : t -> int64

  val encode : t -> JSON.u

  (* May raise [JSON.Error]. *)
  val decode : JSON.t -> t
end = struct
  (* Information about how much time a test takes to run.
     Field [total_time_ns] contains the sum of the duration of runs in nanoseconds,
     and [count] contains the number of runs.
     By storing integers we ensure commutativity and associativity (which would not
     be the case with floats). *)
  type t = {total_time : int64; count : int}

  let zero = {total_time = 0L; count = 0}

  let single_seconds time =
    {total_time = Int64.of_float (time *. 1_000_000.); count = 1}

  let ( + ) a b =
    {
      total_time = Int64.add a.total_time b.total_time;
      count = a.count + b.count;
    }

  let total_seconds {total_time; count = _} =
    Int64.to_float total_time /. 1_000_000.

  let total_nanoseconds {total_time; count = _} = total_time

  let encode {total_time; count} =
    `O
      [
        ("total_time", `String (Int64.to_string total_time));
        ("count", `String (string_of_int count));
      ]

  let decode (json : JSON.t) =
    {
      total_time = JSON.(json |-> "total_time" |> as_int64);
      count = JSON.(json |-> "count" |> as_int);
    }
end

(* Field [id] is used to be able to iterate on tests in order of registration.
   Field [result] contains the result of the last time the test was run.
   If the test was not run, it contains [None]. *)
type test = {
  id : int;
  file : string;
  title : string;
  tags : string list;
  body : unit -> unit Lwt.t;
  mutable session_successful_runs : Summed_durations.t;
  mutable session_failed_runs : Summed_durations.t;
  mutable past_records_successful_runs : Summed_durations.t;
  mutable past_records_failed_runs : Summed_durations.t;
  mutable result : Log.test_result option;
}

let really_run test =
  Log.info "Starting test: %s" test.title ;
  List.iter (fun reset -> reset ()) !reset_functions ;
  test.result <- None ;
  Lwt_main.run
  @@
  (* It may happen that the promise of the function resolves successfully
     at the same time as a background promise is rejected or that we
     receive SIGINT. To handle those race conditions, setting the value
     of [test.result] is done through [set_test_result], which makes sure that:
     - if the test was aborted, [test.result] is [Aborted];
     - otherwise, if anything went wrong, [test.result] is [Failed];
     - the error message in [Failed] is the first error that was encountered. *)
  let set_test_result new_result =
    match test.result with
    | None -> test.result <- Some new_result
    | Some old_result -> (
        match (old_result, new_result) with
        | (Successful, _) | (Failed _, Aborted) ->
            test.result <- Some new_result
        | (Failed _, (Successful | Failed _)) | (Aborted, _) -> ())
  in
  let (fail_promise, fail_awakener) = Lwt.task () in
  (* Ensure that errors raised from background promises are logged
     and cause the test to fail immediately. *)
  let already_woke_up_fail_promise = ref false in
  let handle_background_exception exn =
    let message = Printexc.to_string exn in
    Log.error "%s" message ;
    set_test_result (Log.Failed message) ;
    if not !already_woke_up_fail_promise then (
      already_woke_up_fail_promise := true ;
      Lwt.wakeup_later fail_awakener ())
  in
  Background.start handle_background_exception ;
  (* Run the test until it succeeds, fails, or we receive SIGINT. *)
  let main_temporary_directory = Temp.start () in
  let* () =
    let run_test () =
      let* () = test.body () in
      set_test_result Successful ;
      unit
    in
    let handle_exception = function
      | Lwt.Canceled ->
          (* Aborted with SIGINT, or [fail_promise] resolved (possibly because of
             an [async] promise). So we already logged what happened. *)
          unit
      | exn ->
          let message = Printexc.to_string exn in
          set_test_result (Failed message) ;
          Log.error "%s" message ;
          unit
    in
    let handle_sigint () =
      let* () = sigint () in
      Log.debug "Received SIGINT." ;
      set_test_result Aborted ;
      unit
    in
    let global_timeout =
      match Cli.options.global_timeout with
      | None -> []
      | Some delay ->
          let local_starting_time = Unix.gettimeofday () in
          let remaining_delay =
            max 0. (delay -. local_starting_time +. global_starting_time)
          in
          [
            (let* () = Lwt_unix.sleep remaining_delay in
             fail
               "the set of tests took more than specified global timeout (%gs) \
                to run"
               delay);
          ]
    in
    let test_timeout =
      match Cli.options.test_timeout with
      | None -> []
      | Some delay ->
          [
            (let* () = Lwt_unix.sleep delay in
             fail "test took more than specified timeout (%gs) to run" delay);
          ]
    in
    Lwt.catch
      (fun () ->
        Lwt.pick
          (run_test () :: handle_sigint () :: fail_promise :: global_timeout
          @ test_timeout))
      handle_exception
  in
  (* Terminate all remaining processes. *)
  let* () =
    Lwt.catch Process.clean_up @@ fun exn ->
    Log.warn "Failed to clean up processes: %s" (Printexc.to_string exn) ;
    unit
  in
  (* Remove temporary files. *)
  let kept_temp =
    try
      match Cli.options.temporary_file_mode with
      | Delete ->
          Temp.clean_up () ;
          false
      | Delete_if_successful ->
          if test.result = Some Successful then (
            Temp.clean_up () ;
            false)
          else (
            Temp.stop () ;
            true)
      | Keep ->
          Temp.stop () ;
          true
    with exn ->
      Log.warn "Failed to clean up: %s" (Printexc.to_string exn) ;
      true
  in
  if kept_temp then
    Log.report "Temporary files can be found in: %s" main_temporary_directory ;
  (* Resolve all pending promises so that they won't do anything
     (like raise [Canceled]) during the next test. *)
  let* () = Background.stop () in
  (* Return test result. *)
  let test_result =
    match test.result with
    | None ->
        (* Should not happen: after the test ends we always set [result] to [Some].
           But if it does happen we assume that it failed and that we failed to
           maintain this invariant. *)
        Log.Failed "unknown error"
    | Some result -> result
  in
  return test_result

let test_should_be_run ~file ~title ~tags =
  List.for_all (fun tag -> List.mem tag tags) Cli.options.tags_to_run
  && (not
        (List.exists (fun tag -> List.mem tag tags) Cli.options.tags_not_to_run))
  && (match Cli.options.tests_to_run with
     | [] -> true
     | titles -> List.mem title titles)
  && (not (List.mem title Cli.options.tests_not_to_run))
  &&
  match Cli.options.files_to_run with
  | [] -> true
  | files -> List.mem file files

let tag_rex = rex "^[a-z0-9_]{1,32}$"

let check_tags tags =
  match List.filter (fun tag -> tag =~! tag_rex) tags with
  | [] -> ()
  | invalid_tags ->
      List.iter (Printf.eprintf "Invalid tag: %S\n") invalid_tags ;
      Printf.eprintf
        "Tags may only use lowercase letters, digits and underscores, and must \
         be at most 32 character long.\n" ;
      exit 1

let known_files = ref String_set.empty

let known_titles = ref String_set.empty

let known_tags = ref String_set.empty

let register_file file = known_files := String_set.add file !known_files

let register_title title = known_titles := String_set.add title !known_titles

let register_tag tag = known_tags := String_set.add tag !known_tags

(* Check that all [specified] values are in [!known]. *)
let check_existence kind known specified =
  String_set.iter
    (Log.warn "Unknown %s: %s" kind)
    (String_set.diff (String_set.of_list specified) !known)

(* Tests added using [register] and that match command-line filters. *)
let registered : test String_map.t ref = ref String_map.empty

(* Sort registred jobs in the registration order. *)
let list_registered () =
  let list = ref [] in
  String_map.iter (fun _ test -> list := test :: !list) !registered ;
  let by_id {id = a; _} {id = b; _} = Int.compare a b in
  List.sort by_id !list

(* Using [iter_registered] instead of [String_map.iter] allows to more easily
   change the representation of [registered] in the future if needed. *)
let iter_registered f = List.iter (fun test -> f test) (list_registered ())

let fold_registered acc f =
  String_map.fold (fun _ test acc -> f acc test) !registered acc

(* Map [register] as if it was a list, to obtain a list. *)
let map_registered_list f =
  (* By using [list_registered] we ensure the resulting list is
     in order of registration. *)
  List.map f (list_registered ())

let list_tests format =
  match format with
  | `Tsv ->
      iter_registered @@ fun {file; title; tags; _} ->
      Printf.printf "%s\t%s\t%s\n%!" file title (String.concat " " tags)
  | `Ascii_art ->
      let file_header = "FILE" in
      let title_header = "TITLE" in
      let tags_header = "TAGS" in
      let list =
        map_registered_list @@ fun {file; title; tags; _} ->
        (file, title, String.concat ", " tags)
      in
      (* Compute the size of each column. *)
      let (file_size, title_size, tags_size) =
        List.fold_left
          (fun (max_file, max_title, max_tags) (file, title, tags) ->
            ( max max_file (String.length file),
              max max_title (String.length title),
              max max_tags (String.length tags) ))
          ( String.length file_header,
            String.length title_header,
            String.length tags_header )
          list
      in
      (* Prepare the line separator. *)
      let line =
        "+"
        ^ String.make (file_size + 2) '-'
        ^ "+"
        ^ String.make (title_size + 2) '-'
        ^ "+"
        ^ String.make (tags_size + 2) '-'
        ^ "+\n"
      in
      (* Print the header row. *)
      print_string line ;
      let center size header =
        let padding = size - String.length header in
        let left_padding = padding / 2 in
        let right_padding = padding - left_padding in
        String.make left_padding ' ' ^ header ^ String.make right_padding ' '
      in
      Printf.printf
        "| %s | %s | %s |\n"
        (center file_size file_header)
        (center title_size title_header)
        (center tags_size tags_header) ;
      print_string line ;
      (* Print rows. *)
      let pad size text =
        let padding = size - String.length text in
        text ^ String.make padding ' '
      in
      List.iter
        (fun (file, title, tags) ->
          Printf.printf
            "| %s | %s | %s |\n"
            (pad file_size file)
            (pad title_size title)
            (pad tags_size tags))
        list ;
      if list <> [] then print_string line ;
      ()

(* Total time, in seconds.
   Since this involves floats it should not be used for --job splitting. *)
let total_test_display_time ~past_records ~session test =
  let past_records =
    if past_records then
      Summed_durations.total_seconds test.past_records_successful_runs
      +. Summed_durations.total_seconds test.past_records_failed_runs
    else 0.
  in
  let session =
    if session then
      Summed_durations.total_seconds test.session_successful_runs
      +. Summed_durations.total_seconds test.session_failed_runs
    else 0.
  in
  past_records +. session

let display_time_summary () =
  let test_time = total_test_display_time ~past_records:true ~session:true in
  let total_time =
    fold_registered 0. @@ fun acc test -> acc +. test_time test
  in
  let tests_by_file =
    fold_registered String_map.empty @@ fun acc test ->
    String_map.add
      test.file
      (test :: (String_map.find_opt test.file acc |> Option.value ~default:[]))
      acc
  in
  let show_time seconds =
    let seconds = int_of_float seconds in
    if seconds < 60 then Printf.sprintf "%ds" seconds
    else Printf.sprintf "%dmin %ds" (seconds / 60) (seconds mod 60)
  in
  let print_time prefix title time =
    Printf.printf
      "%s[%d%% - %s] %s\n"
      prefix
      (int_of_float (time *. 100. /. total_time))
      (show_time time)
      title
  in
  let print_time_for_file file tests =
    print_time
      ""
      file
      (List.fold_left (fun acc test -> acc +. test_time test) 0. tests) ;
    List.iter (fun test -> print_time "- " test.title (test_time test)) tests
  in
  String_map.iter print_time_for_file tests_by_file ;
  ()

module Record = struct
  type test = {
    file : string;
    title : string;
    tags : string list;
    successful_runs : Summed_durations.t;
    failed_runs : Summed_durations.t;
  }

  let encode_test {file; title; tags; successful_runs; failed_runs} : JSON.u =
    `O
      [
        ("file", `String file);
        ("title", `String title);
        ("tags", `A (List.map (fun tag -> `String tag) tags));
        ("successful_runs", Summed_durations.encode successful_runs);
        ("failed_runs", Summed_durations.encode failed_runs);
      ]

  let decode_test (json : JSON.t) : test =
    {
      file = JSON.(json |-> "file" |> as_string);
      title = JSON.(json |-> "title" |> as_string);
      tags = JSON.(json |-> "tags" |> as_list |> List.map as_string);
      successful_runs =
        Summed_durations.decode JSON.(json |-> "successful_runs");
      failed_runs = Summed_durations.decode JSON.(json |-> "failed_runs");
    }

  type t = test list

  let encode (record : t) : JSON.u = `A (List.map encode_test record)

  let decode (json : JSON.t) : t =
    JSON.(json |> as_list |> List.map decode_test)

  let output_file (record : t) filename =
    (* Write to file using Marshal.
       This is not very robust but enough for the purposes of this file. *)
    try
      with_open_out filename @@ fun file ->
      output_string file (JSON.encode_u (encode record))
    with Sys_error error -> Log.warn "Failed to write record: %s" error

  let input_file filename : t =
    try decode (JSON.parse_file filename)
    with JSON.Error error ->
      Log.error "%s" (JSON.show_error error) ;
      exit 1

  (* Get the record for the current run. *)
  let current () =
    map_registered_list
    @@ fun {
             id = _;
             file;
             title;
             tags;
             body = _;
             session_successful_runs;
             session_failed_runs;
             past_records_successful_runs = _;
             past_records_failed_runs = _;
             result = _;
           } ->
    {
      file;
      title;
      tags;
      successful_runs = session_successful_runs;
      failed_runs = session_failed_runs;
    }

  (* Read a record and update the time information of registered tests
     that appear in this record. *)
  let use (record : t) =
    let update_test (recorded_test : test) =
      match String_map.find_opt recorded_test.title !registered with
      | None ->
          (* Test no longer exists or was not selected, ignoring. *)
          ()
      | Some test ->
          test.past_records_successful_runs <-
            Summed_durations.(
              test.past_records_successful_runs + recorded_test.successful_runs) ;
          test.past_records_failed_runs <-
            Summed_durations.(
              test.past_records_failed_runs + recorded_test.failed_runs)
    in
    List.iter update_test record
end

(* Get a partition of weighted [items] where the total weights of each subset are
   approximately close to each other. *)
let knapsack (type a) bag_count (items : (int64 * a) list) :
    (int64 * a list) array =
  let bag_count = max 1 bag_count in
  (* [bags] is an array of pairs where the first value is the total
     weight of the bag and the second value is the list of items that
     are currently allocated to this bag. *)
  let bags = Array.make bag_count (0L, []) in
  (* Finding the optimal partition is NP-complete.
     We use a heuristic to find an approximation: allocate heavier items first,
     then fill the gaps with smaller items. *)
  let allocate (item_weight, item) =
    let smallest_bag =
      let best_index = ref 0 in
      let best_weight = ref Int64.max_int in
      for i = 0 to bag_count - 1 do
        let (bag_weight, _) = bags.(i) in
        if bag_weight < !best_weight then (
          best_index := i ;
          best_weight := bag_weight)
      done ;
      !best_index
    in
    let (bag_weight, bag_items) = bags.(smallest_bag) in
    bags.(smallest_bag) <- (Int64.add bag_weight item_weight, item :: bag_items)
  in
  let longest_first (a, _) (b, _) = Int64.compare b a in
  List.iter allocate (List.sort longest_first items) ;
  bags

let split_tests_into_balanced_jobs job_count =
  let test_time test =
    (* Give a default duration of 1 second as specified by --help.
       This allows to split jobs even with no time data (otherwise all jobs
       would be grouped together). *)
    max
      1_000_000L
      (Summed_durations.total_nanoseconds test.past_records_successful_runs)
  in
  let tests = String_map.bindings !registered |> List.map snd in
  let weighted_tests = List.map (fun test -> (test_time test, test)) tests in
  knapsack job_count weighted_tests

(* Apply --job: take the list of registered tests, split it into jobs,
   and unregister all tests that are not selected by --job. *)
let select_job () =
  match Cli.options.job with
  | None ->
      (* No --job: do not unregister any test. *)
      ()
  | Some (job_index, job_count) ->
      let jobs = split_tests_into_balanced_jobs job_count in
      (* [Cli] ensures that [1 <= job_index <= job_count],
         and [split_tests_into_balanced_jobs] ensures that its result
         has length [job_count] if [job_count >= 1]. *)
      let (_, job_tests) = jobs.(job_index - 1) in
      (* Reset the list of tests to run to re-fill it with the requested job. *)
      registered := String_map.empty ;
      List.iter
        (fun (test : test) ->
          registered := String_map.add test.title test !registered)
        job_tests

let skip_test () =
  if Cli.options.skip > 0 || Cli.options.only <> None then (
    (* by using [list_registered] we ensure that we sort jobs in the
       registration order, which is also the order in which tests are
       run. *)
    let list = list_registered () in
    let list = Base.drop Cli.options.skip list in
    let list =
      match Cli.options.only with
      | None -> list
      | Some only -> Base.take only list
    in
    registered := String_map.empty ;
    List.iter
      (fun (test : test) ->
        registered := String_map.add test.title test !registered)
      list)

let suggest_jobs () =
  let jobs = split_tests_into_balanced_jobs Cli.options.job_count in
  let job_count = Array.length jobs in
  (* Jobs are allocated, now display them. *)
  let display_job ~negate (total_job_time, job_tests) =
    print_endline
      (String.concat
         " "
         (List.map
            (fun test ->
              Printf.sprintf
                "%s %s"
                (if negate then "--not-test" else "--test")
                (Log.quote_shell (test : test).title))
            job_tests)
      ^ " # "
      ^ Int64.to_string (Int64.div total_job_time 1_000_000L)
      ^ "s")
  in
  let all_other_tests = ref [] in
  for i = 0 to job_count - 2 do
    display_job ~negate:false jobs.(i) ;
    List.iter
      (fun test -> all_other_tests := test :: !all_other_tests)
      (snd jobs.(i))
  done ;
  (* The last job uses --not-test so that if a test is added and the job list is not
     updated, the new test is automatically added to the last job.
     Note: if [job_count] is 1, this actually outputs no --not-test at all
     since [all_other_tests] is empty, which is consistent
     because it means to run all tests. *)
  display_job ~negate:true (fst jobs.(job_count - 1), !all_other_tests)

let output_junit filename =
  let test_time = total_test_display_time ~past_records:false ~session:true in
  with_open_out filename @@ fun ch ->
  let echo x =
    Printf.ksprintf
      (fun s ->
        output_string ch s ;
        output_char ch '\n')
      x
  in
  let (count, fail_count, skipped_count, total_time) =
    fold_registered (0, 0, 0, 0.)
    @@ fun (count, fail_count, skipped_count, total_time) test ->
    ( count + 1,
      (fail_count + match test.result with Some (Failed _) -> 1 | _ -> 0),
      (skipped_count
      + match test.result with None | Some Aborted -> 1 | _ -> 0),
      total_time +. test_time test )
  in
  echo {|<?xml version="1.0" encoding="UTF-8" ?>|} ;
  echo
    {|<testsuites id="tezt" name="Tezt" tests="%d" failures="%d" skipped="%d" time="%f">|}
    count
    fail_count
    skipped_count
    total_time ;
  echo
    {|  <testsuite id="tezt" name="Tezt" tests="%d" failures="%d" skipped="%d" time="%f">|}
    count
    fail_count
    skipped_count
    total_time ;
  ( iter_registered @@ fun test ->
    match test.result with
    | None | Some Aborted ->
        (* Skipped test, do not output. *)
        ()
    | Some (Successful | Failed _) ->
        let replace_entities s =
          let buffer = Buffer.create (String.length s * 2) in
          for i = 0 to String.length s - 1 do
            match s.[i] with
            | '"' -> Buffer.add_string buffer "&quot;"
            | '&' -> Buffer.add_string buffer "&amp;"
            | '\'' -> Buffer.add_string buffer "&apos;"
            | '<' -> Buffer.add_string buffer "&lt;"
            | '>' -> Buffer.add_string buffer "&gt;"
            | c -> Buffer.add_char buffer c
          done ;
          Buffer.contents buffer
        in
        let title = replace_entities test.title in
        echo
          {|    <testcase id="%s" name="%s: %s" time="%f">|}
          title
          (replace_entities test.file)
          title
          (test_time test) ;
        (match test.result with
        | None | Some Successful | Some Aborted -> ()
        | Some (Failed message) ->
            echo
              {|      <failure message="test failed" type="ERROR">%s</failure>|}
              (replace_entities message)) ;
        echo "    </testcase>" ) ;
  echo "  </testsuite>" ;
  echo "</testsuites>" ;
  ()

let next_id = ref 0

let register ~__FILE__ ~title ~tags body =
  let file = Filename.basename __FILE__ in
  (match String_map.find_opt title !registered with
  | None -> ()
  | Some {file = other_file; tags = other_tags; _} ->
      Printf.eprintf "Error: there are several tests with title: %S\n" title ;
      Printf.eprintf
        "- first seen in: %s with tags: %s\n"
        other_file
        (String.concat ", " other_tags) ;
      Printf.eprintf
        "- also seen in: %s with tags: %s\n%!"
        file
        (String.concat ", " tags) ;
      exit 1) ;
  check_tags tags ;
  register_file file ;
  register_title title ;
  List.iter register_tag tags ;
  let id = !next_id in
  incr next_id ;
  if test_should_be_run ~file ~title ~tags then
    let test =
      {
        id;
        file;
        title;
        tags;
        body;
        session_successful_runs = Summed_durations.zero;
        session_failed_runs = Summed_durations.zero;
        past_records_successful_runs = Summed_durations.zero;
        past_records_failed_runs = Summed_durations.zero;
        result = None;
      }
    in
    registered := String_map.add title test !registered

module Scheduler : sig
  type request = Run_test of {test_title : string}

  type response = Test_result of Log.test_result

  (* Run a scheduler that manages several workers.

     This starts [worker_count] workers.
     As soon as a worker is available, it calls [on_worker_available].
     [on_worker_available] shall return [None] if there is nothing else to do,
     in which case the worker is killed, or [Some (request, on_response)],
     in which case the worker executes [request].
     The result of this request, [response], is then given to [on_response]. *)
  val run :
    on_worker_available:(unit -> (request * (response -> unit)) option) ->
    worker_count:int ->
    unit

  val get_current_worker_id : unit -> int option
end = struct
  type request = Run_test of {test_title : string}

  type response = Test_result of Log.test_result

  type status = Idle | Working of (response -> unit) | Dead

  type worker = {
    pid : int;
    mutable status : status;
    pipe_to_worker : out_channel;
    pipe_from_worker : in_channel;
  }

  let send_request channel request =
    Marshal.to_channel channel (request : request) [] ;
    flush channel

  let read_request channel =
    try Some (Marshal.from_channel channel : request) with End_of_file -> None

  let send_response channel response =
    Marshal.to_channel channel (response : response) [] ;
    flush channel

  let read_response channel =
    try Some (Marshal.from_channel channel : response)
    with End_of_file -> None

  let internal_worker_error x =
    Printf.ksprintf
      (fun s ->
        Log.error "internal error in worker: %s" s ;
        exit 1)
      x

  let internal_scheduler_error x =
    Printf.ksprintf
      (fun s ->
        Log.error "internal error in scheduler: %s" s ;
        exit 1)
      x

  let perform_request (Run_test {test_title}) =
    match String_map.find_opt test_title !registered with
    | None ->
        internal_worker_error
          "scheduler requested to run test %S, but worker doesn't know about \
           this test"
          test_title
    | Some test ->
        let test_result = really_run test in
        Test_result test_result

  let rec worker_listen_loop pipe_from_scheduler pipe_to_scheduler =
    let request = read_request pipe_from_scheduler in
    match request with
    | None ->
        (* End of file: no more request will come. *)
        exit 0
    | Some request ->
        let response = perform_request request in
        send_response pipe_to_scheduler response ;
        worker_listen_loop pipe_from_scheduler pipe_to_scheduler

  let worker_listen pipe_from_scheduler pipe_to_scheduler =
    try worker_listen_loop pipe_from_scheduler pipe_to_scheduler
    with exn ->
      (* Note: if a test fails, its exception is caught and handled by [really_run].
         So here we have an error of Tezt itself. *)
      internal_worker_error "%s" (Printexc.to_string exn)

  let next_worker_id = ref 0

  let current_worker_id = ref None

  let spawn_worker () =
    let worker_id = !next_worker_id in
    incr next_worker_id ;
    let (pipe_to_worker_exit, pipe_to_worker_entrance) = Unix.pipe () in
    let (pipe_from_worker_exit, pipe_from_worker_entrance) = Unix.pipe () in
    let pid = Lwt_unix.fork () in
    if pid = 0 then (
      (* This is now a worker process. *)
      current_worker_id := Some worker_id ;
      Unix.close pipe_to_worker_entrance ;
      Unix.close pipe_from_worker_exit ;
      worker_listen
        (Unix.in_channel_of_descr pipe_to_worker_exit)
        (Unix.out_channel_of_descr pipe_from_worker_entrance))
    else (
      (* This is the scheduler process. *)
      Unix.close pipe_to_worker_exit ;
      Unix.close pipe_from_worker_entrance ;
      {
        pid;
        status = Idle;
        pipe_to_worker = Unix.out_channel_of_descr pipe_to_worker_entrance;
        pipe_from_worker = Unix.in_channel_of_descr pipe_from_worker_exit;
      })

  let kill_worker worker =
    match worker.status with
    | Dead -> ()
    | Idle | Working _ ->
        worker.status <- Dead ;
        close_out worker.pipe_to_worker ;
        close_in worker.pipe_from_worker ;
        Unix.kill worker.pid Sys.sigterm ;
        let (_ : int * Unix.process_status) = Unix.waitpid [] worker.pid in
        ()

  let rec run_single_process ~on_worker_available =
    match on_worker_available () with
    | None -> ()
    | Some (request, on_response) ->
        let response = perform_request request in
        on_response response ;
        run_single_process ~on_worker_available

  let run_multi_process ~on_worker_available ~worker_count =
    (* Start workers. *)
    let workers = List.init worker_count (fun _ -> spawn_worker ()) in
    (* Handle Ctrl+C in the scheduler process.
       Note: Ctrl+C is also received by workers automatically. *)
    let received_sigint = ref false in
    Sys.(set_signal sigint)
      (Signal_handle
         (fun _ ->
           received_sigint := true ;
           (* If the user presses Ctrl+C again, let the program die immediately. *)
           Sys.(set_signal sigint) Signal_default)) ;
    (* Give work to workers until there is no work to give. *)
    let trigger_worker_available worker =
      if !received_sigint then kill_worker worker
      else
        match on_worker_available () with
        | None -> kill_worker worker
        | Some (request, on_response) ->
            worker.status <- Working on_response ;
            send_request worker.pipe_to_worker request
    in
    let rec loop () =
      (* Calling [trigger_worker_available] not only gives work to idle workers,
         it also kills them if we don't need them any more.
         It also ensures that [file_descriptors_to_read] will only be empty if there
         are no working workers. *)
      List.iter
        (fun worker ->
          match worker.status with
          | Dead | Working _ -> ()
          | Idle -> trigger_worker_available worker)
        workers ;
      let file_descriptors_to_read =
        List.filter_map
          (fun worker ->
            match worker.status with
            | Idle | Dead -> None
            | Working _ ->
                Some (Unix.descr_of_in_channel worker.pipe_from_worker))
          workers
      in
      match file_descriptors_to_read with
      | [] ->
          (* We maintain the invariant that if there is work to do, at least one
             worker is [Working] at this particular point.
             This is enforced by the [List.iter] of [trigger_worker_available] above.
             So if there is no working worker, we can stop the loop. *)
          ()
      | _ :: _ ->
          let (ready, _, _) =
            (* In case of SIGINT, this returns EINTR. *)
            try Unix.select file_descriptors_to_read [] [] (-1.)
            with Unix.Unix_error (EINTR, _, _) -> ([], [], [])
          in
          let read_response file_descriptor =
            match
              List.find_opt
                (fun worker ->
                  match worker.status with
                  | Idle | Dead -> false
                  | Working _ ->
                      Unix.descr_of_in_channel worker.pipe_from_worker
                      = file_descriptor)
                workers
            with
            | None ->
                internal_scheduler_error
                  "received a response from an unknown worker"
            | Some worker -> (
                match worker.status with
                | Idle | Dead ->
                    (* Please do not consider this error message to be political. *)
                    internal_scheduler_error
                      "worker is idle or dead while it should be working"
                | Working on_response -> (
                    (* Note: [read_response] is blocking.
                       We assume that if a worker starts writing something,
                       it will finish writing almost immediately. *)
                    let response = read_response worker.pipe_from_worker in
                    match response with
                    | None -> internal_scheduler_error "no response from worker"
                    | Some response ->
                        on_response response ;
                        worker.status <- Idle))
          in
          List.iter read_response ready ;
          loop ()
    in
    loop ()

  let run ~on_worker_available ~worker_count =
    if worker_count = 1 then run_single_process ~on_worker_available
    else
      try run_multi_process ~on_worker_available ~worker_count
      with exn -> internal_scheduler_error "%s" (Printexc.to_string exn)

  let get_current_worker_id () = !current_worker_id
end

(* [iteration] is between 1 and the value of [--loop-count].
   [index] is between 1 and [test_count]. *)
type test_instance = {iteration : int; index : int}

let current_worker_id = Scheduler.get_current_worker_id

let run () =
  List.iter (fun f -> f ()) !before_test_run_functions ;
  (* Check command-line options. *)
  check_existence "--file" known_files Cli.options.files_to_run ;
  check_existence "--test" known_titles Cli.options.tests_to_run ;
  check_existence
    "tag"
    known_tags
    (Cli.options.tags_to_run @ Cli.options.tags_not_to_run) ;
  (* Print a warning if no test was selected. *)
  if String_map.is_empty !registered then (
    Printf.eprintf
      "No test found for filters: %s\n%!"
      (String.concat
         " "
         (List.map
            (fun x -> "--file " ^ Log.quote_shell x)
            Cli.options.files_to_run
         @ List.map
             (fun x -> "--test " ^ Log.quote_shell x)
             Cli.options.tests_to_run
         @ Cli.options.tags_to_run
         @ List.map (sf "/%s") Cli.options.tags_not_to_run)) ;
    if Cli.options.list = None then
      prerr_endline
        "You can use --list to get the list of tests and their tags." ;
    exit 3) ;
  (* Read records. *)
  List.iter
    Record.(fun filename -> use (input_file filename))
    Cli.options.from_records ;
  (* Apply --job if needed. *)
  select_job () ;
  (* Apply --skip and --only if needed. *)
  skip_test () ;
  (* Actually run the tests (or list them). *)
  match (Cli.options.list, Cli.options.suggest_jobs) with
  | (Some format, false) -> list_tests format
  | (None, true) -> suggest_jobs ()
  | (Some _, true) ->
      prerr_endline
        "Cannot use both --list and --suggest-jobs at the same time."
  | (None, false) ->
      let test_count = String_map.cardinal !registered in
      let failure_count = ref 0 in
      let test_queue = Queue.create () in
      let refills = ref 0 in
      let refill_queue () =
        incr refills ;
        let index = ref 0 in
        iter_registered @@ fun test ->
        incr index ;
        let test_instance = {iteration = !refills; index = !index} in
        Queue.add (test, test_instance) test_queue
      in
      refill_queue () ;
      (* [stop] is used to stop dequeuing tests.
         When [stop] is [true] we no longer start new tests but we wait for
         running ones to finish. *)
      let stop = ref false in
      (* [aborted] is used to exit with the right error code in case of Ctrl+C.
         It implies [stop]. *)
      let aborted = ref false in
      let next_test () =
        if !stop then None
        else
          match Queue.take_opt test_queue with
          | None -> (
              match Cli.options.loop_mode with
              | Count n when !refills >= n -> None
              | Count _ | Infinite ->
                  refill_queue () ;
                  Queue.take_opt test_queue)
          | Some _ as x -> x
      in
      let a_test_failed = ref false in
      let on_worker_available () =
        match next_test () with
        | None -> None
        | Some (test, test_instance) ->
            let start = Unix.gettimeofday () in
            let on_response (Scheduler.Test_result test_result) =
              let time = Unix.gettimeofday () -. start in
              (match test_result with
              | Failed _ -> incr failure_count
              | Successful | Aborted -> ()) ;
              Log.test_result
                ~test_index:test_instance.index
                ~test_count
                ~failure_count:!failure_count
                ~iteration:test_instance.iteration
                test_result
                test.title ;
              match test_result with
              | Successful ->
                  test.session_successful_runs <-
                    Summed_durations.(
                      test.session_successful_runs + single_seconds time)
              | Failed _ ->
                  Log.report
                    "Try again with: %s --verbose --test %s"
                    Sys.argv.(0)
                    (Log.quote_shell test.title) ;
                  test.session_failed_runs <-
                    Summed_durations.(
                      test.session_failed_runs + single_seconds time) ;
                  a_test_failed := true ;
                  if not Cli.options.keep_going then stop := true
              | Aborted ->
                  stop := true ;
                  aborted := true
            in
            Some (Scheduler.Run_test {test_title = test.title}, on_response)
      in
      Scheduler.run ~on_worker_available ~worker_count:Cli.options.job_count ;
      (* Output reports. *)
      Option.iter output_junit Cli.options.junit ;
      Option.iter Record.(output_file (current ())) Cli.options.record ;
      if Cli.options.time then display_time_summary () ;
      if !aborted then exit 2 else if !a_test_failed then exit 1
