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
  Printexc.register_printer
  @@ function Failed message -> Some message | _ -> None

(* [Failed] can be raised from [async] promises, but we still want to
   stop the test immediately if those promises raise it.
   So we have a promise which can be fulfilled with exception [Failed]. *)
let fail_awakener = ref None

let fail x =
  Printf.ksprintf
    (fun message ->
      ( match !fail_awakener with
      | None ->
          ()
      | Some awakener ->
          fail_awakener := None ;
          Lwt.wakeup_later_exn awakener (Failed message) ) ;
      raise (Failed message))
    x

let global_starting_time = Unix.gettimeofday ()

let really_run title f =
  Log.info "Starting test: %s" title ;
  List.iter (fun reset -> reset ()) !reset_functions ;
  Lwt_main.run
  @@
  let (fail_promise, new_fail_awakener) = Lwt.task () in
  fail_awakener := Some new_fail_awakener ;
  let already_logged_exn = ref false in
  let test_result = ref Log.Failed in
  (* Run the test until it succeeds, fails, or we receive SIGINT. *)
  Temp.start () ;
  let* () =
    let run_test () =
      let* () = f () in
      test_result := Successful ;
      unit
    in
    let handle_exception = function
      | Lwt.Canceled ->
          (* Aborted with SIGINT, or [fail_promise] resolved (possibly because of
             an [async] promise). So we already logged what happened. *)
          already_logged_exn := true ;
          unit
      | exn ->
          Log.error "%s" (Printexc.to_string exn) ;
          already_logged_exn := true ;
          unit
    in
    let handle_sigint () =
      let* () = sigint () in
      Log.debug "Received SIGINT." ;
      test_result := Aborted ;
      unit
    in
    let global_timeout =
      match Cli.options.global_timeout with
      | None ->
          []
      | Some delay ->
          let local_starting_time = Unix.gettimeofday () in
          let remaining_delay =
            max 0. (delay -. local_starting_time +. global_starting_time)
          in
          [ let* () = Lwt_unix.sleep remaining_delay in
            fail
              "the set of tests took more than specified global timeout (%gs) \
               to run"
              delay ]
    in
    let test_timeout =
      match Cli.options.test_timeout with
      | None ->
          []
      | Some delay ->
          [ let* () = Lwt_unix.sleep delay in
            fail "test took more than specified timeout (%gs) to run" delay ]
    in
    Lwt.catch
      (fun () ->
        Lwt.pick
          ( (run_test () :: handle_sigint () :: fail_promise :: global_timeout)
          @ test_timeout ))
      handle_exception
  in
  (* Terminate all remaining processes. *)
  let* () =
    Lwt.catch Process.clean_up
    @@ fun exn ->
    Log.warn "Failed to clean up processes: %s" (Printexc.to_string exn) ;
    unit
  in
  (* Remove temporary files. *)
  let kept_temp =
    try
      match Cli.options.temporary_file_mode with
      | Delete ->
          Temp.clean_up () ; false
      | Delete_if_successful ->
          if !test_result = Successful then (Temp.clean_up () ; false)
          else true
      | Keep ->
          true
    with exn ->
      Log.warn "Failed to clean up: %s" (Printexc.to_string exn) ;
      true
  in
  if kept_temp then
    Log.report "Temporary files can be found in: %s" Temp.main_dir ;
  (* Resolve all pending promises so that they won't do anything
     (like raise [Canceled]) during the next test. *)
  let* () =
    let handle_exception exn =
      if !already_logged_exn then
        (* In all likelihood the error we already logged is this one,
           as if an async promise raises [Failed] it causes the test
           to print the error and stop but the async promise is still
           rejected with [Failed].
           It is still possible that the error is actually unrelated,
           but we already printed an error for the user to debug so it's ok. *)
        unit
      else (
        (* This could happen if an async promise fails *after* the test
           was successful. In that case, the test is not that successful after all. *)
        Log.error "%s" (Printexc.to_string exn) ;
        test_result := Log.Failed ;
        unit )
    in
    Lwt.catch wait_for_async handle_exception
  in
  (* Display test result. *)
  Log.test_result !test_result title ;
  match !test_result with
  | Successful ->
      unit
  | Failed ->
      Log.report
        "Try again with: %s --verbose --test %s"
        Sys.argv.(0)
        (Log.quote_shell title) ;
      if Cli.options.keep_going then (
        at_exit (fun () -> exit 1) ;
        unit )
      else exit 1
  | Aborted ->
      exit 2

let test_should_be_run ~file ~title ~tags =
  List.for_all (fun tag -> List.mem tag tags) Cli.options.tags_to_run
  && (not
        (List.exists (fun tag -> List.mem tag tags) Cli.options.tags_not_to_run))
  && ( match Cli.options.tests_to_run with
     | [] ->
         true
     | titles ->
         List.mem title titles )
  &&
  match Cli.options.files_to_run with
  | [] ->
      true
  | files ->
      List.mem file files

let list file title tags =
  match tags with
  | [] ->
      Printf.printf "%s: %s (no tags)\n" file title
  | _ :: _ ->
      Printf.printf "%s: %s (tags: %s)\n" file title (String.concat ", " tags)

let tag_rex = rex "^[a-z0-9_]{1,32}$"

let check_tags tags =
  match List.filter (fun tag -> tag =~! tag_rex) tags with
  | [] ->
      ()
  | invalid_tags ->
      List.iter (Printf.eprintf "Invalid tag: %S\n") invalid_tags ;
      Printf.eprintf
        "Tags may only use lowercase letters, digits and underscores, and \
         must be at most 32 character long.\n" ;
      exit 1

module String_set = Set.Make (String)

let known_tags = ref String_set.empty

let register_tag tag = known_tags := String_set.add tag !known_tags

let () =
  at_exit
  @@ fun () ->
  let cli_tags =
    String_set.union
      (String_set.of_list Cli.options.tags_to_run)
      (String_set.of_list Cli.options.tags_not_to_run)
  in
  let unknown_tags = String_set.diff cli_tags !known_tags in
  match String_set.elements unknown_tags with
  | [] ->
      ()
  | unknown_tags ->
      List.iter (Printf.eprintf "Unknown tag: %s\n") unknown_tags ;
      exit 1

let run ~__FILE__ ~title ~tags f =
  let file = Filename.basename __FILE__ in
  check_tags tags ;
  List.iter register_tag tags ;
  if test_should_be_run ~file ~title ~tags then
    if Cli.options.list then list file title tags else really_run title f
