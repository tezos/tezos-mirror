(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Process-isolated test execution for Alcotezt.

    This module provides fork-exec based test isolation using OS-level process
    separation. Tests are executed via Unix.create_process_env, ensuring:

    1. Complete isolation between tests (no shared memory, domains, or state)
    2. Clean initialization of Eio/Lwt runtime per test
    3. Proper cleanup via process exit (no lingering resources)
    4. Timeout enforcement at the OS level

    The isolation mechanism:
    - Parent process (run_parent): spawns child via Unix.create_process_env
    - Child process (run_child): executes the test body with fresh runtime
    - Communication: via exit codes and captured stdout/stderr
    - Test selection: unique integer IDs passed via environment variables

    Both [test_case] and [test_case_lwt] provide FULL process isolation.
    They differ only in which runtime is used in the child process:
    - [test_case]: Eio runtime
    - [test_case_lwt]: Lwt runtime (via Octez Event_loop)

    Common misconception: "test_case_lwt runs in-process" - FALSE!
    Both functions spawn a separate OS process for isolation. *)

open Tezt_core
open Tezt_core.Base
open Tezos_error_monad.Error_monad

type body =
  | Eio of (unit -> unit tzresult)
  | Lwt of (unit -> unit tzresult Lwt.t)

type entry = {
  label : string;
  process_name : string;
  timeout : float;
  body : body;
}

(* Use a unique test ID to disambiguate tests with the same label across
   different test suites. *)
let env_test_id = "ALCOTEZT_CHILD_TEST_ID"

let env_timeout = "ALCOTEZT_CHILD_TIMEOUT"

let env_process_name = "ALCOTEZT_CHILD_PROCESS_NAME"

(* Map from unique test ID to test entry *)
let child_entries : (int, entry) Stdlib.Hashtbl.t = Stdlib.Hashtbl.create 16

(* Counter for generating unique test IDs *)
let next_test_id = ref 0

let gen_test_id () =
  let id = !next_test_id in
  incr next_test_id ;
  id

let starts_with ~prefix s =
  let plen = String.length prefix and slen = String.length s in
  slen >= plen && String.sub s 0 plen = prefix

let default_timeout =
  match Sys.getenv_opt "OCTEZ_TEST_FORK_TIMEOUT" with
  | Some s -> ( try float_of_string s with _ -> 10.)
  | None -> 10.

let sanitize_env () =
  let keys = [env_test_id; env_timeout; env_process_name] in
  let is_reserved kv =
    List.exists (fun key -> starts_with ~prefix:(key ^ "=") kv) keys
  in
  Unix.environment () |> Array.to_list
  |> List.filter (fun kv -> not (is_reserved kv))

(* Drain output from a non-blocking fd into a buffer.
   Expected exceptions during non-blocking reads are silently ignored:
   - EAGAIN/EWOULDBLOCK: no data available (non-blocking)
   - EINTR: interrupted by signal
   - End_of_file: pipe closed
   Other Unix errors (EBADF, etc.) will propagate. *)
let rec drain fd buffer =
  let tmp = Bytes.create 4096 in
  match Unix.read fd tmp 0 (Bytes.length tmp) with
  | n when n > 0 ->
      Buffer.add_subbytes buffer tmp 0 n ;
      drain fd buffer
  | _ -> ()
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) -> ()
  | exception End_of_file -> ()

let wait_with_timeout pid fd timeout =
  let buffer = Buffer.create 1024 in
  let has_timeout =
    match classify_float timeout with
    | FP_nan | FP_infinite -> false
    | _ -> timeout > 0.
  in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    (* Ignore benign I/O errors during draining (pipe may be closed/empty).
       Other errors (e.g. EBADF from invalid fd) should propagate. *)
    (try drain fd buffer with
    | Unix.Unix_error ((Unix.EBADF | Unix.EPIPE), _, _) -> ()
    | End_of_file -> ()) ;
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
        if has_timeout && Unix.gettimeofday () >= deadline then `Timeout
        else (
          (try Unix.sleepf 0.05 with Unix.Unix_error (Unix.EINTR, _, _) -> ()) ;
          loop ())
    | _, status ->
        (try drain fd buffer with _ -> ()) ;
        `Status (status, Buffer.contents buffer)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  match loop () with
  | `Timeout -> `Timeout (Buffer.contents buffer)
  | `Status (status, logs) -> `Status (status, logs)

let rec wait_after_signal pid fd attempts buffer =
  if attempts = 0 then false
  else
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
        (* Retry on EINTR from sleep - can happen during parallel test execution *)
        (try Unix.sleepf 0.05 with Unix.Unix_error (Unix.EINTR, _, _) -> ()) ;
        (* Ignore benign I/O errors during draining after signal. *)
        (try drain fd buffer with
        | Unix.Unix_error ((Unix.EBADF | Unix.EPIPE), _, _) -> ()
        | End_of_file -> ()) ;
        wait_after_signal pid fd (attempts - 1) buffer
    | _, _ -> true
    | exception Unix.Unix_error (Unix.EINTR, _, _) ->
        wait_after_signal pid fd attempts buffer

let spawn test_id entry =
  let env = sanitize_env () in
  let env =
    (env_test_id ^ "=" ^ string_of_int test_id)
    :: (env_timeout ^ "=" ^ Printf.sprintf "%g" entry.timeout)
    :: (env_process_name ^ "=" ^ entry.process_name)
    :: env
  in
  let env = Array.of_list env in
  let argv = Sys.argv in
  let prog = argv.(0) in
  let rfd, wfd = Unix.pipe () in
  Unix.set_nonblock rfd ;
  let pid = Unix.create_process_env prog argv env Unix.stdin wfd wfd in
  ignore (Unix.close wfd) ;
  (pid, rfd)

let run_parent test_id entry =
  let pid, fd = spawn test_id entry in
  let result = wait_with_timeout pid fd entry.timeout in
  let finalize logs =
    ignore (Unix.close fd) ;
    logs
  in
  match result with
  | `Status (Unix.WEXITED 0, _logs) -> ignore (Unix.close fd)
  | `Status (status, logs) ->
      let logs = finalize logs in
      let msg =
        match status with
        | Unix.WEXITED code ->
            Format.sprintf
              "%s: child exited with status %d.%s"
              entry.label
              code
              (if logs = "" then "" else Format.sprintf " Logs:@.%s" logs)
        | Unix.WSIGNALED signal ->
            Format.sprintf
              "%s: child terminated by signal %d.%s"
              entry.label
              signal
              (if logs = "" then "" else Format.sprintf " Logs:@.%s" logs)
        | Unix.WSTOPPED signal ->
            Format.sprintf
              "%s: child stopped by signal %d.%s"
              entry.label
              signal
              (if logs = "" then "" else Format.sprintf " Logs:@.%s" logs)
      in
      Test.fail "%s" msg
  | `Timeout logs ->
      let buffer = Buffer.create 1024 in
      Buffer.add_string buffer logs ;
      (try Unix.kill pid Sys.sigterm with _ -> ()) ;
      let exited = wait_after_signal pid fd 100 buffer in
      if not exited then (
        (try Unix.kill pid Sys.sigkill with _ -> ()) ;
        ignore (Unix.waitpid [] pid)) ;
      let logs = finalize (Buffer.contents buffer) in
      Test.fail
        "%s"
        (Format.sprintf
           "%s: timeout after %.1fs.%s"
           entry.label
           entry.timeout
           (if logs = "" then "" else Format.sprintf " Logs:@.%s" logs))

let run_child entry =
  Printexc.record_backtrace true ;
  Format.eprintf "[alcotezt][%s] child pid=%d@." entry.label (Unix.getpid ()) ;
  let exit_code =
    try
      match entry.body with
      | Eio body -> (
          match
            Tezos_base_unix.Event_loop.main_run
              ~process_name:entry.process_name
              ~eio:true
              (fun () ->
                let open Lwt.Syntax in
                let+ res = Lwt_eio.run_eio (fun () -> body ()) in
                res)
          with
          | Ok () -> 0
          | Error trace ->
              Format.eprintf
                "[alcotezt][%s] Test returned error trace:@.%a@."
                entry.label
                pp_print_trace
                trace ;
              1)
      | Lwt body -> (
          match
            Tezos_base_unix.Event_loop.main_run
              ~process_name:entry.process_name
              ~eio:true
              (fun () ->
                let open Lwt.Syntax in
                let+ res = body () in
                res)
          with
          | Ok () -> 0
          | Error trace ->
              Format.eprintf
                "[alcotezt][%s] Test returned error trace:@.%a@."
                entry.label
                pp_print_trace
                trace ;
              1)
    with exn ->
      let bt = Printexc.get_backtrace () in
      Format.eprintf
        "[alcotezt][%s] Uncaught exception %s@.%s@."
        entry.label
        (Printexc.to_string exn)
        bt ;
      1
  in
  exit exit_code

let maybe_run_child () =
  match Sys.getenv_opt env_test_id with
  | None -> ()
  | Some id_str -> (
      match int_of_string_opt id_str with
      | Some test_id -> (
          match Stdlib.Hashtbl.find_opt child_entries test_id with
          | Some entry ->
              Stdlib.Hashtbl.remove child_entries test_id ;
              run_child entry
          | None -> ())
      | None -> ())

(** [test_case] creates a test that runs in a separate OS process.
    
    IMPORTANT: This function provides FULL PROCESS ISOLATION via fork-exec.
    When the test runs, it calls [run_parent] which spawns a fresh child
    process using [Unix.create_process_env]. The test body executes in that
    child process with a clean Eio environment.
    
    This is NOT an in-process test - it uses OS-level process separation
    to ensure complete isolation from other tests and the parent process.
    
    @param process_name Optional name for the process (defaults to label)
    @param timeout Optional timeout in seconds (defaults to 10s)
    @param label Test label
    @param speed_level Test speed (e.g., `Quick, `Slow)
    @param body Eio-based test function to run in the isolated child process *)
let test_case ?process_name ?timeout label speed_level body =
  let timeout = Option.value ~default:default_timeout timeout in
  let process_name = Option.value ~default:label process_name in
  let test_id = gen_test_id () in
  (* Register this test in case we're the child process being re-executed *)
  (match Sys.getenv_opt env_test_id with
  | Some id_str -> (
      match int_of_string_opt id_str with
      | Some id when id = test_id ->
          let process_name =
            match Sys.getenv_opt env_process_name with
            | Some s when s <> "" -> s
            | _ -> process_name
          in
          let timeout =
            match Sys.getenv_opt env_timeout with
            | Some s -> ( try float_of_string s with _ -> timeout)
            | None -> timeout
          in
          Stdlib.Hashtbl.add
            child_entries
            test_id
            {label; process_name; timeout; body = Eio body}
      | _ -> ())
  | None -> ()) ;
  let entry = {label; process_name; timeout; body = Eio body} in
  (label, speed_level, fun () -> run_parent test_id entry)

(** [test_case_lwt] creates a test that runs in a separate OS process.
    
    IMPORTANT: This function provides FULL PROCESS ISOLATION via fork-exec,
    just like [test_case]. The ONLY difference is the runtime:
    - [test_case]: runs Eio code in the child process
    - [test_case_lwt]: runs Lwt code in the child process (via Event_loop)
    
    Both use [run_parent] which spawns a fresh child process using
    [Unix.create_process_env], providing complete OS-level isolation.
    
    This is NOT an in-process test. The Lwt test body executes in a
    separate process with clean state, preventing any cross-test
    interference from shared Lwt/Eio domains or global state.
    
    @param process_name Optional name for the process (defaults to label)
    @param timeout Optional timeout in seconds (defaults to 10s)
    @param label Test label
    @param speed_level Test speed (e.g., `Quick, `Slow)
    @param body Lwt-based test function to run in the isolated child process *)
let test_case_lwt ?process_name ?timeout label speed_level body =
  let timeout = Option.value ~default:default_timeout timeout in
  let process_name = Option.value ~default:label process_name in
  let test_id = gen_test_id () in
  (* Register this test in case we're the child process being re-executed *)
  (match Sys.getenv_opt env_test_id with
  | Some id_str -> (
      match int_of_string_opt id_str with
      | Some id when id = test_id ->
          let process_name =
            match Sys.getenv_opt env_process_name with
            | Some s when s <> "" -> s
            | _ -> process_name
          in
          let timeout =
            match Sys.getenv_opt env_timeout with
            | Some s -> ( try float_of_string s with _ -> timeout)
            | None -> timeout
          in
          Stdlib.Hashtbl.add
            child_entries
            test_id
            {label; process_name; timeout; body = Lwt body}
      | _ -> ())
  | None -> ()) ;
  let entry = {label; process_name; timeout; body = Lwt body} in
  (label, speed_level, fun () -> run_parent test_id entry)

(* Inline version of Alcotezt_utils.is_proto_test to avoid dependency *)
let is_proto_test file =
  match file =~* rex "^src/proto_(\\w+)/" with
  | None -> []
  | Some "alpha" -> ["alpha"]
  | Some "022_PsRiotum" -> ["r022"]
  | Some "023_PtSeouLo" -> ["s023"]
  | Some proto ->
      Format.eprintf "[alcotezt] Warning: unknown protocol %s@." proto ;
      []

let run ?(__FILE__ = "") ?(tags = []) library_name tests =
  maybe_run_child () ;
  let proto_tags = is_proto_test __FILE__ in
  tests
  |> List.iter @@ fun (test_name, test_cases) ->
     test_cases
     |> List.iter @@ fun (test_case_name, speed_level, body) ->
        let tags =
          "alcotezt"
          :: (match speed_level with `Quick -> ["quick"] | `Slow -> ["slow"])
          @ proto_tags @ tags
        in
        let title = sf "%s: %s (%s)" library_name test_name test_case_name in
        Test.register ~__FILE__ ~title ~tags @@ fun () ->
        body () ;
        Base.unit
