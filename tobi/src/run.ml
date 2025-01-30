(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Misc

(* Similar to [Unix.open_process_full], but without a channel for [stdin]
   and merges [stdout] and [stderr]. Merging both outputs allows to read
   both at the same time without dealing with concurrency and thus greatly
   simplifies the code, at the cost of not being able to distinguish the outputs. *)
let spawn executable arguments =
  let argv = Array.of_list (executable :: arguments) in
  match Unix.pipe () with
  | exception Unix.Unix_error (code, _, _) ->
      fail
        "failed to run %s"
        executable
        ~reason:["failed to create pipe for stdout"; Unix.error_message code]
  | pipe_exit, pipe_entrance -> (
      match
        Unix.set_close_on_exec pipe_exit ;
        Unix.create_process
          executable
          argv
          Unix.stdin
          pipe_entrance
          pipe_entrance
      with
      | exception Unix.Unix_error (code, _, _) ->
          close pipe_exit ;
          close pipe_entrance ;
          fail "failed to run %s" executable ~reason:[Unix.error_message code]
      | pid ->
          close pipe_entrance ;
          Ok (pid, pipe_exit))

(* Read the output of an executable from file descriptor [fd], line by line.
   Call [on_read_line] on each line of output.
   The [executable] argument is the name of the program; it is only used in error messages.
   This function is blocking. *)
let input_lines executable fd on_read_line =
  let rec read ch =
    match input_line ch with
    | exception End_of_file -> unit
    | exception Sys_error message ->
        fail
          "failed to run %s"
          executable
          ~reason:["failed to read process output"; message]
    | line ->
        on_read_line line ;
        read ch
  in
  read (Unix.in_channel_of_descr fd)

(* Wait until the process with PID [pid] returns, and check that its exit code is 0.
   This function is blocking. *)
let wait executable pid =
  match Unix.waitpid [] pid with
  | exception Unix.Unix_error (code, _, _) ->
      fail
        "failed to run %s"
        executable
        ~reason:
          ["failed to wait for process to terminate"; Unix.error_message code]
  | _, WEXITED 0 -> unit
  | _, WEXITED code ->
      fail
        "failed to run %s"
        executable
        ~reason:
          [
            (if code = 127 then
               "process exited with code 127, maybe it is not in PATH"
             else sf "process exited with code %d" code);
          ]
  | _, WSIGNALED code ->
      fail
        "failed to run %s"
        executable
        ~reason:[sf "process was killed by signal %d" code]
  | _, WSTOPPED code ->
      fail
        "failed to run %s"
        executable
        ~reason:[sf "process was stopped by signal %d" code]

(* Do something while in another working directory.
   Ensure the working directory is restored after that. *)
let with_working_directory new_working_directory f =
  let old_working_directory = Sys.getcwd () in
  Fun.protect ~finally:(fun () -> Sys.chdir old_working_directory) @@ fun () ->
  Sys.chdir new_working_directory ;
  f ()

let with_working_directory_opt new_working_directory f =
  match new_working_directory with
  | None -> f ()
  | Some new_working_directory -> with_working_directory new_working_directory f

let command ?working_directory ?on_read_line executable arguments =
  let on_read_line =
    match on_read_line with Some f -> f | None -> echo "[%s] %s" executable
  in
  (* Switch working directory while executing the program, if requested. *)
  with_working_directory_opt working_directory @@ fun () ->
  (* Run the program. *)
  let* pid, pipe_exit = spawn executable arguments in
  Fun.protect ~finally:(fun () -> close pipe_exit) @@ fun () ->
  (* Read the output first before waiting for the program to exit.
     Doing it the other way around may cause the program to block while trying to output,
     causing a deadlock. *)
  let read_result = input_lines executable pipe_exit on_read_line in
  let wait_result = wait executable pid in
  (* We always want to [wait], otherwise the program could stay as a zombie process.
     But this means we may have two errors.
     In that case we just pick the error from [wait]. *)
  match (read_result, wait_result) with
  | Ok (), Ok () -> unit
  | _, (Error _ as x) | (Error _ as x), Ok _ -> x

let command_lines ?working_directory executable arguments =
  let lines = ref [] in
  let* () =
    command
      ?working_directory
      ~on_read_line:(fun line -> lines := line :: !lines)
      executable
      arguments
  in
  Ok (List.rev !lines)
