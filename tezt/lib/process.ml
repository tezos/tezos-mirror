(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(* An [echo] represents the standard output or standard error output of a process.
   Those outputs are duplicated: one copy is automatically logged,
   the other goes into [lwt_channel] so that the user can read it.

   [queue] is the bytes that have been received from the process, but that
   have not yet been read by the user of this module. Those bytes are
   split into chunks ([string]s). Those chunks are never empty.

   Strings of [queue] are references so that we can replace them when we
   read them partially. If efficiency becomes a concern, we could store
   a reference to an offset to avoid a call to String.sub. *)
type echo = {
  queue : string ref Queue.t;
  mutable lwt_channel : Lwt_io.input_channel option;
  mutable closed : bool;
  mutable pending : unit Lwt.u list;
}

let wake_up_echo echo =
  let pending = echo.pending in
  echo.pending <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending ()) pending

let push_to_echo echo string =
  (* Maintain the invariant that strings in the queue are never empty. *)
  if String.length string > 0 then (
    Queue.push (ref string) echo.queue ;
    wake_up_echo echo)

let close_echo echo =
  if not echo.closed then (
    echo.closed <- true ;
    wake_up_echo echo)

let create_echo () =
  let echo =
    {queue = Queue.create (); lwt_channel = None; closed = false; pending = []}
  in
  let rec read bytes ofs len =
    match Queue.peek_opt echo.queue with
    | None ->
        if echo.closed then return 0
        else
          (* Nothing to read, for now. *)
          let (promise, resolver) = Lwt.task () in
          echo.pending <- resolver :: echo.pending ;
          let* () = promise in
          read bytes ofs len
    | Some str_ref ->
        (* Note: we rely on the invariant that strings in the queue are never empty. *)
        let str_len = String.length !str_ref in
        if str_len <= len then (
          (* Caller requested more bytes than available in this item of the queue:
             return the item in full and remove it from the queue. *)
          (* use [Lwt_bytes.blit_from_string] once available *)
          Lwt_bytes.blit_from_bytes
            (Bytes.of_string !str_ref)
            0
            bytes
            ofs
            str_len ;
          let (_ : string ref option) = Queue.take_opt echo.queue in
          return str_len)
        else (
          (* Caller requested strictly less bytes than available in this item of the queue:
             return what caller requested, and only keep the remainder. *)
          (* use [Lwt_bytes.blit_from_string] once available *)
          Lwt_bytes.blit_from_bytes (Bytes.of_string !str_ref) 0 bytes ofs len ;
          str_ref := String.sub !str_ref len (str_len - len) ;
          return len)
  in
  let lwt_channel = Lwt_io.(make ~mode:input) read in
  echo.lwt_channel <- Some lwt_channel ;
  echo

let get_echo_lwt_channel echo =
  match echo.lwt_channel with
  | None ->
      (* Impossible: [lwt_channel] is filled by [Some ...] immediately after the [echo]
         is created by [create_echo]. *)
      assert false
  | Some lwt_channel -> lwt_channel

type hooks = {on_log : string -> unit; on_spawn : string -> string list -> unit}

(* Information which is specific to processes that run on remote runners. *)
type remote = {runner : Runner.t; pid : int Lwt.t}

type t = {
  id : int;
  name : string;
  command : string;
  arguments : string list;
  color : Log.Color.t;
  lwt_process : Lwt_process.process_full;
  log_status_on_exit : bool;
  stdout : echo;
  stderr : echo;
  hooks : hooks option;
  remote : remote option;
}

type failed_info = {
  name : string;
  command : string;
  arguments : string list;
  status : Unix.process_status option;
  expect_failure : bool;
  reason : String.t option;
}

exception Failed of failed_info

(** Converts the given [status] into a string explaining
    why the corresponding process has stopped.

    The resulting string is a subject-less sentence that
    assumes that the subject will be prepended. *)
let status_to_reason = function
  | Unix.WEXITED code -> Format.sprintf "exited with code %d" code
  | Unix.WSIGNALED code -> Format.sprintf "was killed by signal %d" code
  | Unix.WSTOPPED code -> Format.sprintf "was stopped by signal %d" code

let () =
  Printexc.register_printer @@ function
  | Failed {name; command; arguments; status; expect_failure; reason} ->
      let reason =
        Option.value
          ~default:
            (match status with
            | Some st -> status_to_reason st
            | None -> Printf.sprintf "exited")
          reason
      in
      Some
        (Printf.sprintf
           "%s%s %s (full command: %s)"
           name
           (if expect_failure then " was expected to fail," else "")
           reason
           (String.concat " " (List.map Log.quote_shell (command :: arguments))))
  | _ -> None

let get_unique_name =
  let name_counts = ref String_map.empty in
  fun name ->
    let index =
      match String_map.find_opt name !name_counts with None -> 0 | Some i -> i
    in
    name_counts := String_map.add name (index + 1) !name_counts ;
    name ^ "#" ^ string_of_int index

let fresh_id =
  let next = ref 0 in
  fun () ->
    let id = !next in
    incr next ;
    id

module ID_map = Map.Make (Int)

let live_processes = ref ID_map.empty

let show_signal code =
  if code = Sys.sigabrt then "SIGABRT"
  else if code = Sys.sigalrm then "SIGALRM"
  else if code = Sys.sigfpe then "SIGFPE"
  else if code = Sys.sighup then "SIGHUP"
  else if code = Sys.sigill then "SIGILL"
  else if code = Sys.sigint then "SIGINT"
  else if code = Sys.sigkill then "SIGKILL"
  else if code = Sys.sigpipe then "SIGPIPE"
  else if code = Sys.sigquit then "SIGQUIT"
  else if code = Sys.sigsegv then "SIGSEGV"
  else if code = Sys.sigterm then "SIGTERM"
  else if code = Sys.sigusr1 then "SIGUSR1"
  else if code = Sys.sigusr2 then "SIGUSR2"
  else if code = Sys.sigchld then "SIGCHLD"
  else if code = Sys.sigcont then "SIGCONT"
  else if code = Sys.sigstop then "SIGSTOP"
  else if code = Sys.sigtstp then "SIGTSTP"
  else if code = Sys.sigttin then "SIGTTIN"
  else if code = Sys.sigttou then "SIGTTOU"
  else if code = Sys.sigvtalrm then "SIGVTALRM"
  else if code = Sys.sigprof then "SIGPROF"
  else if code = Sys.sigbus then "SIGBUS"
  else if code = Sys.sigpoll then "SIGPOLL"
  else if code = Sys.sigsys then "SIGSYS"
  else if code = Sys.sigtrap then "SIGTRAP"
  else if code = Sys.sigurg then "SIGURG"
  else if code = Sys.sigxcpu then "SIGXCPU"
  else if code = Sys.sigxfsz then "SIGXFSZ"
  else string_of_int code

let wait process =
  let* status = process.lwt_process#status in
  (* If we already removed [process] from [!live_processes], we already logged
     the exit status. *)
  if ID_map.mem process.id !live_processes then (
    live_processes := ID_map.remove process.id !live_processes ;
    if process.log_status_on_exit then
      match status with
      | WEXITED code -> Log.debug "%s exited with code %d." process.name code
      | WSIGNALED code ->
          Log.debug
            "%s was killed by signal %s."
            process.name
            (show_signal code)
      | WSTOPPED code ->
          Log.debug
            "%s was stopped by signal %s."
            process.name
            (show_signal code)) ;
  return status

(* Read process outputs and log them.
   Also take care of removing the process from [live_processes] on termination. *)
let handle_process ~log_output process =
  let rec handle_output name (ch : Lwt_io.input_channel) echo =
    let* line = Lwt_io.read_line_opt ch in
    match line with
    | None ->
        close_echo echo ;
        Lwt_io.close ch
    | Some line ->
        if log_output then (
          Log.debug ~prefix:name ~color:process.color "%s" line ;
          Option.iter (fun hooks -> hooks.on_log line) process.hooks) ;
        push_to_echo echo line ;
        (* TODO: here we assume that all lines end with "\n",
             but it may not always be the case:
           - there may be lines ending with "\r\n";
           - the last line may not end with "\n" before the EOF. *)
        push_to_echo echo "\n" ;
        handle_output name ch echo
  in
  let* () = handle_output process.name process.lwt_process#stdout process.stdout
  and* () = handle_output process.name process.lwt_process#stderr process.stderr
  and* _ = wait process in
  unit

(** [parse_current_environment ()], given that the current environment
    is "K1=V2; K2=V2" (see `export` in a terminal)
    returns a map {K1->V1; K2->V2}. See [to_key_equal_value]
    for a related function. *)
let parse_current_environment : unit -> string String_map.t =
 fun () ->
  let parse_env_kv key_value : (string * string) option =
    String.index_opt key_value '='
    |> Option.map (fun i ->
           ( Re.Str.string_before key_value i,
             Re.Str.string_after key_value (i + 1) ))
  in
  Unix.environment () |> Array.to_seq
  |> Seq.filter_map parse_env_kv
  |> String_map.of_seq

(** [to_key_equal_value kv_map], given that kv_map is {K1->V1; K2->V2}
    returns the array ["K1=V1"; "K2=V2"]. See [parse_current_environment]
    for a related function *)
let to_key_equal_value (kv_map : string String_map.t) : string array =
  kv_map |> String_map.to_seq
  |> Seq.map (fun (name, value) -> name ^ "=" ^ value)
  |> Array.of_seq

let spawn_with_stdin ?runner ?(log_status_on_exit = true) ?(log_output = true)
    ?name ?(color = Log.Color.FG.cyan) ?(env = String_map.empty) ?hooks command
    arguments =
  let name = Option.value ~default:(get_unique_name command) name in
  Option.iter (fun hooks -> hooks.on_spawn command arguments) hooks ;
  Log.command ~color:Log.Color.bold ~prefix:name command arguments ;
  let lwt_command =
    match runner with
    | None -> (command, Array.of_list (command :: arguments))
    | Some runner ->
        let local_env = String_map.bindings env in
        let (ssh, ssh_args) =
          Runner.wrap_with_ssh_pid runner {local_env; name = command; arguments}
        in
        (ssh, Array.of_list (ssh :: ssh_args))
  in
  let lwt_process =
    match runner with
    | None ->
        let env =
          (* Merge [current_env] and [env], choosing [env] on common keys: *)
          String_map.union
            (fun _ _ new_val -> Some new_val)
            (parse_current_environment ())
            env
          |> to_key_equal_value
        in
        Lwt_process.open_process_full ~env lwt_command
    | Some _runner -> Lwt_process.open_process_full lwt_command
  in
  let remote =
    let open Lwt.Infix in
    match runner with
    | None -> None
    | Some runner ->
        let pid =
          Lwt_io.read_line lwt_process#stdout >|= fun pid ->
          match int_of_string_opt pid with
          | Some pid -> pid
          | None ->
              raise
                (Failed
                   {
                     name;
                     command;
                     arguments;
                     status = None;
                     expect_failure = false;
                     reason = Some "unable to read remote process PID";
                   })
        in
        Some {runner; pid}
  in
  let process =
    {
      id = fresh_id ();
      name;
      command;
      arguments;
      color;
      lwt_process;
      log_status_on_exit;
      stdout = create_echo ();
      stderr = create_echo ();
      hooks;
      remote;
    }
  in
  live_processes := ID_map.add process.id process !live_processes ;
  Background.register (handle_process ~log_output process) ;
  (process, process.lwt_process#stdin)

let spawn ?runner ?log_status_on_exit ?log_output ?name ?color ?env ?hooks
    command arguments =
  let (process, stdin) =
    spawn_with_stdin
      ?runner
      ?log_status_on_exit
      ?log_output
      ?name
      ?color
      ?env
      ?hooks
      command
      arguments
  in
  Background.register (Lwt_io.close stdin) ;
  process

(* Propagate the signal in case of remote runner. *)
let kill_remote_if_needed process =
  match process.remote with
  | None -> ()
  | Some {pid; runner} ->
      let open Lwt in
      let open Infix in
      Background.register
        ( ( pid >|= fun pid ->
            let command = "kill" in
            let arguments = ["-9"; "-P"; string_of_int pid] in
            let shell =
              Runner.Shell.(
                redirect_stderr (cmd [] command arguments) "/dev/null")
            in
            Runner.wrap_with_ssh runner shell )
        >>= fun (ssh, ssh_args) ->
          let cmd = (ssh, Array.of_list (ssh :: ssh_args)) in
          Lwt_process.exec cmd >>= fun _ -> Lwt.return_unit )

let terminate (process : t) =
  Log.debug "Send SIGTERM to %s." process.name ;
  kill_remote_if_needed process ;
  process.lwt_process#kill Sys.sigterm

let kill (process : t) =
  Log.debug "Send SIGKILL to %s." process.name ;
  kill_remote_if_needed process ;
  process.lwt_process#terminate

let pid (process : t) = process.lwt_process#pid

let validate_status ?(expect_failure = false) status =
  match status with
  | Unix.WEXITED n
    when (n = 0 && not expect_failure) || (n <> 0 && expect_failure) ->
      Ok ()
  | _ -> Error (`Invalid_status (status_to_reason status))

let check ?(expect_failure = false) process =
  let* status = wait process in
  match validate_status ~expect_failure status with
  | Ok () -> unit
  | Error (`Invalid_status reason) ->
      raise
        (Failed
           {
             name = process.name;
             command = process.command;
             arguments = process.arguments;
             status = Some status;
             expect_failure;
             reason = Some reason;
           })

let run ?log_status_on_exit ?name ?color ?env ?expect_failure command arguments
    =
  spawn ?log_status_on_exit ?name ?color ?env command arguments
  |> check ?expect_failure

let clean_up () =
  let list = ID_map.bindings !live_processes |> List.map snd in
  List.iter terminate list ;
  Lwt_list.iter_p
    (fun process ->
      let* _ = wait process in
      unit)
    list

let stdout process = get_echo_lwt_channel process.stdout

let stderr process = get_echo_lwt_channel process.stderr

let name (process : t) = process.name

let check_and_read ?expect_failure ~channel_getter process =
  let* () = check ?expect_failure process
  and* output = Lwt_io.read (channel_getter process) in
  return output

let check_and_read_both ?expect_failure process =
  let* () = check ?expect_failure process
  and* out = Lwt_io.read (stdout process)
  and* err = Lwt_io.read (stderr process) in
  return (out, err)

let check_and_read_stdout = check_and_read ~channel_getter:stdout

let check_and_read_stderr = check_and_read ~channel_getter:stderr

let run_and_read_stdout ?log_status_on_exit ?name ?color ?env ?expect_failure
    command arguments =
  let process = spawn ?log_status_on_exit ?name ?color ?env command arguments in
  check_and_read_stdout ?expect_failure process

let run_and_read_stderr ?log_status_on_exit ?name ?color ?env ?expect_failure
    command arguments =
  let process = spawn ?log_status_on_exit ?name ?color ?env command arguments in
  check_and_read_stdout ?expect_failure process

let check_error ?exit_code ?msg process =
  let* status = wait process in
  let* err_msg = Lwt_io.read (stderr process) in
  let error =
    {
      name = process.name;
      command = process.command;
      arguments = process.arguments;
      status = Some status;
      expect_failure = true;
      reason = None;
    }
  in
  match status with
  | WEXITED n ->
      if not (Option.fold ~none:(n <> 0) ~some:(( = ) n) exit_code) then
        raise
          (Failed
             {
               error with
               reason =
                 Some
                   (Option.fold
                      ~none:" with any non-zero code"
                      ~some:(fun exit_code ->
                        sf " with code %d but failed with code %d" exit_code n)
                      exit_code);
             }) ;
      Option.iter
        (fun msg ->
          if err_msg =~! msg then
            raise
              (Failed
                 {
                   error with
                   reason =
                     Some (sf " but failed with stderr =~! %s" (show_rex msg));
                 }))
        msg ;
      unit
  | _ -> raise (Failed error)

let program_path program =
  Lwt.catch
    (fun () ->
      let* path = run_and_read_stdout "sh" ["-c"; "command -v " ^ program] in
      return (Some (String.trim path)))
    (fun _ -> return None)

type nonrec 'a runnable = (t, 'a) runnable

let runnable_map f {value; run} =
  let run x =
    let* output = run x in
    return (f output)
  in
  {value; run}
