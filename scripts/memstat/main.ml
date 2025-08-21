(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let sf = Printf.sprintf

let ( let* ) = Result.bind

let echo x = Printf.ksprintf print_endline x

let echo_e x = Printf.ksprintf prerr_endline x

(* List.map for the result monad. *)
let list_map_r l f =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | head :: tail ->
        let* head = f head in
        loop (head :: acc) tail
  in
  loop [] l

(* Possible values for --peak. *)
type peak_mode = No_peak | Peak | Only_peak

(* Get the memory usage of a single process (not recursively)
   by reading from /proc/PID/smaps.
   More specifically, we read the Pss field, which gives a more representative memory usage
   as it divides shared memory by the number of processes that share this memory. *)
(* https://stackoverflow.com/questions/131303/how-can-i-measure-the-actual-memory-usage-of-an-application-or-process *)
let get_memory pid =
  try
    let ch = open_in ("/proc/" ^ string_of_int pid ^ "/smaps") in
    Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
    let rec sum_pss acc =
      match input_line ch with
      | exception End_of_file -> acc
      | line ->
          let parse_error () =
            failwith (Printf.sprintf "failed to parse %S" line)
          in
          let line = String.trim line in
          let value =
            if not (String.starts_with ~prefix:"Pss:" line) then 0
            else if not (String.ends_with ~suffix:"kB" line) then parse_error ()
            else
              let value =
                String.sub
                  line
                  (String.length "Pss:")
                  (String.length line - String.length "Pss:"
                 - String.length "kB")
                |> String.trim
              in
              match int_of_string_opt value with
              | None -> parse_error ()
              | Some value -> value
          in
          sum_pss (acc + value)
    in
    Ok (sum_pss 0)
  with Sys_error message | Failure message -> Error message

(* Information about a process, its memory usage, and its children.
   - [command] is the process name (from /proc/PID/comm).
   - [memory_kb] is the amount of memory used by [pid], in kB.
   - [children] is the list of children of [pid] according to /proc/PID/task/*/children.
   - [total_memory_kb] is [memory_kb] plus the sum of the [total_memory_kb] fields
     of [children]; it is the total memory used by [pid] and its children. *)
type process_info = {
  pid : int;
  command : string;
  memory_kb : int;
  children : process_info list;
  total_memory_kb : int;
}

(* Helper function to measure peak memory usage.
   Sets [peak_info_ref] to [info] if [info] has a bigger [total_memory_kb]. *)
let merge_into_peak_info info peak_info_ref =
  match !peak_info_ref with
  | None -> peak_info_ref := Some info
  | Some old ->
      if info.total_memory_kb > old.total_memory_kb then
        peak_info_ref := Some info

(* Read /proc/PID/task/*/children to get the list of children of a process.
   Returns the list of PIDs of all children. *)
let get_children pid =
  try
    (* I think a task is the same as a thread. *)
    let task_path = "/proc/" ^ string_of_int pid ^ "/task" in
    let parse_tid line =
      match int_of_string_opt (String.trim line) with
      | None ->
          failwith (sf "failed to parse %S from %s as a TID" line task_path)
      | Some pid -> pid
    in
    let task_ids =
      Sys.readdir task_path |> Array.to_list |> List.map parse_tid
    in
    let get_task_children tid =
      let task_children_path =
        task_path ^ "/" ^ string_of_int tid ^ "/children"
      in
      let ch = open_in task_children_path in
      Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
      let parse_pid line =
        match int_of_string_opt line with
        | None ->
            failwith
              (sf "failed to parse %S from %s as a PID" line task_children_path)
        | Some pid -> pid
      in
      In_channel.input_lines ch
      |> List.map (String.split_on_char ' ')
      |> List.flatten |> List.map String.trim
      |> List.filter (( <> ) "")
      |> List.map parse_pid
    in
    Ok (List.flatten (List.map get_task_children task_ids))
  with Sys_error message | Failure message -> Error message

(* Read /proc/PID/comm to get the command that was used to run a process given its PID.
   The command is truncated to a rather short but still helpful string. *)
let get_command pid =
  let path = "/proc/" ^ string_of_int pid ^ "/comm" in
  try
    let ch = open_in path in
    Fun.protect ~finally:(fun () -> close_in ch) @@ fun () -> Ok (input_line ch)
  with Sys_error message -> Error message

(* Get the [process_info] record for a given PID.
   If [recursively] is [true], also do this for children, recursively,
   so that the [children] field of the resulting [process_info] is accurate.
   If [recursively] is [false], set [children] to the empty list instead. *)
let rec get_process_info ~recursively pid =
  let* command = get_command pid in
  let* memory_kb = get_memory pid in
  let* children =
    if recursively then
      let* children = get_children pid in
      list_map_r children (get_process_info ~recursively)
    else Ok []
  in
  let total_memory_kb =
    List.fold_left
      (fun acc child -> acc + child.total_memory_kb)
      memory_kb
      children
  in
  Ok {pid; command; memory_kb; children; total_memory_kb}

(* Convert an amount of kilobytes to a string for display purposes. *)
let show_bytes ~bytes kb =
  if bytes then
    (* TODO: is it 1024? Should be 1000 if kB, 1024 would be kiB, but need to check. *)
    Printf.sprintf "%d" (kb * 1000)
  else Printf.sprintf "%d kB" kb

(* Display a [process_info] on [stdout]. *)
let output_process_info ~verbose ~bytes info =
  (if verbose then
     let rec loop level info =
       let indent = String.make (level * 2) ' ' in
       echo
         "%s[%d] %s (%s)"
         indent
         info.pid
         (show_bytes ~bytes info.memory_kb)
         info.command ;
       List.iter (loop (level + 1)) info.children
     in
     loop 0 info) ;
  let bytes_string = show_bytes ~bytes info.total_memory_kb in
  if bytes then echo "%s" bytes_string else echo "Total: %s" bytes_string

(* Display the [process_info] containing peak memory usage. *)
let output_peak_info ~verbose ~peak ~bytes peak_info_opt =
  match (peak, peak_info_opt) with
  | No_peak, _ | _, None -> ()
  | Peak, Some peak_info ->
      echo "Peak:" ;
      output_process_info ~verbose ~bytes peak_info
  | Only_peak, Some peak_info -> output_process_info ~verbose ~bytes peak_info

(* Open a file and do something with it.
   Ensure the file is then closed. *)
let with_openfile path flags f =
  match Unix.openfile path flags 0o600 with
  | exception Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)
  | file -> Fun.protect ~finally:(fun () -> Unix.close file) @@ fun () -> f file

(* Measure the memory usage of a process that is a child process of memstat
   until this child process exits. *)
let watch_child ~recursively ~verbose ~frequency ~peak ~bytes pid =
  let peak_info = ref None in
  let rec loop () =
    match Unix.waitpid [WNOHANG] pid with
    | exception Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)
    | pid', status ->
        if pid' = pid then (
          match status with
          | WEXITED n ->
              if verbose then echo "Process exited with code %d" n ;
              Ok ()
          | WSIGNALED _ ->
              if verbose then echo "Process was killed by signal" ;
              Ok ()
          | WSTOPPED _ ->
              if verbose then echo "Process was stopped by signal" ;
              Ok ())
        else (
          (match get_process_info ~recursively pid with
          | Error message -> echo_e "Failed to get process info: %s" message
          | Ok info -> (
              merge_into_peak_info info peak_info ;
              match peak with
              | No_peak | Peak -> output_process_info ~verbose ~bytes info
              | Only_peak -> ())) ;
          Unix.sleepf (1. /. frequency) ;
          loop ())
  in
  let result = loop () in
  output_peak_info ~verbose ~peak ~bytes !peak_info ;
  result

(* Measure the memory usage of a process that is not necessarily a child process
   of memstat, until memstat is killed or interrupted by Ctrl+C. *)
let watch_pid ~recursively ~verbose ~frequency ~bytes pid =
  let peak_info = ref None in
  let rec loop () =
    (match get_process_info ~recursively pid with
    | Error message -> echo_e "Failed to get process info: %s" message
    | Ok info ->
        merge_into_peak_info info peak_info ;
        output_process_info ~verbose ~bytes info) ;
    Unix.sleepf (1. /. frequency) ;
    loop ()
  in
  loop ()

(* Run a process, and measure its memory usage until it exits. *)
let run_and_watch ~recursively ~verbose ~frequency ~peak ~bytes ~stdout_path
    ~stderr_path executable arguments =
  with_openfile stdout_path [O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC]
  @@ fun process_stdout ->
  with_openfile stderr_path [O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC]
  @@ fun process_stderr ->
  let argv = Array.of_list (executable :: arguments) in
  match
    Unix.create_process executable argv Unix.stdin process_stdout process_stderr
  with
  | exception Unix.Unix_error (code, _, _) -> Error (Unix.error_message code)
  | pid -> watch_child ~recursively ~verbose ~frequency ~peak ~bytes pid

(* Entry point of memstat. *)
let () =
  (* Parse command-line arguments. *)
  Clap.description "Measure a process memory usage." ;
  let verbose =
    Clap.flag
      ~set_long:"verbose"
      ~set_short:'v'
      ~description:
        "Output more information, such as information about child processes."
      false
  in
  let recursively =
    Clap.flag
      ~set_long:"recursively"
      ~set_short:'r'
      ~description:
        "Measure memory usage of child processes as well, recursively, to \
         output the total."
      false
  in
  let frequency =
    Clap.default_float
      ~long:"frequency"
      ~short:'f'
      ~description:
        "How many times to measure per second, if measuring more than once."
      10.
  in
  let peak =
    Clap.flag_enum
      [
        (["no-peak"], [], No_peak);
        (["peak"], [], Peak);
        (["only-peak"], [], Only_peak);
      ]
      ~description:
        "Whether to output peak memory usage at the end (--peak or \
         --only-peak), or not (--no-peak). With --only-peak, do not output \
         values other than peak memory usage, and do not output the 'Peak:' \
         line, making it easier for scripts. Only works with the 'run' command \
         for now."
      Peak
  in
  let bytes =
    Clap.flag
      ~set_long:"bytes"
      ~description:
        "Output memory usage in bytes instead of kilobytes, and without the \
         unit. Also, do not prefix the total with 'Total:'. Useful for scripts \
         that just want the value. Usually used without --verbose, but can be \
         used with --recursively to output the total, including children \
         memory usage."
      false
  in
  let command =
    Clap.subcommand
      [
        ( Clap.case
            "get"
            ~description:
              "Measure memory usage of a process that is already running."
        @@ fun () ->
          let watch =
            Clap.flag
              ~set_long:"watch"
              ~set_short:'w'
              ~description:"Repeat the measurement forever."
              false
          in
          let pid =
            Clap.mandatory_int
              ~placeholder:"PID"
              ~description:"PID of the process to measure."
              ()
          in
          `get (pid, watch) );
        ( Clap.case
            "run"
            ~description:
              "Run a program and measure its memory usage until it exits."
        @@ fun () ->
          let stdout_path =
            Clap.default_string
              ~long:"redirect-process-stdout-to"
              ~short:'O'
              ~placeholder:"PATH"
              ~description:
                "Redirect the standard output channel of the process to PATH."
              "/dev/null"
          in
          let stderr_path =
            Clap.default_string
              ~long:"redirect-process-stderr-to"
              ~short:'E'
              ~placeholder:"PATH"
              ~description:
                "Redirect the standard error channel of the process to PATH."
              "/dev/null"
          in
          let executable =
            Clap.mandatory_string
              ~placeholder:"EXECUTABLE"
              ~description:"Name of the executable to run."
              ()
          in
          let arguments =
            Clap.list_string
              ~placeholder:"ARGUMENTS"
              ~description:"Arguments to pass to EXECUTABLE."
              ()
          in
          `run (executable, arguments, stdout_path, stderr_path) );
      ]
  in
  Clap.close () ;

  (* Run the requested command. *)
  let result =
    match command with
    | `get (pid, watch) ->
        if watch then watch_pid ~recursively ~verbose ~frequency ~bytes pid
        else
          let* info = get_process_info ~recursively pid in
          output_process_info ~verbose ~bytes info ;
          Ok ()
    | `run (executable, arguments, stdout_path, stderr_path) ->
        run_and_watch
          ~recursively
          ~verbose
          ~frequency
          ~peak
          ~bytes
          ~stdout_path
          ~stderr_path
          executable
          arguments
  in
  match result with Ok () -> () | Error message -> echo_e "Error: %s" message
