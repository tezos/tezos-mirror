(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* The expected output of a normal run of Tezt is the log of the tests.
   So these should be on stdout.
   However, for some commands (especially those that are expected to be parsed
   by scripts such as --list-tsv), the expected output contains no logs, only data.
   For those commands, we thus log to stderr instead. We don't expect much logs
   except maybe warnings like "leftover files from a previous run". *)
let channel =
  (* The use of a pattern with all fields ensures that we will update this if we
     add new commands that should log to stderr. *)
  let {
    Cli.color = _;
    log_level = _;
    log_file = _;
    log_buffer_size = _;
    commands = _;
    temporary_file_mode = _;
    keep_going = _;
    files_to_run = _;
    tests_to_run = _;
    tests_not_to_run = _;
    tags_to_run = _;
    tags_not_to_run = _;
    list;
    global_timeout = _;
    test_timeout = _;
    regression_dir = _;
    reset_regressions = _;
    loop_mode = _;
    time = _;
    starting_port = _;
    record = _;
    from_records = _;
    job = _;
    job_count = _;
    suggest_jobs;
    junit = _;
  } =
    Cli.options
  in
  if
    (match list with None -> false | Some (`Ascii_art | `Tsv) -> true)
    || suggest_jobs
  then stderr
  else stdout

(* In theory we could simply escape spaces, backslashes, double quotes, single quotes
   and other symbols with a meaning for the shell.
   But 'some long argument' is arguably more readable than some\ long\ argument.
   We use this quoting method if the string contains no single quote. *)
let quote_shell s =
  let contains_single_quote = ref false in
  let needs_quotes = ref false in
  let categorize = function
    | '\'' ->
        needs_quotes := true ;
        contains_single_quote := true
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '-' | '_' | '.' | '+' | '/' | ':' | '@' | '%' ->
        ()
    | _ -> needs_quotes := true
  in
  String.iter categorize s ;
  if not !needs_quotes then s
  else if not !contains_single_quote then "'" ^ s ^ "'"
  else Filename.quote s

let quote_shell_command command arguments =
  String.concat " " (List.map quote_shell (command :: arguments))

module Color = struct
  type t = string

  let ( ++ ) = ( ^ )

  let reset = "\027[0m"

  let bold = "\027[1m"

  let apply color string =
    if Cli.options.color then color ^ string ^ reset else string

  module FG = struct
    let black = "\027[30m"

    let red = "\027[31m"

    let green = "\027[32m"

    let yellow = "\027[33m"

    let blue = "\027[34m"

    let magenta = "\027[35m"

    let cyan = "\027[36m"

    let gray = "\027[37m"

    let bright_white = "\027[97m"
  end

  module BG = struct
    let black = "\027[40m"

    let red = "\027[41m"

    let green = "\027[42m"

    let yellow = "\027[43m"

    let blue = "\027[44m"

    let magenta = "\027[45m"

    let cyan = "\027[46m"

    let gray = "\027[47m"

    let bright_white = "\027[107m"
  end
end

(* The log buffer is a queue with a maximum size.
   Older items are dropped. *)
module Log_buffer = struct
  let capacity = Cli.options.log_buffer_size

  (* Each item is a tuple [(timestamp, color, prefix, prefix_color, progress, message)]. *)
  let buffer = Array.make capacity (0., None, None, None, None, "")

  (* Index where to add the next item. *)
  let next = ref 0

  (* Number of items which are actually used in the array. *)
  let used = ref 0

  let reset () =
    next := 0 ;
    used := 0

  let push line =
    if capacity > 0 then (
      if !next >= capacity then next := 0 ;
      buffer.(!next) <- line ;
      incr next ;
      used := min capacity (!used + 1))

  (* Note: don't call [push] in [f]. *)
  let iter f =
    let first = !next - !used in
    let last = !next - 1 in
    for i = first to last do
      (* Add [capacity] to avoid issues with modulo of negative integers. *)
      f buffer.((i + capacity) mod capacity)
    done
end

let output_timestamp output timestamp =
  let time = Unix.gmtime timestamp in
  output
    (Printf.sprintf
       "%02d:%02d:%02d.%03d"
       time.tm_hour
       time.tm_min
       time.tm_sec
       (int_of_float ((timestamp -. float (truncate timestamp)) *. 1000.)))

let log_line_to ~use_colors
    (timestamp, color, prefix, prefix_color, progress, message) channel =
  let output = output_string channel in
  output "[" ;
  output_timestamp output timestamp ;
  output "] " ;
  if use_colors then Option.iter output color ;
  Option.iter
    (fun prefix ->
      output "[" ;
      if use_colors then Option.iter output prefix_color ;
      output prefix ;
      (if use_colors then
       match prefix_color with
       | None -> ()
       | Some _ ->
           output Color.reset ;
           Option.iter output color) ;
      output "] ")
    prefix ;
  Option.iter output progress ;
  output message ;
  if use_colors && color <> None then output Color.reset ;
  output "\n"

let log_string ~(level : Cli.log_level) ?color ?prefix ?prefix_color
    ?progress_msg message =
  match String.split_on_char '\n' message with
  | [] | [""] -> ()
  | lines ->
      let log_line message =
        let line =
          ( Unix.gettimeofday (),
            color,
            prefix,
            prefix_color,
            progress_msg,
            message )
        in
        Option.iter (log_line_to ~use_colors:false line) Cli.options.log_file ;
        match (Cli.options.log_level, level) with
        | (_, Quiet) -> invalid_arg "Log.log_string: level cannot be Quiet"
        | (Error, Error)
        | (Warn, (Error | Warn))
        | (Report, (Error | Warn | Report))
        | (Info, (Error | Warn | Report | Info))
        | (Debug, (Error | Warn | Report | Info | Debug)) ->
            (if level = Error then
             Log_buffer.iter @@ fun line ->
             log_line_to ~use_colors:Cli.options.color line channel) ;
            Log_buffer.reset () ;
            log_line_to ~use_colors:Cli.options.color line channel ;
            flush channel
        | ((Quiet | Error | Warn | Report | Info), _) -> Log_buffer.push line
      in
      List.iter log_line lines

let log ~level ?color ?prefix fmt =
  Format.kasprintf (log_string ~level ?color ?prefix) fmt

let debug ?color = log ~level:Debug ?color

let info ?color = log ~level:Info ?color

let report ?color = log ~level:Report ?color

let warn x = log ~level:Warn ~color:Color.FG.red ~prefix:"warn" x

let error x = log ~level:Error ~color:Color.FG.red ~prefix:"error" x

type test_result = Successful | Failed of string | Aborted

let test_result ~test_index ~test_count ~failure_count ~iteration test_result
    test_name =
  let (prefix, prefix_color) =
    match test_result with
    | Successful -> ("SUCCESS", Color.(FG.green ++ bold))
    | Failed _ -> ("FAILURE", Color.(FG.red ++ bold))
    | Aborted -> ("ABORTED", Color.(FG.red ++ bold))
  in
  let message =
    if Cli.options.loop_mode <> Count 1 then
      Printf.sprintf "(loop %d) %s" iteration test_name
    else test_name
  in
  let progress_msg =
    sf
      "(%d/%d%s) "
      test_index
      test_count
      (if failure_count > 0 then sf ", %d failed" failure_count else "")
  in
  log_string ~level:Report ~prefix ~prefix_color ~progress_msg message

let command ?color ?prefix command arguments =
  let message = quote_shell_command command arguments in
  log_string ~level:Debug ?color ?prefix message ;
  if Cli.options.commands then print_endline message
