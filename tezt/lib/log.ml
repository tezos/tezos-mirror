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

(* In theory we could simply escape spaces, backslashes, double quotes and single quotes.
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
    | '-'
    | '_'
    | '.'
    | '+'
    | '/'
    | ':'
    | '@'
    | '%' ->
        ()
    | _ ->
        needs_quotes := true
  in
  String.iter categorize s ;
  if not !needs_quotes then s
  else if not !contains_single_quote then "'" ^ s ^ "'"
  else
    let buffer = Buffer.create (String.length s * 2) in
    let add_char = function
      | (' ' | '\\' | '"' | '\'') as c ->
          Buffer.add_char buffer '\\' ;
          Buffer.add_char buffer c
      | c ->
          Buffer.add_char buffer c
    in
    String.iter add_char s ; Buffer.contents buffer

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

    let white = "\027[37m"
  end

  module BG = struct
    let black = "\027[40m"

    let red = "\027[41m"

    let green = "\027[42m"

    let yellow = "\027[43m"

    let blue = "\027[44m"

    let magenta = "\027[45m"

    let cyan = "\027[46m"

    let white = "\027[47m"
  end
end

let output_time output =
  let now = Unix.gettimeofday () in
  let time = Unix.gmtime now in
  output
    (Printf.sprintf
       "%02d:%02d:%02d.%03d"
       time.tm_hour
       time.tm_min
       time.tm_sec
       (int_of_float ((now -. float (truncate now)) *. 1000.)))

let log_string ~(level : Cli.log_level) ?color ?prefix message =
  match String.split_on_char '\n' message with
  | [] | [""] ->
      ()
  | lines ->
      let log_line message =
        match (Cli.options.log_level, level) with
        | (_, Quiet) ->
            invalid_arg "Log.log_string: level cannot be Quiet"
        | (Error, Error)
        | (Warn, (Error | Warn))
        | (Report, (Error | Warn | Report))
        | (Info, (Error | Warn | Report | Info))
        | (Debug, (Error | Warn | Report | Info | Debug)) ->
            let output = print_string in
            output "[" ;
            output_time output ;
            output "] " ;
            if Cli.options.color then Option.iter output color ;
            Option.iter
              (fun prefix -> output "[" ; output prefix ; output "] ")
              prefix ;
            output message ;
            if Cli.options.color && color <> None then output Color.reset ;
            output "\n" ;
            flush stdout
        | ((Quiet | Error | Warn | Report | Info), _) ->
            ()
      in
      List.iter log_line lines

let log ~level ?color ?prefix =
  Printf.ksprintf (log_string ~level ?color ?prefix)

let debug ?color = log ~level:Debug ?color

let info ?color = log ~level:Info ?color

let report ?color = log ~level:Report ?color

let warn x = log ~level:Warn ~color:Color.FG.red ~prefix:"warn" x

let error x = log ~level:Error ~color:Color.FG.red ~prefix:"error" x

type test_result = Successful | Failed | Aborted

let test_result test_result test_name =
  let (prefix, color) =
    match test_result with
    | Successful ->
        ("SUCCESS", Color.(FG.green ++ bold))
    | Failed ->
        ("FAILURE", Color.(FG.red ++ bold))
    | Aborted ->
        ("ABORTED", Color.(FG.red ++ bold))
  in
  let message =
    if Cli.options.color then
      Printf.sprintf "[%s%s%s] %s" color prefix Color.reset test_name
    else Printf.sprintf "[%s] %s" prefix test_name
  in
  log_string ~level:Report message

let command ?color ?prefix command arguments =
  let message =
    String.concat " " (List.map quote_shell (command :: arguments))
  in
  log_string ~level:Debug ?color ?prefix message ;
  if Cli.options.commands then print_endline message
