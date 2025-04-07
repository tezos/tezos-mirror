(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module ANSI = struct
  let from_code = Format.sprintf "\027[%sm"

  let mark_open_stag = function
    | Format.String_tag "yellow" -> from_code "33"
    | Format.String_tag "red" -> from_code "31"
    | Format.String_tag "blue" -> from_code "34"
    | Format.String_tag "green" -> from_code "32"
    | Format.String_tag "cyan" -> from_code "36"
    | Format.String_tag "magenta" -> from_code "35"
    | Format.String_tag "blinking" -> from_code "5"
    | Format.String_tag s ->
        failwith (Printf.sprintf "%s is not an accepted semantic tag" s)
    | _ -> failwith (Printf.sprintf "Unknown semantic tag")

  let mark_close_stag = function
    | Format.String_tag "yellow"
    | Format.String_tag "red"
    | Format.String_tag "blue"
    | Format.String_tag "green"
    | Format.String_tag "cyan"
    | Format.String_tag "magenta"
    | Format.String_tag "blinking" ->
        from_code "0"
    | Format.String_tag s ->
        failwith (Printf.sprintf "%s is not an accepted semantic tag" s)
    | _ -> failwith (Printf.sprintf "Unknown semantic tag")

  let set_mark_stag formatter =
    Format.pp_set_mark_tags formatter true ;
    let fs = Format.pp_get_formatter_stag_functions formatter () in
    Format.pp_set_formatter_stag_functions
      formatter
      {fs with mark_open_stag; mark_close_stag}

  let enable () =
    if Unix.isatty Unix.stdout then set_mark_stag Format.std_formatter ;
    if Unix.isatty Unix.stderr then set_mark_stag Format.err_formatter
end

let printf =
  ANSI.enable () ;
  Format.printf

let eprintf =
  ANSI.enable () ;
  Format.eprintf

let fprintfln formatter fmt =
  ANSI.enable () ;
  Format.kfprintf (fun fmt -> Format.pp_print_newline fmt ()) formatter fmt

let printfln fmt = fprintfln Format.std_formatter fmt

let eprintfln fmt = fprintfln Format.err_formatter fmt

let error ?name fmt =
  match name with
  | Some name ->
      Format.kasprintf
        (fun msg ->
          eprintfln "@{<red>Error: @}@{<yellow>%s@} @{<red>%s@}" name msg)
        fmt
  | None -> Format.kasprintf (fun msg -> eprintfln "@{<red>Error: %s@}" msg) fmt

let warning fmt =
  Format.kasprintf (fun msg -> eprintfln "@{<yellow>Warning: %s@}" msg) fmt

let blue fmt = Format.kasprintf (fun msg -> printfln "@{<blue>%s@}" msg) fmt

let green fmt = Format.kasprintf (fun msg -> printfln "@{<green>%s@}" msg) fmt

let cyan fmt = Format.kasprintf (fun msg -> printfln "@{<cyan>%s@}" msg) fmt

let yellow fmt = Format.kasprintf (fun msg -> printfln "@{<yellow>%s@}" msg) fmt

let magenta fmt =
  Format.kasprintf (fun msg -> printfln "@{<magenta>%s@}" msg) fmt
