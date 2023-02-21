(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

exception Incorrect_log_rules_syntax

exception Incorrect_log_rules_not_a_level

exception Incorrect_log_rules_missing_level

exception Incorrect_log_rules_missing_pattern

type rules = (string * Internal_event.Level.t) list

let find_log_rules default =
  match Sys.(getenv_opt "TEZOS_LOG", getenv_opt "LWT_LOG") with
  | Some rules, None -> ("environment variable TEZOS_LOG", Some rules)
  | None, Some rules -> ("environment variable LWT_LOG", Some rules)
  | None, None -> ("configuration file", default)
  | Some rules, Some _ ->
      Format.eprintf
        "@[<v 2>@{<warning>@{<title>Warning@}@} Both environment variables \
         TEZOS_LOG and LWT_LOG defined, using TEZOS_LOG.@]@\n\
         @." ;
      ("environment variable TEZOS_LOG", Some rules)

let parse_rules s =
  let rules = String.split_on_char ';' s in
  let arrow_regexp = Re.(compile (Perl.re "->")) in
  List.filter_map
    (fun rule ->
      if String.trim rule = "" then None
      else
        let pattern, level =
          match
            Re.(split_full arrow_regexp) rule
            |> List.map (function
                   | `Delim _ as s -> s
                   | `Text t -> `Text (String.trim t))
          with
          | [`Delim _; `Text _] | [`Text ""; `Delim _; `Text _] ->
              raise Incorrect_log_rules_missing_pattern
          | [`Text _; `Delim _] | [`Text _; `Delim _; `Text ""] ->
              raise Incorrect_log_rules_missing_level
          | [`Text pattern; `Delim _; `Text level] -> (pattern, level)
          | [`Text level] -> ("", level)
          | _ -> raise Incorrect_log_rules_syntax
        in
        let level =
          match Internal_event.Level.of_string level with
          | None -> raise Incorrect_log_rules_not_a_level
          | Some lvl -> lvl
        in
        Some (pattern, level))
    rules
