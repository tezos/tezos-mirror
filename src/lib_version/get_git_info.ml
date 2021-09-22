(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* this is a script run at build time to get date, hash and version from git *)

module Configurator = Configurator.V1

let query ?env ~default cmd =
  let run_git () =
    try
      let chan = Unix.open_process_in cmd in
      let out = input_line chan in
      if Unix.close_process_in chan = Unix.WEXITED 0 then out else default
    with
    | End_of_file -> default
    | _ ->
        Printf.eprintf "Warning: Error while executing %s. using default" cmd ;
        default
  in
  match env with
  | None -> run_git ()
  | Some env -> ( try Sys.getenv env with Not_found -> run_git ())

let hash =
  query ~env:"GIT_SHORTREF" ~default:"unknown" "git show -s --pretty=format:%H"

let date =
  query
    ~env:"GIT_DATETIME"
    ~default:"not-available"
    "git show -s --pretty=format:%ci"

let parse_version s = Tezos_version_parser.version_tag (Lexing.from_string s)

(* find the most recent tag. If one commit is associated with two or more tags,
   output always the most recently added tag *)
let git_describe =
  let s = query ~env:"GIT_VERSION" ~default:"dev" "git describe --tags" in
  match parse_version s with
  | None -> Tezos_version_parser.default
  | Some v -> v

let lines =
  [
    Format.asprintf "let commit_hash = \"%s\"" hash;
    Format.asprintf "let committer_date = \"%s\"" date;
    Format.asprintf "let git_describe = %a" Tezos_version_parser.pp git_describe;
  ]

let () =
  Configurator.main ~name:"tezos-git-vars" (fun _conf ->
      Configurator.Flags.write_lines "generated_git_info.ml" lines)
