(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module StringMap = Map.Make (String)
open Re

let regexp = Str.regexp "%%[^%]*%%"

let current_dir = Sys.getcwd ()

let guess_version () =
  let prefix = "proto_" in
  let rec loop dir =
    let dirname = Filename.basename dir in
    let x = String.length prefix in
    let n = String.length dirname in
    if n >= x && String.sub dirname 0 x = prefix then
      String.sub dirname x (n - x)
    else
      let updir = Filename.dirname dir in
      if updir = dir then (
        Format.eprintf
          "Cannot guess protocol version in path!@.Looking for `%s*` in `%s`@."
          prefix
          current_dir ;
        exit 1) ;
      loop updir
  in
  loop (Sys.getcwd ())

let warning_message =
  {|

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;

|}

let replace ~template ~destination vars =
  let inch = open_in template in
  let outch = open_out destination in
  output_string outch warning_message ;
  try
    while true do
      let line = input_line inch in
      let line =
        Str.global_substitute
          regexp
          (fun s ->
            let matched = Str.matched_string s in
            let var = String.sub matched 2 (String.length matched - 4) in
            match StringMap.find var vars with
            | Some value -> value
            | None ->
                prerr_endline ("Unknown variable: " ^ var) ;
                exit 1)
          line
      in
      output_string outch line ;
      output_string outch "\n"
    done
  with End_of_file ->
    flush outch ;
    close_out outch ;
    ()

let module_name (c : Protocol.component) = String.capitalize_ascii c.name

let sources_name (c : Protocol.component) =
  let name = String.uncapitalize_ascii c.name in
  match c.interface with
  | None -> Printf.sprintf "%s.ml" name
  | Some _ -> Printf.sprintf "%s.mli %s.ml" name name

let process ~template ~destination (protocol : Protocol.t) lib_version hash
    check_hash =
  let version = String.concat "-" (String.split_on_char '_' lib_version) in
  let vars =
    StringMap.empty
    |> StringMap.add "VERSION" version
    |> StringMap.add "LIB_VERSION" lib_version
    |> StringMap.add
         "ENV_VERSION"
         (Protocol.module_name_of_env_version protocol.expected_env)
    |> StringMap.add "HASH" (Protocol_hash.to_b58check hash)
    |> StringMap.add
         "COMPILE_OPTION"
         (if check_hash then "" else "-no-hash-check")
    |> StringMap.add
         "MODULES"
         (String.concat "\n   " (List.map module_name protocol.components))
    |> StringMap.add
         "SOURCES"
         (String.concat "\n   " (List.map sources_name protocol.components))
  in
  replace ~template ~destination vars

let read_proto destination final_protocol_file =
  let source_dir =
    if Filename.is_relative destination then
      Filename.concat current_dir (Filename.dirname destination)
    else Filename.dirname destination
  in
  Lwt_main.run
    (let open Lwt_syntax in
    let* final_protocol = Lwt_utils_unix.read_file final_protocol_file in
    let final_protocol =
      List.map Protocol_hash.of_b58check_exn
      @@ String.split_on_char '\n' final_protocol
    in
    let+ r = Tezos_base_unix.Protocol_files.read_dir source_dir in
    match r with
    | Ok (None, proto) -> (Protocol.hash proto, proto, false)
    | Ok (Some hash, proto) ->
        (hash, proto, List.mem ~equal:Protocol_hash.equal hash final_protocol)
    | Error err ->
        Format.kasprintf
          Stdlib.failwith
          "Failed to read TEZOS_PROTOCOL in %s:@ %a"
          source_dir
          pp_print_trace
          err)

let main () =
  let template = Sys.argv.(1) in
  let destination = Sys.argv.(2) in
  let final_protocol_file = Sys.argv.(3) in
  let version =
    try Sys.argv.(4) with Invalid_argument _ -> guess_version ()
  in
  let (hash, proto, check_hash) = read_proto destination final_protocol_file in
  process ~template ~destination proto version hash check_hash

let () = main ()
