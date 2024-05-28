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

open Tezos_openapi

let main () =
  (* Parse command line arguments. *)
  let version, title, description, filename =
    if Array.length Sys.argv <> 5 then (
      prerr_endline
        "Usage: rpc_openapi <VERSION> <TITLE> <DESCRIPTION> <API.json>\n\n\
         VERSION is the version of the API, to be put in the \"version\" field \
         of the output.\n\n\
         TITLE is the string to be put in the \"title\" field of the output.\n\n\
         DESCRIPTION is the string to be put in the \"description\" field of \
         the output.\n\n\
         Multiple input files are not supported." ;
      exit (if Array.length Sys.argv = 1 then 0 else 1))
    else (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4))
  in
  (* Parse input file and convert it. *)
  Json.parse_file filename |> Api.parse_tree |> Api.parse_services
  |> Api.flatten
  |> Convert.convert_api ~title ~description version
  |> Openapi.to_json |> Json.output

let () =
  Printexc.record_backtrace true ;
  try main ()
  with exn ->
    Printexc.print_backtrace stderr ;
    prerr_endline (Printexc.to_string exn) ;
    exit 1
