(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Clic
open Lwt_result_syntax

type t = {base_dir : string; endpoint : Uri.t}

let default_base_dir =
  Filename.concat (Sys.getenv "HOME") ".tezos-sc-rollup-client"

let default_endpoint = "http://localhost:8932"

let default =
  {base_dir = default_base_dir; endpoint = Uri.of_string default_endpoint}

let valid_endpoint _configuration s =
  let endpoint = Uri.of_string s in
  match (Uri.scheme endpoint, Uri.query endpoint, Uri.fragment endpoint) with
  | (Some ("http" | "https"), [], None) -> return endpoint
  | _ -> failwith "Endpoint should be of the form http[s]://address:port"

let endpoint_arg () =
  arg
    ~long:"endpoint"
    ~short:'E'
    ~placeholder:"uri"
    ~doc:
      (Printf.sprintf
         "endpoint of the sc rollup node; e.g. '%s'"
         default_endpoint)
  @@ parameter valid_endpoint

let valid_base_dir _configuration base_dir =
  if not (Sys.file_exists base_dir && Sys.is_directory base_dir) then
    failwith "%s does not seem to be an existing directory" base_dir
  else return base_dir

let base_dir_arg () =
  arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:
      (Format.asprintf
         "@[<v>@[<2>Tezos smart-contract rollup client data directory@,\
          The directory where the Tezos smart-contract rollup client stores \
          its data.@,\
          If absent, its value defaults to %s@]@]@."
         default_base_dir)
    (parameter valid_base_dir)

let global_options () = Clic.args2 (base_dir_arg ()) (endpoint_arg ())

let make (base_dir, endpoint) =
  {
    base_dir = Option.value base_dir ~default:default_base_dir;
    endpoint = Option.value endpoint ~default:(Uri.of_string default_endpoint);
  }

let parse argv =
  let* (opts, argv) =
    Clic.parse_global_options (global_options ()) default argv
  in
  return (make opts, argv)
