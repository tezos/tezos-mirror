(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

type t = V0 | V1

let pp ppf v =
  Format.pp_print_string ppf @@ match v with V0 -> "v0" | V1 -> "v1"

let encoding =
  let open Data_encoding in
  conv
    (function V0 -> 0 | V1 -> 1)
    (function
      | 0 -> V0
      | 1 -> V1
      | v -> Format.ksprintf Stdlib.failwith "Unsupported store version %d" v)
    (obj1 (req "store_version" int31))

let path ~dir = Filename.concat dir "version"

let read_version_file ~dir =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! exists = Lwt_unix.file_exists filename in
  if not exists then return_none
  else
    let* json = Lwt_utils_unix.Json.read_file filename in
    return_some (Data_encoding.Json.destruct encoding json)

let write_version_file ~dir version =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let filename = path ~dir in
  let*! () = Lwt_utils_unix.create_dir dir in
  let json = Data_encoding.Json.construct encoding version in
  Lwt_utils_unix.Json.write_file filename json
