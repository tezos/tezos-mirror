(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type t = {
  protocol_store_dir : [`Protocol_dir] Naming.directory;
  mutable protocols : Protocol_hash.Set.t;
}

let mem {protocols; _} protocol_hash =
  Protocol_hash.Set.mem protocol_hash protocols

let all {protocols; _} = protocols

let raw_store store protocol_hash bytes =
  let open Lwt_syntax in
  if mem store protocol_hash then Lwt.return_none
  else
    let protocol_file =
      Naming.protocol_file store.protocol_store_dir protocol_hash
    in
    let* fd =
      Lwt_unix.openfile
        (Naming.file_path protocol_file)
        [Unix.O_CREAT; O_WRONLY; O_CLOEXEC]
        0o644
    in
    let* () = Lwt_utils_unix.write_bytes fd bytes in
    let* _ = Lwt_utils_unix.safe_close fd in
    store.protocols <- Protocol_hash.Set.add protocol_hash store.protocols ;
    Lwt.return_some protocol_hash

let store store protocol_hash protocol =
  raw_store store protocol_hash (Protocol.to_bytes protocol)

let read store protocol_hash =
  let open Lwt_syntax in
  Option.catch_os (fun () ->
      let protocol_file =
        Naming.protocol_file store.protocol_store_dir protocol_hash
      in
      let* content =
        Lwt_utils_unix.read_file (Naming.file_path protocol_file)
      in
      Lwt.return (Protocol.of_string content))

let init store_dir =
  let open Lwt_syntax in
  let protocol_store_dir = Naming.protocol_store_dir store_dir in
  let protocol_store_dir_path = Naming.dir_path protocol_store_dir in
  let* file_exists = Lwt_unix.file_exists protocol_store_dir_path in
  let* () =
    if not file_exists then Lwt_utils_unix.create_dir protocol_store_dir_path
    else Lwt.return_unit
  in
  let* dir = Lwt_unix.opendir protocol_store_dir_path in
  let rec loop set =
    Lwt.catch
      (fun () ->
        let* file = Lwt_unix.readdir dir in
        match Protocol_hash.of_b58check_opt file with
        | Some protocol_hash -> loop (Protocol_hash.Set.add protocol_hash set)
        | None -> loop set)
      (function End_of_file -> Lwt.return set | _ -> loop set)
  in
  let* protocols = loop Protocol_hash.Set.empty in
  let* () = Lwt_unix.closedir dir in
  Lwt.return {protocol_store_dir; protocols}
