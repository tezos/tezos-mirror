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

type t = string

let mockup_subdirectory = "mockup"

let get_mockup_directory ~dirname = Filename.concat dirname mockup_subdirectory

let exists_mockup_directory ~dirname =
  let open Lwt_syntax in
  let filename = get_mockup_directory ~dirname in
  let* b = Lwt_unix.file_exists filename in
  if b then
    let* stat = Lwt_unix.stat filename in
    Lwt.return (stat.st_kind = Lwt_unix.S_DIR)
  else Lwt.return_false

let subdir_file = Filename.concat mockup_subdirectory

let dir_file ~dirname (file : t) = Filename.concat dirname file

module type File_descr = sig
  val basename : string
end

module type ACCESSOR = sig
  val get : dirname:string -> t

  val exists : dirname:string -> bool Lwt.t
end

module Make (F : File_descr) : ACCESSOR = struct
  let file = subdir_file F.basename

  let get ~dirname = dir_file ~dirname file

  let exists ~dirname = Lwt_unix.file_exists @@ get ~dirname
end

module Context = Make (struct
  let basename = "context.json"
end)

module Mempool = Make (struct
  let basename = "mempool.json"
end)

module Trashpool = Make (struct
  let basename = "trashpool.json"
end)
