(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let file_mapping =
  let t = Hashtbl.create 17 in
  let rec loop path =
    List.iter
      (fun name ->
        let full = Filename.concat path name in
        if Sys.is_directory full then loop full else Hashtbl.add t name full)
      (Array.to_list (Sys.readdir path))
  in
  let test_vectors = ["test_vectors"; "test/test_vectors"] in
  List.iter (fun f -> if Sys.file_exists f then loop f) test_vectors ;
  t

let open_file filename =
  let name =
    if Sys.file_exists filename then filename
    else
      try Hashtbl.find file_mapping filename
      with _ ->
        failwith
          (Printf.sprintf
             "Cannot open %S, the file doesn't exists in test_vectors"
             filename)
  in
  let ic = open_in_bin name in
  ic

let read_file filename =
  let lines = ref [] in
  let chan = open_file filename in
  try
    while true do
      lines := input_line chan :: !lines
    done ;
    !lines
  with End_of_file ->
    close_in chan ;
    List.rev !lines

let generate_random_byte () = char_of_int (Random.int 256)

let generate_random_bytes size =
  Bytes.init size (fun _ -> generate_random_byte ())

let rec repeat n f () =
  if n > 0 then (
    f () ;
    repeat (n - 1) f ())
