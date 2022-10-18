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

let string_of_file filename =
  let cin = open_in filename in
  let s = really_input_string cin (in_channel_length cin) in
  close_in cin ;
  s

let save_string filename s =
  let cout = open_out filename in
  output_string cout s ;
  close_out cout

let path data_dir pvm_name hash =
  let hash =
    Format.asprintf "%a" Protocol.Alpha_context.Sc_rollup.Reveal_hash.pp hash
  in
  Filename.(concat (concat data_dir pvm_name) hash)

let ensure_dir_exists data_dir pvm_name =
  let path = Filename.concat data_dir pvm_name in
  if Sys.(file_exists path) then (
    if not (Sys.is_directory path) then
      Stdlib.failwith (path ^ " should be a directory."))
  else Sys.mkdir path 0o700

let get ~data_dir ~pvm_name ~hash =
  try Some (string_of_file (path data_dir pvm_name hash)) with _ -> None

module Arith = struct
  let pvm_name =
    Protocol.Alpha_context.Sc_rollup.ArithPVM.Protocol_implementation.name

  let rev_chunks_of_file filename =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3853
       Can be made more efficient. *)
    let get_char cin = try Some (input_char cin) with End_of_file -> None in
    let buf = Buffer.create 31 in
    let tokens =
      let cin = open_in filename in
      let rec aux tokens =
        match get_char cin with
        | None ->
            List.rev
            @@
            if Buffer.length buf > 0 then
              let token = Buffer.contents buf in
              token :: tokens
            else tokens
        | Some ' ' ->
            let token = Buffer.contents buf in
            Buffer.clear buf ;
            aux (token :: tokens)
        | Some c ->
            Buffer.add_char buf c ;
            aux tokens
      in
      let tokens = aux [] in
      close_in cin ;
      tokens
    in
    let limit =
      (4 * 1024) - 100 (* We reserve 100 bytes for the continuation hash. *)
    in
    Buffer.clear buf ;
    let make_chunk () =
      let chunk = Buffer.contents buf in
      Buffer.clear buf ;
      chunk
    in
    let chunks, _ =
      List.fold_left
        (fun (chunks, size) token ->
          let len = String.length token in
          if size + len > limit then (
            let chunk = make_chunk () in
            Buffer.add_string buf token ;
            (chunk :: chunks, len))
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf ' ' ;
            Buffer.add_string buf token ;
            (chunks, size + len)))
        ([], 0)
        tokens
    in
    let chunks =
      if Buffer.length buf > 0 then make_chunk () :: chunks else chunks
    in
    chunks

  let link_rev_chunks rev_chunks =
    let rec aux successor_hash linked_chunks = function
      | [] -> linked_chunks
      | chunk :: rev_chunks ->
          let open Protocol.Alpha_context.Sc_rollup in
          let cell =
            match successor_hash with
            | None -> chunk
            | Some h -> Format.asprintf "%s hash:%a" chunk Reveal_hash.pp h
          in
          let hash = Reveal_hash.hash_string [cell] in
          aux (Some hash) ((cell, hash) :: linked_chunks) rev_chunks
    in
    aux None [] rev_chunks

  let import data_dir filename =
    ensure_dir_exists data_dir pvm_name ;
    let rev_chunks = rev_chunks_of_file filename in
    let linked_hashed_chunks = link_rev_chunks rev_chunks in
    List.iter
      (fun (data, hash) -> save_string (path data_dir pvm_name hash) data)
      linked_hashed_chunks ;
    Stdlib.List.hd linked_hashed_chunks |> snd
end

let import ~data_dir ~pvm_name ~filename =
  if
    String.equal
      pvm_name
      Protocol.Alpha_context.Sc_rollup.ArithPVM.Protocol_implementation.name
  then Arith.import data_dir filename
  else Stdlib.failwith "Not supported yet"
