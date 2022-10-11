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

module Reveal_hash = Protocol.Alpha_context.Sc_rollup.Reveal_hash

type error +=
  | Wrong_hash of {found : Reveal_hash.t; expected : Reveal_hash.t}
  | Could_not_open_preimage_file of String.t

let () =
  register_error_kind
    ~id:"sc_rollup.node.wrong_hash_of_reveal_preimage"
    ~title:"Hash of reveal preimage is not correct"
    ~description:"Hash of reveal preimage is not correct."
    ~pp:(fun ppf (found, expected) ->
      Format.fprintf
        ppf
        "The hash of reveal preimage is %a while a value of %a is expected"
        Reveal_hash.pp
        found
        Reveal_hash.pp
        expected)
    `Permanent
    Data_encoding.(
      obj2
        (req "found" Reveal_hash.encoding)
        (req "expected" Reveal_hash.encoding))
    (function
      | Wrong_hash {found; expected} -> Some (found, expected) | _ -> None)
    (fun (found, expected) -> Wrong_hash {found; expected}) ;
  register_error_kind
    ~id:"sc_rollup.node.could_not_open_reveal_preimage_file"
    ~title:"Could not open reveal preimage file"
    ~description:"Could not open reveal preimage file."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Could not open file containing preimage of reveal hash %s"
        hash)
    `Permanent
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Could_not_open_preimage_file filename -> Some filename | _ -> None)
    (fun filename -> Could_not_open_preimage_file filename)

type source = String of string | File of string

let file_contents filename =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! contents = Lwt_utils_unix.read_file filename in
      return contents)
    (fun _ -> tzfail @@ Could_not_open_preimage_file filename)

let save_string filename s =
  let cout = open_out filename in
  output_string cout s ;
  close_out cout

let path data_dir pvm_name hash =
  let hash = Format.asprintf "%a" Reveal_hash.pp hash in
  Filename.(concat (concat data_dir pvm_name) hash)

let ensure_dir_exists data_dir pvm_name =
  let path = Filename.concat data_dir pvm_name in
  if Sys.(file_exists path) then (
    if not (Sys.is_directory path) then
      Stdlib.failwith (path ^ " should be a directory."))
  else Sys.mkdir path 0o700

let get ~data_dir ~pvm_name ~hash =
  let open Lwt_result_syntax in
  let filename = path data_dir pvm_name hash in
  let* contents = file_contents filename in
  let*? () =
    let contents_hash = Reveal_hash.hash_string [contents] in
    error_unless
      (Reveal_hash.equal contents_hash hash)
      (Wrong_hash {found = contents_hash; expected = hash})
  in
  return contents

module Arith = struct
  let pvm_name =
    Protocol.Alpha_context.Sc_rollup.ArithPVM.Protocol_implementation.name

  type input =
    | String of {s : string; mutable pos : int; len : int}
    | Input of in_channel

  let rev_chunks_of_input input =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3853
       Can be made more efficient. *)
    let get_char () =
      match input with
      | Input cin -> ( try Some (input_char cin) with End_of_file -> None)
      | String ({s; pos; len} as sin) ->
          if pos >= len then None
          else (
            sin.pos <- pos + 1 ;
            Some s.[pos])
    in
    let buf = Buffer.create 31 in
    let tokens =
      (* let cin = open_in filename in *)
      let rec aux tokens =
        match get_char () with
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
          else
            let len =
              if Buffer.length buf > 0 then (
                Buffer.add_char buf ' ' ;
                len + 1)
              else len
            in
            Buffer.add_string buf token ;
            (chunks, size + len))
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
          let cell =
            match successor_hash with
            | None -> chunk
            | Some h -> Format.asprintf "%s hash:%a" chunk Reveal_hash.pp h
          in
          let hash = Reveal_hash.hash_string [cell] in
          aux (Some hash) ((hash, cell) :: linked_chunks) rev_chunks
    in
    aux None [] rev_chunks

  let input_of_source = function
    | File filename -> Input (open_in filename)
    | String s -> String {s; pos = 0; len = String.length s}

  let close_input = function String _ -> () | Input cin -> close_in cin

  let chunkify source =
    let input = input_of_source source in
    let rev_chunks = rev_chunks_of_input input in
    let linked_hashed_chunks = link_rev_chunks rev_chunks in
    close_input input ;
    linked_hashed_chunks

  let import data_dir source =
    ensure_dir_exists data_dir pvm_name ;
    let linked_hashed_chunks = chunkify source in
    List.iter
      (fun (hash, data) -> save_string (path data_dir pvm_name hash) data)
      linked_hashed_chunks ;
    Stdlib.List.hd linked_hashed_chunks |> fst

  let chunkify source =
    let linked_hashed_chunks = chunkify source in
    let chunks_map =
      linked_hashed_chunks |> List.to_seq
      |> Protocol.Alpha_context.Sc_rollup.Reveal_hash.Map.of_seq
    in
    (chunks_map, Stdlib.List.hd linked_hashed_chunks |> fst)
end

let import ~data_dir (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t)
    ~filename =
  match pvm_kind with
  | Example_arith -> Arith.import data_dir (File filename)
  | Wasm_2_0_0 -> Stdlib.failwith "Not supported yet"

let chunkify (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t) source =
  match pvm_kind with
  | Example_arith -> Arith.chunkify source
  | Wasm_2_0_0 -> Stdlib.failwith "Not supported yet"
