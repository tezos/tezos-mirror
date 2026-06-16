(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = Irmin_context.tree

let chunk_size = 512
(* Extracted from [Tezos_lazy_containers.Chunked_byte_vector.Chunk.size]. *)

(* This cannot be a constant because of the use of [Bytes.unsafe_of_string] in
   this module. *)
let empty_chunk () = String.make chunk_size (Char.chr 0)

let load_chunk (vector : t) i =
  let open Lwt_syntax in
  let+ chunk = Irmin_context.Tree.find vector ["contents"; Int.to_string i] in
  match chunk with
  | Some chunk ->
      (* we need to copy because this is a bytes used in Irmin cache *)
      Bytes.to_string chunk
  | None -> empty_chunk ()

let write_chunk vector i new_chunk =
  Irmin_context.Tree.add
    vector
    ["contents"; Int.to_string i]
    (Bytes.unsafe_of_string new_chunk)

let fill_buffer (vector : t) len buffer =
  let open Lwt_syntax in
  let rec aux i =
    let offset = Buffer.length buffer in
    if offset >= len then return (Buffer.contents buffer)
    else
      let to_read = min (len - offset) chunk_size in
      let* chunk = load_chunk vector i in
      Buffer.add_substring buffer chunk 0 to_read ;
      aux (i + 1)
  in
  aux 0

let length vector =
  let open Lwt_syntax in
  let* len_bytes = Irmin_context.Tree.find vector ["length"] in
  match len_bytes with
  | Some len_bytes ->
      return_ok
        (Int64.to_int Data_encoding.(Binary.of_bytes_exn int64 len_bytes))
  | None -> return (Error Error_code.store_not_a_value)

let load_all (vector : t) =
  let open Lwt_syntax in
  let* len = length vector in
  match len with
  | Ok len ->
      let buffer = Buffer.create len in
      fill_buffer vector len buffer
  | Error _ -> raise Not_found

let set_length vector new_len =
  Irmin_context.Tree.add
    vector
    ["length"]
    Data_encoding.(Binary.to_bytes_exn int64 (Int64.of_int new_len))

let empty () =
  let open Lwt_syntax in
  let* tree =
    Irmin_context.Tree.add
      (Irmin_context.Tree.empty ())
      ["length"]
      Data_encoding.(Binary.to_bytes_exn int64 0L)
  in
  return tree

let get ?(create_if_absent = false) tree key : (t, Error_code.t) result Lwt.t =
  let open Lwt_result_syntax in
  let*! v = Irmin_context.Tree.find_tree tree (key @ ["@"]) in
  match v with
  | Some v -> return v
  | None ->
      if create_if_absent then
        let*! tree = empty () in
        return tree
      else fail Error_code.store_not_a_value

let add_in_tree tree key vector =
  Irmin_context.Tree.add_tree tree (key @ ["@"]) vector

let load_bytes vector ~offset ~num_bytes =
  let open Lwt_result_syntax in
  let begin_chunk = offset / chunk_size in
  let begin_offset = offset mod chunk_size in

  let* len = length vector in
  let store_read_chunk i buffer ofs num_bytes =
    let open Lwt_syntax in
    let+ chunk = load_chunk vector i in
    Buffer.add_substring buffer chunk ofs num_bytes
  in
  let rec read_chunks next buffer ofs remaining_bytes =
    let open Lwt_syntax in
    let to_read = min remaining_bytes (chunk_size - ofs) in
    let* () = store_read_chunk next buffer ofs to_read in
    let remaining_bytes = remaining_bytes - to_read in
    if remaining_bytes > 0 then read_chunks (next + 1) buffer 0 remaining_bytes
    else return ()
  in

  if len < offset + num_bytes then fail Error_code.store_invalid_access
  else
    let buffer = Buffer.create num_bytes in
    let*! () = read_chunks begin_chunk buffer begin_offset num_bytes in
    return (Buffer.to_bytes buffer)

let write_bytes vector offset bytes =
  let open Lwt_result_syntax in
  let to_write = Bytes.length bytes in
  let begin_chunk = offset / chunk_size in
  let begin_offset = offset mod chunk_size in
  let* len = length vector in
  let store_write_chunk vector i already_written offset to_write =
    let open Lwt_syntax in
    let* chunk = load_chunk vector i in
    Bytes.blit
      bytes
      already_written
      (Bytes.unsafe_of_string chunk)
      offset
      to_write ;
    write_chunk vector i chunk
  in
  let rec write_chunks vector next ofs already_written =
    let open Lwt_syntax in
    if already_written < to_write then
      let to_write_step = min (chunk_size - ofs) (to_write - already_written) in
      let* vector =
        store_write_chunk vector next already_written ofs to_write_step
      in
      let already_written = already_written + to_write_step in
      write_chunks vector (next + 1) 0 already_written
    else return vector
  in

  if offset > len then fail Error_code.store_invalid_access
  else
    let*! vector =
      if len < offset + to_write then
        (* The vector will grow *)
        set_length vector (offset + to_write)
      else (* The vector won’t grow *) Lwt.return vector
    in
    let*! vector = write_chunks vector begin_chunk begin_offset 0 in
    return vector
