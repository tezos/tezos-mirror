(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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

module String = StdLabels.String
module Bytes = StdLabels.Bytes

exception Bounds

exception SizeOverflow

let reraise = function
  | Lazy_vector.Bounds -> raise Bounds
  | Lazy_vector.SizeOverflow -> raise SizeOverflow
  | exn -> raise exn

module Chunk = struct
  type t = string

  (** Number of bits in an address for the chunk offset *)
  let offset_bits = 9

  (** Size of a chunk in bytes - with 9 bits of address space the
      chunk is 512 bytes *)
  let size = Int64.shift_left 1L offset_bits

  (** The same size but of type int, for internal usage. *)
  let size_int : int = Int.shift_left 1 offset_bits

  (** Get the chunk index for an address. *)
  let index address = Int64.shift_right address offset_bits

  (** Get the offset within its chunk for a given address. *)
  let offset address = Int64.(logand address (sub size 1L))

  (** Get the address from a page index and an offset. *)
  let address ~index ~offset = Int64.(add (shift_left index offset_bits) offset)

  let alloc () = String.make size_int (Char.chr 0)

  (* Replace [dst] chunk's substring [[dst_pos]..[dst_pos] + [len] - 1]
     with substring of [src] bytes [[src_pos]..[src_pos] + [len] - 1].
     Creates new chunk, leaving original [dst] unchanged. *)
  let blit ~(src : bytes) ~(src_pos : int) ~(dst : t) ~(dst_pos : int64)
      ~(len : int) : t =
    let bytes_len = Bytes.length src in
    if
      len <= 0
      || src_pos + len > bytes_len
      || Int64.add dst_pos (Int64.of_int len) > size
    then raise Bounds
    else if len = size_int then
      (* The whole chunk should be replaced, so we just recreate a new one *)
      Bytes.unsafe_to_string @@ Bytes.sub src ~pos:src_pos ~len
    else
      (* Copy original chunk and replace corresponding substring *)
      let new_chunk_bytes = String.to_bytes dst in
      Bytes.blit
        ~src
        ~src_pos
        ~dst:new_chunk_bytes
        ~dst_pos:(Int64.to_int dst_pos)
        ~len ;
      Bytes.unsafe_to_string new_chunk_bytes

  let of_bytes_sub bytes ~(pos : int) ~(len : int) =
    if len > size_int then raise Bounds
    else if len = size_int then
      (* Just create the whole chunk *)
      let copied_sub = Bytes.sub bytes ~pos ~len in
      Bytes.unsafe_to_string copied_sub
    else blit ~src:bytes ~src_pos:pos ~dst:(alloc ()) ~dst_pos:0L ~len

  let of_bytes bytes =
    of_bytes_sub bytes ~pos:0 ~len:(Int.min (Bytes.length bytes) size_int)

  let to_bytes = String.to_bytes

  let num_needed length =
    if Int64.compare length 0L > 0 then
      (* [pred length] is used to cover the edge cases where [length] is an exact
          multiple of [Chunk.size]. For example [div Chunk.size Chunk.size] is 1
          but would allocate 2 chunks without a [pred] applied to the first
          argument. *)
      Int64.(div (pred length) size |> succ)
    else 0L

  (* Return left and right addresses of a chunk *)
  let inclusive_boundaries chunk_id =
    ( address ~index:chunk_id ~offset:0L,
      address ~index:chunk_id ~offset:(Int64.sub size 1L) )

  (* Sets char at position [offset] to [chr] and returns new chunk,
     leaving original one unchanged *)
  let set_char (s : t) (offset : int64) (chr : char) =
    let copied_bytes = String.to_bytes s in
    Bytes.set copied_bytes (Int64.to_int offset) chr ;
    Bytes.unsafe_to_string copied_bytes

  let encoding =
    let open Tezos_tree_encoding in
    conv of_bytes to_bytes (raw [])
end

module Vector = Lazy_vector.Int64Vector

type t = {length : int64; chunks : Chunk.t Vector.t}

let def_get_chunk _ = Lwt.return (Chunk.alloc ())

let set_chunk (vec : t) (chunk_id : int64) (chunk : Chunk.t) =
  let new_chunks = Vector.set chunk_id chunk vec.chunks in
  {vec with chunks = new_chunks}

let create ?origin ?get_chunk length =
  let chunks =
    Vector.create ?origin ?produce_value:get_chunk (Chunk.num_needed length)
  in
  {length; chunks}

let origin vector = Vector.origin vector.chunks

let grow vector size_delta =
  if size_delta > 0L then
    let new_size = Int64.add vector.length size_delta in
    let new_chunks = Chunk.num_needed new_size in
    let current_chunks = Vector.num_elements vector.chunks in
    let chunk_count_delta = Int64.sub new_chunks current_chunks in
    if Int64.compare chunk_count_delta 0L > 0 then
      (* We cannot make any assumption on the previous value of
         [produce_value]. In particular, it may very well raise an
         error in case of absent value (which is the case when
         growing the chunked byte vector requires to allocate new
         chunks). *)
      {length = new_size; chunks = Vector.grow chunk_count_delta vector.chunks}
    else {vector with length = new_size}
  else vector

let allocate length = grow (create 0L) length

let of_bytes bytes =
  let length = Int64.of_int (Bytes.length bytes) in
  let rec set_chunks (chunk_id : int64) vec =
    let chunk_left, _ = Chunk.inclusive_boundaries chunk_id in
    if chunk_left >= length then vec
    else
      let suffix_len = Int64.to_int @@ Int64.sub length chunk_left in
      let len = Int.min Chunk.size_int suffix_len in
      let chunk =
        Chunk.of_bytes_sub bytes ~pos:(Int64.to_int chunk_left) ~len
      in
      let new_vec = set_chunk vec chunk_id chunk in
      (set_chunks [@tailcall]) (Int64.add chunk_id 1L) new_vec
  in
  set_chunks 0L (allocate length)

let of_string str = of_bytes @@ Bytes.unsafe_of_string str

let length vector = vector.length

let get_chunk index {chunks; _} =
  Lwt.catch
    (fun () -> Vector.get index chunks)
    (function
      | Lazy_vector.Bounds as exn -> reraise exn | _ -> def_get_chunk ())

let load_byte vector address =
  let open Lwt.Syntax in
  if address >= vector.length || address < 0L then raise Bounds ;
  let+ chunk = get_chunk (Chunk.index address) vector in
  let offset = Int64.to_int @@ Chunk.offset address in
  Char.code @@ String.get chunk offset

let load_bytes vector offset length =
  let open Lwt.Syntax in
  let end_offset = Int64.pred @@ Int64.add offset length in
  (* Ensure [offset] and [end_offset] are valid indeces in the vector.

     Once we ensure the vector can be contained in a string, we can safely
     convert everything to int, since the size of the vector is contained in
     a `nativeint`. See {!of_string} comment. *)
  if
    offset < 0L || length < 0L
    || end_offset >= vector.length
    || vector.length > Int64.of_int Sys.max_string_length
  then raise Bounds ;
  let accum = Bytes.make (Int64.to_int length) (Char.chr 0) in
  let rec copy_chunks (vector_pos : int64) =
    if vector_pos > end_offset then Lwt.return_unit
    else
      let chunk_id = Chunk.index vector_pos in
      let left, right = Chunk.inclusive_boundaries chunk_id in
      let* chunk = get_chunk chunk_id vector in
      let l_chunk_offset, r_chunk_offset =
        (* Chunk fully lies in the the suffix of target bytes *)
        if left == vector_pos && right <= end_offset then (0, Chunk.size_int - 1)
        else
          ( Int64.to_int @@ Chunk.offset @@ Int64.max left vector_pos,
            Int64.to_int @@ Chunk.offset @@ Int64.min right end_offset )
      in
      let sub_chunk_len = r_chunk_offset - l_chunk_offset + 1 in
      let accum_pos = Int64.to_int @@ Int64.sub vector_pos offset in
      String.blit
        ~src:chunk
        ~src_pos:l_chunk_offset
        ~dst:accum
        ~dst_pos:accum_pos
        ~len:sub_chunk_len ;
      (copy_chunks [@tailcall])
        (Int64.add vector_pos @@ Int64.of_int sub_chunk_len)
  in
  let+ () = copy_chunks offset in
  accum

let store_byte vector address byte =
  let open Lwt.Syntax in
  if address >= vector.length || address < 0L then raise Bounds ;
  let+ chunk = get_chunk (Chunk.index address) vector in
  let offset = Chunk.offset address in
  let new_chunk = Chunk.set_char chunk offset (Char.chr byte) in
  set_chunk vector (Chunk.index address) new_chunk

let store_bytes vector offset bytes =
  let open Lwt.Syntax in
  let length = Int64.of_int @@ Bytes.length bytes in
  let end_offset = Int64.pred @@ Int64.add offset length in
  if
    offset < 0L
    || end_offset >= vector.length
    || vector.length > Int64.of_int Sys.max_string_length
  then raise Bounds ;

  let rec set_chunks (bytes_pos : int) vec =
    if Int64.of_int bytes_pos >= length then Lwt.return vec
    else
      let offseted_pos = Int64.add offset (Int64.of_int bytes_pos) in
      let chunk_id = Chunk.index offseted_pos in
      let _, chunk_right = Chunk.inclusive_boundaries chunk_id in
      let* chunk = get_chunk chunk_id vec in
      (* [l_range; r_range] in the chunk which has to be rewritten *)
      let l_range = Chunk.offset offseted_pos in
      let r_range =
        if end_offset >= chunk_right then
          (* If vector's right offset exceeds chunk's right offset in the vector,
             the we just replace a suffix of the chunk *)
          Chunk.offset chunk_right
        else
          (* Otherwise we replace a substring of the chunk *)
          Chunk.offset end_offset
      in
      let len = Int64.succ (Int64.sub r_range l_range) |> Int64.to_int in
      let new_chunk =
        Chunk.blit
          ~src:bytes
          ~src_pos:bytes_pos
          ~dst:chunk
          ~dst_pos:l_range
          ~len
      in
      let new_vec = set_chunk vec chunk_id new_chunk in
      (set_chunks [@tailcall]) (bytes_pos + len) new_vec
  in
  set_chunks 0 vector

let to_bytes vector = load_bytes vector 0L vector.length

let to_string vector =
  let open Lwt.Syntax in
  let+ buffer = to_bytes vector in
  Bytes.to_string buffer

let loaded_chunks vector = Vector.loaded_bindings vector.chunks

module Enc_intf = struct
  type nonrec t = t

  type chunk = Chunk.t

  let origin = origin

  let loaded_chunks = loaded_chunks

  let length = length

  let create = create
end

module Encoding = Tezos_tree_encoding.CBV_encoding.Make (Enc_intf)

let encoding = Encoding.cbv Chunk.encoding
