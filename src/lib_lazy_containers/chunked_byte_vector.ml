(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech  <contact@trili.tech>                        *)
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

open Bigarray

exception Bounds

exception SizeOverflow

let reraise = function
  | Lazy_vector.Bounds -> raise Bounds
  | Lazy_vector.SizeOverflow -> raise SizeOverflow
  | exn -> raise exn

module Array1_64 = struct
  let create kind layout n =
    if n < 0L || n > Int64.of_int max_int then
      raise (Invalid_argument "Bigarray.Array1_64.create") ;
    Array1.create kind layout (Int64.to_int n)

  let index_of_int64 i =
    if i < 0L || i > Int64.of_int max_int then -1 else Int64.to_int i

  let get a i = Array1.get a (index_of_int64 i)

  let set a i x = Array1.set a (index_of_int64 i) x
end

module Chunk = struct
  type t = (int, int8_unsigned_elt, c_layout) Array1.t

  (** Number of bits in an address for the chunk offset *)
  let offset_bits = 9

  (** Size of a chunk in bytes - with 9 bits of address space the
      chunk is 512 bytes *)
  let size = Int64.shift_left 1L offset_bits

  (** Get the chunk index for an address. *)
  let index address = Int64.shift_right address offset_bits

  (** Get the offset within its chunk for a given address. *)
  let offset address = Int64.(logand address (sub size 1L))

  (** Get the address from a page index and an offset. *)
  let address ~index ~offset = Int64.(add (shift_left index offset_bits) offset)

  let alloc () =
    let chunk = Array1_64.create Int8_unsigned C_layout size in
    Array1.fill chunk 0 ;
    chunk

  let of_bytes bytes =
    let chunk = alloc () in
    for i = 0 to Int.max (Int64.to_int size) (Bytes.length bytes) - 1 do
      Array1.set chunk i (Char.code (Bytes.get bytes i))
    done ;
    chunk

  let to_bytes chunk =
    let len = Array1.size_in_bytes chunk in
    Bytes.init len (fun i -> Char.chr @@ Array1.get chunk i)

  let num_needed length =
    if Int64.compare length 0L > 0 then
      (* [pred length] is used to cover the edge cases where [length] is an exact
          multiple of [Chunk.size]. For example [div Chunk.size Chunk.size] is 1
          but would allocate 2 chunks without a [pred] applied to the first
          argument. *)
      Int64.(div (pred length) size |> succ)
    else 0L

  let encoding =
    let open Tezos_tree_encoding in
    conv of_bytes to_bytes (raw [])
end

module Vector = Lazy_vector.Mutable.Int64Vector

type t = {mutable length : int64; chunks : Chunk.t Vector.t}

let def_get_chunk _ = Lwt.return (Chunk.alloc ())

let create ?origin ?get_chunk length =
  let chunks =
    Vector.create ?origin ?produce_value:get_chunk (Chunk.num_needed length)
  in
  {length; chunks}

let origin vector = Vector.origin vector.chunks

let grow vector size_delta =
  if 0L < size_delta then (
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
      Vector.grow chunk_count_delta vector.chunks ;
    vector.length <- new_size)

let allocate length =
  let res = create 0L in
  grow res length ;
  res

let length vector = vector.length

let get_chunk index {chunks; _} =
  Lwt.catch
    (fun () -> Vector.get index chunks)
    (function
      | Lazy_vector.Bounds as exn -> reraise exn | _ -> def_get_chunk ())

let set_chunk index chunk {chunks; _} =
  try Vector.set index chunk chunks with exn -> reraise exn

let load_byte vector address =
  let open Lwt.Syntax in
  if Int64.compare address vector.length >= 0 then raise Bounds ;
  let+ chunk = get_chunk (Chunk.index address) vector in
  Array1_64.get chunk (Chunk.offset address)

let load_bytes vector start length =
  let open Lwt.Syntax in
  let end_offset = Int64.pred @@ Int64.add start length in
  (* Ensure [offset] and [end_offset] are valid indeces in the vector.

     Once we ensure the vector can be contained in a string, we can safely
     convert everything to int, since the size of the vector is contained in
     a `nativeint`. See {!of_string} comment. *)
  if
    start < 0L || length < 0L || end_offset > vector.length
    || vector.length > Int64.of_int Sys.max_string_length
  then raise Bounds ;

  if length = 0L then Lwt.return Bytes.empty
  else
    let buffer = Bytes.create @@ Int64.to_int length in

    let rec copy chunk offset length dest_offset =
      if length > 0L then (
        Array1.get chunk offset |> Char.chr |> Bytes.set buffer dest_offset ;
        (copy [@tailcall])
          chunk
          (Int.succ offset)
          (Int64.pred length)
          (Int.succ dest_offset))
      else ()
    in

    let rec go offset length =
      if length > 0L then (
        let chunk_index = Chunk.index offset in
        let chunk_offset = Chunk.offset offset in
        let chunk_length = Int64.(min (sub Chunk.size chunk_offset) length) in
        let* chunk = get_chunk chunk_index vector in

        copy
          chunk
          (Int64.to_int chunk_offset)
          chunk_length
          Int64.(sub offset start |> to_int) ;

        (go [@tailcall])
          (Int64.add offset chunk_length)
          (Int64.sub length chunk_length))
      else Lwt.return_unit
    in

    let+ () = go start length in
    buffer

let store_byte vector address byte =
  let open Lwt.Syntax in
  if Int64.compare address vector.length >= 0 then raise Bounds ;
  let+ chunk = get_chunk (Chunk.index address) vector in
  Array1_64.set chunk (Chunk.offset address) byte ;
  (* This is necessary because [get_chunk] might provide a default
     value without loading any data. *)
  Vector.set (Chunk.index address) chunk vector.chunks

let store_bytes vector address bytes =
  List.init (Bytes.length bytes) (fun i ->
      let c = Bytes.get bytes i in
      store_byte vector Int64.(of_int i |> add address) (Char.code c))
  |> Lwt.join

let of_string str =
  (* Strings are limited in size and contained in `nativeint` (either int31 or
     int63 depending of the architecture). The maximum size of strings in
     OCaml is limited by {!Sys.max_string_length} which is lesser than
     `Int64.max_int` (and even Int.max_int). As such conversions from / to
     Int64 to manipulate the vector is safe since the size of the
     Chunked_byte_vector from a string can be contained in an `int`.

       Moreover, WASM strings are limited to max_uint32 in size for data
       segments, which is the primary usage of this function in the text
       parser. *)
  let len = String.length str in
  let vector = create (Int64.of_int len) in
  let _ =
    List.init
      (Vector.num_elements vector.chunks |> Int64.to_int)
      (fun index ->
        let index = Int64.of_int index in
        let chunk = Chunk.alloc () in
        let _ =
          List.init (Chunk.size |> Int64.to_int) (fun offset ->
              let offset = Int64.of_int offset in
              let address = Chunk.address ~index ~offset |> Int64.to_int in
              if address < len then
                let c = String.get str address in
                Array1_64.set chunk offset (Char.code c))
        in
        set_chunk index chunk vector)
  in
  vector

let of_bytes bytes =
  (* See [of_string] heading comment *)
  let len = Bytes.length bytes in
  let vector = create (Int64.of_int len) in
  let _ =
    List.init
      (Vector.num_elements vector.chunks |> Int64.to_int)
      (fun index ->
        let index = Int64.of_int index in
        let chunk = Chunk.alloc () in
        let _ =
          List.init (Chunk.size |> Int64.to_int) (fun offset ->
              let offset = Int64.of_int offset in
              let address = Chunk.address ~index ~offset |> Int64.to_int in
              if address < len then
                let c = Bytes.get bytes address in
                Array1_64.set chunk offset (Char.code c))
        in
        set_chunk index chunk vector)
  in
  vector

let to_bytes vector = load_bytes vector 0L vector.length

let to_string vector =
  let open Lwt.Syntax in
  let+ buffer = to_bytes vector in
  Bytes.to_string buffer

let loaded_chunks vector =
  Vector.Vector.loaded_bindings (Vector.snapshot vector.chunks)

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
