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

module ICBV = Immutable_chunked_byte_vector

exception Bounds = ICBV.Bounds

exception SizeOverflow = ICBV.SizeOverflow

module Chunk = Immutable_chunked_byte_vector.Chunk

type t = ICBV.t ref

let create ?origin ?get_chunk length =
  ref @@ ICBV.create ?origin ?get_chunk length

let origin cbv = ICBV.origin !cbv

let grow vector size_delta = vector := ICBV.grow !vector size_delta

let allocate length = ref @@ ICBV.allocate length

let length vector = ICBV.length !vector

let load_byte vector address = ICBV.load_byte !vector address

let load_bytes vector start length = ICBV.load_bytes !vector start length

let store_byte vector address byte =
  Lwt.map (fun res -> vector := res) @@ ICBV.store_byte !vector address byte

let store_bytes vector address bytes =
  Lwt.map (fun res -> vector := res) @@ ICBV.store_bytes !vector address bytes

let of_string str = ref @@ ICBV.of_string str

let of_bytes bytes = ref @@ ICBV.of_bytes bytes

let to_bytes vector = ICBV.to_bytes !vector

let to_string vector = ICBV.to_string !vector

let loaded_chunks vector = ICBV.loaded_chunks !vector

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
