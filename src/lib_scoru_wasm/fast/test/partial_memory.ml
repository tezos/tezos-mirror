(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

module Partial_memory = Tezos_webassembly_interpreter.Partial_memory
module Chunked_byte_vector = Tezos_lazy_containers.Chunked_byte_vector

let char_to_uint8 chr = Char.code chr |> Unsigned.UInt8.of_int

let uint8_to_char ui8 = Char.chr @@ Unsigned.UInt8.to_int ui8

let of_list (content : Unsigned.uint8 list) : Partial_memory.t =
  let mem_length = List.length content in
  let page_size = Int64.to_int Partial_memory.page_size in
  let pages = mem_length / page_size in

  assert (Int.rem mem_length page_size = 0) ;

  let cbv =
    Chunked_byte_vector.of_string @@ String.of_seq @@ Seq.map uint8_to_char
    @@ List.to_seq content
  in

  Partial_memory.of_chunks
    (MemoryType {min = Int32.of_int pages; max = None})
    cbv

let to_list partial_memory : Unsigned.uint8 list Lwt.t =
  let open Lwt.Syntax in
  let+ chunks =
    Chunked_byte_vector.to_string (Partial_memory.content partial_memory)
  in
  String.to_seq chunks |> Seq.map char_to_uint8 |> List.of_seq
