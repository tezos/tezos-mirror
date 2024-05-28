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

open Block_repr

(** Unix-dependent accessors for {!Block_repr}. *)

(** [read_next_block_exn fd] reads from [fd] and decode the next block
   found in the descriptor. The [fd]'s offset is moved as a side
   effect. This returns the decoded block along with the block length
   (number of bytes) of the encoded block. This function updates the
   given [fd] state and may raise Unix.error errors, see Unix.read. *)
val read_next_block_exn : Lwt_unix.file_descr -> (t * int) Lwt.t

(** Same as [read_next_block fd] but returns [None] if there was an
    error. *)
val read_next_block : Lwt_unix.file_descr -> (t * int) option Lwt.t

(** [pread_block_exn fd ~file_offset] reads from [fd] and decode the
   block at offset [file_offset] in the descriptor. This returns the
   decoded block along with the block length (number of bytes) of the
   encoded block. This function may raise Unix.error errors, see
   Unix.read. *)
val pread_block_exn : Lwt_unix.file_descr -> file_offset:int -> (t * int) Lwt.t

(** Same as [pread_block fd ~file_offset] but returns [None] if there
    was an error. *)
val pread_block :
  Lwt_unix.file_descr -> file_offset:int -> (t * int) option Lwt.t

(**/**)

(** Unsafe set of functions intended for internal data manipulation.
    Must not be used outside of the [Tezos_store]. *)

(** [prune_raw_block_bytes block_buffer] prunes the block contained in
    [block_buffer] and returns [metadata_offset] such that
    [decode Block_repr.encoding (Bytes.sub block_bytes 0 metadata_offset)]
    is consistent and returns a [Block_repr.t] where [metadata = None]. *)
val prune_raw_block_bytes : bytes -> int

(** [raw_get_block_hash block_buffer] introspects the hash of the
    block contained in [block_buffer]. *)
val raw_get_block_hash : bytes -> Block_hash.t

(** [raw_get_block_level block_buffer] introspects the level of the
    block contained in [block_buffer]. *)
val raw_get_block_level : bytes -> int32

(** [raw_get_block_predecessor block_buffer] introspects the
    predecessor's hash of the block contained in [block_buffer]. *)
val raw_get_block_predecessor : bytes -> Block_hash.t

(** [raw_get_last_preserved_block_level block_buffer] introspects the
    last preserved block level of the block's metadata contained in
    [block_buffer] if there is any. Returns [None] if no metadata is
    present. *)
val raw_get_last_preserved_block_level : bytes -> int -> int32 option

(** [raw_get_context block_buffer] introspects the
    context of the block contained in
    [block_buffer]. *)
val raw_get_context : bytes -> Context_hash.t
