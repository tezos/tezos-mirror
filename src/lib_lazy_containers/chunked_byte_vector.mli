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

exception Bounds

exception SizeOverflow

module Chunk : sig
  (** Chunk within the byte vector *)
  type t

  (** Create a chunk and copy the given bytes into it. *)
  val of_bytes : bytes -> t

  (** Copy the contents of a chunk into a fresh [bytes]. *)
  val to_bytes : t -> bytes

  (** Size of a chunk in bytes - with 9 bits of address space the chunk is 512B *)
  val size : int64

  (** [num_needed len] Computes the number of chunks needed to cover [len]. *)
  val num_needed : int64 -> int64

  (** [encoding] is a [Tezos_tree_encoding] for [Chunk]. *)
  val encoding : t Tezos_tree_encoding.t
end

(** Chunked byte vector *)
type t

(** [create length] creates a chunked byte vector that has capacity
    for [length] bytes.

    {b Note:} This function is expected to be use only by the
    tree-encoding library. To create a brand new chunked byte vector,
    use {!allocate}. *)
val create :
  ?origin:Tezos_tree_encoding.wrapped_tree ->
  ?get_chunk:(int64 -> Chunk.t Lwt.t) ->
  int64 ->
  t

(** [origin vec] returns the tree of origin of the vector, if it exists.

    {b Note:} The sole consumer of this function is expected to be the
    tree-encoding library. *)
val origin : t -> Tezos_tree_encoding.wrapped_tree option

(** [allocate len] creates a new zeroed chunked byte vector.

    {b Note:} This function may be dangerous to use in a tick if [len]
    is too large. *)
val allocate : int64 -> t

(** [of_string str] creates a chunked byte vector from the given [str]. *)
val of_string : string -> t

(** [of_bytes bytes] creates a chunked byte vector from the given
    [bytes]. The underlying memory is effectively copied - further
    modifications to [bytes] are not reflected in the chunked byte
    vector. Use this over [of_string] when turning your [bytes] into a
    [string] would be potentially expensive. *)
val of_bytes : bytes -> t

(** [to_string vector] creates a string from the given [vector]. *)
val to_string : t -> string Lwt.t

(** [to_bytes vector] creates a bytes from the given [vector]. *)
val to_bytes : t -> bytes Lwt.t

(** [grow vector length_delta] increases the byte vector length by
    [length_delta] and initializes the memory with empty chunks.

    {b Note:} This function may be dangerous to use in a tick if
    [length_delta] is too large. *)
val grow : t -> int64 -> unit

(** [length vector] returns the length of [vector] in bytes. *)
val length : t -> int64

(** [load_byte vector offset] read the byte at [offset]. *)
val load_byte : t -> int64 -> int Lwt.t

(** [load_bytes vector offset num_bytes] loads the bytes at [offset]
    to [offset + num_bytes].

    {b Note:} This function may be dangerous to use in a tick if
    [num_bytes] is too large. *)
val load_bytes : t -> int64 -> int64 -> bytes Lwt.t

(** [store_byte vector offset byte] set the byte at [offset] to [byte]. *)
val store_byte : t -> int64 -> int -> unit Lwt.t

(** [store_bytes vector offset bytes] set the bytes from [offset] to
    the given [bytes]. *)
val store_bytes : t -> int64 -> bytes -> unit Lwt.t

(** [loaded_chunks vector] returns the chunks of [vector] that have
    been cached in-memory since [vector] has been created, either by
    reading its contents, or by modifying it. *)
val loaded_chunks : t -> (int64 * Chunk.t option) list

(** [encoding] is a [Tezos_tree_encoding] for [t]. *)
val encoding : t Tezos_tree_encoding.t
