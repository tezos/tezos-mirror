(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Tezos_webassembly_interpreter

(** A key in the tree is a list of string. *)
type key = string trace

module type S = sig
  (** {2 Types}*)

  (** The underlying tree type. *)
  type tree

  (** The map structure used. *)
  type 'a map

  (** The type of vector keys. *)
  type vector_key

  (** The vector structure used. *)
  type 'a vector

  (** The chunked byte vector structure used. *)
  type chunked_byte_vector

  (** Represents a partial encoder for a specific constructor of a sum-type. *)
  type ('tag, 'a) case

  (** Represents an encoder and a decoder. *)
  type 'a t

  (** A decoding module with the same tree type. *)
  module Decoding : Tree_decoding.S with type tree = tree

  (** An encoding module with the same tree type. *)
  module Encoding : Tree_encoding.S with type tree = tree

  (** {2 Functions }*)

  (** [encode enc x tree] encodes a value x using the encoder [enc] into the
      provided [tree]. *)
  val encode : 'a t -> 'a -> tree -> tree Lwt.t

  (** [decode enc x tree] decodes a value using the encoder [enc] from the
      provided [tree]. *)
  val decode : 'a t -> tree -> 'a Lwt.t

  (** [custom enc dec] creates a custom encoder that uses [enc] and [dec]. It's
      the users responsibility to provide matching encoder and decoder values.
    **)
  val custom : 'a Encoding.t -> 'a Decoding.t -> 'a t

  (** [conv f g enc] transforms from one encoding to a different one using
      [f] for mapping the results decoded using [enc], and [g] for mapping from
      the input. *)
  val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

  (** [conv_lwt f g enc] is the same as [conv] but where [f] and [g] are
      effectful (produce lwt promises). *)
  val conv_lwt : ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t -> 'b t

  (** [tup2 e1 e2] combines [e1] and [e2] into an encoder for pairs. *)
  val tup2 : 'a t -> 'b t -> ('a * 'b) t

  (** [tup3 e1 e2 e3] combines [e1], [e2], and [e3] into an encoder for
      triples. *)
  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [raw key] is an encoder for bytes under the given [key]. *)
  val raw : key -> bytes t

  (** [value key enc] creates an encoder under the given [key] using the
      provided data-encoding [enc] for encoding/decoding values. *)
  val value : key -> 'a Data_encoding.t -> 'a t

  (** [scope key enc] moves the given encoder [enc] to encode values under a
      branch [key]. *)
  val scope : key -> 'a t -> 'a t

  (** [lazy_mapping enc] produces an encoder for [map]s that uses the given
      [enc] for encoding values. *)
  val lazy_mapping : 'a t -> 'a map t

  (** [lazy_vector enc] produces an encoder for [vector]s that uses the given
      [enc] for encoding values. *)
  val lazy_vector : vector_key t -> 'a t -> 'a vector t

  (** [chunk] is an encoder for the chunks used by [chunked_by_vector]. *)
  val chunk : Chunked_byte_vector.Chunk.t t

  (** [chunked_byte_vector] is an encoder for [chunked_byte_vector]. *)
  val chunked_byte_vector : chunked_byte_vector t

  (** [case tag enc f] returns a partial encoder that represents a case in a
      sum-type. The encoder hides the (existentially bound) type of the
      parameter to the specific case, provided a converter function [f] and
      base encoder [enc]. *)
  val case : 'tag -> 'b t -> ('a -> 'b option) -> ('b -> 'a) -> ('tag, 'a) case

  (** [tagged_union tag_enc cases] returns an encoder that use [tag_enc] for
      encoding the value of a field [tag]. The encoder searches through the list
      of cases for a matching branch. When a matching branch is found, it uses
      its embedded encoder for the value. This function is used for constructing
      encoders for sum-types. *)
  val tagged_union : 'tag t -> ('tag, 'a) case list -> 'a t
end

(** Produces an encoder/decoder module with the provided map, vector and tree
    structures. *)
module Make
    (M : Lazy_map.S with type 'a effect = 'a Lwt.t)
    (V : Lazy_vector.S with type 'a effect = 'a Lwt.t)
    (C : Chunked_byte_vector.S with type 'a effect = 'a Lwt.t)
    (T : Tree.S) :
  S
    with type tree = T.tree
     and type 'a map = 'a M.t
     and type vector_key = V.key
     and type 'a vector = 'a V.t
     and type chunked_byte_vector = C.t
