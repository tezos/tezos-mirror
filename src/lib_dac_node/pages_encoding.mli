(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

(** Library for encoding payloads of arbitrary size in formats that can be
    decoded by the Sc-rollup kernels. *)

type error +=
  | Payload_cannot_be_empty
  | Cannot_serialize_page_payload
  | Cannot_deserialize_page
  | Non_positive_size_of_payload of int
  | Merkle_tree_branching_factor_not_high_enough of int
  | Cannot_combine_pages_data_of_different_type
  | Hashes_page_repr_expected_single_element

(** [CONFIG] is a module for configuration of encoding scheme. *)
module type CONFIG = sig
  (** [max_page_size] is max size of the page, that encoding can produce. *)
  val max_page_size : int
end

type version = int

(** [VERSION] is a module for versioning [Content] and [Hashes] pages of
    a specific encoding. *)
module type VERSION = sig
  (** [content_version] versions [content] pages. *)
  val content_version : version

  (** [hashes_version] versions [hashes] pages. *)
  val hashes_version : version
end

(** [Dac_codec] is a module for encoding a payload as a whole and returning
    the calculated root hash. *)
module type Dac_codec = sig
  (** Page store type. *)
  type page_store

  (** [serialize_payload dac_plugin page_store payload] serializes a
      [payload] into pages suitable to be read by a PVM's `reveal_preimage`
      and stores it into [page_store]. The serialization scheme is some
      hash-based encoding scheme, that produces a root hash representing
      the preimage. *)
  val serialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    bytes ->
    Dac_plugin.hash tzresult Lwt.t

  (** [deserialize_payload dac_plugin page_store hash] deserializes a payload
      from [hash] using some hash-based encoding scheme. Any payload serialized
      by [serialize_payload] can de deserialized from its root hash by
      [deserialize_payload], that is, these functions are inverses of each
      other. *)
  val deserialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    Dac_plugin.hash ->
    bytes tzresult Lwt.t
end

(** [Buffered_dac_codec] icrementally constructs and serializes payload by
    aggregating  messages one message at a time. [add] maintains partially
    constructed pages in a buffer and persist full pages to [page_store].
    [finalize] is called to end the aggregation process, by flushing all the
    remaining data inside the buffer and calculating the resulting root hash. *)
module type Buffered_dac_codec = sig
  (** Buffer type. *)
  type t

  (** Page store type. *)
  type page_store

  (** [empty] returns an empty buffer. *)
  val empty : unit -> t

  (** [add dac_plugin page_store buffer message] adds a [message] to [buffer].
      The [buffer] is serialized to [page_store] when it is full. Serialization
      logic is dependent on the encoding scheme. *)
  val add :
    Dac_plugin.t -> page_store:page_store -> t -> bytes -> unit tzresult Lwt.t

  (** [finalize dac_plugin page_store buffer] serializes the [buffer] to
      [page_store] and returns a root hash that represents the final payload.
      The serialization logic is dependent on the encoding scheme. [buffer] is
      emptied after this call. *)
  val finalize :
    Dac_plugin.t -> page_store:page_store -> t -> Dac_plugin.hash tzresult Lwt.t

  (** [deserialize_payload dac_plugin page_store hash] deserializes a payload
      from [hash]  using some hash-based encoding scheme.
      Any payload serialized by [add] + [finalize] can be deserialized
      by this function. *)
  val deserialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    Dac_plugin.hash ->
    bytes tzresult Lwt.t
end

(** Encoding of DAC payload as a Merkle tree with an arbitrary branching
    factor greater or equal to 2. The serialization process works as follows:
    {ul
      {li A large sequence of bytes, the payload, is split into several pages
          of fixed size, each of which is prefixed with a small sequence
          of bytes (also of fixed size), which is referred to as the preamble
          of the page. Pages obtained directly from the original payload
          are referred to as `Contents pages`. Contents pages constitute the
          leaves of the Merkle tree being built,
      }
      {li Each content page (each of which is a sequence of bytes consisting
        of the preamble followed by the actual content from the original
        payload) is then hashed. The size of each hash is fixed. The hashes are
        concatenated together, and the resulting sequence of bytes is split
        into pages of the same size of `Hashes pages`, each of which is
        prefixed with a preamble whose size is the same as in Contents pages.
        Hashes pages correspond to nodes of the Merkle tree being built, and
        the children of a hash page are the (either Payload or Hashes) pages
        whose hash appear into the former,
      }
      {li Hashes pages are hashed using the same process described above, leading
        to a smaller list of hashes pages. To guarantee that the list of hashes
        pages is actually smaller than the original list of pages being hashed,
        we require the size of pages to be large enough to contain at least two
        hashes.
      }
    }

    Merkle tree encodings of DAC pages are versioned, to allow for multiple
    hashing schemes to be used.
*)
module Merkle_tree : sig
  module V0 : sig
    module Filesystem : Dac_codec with type page_store = Page_store.Filesystem.t

    module Remote : Dac_codec with type page_store = Page_store.Remote.t
  end

  module Internal_for_tests : sig
    module Make_buffered (S : Page_store.S) (V : VERSION) (C : CONFIG) :
      Buffered_dac_codec with type page_store = S.t

    module Make (B : Buffered_dac_codec) :
      Dac_codec with type page_store := B.page_store
  end
end

