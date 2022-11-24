(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(* DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4088
   Add .mli file. *)

(** Library for encoding payloads of arbitrary size in formats that can be
    decoded by the Sc-rollup kernels.
 *)

open Protocol
open Alpha_context
open Environment.Error_monad

type error +=
  | Payload_cannot_be_empty
  | Cannot_serialize_page_payload
  | Cannot_deserialize_page
  | Non_positive_size_of_payload
  | Merkle_tree_branching_factor_not_high_enough

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_deserialize_dac_page_payload"
    ~title:"DAC payload could not be deserialized"
    ~description:"Error when recovering DAC payload payload from binary"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Error when recovering DAC payload from list of data chunks")
    Data_encoding.(unit)
    (function Cannot_deserialize_page -> Some () | _ -> None)
    (fun () -> Cannot_deserialize_page) ;
  register_error_kind
    `Permanent
    ~id:"cannot_serialize_dac_page"
    ~title:"DAC page could not be serialized"
    ~description:"Error when serializing DAC page"
    ~pp:(fun ppf () -> Format.fprintf ppf "Error when serializing DAC page")
    Data_encoding.(unit)
    (function Cannot_serialize_page_payload -> Some () | _ -> None)
    (fun () -> Cannot_serialize_page_payload) ;
  register_error_kind
    `Permanent
    ~id:"non_positive_payload_size"
    ~title:"Non positive size for dac payload"
    ~description:"Dac page payload (excluded preamble) are non positive"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Dac page payload (excluded preamble) are non positive")
    Data_encoding.(unit)
    (function Non_positive_size_of_payload -> Some () | _ -> None)
    (fun () -> Non_positive_size_of_payload) ;
  register_error_kind
    `Permanent
    ~id:"dac_payload_cannot_be_empty"
    ~title:"Cannot serialize empty DAC payload"
    ~description:"Cannot serialize empty DAC payload"
    ~pp:(fun ppf () -> Format.fprintf ppf "Cannot serialize empty DAC payload")
    Data_encoding.(unit)
    (function Payload_cannot_be_empty -> Some () | _ -> None)
    (fun () -> Payload_cannot_be_empty) ;
  register_error_kind
    `Permanent
    ~id:"merkle_tree_branching_factor_not_high_enough"
    ~title:"Merkle tree branching factor must be at least 2"
    ~description:"Merkle tree branching factor must be at least 2"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot serialize DAC payload: pages must be able to contain at least \
         two hashes")
    Data_encoding.(unit)
    (function
      | Merkle_tree_branching_factor_not_high_enough -> Some () | _ -> None)
    (fun () -> Merkle_tree_branching_factor_not_high_enough)

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
      {li Each contents page (each of which is a sequence of bytes consisting
        of the preamble followed by the actual contents from the original
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
module Merkle_tree = struct
  type version = int

  (** A page is either a `Contents page`, containing a chunk of the payload
      that needs to be serialized, or a `Hashes page`, containing a list
      of hashes. The size of contents and hashes pages is not fixed.
    *)
  type 'a page = Contents of bytes | Hashes of 'a list

  let max_version = 127

  module type VERSION = sig
    val contents_version_tag : version

    val hashes_version_tag : version
  end

  (* Even numbers are used for versioning Contents pages, odd numbers are used
     for versioning Hashes pages. *)
  module Make_version (V : sig
    val contents_version : int

    val hashes_version : int
  end) =
  struct
    let contents_version_tag = 2 * V.contents_version

    let hashes_version_tag = (2 * V.hashes_version) + 1
  end

  module Make (Hashing_scheme : sig
    include Sc_rollup.REVEAL_HASH

    val scheme : supported_hashes
  end)
  (V : VERSION) =
  struct
    let hash bytes = Hashing_scheme.hash_bytes [bytes]

    let hash_encoding = Hashing_scheme.encoding

    let hashes_encoding = Data_encoding.list hash_encoding

    let to_b58check = Hashing_scheme.to_b58check

    (* The preamble of a serialized page contains 1 byte denoting the version,
       and 4 bytes encoding the size of the rest of the page. In total, 5
       bytes. *)
    let page_preamble_size = 5

    let hash_bytes_size = Hashing_scheme.size ~scheme:Hashing_scheme.scheme

    (** Payload pages are encoded as follows: the first byte is an integer,
        which is corresponds to either `payload_version` (for payload pages) or
        `hashes_version` (for hashes pages). The next four bytes will contain
        the size of the rest of the page; the remainder of the page is either a
        list of raw bytes (in the case of a payload page), or a list of hashes,
        which occupy 32 bytes each. *)
    let page_encoding =
      Data_encoding.(
        union
          ~tag_size:`Uint8
          [
            case
              ~title:"contents"
              (Tag V.contents_version_tag)
              bytes
              (function Contents payload -> Some payload | _ -> None)
              (fun payload -> Contents payload);
            case
              ~title:"hashes"
              (Tag V.hashes_version_tag)
              hashes_encoding
              (function Hashes hashes -> Some hashes | _ -> None)
              (fun hashes -> Hashes hashes);
          ])

    (** Serialization function for a single page. It converts a page to a
        sequence of bytes using [page_encoding]. It also checks that the
        serialized page does not exceed [page_size] bytes. *)
    let serialize_page ~max_page_size page =
      match
        Data_encoding.Binary.to_bytes
          (Data_encoding.check_size max_page_size page_encoding)
          page
      with
      | Ok raw_page -> Ok raw_page
      | Error _ -> error Cannot_serialize_page_payload

    let split_hashes ~max_page_size hashes =
      (* 1 byte for the version size, 4 bytes for the length of the list. *)
      let number_of_hashes =
        (max_page_size - page_preamble_size) / hash_bytes_size
      in
      (* Requiring a branching factor of at least 2 is necessary to ensure that
         the serialization process terminates. If only one hash were stored per page, then
         the number of pages at height `n-1` could be potentially equal to the number of
         pages at height `n`. *)
      if number_of_hashes < 2 then
        error Merkle_tree_branching_factor_not_high_enough
      else
        let rec go aux list =
          match list with
          | [] -> List.rev aux
          | list ->
              let chunk, rest = List.split_n number_of_hashes list in
              (go [@tailcall]) (chunk :: aux) rest
        in
        Ok (go [] hashes)

    let split_contents ~max_page_size page =
      let open Result_syntax in
      (* 1 byte for the version size, 4 bytes for the size of the payload. *)
      let actual_page_size = max_page_size - page_preamble_size in
      if actual_page_size <= 0 then error Non_positive_size_of_payload
      else
        let+ pages = String.chunk_bytes actual_page_size page in
        List.map String.to_bytes pages

    (* Splits a page into smaller pages whose serialization does
        not exceed [page_size] bytes. *)
    let split_page ~max_page_size page =
      let open Result_syntax in
      match page with
      | Contents contents ->
          let+ pages = split_contents ~max_page_size contents in
          List.map (fun page_contents -> Contents page_contents) pages
      | Hashes hashes ->
          let+ pages = split_hashes ~max_page_size hashes in
          List.map (fun page_hashes -> Hashes page_hashes) pages

    (* DAC/FIXME: https://gitlab.com/tezos/tezos/-/issues/4014
       Improve performance of the functions below. Currently we load in memory
       the whole payload to be split in pages. This is not ideal if the size
       of the payload is large. *)

    (** Main function for computing the pages of a Merkle tree from a sequence
        of bytes. Each page is processed using the function [for_each_page]
        provided in input, which is responsible for ensuring that the original
        payload can be reconstructed from the Merkle tree root; this can be
        achieved, for example, by letting [for_each_page] persist a serialized
        page to disk using the page hash as its filename.
        The function [serialize_payload] returns the root hash of the Merkle
        tree constructed. *)
    let serialize_payload ~max_page_size payload ~for_each_page =
      let open Lwt_result_syntax in
      let* () =
        fail_unless (Bytes.length payload > 0) Payload_cannot_be_empty
      in
      let rec go payload =
        let*? pages = split_page ~max_page_size payload in
        let*? serialized_pages =
          List.map_e (serialize_page ~max_page_size) pages
        in
        (* Hashes are computed from raw pages, each of which consists of a
           preamble of 5 bytes followed by a page payload - a raw sequence
           of bytes from the original payload for Contents pages, and a
           a sequence of serialized hashes for hashes pages. The preamble
           bytes is part of the sequence of bytes which is hashed.

           Hashes are stored in reverse order in memory for performance
           reasons. They are reversed again before the recursive tailcall
           is performed.*)
        let hashes_with_serialized_pages =
          List.rev_map
            (fun page -> (hash ~scheme:Hashing_scheme.scheme page, page))
            serialized_pages
        in

        let* () =
          List.iter_es
            (fun (hash, page) -> for_each_page (hash, page))
            hashes_with_serialized_pages
        in

        match hashes_with_serialized_pages with
        | [(hash, _page)] -> return hash
        | hashes_with_raw_pages ->
            let hashes =
              (* Hashes_with_raw_pages stores the hash of pages in reverse
                 order. We use `List.rev_map` to recover the original order
                 of hashes. *)
              List.rev_map (fun (hash, _page) -> hash) hashes_with_raw_pages
            in
            (go [@tailcall]) (Hashes hashes)
      in
      go (Contents payload)

    (** Deserialization function for a single page. A sequence of bytes is
        converted to a page using [page_encoding]. *)
    let deserialize_page raw_page =
      match Data_encoding.Binary.of_bytes page_encoding raw_page with
      | Ok page -> Ok page
      | Error _ -> error Cannot_deserialize_page

    (** Deserialization function for reconstructing the original payload from
        its Merkle tree root hash. The function [retrieve_page_from_hash]
        passed in input is responsible for determining how to retrieve the
        serialized page from its hash. For example, if the page has been
        persisted to disk using the page hash as its filename,
        [retrieve_page_from_hash] simply loads the corresponding file from
        disk to memory. The function [deserialize_payload] returns the
        original payload that was used to compute the Merkle tree root hash.
        This function is guaranteed to terminate if the directed graph induced
        by the retrieved pages (that is, the graph where there is an edge
        from one page to another if and only if the former contains the hash
        of the latter) is acyclic. This property is guaranteed if the root
        hash and pages are computed using the serialized_payload function
        outlined above, but it is not guaranteed in more general cases.
     *)
    let deserialize_payload root_hash ~retrieve_page_from_hash =
      let rec go retrieved_hashes retrieved_contents =
        let open Lwt_result_syntax in
        match retrieved_hashes with
        | [] -> return @@ Bytes.concat Bytes.empty retrieved_contents
        | hash :: hashes -> (
            let* serialized_page = retrieve_page_from_hash hash in
            let*? page = deserialize_page serialized_page in
            match page with
            | Hashes page_hashes ->
                (* Hashes are saved in reverse order. *)
                (go [@tailcall])
                  (List.rev_append page_hashes hashes)
                  retrieved_contents
            | Contents contents ->
                (* Because hashes are saved in reversed order, content pages
                   will be retrieved in reverse order. By always appending a
                   conetent page to the list of retrieved content pages,
                   we ensure that pages are saved in `retrieved_contents` in
                   their original order. *)
                (go [@tailcall]) hashes (contents :: retrieved_contents))
      in
      go [root_hash] []
  end

  module V0 =
    Make
      (struct
        include Sc_rollup.Reveal_hash

        let scheme = Sc_rollup.Reveal_hash.Blake2B
      end)
      (Make_version (struct
        (* Cntents_version_tag used in contents pages is 0. *)
        let contents_version = 0

        (* Hashes_version_tag used in hashes pages is 1. *)
        let hashes_version = 0
      end))
end
