(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
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

type error +=
  | Payload_cannot_be_empty
  | Cannot_serialize_page_payload
  | Cannot_deserialize_page
  | Non_positive_size_of_payload
  | Merkle_tree_branching_factor_not_high_enough
  | Cannot_combine_pages_data_of_different_type
  | Hashes_page_repr_expected_single_element

module type CONFIG = sig
  val max_page_size : int
end

type version = int

module type VERSION = sig
  val content_version : version

  val hashes_version : version
end

module type Dac_codec = sig
  type page_store

  val serialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    bytes ->
    Dac_plugin.hash tzresult Lwt.t

  val deserialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    Dac_plugin.hash ->
    bytes tzresult Lwt.t
end

module type Buffered_dac_codec = sig
  type t

  type page_store

  val empty : unit -> t

  val add :
    Dac_plugin.t -> page_store:page_store -> t -> bytes -> unit tzresult Lwt.t

  val finalize :
    Dac_plugin.t -> page_store:page_store -> t -> Dac_plugin.hash tzresult Lwt.t

  val deserialize_payload :
    Dac_plugin.t ->
    page_store:page_store ->
    Dac_plugin.hash ->
    bytes tzresult Lwt.t
end

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
    (fun () -> Merkle_tree_branching_factor_not_high_enough) ;
  register_error_kind
    `Permanent
    ~id:"cannot_combine_pages_data_of_different_type"
    ~title:"Cannot combine pages data of different type"
    ~description:"Cannot combine pages data of different type"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Merkle level serizalization: when adding page data to a given level \
         repr., the type of page data received is different then the one \
         inside a level repr.")
    Data_encoding.(unit)
    (function
      | Cannot_combine_pages_data_of_different_type -> Some () | _ -> None)
    (fun () -> Cannot_combine_pages_data_of_different_type) ;
  register_error_kind
    `Permanent
    ~id:"hashes_page_repr_expected_single_element"
    ~title:"Hashes page representation expected a single element"
    ~description:"Hashes page representation expected a single element"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Hashes page representation expected a single element")
    Data_encoding.unit
    (function Hashes_page_repr_expected_single_element -> Some () | _ -> None)
    (fun () -> Hashes_page_repr_expected_single_element)

module Merkle_tree = struct
  (** A page is either a [Contents] page, containing a chunk of the payload
      to serialize, or a [Hashes] page, containing a list of hashes.
      The maximum size of bytes inside [Contents] page, or number
      of hashes inside [Hashes] page is such, that when serializing a page
      using [page_encoding], it does not exceed [max_page_size] bytes.
  *)
  type 'a page = Contents of bytes | Hashes of 'a list

  let max_version = 127

  (** [Make_buffered] is a functor for creating a [Buffered_dac_codec] module.

      It uses an in-memory data structure that enables aggregating DAC payload.
      Data is serialized with respect to [page_encoding], by storing the full
      payload in the shape of a k-ary merkle tree onto the disk.
      During the proccess of partial serialization, the minimum amount of data
      is kept in memory, by eagerly persisting the full pages of a given merkle
      level whenever possible, to ease the load on the memory.

      The serializer respects the following invariant:
      Starting with an [empty] serializer, splitting arbitrary [payload] into
      chunks of arbitrary size, and adding them to the serializer from left to
      right, should result in same root hash as adding all the payload data in
      one chunk, provided it could fit into the memory.
  *)
  module Make_buffered (S : Page_store.S) (V : VERSION) (C : CONFIG) = struct
    (** Even numbers are used for versioning [Contents] pages,
        odd numbers are used for versioning [Hashes] pages. *)
    module V = struct
      let content_version_tag =
        assert (V.content_version < max_version) ;
        2 * V.content_version

      let hashes_version_tag =
        assert (V.hashes_version < max_version) ;
        (2 * V.hashes_version) + 1
    end

    (** [page_data] is either [Cont_data] or [Hash_data] where each represents
        data not bound in size for [Contents] or [Hashes] page respectively. *)
    type page_data = Cont_data of bytes | Hash_data of Dac_plugin.hash list

    type t = page_data Stack.t

    type page_store = S.t

    let hash ((module P) : Dac_plugin.t) bytes =
      P.hash_bytes [bytes] ~scheme:Blake2B

    let hashes_encoding ((module P) : Dac_plugin.t) =
      Data_encoding.list P.encoding

    (** The preamble of a serialized page contains 1 byte denoting the version,
        and 4 bytes encoding the size of the rest of the page. In total, 5
        bytes. *)
    let page_preamble_size = 5

    let hash_bytes_size ((module P) : Dac_plugin.t) = P.size ~scheme:Blake2B

    (** Payload pages are encoded as follows: the first byte is an integer,
        which corresponds to either [content_version_tag] (for payload pages)
        or [hashes_version_tag] (for hashes pages). The next four bytes
        contain the size of the rest of the page. The remainder of the page is
        either a list of raw bytes (in the case of a payload page), or a list
        of hashes, which occupy 33 bytes each. I.e. 32 bytes for the inner hash
        and 1 byte for the tag identifying the hashing scheme. *)
    let page_encoding dac_plugin =
      Data_encoding.(
        union
          ~tag_size:`Uint8
          [
            case
              ~title:"content"
              (Tag V.content_version_tag)
              bytes
              (function Contents payload -> Some payload | _ -> None)
              (fun payload -> Contents payload);
            case
              ~title:"hashes"
              (Tag V.hashes_version_tag)
              (hashes_encoding dac_plugin)
              (function Hashes hashes -> Some hashes | _ -> None)
              (fun hashes -> Hashes hashes);
          ])

    (** Serialization function for a single page. It converts a page to a
        sequence of bytes using [page_encoding]. It also checks that the
        serialized page does not exceed [page_size] bytes. *)
    let serialize_page dac_plugin page =
      match
        Data_encoding.Binary.to_bytes
          (Data_encoding.check_size C.max_page_size (page_encoding dac_plugin))
          page
      with
      | Ok raw_page -> Ok raw_page
      | Error _ -> Result_syntax.tzfail Cannot_serialize_page_payload

    let store_page dac_plugin ~page_store raw_page =
      let open Lwt_result_syntax in
      let*? serialized_page = serialize_page dac_plugin raw_page in
      (* Hashes are computed from raw pages, each of which consists of a
         preamble of 5 bytes followed by a page payload - a raw sequence
         of bytes from the original payload for `Contents` pages, and a
         a sequence of serialized hashes for `Hashes` pages. The preamble
         bytes is part of the sequence of bytes which is hashed.
      *)
      let hash = hash dac_plugin serialized_page in
      let* () = S.save dac_plugin page_store ~hash ~content:serialized_page in
      return hash

    let is_empty_page_data = function
      | Cont_data cont -> Bytes.empty = cont
      | Hash_data ls -> List.is_empty ls

    let max_hashes_per_page dac_plugin =
      (C.max_page_size - page_preamble_size) / hash_bytes_size dac_plugin

    (** Represents data that is held in-memory and is yet to be persisted to the
        disk - effectively acting as a buffer, making sure that all [pages] of
        the given level that are written to the disk are full (with the
        exception of last one upon finalization). For every existing level
        written onto to the disk, the stack holds a corresponding [page_data]
        in-memory representation.
    *)
    module Merkle_level = struct
      let init_level page_data = page_data

      let is_empty level = is_empty_page_data level

      let has_single_hash = function Hash_data [_] -> true | _ -> false

      let get_single_hash level =
        let open Lwt_result_syntax in
        match level with
        | Hash_data [x] -> return x
        | _ -> tzfail Hashes_page_repr_expected_single_element

      (** Splits payload into bytes chunks whose size does not exceed
          [page_size] bytes. *)
      let split_payload payload =
        let open Result_syntax in
        (* 1 byte for the version size, 4 bytes for the size of the payload. *)
        let actual_page_size = C.max_page_size - page_preamble_size in
        if actual_page_size <= 0 then tzfail Non_positive_size_of_payload
        else
          let+ splitted_payload = String.chunk_bytes actual_page_size payload in
          List.map
            (fun cont -> Contents (String.to_bytes cont))
            splitted_payload

      let split_hashes dac_plugin hashes =
        let number_of_hashes = max_hashes_per_page dac_plugin in
        (* Requiring a branching factor of at least 2 is necessary to ensure that
           the serialization process terminates. If only one hash was stored
           per page, then the number of pages at height `n-1` could be
           potentially equal to the number of pages at height `n`. *)
        if number_of_hashes < 2 then
          Result_syntax.tzfail Merkle_tree_branching_factor_not_high_enough
        else
          let rec go aux list =
            match list with
            | [] -> List.rev aux
            | list ->
                let chunk, rest = List.split_n number_of_hashes list in
                (go [@tailcall]) (Hashes chunk :: aux) rest
          in
          Ok (go [] hashes)

      let split dac_plugin page_data =
        let leftover_with_full_pages ls =
          let open Lwt_result_syntax in
          match List.rev ls with
          | [] ->
              tzfail Payload_cannot_be_empty (* We don't expect to get here. *)
          | Contents h :: xs -> return (Cont_data h, List.rev xs)
          | Hashes h :: xs -> return (Hash_data h, List.rev xs)
        in
        let open Lwt_result_syntax in
        let*? pages =
          match page_data with
          | Cont_data cont -> split_payload cont
          | Hash_data ls -> split_hashes dac_plugin ls
        in
        leftover_with_full_pages pages

      let add_page_data ~level ~page_data =
        let open Lwt_result_syntax in
        match (level, page_data) with
        | Cont_data level, Cont_data page_data ->
            return @@ Cont_data (Bytes.cat level page_data)
        | Hash_data level, Hash_data page_data ->
            return @@ Hash_data (List.append level page_data)
        | Hash_data _, Cont_data _ | Cont_data _, Hash_data _ ->
            (* We dont expect to get here. *)
            tzfail Cannot_combine_pages_data_of_different_type

      let process dac_plugin ~page_store level =
        let open Lwt_result_syntax in
        let* level, pages = split dac_plugin level in
        let* rev_hashes =
          List.fold_left_es
            (fun accm page ->
              let* hash = store_page dac_plugin ~page_store page in
              return @@ (hash :: accm))
            []
            pages
        in
        return (level, Hash_data (List.rev rev_hashes))
    end

    let empty : unit -> t = Stack.create

    (** Adds a [page_data] to the bottom level of the in-memory merkle tree repr.
        If [page_data] inside level exceeds its [max_page_size], then we split
        it into valid full [page]/s and a leftover that is smaller or equal to
        [max_page_size].

        Full pages are eagerly stored to disk and parent hashes are added
        in batch to next level. The leftover represents the content
        of current level. Procedure repeats recursively if necessarily.

        Throws [Payload_cannot_be_empty] if [page_data] is empty.
    *)
    let rec add_rec dac_plugin ~page_store stack page_data =
      let open Lwt_result_syntax in
      let open Merkle_level in
      let* merkle_level =
        if Stack.is_empty stack then return (init_level page_data)
        else add_page_data ~level:(Stack.pop stack) ~page_data
      in
      let* merkle_level, page_data =
        process dac_plugin ~page_store merkle_level
      in
      let* () =
        if not @@ is_empty_page_data page_data then
          add_rec dac_plugin ~page_store stack page_data
        else return ()
      in
      return @@ Stack.push merkle_level stack

    (** Adding [payload] to the serializer does not guarantee its persistance
        to disk. The data is fully serialized after the call to [finalize].
        This guarantees that only the last [page] in the given level could be
        partially filled.

        Throws [Payload_cannot_be_empty] in case of empty payload. *)
    let add dac_plugin ~page_store stack payload =
      let open Lwt_result_syntax in
      let* () =
        fail_unless (Bytes.length payload > 0) Payload_cannot_be_empty
      in
      let number_of_hashes = max_hashes_per_page dac_plugin in
      (* Requiring a branching factor of at least 2 is necessary to ensure that
         the serialization process terminates. If only one hash were stored
         per page, then the number of pages at height `n-1`could be potentially
         equal to the number of pages at height `n`. *)
      let* () =
        fail_unless
          (number_of_hashes > 1)
          Merkle_tree_branching_factor_not_high_enough
      in
      add_rec dac_plugin ~page_store stack (Cont_data payload)

    (** [finalize] returns a root hash of aggregated data, by eagerly storing
         every partially filled level on the stack. Starting at the bottom and
         adding stored page hash to next level, repeating this procedure until
         left with a single (root) hash. *)
    let rec finalize dac_plugin ~page_store stack =
      let open Merkle_level in
      let finalize_level page_data =
        let store_page = store_page ~page_store in
        match page_data with
        | Cont_data cont -> store_page dac_plugin (Contents cont)
        | Hash_data ls -> store_page dac_plugin (Hashes ls)
      in
      let open Lwt_result_syntax in
      let* () =
        (* Empty stack means no payload added in the first place. *)
        fail_unless (not @@ Stack.is_empty stack) Payload_cannot_be_empty
      in
      let merkle_level = Stack.pop stack in
      if is_empty merkle_level then
        (* If level is empty, there is nothing to hash. *)
        finalize dac_plugin ~page_store stack
      else if Stack.is_empty stack && has_single_hash merkle_level then
        (* If top level of the tree and only one hash, then this is a root hash. *)
        get_single_hash merkle_level
      else
        let* hash = finalize_level merkle_level in
        let* () = add_rec dac_plugin ~page_store stack (Hash_data [hash]) in
        finalize dac_plugin ~page_store stack

    (** Deserialization function for a single page. A sequence of bytes is
        converted to a page using [page_encoding]. *)
    let deserialize_page dac_plugin raw_page =
      match
        Data_encoding.Binary.of_bytes (page_encoding dac_plugin) raw_page
      with
      | Ok page -> Ok page
      | Error _ -> Result_syntax.tzfail Cannot_deserialize_page

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
    let deserialize_payload dac_plugin ~page_store root_hash =
      let rec go retrieved_hashes retrieved_content =
        let open Lwt_result_syntax in
        match retrieved_hashes with
        | [] -> return @@ Bytes.concat Bytes.empty retrieved_content
        | hash :: hashes -> (
            let* serialized_page = S.load dac_plugin page_store hash in
            let*? page = deserialize_page dac_plugin serialized_page in
            match page with
            | Hashes page_hashes ->
                (* Hashes are saved in reverse order. *)
                (go [@tailcall])
                  (List.rev_append page_hashes hashes)
                  retrieved_content
            | Contents content ->
                (* Because hashes are saved in reversed order, content pages
                   will be retrieved in reverse order. By always appending a
                   content page to the list of retrieved content pages,
                   we ensure that pages are saved in `retrieved_content` in
                   their original order. *)
                (go [@tailcall]) hashes (content :: retrieved_content))
      in
      go [root_hash] []
  end

  (** [Make] functor derives a [Dac_codec] from [Buffered_dac_codec]
      sharing the same underlying encoding scheme. *)
  module Make (B : Buffered_dac_codec) = struct
    type page_store = B.page_store

    (** Main function for computing the pages of a Merkle tree from a sequence
        of bytes. Each page is processed and persisted to [page_store] such that
        the original payload can be  reconstructed from its Merkle tree root;
        The function [serialize_payload] returns the root hash of the Merkle
        tree constructed. *)
    let serialize_payload dac_plugin ~page_store payload =
      let open Lwt_result_syntax in
      let stack = B.empty () in
      let* () = B.add dac_plugin ~page_store stack payload in
      B.finalize dac_plugin ~page_store stack

    let deserialize_payload = B.deserialize_payload
  end

  module V0_metadata = struct
    (* [contents_version_tag] used in contents pages is 0. *)
    let content_version = 0

    (* [hashes_version_tag] used in hashes pages is 1. *)
    let hashes_version = 0
  end

  module V0_page_size = struct
    let max_page_size = 4096
  end

  module V0_Buffered = struct
    module Filesystem =
      Make_buffered (Page_store.Filesystem) (V0_metadata) (V0_page_size)
    module Remote =
      Make_buffered (Page_store.Remote) (V0_metadata) (V0_page_size)
  end

  module V0 = struct
    module Filesystem = Make (V0_Buffered.Filesystem)
    module Remote = Make (V0_Buffered.Remote)
  end

  module Internal_for_tests = struct
    module Make_buffered = Make_buffered
    module Make = Make
  end
end

module Hash_chain = struct
  module V0 = struct
    type page = {succ_hash : Dac_plugin.hash; content : string}

    let hash ((module P) : Dac_plugin.t) bytes =
      P.hash_bytes ~scheme:Blake2B [bytes]

    let content_limit =
      (4 * 1024) - 100 (* We reserve 100 bytes for the continuation hash. *)

    let serialize_page ((module P) : Dac_plugin.t) page =
      Format.asprintf "%s hash:%s" page.content (P.to_hex page.succ_hash)

    let link_chunks dac_plugin chunks : (Dac_plugin.hash * bytes) list =
      let rec link_chunks_rev linked_pages rev_pages =
        match rev_pages with
        | [] -> linked_pages
        | chunk :: rev_chunks ->
            let page =
              match linked_pages with
              | [] -> chunk
              | (succ_hash, _) :: _ ->
                  serialize_page dac_plugin {succ_hash; content = chunk}
            in
            let page = Bytes.of_string page in
            let hash = hash dac_plugin page in
            (link_chunks_rev [@tailcall])
              ((hash, page) :: linked_pages)
              rev_chunks
      in
      let rev_chunks = List.rev chunks in
      link_chunks_rev [] rev_chunks

    let make_hash_chain dac_plugin data =
      let open Result_syntax in
      let+ chunks = String.chunk_bytes content_limit data in
      link_chunks dac_plugin chunks

    (** Main function for computing a hash chain from a byte sequence. Returns the
        chain head hash.[for_each_page] may be supplied to run post processing
        tasks on each page, for example, to persisit a serialized page to disk.
      *)
    let serialize_payload dac_plugin ~for_each_page payload =
      let open Lwt_result_syntax in
      let* () =
        fail_unless (Bytes.length payload > 0) Payload_cannot_be_empty
      in
      let*? hash_chain = make_hash_chain dac_plugin payload in
      let+ () = List.iter_es for_each_page hash_chain in
      Stdlib.List.hd hash_chain |> fst
  end
end
