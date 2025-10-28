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

(* block_store_status *)

module Block_store_status = struct
  type t = Idle of int | Merging of int

  let get_value status_data =
    let open Lwt_syntax in
    let* status = Stored_data.get status_data in
    match status with
    | Idle value -> return value
    | Merging value -> return value

  let set_idle_status status_data =
    let open Lwt_syntax in
    let* status_value = get_value status_data in
    Stored_data.write status_data (Idle (status_value + 1))

  let set_merge_status status_data =
    let open Lwt_syntax in
    let* status_value = get_value status_data in
    Stored_data.write status_data (Merging (status_value + 1))

  let is_idle = function Idle _ -> true | Merging _ -> false

  let is_merging = function Idle _ -> false | Merging _ -> true

  let get_status_value = function Idle v | Merging v -> v

  let create_idle_status = Idle 0

  let equal s1 s2 =
    match (s1, s2) with
    | Idle c1, Idle c2 when c1 = c2 -> true
    | Merging c1, Merging c2 when c1 = c2 -> true
    | Idle _, Idle _
    | Merging _, Merging _
    | Idle _, Merging _
    | Merging _, Idle _ ->
        false

  let encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"idle"
          int31
          (function Idle c -> Some c | _ -> None)
          (fun c -> Idle c);
        case
          (Tag 1)
          ~title:"merging"
          int31
          (function Merging c -> Some c | _ -> None)
          (fun c -> Merging c);
      ]

  let pp ppf = function
    | Idle i -> Format.fprintf ppf "Idle %d" i
    | Merging i -> Format.fprintf ppf "Merging %d" i

  module Legacy = struct
    type t = Idle | Merging

    let set_idle_status status_data = Stored_data.write status_data Idle

    let set_merge_status status_data = Stored_data.write status_data Merging

    let is_idle = function Idle -> true | Merging -> false

    let is_merging = function Idle -> false | Merging -> true

    let create_idle_status = Idle

    let equal s1 s2 =
      match (s1, s2) with
      | Idle, Idle -> true
      | Merging, Merging -> true
      | Idle, Merging | Merging, Idle -> false

    let encoding =
      let open Data_encoding in
      conv
        (function Idle -> false | Merging -> true)
        (function false -> Idle | true -> Merging)
        bool

    let pp ppf = function
      | Idle -> Format.fprintf ppf "Idle"
      | Merging -> Format.fprintf ppf "Merging"
  end

  let of_legacy legacy_status_data =
    let open Lwt_result_syntax in
    let*! legacy_status = Stored_data.get legacy_status_data in
    match legacy_status with
    | Legacy.Idle -> return (Idle 0)
    | Merging -> return (Merging 0)
end

(* block_descriptor *)

type block_descriptor = Block_hash.t * int32

let block_descriptor_equal (bh1, lvl1) (bh2, lvl2) =
  Block_hash.equal bh1 bh2 && Int32.equal lvl1 lvl2

let block_descriptor_encoding =
  let open Data_encoding in
  tup2 Block_hash.encoding int32

let pp_block_descriptor fmt (hash, level) =
  Format.fprintf fmt "%a (level: %ld)" Block_hash.pp_short hash level

(* chain_config *)

type chain_config = {
  history_mode : History_mode.t;
  genesis : Genesis.t;
  expiration : Time.Protocol.t option;
}

let chain_config_equal {history_mode = hm1; genesis = g1; expiration = e1}
    {history_mode = hm2; genesis = g2; expiration = e2} =
  History_mode.equal hm1 hm2 && Genesis.equal g1 g2
  && Option.equal Time.Protocol.equal e1 e2

let chain_config_encoding =
  let open Data_encoding in
  conv
    (fun {history_mode; genesis; expiration} ->
      (history_mode, genesis, expiration))
    (fun (history_mode, genesis, expiration) ->
      {history_mode; genesis; expiration})
    (obj3
       (req "history_mode" History_mode.encoding)
       (req "genesis" Genesis.encoding)
       (varopt "expiration" Time.Protocol.encoding))

(* invalid_block *)

type invalid_block = {level : int32; errors : Error_monad.error list}

let invalid_block_equal {level = l1; errors = e1} {level = l2; errors = e2} =
  Int32.equal l1 l2 && Stdlib.( = ) e1 e2

let invalid_block_encoding =
  let open Data_encoding in
  conv
    (fun {level; errors} -> (level, errors))
    (fun (level, errors) -> {level; errors})
    (obj2 (req "level" int32) (req "errors" (list Error_monad.error_encoding)))

module Block_lru_cache =
  Aches_lwt.Lache.Make_option
    (Aches.Rache.Transfer (Aches.Rache.LRU) (Block_hash))

module Protocol_levels = struct
  include Map.Make (struct
    type t = int

    let compare = Compare.Int.compare
  end)

  type protocol_info = {
    protocol : Protocol_hash.t;
    activation_block : block_descriptor;
    expect_predecessor_context : bool;
  }

  let protocol_info_encoding =
    let open Data_encoding in
    conv
      (fun {protocol; activation_block; expect_predecessor_context} ->
        (protocol, activation_block, expect_predecessor_context))
      (fun (protocol, activation_block, expect_predecessor_context) ->
        {protocol; activation_block; expect_predecessor_context})
      (obj3
         (req "protocol" Protocol_hash.encoding)
         (req "activation_block" block_descriptor_encoding)
         (req "expect_predecessor_context" bool))

  let with_protocol_info
      {protocol; activation_block; expect_predecessor_context} f =
    f protocol activation_block expect_predecessor_context

  let equal_protocol_info pi1 pi2 =
    with_protocol_info pi1 @@ fun p1 (bh1, lvl1) epc1 ->
    with_protocol_info pi2 @@ fun p2 (bh2, lvl2) epc2 ->
    Protocol_hash.equal p1 p2 && Block_hash.equal bh1 bh2
    && Int32.equal lvl1 lvl2 && Bool.equal epc1 epc2

  let equal map1 map2 = equal equal_protocol_info map1 map2

  let encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun bindings ->
        List.fold_left (fun map (k, v) -> add k v map) empty bindings)
      Data_encoding.(list (tup2 uint8 protocol_info_encoding))

  module Legacy = struct
    type commit_info = {
      author : string;
      message : string;
      test_chain_status : Test_chain_status.t;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      data_merkle_root : Context_hash.t;
      parents_contexts : Context_hash.t list;
    }

    let commit_info_encoding =
      let open Data_encoding in
      conv
        (fun {
               author;
               message;
               test_chain_status;
               data_merkle_root;
               predecessor_block_metadata_hash;
               predecessor_ops_metadata_hash;
               parents_contexts;
             }
           ->
          ( author,
            message,
            test_chain_status,
            data_merkle_root,
            predecessor_block_metadata_hash,
            predecessor_ops_metadata_hash,
            parents_contexts ))
        (fun ( author,
               message,
               test_chain_status,
               data_merkle_root,
               predecessor_block_metadata_hash,
               predecessor_ops_metadata_hash,
               parents_contexts )
           ->
          {
            author;
            message;
            test_chain_status;
            data_merkle_root;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
            parents_contexts;
          })
        (obj7
           (req "author" string)
           (req "message" string)
           (req "test_chain_status" Test_chain_status.encoding)
           (req "data_merkle_root" Context_hash.encoding)
           (opt "predecessor_block_metadata_hash" Block_metadata_hash.encoding)
           (opt
              "predecessor_ops_metadata_hash"
              Operation_metadata_list_list_hash.encoding)
           (req "parents_contexts" (list Context_hash.encoding)))

    type activation_block = {
      block : block_descriptor;
      protocol : Protocol_hash.t;
      commit_info : commit_info option;
    }

    include Map.Make (struct
      type t = int

      let compare = Compare.Int.compare
    end)

    let with_commit_info
        {
          author;
          message;
          test_chain_status;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          data_merkle_root;
          parents_contexts;
        } f =
      f
        author
        message
        test_chain_status
        predecessor_block_metadata_hash
        predecessor_ops_metadata_hash
        data_merkle_root
        parents_contexts

    let commit_info_equal ci1 ci2 =
      with_commit_info ci1 @@ fun a1 m1 tcs1 pbmh1 pomh1 dmr1 pc1 ->
      with_commit_info ci2 @@ fun a2 m2 tcs2 pbmh2 pomh2 dmr2 pc2 ->
      String.equal a1 a2 && String.equal m1 m2
      && Test_chain_status.equal tcs1 tcs2
      && Option.equal Block_metadata_hash.equal pbmh1 pbmh2
      && Option.equal Operation_metadata_list_list_hash.equal pomh1 pomh2
      && Context_hash.equal dmr1 dmr2
      && List.equal Context_hash.equal pc1 pc2

    let activation_block_equal {block = b1; protocol = p1; commit_info = ci1}
        {block = b2; protocol = p2; commit_info = ci2} =
      block_descriptor_equal b1 b2
      && Protocol_hash.equal p1 p2
      && Option.equal commit_info_equal ci1 ci2

    let equal map1 map2 = equal activation_block_equal map1 map2

    let legacy_activation_block_encoding =
      let open Data_encoding in
      conv
        (fun {block; protocol; commit_info} -> (block, protocol, commit_info))
        (fun (block, protocol, commit_info) -> {block; protocol; commit_info})
        (obj3
           (req "block" block_descriptor_encoding)
           (req "protocol" Protocol_hash.encoding)
           (opt "commit_info" commit_info_encoding))

    let encoding =
      Data_encoding.conv
        (fun map -> bindings map)
        (fun bindings ->
          List.fold_left (fun map (k, v) -> add k v map) empty bindings)
        Data_encoding.(list (tup2 uint8 legacy_activation_block_encoding))
  end
end

type metadata_stat = {
  block_level : Int32.t;
  block_metadata_size : Int64.t;
  operation_metadata_arity : int list;
  operation_metadata_size : Int64.t;
  manager_operation_metadata_sizes : int list;
  too_large_operation_metadata_count : Int64.t;
}
