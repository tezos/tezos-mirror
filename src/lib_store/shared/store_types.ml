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

type block_descriptor = Tezos_crypto.Block_hash.t * int32

let block_descriptor_encoding =
  let open Data_encoding in
  tup2 Tezos_crypto.Block_hash.encoding int32

let pp_block_descriptor fmt (hash, level) =
  Format.fprintf fmt "%a (level: %ld)" Tezos_crypto.Block_hash.pp hash level

type chain_config = {
  history_mode : History_mode.t;
  genesis : Genesis.t;
  expiration : Time.Protocol.t option;
}

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

type invalid_block = {level : int32; errors : Error_monad.error list}

let invalid_block_encoding =
  let open Data_encoding in
  conv
    (fun {level; errors} -> (level, errors))
    (fun (level, errors) -> {level; errors})
    (obj2 (req "level" int32) (req "errors" (list Error_monad.error_encoding)))

module Block_lru_cache =
  Ringo_lwt.Functors.Make_opt
    ((val Ringo.(
            map_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise))
       (Tezos_crypto.Block_hash))

module Protocol_levels = struct
  include Map.Make (struct
    type t = int

    let compare = Compare.Int.compare
  end)

  type commit_info = {
    author : string;
    message : string;
    test_chain_status : Test_chain_status.t;
    predecessor_block_metadata_hash : Tezos_crypto.Block_metadata_hash.t option;
    predecessor_ops_metadata_hash :
      Tezos_crypto.Operation_metadata_list_list_hash.t option;
    data_merkle_root : Tezos_crypto.Context_hash.t;
    parents_contexts : Tezos_crypto.Context_hash.t list;
  }

  let commit_info_of_tuple
      ( _protocol_hash,
        author,
        message,
        _timestamp,
        test_chain_status,
        data_merkle_root,
        predecessor_block_metadata_hash,
        predecessor_ops_metadata_hash,
        parents_contexts ) =
    {
      author;
      message;
      test_chain_status;
      data_merkle_root;
      predecessor_block_metadata_hash;
      predecessor_ops_metadata_hash;
      parents_contexts;
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
           } ->
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
             parents_contexts ) ->
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
         (req "data_merkle_root" Tezos_crypto.Context_hash.encoding)
         (opt
            "predecessor_block_metadata_hash"
            Tezos_crypto.Block_metadata_hash.encoding)
         (opt
            "predecessor_ops_metadata_hash"
            Tezos_crypto.Operation_metadata_list_list_hash.encoding)
         (req "parents_contexts" (list Tezos_crypto.Context_hash.encoding)))

  type activation_block = {
    block : block_descriptor;
    protocol : Tezos_crypto.Protocol_hash.t;
    commit_info : commit_info option;
  }

  let activation_block_encoding =
    let open Data_encoding in
    conv
      (fun {block; protocol; commit_info} -> (block, protocol, commit_info))
      (fun (block, protocol, commit_info) -> {block; protocol; commit_info})
      (obj3
         (req "block" block_descriptor_encoding)
         (req "protocol" Tezos_crypto.Protocol_hash.encoding)
         (opt "commit_info" commit_info_encoding))

  let encoding =
    Data_encoding.conv
      (fun map -> bindings map)
      (fun bindings ->
        List.fold_left (fun map (k, v) -> add k v map) empty bindings)
      Data_encoding.(list (tup2 uint8 activation_block_encoding))
end
