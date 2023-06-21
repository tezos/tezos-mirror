(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** This module is a copy of
    [src/proto_016_PtMumbai/lib_sc_rollup_node/store.ml], which contains the
    store for the Mumbai rollup node. *)

include Store_sigs
include Store_utils

let version = Store_version.V0

module Irmin_store = struct
  module IStore = Irmin_store.Make (struct
    let name = "Tezos smart rollup node"
  end)

  include IStore
  include Store_utils.Make (IStore)
end

module Empty_header = struct
  type t = unit

  let name = "empty"

  let encoding = Data_encoding.unit

  let fixed_size = 0
end

module Add_empty_header = struct
  module Header = Empty_header

  let header _ = ()
end

module Make_hash_index_key (H : Tezos_crypto.Intfs.HASH) =
Indexed_store.Make_index_key (struct
  include Indexed_store.Make_fixed_encodable (H)

  let equal = H.equal
end)

(** L2 blocks *)
module L2_blocks =
  Indexed_store.Make_indexed_file
    (struct
      let name = "l2_blocks"
    end)
    (Tezos_store_shared.Block_key)
    (struct
      type t = (unit, unit) Sc_rollup_block.block

      let name = "sc_rollup_block_info"

      let encoding =
        Sc_rollup_block.block_encoding Data_encoding.unit Data_encoding.unit

      module Header = struct
        type t = Sc_rollup_block.header

        let name = "sc_rollup_block_header"

        let encoding = Sc_rollup_block.header_encoding

        let fixed_size = Sc_rollup_block.header_size
      end
    end)

(** Unaggregated messages per block *)
module Messages =
  Indexed_store.Make_indexed_file
    (struct
      let name = "messages"
    end)
    (Make_hash_index_key (Octez_smart_rollup.Merkelized_payload_hashes_hash))
    (struct
      type t = string list

      let name = "messages_list"

      let encoding = Data_encoding.(list @@ dynamic_size (Variable.string' Hex))

      module Header = struct
        type t = Block_hash.t * Time.Protocol.t * int

        let name = "messages_inbox_info"

        let encoding =
          let open Data_encoding in
          obj3
            (req "predecessor" Block_hash.encoding)
            (req "predecessor_timestamp" Time.Protocol.encoding)
            (req "num_messages" int31)

        let fixed_size =
          WithExceptions.Option.get ~loc:__LOC__
          @@ Data_encoding.Binary.fixed_length encoding
      end
    end)

(** Inbox state for each block *)
module Inboxes =
  Indexed_store.Make_simple_indexed_file
    (struct
      let name = "inboxes"
    end)
    (Make_hash_index_key (Octez_smart_rollup.Inbox_hash))
    (struct
      type t = Octez_smart_rollup.Inbox.V1.t

      let name = "inbox"

      let encoding = Octez_smart_rollup.Inbox.V1.encoding

      include Add_empty_header
    end)

module Commitments =
  Indexed_store.Make_indexable
    (struct
      let name = "commitments"
    end)
    (Make_hash_index_key (Octez_smart_rollup.Commitment.Hash))
    (Indexed_store.Make_index_value (Indexed_store.Make_fixed_encodable (struct
      include Octez_smart_rollup.Commitment.V1

      let name = "commitment"
    end)))

module Commitments_published_at_level = struct
  type element = {
    first_published_at_level : int32;
    published_at_level : int32 option;
  }

  let element_encoding =
    let open Data_encoding in
    let opt_level_encoding =
      conv
        (function None -> -1l | Some l -> l)
        (fun l -> if l = -1l then None else Some l)
        int32
    in
    conv
      (fun {first_published_at_level; published_at_level} ->
        (first_published_at_level, published_at_level))
      (fun (first_published_at_level, published_at_level) ->
        {first_published_at_level; published_at_level})
    @@ obj2
         (req "first_published_at_level" int32)
         (req "published_at_level" opt_level_encoding)

  include
    Indexed_store.Make_indexable
      (struct
        let name = "commitments"
      end)
      (Make_hash_index_key (Octez_smart_rollup.Commitment.Hash))
      (Indexed_store.Make_index_value (Indexed_store.Make_fixed_encodable (struct
        type t = element

        let name = "published_levels"

        let encoding = element_encoding
      end)))
end

module L2_head = Indexed_store.Make_singleton (struct
  type t = Sc_rollup_block.t

  let name = "l2_head"

  let encoding = Sc_rollup_block.encoding
end)

module Last_finalized_level = Indexed_store.Make_singleton (struct
  type t = int32

  let name = "finalized_level"

  let encoding = Data_encoding.int32
end)

(** Table from L1 levels to blocks hashes. *)
module Levels_to_hashes =
  Indexed_store.Make_indexable
    (struct
      let name = "tezos_levels"
    end)
    (Indexed_store.Make_index_key (struct
      type t = int32

      let encoding = Data_encoding.int32

      let name = "level"

      let fixed_size = 4

      let equal = Int32.equal
    end))
    (Tezos_store_shared.Block_key)

(* Published slot headers per block hash,
   stored as a list of bindings from `Dal_slot_index.t`
   to `Dal.Slot.t`. The encoding function converts this
   list into a `Dal.Slot_index.t`-indexed map. *)
module Dal_slot_pages =
  Irmin_store.Make_nested_map
    (struct
      let path = ["dal"; "slot_pages"]
    end)
    (struct
      type key = Block_hash.t

      let to_path_representation = Block_hash.to_b58check
    end)
    (struct
      type key =
        Octez_smart_rollup.Dal.Slot_index.t
        * Octez_smart_rollup.Dal.Page_index.t

      let encoding =
        Data_encoding.(tup2 Dal.Slot_index.encoding Dal.Page_index.encoding)

      let compare = Stdlib.compare

      let name = "slot_index"
    end)
    (struct
      type value = bytes

      let encoding = Data_encoding.(bytes' Hex)

      let name = "slot_pages"
    end)

(** stores slots whose data have been considered and pages stored to disk (if
    they are confirmed). *)
module Dal_processed_slots =
  Irmin_store.Make_nested_map
    (struct
      let path = ["dal"; "processed_slots"]
    end)
    (struct
      type key = Block_hash.t

      let to_path_representation = Block_hash.to_b58check
    end)
    (struct
      type key = Octez_smart_rollup.Dal.Slot_index.t

      let encoding = Octez_smart_rollup.Dal.Slot_index.encoding

      let compare = Compare.Int.compare

      let name = "slot_index"
    end)
    (struct
      type value = [`Confirmed | `Unconfirmed]

      let name = "slot_processing_status"

      let encoding =
        let open Data_encoding in
        let mk_case constr ~tag ~title =
          case
            ~title
            (Tag tag)
            (obj1 (req "kind" (constant title)))
            (fun x -> if x = constr then Some () else None)
            (fun () -> constr)
        in
        union
          ~tag_size:`Uint8
          [
            mk_case `Confirmed ~tag:0 ~title:"Confirmed";
            mk_case `Unconfirmed ~tag:1 ~title:"Unconfirmed";
          ]
    end)

module Dal_slots_headers =
  Irmin_store.Make_nested_map
    (struct
      let path = ["dal"; "slot_headers"]
    end)
    (struct
      type key = Block_hash.t

      let to_path_representation = Block_hash.to_b58check
    end)
    (struct
      type key = Octez_smart_rollup.Dal.Slot_index.t

      let encoding = Octez_smart_rollup.Dal.Slot_index.encoding

      let compare = Compare.Int.compare

      let name = "slot_index"
    end)
    (struct
      type value = Octez_smart_rollup.Dal.Slot_header.t

      let name = "slot_header"

      let encoding = Octez_smart_rollup.Dal.Slot_header.V1.encoding
    end)

(* Published slot headers per block hash, stored as a list of bindings from
   `Dal_slot_index.t` to `Dal.Slot.t`. The encoding function converts this
   list into a `Dal.Slot_index.t`-indexed map. Note that the block_hash
   refers to the block where slots headers have been confirmed, not
   the block where they have been published.
*)

(** Confirmed DAL slots history. See documentation of
    {!Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_history =
  Irmin_store.Make_append_only_map
    (struct
      let path = ["dal"; "confirmed_slots_history"]
    end)
    (struct
      type key = Block_hash.t

      let to_path_representation = Block_hash.to_b58check
    end)
    (struct
      type value = Octez_smart_rollup.Dal.Slot_history.t

      let name = "dal_slot_histories"

      let encoding = Octez_smart_rollup.Dal.Slot_history.V1.encoding
    end)

(** Confirmed DAL slots histories cache. See documentation of
    {!Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_histories =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4390
     Store single history points in map instead of whole history. *)
    Irmin_store.Make_append_only_map
      (struct
        let path = ["dal"; "confirmed_slots_histories_cache"]
      end)
      (struct
        type key = Block_hash.t

        let to_path_representation = Block_hash.to_b58check
      end)
    (struct
      type value = Octez_smart_rollup.Dal.Slot_history_cache.t

      let name = "dal_slot_histories"

      let encoding = Octez_smart_rollup.Dal.Slot_history_cache.V1.encoding
    end)

type 'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  irmin_store : 'a Irmin_store.t;
}

type 'a t = ([< `Read | `Write > `Read] as 'a) store

type rw = Store_sigs.rw t

type ro = Store_sigs.ro t

let readonly
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       l2_head;
       last_finalized_level;
       levels_to_hashes;
       irmin_store;
     } :
      _ t) : ro =
  {
    l2_blocks = L2_blocks.readonly l2_blocks;
    messages = Messages.readonly messages;
    inboxes = Inboxes.readonly inboxes;
    commitments = Commitments.readonly commitments;
    commitments_published_at_level =
      Commitments_published_at_level.readonly commitments_published_at_level;
    l2_head = L2_head.readonly l2_head;
    last_finalized_level = Last_finalized_level.readonly last_finalized_level;
    levels_to_hashes = Levels_to_hashes.readonly levels_to_hashes;
    irmin_store = Irmin_store.readonly irmin_store;
  }

let close
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       l2_head = _;
       last_finalized_level = _;
       levels_to_hashes;
       irmin_store;
     } :
      _ t) =
  let open Lwt_result_syntax in
  let+ () = L2_blocks.close l2_blocks
  and+ () = Messages.close messages
  and+ () = Inboxes.close inboxes
  and+ () = Commitments.close commitments
  and+ () = Commitments_published_at_level.close commitments_published_at_level
  and+ () = Levels_to_hashes.close levels_to_hashes
  and+ () = Irmin_store.close irmin_store in
  ()

let load (type a) (mode : a mode) ~l2_blocks_cache_size data_dir :
    a store tzresult Lwt.t =
  let open Lwt_result_syntax in
  let path name = Filename.concat data_dir name in
  let cache_size = l2_blocks_cache_size in
  let* l2_blocks = L2_blocks.load mode ~path:(path "l2_blocks") ~cache_size in
  let* messages = Messages.load mode ~path:(path "messages") ~cache_size in
  let* inboxes = Inboxes.load mode ~path:(path "inboxes") ~cache_size in
  let* commitments = Commitments.load mode ~path:(path "commitments") in
  let* commitments_published_at_level =
    Commitments_published_at_level.load
      mode
      ~path:(path "commitments_published_at_level")
  in
  let* l2_head = L2_head.load mode ~path:(path "l2_head") in
  let* last_finalized_level =
    Last_finalized_level.load mode ~path:(path "last_finalized_level")
  in
  let* levels_to_hashes =
    Levels_to_hashes.load mode ~path:(path "levels_to_hashes")
  in
  let+ irmin_store = Irmin_store.load mode (path "irmin_store") in
  {
    l2_blocks;
    messages;
    inboxes;
    commitments;
    commitments_published_at_level;
    l2_head;
    last_finalized_level;
    levels_to_hashes;
    irmin_store;
  }

let iter_l2_blocks ({l2_blocks; l2_head; _} : _ t) f =
  let open Lwt_result_syntax in
  let* head = L2_head.read l2_head in
  match head with
  | None ->
      (* No reachable head, nothing to do *)
      return_unit
  | Some head ->
      let rec loop hash =
        let* block = L2_blocks.read l2_blocks hash in
        match block with
        | None ->
            (* The block does not exist, the known chain stops here, so do we. *)
            return_unit
        | Some (block, header) ->
            let* () = f {block with header} in
            loop header.predecessor
      in
      loop head.header.block_hash
