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

include Store_sigs
include Store_utils
include Store_v1

let version = Store_version.V2

module Make_hash_index_key (H : Tezos_crypto.Intfs.HASH) =
Indexed_store.Make_index_key (struct
  include Indexed_store.Make_fixed_encodable (H)

  let equal = H.equal
end)

(** Unaggregated messages per block *)
module Messages =
  Indexed_store.Make_indexed_file
    (struct
      let name = "messages"
    end)
    (Make_hash_index_key (Merkelized_payload_hashes_hash))
    (struct
      type t = string list

      let name = "messages_list"

      let encoding = Data_encoding.(list @@ dynamic_size (Variable.string' Hex))

      module Header = struct
        type t = Block_hash.t

        let name = "messages_block"

        let encoding = Block_hash.encoding

        let fixed_size =
          WithExceptions.Option.get ~loc:__LOC__
          @@ Data_encoding.Binary.fixed_length encoding
      end
    end)

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

(** Versioned inboxes *)
module Inboxes =
  Indexed_store.Make_simple_indexed_file
    (struct
      let name = "inboxes"
    end)
    (Make_hash_index_key (Octez_smart_rollup.Inbox.Hash))
    (struct
      type t = Octez_smart_rollup.Inbox.t

      let encoding =
        Data_encoding.conv
          Octez_smart_rollup.Inbox.to_versioned
          Octez_smart_rollup.Inbox.of_versioned
          Octez_smart_rollup.Inbox.versioned_encoding

      let name = "inbox"

      include Add_empty_header
    end)

(** Versioned commitments *)
module Commitments =
  Indexed_store.Make_simple_indexed_file
    (struct
      let name = "commitments"
    end)
    (Make_hash_index_key (Octez_smart_rollup.Commitment.Hash))
    (struct
      type t = Octez_smart_rollup.Commitment.t

      let encoding =
        Data_encoding.conv
          Octez_smart_rollup.Commitment.to_versioned
          Octez_smart_rollup.Commitment.of_versioned
          Octez_smart_rollup.Commitment.versioned_encoding

      let name = "commitment"

      include Add_empty_header
    end)

(** Single commitment for LCC. *)
module Lcc = struct
  type lcc = {commitment : Commitment.Hash.t; level : int32}

  include Indexed_store.Make_singleton (struct
    type t = lcc

    let name = "lcc"

    let encoding =
      let open Data_encoding in
      conv
        (fun {commitment; level} -> (commitment, level))
        (fun (commitment, level) -> {commitment; level})
      @@ obj2
           (req "commitment" Octez_smart_rollup.Commitment.Hash.encoding)
           (req "level" int32)
  end)
end

(** Single commitment for LPC. *)
module Lpc = Indexed_store.Make_singleton (struct
  type t = Octez_smart_rollup.Commitment.t

  let encoding =
    Data_encoding.conv
      Octez_smart_rollup.Commitment.to_versioned
      Octez_smart_rollup.Commitment.of_versioned
      Octez_smart_rollup.Commitment.versioned_encoding

  let name = "lpc"
end)

(** Versioned slot headers *)
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

      let encoding =
        Data_encoding.conv
          Octez_smart_rollup.Dal.Slot_header.to_versioned
          Octez_smart_rollup.Dal.Slot_header.of_versioned
          Octez_smart_rollup.Dal.Slot_header.versioned_encoding
    end)

(** Versioned Confirmed DAL slots history *)
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

      let encoding =
        Data_encoding.conv
          Octez_smart_rollup.Dal.Slot_history.to_versioned
          Octez_smart_rollup.Dal.Slot_history.of_versioned
          Octez_smart_rollup.Dal.Slot_history.versioned_encoding
    end)

(** Versioned Confirmed DAL slots histories cache. *)
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

      let encoding =
        Data_encoding.conv
          Octez_smart_rollup.Dal.Slot_history_cache.to_versioned
          Octez_smart_rollup.Dal.Slot_history_cache.of_versioned
          Octez_smart_rollup.Dal.Slot_history_cache.versioned_encoding
    end)

module Protocols = struct
  type level = First_known of int32 | Activation_level of int32

  type proto_info = {
    level : level;
    proto_level : int;
    protocol : Protocol_hash.t;
  }

  type value = proto_info list

  let level_encoding =
    let open Data_encoding in
    conv
      (function First_known l -> (l, false) | Activation_level l -> (l, true))
      (function l, false -> First_known l | l, true -> Activation_level l)
    @@ obj2 (req "level" int32) (req "activates" bool)

  let proto_info_encoding =
    let open Data_encoding in
    conv
      (fun {level; proto_level; protocol} -> (level, proto_level, protocol))
      (fun (level, proto_level, protocol) -> {level; proto_level; protocol})
    @@ obj3
         (req "level" level_encoding)
         (req "proto_level" int31)
         (req "protocol" Protocol_hash.encoding)

  include Indexed_store.Make_singleton (struct
    type t = value

    let name = "protocols"

    let level_encoding =
      let open Data_encoding in
      conv
        (function
          | First_known l -> (l, false) | Activation_level l -> (l, true))
        (function l, false -> First_known l | l, true -> Activation_level l)
      @@ obj2 (req "level" int32) (req "activates" bool)

    let proto_info_encoding =
      let open Data_encoding in
      conv
        (fun {level; proto_level; protocol} -> (level, proto_level, protocol))
        (fun (level, proto_level, protocol) -> {level; proto_level; protocol})
      @@ obj3
           (req "level" level_encoding)
           (req "proto_level" int31)
           (req "protocol" Protocol_hash.encoding)

    let encoding = Data_encoding.list proto_info_encoding
  end)
end

module Gc_levels = struct
  type levels = {last_gc_level : int32; first_available_level : int32}

  type value = levels

  include Indexed_store.Make_singleton (struct
    type t = levels

    let name = "gc_levels"

    let encoding : t Data_encoding.t =
      let open Data_encoding in
      conv
        (fun {last_gc_level; first_available_level} ->
          (last_gc_level, first_available_level))
        (fun (last_gc_level, first_available_level) ->
          {last_gc_level; first_available_level})
      @@ obj2 (req "last_gc_level" int32) (req "first_available_level" int32)
  end)
end

module History_mode = Indexed_store.Make_singleton (struct
  type t = Configuration.history_mode

  let name = "history_mode"

  let encoding = Configuration.history_mode_encoding
end)

type 'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  lcc : 'a Lcc.t;
  lpc : 'a Lpc.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  protocols : 'a Protocols.t;
  irmin_store : 'a Irmin_store.t;
  gc_levels : 'a Gc_levels.t;
  history_mode : 'a History_mode.t;
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
       lcc;
       lpc;
       levels_to_hashes;
       protocols;
       irmin_store;
       gc_levels;
       history_mode;
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
    lcc = Lcc.readonly lcc;
    lpc = Lpc.readonly lpc;
    levels_to_hashes = Levels_to_hashes.readonly levels_to_hashes;
    protocols = Protocols.readonly protocols;
    irmin_store = Irmin_store.readonly irmin_store;
    gc_levels = Gc_levels.readonly gc_levels;
    history_mode = History_mode.readonly history_mode;
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
       lcc = _;
       lpc = _;
       levels_to_hashes;
       protocols = _;
       irmin_store;
       gc_levels = _;
       history_mode = _;
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

let load (type a) (mode : a mode) ~index_buffer_size ~l2_blocks_cache_size
    data_dir : a store tzresult Lwt.t =
  let open Lwt_result_syntax in
  let path name = Filename.concat data_dir name in
  let cache_size = l2_blocks_cache_size in
  let* l2_blocks =
    L2_blocks.load mode ~index_buffer_size ~path:(path "l2_blocks") ~cache_size
  in
  let* messages =
    Messages.load mode ~index_buffer_size ~path:(path "messages") ~cache_size
  in
  let* inboxes =
    Inboxes.load mode ~index_buffer_size ~path:(path "inboxes") ~cache_size
  in
  let* commitments =
    Commitments.load
      mode
      ~index_buffer_size
      ~path:(path "commitments")
      ~cache_size
  in
  let* commitments_published_at_level =
    Commitments_published_at_level.load
      ~index_buffer_size
      mode
      ~path:(path "commitments_published_at_level")
  in
  let* l2_head = L2_head.load mode ~path:(path "l2_head") in
  let* last_finalized_level =
    Last_finalized_level.load mode ~path:(path "last_finalized_level")
  in
  let* lcc = Lcc.load mode ~path:(path "lcc") in
  let* lpc = Lpc.load mode ~path:(path "lpc") in
  let* levels_to_hashes =
    Levels_to_hashes.load
      mode
      ~index_buffer_size
      ~path:(path "levels_to_hashes")
  in
  let* protocols = Protocols.load mode ~path:(path "protocols") in
  let* gc_levels = Gc_levels.load mode ~path:(path "gc_levels") in
  let* history_mode = History_mode.load mode ~path:(path "history_mode") in
  let+ irmin_store = Irmin_store.load mode (path "irmin_store") in
  {
    l2_blocks;
    messages;
    inboxes;
    commitments;
    commitments_published_at_level;
    l2_head;
    last_finalized_level;
    lcc;
    lpc;
    levels_to_hashes;
    protocols;
    irmin_store;
    gc_levels;
    history_mode;
  }

let first_available_level metadata store =
  let open Lwt_result_syntax in
  let* gc_levels = Gc_levels.read store.gc_levels in
  match gc_levels with
  | Some {first_available_level; _} -> return first_available_level
  | None -> return metadata.Metadata.genesis_info.level

let iter_l2_blocks ?progress metadata ({l2_blocks; l2_head; _} as store) f =
  let open Lwt_result_syntax in
  let* head = L2_head.read l2_head in
  match head with
  | None ->
      (* No reachable head, nothing to do *)
      return_unit
  | Some head ->
      let* track_progress =
        match progress with
        | None -> return (fun f -> f (fun _ -> Lwt.return_unit))
        | Some message ->
            let+ first_level = first_available_level metadata store in
            let progress_bar =
              let total =
                Int32.sub head.header.level first_level |> Int32.to_int
              in
              Progress_bar.progress_bar ~counter:`Int ~message total
            in
            fun f -> Progress_bar.Lwt.with_reporter progress_bar f
      in
      track_progress @@ fun count_progress ->
      let rec loop hash =
        let* block = L2_blocks.read l2_blocks hash in
        match block with
        | None ->
            (* The block does not exist, the known chain stops here, so do we. *)
            return_unit
        | Some (block, header) ->
            let* () = f {block with header} in
            let*! () = count_progress 1 in
            loop header.predecessor
      in
      loop head.header.block_hash

let gc_l2_blocks l2_blocks ~(head : Sc_rollup_block.t) ~level =
  L2_blocks.gc
    l2_blocks
    (Indexed_store.Iterator
       {
         first = head.header.block_hash;
         next =
           (fun _hash (_content, header) ->
             if header.Sc_rollup_block.level <= level then Lwt.return_none
             else Lwt.return_some header.predecessor);
       })

let gc_commitments commitments ~last_commitment ~level =
  Commitments.gc
    commitments
    (Indexed_store.Iterator
       {
         first = last_commitment;
         next =
           (fun _hash (commitment, ()) ->
             if commitment.Commitment.inbox_level <= level then Lwt.return_none
             else Lwt.return_some commitment.predecessor);
       })

let gc_levels_to_hashes levels_to_hashes ~(head : Sc_rollup_block.t) ~level =
  Levels_to_hashes.gc
    levels_to_hashes
    (Indexed_store.Iterator
       {
         first = head.header.level;
         next =
           (fun blevel _bhash ->
             if blevel <= level then Lwt.return_none
             else Lwt.return_some (Int32.pred blevel));
       })

let gc_messages messages l2_blocks ~(head : Sc_rollup_block.t) ~level =
  Messages.gc
    messages
    (Indexed_store.Iterator
       {
         first = head.header.inbox_witness;
         next =
           (fun _witness (_msgs, predecessor) ->
             let open Lwt_syntax in
             let* pred_inbox_witness =
               let open Lwt_result_syntax in
               let+ pred = L2_blocks.header l2_blocks predecessor in
               match pred with
               | Some {level = pred_level; inbox_witness; _}
                 when pred_level >= level ->
                   Some inbox_witness
               | _ -> None
             in
             match pred_inbox_witness with
             | Error e ->
                 Fmt.failwith
                   "Could not compute messages witness for GC: %a"
                   pp_print_trace
                   e
             | Ok witness -> return witness);
       })

let gc_commitments_published_at_level commitments_published_at_level commitments
    lpc ~level =
  let open Lwt_result_syntax in
  let* lpc = Lpc.read lpc in
  match lpc with
  | None -> return_unit
  | Some lpc ->
      Commitments_published_at_level.gc
        commitments_published_at_level
        (Indexed_store.Iterator
           {
             first = Commitment.hash lpc;
             next =
               (fun commitment_hash _ ->
                 let open Lwt_syntax in
                 let* commitment =
                   Commitments.read commitments commitment_hash
                 in
                 match commitment with
                 | Error e ->
                     Fmt.failwith
                       "Could not compute commitment published at level for \
                        GC: %a"
                       pp_print_trace
                       e
                 | Ok None -> return_none
                 | Ok (Some (commitment, ())) ->
                     if commitment.Commitment.inbox_level <= level then
                       return_none
                     else return_some commitment.predecessor);
           })

let gc_inboxes inboxes ~(head : Sc_rollup_block.t) ~level =
  Inboxes.gc
    inboxes
    (Indexed_store.Iterator
       {
         first = head.header.inbox_hash;
         next =
           (fun _inbox_hash (inbox, ()) ->
             let open Lwt_syntax in
             if inbox.level <= level then return_none
             else
               return (Inbox.Skip_list.back_pointer inbox.old_levels_messages 0));
       })

let gc
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       l2_head;
       last_finalized_level = _;
       lcc = _;
       lpc;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       history_mode = _;
     } :
      _ t) ~level =
  let open Lwt_result_syntax in
  let* head = L2_head.read l2_head in
  match head with
  | None -> return_unit
  | Some head ->
      let last_commitment =
        Sc_rollup_block.most_recent_commitment head.header
      in
      let* () =
        tzjoin
          [
            gc_l2_blocks l2_blocks ~head ~level;
            gc_commitments commitments ~last_commitment ~level;
            gc_levels_to_hashes levels_to_hashes ~head ~level;
            gc_messages messages l2_blocks ~head ~level;
            gc_commitments_published_at_level
              commitments_published_at_level
              commitments
              lpc
              ~level;
            gc_inboxes inboxes ~head ~level;
          ]
      in
      return_unit

let wait_gc_completion
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       history_mode = _;
     } :
      _ t) =
  let open Lwt_syntax in
  let* () = L2_blocks.wait_gc_completion l2_blocks
  and* () = Messages.wait_gc_completion messages
  and* () = Inboxes.wait_gc_completion inboxes
  and* () = Commitments.wait_gc_completion commitments
  and* () =
    Commitments_published_at_level.wait_gc_completion
      commitments_published_at_level
  and* () = Levels_to_hashes.wait_gc_completion levels_to_hashes in
  return_unit

let is_gc_finished
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       history_mode = _;
     } :
      _ t) =
  L2_blocks.is_gc_finished l2_blocks
  && Messages.is_gc_finished messages
  && Inboxes.is_gc_finished inboxes
  && Commitments.is_gc_finished commitments
  && Commitments_published_at_level.is_gc_finished
       commitments_published_at_level
  && Levels_to_hashes.is_gc_finished levels_to_hashes
