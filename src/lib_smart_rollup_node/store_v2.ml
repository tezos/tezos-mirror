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

module Outbox_messages = struct
  type messages_per_level = {messages : Bitset.t; executed_messages : Bitset.t}

  module H = Hashtbl.Make (struct
    include Int32

    let hash = to_int
  end)

  type pending_messages = messages_per_level H.t

  (* Relatively small so we keep all in memory. *)
  include Indexed_store.Make_singleton (struct
    type t = pending_messages

    let name = "outbox_messages"

    let encoding =
      let open Data_encoding in
      conv
        (fun tbl -> H.to_seq tbl |> List.of_seq)
        (fun l -> List.to_seq l |> H.of_seq)
      @@ list
           (conv
              (fun (outbox_level, {messages; executed_messages}) ->
                (outbox_level, (messages, executed_messages)))
              (fun (outbox_level, (messages, executed_messages)) ->
                (outbox_level, {messages; executed_messages}))
           @@ merge_objs
                (obj1 (req "outbox_level" int32))
                (obj2
                   (req "messages" Bitset.encoding)
                   (req "executed_messages" Bitset.encoding)))
  end)

  let iter (t : _ t) f =
    let open Lwt_result_syntax in
    let* res = read t in
    match res with
    | None -> return_unit
    | Some h ->
        H.iter_es
          (fun outbox_level {messages; executed_messages} ->
            f
              ~outbox_level
              ~messages:(Bitset.to_list messages)
              ~executed_messages:(Bitset.to_list executed_messages))
          h

  let by_outbox_level (t : _ t) ~outbox_level =
    let open Lwt_result_syntax in
    let* res = read t in
    match res with
    | None -> return_nil
    | Some h -> (
        match H.find_opt h outbox_level with
        | None -> return_nil
        | Some {messages; executed_messages} ->
            let indexes = Bitset.to_list messages in
            List.map_e
              (fun i ->
                let open Result_syntax in
                let* exec = Bitset.mem executed_messages i in
                return (i, exec))
              indexes
            |> Lwt.return)

  let pending (t : _ t) ~min_level ~max_level =
    let open Lwt_result_syntax in
    let* res = read t in
    match res with
    | None -> return_nil
    | Some h ->
        H.fold
          (fun outbox_level {messages; executed_messages} acc ->
            if outbox_level < min_level || outbox_level > max_level then acc
            else
              let pending_at_level = Bitset.diff messages executed_messages in
              let l = Bitset.to_list pending_at_level in
              if List.is_empty l then acc else (outbox_level, l) :: acc)
          h
          []
        |> List.fast_sort (fun (o1, _) (o2, _) -> Int32.compare o1 o2)
        |> return

  let register_new_outbox_messages (t : _ t) ~outbox_level ~indexes =
    let open Lwt_result_syntax in
    let* res = read t in
    let h = match res with None -> H.create 97 | Some h -> h in
    let*? messages = Bitset.from_list indexes in
    H.replace h outbox_level {messages; executed_messages = Bitset.empty} ;
    write t h

  let register_missing_outbox_messages (t : _ t) ~outbox_level ~indexes =
    let open Lwt_result_syntax in
    let* res = read t in
    let h = match res with None -> H.create 97 | Some h -> h in
    let*? messages = Bitset.from_list indexes in
    if H.mem h outbox_level then return_unit
    else (
      H.replace h outbox_level {messages; executed_messages = Bitset.empty} ;
      write t h)

  let set_outbox_message_executed (t : _ t) ~outbox_level ~index =
    let open Lwt_result_syntax in
    let* res = read t in
    match res with
    | None ->
        (* Not tracking *)
        return_unit
    | Some h -> (
        match H.find h outbox_level with
        | None ->
            (* Not tracking *)
            return_unit
        | Some {messages; executed_messages} ->
            let*? executed_messages = Bitset.add executed_messages index in
            if Bitset.is_empty (Bitset.diff messages executed_messages) then
              (* Clean outbox level when all messages are executed. *)
              H.remove h outbox_level
            else H.replace h outbox_level {messages; executed_messages} ;
            write t h)
end

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

module Last_context_split = Indexed_store.Make_singleton (struct
  type t = int32

  let name = "last_context_split_level"

  let encoding = Data_encoding.int32
end)

type history_mode = Archive | Full

let history_mode_encoding : history_mode Data_encoding.t =
  Data_encoding.string_enum [("archive", Archive); ("full", Full)]

module History_mode = Indexed_store.Make_singleton (struct
  type t = history_mode

  let name = "history_mode"

  let encoding = history_mode_encoding
end)

type 'a store = {
  l2_blocks : 'a L2_blocks.t;
  messages : 'a Messages.t;
  inboxes : 'a Inboxes.t;
  commitments : 'a Commitments.t;
  commitments_published_at_level : 'a Commitments_published_at_level.t;
  outbox_messages : 'a Outbox_messages.t;
  l2_head : 'a L2_head.t;
  last_finalized_level : 'a Last_finalized_level.t;
  lcc : 'a Lcc.t;
  lpc : 'a Lpc.t;
  levels_to_hashes : 'a Levels_to_hashes.t;
  protocols : 'a Protocols.t;
  irmin_store : 'a Irmin_store.t;
  gc_levels : 'a Gc_levels.t;
  successful_gc_levels : 'a Gc_levels.t;
  last_context_split_level : 'a Last_context_split.t;
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
       outbox_messages;
       l2_head;
       last_finalized_level;
       lcc;
       lpc;
       levels_to_hashes;
       protocols;
       irmin_store;
       gc_levels;
       successful_gc_levels;
       last_context_split_level;
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
    outbox_messages = Outbox_messages.readonly outbox_messages;
    l2_head = L2_head.readonly l2_head;
    last_finalized_level = Last_finalized_level.readonly last_finalized_level;
    lcc = Lcc.readonly lcc;
    lpc = Lpc.readonly lpc;
    levels_to_hashes = Levels_to_hashes.readonly levels_to_hashes;
    protocols = Protocols.readonly protocols;
    irmin_store = Irmin_store.readonly irmin_store;
    gc_levels = Gc_levels.readonly gc_levels;
    successful_gc_levels = Gc_levels.readonly successful_gc_levels;
    last_context_split_level =
      Last_context_split.readonly last_context_split_level;
    history_mode = History_mode.readonly history_mode;
  }

let close
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       outbox_messages = _;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       protocols = _;
       irmin_store;
       gc_levels = _;
       successful_gc_levels = _;
       last_context_split_level = _;
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
  let* outbox_messages =
    Outbox_messages.load mode ~path:(path "outbox_messages")
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
  let* successful_gc_levels =
    Gc_levels.load mode ~path:(path "successful_gc_levels")
  in
  let* last_context_split_level =
    Last_context_split.load mode ~path:(path "last_context_split_level")
  in
  let* history_mode = History_mode.load mode ~path:(path "history_mode") in
  let+ irmin_store = Irmin_store.load mode (path "irmin_store") in
  {
    l2_blocks;
    messages;
    inboxes;
    commitments;
    commitments_published_at_level;
    outbox_messages;
    l2_head;
    last_finalized_level;
    lcc;
    lpc;
    levels_to_hashes;
    protocols;
    irmin_store;
    gc_levels;
    successful_gc_levels;
    last_context_split_level;
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

let gc_l2_blocks l2_blocks ~level =
  L2_blocks.gc l2_blocks (fun _hash header _content ->
      Lwt_result.return (header.Sc_rollup_block.level >= level))

let gc_commitments commitments ~level =
  Commitments.gc commitments (fun _hash () commitment ->
      Lwt_result.return (commitment.Commitment.inbox_level >= level))

let gc_levels_to_hashes levels_to_hashes ~level =
  Levels_to_hashes.gc levels_to_hashes (fun block_level _block_hash ->
      Lwt_result.return (block_level >= level))

let gc_messages messages l2_blocks ~level =
  Messages.gc messages (fun _witness predecessor _msgs ->
      let open Lwt_result_syntax in
      let+ pred = L2_blocks.header l2_blocks predecessor in
      match pred with
      | Some {level = pred_level; _} -> pred_level >= Int32.pred level
      | None -> false)

let gc_commitments_published_at_level commitments_published_at_level commitments
    ~level =
  Commitments_published_at_level.gc
    commitments_published_at_level
    (fun commitment_hash _ ->
      let open Lwt_result_syntax in
      let* commitment = Commitments.read commitments commitment_hash in
      match commitment with
      | None -> return_false
      | Some ({inbox_level; _}, ()) -> return (inbox_level >= level))

let gc_inboxes inboxes ~level =
  Inboxes.gc inboxes (fun _inbox_hash () inbox ->
      Lwt_result.return (inbox.level >= level))

let gc
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       outbox_messages = _;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       successful_gc_levels = _;
       last_context_split_level = _;
       history_mode = _;
     } :
      _ t) ~level =
  let open Lwt_result_syntax in
  tzjoin
    [
      gc_l2_blocks l2_blocks ~level;
      gc_commitments commitments ~level;
      gc_levels_to_hashes levels_to_hashes ~level;
      gc_messages messages l2_blocks ~level;
      gc_commitments_published_at_level
        commitments_published_at_level
        commitments
        ~level;
      gc_inboxes inboxes ~level;
    ]

let wait_gc_completion
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       outbox_messages = _;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       successful_gc_levels = _;
       last_context_split_level = _;
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
       outbox_messages = _;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       successful_gc_levels = _;
       last_context_split_level = _;
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

let cancel_gc
    ({
       l2_blocks;
       messages;
       inboxes;
       commitments;
       commitments_published_at_level;
       outbox_messages = _;
       l2_head = _;
       last_finalized_level = _;
       lcc = _;
       lpc = _;
       levels_to_hashes;
       irmin_store = _;
       protocols = _;
       gc_levels = _;
       successful_gc_levels = _;
       last_context_split_level = _;
       history_mode = _;
     } :
      _ t) =
  let open Lwt_syntax in
  let+ canceled =
    Lwt.all
      [
        L2_blocks.cancel_gc l2_blocks;
        Messages.cancel_gc messages;
        Inboxes.cancel_gc inboxes;
        Commitments.cancel_gc commitments;
        Commitments_published_at_level.cancel_gc commitments_published_at_level;
        Levels_to_hashes.cancel_gc levels_to_hashes;
      ]
  in
  List.exists Fun.id canceled
