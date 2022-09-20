(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
include Store_utils

(** Aggregated collection of messages from the L1 inbox *)
open Alpha_context

type state_info = {
  num_messages : Z.t;
  num_ticks : Z.t;
  initial_tick : Sc_rollup.Tick.t;
}

(** Extraneous state information for the PVM *)
module StateInfo = Make_append_only_map (struct
  let path = ["state_info"]

  let keep_last_n_entries_in_memory = 6000

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = state_info

  let value_encoding =
    let open Data_encoding in
    conv
      (fun {num_messages; num_ticks; initial_tick} ->
        (num_messages, num_ticks, initial_tick))
      (fun (num_messages, num_ticks, initial_tick) ->
        {num_messages; num_ticks; initial_tick})
      (obj3
         (req "num_messages" Data_encoding.z)
         (req "num_ticks" Data_encoding.z)
         (req "initial_tick" Sc_rollup.Tick.encoding))
end)

module StateHistoryRepr = struct
  let path = ["state_history"]

  type event = {
    tick : Sc_rollup.Tick.t;
    block_hash : Block_hash.t;
    predecessor_hash : Block_hash.t;
    level : Raw_level.t;
  }

  module TickMap = Map.Make (Sc_rollup.Tick)

  type value = event TickMap.t

  let event_encoding =
    let open Data_encoding in
    conv
      (fun {tick; block_hash; predecessor_hash; level} ->
        (tick, block_hash, predecessor_hash, level))
      (fun (tick, block_hash, predecessor_hash, level) ->
        {tick; block_hash; predecessor_hash; level})
      (obj4
         (req "tick" Sc_rollup.Tick.encoding)
         (req "block_hash" Block_hash.encoding)
         (req "predecessor_hash" Block_hash.encoding)
         (req "level" Raw_level.encoding))

  let value_encoding =
    let open Data_encoding in
    conv
      TickMap.bindings
      (fun bindings -> TickMap.of_seq (List.to_seq bindings))
      (Data_encoding.list (tup2 Sc_rollup.Tick.encoding event_encoding))
end

module StateHistory = struct
  include Make_mutable_value (StateHistoryRepr)

  let insert store event =
    let open Lwt_result_syntax in
    let open StateHistoryRepr in
    let*! history = find store in
    let history =
      match history with
      | None -> StateHistoryRepr.TickMap.empty
      | Some history -> history
    in
    set store (TickMap.add event.tick event history)

  let event_of_largest_tick_before store tick =
    let open Lwt_result_syntax in
    let open StateHistoryRepr in
    let*! history = find store in
    match history with
    | None -> return_none
    | Some history -> (
        let events_before, opt_value, _ = TickMap.split tick history in
        match opt_value with
        | Some event -> return (Some event)
        | None ->
            return @@ Option.map snd @@ TickMap.max_binding_opt events_before)
end

(** Unaggregated messages per block *)
module Messages = Make_append_only_map (struct
  let path = ["messages"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Sc_rollup.Inbox_message.t list

  let value_encoding =
    Data_encoding.(list @@ dynamic_size Sc_rollup.Inbox_message.encoding)
end)

(** Inbox state for each block *)
module Inboxes = Make_append_only_map (struct
  let path = ["inboxes"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Sc_rollup.Inbox.t

  let value_encoding = Sc_rollup.Inbox.encoding
end)

(** Message history for the inbox at a given block *)
module Histories = Make_append_only_map (struct
  let path = ["histories"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Sc_rollup.Inbox.History.t

  let value_encoding = Sc_rollup.Inbox.History.encoding
end)

module Commitments = Make_append_only_map (struct
  let path = ["commitments"; "computed"]

  let keep_last_n_entries_in_memory = 10

  type key = Raw_level.t

  let string_of_key l = Int32.to_string @@ Raw_level.to_int32 l

  type value = Sc_rollup.Commitment.t * Sc_rollup.Commitment.Hash.t

  let value_encoding =
    Data_encoding.(
      obj2
        (req "commitment" Sc_rollup.Commitment.encoding)
        (req "hash" Sc_rollup.Commitment.Hash.encoding))
end)

module Last_stored_commitment_level = Make_mutable_value (struct
  let path = ["commitments"; "last_stored_level"]

  type value = Raw_level.t

  let value_encoding = Raw_level.encoding
end)

module Last_published_commitment_level = Make_mutable_value (struct
  let path = ["commitments"; "last_published_level"]

  type value = Raw_level.t

  let value_encoding = Raw_level.encoding
end)

module Last_cemented_commitment_level = Make_mutable_value (struct
  let path = ["commitments"; "last_cemented_commitment"; "level"]

  type value = Raw_level.t

  let value_encoding = Raw_level.encoding
end)

module Last_cemented_commitment_hash = Make_mutable_value (struct
  let path = ["commitments"; "last_cemented_commitment"; "hash"]

  type value = Sc_rollup.Commitment.Hash.t

  let value_encoding = Sc_rollup.Commitment.Hash.encoding
end)

module Commitments_published_at_level = Make_updatable_map (struct
  let path = ["commitments"; "published_at_level"]

  let keep_last_n_entries_in_memory = 10

  type key = Sc_rollup.Commitment.Hash.t

  let string_of_key = Sc_rollup.Commitment.Hash.to_b58check

  type value = Raw_level.t

  let value_encoding = Raw_level.encoding
end)

(* Slot subscriptions per block hash, saved as a list of
   `Dal.Slot_index.t`, which is a bounded integer between `0` and `255`
   included. *)
module Dal_slot_subscriptions = Make_append_only_map (struct
  let path = ["dal"; "slot_subscriptions"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Dal.Slot_index.t list

  let value_encoding = Data_encoding.list Dal.Slot_index.encoding
end)

module Contexts = Make_append_only_map (struct
  let path = ["contexts"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Context.hash

  let value_encoding = Context.hash_encoding
end)

module Block_slot_map_parameter = struct
  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type secondary_key = Dal.Slot_index.t

  let compare_secondary_keys = Dal.Slot_index.compare

  type value = Dal.Slot.t

  let secondary_key_encoding = Dal.Slot_index.encoding

  let secondary_key_name = "slot_index"

  let value_encoding = Dal.Slot.encoding

  let value_name = "slots_metadata"
end

(* Published slot headers per block hash,
   stored as a list of bindings from `Dal_slot_index.t`
   to `Dal.Slot.t`. The encoding function converts this
   list into a `Dal.Slot_index.t`-indexed map. *)
module Dal_slots = Make_nested_map (struct
  let path = ["dal"; "slot_headers"]

  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3527
     This value is currently not used, but required by the module type. *)
  let keep_last_n_entries_in_memory = 10

  include Block_slot_map_parameter
end)

(* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3515
   temporary - this is used for test purposes only.
   Remove this piece of storage once we download blocks from the dal node.
*)
(* Published slot headers per block hash, stored as a list of bindings from
   `Dal_slot_index.t` to `Dal.Slot.t`. The encoding function converts this
   list into a `Dal.Slot_index.t`-indexed map. Note that the block_hash
   refers to the block where slots headers have been confirmed, not
   the block where they have been published.
*)
module Dal_confirmed_slots = Make_nested_map (struct
  let path = ["dal"; "confirmed_slots"]

  (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3527
     This value is currently not used, but required by the module type. *)
  let keep_last_n_entries_in_memory = 10

  include Block_slot_map_parameter
end)

(** Confirmed DAL slots history. See documentation of
    {Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_history = Make_append_only_map (struct
  let path = ["dal"; "confirmed_slots_history"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Dal.Slots_history.t

  let value_encoding = Dal.Slots_history.encoding
end)

(** Confirmed DAL slots histories cache. See documentation of
    {Dal_slot_repr.Slots_history} for more details. *)
module Dal_confirmed_slots_histories = Make_append_only_map (struct
  let path = ["dal"; "confirmed_slots_histories_cache"]

  let keep_last_n_entries_in_memory = 10

  type key = Block_hash.t

  let string_of_key = Block_hash.to_b58check

  type value = Dal.Slots_history.History_cache.t

  let value_encoding = Dal.Slots_history.History_cache.encoding
end)
