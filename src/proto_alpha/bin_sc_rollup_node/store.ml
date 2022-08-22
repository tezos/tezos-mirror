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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3471
   Use indexed file for append-only instead of Irmin. *)

open Protocol
open Alpha_context
module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)

module IStore = struct
  include Maker.Make (Tezos_context_encoding.Context.Schema)
  module Schema = Tezos_context_encoding.Context.Schema
end

type t = IStore.t

type tree = IStore.tree

type path = string list

let load configuration =
  let open Lwt_syntax in
  let open Configuration in
  let* repo =
    IStore.Repo.v
      (Irmin_pack.config (default_storage_dir configuration.data_dir))
  in
  IStore.main repo

let flush store = IStore.flush (IStore.repo store)

let close store = IStore.Repo.close (IStore.repo store)

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.Default.v ~author:"Tezos smart-contract rollup node" ~message date

module type Mutable_value = sig
  type value

  val path_key : path

  val decode_value : bytes -> value Lwt.t

  val set : t -> value -> unit Lwt.t

  val get : t -> value Lwt.t

  val find : t -> value option Lwt.t
end

module type KeyValue = sig
  val path : path

  val keep_last_n_entries_in_memory : int

  type key

  val string_of_key : key -> string

  type value

  val value_encoding : value Data_encoding.t
end

module Make_map (P : KeyValue) = struct
  (* Ignored for now. *)
  let _ = P.keep_last_n_entries_in_memory

  let path_key = P.path

  let make_key key = path_key @ [P.string_of_key key]

  let mem store key = IStore.mem store (make_key key)

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let get store key = IStore.get store (make_key key) >>= decode_value

  let find store key =
    let open Lwt_syntax in
    let* value = IStore.find store (make_key key) in
    Option.map_s decode_value value

  let find_with_default store key ~on_default =
    let open Lwt_syntax in
    let+ value = find store key in
    Option.value_f value ~default:on_default
end

module Make_updatable_map (P : KeyValue) = struct
  include Make_map (P)

  let add store key value =
    let full_path = String.concat "/" (P.path @ [P.string_of_key key]) in
    let encode v = Data_encoding.Binary.to_bytes_exn P.value_encoding v in
    let encoded_value = encode value in
    let info () = info full_path in
    IStore.set_exn ~info store (make_key key) encoded_value
end

module Make_append_only_map (P : KeyValue) = struct
  include Make_map (P)

  let add store key value =
    let open Lwt_syntax in
    let* existing_value = find store key in
    let full_path = String.concat "/" (P.path @ [P.string_of_key key]) in
    let encode v = Data_encoding.Binary.to_bytes_exn P.value_encoding v in
    let encoded_value = encode value in
    match existing_value with
    | None ->
        let info () = info full_path in
        IStore.set_exn ~info store (make_key key) encoded_value
    | Some existing_value ->
        (* To be robust to interruption in the middle of processes,
           we accept to redo some work when we restart the node.
           Hence, it is fine to insert twice the same value for a
           given value. *)
        if not (Bytes.equal (encode existing_value) encoded_value) then
          Stdlib.failwith
            (Printf.sprintf
               "Key %s already exists with a different value"
               full_path)
        else return_unit
end

module Make_mutable_value (P : sig
  val path : path

  type value

  val value_encoding : value Data_encoding.t
end) =
struct
  type value = P.value

  let path_key = P.path

  let decode_value encoded_value =
    Lwt.return
    @@ Data_encoding.Binary.of_bytes_exn P.value_encoding encoded_value

  let set store value =
    let encoded_value =
      Data_encoding.Binary.to_bytes_exn P.value_encoding value
    in
    let info () = info (String.concat "/" P.path) in
    IStore.set_exn ~info store path_key encoded_value

  let get store = IStore.get store path_key >>= decode_value

  let find store =
    let open Lwt_syntax in
    let* value = IStore.find store path_key in
    Option.map_s decode_value value
end

(** Aggregated collection of messages from the L1 inbox *)

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

  type value = Sc_rollup.Inbox.Message.t list

  let value_encoding =
    Data_encoding.(list @@ dynamic_size Sc_rollup.Inbox.Message.encoding)
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

  type value = Sc_rollup.Inbox.history

  let value_encoding = Sc_rollup.Inbox.history_encoding
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
