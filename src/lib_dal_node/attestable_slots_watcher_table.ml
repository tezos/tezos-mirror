(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module SlotIdSet =
  Aches.Vache.Set (Aches.Vache.LRU_Sloppy) (Aches.Vache.Strong) (Types.Slot_id)

(** A watcher used to stream newly-attestable slots for a given delegate (pkh).
    - [stream] is the push endpoint used by the DAL node to notify consumers
      (RPC layer / baker) with [attestable_event] information.
    - [num_subscribers] is the number of active consumers currently subscribed to
      this pkh’s stream.
    - [notified_slots] is an LRU set of slot ids already notified, so that we avoid
      sending duplicates in the stream. *)
type watcher = {
  stream : Types.Attestable_event.t Lwt_watcher.input;
  mutable num_subscribers : int;
  notified_slots : SlotIdSet.t;
}

let get_stream watcher = watcher.stream

let get_num_subscribers watcher = watcher.num_subscribers

let set_num_subscribers watcher value = watcher.num_subscribers <- value

type t = watcher Signature.Public_key_hash.Table.t

let create ~initial_size = Signature.Public_key_hash.Table.create initial_size

let get_or_init t pkh proto_params =
  match Signature.Public_key_hash.Table.find t pkh with
  | Some watcher ->
      watcher.num_subscribers <- watcher.num_subscribers + 1 ;
      watcher
  | None ->
      (* We only need to remember slot ids from their publication level until the
         attestation window closes ([attestation_lag] levels later). There can be
         at most [number_of_slots] distinct slot ids per level across that window.
         We allow a small (2 levels) additional window for safety. *)
      let capacity =
        match proto_params with
        | Some Types.{number_of_slots; attestation_lag; _} ->
            number_of_slots * (attestation_lag + 2)
        | None -> 320 (* 32 * (8 + 2) - hardcoded values as of protocol T *)
      in
      let watcher =
        {
          stream = Lwt_watcher.create_input ();
          num_subscribers = 1;
          notified_slots = SlotIdSet.create capacity;
        }
      in
      Signature.Public_key_hash.Table.add t pkh watcher ;
      watcher

let notify_attestable_slot t pkh ~slot_id =
  match Signature.Public_key_hash.Table.find t pkh with
  | None -> ()
  | Some watcher ->
      if not @@ SlotIdSet.mem watcher.notified_slots slot_id then (
        SlotIdSet.add watcher.notified_slots slot_id ;
        Lwt_watcher.notify watcher.stream (Attestable_slot {slot_id}))

let notify_no_shards_assigned t pkh ~attestation_level =
  match Signature.Public_key_hash.Table.find t pkh with
  | None -> ()
  | Some watcher ->
      Lwt_watcher.notify watcher.stream (No_shards_assigned {attestation_level})

let notify_slot_has_trap t pkh ~slot_id =
  match Signature.Public_key_hash.Table.find t pkh with
  | None -> ()
  | Some watcher ->
      if not @@ SlotIdSet.mem watcher.notified_slots slot_id then (
        SlotIdSet.add watcher.notified_slots slot_id ;
        Lwt_watcher.notify watcher.stream (Slot_has_trap {slot_id}))

let notify_backfill_payload t pkh ~backfill_payload =
  match Signature.Public_key_hash.Table.find t pkh with
  | None -> ()
  | Some watcher ->
      Lwt_watcher.notify watcher.stream (Backfill {backfill_payload})

let remove = Signature.Public_key_hash.Table.remove

let elements = Signature.Public_key_hash.Table.to_seq_keys
