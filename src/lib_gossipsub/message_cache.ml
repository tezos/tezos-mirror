(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Make
    (Peer : Gossipsub_intf.ITERABLE)
    (Topic : Gossipsub_intf.ITERABLE)
    (Message_id : Gossipsub_intf.ITERABLE)
    (Message : Gossipsub_intf.PRINTABLE) =
struct
  type message_with_counters = {
    message : Message.t;
    store_counter : int;
    (* This field is used to count how many times the message has been added to
       the cache. Normally the caller ensures that the same messages (that is,
       with the same id) is not insert twice; however, this module does not make
       this assumption. *)
    access_counters : int Peer.Map.t;
  }

  (* A slot is just an index, normally a heartbeat tick, and this is why the
     same type, namely [Int64], is used. Note that a bigger slots is a more
     recent slot. *)
  module SlotMap = Map.Make (Int64)

  type slot_entry = Message_id.t list Topic.Map.t

  type t = {
    history_slots : int;
    gossip_slots : int;
    messages : message_with_counters Message_id.Map.t;
    cache : slot_entry SlotMap.t;
        (** The cache without the entry for the last slot, which is stored
            separately, see next field. *)
    last_slot_entry : slot_entry;
    last_slot : Int64.t;
  }

  let create ~history_slots ~gossip_slots =
    assert (gossip_slots > 0) ;
    assert (gossip_slots <= history_slots) ;
    {
      history_slots;
      gossip_slots;
      messages = Message_id.Map.empty;
      cache = SlotMap.empty;
      last_slot_entry = Topic.Map.empty;
      last_slot = 0L;
    }

  let add_message message_id message topic t =
    let last_slot_entry =
      Topic.Map.update
        topic
        (function
          | None -> Some [message_id]
          | Some message_ids -> Some (message_id :: message_ids))
        t.last_slot_entry
    in
    let messages =
      Message_id.Map.update
        message_id
        (function
          | None ->
              Some
                {message; store_counter = 1; access_counters = Peer.Map.empty}
          | Some {message; store_counter; access_counters} ->
              Some {message; store_counter = store_counter + 1; access_counters})
        t.messages
    in
    {t with messages; last_slot_entry}

  let get_message_for_peer peer message_id t =
    match Message_id.Map.find message_id t.messages with
    | None -> None
    | Some {message; store_counter; access_counters} ->
        let counter = ref 1 in
        let access_counters =
          Peer.Map.update
            peer
            (function
              | None -> Some 1
              | Some c ->
                  counter := c + 1 ;
                  Some !counter)
            access_counters
        in
        let t =
          {
            t with
            messages =
              Message_id.Map.add
                message_id
                {message; store_counter; access_counters}
                t.messages;
          }
        in
        Some (t, message, !counter)

  let get_message_ids_to_gossip topic t =
    SlotMap.to_rev_seq t.cache
    |> Seq.take ~when_negative_length:() (t.gossip_slots - 1)
    |> WithExceptions.Result.get_ok ~loc:__LOC__
    |> Seq.cons (t.last_slot, t.last_slot_entry)
    |> Seq.fold_left
         (fun acc_message_ids (_slot, entries) ->
           match Topic.Map.find topic entries with
           | None -> acc_message_ids
           | Some message_ids -> List.rev_append message_ids acc_message_ids)
         []

  let shift t =
    let drop_old_messages oldest_entries =
      Topic.Map.fold
        (fun _topic message_ids messages ->
          List.fold_left
            (fun messages message_id ->
              Message_id.Map.update
                message_id
                (function
                  | None -> None
                  | Some {message; store_counter; access_counters} ->
                      if store_counter = 1 then None
                      else
                        Some
                          {
                            message;
                            store_counter = store_counter - 1;
                            access_counters;
                          })
                messages)
            messages
            message_ids)
        oldest_entries
        t.messages
    in
    let cache = SlotMap.add t.last_slot t.last_slot_entry t.cache in
    let last_slot = Int64.succ t.last_slot in
    let cache, messages =
      match SlotMap.min_binding cache with
      | None -> (* impossible *) (cache, t.messages)
      | Some (first_slot, entries) ->
          if Int64.(sub last_slot first_slot < of_int t.history_slots) then
            (cache, t.messages)
          else (SlotMap.remove first_slot cache, drop_old_messages entries)
    in
    {t with cache; messages; last_slot; last_slot_entry = Topic.Map.empty}
end
