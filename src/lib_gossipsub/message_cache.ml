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

module Sliding_message_id_map (C : Gossipsub_intf.AUTOMATON_SUBCONFIG) : sig
  (** A data structure containing a mapping from message ids to values of type
      ['a]. Message ids are grouped by topic and by slot (which is a just an
      integer). The grouping by slots is used to remove entries from old slots,
      thus it allows to use this data structure as a sliding window. *)

  type 'a t

  (** [make ~window_size] creates an empty map with window size [window_size].
      This means that the values will be removed from the map after [window_size] shifts. *)
  val make : window_size:int -> 'a t

  (** [add topic message_id ~update_value t] adds an entry to the map for [message_id] with [topic].
      [update_value] is used to update the value stored for [message_id]. *)
  val add :
    C.Topic.t ->
    C.Message_id.t ->
    update_value:('a option -> 'a option) ->
    'a t ->
    'a t

  (** [update_value message_id f t] updates the value stored for key [message_id]
      by applying [f] to the stored value. Returns [None] if no value was stored
      for the [message_id], and otherwise returns [Some (t, value)] where [t] is the new map
      and [value] is the updated value. *)
  val update_value : C.Message_id.t -> ('a -> 'a) -> 'a t -> ('a t * 'a) option

  (** [get_value message_id t] returns the value stored for key [message_id].
      Returns [None] if nothing is stored. *)
  val get_value : C.Message_id.t -> 'a t -> 'a option

  (** [get_message_ids ~slots t] returns the [topic] to [message list] mapping
      for the past [slots] slots. *)
  val get_latest_message_ids :
    slots:int -> 'a t -> (Int64.t * C.Message_id.t list C.Topic.Map.t) Seq.t

  (** Shifts the sliding window by one slot. *)
  val shift : 'a t -> 'a t

  module Introspection : sig
    module Map : Map.S with type key = Int64.t

    val get_slot_entries : 'a t -> C.Message_id.t list C.Topic.Map.t Map.t
  end

  module Internal_for_tests : sig
    val get_values : 'a t -> (C.Message_id.t * 'a) Seq.t
  end
end = struct
  module Topic = C.Topic
  module Message_id = C.Message_id

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5536
     Transform the list into a set? *)
  type slot_entry = Message_id.t list Topic.Map.t

  (* A slot is just an index, normally a heartbeat tick, and this is why the
     same type, namely [Int64], is used. Note that a bigger slot is a more
     recent slot. *)
  module Map = Map.Make (Int64)

  type 'a value_with_counter = {
    value : 'a;
    store_counter : int;
        (* This field is used to count how many times the message has been added to
           the map. Normally the caller ensures that the same messages (that is,
           with the same id) is not inserted twice; however, this module does not make
           this assumption. *)
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5536
           Make the above assumption and remove this field? *)
  }

  type 'a t = {
    values_with_counter : 'a value_with_counter Message_id.Map.t;
    slot_entries : slot_entry Map.t;
        (** The slot entries without the entry for the last slot, which is stored
            separately, see next field. *)
    latest_slot_entry : slot_entry;
    latest_slot : Int64.t;
    window_size : int;
  }

  let make ~window_size =
    {
      values_with_counter = Message_id.Map.empty;
      slot_entries = Map.empty;
      latest_slot_entry = Topic.Map.empty;
      latest_slot = 0L;
      window_size;
    }

  let add topic message_id ~update_value t =
    let latest_slot_entry =
      Topic.Map.update
        topic
        (function
          | None -> Some [message_id]
          | Some message_ids -> Some (message_id :: message_ids))
        t.latest_slot_entry
    in
    let values_with_counter =
      Message_id.Map.update
        message_id
        (function
          | None ->
              update_value None
              |> Option.map (fun value -> {value; store_counter = 1})
          | Some {value; store_counter} ->
              update_value (Some value)
              |> Option.map (fun value ->
                     {value; store_counter = store_counter + 1}))
        t.values_with_counter
    in
    {t with latest_slot_entry; values_with_counter}

  let update_value message_id f t =
    let updated_value = ref None in
    let values_with_counter =
      Message_id.Map.update
        message_id
        (function
          | None -> None
          | Some {value; store_counter} ->
              let value = f value in
              updated_value := Some value ;
              Some {value; store_counter})
        t.values_with_counter
    in
    !updated_value
    |> Option.map (fun value -> ({t with values_with_counter}, value))

  let get_value message_id t =
    Message_id.Map.find message_id t.values_with_counter
    |> Option.map (fun {value; _} -> value)

  let get_latest_message_ids ~slots t =
    let slots =
      Map.to_rev_seq t.slot_entries
      |> Seq.take ~when_negative_length:() (slots - 1)
      |> WithExceptions.Result.get_ok ~loc:__LOC__
    in
    Seq.cons (t.latest_slot, t.latest_slot_entry) slots

  let shift t =
    let drop_old_values oldest_entries =
      Topic.Map.fold
        (fun _topic message_ids messages ->
          List.fold_left
            (fun messages message_id ->
              Message_id.Map.update
                message_id
                (function
                  | None -> None
                  | Some {value; store_counter} ->
                      if store_counter = 1 then None
                      else Some {value; store_counter = store_counter - 1})
                messages)
            messages
            message_ids)
        oldest_entries
        t.values_with_counter
    in
    let slot_entries =
      Map.add t.latest_slot t.latest_slot_entry t.slot_entries
    in
    let latest_slot = Int64.succ t.latest_slot in
    let latest_slot_entry = Topic.Map.empty in
    match Map.min_binding slot_entries with
    | None -> {t with slot_entries; latest_slot; latest_slot_entry}
    | Some (first_slot, entry) ->
        if Int64.(sub latest_slot first_slot < of_int t.window_size) then
          {
            slot_entries;
            latest_slot;
            latest_slot_entry;
            values_with_counter = t.values_with_counter;
            window_size = t.window_size;
          }
        else
          {
            slot_entries = Map.remove first_slot slot_entries;
            latest_slot_entry;
            latest_slot;
            values_with_counter = drop_old_values entry;
            window_size = t.window_size;
          }

  module Introspection = struct
    module Map = Map

    let get_slot_entries t = t.slot_entries
  end

  module Internal_for_tests = struct
    let get_values t =
      t.values_with_counter |> Message_id.Map.to_seq
      |> Seq.map (fun (message_id, {value; store_counter = _}) ->
             (message_id, value))
  end
end

module Make
    (C : Gossipsub_intf.AUTOMATON_SUBCONFIG)
    (Time : Gossipsub_intf.TIME) :
  Gossipsub_intf.MESSAGE_CACHE
    with module Peer = C.Peer
     and module Topic = C.Topic
     and module Message_id = C.Message_id
     and module Message = C.Message
     and module Time = Time = struct
  module Peer = C.Peer
  module Topic = C.Topic
  module Message_id = C.Message_id
  module Message = C.Message
  module Time = Time
  module Sliding_message_id_map = Sliding_message_id_map (C)

  type message_with_access_counter = {
    message : Message.t;
    access_counters : int Peer.Map.t;
  }

  type t = {
    gossip_slots : int;
    messages : message_with_access_counter Sliding_message_id_map.t;
    first_seen_times : Time.t Sliding_message_id_map.t;
  }

  let create ~history_slots ~gossip_slots ~seen_message_slots =
    assert (gossip_slots > 0) ;
    assert (gossip_slots <= history_slots) ;
    assert (seen_message_slots > 0) ;
    {
      gossip_slots;
      messages = Sliding_message_id_map.make ~window_size:history_slots;
      first_seen_times =
        Sliding_message_id_map.make ~window_size:seen_message_slots;
    }

  let add_message message_id message topic t =
    let messages =
      Sliding_message_id_map.add
        topic
        message_id
        ~update_value:(function
          | None -> Some {message; access_counters = Peer.Map.empty}
          | Some m -> Some m)
        t.messages
    in
    let first_seen_times =
      Sliding_message_id_map.add
        topic
        message_id
        ~update_value:(function
          | None -> Some (Time.now ()) | Some time -> Some time)
        t.first_seen_times
    in
    {t with messages; first_seen_times}

  let get_message_for_peer peer message_id t =
    let access_count = ref 0 in
    let update_result =
      Sliding_message_id_map.update_value
        message_id
        (fun {message; access_counters} ->
          let access_counters =
            Peer.Map.update
              peer
              (function
                | None ->
                    access_count := 1 ;
                    Some !access_count
                | Some c ->
                    access_count := c + 1 ;
                    Some !access_count)
              access_counters
          in
          {message; access_counters})
        t.messages
    in
    update_result
    |> Option.map (fun (messages, {message; access_counters = _}) ->
           ({t with messages}, message, !access_count))

  let get_message_ids_to_gossip topic t =
    Sliding_message_id_map.get_latest_message_ids
      ~slots:t.gossip_slots
      t.messages
    |> Seq.fold_left
         (fun acc_message_ids (_slot, entries) ->
           match Topic.Map.find topic entries with
           | None -> acc_message_ids
           | Some message_ids -> List.rev_append message_ids acc_message_ids)
         []

  let get_first_seen_time message_id t =
    Sliding_message_id_map.get_value message_id t.first_seen_times

  let seen_message message_id t =
    Option.is_some
    @@ Sliding_message_id_map.get_value message_id t.first_seen_times

  let shift t =
    {
      t with
      messages = Sliding_message_id_map.shift t.messages;
      first_seen_times = Sliding_message_id_map.shift t.first_seen_times;
    }

  module Introspection = struct
    module Map = Sliding_message_id_map.Introspection.Map

    let get_message_ids t =
      Sliding_message_id_map.Introspection.get_slot_entries t.messages
  end

  module Internal_for_tests = struct
    let get_access_counters t =
      Sliding_message_id_map.Internal_for_tests.get_values t.messages
      |> Seq.map (fun (message_id, {access_counters; _}) ->
             (message_id, access_counters))
  end
end
