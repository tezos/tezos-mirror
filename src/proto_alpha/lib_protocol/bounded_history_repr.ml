(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module type NAME = sig
  val name : string
end

module type KEY = sig
  type t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

module type VALUE = sig
  type t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t
end

module type S = sig
  type t

  type key

  type value

  val empty : capacity:int64 -> t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val find : key -> t -> value option

  type error +=
    | Key_bound_to_different_value of {
        key : key;
        existing_value : value;
        given_value : value;
      }

  val remember : key -> value -> t -> t tzresult

  module Internal_for_tests : sig
    val empty : capacity:int64 -> next_index:int64 -> t

    val keys : t -> key list
  end
end

module Make (Name : NAME) (Key : KEY) (Value : VALUE) :
  S with type key = Key.t and type value = Value.t = struct
  type key = Key.t

  type value = Value.t

  module Int64_map = Map.Make (Int64)
  module Map = Map.Make (Key)

  type t = {
    events : value Map.t;
        (** Values stored in the structure, indexes with the keys. *)
    sequence : key Int64_map.t;
        (** An additional map from int64 indexes to keys, to be able
            to remove old entries when the structure is full.  *)
    capacity : int64;
        (** The max number of the entries in the structure. Once the maximum size
            is reached, older entries are deleted to free space for new ones. *)
    next_index : int64;
        (** The index to use for the next entry to add in the structure. *)
    oldest_index : int64;
        (** The oldest index of the (oldest) entry that has been added to the
            data structure. If the structure is empty, [oldest_index] is
            equal to [next_index]. *)
    size : int64;
        (** Counts the number of entries that are stored in history. It
            satisfies the invariant: `0 <= size <= capacity` *)
  }

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    let events_encoding =
      Data_encoding.conv
        Map.bindings
        (fun l -> Map.add_seq (List.to_seq l) Map.empty)
        Data_encoding.(list (tup2 Key.encoding Value.encoding))
    in
    let sequence_encoding =
      conv
        Int64_map.bindings
        (List.fold_left (fun m (k, v) -> Int64_map.add k v m) Int64_map.empty)
        (list (tup2 int64 Key.encoding))
    in
    conv
      (fun {events; sequence; capacity; next_index; oldest_index; size} ->
        (events, sequence, capacity, next_index, oldest_index, size))
      (fun (events, sequence, capacity, next_index, oldest_index, size) ->
        {events; sequence; capacity; next_index; oldest_index; size})
      (obj6
         (req "events" events_encoding)
         (req "sequence" sequence_encoding)
         (req "capacity" int64)
         (req "next_index" int64)
         (req "oldest_index" int64)
         (req "size" int64))

  let pp fmt {events; sequence; capacity; size; oldest_index; next_index} =
    Map.bindings events |> fun bindings ->
    Int64_map.bindings sequence |> fun sequence_bindings ->
    let pp_binding fmt (hash, history_proof) =
      Format.fprintf fmt "@[%a -> %a@;@]" Key.pp hash Value.pp history_proof
    in
    let pp_sequence_binding fmt (counter, hash) =
      Format.fprintf fmt "@[%s -> %a@;@]" (Int64.to_string counter) Key.pp hash
    in
    Format.fprintf
      fmt
      "@[<hov 2>History:@;\
      \ { capacity: %Ld;@;\
      \ current size: %Ld;@;\
      \ oldest index: %Ld;@;\
      \ next_index : %Ld;@;\
      \ bindings: %a;@;\
      \ sequence: %a; }@]"
      capacity
      size
      oldest_index
      next_index
      (Format.pp_print_list pp_binding)
      bindings
      (Format.pp_print_list pp_sequence_binding)
      sequence_bindings

  let empty ~capacity =
    let next_index = 0L in
    {
      events = Map.empty;
      sequence = Int64_map.empty;
      capacity;
      next_index;
      oldest_index = next_index;
      size = 0L;
    }

  type error +=
    | Key_bound_to_different_value of {
        key : key;
        existing_value : value;
        given_value : value;
      }

  let () =
    assert (not (String.equal Name.name "")) ;
    register_error_kind
      `Temporary
      ~id:
        (Format.sprintf
           "Bounded_history_repr.%s.key_bound_to_different_value"
           Name.name)
      ~title:(Name.name ^ ": Key already bound to a different value.")
      ~description:
        (Name.name
       ^ ": Remember called with a key that is already bound to a different\n\
         \        value.")
      Data_encoding.(
        obj3
          (req "key" Key.encoding)
          (req "existing_value" Value.encoding)
          (req "given_value" Value.encoding))
      (function
        | Key_bound_to_different_value {key; existing_value; given_value} ->
            Some (key, existing_value, given_value)
        | _ -> None)
      (fun (key, existing_value, given_value) ->
        Key_bound_to_different_value {key; existing_value; given_value})

  let remember key value t =
    let open Result_syntax in
    if Compare.Int64.(t.capacity <= 0L) then return t
    else
      match Map.find key t.events with
      | Some value' when not (Value.equal value value') ->
          tzfail
          @@ Key_bound_to_different_value
               {key; existing_value = value'; given_value = value}
      | _ -> (
          let events = Map.add key value t.events in
          let current_index = t.next_index in
          let next_index = Int64.succ current_index in
          let t =
            {
              events;
              sequence = Int64_map.add current_index key t.sequence;
              capacity = t.capacity;
              next_index;
              oldest_index = t.oldest_index;
              size = Int64.succ t.size;
            }
          in
          (* A negative size means that [t.capacity] is set to [Int64.max_int]
             and that the structure is full, so adding a new entry makes the size
             overflows. In this case, we remove an element in the else branch to
             keep the size of the structure equal to [Int64.max_int] at most. *)
          if Compare.Int64.(t.size > 0L && t.size <= t.capacity) then return t
          else
            let l = t.oldest_index in
            match Int64_map.find l t.sequence with
            | None ->
                (* If t.size > t.capacity > 0, there is necessarily
                   an entry whose index is t.oldest_index in [sequence]. *)
                assert false
            | Some h ->
                let sequence = Int64_map.remove l t.sequence in
                let events = Map.remove h events in
                return
                  {
                    next_index = t.next_index;
                    capacity = t.capacity;
                    size = t.capacity;
                    oldest_index = Int64.succ t.oldest_index;
                    sequence;
                    events;
                  })

  let find key t = Map.find_opt key t.events

  module Internal_for_tests = struct
    let empty ~capacity ~next_index =
      {(empty ~capacity) with next_index; oldest_index = next_index}

    let keys {sequence; oldest_index; _} =
      let l = Int64_map.bindings sequence in
      (* All entries with an index greater than oldest_index are well ordered.
         There are put in the [lp] list. Entries with an index smaller than
         oldest_index are also well ordered, but they should come after
         elements in [lp]. This happens in theory when the index reaches
         max_int and then overflows. *)
      let ln, lp =
        List.partition_map
          (fun (n, h) ->
            if Compare.Int64.(n < oldest_index) then Left h else Right h)
          l
      in
      (* do a tail recursive concatenation lp @ ln *)
      List.rev_append (List.rev lp) ln
  end
end
