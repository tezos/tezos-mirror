(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Shell_operation

(* Ordering is important, as it is used below in map keys comparison *)
type priority = High | Medium | Low of Q.t list

module Priority_map : Map.S with type key = priority = Map.Make (struct
  type t = priority

  module CompareListQ = Compare.List (Q)

  let compare_low_prio p1 p2 =
    (* A higher priority operation should appear before in the map. So we use
       the pointwise comparison of p2 and p1 *)
    CompareListQ.compare p2 p1

  let compare p1 p2 =
    (* - Explicit comparison, High is smaller,
       - Avoid fragile patterns in case the type is extended in the future *)
    match (p1, p2) with
    | High, High | Medium, Medium -> 0
    | Low p1, Low p2 -> compare_low_prio p1 p2
    | High, (Low _ | Medium) -> -1
    | (Low _ | Medium), High -> 1
    | Low _, Medium -> 1
    | Medium, Low _ -> -1
end)

module Map = Operation_hash.Map
module Sized_set = Tezos_base.Sized.MakeSizedSet (Operation_hash.Set)

(*
   The type below is used for representing pending operations data of the
   prevalidator. The functions of this module (should) maintain the
   following invariants:
   1 - Union (preimage(pending(prio))) = hashes, for each prio in dom(pending)
   2 - preimage (priority_of) = hashes
   3 - image(priority_of) = preimage (pending)
   4 - map in pending(priority) => map <> empty
*)
type 'a t = {
  (* The main map *)
  pending : 'a operation Map.t Priority_map.t;
  (* Used for advertising *)
  hashes : Sized_set.t;
  (* We need to remember the priority of each hash, to be used when removing
     without providing the priority *)
  priority_of : priority Map.t;
}

let empty =
  {
    pending = Priority_map.empty;
    hashes = Sized_set.empty;
    priority_of = Map.empty;
  }

let is_empty {pending = _; priority_of = _; hashes} = Sized_set.is_empty hashes

let hashes {pending = _; priority_of = _; hashes} = Sized_set.to_set hashes

let operations {pending; priority_of = _; hashes = _} =
  (* Build a flag map [oph -> op] from pending. Needed when re-cycling
     operations *)
  Priority_map.fold
    (fun _prio -> Map.union (fun _ _ b -> Some b))
    pending
    Map.empty

let mem oph {hashes; priority_of = _; pending = _} = Sized_set.mem oph hashes

let get_priority_map prio pending =
  match Priority_map.find prio pending with None -> Map.empty | Some mp -> mp

let add op prio {pending; hashes; priority_of} =
  let oph = op.hash in
  let mp = get_priority_map prio pending |> Map.add oph op in
  {
    pending = Priority_map.add prio mp pending;
    hashes = Sized_set.add oph hashes;
    priority_of = Map.add oph prio priority_of;
  }

let remove oph ({pending; hashes; priority_of} as t) =
  match Map.find oph priority_of with
  | None -> t
  | Some prio ->
      let mp = get_priority_map prio pending |> Map.remove oph in
      {
        pending =
          (if Map.is_empty mp then Priority_map.remove prio pending
          else Priority_map.add prio mp pending);
        hashes = Sized_set.remove oph hashes;
        priority_of = Map.remove oph priority_of;
      }

let cardinal {pending = _; hashes; priority_of = _} = Sized_set.cardinal hashes

let fold_es f {pending; hashes = _; priority_of = _} acc =
  Priority_map.fold_es
    (fun prio mp acc -> Map.fold_es (f prio) mp acc)
    pending
    acc

let fold f {pending; hashes = _; priority_of = _} acc =
  Priority_map.fold (fun prio mp acc -> Map.fold (f prio) mp acc) pending acc

let iter f {pending; hashes = _; priority_of = _} =
  Priority_map.iter (fun prio mp -> Map.iter (f prio) mp) pending
