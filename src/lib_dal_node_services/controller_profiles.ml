(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Slot_set = Set.Make (Int)
module Pkh_set = Signature.Public_key_hash.Set

type t = {operators : Slot_set.t; attesters : Pkh_set.t; observers : Slot_set.t}

let empty =
  {
    operators = Slot_set.empty;
    attesters = Pkh_set.empty;
    observers = Slot_set.empty;
  }

let has_operator {operators; _} = not (Slot_set.is_empty operators)

let has_attester {attesters; _} = not (Pkh_set.is_empty attesters)

let has_observer {observers; _} = not (Slot_set.is_empty observers)

let attester_only t =
  has_attester t && (not (has_operator t)) && not (has_observer t)

let attesters t = t.attesters

let is_empty t =
  (not (has_observer t)) && (not (has_attester t)) && not (has_operator t)

let operator_slot_out_of_bounds number_of_slots t =
  Slot_set.find_first (fun i -> i < 0 || i >= number_of_slots) t.operators

let observer_slot_out_of_bounds number_of_slots t =
  Slot_set.find_first (fun i -> i < 0 || i >= number_of_slots) t.observers

let is_observed_slot slot_index {observers; _} =
  Slot_set.mem slot_index observers

let can_publish_on_slot_index slot_index {observers; operators; _} =
  Slot_set.mem slot_index observers || Slot_set.mem slot_index operators

let get_all_slot_indexes {operators; observers; _} =
  Slot_set.(union operators observers |> to_seq) |> List.of_seq

let make ?(attesters = []) ?(operators = []) ?(observers = []) () =
  {
    operators = Slot_set.of_list operators;
    observers = Slot_set.of_list observers;
    attesters = Pkh_set.of_list attesters;
  }

let merge ?(on_new_attester = fun _ -> ()) t1 t2 =
  let ( @ ) = Slot_set.union in
  let ( @. ) =
    Pkh_set.iter
      (fun pkh ->
        if not (Pkh_set.mem pkh t1.attesters) then on_new_attester pkh)
      t2.attesters ;
    Pkh_set.union
  in
  {
    operators = t1.operators @ t2.operators;
    attesters = t1.attesters @. t2.attesters;
    observers = t1.observers @ t2.observers;
  }

let encoding =
  let open Data_encoding in
  conv
    (fun {operators; observers; attesters} ->
      ( Slot_set.elements operators,
        Slot_set.elements observers,
        Pkh_set.elements attesters ))
    (fun (operators, observers, attesters) ->
      {
        operators = Slot_set.of_list operators;
        observers = Slot_set.of_list observers;
        attesters = Pkh_set.of_list attesters;
      })
    (obj3
       (* Use the new name in the encoding. *)
       (dft "operators" (list int31) [])
       (dft "observers" (list int31) [])
       (dft "attesters" (list Signature.Public_key_hash.encoding) []))
