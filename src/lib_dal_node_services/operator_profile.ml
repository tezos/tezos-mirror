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

type t = {producers : Slot_set.t; attesters : Pkh_set.t; observers : Slot_set.t}

let empty =
  {
    producers = Slot_set.empty;
    attesters = Pkh_set.empty;
    observers = Slot_set.empty;
  }

let has_producer {producers; _} = not (Slot_set.is_empty producers)

let has_attester {attesters; _} = not (Pkh_set.is_empty attesters)

let has_observer {observers; _} = not (Slot_set.is_empty observers)

let is_empty op =
  (not (has_observer op)) && (not (has_attester op)) && not (has_producer op)

let producer_slot_out_of_bounds number_of_slots op =
  Slot_set.find_first (fun i -> i < 0 || i >= number_of_slots) op.producers

let is_observed_slot slot_index {observers; _} =
  Slot_set.mem slot_index observers

let get_all_slot_indexes {producers; observers; _} =
  Slot_set.(union producers observers |> to_seq) |> List.of_seq

let make ?(attesters = []) ?(producers = []) ?(observers = []) () =
  {
    producers = Slot_set.of_list producers;
    observers = Slot_set.of_list observers;
    attesters = Pkh_set.of_list attesters;
  }

let merge ?(on_new_attester = fun _ -> ()) op1 op2 =
  let ( @ ) = Slot_set.union in
  let ( @. ) =
    Pkh_set.iter
      (fun pkh ->
        if not (Pkh_set.mem pkh op1.attesters) then on_new_attester pkh)
      op2.attesters ;
    Pkh_set.union
  in
  {
    producers = op1.producers @ op2.producers;
    attesters = op1.attesters @. op2.attesters;
    observers = op1.observers @ op2.observers;
  }

let encoding =
  let open Data_encoding in
  conv
    (fun {producers; observers; attesters} ->
      ( Slot_set.elements producers,
        Slot_set.elements observers,
        Pkh_set.elements attesters ))
    (fun (producers, observers, attesters) ->
      {
        producers = Slot_set.of_list producers;
        observers = Slot_set.of_list observers;
        attesters = Pkh_set.of_list attesters;
      })
    (obj3
       (req "producers" (list int31))
       (req "observers" (list int31))
       (req "attesters" (list Signature.Public_key_hash.encoding)))
