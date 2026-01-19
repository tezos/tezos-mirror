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

(* A set of (attested) slot indexes. *)
type t = Bitset.t

let encoding = Bitset.encoding

let empty = Bitset.empty

let is_empty = Bitset.is_empty

let is_attested t index =
  let open Dal_slot_index_repr in
  match Bitset.mem t (to_int index) with Ok b -> b | Error _ -> assert false

let commit t index =
  let open Dal_slot_index_repr in
  match Bitset.add t (to_int index) with Ok t -> t | Error _ -> assert false

let occupied_size_in_bits = Bitset.occupied_size_in_bits

let expected_size_in_bits ~max_index =
  (* We compute an encoding of the data-availability attestations
     which is a (tight) upper bound of what we expect. *)
  let open Bitset in
  let open Dal_slot_index_repr in
  match add empty @@ to_int max_index with
  | Error _ -> (* Happens if max_index < 1 *) 0
  | Ok t -> occupied_size_in_bits t

let number_of_attested_slots = Bitset.cardinal

let of_attestations (t : Dal_attestations_repr.t) = (t :> Bitset.t)

module Internal_for_tests = struct
  let of_z = Bitset.from_z
end
