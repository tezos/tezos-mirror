(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Alpha_context
open Script_typed_ir

let compare_address {destination = destination1; entrypoint = entrypoint1}
    {destination = destination2; entrypoint = entrypoint2} =
  let lres = Destination.compare destination1 destination2 in
  if Compare.Int.(lres = 0) then Entrypoint.compare entrypoint1 entrypoint2
  else lres

let compare_tx_rollup_l2_address = Tx_rollup_l2_address.Indexable.compare_values

type compare_comparable_cont =
  | Compare_comparable :
      'a comparable_ty * 'a * 'a * compare_comparable_cont
      -> compare_comparable_cont
  | Compare_comparable_return : compare_comparable_cont

let compare_comparable : type a. a comparable_ty -> a -> a -> int =
  let rec compare_comparable :
      type a. a comparable_ty -> compare_comparable_cont -> a -> a -> int =
   fun kind k x y ->
    match (kind, x, y) with
    | Unit_t, (), () -> (apply [@tailcall]) 0 k
    | Never_t, _, _ -> .
    | Signature_t, x, y -> (apply [@tailcall]) (Script_signature.compare x y) k
    | String_t, x, y -> (apply [@tailcall]) (Script_string.compare x y) k
    | Bool_t, x, y -> (apply [@tailcall]) (Compare.Bool.compare x y) k
    | Mutez_t, x, y -> (apply [@tailcall]) (Tez.compare x y) k
    | Key_hash_t, x, y ->
        (apply [@tailcall]) (Signature.Public_key_hash.compare x y) k
    | Key_t, x, y -> (apply [@tailcall]) (Signature.Public_key.compare x y) k
    | Int_t, x, y -> (apply [@tailcall]) (Script_int.compare x y) k
    | Nat_t, x, y -> (apply [@tailcall]) (Script_int.compare x y) k
    | Timestamp_t, x, y -> (apply [@tailcall]) (Script_timestamp.compare x y) k
    | Address_t, x, y -> (apply [@tailcall]) (compare_address x y) k
    | Tx_rollup_l2_address_t, x, y ->
        (apply [@tailcall]) (compare_tx_rollup_l2_address x y) k
    | Bytes_t, x, y -> (apply [@tailcall]) (Compare.Bytes.compare x y) k
    | Chain_id_t, x, y -> (apply [@tailcall]) (Script_chain_id.compare x y) k
    | Pair_t (tl, tr, _, YesYes), (lx, rx), (ly, ry) ->
        (compare_comparable [@tailcall])
          tl
          (Compare_comparable (tr, rx, ry, k))
          lx
          ly
    | Or_t (tl, _, _, YesYes), L x, L y ->
        (compare_comparable [@tailcall]) tl k x y
    | Or_t _, L _, R _ -> -1
    | Or_t _, R _, L _ -> 1
    | Or_t (_, tr, _, YesYes), R x, R y ->
        (compare_comparable [@tailcall]) tr k x y
    | Option_t _, None, None -> (apply [@tailcall]) 0 k
    | Option_t _, None, Some _ -> -1
    | Option_t _, Some _, None -> 1
    | Option_t (t, _, Yes), Some x, Some y ->
        (compare_comparable [@tailcall]) t k x y
  and apply ret k =
    match (ret, k) with
    | 0, Compare_comparable (ty, x, y, k) ->
        (compare_comparable [@tailcall]) ty k x y
    | 0, Compare_comparable_return -> 0
    | ret, _ ->
        (* ret <> 0, we perform an early exit *)
        if Compare.Int.(ret > 0) then 1 else -1
  in
  fun t -> compare_comparable t Compare_comparable_return
