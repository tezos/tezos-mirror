(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Script_tc_errors
open Script_typed_ir

(**
   [deduce_comparable_type_size ~remaining ty] returns [remaining] minus the size of type [ty].
   It is guaranteed to not grow the stack by more than [remaining] non-tail calls.
*)
let rec deduce_comparable_type_size :
    type t. remaining:int -> t comparable_ty -> int =
 fun ~remaining ty ->
  if Compare.Int.(remaining < 0) then remaining
  else
    match ty with
    | Unit_key _ | Never_key _ | Int_key _ | Nat_key _ | Signature_key _
    | String_key _ | Bytes_key _ | Mutez_key _ | Bool_key _ | Key_hash_key _
    | Key_key _ | Timestamp_key _ | Chain_id_key _ | Address_key _ ->
        remaining - 1
    | Pair_key ((t1, _), (t2, _), _) ->
        let remaining = remaining - 1 in
        let remaining = deduce_comparable_type_size ~remaining t1 in
        deduce_comparable_type_size ~remaining t2
    | Union_key ((t1, _), (t2, _), _) ->
        let remaining = remaining - 1 in
        let remaining = deduce_comparable_type_size ~remaining t1 in
        deduce_comparable_type_size ~remaining t2
    | Option_key (t, _) ->
        let remaining = remaining - 1 in
        deduce_comparable_type_size ~remaining t

(**
   [deduce_type_size ~remaining ty] returns [remaining] minus the size of type [ty].
   It is guaranteed to not grow the stack by more than [remaining] non-tail calls.
*)
let rec deduce_type_size : type t. remaining:int -> t ty -> int =
 fun ~remaining ty ->
  match ty with
  | Unit_t _ | Int_t _ | Nat_t _ | Signature_t _ | Bytes_t _ | String_t _
  | Mutez_t _ | Key_hash_t _ | Key_t _ | Timestamp_t _ | Address_t _ | Bool_t _
  | Operation_t _ | Chain_id_t _ | Never_t _ | Bls12_381_g1_t _
  | Bls12_381_g2_t _ | Bls12_381_fr_t _ | Sapling_transaction_t _
  | Sapling_state_t _ ->
      remaining - 1
  | Pair_t ((l, _, _), (r, _, _), _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining l in
      deduce_type_size ~remaining r
  | Union_t ((l, _), (r, _), _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining l in
      deduce_type_size ~remaining r
  | Lambda_t (arg, ret, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining arg in
      deduce_type_size ~remaining ret
  | Option_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining t
  | List_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining t
  | Ticket_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_comparable_type_size ~remaining t
  | Set_t (k, _) ->
      let remaining = remaining - 1 in
      deduce_comparable_type_size ~remaining k
  | Map_t (k, v, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_comparable_type_size ~remaining k in
      deduce_type_size ~remaining v
  | Big_map_t (k, v, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_comparable_type_size ~remaining k in
      deduce_type_size ~remaining v
  | Contract_t (arg, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining arg

let check_type_size_internal ~loc ~maximum_type_size ty =
  if Compare.Int.(deduce_type_size ~remaining:maximum_type_size ty >= 0) then
    ok_unit
  else error (Type_too_large (loc, maximum_type_size))

let rec check_type_size_of_stack_head :
    type a s.
    loc:Script.location ->
    maximum_type_size:int ->
    (a, s) stack_ty ->
    up_to:int ->
    unit tzresult =
 fun ~loc ~maximum_type_size stack ~up_to ->
  if Compare.Int.(up_to <= 0) then ok_unit
  else
    match stack with
    | Bot_t -> ok_unit
    | Item_t (head, tail, _annot) ->
        check_type_size_internal ~loc ~maximum_type_size head >>? fun () ->
        (check_type_size_of_stack_head [@tailcall])
          ~loc
          ~maximum_type_size
          tail
          ~up_to:(up_to - 1)

let check_comparable_type_size ~legacy ctxt ~loc ty =
  if legacy then ok_unit
  else
    let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
    if
      Compare.Int.(
        deduce_comparable_type_size ~remaining:maximum_type_size ty >= 0)
    then ok_unit
    else error (Type_too_large (loc, maximum_type_size))

let check_type_size ~legacy ctxt ~loc ty =
  if legacy then ok_unit
  else
    let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
    check_type_size_internal ~loc ~maximum_type_size ty
