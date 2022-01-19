(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
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

let get_cpmm_address = Storage.Liquidity_baking.Cpmm_address.get

let get_escape_ema = Storage.Liquidity_baking.Escape_ema.get

type escape_ema = Int32.t

let on_cpmm_exists ctxt f =
  get_cpmm_address ctxt >>=? fun cpmm_contract ->
  Contract_storage.exists ctxt cpmm_contract >>=? function
  | false ->
      (* do nothing if the cpmm is not found *)
      return (ctxt, [])
  | true -> f ctxt cpmm_contract

let on_below_sunset ctxt f =
  let sunset_level = Constants_storage.liquidity_baking_sunset_level ctxt in
  let level = Raw_level_repr.to_int32 (Level_storage.current ctxt).level in
  if Compare.Int32.(level >= sunset_level) then return (ctxt, [])
  else on_cpmm_exists ctxt f

(* ema starts at zero
   ema[n+1] = (1999 * ema[n] // 2000) + (1000 if escape_vote[n] else 0)
   where escape_vote is protocol_data.contents.liquidity_baking_escape_vote *)
let update_escape_ema ctxt ~escape_vote =
  get_escape_ema ctxt >>=? fun old_ema ->
  (* if ema is over threshold, we don't update it because liquidity baking is permanently off *)
  if
    Compare.Int32.(
      old_ema < Constants_storage.liquidity_baking_escape_ema_threshold ctxt)
  then
    let new_ema =
      Int32.(
        add
          (div (mul 1999l old_ema) 2000l)
          (match escape_vote with
          | Block_header_repr.LB_off -> 1000l
          | LB_on -> 0l))
    in
    Storage.Liquidity_baking.Escape_ema.update ctxt new_ema >|=? fun ctxt ->
    (ctxt, new_ema, false)
  else return (ctxt, old_ema, true)

let on_subsidy_allowed ctxt ~escape_vote f =
  update_escape_ema ctxt ~escape_vote
  >>=? fun (ctxt, escape_ema, threshold_reached) ->
  (* liquidity baking permanently shuts off if threshold is reached once *)
  if threshold_reached then return (ctxt, [], escape_ema)
  else
    on_below_sunset ctxt f >|=? fun (ctxt, operation_results) ->
    (ctxt, operation_results, escape_ema)
