(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
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

open Liquidity_baking_repr

let get_cpmm_address = Storage.Liquidity_baking.Cpmm_address.get

let get_escape_ema ctxt =
  Storage.Liquidity_baking.Escape_ema.get ctxt >>=? fun ema ->
  Escape_EMA.of_int32 ema

let on_cpmm_exists ctxt f =
  get_cpmm_address ctxt >>=? fun cpmm_contract ->
  Contract_storage.exists ctxt cpmm_contract >>=? function
  | false ->
      (* do nothing if the cpmm is not found *)
      return (ctxt, [])
  | true -> f ctxt cpmm_contract

let check_below_sunset ctxt =
  let sunset_level = Constants_storage.liquidity_baking_sunset_level ctxt in
  let level = Raw_level_repr.to_int32 (Level_storage.current ctxt).level in
  Compare.Int32.(level < sunset_level)

let update_escape_ema ctxt ~escape_vote =
  get_escape_ema ctxt >>=? fun old_ema ->
  let new_ema = compute_new_ema ~escape_vote old_ema in
  Storage.Liquidity_baking.Escape_ema.update ctxt (Escape_EMA.to_int32 new_ema)
  >|=? fun ctxt -> (ctxt, new_ema)

let check_ema_below_threshold ctxt ema =
  Escape_EMA.(
    ema < Constants_storage.liquidity_baking_escape_ema_threshold ctxt)

let on_subsidy_allowed ctxt ~escape_vote f =
  update_escape_ema ctxt ~escape_vote >>=? fun (ctxt, escape_ema) ->
  if check_ema_below_threshold ctxt escape_ema && check_below_sunset ctxt then
    on_cpmm_exists ctxt f >|=? fun (ctxt, operation_results) ->
    (ctxt, operation_results, escape_ema)
  else return (ctxt, [], escape_ema)
