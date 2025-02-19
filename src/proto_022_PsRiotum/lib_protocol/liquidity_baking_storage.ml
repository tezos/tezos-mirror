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

open Per_block_votes_repr

let get_cpmm_address = Storage.Liquidity_baking.Cpmm_address.get

let get_toggle_ema ctxt =
  let open Lwt_result_syntax in
  let* ema = Storage.Liquidity_baking.Toggle_ema.get ctxt in
  Liquidity_baking_toggle_EMA.of_int32 ema

let on_cpmm_exists ctxt f =
  let open Lwt_result_syntax in
  let* cpmm_contract = get_cpmm_address ctxt in
  let*! exists =
    Contract_storage.exists ctxt (Contract_repr.Originated cpmm_contract)
  in
  match exists with
  | false ->
      (* do nothing if the cpmm is not found *)
      return (ctxt, [])
  | true -> f ctxt cpmm_contract

let update_toggle_ema ctxt ~per_block_vote =
  let open Lwt_result_syntax in
  let* old_ema = get_toggle_ema ctxt in
  let new_ema = compute_new_liquidity_baking_ema ~per_block_vote old_ema in
  let+ ctxt =
    Storage.Liquidity_baking.Toggle_ema.update
      ctxt
      (Liquidity_baking_toggle_EMA.to_int32 new_ema)
  in
  (ctxt, new_ema)

let check_ema_below_threshold ctxt ema =
  Liquidity_baking_toggle_EMA.(
    ema < Constants_storage.liquidity_baking_toggle_ema_threshold ctxt)

let on_subsidy_allowed ctxt ~per_block_vote f =
  let open Lwt_result_syntax in
  let* ctxt, toggle_ema = update_toggle_ema ctxt ~per_block_vote in
  if check_ema_below_threshold ctxt toggle_ema then
    let+ ctxt, operation_results = on_cpmm_exists ctxt f in
    (ctxt, operation_results, toggle_ema)
  else return (ctxt, [], toggle_ema)
