(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
(* Copyright (c) 2022-2023 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Options available for toggle per-block votes *)

type toggle_vote = Toggle_vote_on | Toggle_vote_off | Toggle_vote_pass

type toggle_votes = {
  liquidity_baking_vote : toggle_vote;
  adaptive_inflation_vote : toggle_vote;
}

val liquidity_baking_vote_encoding : toggle_vote Data_encoding.encoding

val adaptive_inflation_vote_encoding : toggle_vote Data_encoding.encoding

val toggle_votes_encoding : toggle_votes Data_encoding.encoding

(** [compute_new_liquidity_baking_ema ~toggle_vote old_ema] returns the value
    [new_ema] of the exponential moving average [old_ema] updated by the vote
    [toggle_vote] interpreted as a vote to deactivate the liquidity baking
    feature (Off increases the EMA).

    The EMA is updated as follows:
    - if [toggle_vote] is [Toggle_vote_pass] then [new_ema] = [old_ema],
    - if [toggle_vote] is [Toggle_vote_off], then [new_ema] = (1999 * ema[n] // 2000) + 1,000,000,
    - if [toggle_vote] is [Toggle_vote_on], then [new_ema] = (1999 * ema[n] // 2000).

    The multiplication is performed in [Z.t] to avoid overflows, division is
    rounded toward 1,000,000,000 (the middle of the interval).
    *)
val compute_new_liquidity_baking_ema :
  toggle_vote:toggle_vote -> Toggle_EMA.t -> Toggle_EMA.t

(** [compute_new_adaptive_inflation_ema ~toggle_vote old_ema] returns the value
    [new_ema] of the exponential moving average [old_ema] updated by the vote
    [toggle_vote] interpreted as a vote to activate the adaptive inflation
    feature (Off decreases the EMA).

    The EMA is updated as follows:
    - if [toggle_vote] is [Toggle_vote_pass] then [new_ema] = [old_ema],
    - if [toggle_vote] is [Toggle_vote_off], then [new_ema] = (1999 * ema[n] // 2000),
    - if [toggle_vote] is [Toggle_vote_on], then [new_ema] = (1999 * ema[n] // 2000) + 1,000,000.

    The multiplication is performed in [Z.t] to avoid overflows, division is
    rounded toward 1,000,000,000 (the middle of the interval).
    *)
val compute_new_adaptive_inflation_ema :
  toggle_vote:toggle_vote -> Toggle_EMA.t -> Toggle_EMA.t
