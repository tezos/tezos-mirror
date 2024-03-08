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

(** Options available for per-block votes *)

type per_block_vote =
  | Per_block_vote_on
  | Per_block_vote_off
  | Per_block_vote_pass

type per_block_votes = {
  liquidity_baking_vote : per_block_vote;
  adaptive_issuance_vote : per_block_vote;
}

val liquidity_baking_vote_encoding : per_block_vote Data_encoding.encoding

val adaptive_issuance_vote_encoding : per_block_vote Data_encoding.encoding

val per_block_votes_encoding : per_block_votes Data_encoding.encoding

module Liquidity_baking_toggle_EMA : Votes_EMA_repr.T

module Adaptive_issuance_launch_EMA : Votes_EMA_repr.T

(** [compute_new_liquidity_baking_ema ~per_block_vote old_ema] returns the value
    [new_ema] of the exponential moving average [old_ema] updated by the vote
    [per_block_vote] interpreted as a vote to deactivate the liquidity baking
    feature (Off increases the EMA).

    The EMA is updated as follows:
    - if [per_block_vote] is [Per_block_vote_pass] then [new_ema] = [old_ema],
    - if [per_block_vote] is [Per_block_vote_off], then [new_ema] = (1999 * ema[n] // 2000) + 1,000,000,
    - if [per_block_vote] is [Per_block_vote_on], then [new_ema] = (1999 * ema[n] // 2000).

    The multiplication is performed in [Z.t] to avoid overflows, division is
    rounded toward 1,000,000,000 (the middle of the interval).
    *)
val compute_new_liquidity_baking_ema :
  per_block_vote:per_block_vote ->
  Liquidity_baking_toggle_EMA.t ->
  Liquidity_baking_toggle_EMA.t

(** [compute_new_adaptive_issuance_ema ~per_block_vote old_ema] returns the value
    [new_ema] of the exponential moving average [old_ema] updated by the vote
    [per_block_vote] interpreted as a vote to activate the adaptive issuance
    feature (Off decreases the EMA).

    The EMA is updated as follows:
    - if [per_block_vote] is [Per_block_vote_pass] then [new_ema] = [old_ema],
    - if [per_block_vote] is [Per_block_vote_off], then [new_ema] = (1999 * ema[n] // 2000),
    - if [per_block_vote] is [Per_block_vote_on], then [new_ema] = (1999 * ema[n] // 2000) + 1,000,000.

    The multiplication is performed in [Z.t] to avoid overflows, division is
    rounded toward 1,000,000,000 (the middle of the interval).
    *)
val compute_new_adaptive_issuance_ema :
  per_block_vote:per_block_vote ->
  Adaptive_issuance_launch_EMA.t ->
  Adaptive_issuance_launch_EMA.t

module Internal_for_tests : sig
  (* Maximum value for EMA representation (both LB and AI) *)
  val ema_max : Int32.t
end
