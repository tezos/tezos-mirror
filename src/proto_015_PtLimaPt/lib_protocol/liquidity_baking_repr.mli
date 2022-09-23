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

(** Options available for the Liquidity Baking per-block vote *)

type liquidity_baking_toggle_vote = LB_on | LB_off | LB_pass

val liquidity_baking_toggle_vote_encoding :
  liquidity_baking_toggle_vote Data_encoding.encoding

(** Exponential moving average of toggle votes. Represented as an int32 between
    0 and 2,000,000. It is an exponential moving average of the [LB_off] votes
    over a window of the most recent 2000 blocks that did not vote [LB_pass]. *)

module Toggle_EMA : sig
  type t

  val of_int32 : Int32.t -> t tzresult Lwt.t

  val zero : t

  val to_int32 : t -> Int32.t

  val encoding : t Data_encoding.t

  val ( < ) : t -> Int32.t -> bool
end

(** [compute_new_ema ~toggle_vote old_ema] returns the value [new_ema] of the
    exponential moving average [old_ema] updated by the vote [toggle_vote].

    It is updated as follows:
    - if [toggle_vote] is [LB_pass] then [new_ema] = [old_ema],
    - if [toggle_vote] is [LB_off], then [new_ema] = (1999 * ema[n] // 2000) + 1,000,000,
    - if [toggle_vote] is [LB_on], then [new_ema] = (1999 * ema[n] // 2000).

    The multiplication is performed in [Z.t] to avoid overflows, division is
    rounded toward 1,000,000,000 (the middle of the interval).
    *)
val compute_new_ema :
  toggle_vote:liquidity_baking_toggle_vote -> Toggle_EMA.t -> Toggle_EMA.t
