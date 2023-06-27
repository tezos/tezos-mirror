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

let toggle_vote_compact_encoding =
  let open Data_encoding in
  let open Compact in
  union
    ~union_tag_bits:2
    ~cases_tag_bits:0
    [
      case
        ~title:"toggle_data_vote_on"
        (payload (constant "on"))
        (function Toggle_vote_on -> Some () | _ -> None)
        (fun () -> Toggle_vote_on);
      case
        ~title:"toggle_data_vote_off"
        (payload (constant "off"))
        (function Toggle_vote_off -> Some () | _ -> None)
        (fun () -> Toggle_vote_off);
      case
        ~title:"toggle_data_vote_pass"
        (payload (constant "pass"))
        (function Toggle_vote_pass -> Some () | _ -> None)
        (fun () -> Toggle_vote_pass);
    ]

let liquidity_baking_vote_encoding =
  let open Data_encoding in
  def
    "liquidity_baking_vote"
    (Compact.make ~tag_size:`Uint8 toggle_vote_compact_encoding)

let adaptive_inflation_vote_encoding =
  let open Data_encoding in
  def
    "adaptive_inflation_vote"
    (Compact.make ~tag_size:`Uint8 toggle_vote_compact_encoding)

let toggle_votes_compact_encoding =
  let open Data_encoding in
  let open Compact in
  conv
    (fun {liquidity_baking_vote; adaptive_inflation_vote} ->
      (liquidity_baking_vote, adaptive_inflation_vote))
    (fun (liquidity_baking_vote, adaptive_inflation_vote) ->
      {liquidity_baking_vote; adaptive_inflation_vote})
    (obj2
       (req "liquidity_baking_vote" toggle_vote_compact_encoding)
       (req "adaptive_inflation_vote" toggle_vote_compact_encoding))

let toggle_votes_encoding =
  let open Data_encoding in
  def
    "toggle_votes"
    (Compact.make ~tag_size:`Uint8 toggle_votes_compact_encoding)

module Liquidity_baking_toggle_EMA = Toggle_EMA.Make (struct
  let baker_contribution = Z.of_int 500_000

  let ema_max = 2_000_000_000l
end)

module Adaptive_inflation_launch_EMA = Toggle_EMA.Make (struct
  (* The baker_contribution parameter of the adaptive inflation
     activation vote was chosen so that 2 weeks are needed to move
     the EMA from 0% to 50% when all bakers vote On.

     This was computed using the following formula:

     baker_contrib = (1/2) * ema_max * (1 - 2^(-1/k))

     where k is the number of blocks in 2 weeks (which is 80640).

     Because of a small accumulation of rounding errors, two more
     blocks are actually needed. *)
  let baker_contribution = Z.of_int 8595

  let ema_max = 2_000_000_000l
end)

let compute_new_liquidity_baking_ema ~toggle_vote ema =
  match toggle_vote with
  | Toggle_vote_pass -> ema
  | Toggle_vote_off -> Liquidity_baking_toggle_EMA.update_ema_up ema
  | Toggle_vote_on -> Liquidity_baking_toggle_EMA.update_ema_down ema

let compute_new_adaptive_inflation_ema ~toggle_vote ema =
  match toggle_vote with
  | Toggle_vote_pass -> ema
  | Toggle_vote_off -> Adaptive_inflation_launch_EMA.update_ema_down ema
  | Toggle_vote_on -> Adaptive_inflation_launch_EMA.update_ema_up ema
