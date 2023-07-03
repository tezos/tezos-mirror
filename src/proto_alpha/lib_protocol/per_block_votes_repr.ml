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
  adaptive_inflation_vote : per_block_vote;
}

let per_block_vote_compact_encoding =
  let open Data_encoding in
  let open Compact in
  union
    ~union_tag_bits:2
    ~cases_tag_bits:0
    [
      case
        ~title:"per_block_vote_on"
        (payload (constant "on"))
        (function Per_block_vote_on -> Some () | _ -> None)
        (fun () -> Per_block_vote_on);
      case
        ~title:"per_block_vote_off"
        (payload (constant "off"))
        (function Per_block_vote_off -> Some () | _ -> None)
        (fun () -> Per_block_vote_off);
      case
        ~title:"per_block_vote_pass"
        (payload (constant "pass"))
        (function Per_block_vote_pass -> Some () | _ -> None)
        (fun () -> Per_block_vote_pass);
    ]

let liquidity_baking_vote_encoding =
  let open Data_encoding in
  def
    "liquidity_baking_vote"
    (Compact.make ~tag_size:`Uint8 per_block_vote_compact_encoding)

let adaptive_inflation_vote_encoding =
  let open Data_encoding in
  def
    "adaptive_inflation_vote"
    (Compact.make ~tag_size:`Uint8 per_block_vote_compact_encoding)

let per_block_votes_compact_encoding =
  let open Data_encoding in
  let open Compact in
  conv
    (fun {liquidity_baking_vote; adaptive_inflation_vote} ->
      (liquidity_baking_vote, adaptive_inflation_vote))
    (fun (liquidity_baking_vote, adaptive_inflation_vote) ->
      {liquidity_baking_vote; adaptive_inflation_vote})
    (obj2
       (req "liquidity_baking_vote" per_block_vote_compact_encoding)
       (req "adaptive_inflation_vote" per_block_vote_compact_encoding))

let per_block_votes_encoding =
  let open Data_encoding in
  def
    "per_block_votes"
    (Compact.make ~tag_size:`Uint8 per_block_votes_compact_encoding)

module Liquidity_baking_toggle_EMA = Votes_EMA_repr.Make (struct
  let baker_contribution = Z.of_int 500_000

  let ema_max = 2_000_000_000l
end)

module Adaptive_inflation_launch_EMA = Votes_EMA_repr.Make (struct
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

let compute_new_liquidity_baking_ema ~per_block_vote ema =
  match per_block_vote with
  | Per_block_vote_pass -> ema
  | Per_block_vote_off -> Liquidity_baking_toggle_EMA.update_ema_up ema
  | Per_block_vote_on -> Liquidity_baking_toggle_EMA.update_ema_down ema

let compute_new_adaptive_inflation_ema ~per_block_vote ema =
  match per_block_vote with
  | Per_block_vote_pass -> ema
  | Per_block_vote_off -> Adaptive_inflation_launch_EMA.update_ema_down ema
  | Per_block_vote_on -> Adaptive_inflation_launch_EMA.update_ema_up ema
