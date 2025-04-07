(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context

(** This is a simulation of the CPMM contract, as implemented in mligo
    in [src/proto_alpha/lib_protocol/contracts/cpmm.mligo]. The
    interested reader should look for comments in this file to gain a
    better understanding of the contract logic. *)
module Simulate_raw = struct
  let mutez_to_natural t = Z.of_int64 (Tez.to_mutez t)

  let natural_to_mutez n = Tez.of_mutez_exn (Z.to_int64 n)

  let addLiquidity ~tokenPool ~xtzPool ~lqtTotal ~amount =
    let xtzPool = mutez_to_natural xtzPool in
    let nat_amount = mutez_to_natural amount in
    let lqt_minted = Z.(nat_amount * lqtTotal / xtzPool) in
    let tokens_deposited = Z.(cdiv (nat_amount * tokenPool) xtzPool) in
    (lqt_minted, tokens_deposited)

  let removeLiquidity ~tokenPool ~xtzPool ~lqtTotal ~lqtBurned =
    let xtz_withdrawn =
      natural_to_mutez Z.(lqtBurned * mutez_to_natural xtzPool / lqtTotal)
    in
    let tokens_withdrawn = Z.(lqtBurned * tokenPool / lqtTotal) in
    (xtz_withdrawn, tokens_withdrawn)

  let tokenToXtz ~tokenPool ~xtzPool ~tokensSold =
    let fee = Z.of_int 999 in
    let xtz_bought_nat =
      Z.(
        tokensSold * fee * mutez_to_natural xtzPool
        / ((tokenPool * of_int 1000) + (tokensSold * fee)))
    in
    let bought = Z.(xtz_bought_nat * of_int 999 / of_int 1000) in
    (natural_to_mutez bought, xtz_bought_nat)

  let xtzToToken ~tokenPool ~xtzPool ~amount =
    let fee = Z.of_int 999 in
    let xtzPool = mutez_to_natural xtzPool in
    let nat_amount = mutez_to_natural amount in
    let amount_net_burn = Z.(nat_amount * Z.of_int 999 / Z.of_int 1000) in
    let tokens_bought =
      Z.(
        amount_net_burn * fee * tokenPool
        / ((xtzPool * Z.of_int 1000) + (amount_net_burn * fee)))
    in
    (tokens_bought, amount_net_burn)

  let tokenToToken ~tokenPool ~xtzPool ~tokensSold =
    let fee = Z.of_int 999 in
    let xtz_bought_nat =
      Z.(
        tokensSold * fee * mutez_to_natural xtzPool
        / ((tokenPool * of_int 1000) + (tokensSold * fee)))
    in
    let xtz_bought_net_burn = Z.(xtz_bought_nat * of_int 999 / of_int 1000) in
    (natural_to_mutez xtz_bought_net_burn, xtz_bought_nat)
end

module Simulate = struct
  open Cpmm_repr.Storage

  let addLiquidity {tokenPool; xtzPool; lqtTotal; _} amount =
    Simulate_raw.addLiquidity ~xtzPool ~tokenPool ~lqtTotal ~amount

  let removeLiquidity {tokenPool; xtzPool; lqtTotal; _} lqtBurned =
    Simulate_raw.removeLiquidity ~tokenPool ~xtzPool ~lqtTotal ~lqtBurned

  let tokenToXtz {tokenPool; xtzPool; _} tokensSold =
    Simulate_raw.tokenToXtz ~tokenPool ~xtzPool ~tokensSold

  let xtzToToken {tokenPool; xtzPool; _} amount =
    Simulate_raw.xtzToToken ~tokenPool ~xtzPool ~amount

  let tokenToToken {tokenPool; xtzPool; _} tokensSold =
    Simulate_raw.tokenToToken ~tokenPool ~xtzPool ~tokensSold
end
