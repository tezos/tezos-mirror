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

(** Testing
    -------
    Component:    liquidity baking
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^liquidity baking$"
    Subject:      Test liquidity baking subsidies and CPMM storage updates.
*)

open Protocol
open Test_tez

(* Test that after [n] blocks, the liquidity baking CPMM contract is credited [n] times the subsidy amount. *)
let liquidity_baking_subsidies n () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.Contract.balance (B blk) liquidity_baking
  >>=? fun old_balance ->
  Block.bake_n n blk
  >>=? fun blk ->
  Context.get_liquidity_baking_subsidy (B blk)
  >>=? fun liquidity_baking_subsidy ->
  Tez.(liquidity_baking_subsidy *? Int64.(of_int n))
  >>?= fun expected_credit ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk)
    liquidity_baking
    old_balance
    expected_credit
  >>=? fun () -> return_unit

let liquidity_baking_storage n () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.get_liquidity_baking_subsidy (B blk)
  >>=? fun subsidy ->
  let expected_storage =
    Expr.from_string
      (Printf.sprintf
         "Pair 1\n\
         \        %d\n\
         \        100\n\
         \        \"KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG\"\n\
         \        \"KT1SLWhfqPtQq7f4zLomh8BNgDeprF9B6d2M\""
         (100 + (n * Int64.to_int (Tez.to_mutez subsidy))))
  in
  Block.bake_n n blk
  >>=? fun blk ->
  Context.Contract.storage (B blk) liquidity_baking
  >>=? fun storage ->
  let to_string expr =
    Format.asprintf "%a" Michelson_v1_printer.print_expr expr
  in
  Assert.equal
    ~loc:__LOC__
    String.equal
    "Storage isn't equal"
    Format.pp_print_string
    (to_string storage)
    (to_string expected_storage)
  >>=? fun () -> return_unit

let liquidity_baking_balance_update n () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.get_constants (B blk)
  >>=? fun csts ->
  let subsidy = csts.parametric.liquidity_baking_subsidy in
  Block.bake_n_with_all_balance_updates n blk
  >>=? fun (_blk, balance_updates) ->
  let liquidity_baking_updates =
    List.filter
      (fun el ->
        match el with
        | ( Alpha_context.Receipt.Contract contract,
            Alpha_context.Receipt.Credited _,
            Alpha_context.Receipt.Subsidy ) ->
            Alpha_context.Contract.(contract = liquidity_baking)
        | _ ->
            false)
      balance_updates
  in
  List.fold_left_e
    (fun accum (_, update, _) ->
      match update with
      | Alpha_context.Receipt.Credited x ->
          Tez.(accum +? x)
      | Alpha_context.Receipt.Debited _ ->
          assert false)
    Tez.(of_int 0)
    liquidity_baking_updates
  >>?= fun credits ->
  Assert.equal_int
    ~loc:__LOC__
    (Int64.to_int (Tez.to_mutez credits))
    (n * Int64.to_int (Tez.to_mutez subsidy))
  >>=? fun () -> return_unit

let tests =
  [ Test_services.tztest
      "test liquidity baking subsidy is correct"
      `Quick
      (liquidity_baking_subsidies 64);
    Test_services.tztest
      "test liquidity baking storage is updated"
      `Quick
      (liquidity_baking_storage 64);
    Test_services.tztest
      "test liquidity baking balance updates"
      `Quick
      (liquidity_baking_balance_update 64) ]
