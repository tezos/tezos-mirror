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
    Subject:      Test liquidity baking subsidies, CPMM storage updates,
                  sunset shut off, and escape hatch shut off.
*)

open Protocol
open Test_tez

(* The script hash of https://gitlab.com/dexter2tz/dexter2tz/-/blob/liquidity_baking/dexter.liquidity_baking.mligo.tz *)
let expected_cpmm_hash =
  Script_expr_hash.of_b58check_exn
    "exprvEBYbxZruLZ9aUDEC9cUxn5KUj361xsaZXGfCxogFoKQ1er9Np"

(* The script hash of https://gitlab.com/dexter2tz/dexter2tz/-/blob/liquidity_baking/lqt_fa12.mligo.tz *)
let expected_lqt_hash =
  Script_expr_hash.of_b58check_exn
    "exprufAK15C2FCbxGLCEVXFe26p3eQdYuwZRk1morJUwy9NBUmEZVB"

(* Test that the scripts of the Liquidity Baking contracts (CPMM and LQT) have the expected hashes. *)
let liquidity_baking_origination () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun cpmm_address ->
  Context.Contract.script_hash (B blk) cpmm_address
  >>=? fun cpmm_hash ->
  Lwt.return @@ Environment.wrap_tzresult
  @@ Alpha_context.Contract.of_b58check "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
  >>=? fun lqt_address ->
  Context.Contract.script_hash (B blk) lqt_address
  >>=? fun lqt_hash ->
  Assert.equal
    ~loc:__LOC__
    Script_expr_hash.equal
    "Unexpected CPMM script."
    Script_expr_hash.pp
    cpmm_hash
    expected_cpmm_hash
  >>=? fun () ->
  Assert.equal
    ~loc:__LOC__
    Script_expr_hash.equal
    "Unexpected LQT script."
    Script_expr_hash.pp
    lqt_hash
    expected_lqt_hash
  >>=? fun () -> return_unit

(* Test that the CPMM address in storage is correct *)
let liquidity_baking_cpmm_address () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Assert.equal
    ~loc:__LOC__
    String.equal
    "CPMM address in storage is incorrect"
    Format.pp_print_string
    (Alpha_context.Contract.to_b58check liquidity_baking)
    "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"
  >>=? fun () -> return_unit

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

(* Test that [n] blocks after the liquidity baking sunset, the subsidy is not applied anymore.
   More precisely, after the sunset, the total amount credited to the subsidy is only proportional
   to the sunset level and in particular it does not depend on [n]. *)
let liquidity_baking_sunset_level n () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.get_constants (B blk)
  >>=? fun csts ->
  let sunset = csts.parametric.liquidity_baking_sunset_level in
  Context.Contract.balance (B blk) liquidity_baking
  >>=? fun old_balance ->
  Block.bake_n (Int32.to_int sunset + n) blk
  >>=? fun blk ->
  Context.get_liquidity_baking_subsidy (B blk)
  >>=? fun liquidity_baking_subsidy ->
  Tez.(liquidity_baking_subsidy *? Int64.(sub (of_int32 sunset) 1L))
  >>?= fun expected_credit ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk)
    liquidity_baking
    old_balance
    expected_credit
  >>=? fun () -> return_unit

(* Test that subsidy shuts off at correct escape level alternating baking [n_vote_false] blocks with liquidity_baking_escape_vote = false and [n_vote_true] blocks with it true followed by [bake_after_escape] blocks with it false. *)
(* Escape level is roughly 2*(log(1-1/(2*percent_flagging)) / log(0.999)) *)
let liquidity_baking_escape_hatch n_vote_false n_vote_true escape_level
    bake_after_escape () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.Contract.balance (B blk) liquidity_baking
  >>=? fun old_balance ->
  let rec bake_escaping blk i =
    if i < escape_level then
      Block.bake_n n_vote_false blk
      >>=? fun blk ->
      Block.bake_n ~liquidity_baking_escape_vote:true n_vote_true blk
      >>=? fun blk -> bake_escaping blk (i + n_vote_false + n_vote_true)
    else return blk
  in
  bake_escaping blk 0
  >>=? fun blk ->
  Block.bake_n bake_after_escape blk
  >>=? fun blk ->
  Context.get_liquidity_baking_subsidy (B blk)
  >>=? fun liquidity_baking_subsidy ->
  Tez.(liquidity_baking_subsidy *? Int64.of_int escape_level)
  >>?= fun expected_balance ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk)
    liquidity_baking
    old_balance
    expected_balance
  >>=? fun () -> return_unit

(* 100% of blocks have liquidity_baking_escape_vote = true *)
let liquidity_baking_escape_hatch_100 n () =
  liquidity_baking_escape_hatch 0 1 1387 n ()

(* 80% of blocks have liquidity_baking_escape_vote = true *)
let liquidity_baking_escape_hatch_80 n () =
  liquidity_baking_escape_hatch 1 4 1964 n ()

(* 60% of blocks have liquidity_baking_escape_vote = true *)
let liquidity_baking_escape_hatch_60 n () =
  liquidity_baking_escape_hatch 2 3 3590 n ()

(* 50% of blocks have liquidity_baking_escape_vote = true.
   Escape hatch should not be activated. *)
let liquidity_baking_escape_hatch_50 n () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.get_constants (B blk)
  >>=? fun csts ->
  let sunset = csts.parametric.liquidity_baking_sunset_level in
  Context.Contract.balance (B blk) liquidity_baking
  >>=? fun old_balance ->
  let rec bake_50_percent_escaping blk i =
    if i < Int32.to_int sunset + n then
      Block.bake blk
      >>=? fun blk ->
      Block.bake ~liquidity_baking_escape_vote:true blk
      >>=? fun blk -> bake_50_percent_escaping blk (i + 2)
    else return blk
  in
  bake_50_percent_escaping blk 0
  >>=? fun blk ->
  Context.get_liquidity_baking_subsidy (B blk)
  >>=? fun liquidity_baking_subsidy ->
  Tez.(liquidity_baking_subsidy *? Int64.(sub (of_int32 sunset) 1L))
  >>?= fun expected_balance ->
  Assert.balance_was_credited
    ~loc:__LOC__
    (B blk)
    liquidity_baking
    old_balance
    expected_balance
  >>=? fun () -> return_unit

(* Test that the escape EMA in block metadata is correct. *)
let liquidity_baking_escape_ema n_vote_false n_vote_true escape_level
    bake_after_escape expected_escape_ema () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  let rec bake_escaping blk i =
    if i < escape_level then
      Block.bake_n n_vote_false blk
      >>=? fun blk ->
      Block.bake_n ~liquidity_baking_escape_vote:true n_vote_true blk
      >>=? fun blk -> bake_escaping blk (i + n_vote_false + n_vote_true)
    else return blk
  in
  bake_escaping blk 0
  >>=? fun blk ->
  (* We only need to return the escape EMA at the end. *)
  Block.bake_n_with_liquidity_baking_escape_ema bake_after_escape blk
  >>=? fun (_blk, escape_ema) ->
  Assert.leq_int ~loc:__LOC__ (Int32.to_int escape_ema) expected_escape_ema
  >>=? fun () -> return_unit

(* With no bakers setting the escape vote, EMA should be zero. *)
let liquidity_baking_escape_ema_zero () =
  liquidity_baking_escape_ema 0 0 0 100 0 ()

(* The EMA should be not much over the threshold after the escape hatch has been activated. We add 50_000 to the constant to give room for the last update. *)
let liquidity_baking_escape_ema_threshold () =
  liquidity_baking_escape_ema 0 1 1387 1 1_050_000 ()

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
         \        \"KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN\"\n\
         \        \"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo\""
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

let liquidity_baking_balance_update () =
  Context.init 1
  >>=? fun (blk, _contracts) ->
  Context.get_liquidity_baking_cpmm_address (B blk)
  >>=? fun liquidity_baking ->
  Context.get_constants (B blk)
  >>=? fun csts ->
  let sunset = csts.parametric.liquidity_baking_sunset_level in
  let subsidy = csts.parametric.liquidity_baking_subsidy in
  Block.bake_n_with_all_balance_updates Int32.(to_int (add sunset 100l)) blk
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
    ((Int32.to_int sunset - 1) * Int64.to_int (Tez.to_mutez subsidy))
  >>=? fun () -> return_unit

let tests =
  [ Test_services.tztest
      "test liquidity baking script hashes"
      `Quick
      liquidity_baking_origination;
    Test_services.tztest
      "test liquidity baking cpmm is originated at the expected address"
      `Quick
      liquidity_baking_cpmm_address;
    Test_services.tztest
      "test liquidity baking subsidy is correct"
      `Quick
      (liquidity_baking_subsidies 64);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level when baking one block \
       longer"
      `Quick
      (liquidity_baking_sunset_level 1);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level when baking two blocks \
       longer"
      `Quick
      (liquidity_baking_sunset_level 2);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level when baking 100 blocks \
       longer"
      `Quick
      (liquidity_baking_sunset_level 100);
    Test_services.tztest
      "test liquidity baking escape hatch with 100% of bakers flagging when \
       baking one block longer"
      `Quick
      (liquidity_baking_escape_hatch_100 1);
    Test_services.tztest
      "test liquidity baking escape hatch with 100% of bakers flagging when \
       baking two blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_100 2);
    Test_services.tztest
      "test liquidity baking escape hatch with 100% of bakers flagging when \
       baking 100 blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_100 100);
    Test_services.tztest
      "test liquidity baking escape hatch with 80% of bakers flagging when \
       baking one block longer"
      `Quick
      (liquidity_baking_escape_hatch_80 1);
    Test_services.tztest
      "test liquidity baking escape hatch with 80% of bakers flagging when \
       baking two blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_80 2);
    Test_services.tztest
      "test liquidity baking escape hatch with 80% of bakers flagging when \
       baking 100 blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_80 100);
    Test_services.tztest
      "test liquidity baking escape hatch with 60% of bakers flagging when \
       baking one block longer"
      `Quick
      (liquidity_baking_escape_hatch_60 1);
    Test_services.tztest
      "test liquidity baking escape hatch with 60% of bakers flagging when \
       baking two blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_60 2);
    Test_services.tztest
      "test liquidity baking escape hatch with 60% of bakers flagging when \
       baking 100 blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_60 100);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level with escape hatch at \
       50% and baking one block longer"
      `Quick
      (liquidity_baking_escape_hatch_50 1);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level with escape hatch at \
       50% and baking two blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_50 2);
    Test_services.tztest
      "test liquidity baking shuts off at sunset level with escape hatch at \
       50% and baking 100 blocks longer"
      `Quick
      (liquidity_baking_escape_hatch_50 100);
    Test_services.tztest
      "test liquidity baking escape ema in block metadata is zero with no \
       bakers flagging."
      `Quick
      liquidity_baking_escape_ema_zero;
    Test_services.tztest
      "test liquidity baking escape ema is equal to the threshold after the \
       escape hatch has been activated"
      `Quick
      liquidity_baking_escape_ema_threshold;
    Test_services.tztest
      "test liquidity baking storage is updated"
      `Quick
      (liquidity_baking_storage 64);
    Test_services.tztest
      "test liquidity baking balance updates"
      `Quick
      liquidity_baking_balance_update ]
