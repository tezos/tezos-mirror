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
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                   -- --file test_liquidity_baking.ml
    Subject:      Test liquidity baking subsidies, CPMM storage updates,
                  and toggle vote.
*)

open Liquidity_baking_machine
open Protocol
open Test_tez

let generate_init_state () =
  let open Lwt_result_syntax in
  let cpmm_min_xtz_balance = 10_000_000L in
  let cpmm_min_tzbtc_balance = 100_000 in
  let accounts_balances =
    [
      {xtz = 1_000_000L; tzbtc = 1; liquidity = 100};
      {xtz = 1_000L; tzbtc = 1000; liquidity = 100};
      {xtz = 40_000_000L; tzbtc = 350000; liquidity = 300};
    ]
  in
  let* _, _ =
    ValidationMachine.build
      {cpmm_min_xtz_balance; cpmm_min_tzbtc_balance; accounts_balances}
  in
  return_unit

(* The script hash of

   https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/dexter.liquidity_baking.mligo.tz
*)
let expected_cpmm_hash =
  Script_expr_hash.of_b58check_exn
    "exprvEBYbxZruLZ9aUDEC9cUxn5KUj361xsaZXGfCxogFoKQ1er9Np"

(* The script hash of

   https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/lqt_fa12.mligo.tz
*)
let expected_lqt_hash =
  Script_expr_hash.of_b58check_exn
    "exprufAK15C2FCbxGLCEVXFe26p3eQdYuwZRk1morJUwy9NBUmEZVB"

(* Test that the scripts of the Liquidity Baking contracts (CPMM and LQT) have the expected hashes. *)
let liquidity_baking_origination () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* cpmm_address = Context.get_liquidity_baking_cpmm_address (B blk) in
  let* cpmm_hash = Context.Contract.script_hash (B blk) cpmm_address in
  let lqt_address =
    Contract_hash.of_b58check_exn "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
  in
  let* lqt_hash = Context.Contract.script_hash (B blk) lqt_address in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Script_expr_hash.equal
      "Unexpected CPMM script."
      Script_expr_hash.pp
      cpmm_hash
      expected_cpmm_hash
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Script_expr_hash.equal
      "Unexpected LQT script."
      Script_expr_hash.pp
      lqt_hash
      expected_lqt_hash
  in
  return_unit

(* Test that the CPMM address in storage is correct *)
let liquidity_baking_cpmm_address () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let* () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "CPMM address in storage is incorrect"
      Format.pp_print_string
      (Contract_hash.to_b58check liquidity_baking)
      "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"
  in
  return_unit

(* Test that after [n] blocks, the liquidity baking CPMM contract is credited [n] times the subsidy amount. *)
let liquidity_baking_subsidies n () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let liquidity_baking = Alpha_context.Contract.Originated liquidity_baking in
  let* old_balance = Context.Contract.balance (B blk) liquidity_baking in
  let* blk = Block.bake_n n blk in
  let* liquidity_baking_subsidy =
    Context.get_liquidity_baking_subsidy (B blk)
  in
  let*? expected_credit = liquidity_baking_subsidy *? Int64.(of_int n) in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (B blk)
      liquidity_baking
      old_balance
      expected_credit
  in
  return_unit

(* Test that subsidy shuts off at correct level alternating baking
   blocks with liquidity_baking_toggle_vote set to [Per_block_vote_on], [Per_block_vote_off], and [Per_block_vote_pass] followed by [bake_after_toggle] blocks with it set to [Per_block_vote_pass]. *)
(* Expected level is roughly 2*(log(1-1/(2*p)) / log(0.999)) where [p] is the proportion [Per_block_vote_off / (Per_block_vote_on + Per_block_vote_off)]. *)
let liquidity_baking_toggle ~n_vote_on ~n_vote_off ~n_vote_pass expected_level
    bake_after () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let liquidity_baking = Alpha_context.Contract.Originated liquidity_baking in
  let* old_balance = Context.Contract.balance (B blk) liquidity_baking in
  let* liquidity_baking_subsidy =
    Context.get_liquidity_baking_subsidy (B blk)
  in
  let rec bake_stopping blk i =
    if i < expected_level then
      let* blk =
        Block.bake_n
          ~liquidity_baking_toggle_vote:Per_block_vote_on
          n_vote_on
          blk
      in
      let* blk =
        Block.bake_n
          ~liquidity_baking_toggle_vote:Per_block_vote_off
          n_vote_off
          blk
      in
      let* blk =
        Block.bake_n
          ~liquidity_baking_toggle_vote:Per_block_vote_pass
          n_vote_pass
          blk
      in
      bake_stopping blk (i + n_vote_on + n_vote_off + n_vote_pass)
    else return blk
  in
  let* blk = bake_stopping blk 0 in
  let* balance = Context.Contract.balance (B blk) liquidity_baking in
  let* blk =
    Block.bake_n
      ~liquidity_baking_toggle_vote:Per_block_vote_pass
      bake_after
      blk
  in
  let* () = Assert.balance_is ~loc:__LOC__ (B blk) liquidity_baking balance in
  let*? expected_final_balance =
    liquidity_baking_subsidy *? Int64.of_int (expected_level - 1)
  in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (B blk)
      liquidity_baking
      old_balance
      expected_final_balance
  in
  return_unit

(* 100% of blocks have liquidity_baking_toggle_vote = Per_block_vote_off *)
let liquidity_baking_toggle_100 n () =
  liquidity_baking_toggle ~n_vote_on:0 ~n_vote_off:1 ~n_vote_pass:0 1386 n ()

(* 80% of blocks have liquidity_baking_toggle_vote = Per_block_vote_off *)
let liquidity_baking_toggle_80 n () =
  liquidity_baking_toggle ~n_vote_on:1 ~n_vote_off:4 ~n_vote_pass:0 1963 n ()

(* 60% of blocks have liquidity_baking_toggle_vote = Per_block_vote_off *)
let liquidity_baking_toggle_60 n () =
  liquidity_baking_toggle ~n_vote_on:2 ~n_vote_off:3 ~n_vote_pass:0 3583 n ()

(* 50% of blocks have liquidity_baking_toggle_vote = Per_block_vote_off.
   Subsidy should not be stopped.
   Bakes until 100 blocks after the test sunset level of 4096 used in previous protocols. *)
let liquidity_baking_toggle_50 () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let liquidity_baking = Alpha_context.Contract.Originated liquidity_baking in
  let* old_balance = Context.Contract.balance (B blk) liquidity_baking in
  let* liquidity_baking_subsidy =
    Context.get_liquidity_baking_subsidy (B blk)
  in
  let rec bake_stopping blk i =
    if i < 4196 then
      let* blk =
        Block.bake ~liquidity_baking_toggle_vote:Per_block_vote_on blk
      in
      let* blk =
        Block.bake ~liquidity_baking_toggle_vote:Per_block_vote_off blk
      in
      bake_stopping blk (i + 2)
    else return blk
  in
  let* blk = bake_stopping blk 0 in
  let* balance = Context.Contract.balance (B blk) liquidity_baking in
  let* () = Assert.balance_is ~loc:__LOC__ (B blk) liquidity_baking balance in
  let*? expected_final_balance =
    liquidity_baking_subsidy *? Int64.of_int 4196
  in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (B blk)
      liquidity_baking
      old_balance
      expected_final_balance
  in
  return_unit

(* Test that the subsidy can restart if Per_block_vote_on votes regain majority.
   Bake n_votes with Per_block_vote_off, check that the subsidy is paused, bake
   n_votes with Per_block_vote_on, check that the subsidy flows.
*)
let liquidity_baking_restart n_votes n () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let liquidity_baking = Alpha_context.Contract.Originated liquidity_baking in
  let* blk =
    Block.bake_n ~liquidity_baking_toggle_vote:Per_block_vote_off n_votes blk
  in
  let* balance_when_paused =
    Context.Contract.balance (B blk) liquidity_baking
  in
  let* blk =
    Block.bake_n ~liquidity_baking_toggle_vote:Per_block_vote_pass n blk
  in
  let* () =
    Assert.balance_is ~loc:__LOC__ (B blk) liquidity_baking balance_when_paused
  in
  let* blk =
    Block.bake_n ~liquidity_baking_toggle_vote:Per_block_vote_on n_votes blk
  in
  let* balance_when_restarted =
    Context.Contract.balance (B blk) liquidity_baking
  in
  let* blk =
    Block.bake_n ~liquidity_baking_toggle_vote:Per_block_vote_pass n blk
  in
  let* liquidity_baking_subsidy =
    Context.get_liquidity_baking_subsidy (B blk)
  in
  let*? expected_balance = liquidity_baking_subsidy *? Int64.of_int n in
  let* () =
    Assert.balance_was_credited
      ~loc:__LOC__
      (B blk)
      liquidity_baking
      balance_when_restarted
      expected_balance
  in
  return_unit

(* Test that the toggle EMA in block metadata is correct. *)
let liquidity_baking_toggle_ema n_vote_on n_vote_off level bake_after
    expected_toggle_ema () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let rec bake_escaping blk i =
    if i < level then
      let* blk =
        Block.bake_n
          ~liquidity_baking_toggle_vote:Per_block_vote_on
          n_vote_on
          blk
      in
      let* blk =
        Block.bake_n
          ~liquidity_baking_toggle_vote:Per_block_vote_off
          n_vote_off
          blk
      in
      bake_escaping blk (i + n_vote_on + n_vote_off)
    else return blk
  in
  let* blk = bake_escaping blk 0 in
  (* We only need to return the toggle EMA at the end. *)
  let* _blk, toggle_ema =
    Block.bake_n_with_liquidity_baking_toggle_ema bake_after blk
  in
  let* () =
    Assert.leq_int
      ~loc:__LOC__
      (toggle_ema
     |> Alpha_context.Per_block_votes.Liquidity_baking_toggle_EMA.to_int32
     |> Int32.to_int)
      expected_toggle_ema
  in
  return_unit

(* With no bakers setting the toggle vote, EMA should be zero. *)
let liquidity_baking_toggle_ema_zero () =
  liquidity_baking_toggle_ema 0 0 0 100 0 ()

(* The EMA should be not much over the threshold after the subsidy has been stopped by a toggle vote. We add 1_000_000 to the constant to give room for the last update. *)
let liquidity_baking_toggle_ema_threshold () =
  liquidity_baking_toggle_ema 0 1 1386 1 1_001_000_000 ()

let liquidity_baking_storage n () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let* subsidy = Context.get_liquidity_baking_subsidy (B blk) in
  let expected_storage =
    Expr.from_string
      (Printf.sprintf
         "Pair 1\n\
         \        %d\n\
         \        100\n\
         \        \"KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN\"\n\
         \        \"KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo\""
         (100 + (n * Int64.to_int (to_mutez subsidy))))
  in
  let* blk = Block.bake_n n blk in
  let* storage = Context.Contract.storage (B blk) liquidity_baking in
  let to_string expr =
    Format.asprintf "%a" Michelson_v1_printer.print_expr expr
  in
  let* () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "Storage isn't equal"
      Format.pp_print_string
      (to_string storage)
      (to_string expected_storage)
  in
  return_unit

let liquidity_baking_balance_update () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 ~consensus_threshold:0 () in
  let* liquidity_baking = Context.get_liquidity_baking_cpmm_address (B blk) in
  let* subsidy = Context.get_liquidity_baking_subsidy (B blk) in
  let* _blk, balance_updates = Block.bake_n_with_all_balance_updates 128 blk in
  let liquidity_baking_updates =
    List.filter
      (fun el ->
        match el with
        | Alpha_context.Receipt.Balance_update_item
            (Contract (Originated contract), Credited _, Subsidy) ->
            Contract_hash.(contract = liquidity_baking)
        | _ -> false)
      balance_updates
  in
  let*? credits =
    List.fold_left_e
      (fun accum
           (Alpha_context.Receipt.Balance_update_item (balance, update, _)) ->
        match Alpha_context.Receipt.token_of_balance balance with
        | Tez -> (
            match update with
            | Credited x -> accum +? x
            | Debited _ -> assert false)
        | _ -> assert false)
      (of_int 0)
      liquidity_baking_updates
  in
  let* () =
    Assert.equal_int
      ~loc:__LOC__
      (Int64.to_int (to_mutez credits))
      (128 * Int64.to_int (to_mutez subsidy))
  in
  return_unit

let get_cpmm_result results =
  match results with
  | cpmm_result :: _results -> cpmm_result
  | _ -> assert false

let get_lqt_result results =
  match results with
  | _cpmm_result :: lqt_result :: _results -> lqt_result
  | _ -> assert false

let get_address_in_result result =
  match result with
  | Apply_results.Origination_result {originated_contracts; _} -> (
      match originated_contracts with [c] -> c | _ -> assert false)

let get_balance_updates_in_result result =
  match result with
  | Apply_results.Origination_result {balance_updates; _} -> balance_updates

let get_balance_update_in_result result =
  match get_balance_updates_in_result result with
  | [Balance_update_item (Contract _, Credited balance, Protocol_migration)] ->
      (balance : Alpha_context.Tez.t)
  | [
   _;
   _;
   _;
   _;
   _;
   Balance_update_item (Contract _, Credited balance, Protocol_migration);
  ] ->
      balance
  | _ -> assert false

let liquidity_baking_origination_result_cpmm_address () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* cpmm_address_in_storage =
    Context.get_liquidity_baking_cpmm_address (B blk)
  in
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 1 blk
  in
  let result = get_cpmm_result origination_results in
  let address = get_address_in_result result in
  let* () =
    Assert.equal
      ~loc:__LOC__
      Contract_hash.equal
      "CPMM address in storage is not the same as in origination result"
      Contract_hash.pp
      address
      cpmm_address_in_storage
  in
  return_unit

let liquidity_baking_origination_result_cpmm_balance () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 1 blk
  in
  let result = get_cpmm_result origination_results in
  let balance_update = get_balance_update_in_result result in
  let* () = Assert.equal_tez ~loc:__LOC__ balance_update (of_mutez 100L) in
  return_unit

let liquidity_baking_origination_result_lqt_address () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 1 blk
  in
  let result = get_lqt_result origination_results in
  let address = get_address_in_result result in
  let* () =
    Assert.equal
      ~loc:__LOC__
      String.equal
      "LQT address in origination result is incorrect"
      Format.pp_print_string
      (Contract_hash.to_b58check address)
      "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"
  in
  return_unit

let liquidity_baking_origination_result_lqt_balance () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 1 blk
  in
  let result = get_lqt_result origination_results in
  let balance_updates = get_balance_updates_in_result result in
  match balance_updates with
  | [
   Balance_update_item
     (Liquidity_baking_subsidies, Debited am1, Protocol_migration);
   Balance_update_item (Storage_fees, Credited am2, Protocol_migration);
   Balance_update_item
     (Liquidity_baking_subsidies, Debited am3, Protocol_migration);
   Balance_update_item (Storage_fees, Credited am4, Protocol_migration);
  ] ->
      let* () = Assert.equal_tez ~loc:__LOC__ am1 am2 in
      let* () = Assert.equal_tez ~loc:__LOC__ am3 am4 in
      let* () = Assert.equal_tez ~loc:__LOC__ am1 (of_mutez 64_250L) in
      Assert.equal_tez ~loc:__LOC__ am3 (of_mutez 494_500L)
  | _ -> failwith "Unexpected balance updates (%s)" __LOC__

(* Test that with no contract at the tzBTC address and the level low enough to indicate we're not on mainnet, three contracts are originated in stitching. *)
let liquidity_baking_origination_test_migration () =
  let open Lwt_result_syntax in
  let* blk, _contract = Context.init1 () in
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 1 blk
  in
  let num_results = List.length origination_results in
  Assert.equal_int ~loc:__LOC__ num_results 3

(* Test that with no contract at the tzBTC address and the level high enough to indicate we could be on mainnet, no contracts are originated in stitching. *)
let liquidity_baking_origination_no_tzBTC_mainnet_migration () =
  let open Lwt_result_syntax in
  let* blk, _contract =
    Context.init1 ~consensus_threshold:0 ~level:1_437_862l ()
  in
  (* By baking a bit we also check that the subsidy application with no CPMM present does nothing rather than stopping the chain.*)
  let* _blk, origination_results =
    Block.bake_n_with_origination_results 64 blk
  in
  let num_results = List.length origination_results in
  Assert.equal_int ~loc:__LOC__ num_results 0

let tests =
  [
    Tztest.tztest
      "liquidity baking script hashes"
      `Quick
      liquidity_baking_origination;
    Tztest.tztest
      "liquidity baking cpmm is originated at the expected address"
      `Quick
      liquidity_baking_cpmm_address;
    Tztest.tztest "Init Context" `Quick generate_init_state;
    Tztest.tztest
      "liquidity baking subsidy is correct"
      `Quick
      (liquidity_baking_subsidies 64);
    Tztest.tztest
      "liquidity baking toggle vote with 100% of bakers voting \
       Per_block_vote_off baking one block longer"
      `Quick
      (liquidity_baking_toggle_100 1);
    Tztest.tztest
      "liquidity baking toggle vote with 100% of bakers voting \
       Per_block_vote_off baking two blocks longer"
      `Quick
      (liquidity_baking_toggle_100 2);
    Tztest.tztest
      "liquidity baking toggle vote with 100% of bakers voting \
       Per_block_vote_off baking 100 blocks longer"
      `Quick
      (liquidity_baking_toggle_100 100);
    Tztest.tztest
      "liquidity baking toggle vote with 80% of bakers voting \
       Per_block_vote_off baking one block longer"
      `Quick
      (liquidity_baking_toggle_80 1);
    Tztest.tztest
      "liquidity baking toggle vote with 80% of bakers voting \
       Per_block_vote_off baking two blocks longer"
      `Quick
      (liquidity_baking_toggle_80 2);
    Tztest.tztest
      "liquidity baking toggle vote with 80% of bakers voting \
       Per_block_vote_off baking 100 blocks longer"
      `Quick
      (liquidity_baking_toggle_80 100);
    Tztest.tztest
      "liquidity baking toggle vote with 60% of bakers voting \
       Per_block_vote_off baking one block longer"
      `Quick
      (liquidity_baking_toggle_60 1);
    Tztest.tztest
      "liquidity baking toggle vote with 60% of bakers voting \
       Per_block_vote_off baking two blocks longer"
      `Quick
      (liquidity_baking_toggle_60 2);
    Tztest.tztest
      "liquidity baking toggle vote with 60% of bakers voting \
       Per_block_vote_off baking 100 blocks longer"
      `Quick
      (liquidity_baking_toggle_60 100);
    Tztest.tztest
      "liquidity baking does not shut off with toggle vote at 50% and baking \
       100 blocks longer than sunset level in previous protocols"
      `Quick
      liquidity_baking_toggle_50;
    Tztest.tztest
      "liquidity baking restart with 100% of bakers voting off, then pass, \
       then on"
      `Quick
      (liquidity_baking_restart 2000 1);
    Tztest.tztest
      "liquidity baking toggle ema in block metadata is zero with no bakers \
       voting Per_block_vote_off."
      `Quick
      liquidity_baking_toggle_ema_zero;
    Tztest.tztest
      "liquidity baking toggle ema is equal to the threshold after the subsidy \
       has been stopped by a toggle vote"
      `Quick
      liquidity_baking_toggle_ema_threshold;
    Tztest.tztest
      "liquidity baking storage is updated"
      `Quick
      (liquidity_baking_storage 64);
    Tztest.tztest
      "liquidity baking balance updates"
      `Quick
      liquidity_baking_balance_update;
    Tztest.tztest
      "liquidity baking CPMM address in storage matches address in the \
       origination result"
      `Quick
      liquidity_baking_origination_result_cpmm_address;
    Tztest.tztest
      "liquidity baking CPMM balance in origination result is 100 mutez"
      `Quick
      liquidity_baking_origination_result_cpmm_balance;
    Tztest.tztest
      "liquidity baking LQT contract is originated at expected address"
      `Quick
      liquidity_baking_origination_result_lqt_address;
    Tztest.tztest
      "liquidity baking LQT balance in origination result is 0 mutez"
      `Quick
      liquidity_baking_origination_result_lqt_balance;
    Tztest.tztest
      "liquidity baking originates three contracts when tzBTC does not exist \
       and level indicates we are not on mainnet"
      `Quick
      liquidity_baking_origination_test_migration;
    Tztest.tztest
      "liquidity baking originates three contracts when tzBTC does not exist \
       and level indicates we might be on mainnet"
      `Quick
      liquidity_baking_origination_no_tzBTC_mainnet_migration;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("liquidity baking", tests)]
  |> Lwt_main.run
