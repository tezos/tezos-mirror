(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Testing
    -------
    Component:  Protocol (increase_paid_storage)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_paid_storage_increase.ml
    Subject:    On increasing a paid amount of contract storage.
*)

open Protocol
open Alpha_context

let ten_tez = Tez_helpers.of_int 10

let dummy_script =
  "{parameter unit; storage unit; code { CAR ; NIL operation ; PAIR }}"

let contract_originate block ?(script = dummy_script)
    ?(storage = Expr.from_string "Unit") account =
  let open Lwt_result_syntax in
  let code = Expr.from_string script in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  let source_contract = account in
  let baker = Context.Contract.pkh account in
  let* op, dst =
    Op.contract_origination_hash (B block) source_contract ~fee:Tez.zero ~script
  in
  let* inc =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let* inc = Incremental.add_operation inc op in
  let+ b = Incremental.finalize_block inc in
  (b, dst)

(** [test_balances] runs a simple [increase_paid_storage] and verifies that the
    source contract balance is correct and that the storage of the
    destination contract has been increased by the right amount. *)
let test_balances ~amount =
  let open Lwt_result_wrap_syntax in
  let* b, source = Context.init1 () in
  let* b, destination = contract_originate b source in
  let* inc = Incremental.begin_construction b in
  let* balance_before_op = Context.Contract.balance (I inc) source in
  let contract_dst = Contract.Originated destination in
  let*@ storage_before_op =
    Contract.Internal_for_tests.paid_storage_space
      (Incremental.alpha_ctxt inc)
      contract_dst
  in
  let* op =
    Op.increase_paid_storage ~fee:Tez.zero (I inc) ~source ~destination amount
  in
  let* inc = Incremental.add_operation inc op in
  (* check that after the block has been baked, the source was debited of all
     the burned tez *)
  let* {parametric = {cost_per_byte; _}; _} = Context.get_constants (I inc) in
  let burned_tez = Tez_helpers.(cost_per_byte *! Z.to_int64 amount) in
  let* () =
    Assert.balance_was_debited
      ~loc:__LOC__
      (I inc)
      source
      balance_before_op
      burned_tez
  in
  (* check that the storage has been increased by the right amount *)
  let*@ storage =
    Contract.Internal_for_tests.paid_storage_space
      (Incremental.alpha_ctxt inc)
      contract_dst
  in
  let storage_minus_amount = Z.sub storage amount in
  Assert.equal_int
    ~loc:__LOC__
    (Z.to_int storage_before_op)
    (Z.to_int storage_minus_amount)

(******************************************************)
(* Tests *)
(******************************************************)

(** Basic test. We test balances in simple cases. *)
let test_balances_simple () = test_balances ~amount:(Z.of_int 100)

(******************************************************)
(* Errors *)
(******************************************************)

(** We test the operation when the amount given is null. *)
let test_null_amount () =
  let open Lwt_result_syntax in
  let*! result = test_balances ~amount:Z.zero in
  Assert.proto_error ~loc:__LOC__ result (function
    | Fees_storage.Negative_storage_input -> true
    | _ -> false)

(** We test the operation when the amount given is negative. *)
let test_negative_amount () =
  let open Lwt_result_syntax in
  let amount = Z.of_int (-10) in
  let*! result = test_balances ~amount in
  Assert.proto_error ~loc:__LOC__ result (function
    | Fees_storage.Negative_storage_input -> true
    | _ -> false)

(** We create an implicit account with not enough tez to pay for the
    storage increase. *)
let test_no_tez_to_pay () =
  let open Lwt_result_syntax in
  let* b, (source, baker, receiver) =
    Context.init3 ~consensus_threshold_size:0 ()
  in
  let* b, destination = contract_originate b source in
  let pkh_for_bake = Context.Contract.pkh baker in
  let* inc =
    Incremental.begin_construction ~policy:Block.(By_account pkh_for_bake) b
  in
  let* {parametric = {cost_per_byte; _}; _} = Context.get_constants (I inc) in
  let increase_amount =
    Z.div (Z.of_int 2_000_000) (Z.of_int64 (Tez.to_mutez cost_per_byte))
  in
  let* balance = Context.Contract.balance (I inc) source in
  let*? tez_to_substract = Tez_helpers.(balance -? Tez.one) in
  let* op =
    Op.transaction (I inc) ~fee:Tez.zero source receiver tez_to_substract
  in
  let* inc = Incremental.add_operation inc op in
  let* b = Incremental.finalize_block inc in
  let* inc =
    Incremental.begin_construction ~policy:Block.(By_account pkh_for_bake) b
  in
  let* op =
    Op.increase_paid_storage (I inc) ~source ~destination increase_amount
  in
  let*! inc = Incremental.add_operation inc op in
  Assert.proto_error ~loc:__LOC__ inc (function
    | Fees_storage.Cannot_pay_storage_fee -> true
    | _ -> false)

(** To test when there is no smart contract at the address given. *)
let test_no_contract () =
  let open Lwt_result_syntax in
  let* b, source = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let destination = Contract_helpers.fake_KT1 in
  let* op = Op.increase_paid_storage (I inc) ~source ~destination Z.zero in
  let*! inc = Incremental.add_operation inc op in
  Assert.proto_error ~loc:__LOC__ inc (function
    | Raw_context.Storage_error (Missing_key (_, Raw_context.Get)) -> true
    | _ -> false)

(** To test if the increase in storage is effective. *)
let test_effectiveness () =
  let open Lwt_result_syntax in
  let* b, (source, _contract_source) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let script =
    "{parameter unit; storage int; code { CDR ; PUSH int 65536 ; MUL ; NIL \
     operation ; PAIR }}"
  in
  let storage =
    Tezos_micheline.Micheline.strip_locations (Expr_common.int Z.one)
  in
  let* b, destination = contract_originate ~script ~storage b source in
  let* inc = Incremental.begin_construction b in
  (* We ensure that the transaction can't be made with a 0 burn cap. *)
  let contract_dst = Contract.Originated destination in
  let* op =
    Op.transaction
      ~storage_limit:Z.zero
      ~fee:Tez.zero
      (I inc)
      source
      contract_dst
      Tez.zero
  in
  let*! inc_test = Incremental.add_operation inc op in
  let* () =
    Assert.proto_error ~loc:__LOC__ inc_test (function
      | Fees.Operation_quota_exceeded -> true
      | _ -> false)
  in
  let* b = Incremental.finalize_block inc in
  let* inc = Incremental.begin_construction b in
  let* op =
    Op.increase_paid_storage
      (I inc)
      ~fee:Tez.zero
      ~source
      ~destination
      (Z.of_int 10)
  in
  let* inc = Incremental.add_operation inc op in
  let* b = Incremental.finalize_block inc in
  let* inc = Incremental.begin_construction b in
  (* We test the same transaction to see if increase_paid_storage worked. *)
  let* op =
    Op.transaction
      ~storage_limit:Z.zero
      ~fee:Tez.zero
      (I inc)
      source
      contract_dst
      Tez.zero
  in
  let+ (_inc : Incremental.t) = Incremental.add_operation inc op in
  ()

let tests =
  [
    Tztest.tztest "balances simple" `Quick test_balances_simple;
    Tztest.tztest "null amount" `Quick test_null_amount;
    Tztest.tztest "negative amount" `Quick test_negative_amount;
    Tztest.tztest "not enough tez to pay" `Quick test_no_tez_to_pay;
    Tztest.tztest "no contract to bump its paid storage" `Quick test_no_contract;
    Tztest.tztest "effectiveness" `Quick test_effectiveness;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("paid storage increase", tests)]
  |> Lwt_main.run
