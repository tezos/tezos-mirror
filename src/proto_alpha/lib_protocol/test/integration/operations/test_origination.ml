(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
    Component:  Protocol (origination)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_origination.ml
    Subject:    On originating contracts.
*)

open Protocol
open Alpha_context
open Tez_helpers

let ten_tez = of_int 10

(* The possible fees are: a given credit, an origination burn fee
   (constants_repr.default.origination_burn = 257 mtez), a fee that is paid when
   creating an originate contract. *)
let total_fees_for_origination ?(fee = Tez.zero) ?(credit = Tez.zero) b =
  let open Lwt_result_syntax in
  let* {parametric = {origination_size; cost_per_byte; _}; _} =
    Context.get_constants (B b)
  in
  let*? origination_burn = cost_per_byte *? Int64.of_int origination_size in
  let*? t = credit +? fee in
  let*? t = origination_burn +? t in
  let*? total_fee = Op.dummy_script_cost +? t in
  return total_fee

(* [test_origination_balances fee credit spendable delegatable] takes four
   optional parameter: fee is the fee that pay if require to create an
   originated contract; credit is the amount of tez that will send to this
   contract; delegatable default is set to true meaning that this contract is
   able to delegate.

   This function creates 2 contracts, one for originating (called source) and
   one for baking; get the balance of the source contract, call the
   origination operation to create a new originated contract from this contract
   with all the possible fees; and check the balance before/after originated
   operation valid.
   - the source contract has payed all the fees
   - the originated has been credited correctly.
     Note that we need 2 contracts because in Tenderbake the baker receives the
     fees instantaneously. So to see that the fees are subtracted, we need that
     the bake is done by another delegated. *)
let test_origination_balances ~loc:_ ?(fee = Tez.zero) ?(credit = Tez.zero) () =
  let open Lwt_result_syntax in
  let* b, (source, contract_for_bake) = Context.init2 () in
  let pkh_for_orig = Context.Contract.pkh source in
  let pkh_for_bake = Context.Contract.pkh contract_for_bake in
  let* operation, new_contract =
    Op.contract_origination (B b) source ~fee ~credit ~script:Op.dummy_script
  in
  let* total_fee = total_fees_for_origination ~fee ~credit b in
  let* b = Block.bake ~operation ~policy:(By_account pkh_for_bake) b in
  (* check that after the block has been baked the contract for originating
     was debited all the fees *)
  let* deposits = Context.Delegate.current_frozen_deposits (B b) pkh_for_orig in
  let*? total_fee_plus_deposits = total_fee +? deposits in
  let* () =
    Assert.balance_was_debited
      ~loc:__LOC__
      (B b)
      source
      Account.default_initial_full_balance
      total_fee_plus_deposits
  in
  (* check the balance of the originate contract is equal to credit *)
  Assert.balance_is ~loc:__LOC__ (B b) new_contract credit

(** [register_origination fee credit spendable delegatable] takes four
    optional parameter: fee for the fee need to be paid if set to
    create an originated contract; credit is the amount of tez that
    send to this originated contract; spendable default is set to true
    meaning that this contract is spendable; delegatable default is
    set to true meaning that this contract is able to delegate. *)
let register_origination ?(fee = Tez.zero) ?(credit = Tez.zero) () =
  let open Lwt_result_syntax in
  let* b, (source, contract_for_bake) = Context.init2 () in
  let source_pkh = Context.Contract.pkh source in
  let pkh_for_bake = Context.Contract.pkh contract_for_bake in
  let* operation, originated =
    Op.contract_origination (B b) source ~fee ~credit ~script:Op.dummy_script
  in
  let* b = Block.bake ~operation ~policy:(By_account pkh_for_bake) b in
  (* fee + credit were debited from source *)
  let* total_fee = total_fees_for_origination ~fee ~credit b in
  let* deposits = Context.Delegate.current_frozen_deposits (B b) source_pkh in
  let*? total_fee_plus_deposits = total_fee +? deposits in
  let* () =
    Assert.balance_was_debited
      ~loc:__LOC__
      (B b)
      source
      Account.default_initial_full_balance
      total_fee_plus_deposits
  in
  (* originated contract has been credited *)
  let+ () =
    Assert.balance_was_credited ~loc:__LOC__ (B b) originated Tez.zero credit
  in
  (* TODO spendable or not and delegatable or not if relevant for some
     test. Not the case at the moment, cf. uses of
     register_origination *)
  (b, source, originated)

(******************************************************)
(* Tests *)
(******************************************************)

(** Basic test. A contract is created as well as the newly originated
    contract (called from origination operation). The balance
    before/after are checked. *)
let test_balances_simple () = test_origination_balances ~loc:__LOC__ ()

(** Same as [balances_simple] but credits 10 tez to the originated
    contract (no fees). *)
let test_balances_credit () =
  test_origination_balances ~loc:__LOC__ ~credit:ten_tez ()

(** Same as [balances_credit] with 10 tez fees. *)
let test_balances_credit_fee () =
  test_origination_balances ~loc:__LOC__ ~credit:(of_int 2) ~fee:ten_tez ()

(** Ask source contract to pay a fee when originating a contract. *)
let test_pay_fee () =
  let open Lwt_result_syntax in
  let* _b, _contract, _new_contract =
    register_origination ~credit:(of_int 2) ~fee:ten_tez ()
  in
  return_unit

(******************************************************)
(** Errors *)

(******************************************************)

(** Create an originate contract where the contract does not have
    enough tez to pay for the fee. *)
let test_not_tez_in_contract_to_pay_fee () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  (* transfer everything but one tez from 1 to 2 and check balance of 1 *)
  let* balance = Context.Contract.balance (B b) contract_1 in
  let*? amount = balance -? Tez.one in
  let* operation = Op.transaction (B b) contract_1 contract_2 amount in
  let pkh1 = Context.Contract.pkh contract_1 in
  let* b = Block.bake ~policy:(Excluding [pkh1]) ~operation b in
  let* () =
    Assert.balance_was_debited ~loc:__LOC__ (B b) contract_1 balance amount
  in
  (* use this source contract to create an originate contract where it requires
     to pay a fee and add an amount of credit into this new contract *)
  let* op, _ =
    Op.contract_origination
      (B b)
      ~fee:ten_tez
      ~credit:Tez.one
      contract_1
      ~script:Op.dummy_script
  in
  let* inc = Incremental.begin_construction b in
  let*! inc = Incremental.add_operation inc op in
  Assert.proto_error_with_info ~loc:__LOC__ inc "Balance too low"

(* Create multiple originated contracts and ask contract to pay the fee. *)
let n_originations n ?credit ?fee () =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun new_contracts _ ->
      let+ _b, _source, new_contract = register_origination ?fee ?credit () in
      new_contract :: new_contracts)
    []
    (1 -- n)

(** Create 100 originations. *)
let test_multiple_originations () =
  let open Lwt_result_syntax in
  let* contracts = n_originations 100 ~credit:(of_int 2) ~fee:ten_tez () in
  Assert.equal_int ~loc:__LOC__ (List.length contracts) 100

(** Cannot originate two contracts with the same context's counter. *)
let test_counter () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init1 ~consensus_threshold_size:0 () in
  let* op1, _ =
    Op.contract_origination
      (B b)
      ~credit:Tez.one
      contract
      ~script:Op.dummy_script
  in
  let* op2, _ =
    Op.contract_origination
      (B b)
      ~credit:Tez.one
      contract
      ~script:Op.dummy_script
  in
  let* b = Block.bake ~operation:op1 b in
  let* inc = Incremental.begin_construction b in
  let*! res = Incremental.add_operation inc op2 in
  Assert.proto_error_with_info
    ~loc:__LOC__
    res
    "Invalid counter (already used) in a manager operation"

let test_unparsable_script () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init1 ~consensus_threshold_size:0 () in
  let open Alpha_context in
  (* Craft an ill-typed origination's contract. *)
  let pkh =
    match contract with Implicit pkh -> pkh | Originated _ -> assert false
  in
  let dummy_expr =
    Script.lazy_expr
      Environment.Micheline.(strip_locations (Int ((), Z.of_int 123)))
  in
  let script = Script.{code = dummy_expr; storage = dummy_expr} in
  let origination = Origination {delegate = None; script; credit = Tez.one} in
  let gas_limit =
    Gas.Arith.integral_of_int_exn
      (49_000
     + Michelson_v1_gas.Internal_for_tests.int_cost_of_manager_operation)
  in
  let op =
    Contents_list
      (Single
         (Manager_operation
            {
              source = pkh;
              fee = Tez.one;
              counter = Manager_counter.Internal_for_tests.of_int 1;
              operation = origination;
              gas_limit;
              storage_limit = Z.zero;
            }))
  in
  let encoded_op =
    Data_encoding.Binary.to_bytes_exn Operation.contents_list_encoding op
    |> Bytes.to_string
  in
  let* account = Account.find pkh in
  let* ill_typed_op =
    Data_encoding.Binary.of_string_exn
      Operation.contents_list_encoding
      encoded_op
    |> Op.sign (B b) account.sk (Context.branch (B b))
  in
  (* Ensure that the application fails with [Ill_typed_contract]. *)
  let* i = Incremental.begin_construction b in
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:(function
        | Environment.Ecoproto_error (Script_tc_errors.Ill_typed_contract _)
          :: _ ->
            return_unit
        | trace ->
            failwith
              "Expected error trace [Ill_typed_contract], but got:@\n%a"
              pp_print_trace
              trace)
      i
      ill_typed_op
  in
  (* Craft an unparsable lazy expr. *)
  let encoded_dummy_expr =
    let b =
      Data_encoding.Binary.to_bytes_exn Script.lazy_expr_encoding dummy_expr
    in
    assert (Hex.to_bytes_exn (`Hex "0000000300bb01") = b) ;
    Bytes.to_string b
  in
  let unparsable_dummy_expr =
    Hex.to_bytes_exn (`Hex "00000003ffffff") |> Bytes.to_string
  in
  let* unparsable_operation =
    let encoded_bad_op =
      Re.(
        replace_string
          ~all:true
          (compile (str encoded_dummy_expr))
          encoded_op
          ~by:unparsable_dummy_expr)
    in
    Data_encoding.Binary.of_string_exn
      Operation.contents_list_encoding
      encoded_bad_op
    |> Op.sign (B b) account.sk (Context.branch (B b))
  in
  (* Ensure that the operation is valid but the application fails with
     [Lazy_script_decode]. *)
  let* (_i : Incremental.t) =
    Incremental.add_operation
      ~expect_apply_failure:(function
        | [Environment.Ecoproto_error Script.Lazy_script_decode] -> return_unit
        | trace ->
            failwith
              "Expected error trace [Lazy_script_decode], but got:@\n%a"
              pp_print_trace
              trace)
      i
      unparsable_operation
  in
  return_unit

(******************************************************)

let tests =
  [
    Tztest.tztest "balances_simple" `Quick test_balances_simple;
    Tztest.tztest "balances_credit" `Quick test_balances_credit;
    Tztest.tztest "balances_credit_fee" `Quick test_balances_credit_fee;
    Tztest.tztest "pay_fee" `Quick test_pay_fee;
    Tztest.tztest
      "not enough tez in contract to pay fee"
      `Quick
      test_not_tez_in_contract_to_pay_fee;
    Tztest.tztest "multiple originations" `Quick test_multiple_originations;
    Tztest.tztest "counter" `Quick test_counter;
    Tztest.tztest "unparsable script" `Quick test_unparsable_script;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("orgination", tests)]
  |> Lwt_main.run
