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
    Component:  Protocol (transfer)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_transfer.ml
    Subject:    Quantities transfer between contracts.
*)

open Protocol
open Alpha_context
open Tez_helpers
open Transfers

(*********************************************************************)
(* Utility functions                                                 *)
(*********************************************************************)

(**
   [transfer_to_itself_and_check_balances b fee contract amount]
   this function takes a block, an optional parameter fee,
   a contract that is a source and a destination contract,
   and an amount of tez that one wants to transfer.

   1- Transfer the amount of tez (w/wo transfer fee) from/to a contract itself.

   2- Check the equivalent of the balance of the contract before
       and after transfer.

   This function returns a pair:
   - a block that added the valid transaction
   - an valid transaction *)
let transfer_to_itself_and_check_balances ~loc ?policy b ?(fee = Tez.zero)
    contract amount =
  let open Lwt_result_syntax in
  let* bal = Context.Contract.balance (B b) contract in
  let* operation = Op.transaction (B b) ~fee contract contract amount in
  let* b = Block.bake ?policy ~operation b in
  let+ () = Assert.balance_was_debited ~loc (B b) contract bal fee in
  (b, operation)

let ten_tez = of_int 10

(*********************************************************************)
(* Tests                                                             *)
(*********************************************************************)

(** Compute a fraction of 2/[n] of the balance of [contract] *)
let two_over_n_of_balance ctxt contract n =
  let open Lwt_result_syntax in
  let* balance = Context.Contract.balance ctxt contract in
  let res = balance /! n in
  let res = res *? 2L in
  Lwt.return res

(********************)
(** Single transfer *)

(********************)

let single_transfer ?fee ?expect_apply_failure amount =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) = Context.init2 () in
  let* b = Incremental.begin_construction b in
  let* b, _ =
    transfer_and_check_balances
      ~loc:__LOC__
      ?fee
      ?expect_apply_failure
      b
      contract_1
      contract_2
      amount
  in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(** Single transfer without fee. *)
let test_block_with_a_single_transfer () = single_transfer Tez.one

(** Single transfer with fee. *)
let test_block_with_a_single_transfer_with_fee () =
  single_transfer ~fee:Tez.one Tez.one

(** Single transfer without fee. *)
let test_transfer_zero_tez () =
  let open Lwt_result_syntax in
  let expect_apply_failure = function
    | Environment.Ecoproto_error (Apply.Empty_transaction _ as err) :: _ ->
        Assert.test_error_encodings err ;
        return_unit
    | _ -> failwith "Empty transaction should fail"
  in
  single_transfer ~expect_apply_failure Tez.zero

(** Transfer zero tez from an implicit contract. *)
let test_transfer_zero_implicit () =
  let open Lwt_result_syntax in
  let* b, dest = Context.init1 () in
  let account = Account.new_account () in
  let* i = Incremental.begin_construction b in
  let src = Contract.Implicit account.Account.pkh in
  let* op = Op.transaction (I i) src dest Tez.zero in
  let*! res = Incremental.add_operation i op in
  Assert.proto_error ~loc:__LOC__ res (function
    | Contract_storage.Empty_implicit_contract _ as err ->
        Assert.test_error_encodings err ;
        true
    | _ -> false)

(** Transfer to originated contract. *)
let test_transfer_to_originate_with_fee () =
  let open Lwt_result_syntax in
  let* b, contract = Context.init1 ~consensus_threshold_size:0 () in
  let* fee = two_over_n_of_balance (B b) contract 10L in
  (* originated contract, paying a fee to originated this contract *)
  let* operation, new_contract =
    Op.contract_origination (B b) ~fee:ten_tez contract ~script:Op.dummy_script
  in
  let* b = Block.bake ~operation b in
  let* amount = two_over_n_of_balance (B b) contract 3L in
  let* i = Incremental.begin_construction b in
  let* i, _ =
    transfer_and_check_balances ~loc:__LOC__ i ~fee contract new_contract amount
  in
  let* (_ : Block.t) = Incremental.finalize_block i in
  return_unit

(** Transfer from balance. *)
let test_transfer_amount_of_contract_balance () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) = Context.init2 () in
  let pkh1 = Context.Contract.pkh contract_1 in
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  let* b = Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) in
  (* get the balance of the source contract *)
  let* balance = Context.Contract.balance (I b) contract_1 in
  (* transfer all the tez inside contract 1 *)
  let* b, _ =
    transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 balance
  in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(** Transfer to oneself. *)
let test_transfers_to_self () =
  let open Lwt_result_syntax in
  let* b, (contract, _) = Context.init2 ~consensus_threshold_size:0 () in
  let* amount = two_over_n_of_balance (B b) contract 3L in
  let pkh1 = Context.Contract.pkh contract in
  let* b, _ =
    transfer_to_itself_and_check_balances
      ~loc:__LOC__
      ~policy:(Block.Excluding [pkh1])
      b
      contract
      amount
  in
  let* fee = two_over_n_of_balance (B b) contract 5L in
  let* _, _ =
    transfer_to_itself_and_check_balances
      ~loc:__LOC__
      b
      ~policy:(Block.Excluding [pkh1])
      ~fee
      contract
      ten_tez
  in
  return_unit

(** Forgot to add the valid transaction into the block. *)
let test_missing_transaction () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  let pkh1 = Context.Contract.pkh contract_1 in
  let* i = Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) in
  let* amount = two_over_n_of_balance (B b) contract_1 6L in
  (* Do the transfer 3 times from source contract to destination contract *)
  let* i = n_transactions 3 i contract_1 contract_2 amount in
  (* do the fourth transfer from source contract to destination contract *)
  let* (_ : packed_operation) =
    Op.transaction (I i) contract_1 contract_2 amount
  in
  let* (_ : Block.t) = Incremental.finalize_block i in
  return_unit

(** Transfer zero tez to an implicit contract, with fee equals balance of src. *)
let test_transfer_zero_implicit_with_bal_src_as_fee () =
  let open Lwt_result_syntax in
  let* b, dest = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let src_pkh = account.Account.pkh in
  let src = Contract.Implicit src_pkh in
  let* operation =
    Op.transaction ~force_reveal:true (B b) dest src (Tez.of_mutez_exn 100L)
  in
  let* b = Block.bake ~operation b in
  let* bal_src = Context.Contract.balance (B b) src in
  let* () = Assert.equal_tez ~loc:__LOC__ bal_src (Tez.of_mutez_exn 100L) in
  let* op =
    Op.transaction ~force_reveal:true (B b) ~fee:bal_src src dest Tez.zero
  in
  (* Transferring zero tez should result in an application failure as
     the implicit contract has been depleted. *)
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract pkh);
      ]
      when pkh = src_pkh ->
        return_unit
    | _ -> assert false
  in
  let* i = Incremental.begin_construction b in
  let* inc = Incremental.add_operation ~expect_apply_failure i op in
  let* balance = Context.Contract.balance (I inc) src in
  (* We assert that the failing operation was included and that the
     fees were taken, effectively depleting the contract. *)
  let* () = Assert.equal_tez ~loc:__LOC__ balance Tez.zero in
  (* Empty contracts should be unrevealed *)
  let* revelead = Context.Contract.is_manager_key_revealed (I inc) src in
  when_ revelead (fun () ->
      Stdlib.failwith "Empty account still exists and is revealed.")

(** Transfer zero tez to an originated contract, with fee equals balance of src. *)
let test_transfer_zero_to_originated_with_bal_src_as_fee () =
  let open Lwt_result_syntax in
  let* b, dest = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let src = Contract.Implicit account.Account.pkh in
  let* operation = Op.transaction (B b) dest src (Tez.of_mutez_exn 100L) in
  let* b = Block.bake ~operation b in
  let* operation, new_contract =
    Op.contract_origination (B b) dest ~script:Op.dummy_script
  in
  let* b = Block.bake ~operation b in
  let* bal_src = Context.Contract.balance (B b) src in
  let* operation = Op.revelation (B b) ~fee:Tez.zero account.pk in
  let* b = Block.bake ~operation b in
  let* operation =
    Op.transaction (B b) ~fee:bal_src src new_contract Tez.zero
  in
  let* () = Assert.equal_tez ~loc:__LOC__ bal_src (Tez.of_mutez_exn 100L) in
  let* (_ : Block.t) = Block.bake ~operation b in
  return_unit

(** Transfer one tez to an implicit contract, with fee equals balance of src. *)
let test_transfer_one_to_implicit_with_bal_src_as_fee () =
  let open Lwt_result_syntax in
  let* b, dest = Context.init1 ~consensus_threshold_size:0 () in
  let account = Account.new_account () in
  let src = Contract.Implicit account.Account.pkh in
  let* operation = Op.transaction (B b) dest src (Tez.of_mutez_exn 100L) in
  let* b = Block.bake ~operation b in
  let* bal_src = Context.Contract.balance (B b) src in
  let* () = Assert.equal_tez ~loc:__LOC__ bal_src (Tez.of_mutez_exn 100L) in
  let* operation = Op.revelation (B b) ~fee:Tez.zero account.pk in
  let* b = Block.bake ~operation b in
  let* op = Op.transaction (B b) ~fee:bal_src src dest Tez.one in
  let* i = Incremental.begin_construction b in
  let* (_ : Incremental.t) =
    Incremental.add_operation i op ~expect_apply_failure:(function
      | Environment.Ecoproto_error (Contract_storage.Balance_too_low _ as err)
        :: _ ->
          Assert.test_error_encodings err ;
          return_unit
      | t -> failwith "Unexpected error: %a" Error_monad.pp_print_trace t)
  in
  return_unit

(********************)
(* The following tests are for different kind of contracts:
   - implicit to implicit
   - implicit to originated
   - originated to implicit
   - originated to originated *)

(********************)

(** Implicit to Implicit. *)
let test_transfer_from_implicit_to_implicit_contract () =
  let open Lwt_result_syntax in
  let* b, bootstrap_contract = Context.init1 ~consensus_threshold_size:0 () in
  let account_a = Account.new_account () in
  let account_b = Account.new_account () in
  let src = Contract.Implicit account_a.Account.pkh in
  let* amount1 = two_over_n_of_balance (B b) bootstrap_contract 3L in
  let* fee1 = two_over_n_of_balance (B b) bootstrap_contract 10L in
  let* i = Incremental.begin_construction b in
  let* i, _ =
    transfer_and_check_balances
      ~with_burn:true
      ~loc:__LOC__
      ~fee:fee1
      i
      bootstrap_contract
      src
      amount1
  in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  (* Create an implicit contract as a destination contract. *)
  let dest = Contract.Implicit account_b.pkh in
  let* amount2 = two_over_n_of_balance (I i) bootstrap_contract 4L in
  let* fee2 = two_over_n_of_balance (I i) bootstrap_contract 10L in
  (* Transfer from implicit contract to another implicit contract. *)
  let* b, _ =
    transfer_and_check_balances
      ~with_burn:true
      ~loc:__LOC__
      ~fee:fee2
      i
      src
      dest
      amount2
  in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(** Implicit to originated. *)
let test_transfer_from_implicit_to_originated_contract () =
  let open Lwt_result_syntax in
  let* b, bootstrap_contract = Context.init1 ~consensus_threshold_size:0 () in
  let contract = bootstrap_contract in
  let account = Account.new_account () in
  let src = Contract.Implicit account.Account.pkh in
  let* amount1 = two_over_n_of_balance (B b) bootstrap_contract 3L in
  let* i = Incremental.begin_construction b in
  (* transfer the money to implicit contract *)
  let* i, _ =
    transfer_and_check_balances
      ~with_burn:true
      ~loc:__LOC__
      i
      bootstrap_contract
      src
      amount1
  in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  (* originated contract *)
  let* operation, new_contract =
    Op.contract_origination
      ~force_reveal:true
      (I i)
      contract
      ~script:Op.dummy_script
  in
  let* i = Incremental.add_operation i operation in
  let* amount2 = two_over_n_of_balance (I i) bootstrap_contract 4L in
  (* transfer from implicit contract to originated contract *)
  let* i, _ =
    transfer_and_check_balances ~loc:__LOC__ i src new_contract amount2
  in
  let* (_ : Block.t) = Incremental.finalize_block i in
  return_unit

(********************)
(* Slow tests case *)

(********************)

let multiple_transfer n ?fee amount =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let* b = Incremental.begin_construction b in
  let* b = n_transactions n b ?fee contract_1 contract_2 amount in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(** 1- Create a block with two contracts;
    2- Apply 100 transfers.
*)
let test_block_with_multiple_transfers () = multiple_transfer 99 (of_int 1000)

(** 1- Create a block with two contracts;
    2- Apply 100 transfers with 10tz fee. *)
let test_block_with_multiple_transfers_pay_fee () =
  multiple_transfer 10 ~fee:ten_tez (of_int 1000)

(* TODO : increase the number of operations and add a `Slow tag to it in `tests` *)

(** 1- Create a block with 8 contracts;
    2- Apply multiple transfers without fees;
    3- Apply multiple transfers with fees. *)
let test_block_with_multiple_transfers_with_without_fee () =
  let open Lwt_result_syntax in
  let* b, contracts = Context.init_n ~consensus_threshold_size:0 8 () in
  let contracts = Array.of_list contracts in
  let* b = Incremental.begin_construction b in
  let hundred = of_int 100 in
  let ten = of_int 10 in
  let twenty = of_int 20 in
  let* b = n_transactions 10 b contracts.(0) contracts.(1) Tez.one in
  let* b = n_transactions 30 b contracts.(1) contracts.(2) hundred in
  let* b = n_transactions 30 b contracts.(1) contracts.(3) hundred in
  let* b = n_transactions 30 b contracts.(4) contracts.(3) hundred in
  let* b = n_transactions 20 b contracts.(0) contracts.(1) hundred in
  let* b = n_transactions 10 b contracts.(1) contracts.(3) hundred in
  let* b = n_transactions 10 b contracts.(1) contracts.(3) hundred in
  let* b = n_transactions 20 ~fee:ten b contracts.(3) contracts.(4) ten in
  let* b = n_transactions 10 ~fee:twenty b contracts.(4) contracts.(5) ten in
  let* b = n_transactions 70 ~fee:twenty b contracts.(6) contracts.(0) twenty in
  let* b =
    n_transactions 550 ~fee:twenty b contracts.(6) contracts.(4) twenty
  in
  let* b = n_transactions 50 ~fee:ten b contracts.(7) contracts.(5) twenty in
  let* b = n_transactions 30 ~fee:ten b contracts.(0) contracts.(7) hundred in
  let* b = n_transactions 20 ~fee:ten b contracts.(1) contracts.(0) twenty in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(** Build a chain that has 10 blocks. *)
let test_build_a_chain () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let ten = of_int 10 in
  let* (_ : Block.t) =
    List.fold_left_es
      (fun b _ ->
        let* b = Incremental.begin_construction b in
        let* b, _ =
          transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 ten
        in
        Incremental.finalize_block b)
      b
      (1 -- 10)
  in
  return_unit

(*********************************************************************)
(* Expected error test cases                                         *)
(*********************************************************************)

(** Transferring zero tez is forbidden in implicit contract. *)
let test_empty_implicit () =
  let open Lwt_result_syntax in
  let* b, dest = Context.init1 () in
  let account = Account.new_account () in
  let src = Contract.Implicit account.Account.pkh in
  let* amount = two_over_n_of_balance (B b) dest 3L in
  (* Transfer zero tez from an implicit contract. *)
  let* op = Op.transaction (B b) src dest amount in
  let* incr = Incremental.begin_construction b in
  let*! res = Incremental.add_operation incr op in
  Assert.proto_error ~loc:__LOC__ res (function
    | Contract_storage.Empty_implicit_contract _ as err ->
        Assert.test_error_encodings err ;
        true
    | _ -> false)

(** Balance is too low to transfer. *)
let test_balance_too_low fee () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let* balance1 = Context.Contract.balance (B b) contract_1 in
  let* balance2 = Context.Contract.balance (B b) contract_2 in
  (* transfer the amount of tez that is bigger than the balance in the source contract *)
  let* op = Op.transaction ~fee (B b) contract_1 contract_2 max_tez in
  let expect_failure = function
    | Environment.Ecoproto_error (Contract_storage.Balance_too_low _ as err)
      :: _ ->
        Assert.test_error_encodings err ;
        return_unit
    | t -> failwith "Unexpected error: %a" Error_monad.pp_print_trace t
  in
  let* i = Incremental.begin_construction b in
  if fee > balance1 then
    (* The fee is higher than the balance, so the operation validation
       fails with the [Balance_too_low] error. *)
    let* (_res : Incremental.t) =
      Incremental.add_operation ~expect_failure i op
    in
    return_unit
  else
    (* The fee is smaller than or equal to the balance, so the
       operation is successfully validated and its fees are
       taken. However, since the amount to transfer exceeds the
       balance, the application has no further effects and the
       operation is marked with the [Balance_too_low] error. *)
    let* i =
      Incremental.add_operation ~expect_apply_failure:expect_failure i op
    in
    (* contract_1 loses the fees *)
    let* () =
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee
    in
    (* contract_2 is not credited *)
    Assert.balance_was_credited ~loc:__LOC__ (I i) contract_2 balance2 Tez.zero

(** 1- Create a block, and three contracts;
    2- Add a transfer that at the end the balance of a contract is
       zero into this block;
    3- Add another transfer that send tez from a zero balance contract;
    4- Catch the expected error: Balance_too_low. *)
let test_balance_too_low_two_transfers fee () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2, contract_3) =
    Context.init3 ~consensus_threshold_size:0 ()
  in
  let* i = Incremental.begin_construction b in
  let* balance = Context.Contract.balance (I i) contract_1 in
  let res = balance /! 3L in
  let*? two_third_of_balance = res *? 2L in
  let* i, _ =
    transfer_and_check_balances
      ~loc:__LOC__
      i
      contract_1
      contract_2
      two_third_of_balance
  in
  let* b = Incremental.finalize_block i in
  let* balance1 = Context.Contract.balance (B b) contract_1 in
  let* balance3 = Context.Contract.balance (B b) contract_3 in
  let* operation =
    Op.transaction ~fee (B b) contract_1 contract_3 two_third_of_balance
  in
  let expect_apply_failure = function
    | Environment.Ecoproto_error (Contract_storage.Balance_too_low _ as err)
      :: _ ->
        Assert.test_error_encodings err ;
        return_unit
    | t -> failwith "Unexpected error: %a" Error_monad.pp_print_trace t
  in
  let* i = Incremental.begin_construction b in
  let* i = Incremental.add_operation ~expect_apply_failure i operation in
  (* contract_1 loses the fees *)
  let* () =
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee
  in
  (* contract_3 is not credited *)
  Assert.balance_was_credited ~loc:__LOC__ (I i) contract_3 balance3 Tez.zero

(** The counter is already used for the previous operation. *)
let invalid_counter () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) =
    Context.init2 ~consensus_threshold_size:0 ()
  in
  let* op1 = Op.transaction (B b) contract_1 contract_2 Tez.one in
  let* op2 = Op.transaction (B b) contract_1 contract_2 Tez.one in
  let* b = Block.bake ~operation:op1 b in
  let* i = Incremental.begin_construction b in
  let*! b = Incremental.add_operation i op2 in
  Assert.proto_error ~loc:__LOC__ b (function
    | Contract_storage.Counter_in_the_past _ as err ->
        Assert.test_error_encodings err ;
        true
    | _ -> false)

(** Same as before but through a different way to perform this
    error. *)
let test_add_the_same_operation_twice () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) = Context.init2 () in
  let* i = Incremental.begin_construction b in
  let* i, op_transfer =
    transfer_and_check_balances ~loc:__LOC__ i contract_1 contract_2 ten_tez
  in
  let* b = Incremental.finalize_block i in
  let* i = Incremental.begin_construction b in
  let* (_ : packed_operation) =
    Op.transaction (I i) contract_1 contract_2 ten_tez
  in
  let*! b = Incremental.add_operation i op_transfer in
  Assert.proto_error ~loc:__LOC__ b (function
    | Contract_storage.Counter_in_the_past _ as err ->
        Assert.test_error_encodings err ;
        true
    | _ -> false)

(** The counter is in the future *)
let invalid_counter_in_the_future () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) = Context.init2 () in
  let* b = Incremental.begin_construction b in
  let* cpt = Context.Contract.counter (I b) contract_1 in
  let counter = Manager_counter.Internal_for_tests.add cpt 10 in
  let* op = Op.transaction (I b) contract_1 contract_2 Tez.one ~counter in
  let*! b = Incremental.add_operation b op in
  Assert.proto_error ~loc:__LOC__ b (function
    | Contract_storage.Counter_in_the_future _ as err ->
        Assert.test_error_encodings err ;
        true
    | _ -> false)

(** Check ownership. *)
let test_ownership_sender () =
  let open Lwt_result_syntax in
  let* b, (contract_1, contract_2) = Context.init2 () in
  let* b = Incremental.begin_construction b in
  (* get the manager of the contract_1 as a sender *)
  let* manager = Context.Contract.manager (I b) contract_1 in
  let imcontract_1 = Alpha_context.Contract.Implicit manager.pkh in
  let* b, _ =
    transfer_and_check_balances ~loc:__LOC__ b imcontract_1 contract_2 Tez.one
  in
  let* (_ : Block.t) = Incremental.finalize_block b in
  return_unit

(*********************************************************************)
(* Random transfer *)

(* Return a pair of minimum and maximum random number. *)
let random_range (min, max) =
  let interv = max - min + 1 in
  let init =
    Random.self_init () ;
    Random.int interv + min
  in
  init

(* Return a random contract. *)
let random_contract contract_array =
  let i = Random.int (Array.length contract_array) in
  contract_array.(i)

(** Transfer by randomly choose amount 10 contracts, and randomly
    choose the amount in the source contract. *)
let test_random_transfer () =
  let open Lwt_result_syntax in
  let* b, contracts = Context.init_n 10 () in
  let contracts = Array.of_list contracts in
  let source = random_contract contracts in
  let dest = random_contract contracts in
  let source_pkh = Context.Contract.pkh source in
  (* given that source may not have a sufficient balance for the transfer + to bake,
     make sure it cannot be chosen as baker *)
  let* amount = Context.Contract.balance (B b) source in
  if source = dest then
    let* _, _ =
      transfer_to_itself_and_check_balances
        ~loc:__LOC__
        ~policy:(Block.Excluding [source_pkh])
        b
        source
        amount
    in
    return_unit
  else
    let* i =
      Incremental.begin_construction ~policy:(Block.Excluding [source_pkh]) b
    in
    let* _, _ = transfer_and_check_balances ~loc:__LOC__ i source dest amount in
    return_unit

(** Transfer random transactions. *)
let test_random_multi_transactions () =
  let n = random_range (1, 100) in
  multiple_transfer n (of_int 100)

(*********************************************************************)

let test_bad_entrypoint () =
  let open Lwt_result_syntax in
  let* b, _c = Context.init1 () in
  let* v = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt v in
  let storage = "Unit" in
  let parameter = "Unit" in
  let entrypoint = Entrypoint.of_string_strict_exn "bad entrypoint" in
  (* bad entrypoint *)
  let*! result =
    Contract_helpers.run_script
      ctxt
      "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
      ~entrypoint
      ~storage
      ~parameter
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected error"
  | Error lst
    when List.mem
           ~equal:( = )
           (Environment.Ecoproto_error
              (Script_tc_errors.No_such_entrypoint entrypoint))
           lst ->
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

let test_bad_parameter () =
  let open Lwt_result_syntax in
  let* b, _c = Context.init1 () in
  let* v = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt v in
  let storage = "Unit" in
  let parameter = "1" in
  (* bad parameter *)
  let*! result =
    Contract_helpers.run_script
      ctxt
      "{parameter unit; storage unit; code { CAR; NIL operation; PAIR }}"
      ~storage
      ~parameter
      ()
  in
  match result with
  | Ok _ -> Alcotest.fail "expected error"
  | Error lst
    when List.mem
           ~equal:( = )
           (Environment.Ecoproto_error
              (Script_interpreter_errors.Bad_contract_parameter
                 (Contract.Originated Contract_helpers.default_self)))
           lst ->
      return_unit
  | Error errs ->
      Alcotest.failf "Unexpected error: %a" Error_monad.pp_print_trace errs

let transfer_to_itself_with_no_such_entrypoint () =
  let open Lwt_result_syntax in
  let entrypoint = Entrypoint.of_string_strict_exn "bad entrypoint" in
  let* b, addr = Context.init1 () in
  let* i = Incremental.begin_construction b in
  let* transaction = Op.transaction (B b) addr addr Tez.one ~entrypoint in
  let expect_apply_failure = function
    | Environment.Ecoproto_error (Script_tc_errors.No_such_entrypoint _ as e)
      :: _ ->
        Assert.test_error_encodings e ;
        return_unit
    | _ -> failwith "no such entrypoint should fail"
  in
  let* (_res : Incremental.t) =
    Incremental.add_operation ~expect_apply_failure i transaction
  in
  return_unit

(** Originates a contract with a [script] and an initial [credit] and
      [storage]. *)
let contract_originate ~baker ~block ~script ~credit ~storage ~source =
  let open Lwt_result_syntax in
  let code = Expr.from_string script in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  let* op, dst =
    Op.contract_origination_hash (B block) source ~fee:Tez.zero ~script ~credit
  in
  let+ state =
    Block.bake ~policy:Block.(By_account baker) ~operations:[op] block
  in
  (state, dst)

(** Runs a transaction from a [source] to a [destination]. *)
let transfer ?force_reveal ?parameters ~baker ~block ~source ~destination amount
    =
  let open Lwt_result_syntax in
  let* operation =
    Op.transaction
      ?force_reveal
      ?parameters
      ~fee:Tez.zero
      (B block)
      source
      destination
      amount
  in
  Block.bake ~policy:Block.(By_account baker) ~operations:[operation] block

(** The script of a contract that transfers its balance to the caller, and
    stores the parameter of the call. *)
let script =
  {| { parameter string ;
     storage string ;
       code {
         CAR ;
         SOURCE ;
         CONTRACT unit ;
         ASSERT_SOME ;
         BALANCE ;
         UNIT ;
         TRANSFER_TOKENS ;
         NIL operation ;
         SWAP ;
         CONS ;
         PAIR }
} |}

(** The tested scenarios are the following :

    - originate a contract with the above [script] and no initial balance,
    call it from an account short of sufficient funds to cover storage fees,
    and check that this indeed fails.

    - originate a contract with the above [script] and sufficient balance to
    cover storage fees of a subsequent call, call the originated contract from
    an account short of sufficient funds to cover storage fees, as expected,
    this succeeds since the caller receives the originated contract's initial
    balance. *)
let test_storage_fees_and_internal_operation () =
  let open Lwt_result_syntax in
  let* initial_block, contract = Context.init1 ~consensus_threshold_size:0 () in
  let null_string = Expr.from_string "\"\"" in
  let caller = Account.new_account () in
  (* Initialize a caller account. *)
  let* initial_block =
    transfer
      ~block:initial_block
      ~baker:(Context.Contract.pkh contract)
      ~source:contract
      ~destination:(Contract.Implicit caller.pkh)
      Tez.one_mutez
  in
  (* [originate_and_call] first, originates a contract with an empty string as
     initial storage, and an initial credit of [initial_amount]. And then, calls
     the originated contract from [caller] with a parameter that allocates
     additional storage. *)
  let originate_and_call ~initial_block ~initial_amount =
    let* block, contract_hash =
      contract_originate
        ~block:initial_block
        ~baker:(Context.Contract.pkh contract)
        ~script
        ~source:contract
        ~credit:initial_amount
        ~storage:null_string
    in
    let random_string = Expr.from_string "\"Abracadabra\"" in
    transfer
      ~force_reveal:true
      ~parameters:(Alpha_context.Script.lazy_expr random_string)
      ~block
      ~baker:(Context.Contract.pkh contract)
      ~source:(Contract.Implicit caller.pkh)
      ~destination:(Contract.Originated contract_hash)
      Tez.zero
  in
  (* Ensure failure when the initial balance of the originated contract is not
     sufficient to pay storage fees. *)
  let*! res = originate_and_call ~initial_block ~initial_amount:Tez.one_mutez in
  let* () =
    Assert.proto_error_with_info ~loc:__LOC__ res "Cannot pay storage fee"
  in
  (* Ensure success when the initial balance of the originated contract is
     sufficient to pay storage fees. *)
  let+ (_ : Block.t) =
    originate_and_call ~initial_block ~initial_amount:Tez.one_cent
  in
  ()

let tests =
  [
    (* single transfer *)
    Tztest.tztest "single transfer" `Quick test_block_with_a_single_transfer;
    Tztest.tztest
      "single transfer with fee"
      `Quick
      test_block_with_a_single_transfer_with_fee;
    (* transfer zero tez *)
    Tztest.tztest "single transfer zero tez" `Quick test_transfer_zero_tez;
    Tztest.tztest
      "transfer zero tez from implicit contract"
      `Quick
      test_transfer_zero_implicit;
    Tztest.tztest
      "transfer zero tez to an implicit contract with balance of src as fee"
      `Quick
      test_transfer_zero_implicit_with_bal_src_as_fee;
    (* transfer to originated contract *)
    Tztest.tztest
      "transfer to originated contract paying transaction fee"
      `Quick
      test_transfer_to_originate_with_fee;
    Tztest.tztest
      "transfer zero tez to an originated contract with balance of src as fee"
      `Quick
      test_transfer_zero_to_originated_with_bal_src_as_fee;
    (* transfer by the balance of contract *)
    Tztest.tztest
      "transfer the amount from source contract balance"
      `Quick
      test_transfer_amount_of_contract_balance;
    (* transfer to itself *)
    Tztest.tztest "transfers to itself" `Quick test_transfers_to_self;
    (* missing operation *)
    Tztest.tztest "missing transaction" `Quick test_missing_transaction;
    (* transfer from/to implicit/originated contracts*)
    Tztest.tztest
      "transfer from an implicit to implicit contract"
      `Quick
      test_transfer_from_implicit_to_implicit_contract;
    Tztest.tztest
      "transfer from an implicit to an originated contract"
      `Quick
      test_transfer_from_implicit_to_originated_contract;
    (* Slow tests *)
    Tztest.tztest
      "block with multiple transfers"
      `Slow
      test_block_with_multiple_transfers;
    (* TODO increase the number of transaction times *)
    Tztest.tztest
      "block with multiple transfer paying fee"
      `Slow
      test_block_with_multiple_transfers_pay_fee;
    Tztest.tztest
      "block with multiple transfer without paying fee"
      `Slow
      test_block_with_multiple_transfers_with_without_fee;
    (* build the chain *)
    Tztest.tztest "build a chain" `Quick test_build_a_chain;
    (* Erroneous *)
    Tztest.tztest "empty implicit" `Quick test_empty_implicit;
    Tztest.tztest
      "balance too low - transfer zero"
      `Quick
      (test_balance_too_low Tez.zero);
    Tztest.tztest "balance too low" `Quick (test_balance_too_low Tez.one);
    Tztest.tztest
      "balance too low (max fee)"
      `Quick
      (test_balance_too_low max_tez);
    Tztest.tztest
      "balance too low with two transfers - transfer zero"
      `Quick
      (test_balance_too_low_two_transfers Tez.zero);
    Tztest.tztest
      "balance too low with two transfers"
      `Quick
      (test_balance_too_low_two_transfers Tez.one);
    Tztest.tztest
      "transfer one tez to an implicit contract with balance of src as fee"
      `Quick
      test_transfer_one_to_implicit_with_bal_src_as_fee;
    Tztest.tztest "invalid_counter" `Quick invalid_counter;
    Tztest.tztest
      "add the same operation twice"
      `Quick
      test_add_the_same_operation_twice;
    Tztest.tztest
      "invalid_counter_in_the_future"
      `Quick
      invalid_counter_in_the_future;
    Tztest.tztest "ownership sender" `Quick test_ownership_sender;
    (* Random tests *)
    Tztest.tztest "random transfer" `Quick test_random_transfer;
    Tztest.tztest "random multi transfer" `Quick test_random_multi_transactions;
    Tztest.tztest "bad entrypoint" `Quick test_bad_entrypoint;
    Tztest.tztest "bad parameter" `Quick test_bad_parameter;
    Tztest.tztest
      "no such entrypoint"
      `Quick
      transfer_to_itself_with_no_such_entrypoint;
    Tztest.tztest
      "storage fees after contract call and allocation"
      `Quick
      test_storage_fees_and_internal_operation;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("transfer", tests)] |> Lwt_main.run
