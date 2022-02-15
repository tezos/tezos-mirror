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
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/operations/main.exe -- test "^transfer$"
    Subject:    Quantities transfer between contracts.
*)

open Protocol
open Alpha_context
open Test_tez
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
let transfer_to_itself_and_check_balances ~loc b ?(fee = Tez.zero) contract
    amount =
  Context.Contract.balance (I b) contract >>=? fun bal ->
  Op.transaction (I b) ~fee contract contract amount >>=? fun op ->
  Incremental.add_operation b op >>=? fun b ->
  Assert.balance_was_debited ~loc (I b) contract bal fee >|=? fun () -> (b, op)

let ten_tez = Tez.of_int 10

(*********************************************************************)
(* Tests                                                             *)
(*********************************************************************)

(** Compute a fraction of 2/[n] of the balance of [contract] *)
let two_over_n_of_balance incr contract n =
  Context.Contract.balance (I incr) contract >>=? fun balance ->
  Lwt.return (Tez.( /? ) balance n >>? fun res -> Tez.( *? ) res 2L)

(********************)
(** Single transfer *)

(********************)

let single_transfer ?fee ?expect_failure amount =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  transfer_and_check_balances
    ~loc:__LOC__
    ?fee
    ?expect_failure
    b
    contract_1
    contract_2
    amount
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Single transfer without fee. *)
let test_block_with_a_single_transfer () = single_transfer Tez.one

(** Single transfer with fee. *)
let test_block_with_a_single_transfer_with_fee () =
  single_transfer ~fee:Tez.one Tez.one

(** Single transfer without fee. *)
let test_transfer_zero_tez () =
  single_transfer
    ~expect_failure:(function
      | Environment.Ecoproto_error (Contract_storage.Empty_transaction _) :: _
        ->
          return_unit
      | _ -> failwith "Empty transaction should fail")
    Tez.zero

(** Transfer zero tez from an implicit contract. *)
let test_transfer_zero_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let dest = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0 in
  let account = Account.new_account () in
  Incremental.begin_construction b >>=? fun i ->
  let src = Contract.implicit_contract account.Account.pkh in
  Op.transaction (I i) src dest Tez.zero >>=? fun op ->
  Incremental.add_operation i op >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Contract_storage.Empty_implicit_contract _ -> true
      | _ -> false)

(** Transfer to originated contract. *)
let test_transfer_to_originate_with_fee () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Incremental.begin_construction b >>=? fun b ->
  two_over_n_of_balance b contract 10L >>=? fun fee ->
  (* originated contract, paying a fee to originated this contract *)
  Op.origination (I b) ~fee:ten_tez contract ~script:Op.dummy_script
  >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  two_over_n_of_balance b contract 3L >>=? fun amount ->
  transfer_and_check_balances ~loc:__LOC__ b ~fee contract new_contract amount
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Transfer from balance. *)
let test_transfer_amount_of_contract_balance () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Context.Contract.pkh contract_1 >>=? fun pkh1 ->
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1])
  >>=? fun b ->
  (* get the balance of the source contract *)
  Context.Contract.balance (I b) contract_1 >>=? fun balance ->
  (* transfer all the tez inside contract 1 *)
  transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 balance
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Transfer to oneself. *)
let test_transfers_to_self () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  Incremental.begin_construction b >>=? fun b ->
  two_over_n_of_balance b contract 3L >>=? fun amount ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b contract amount
  >>=? fun (b, _) ->
  two_over_n_of_balance b contract 5L >>=? fun fee ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b ~fee contract ten_tez
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Forgot to add the valid transaction into the block. *)
let test_missing_transaction () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  Context.Contract.pkh contract_1 >>=? fun pkh1 ->
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1])
  >>=? fun b ->
  two_over_n_of_balance b contract_1 6L >>=? fun amount ->
  (* Do the transfer 3 times from source contract to destination contract *)
  n_transactions 3 b contract_1 contract_2 amount >>=? fun b ->
  (* do the fourth transfer from source contract to destination contract *)
  Op.transaction (I b) contract_1 contract_2 amount >>=? fun _ ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(********************)
(* The following tests are for different kind of contracts:
   - implicit to implicit
   - implicit to originated
   - originated to implicit
   - originated to originated *)

(********************)

(** Implicit to Implicit. *)
let test_transfer_from_implicit_to_implicit_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  let account_a = Account.new_account () in
  let account_b = Account.new_account () in
  Incremental.begin_construction b >>=? fun b ->
  let src = Contract.implicit_contract account_a.Account.pkh in
  two_over_n_of_balance b bootstrap_contract 3L >>=? fun amount1 ->
  two_over_n_of_balance b bootstrap_contract 10L >>=? fun fee1 ->
  transfer_and_check_balances
    ~with_burn:true
    ~loc:__LOC__
    ~fee:fee1
    b
    bootstrap_contract
    src
    amount1
  >>=? fun (b, _) ->
  (* Create an implicit contract as a destination contract. *)
  let dest = Contract.implicit_contract account_b.pkh in
  two_over_n_of_balance b bootstrap_contract 4L >>=? fun amount2 ->
  two_over_n_of_balance b bootstrap_contract 10L >>=? fun fee2 ->
  (* Transfer from implicit contract to another implicit contract. *)
  transfer_and_check_balances
    ~with_burn:true
    ~loc:__LOC__
    ~fee:fee2
    b
    src
    dest
    amount2
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Implicit to originated. *)
let test_transfer_from_implicit_to_originated_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  let contract =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  let account = Account.new_account () in
  let src = Contract.implicit_contract account.Account.pkh in
  Incremental.begin_construction b >>=? fun b ->
  two_over_n_of_balance b bootstrap_contract 3L >>=? fun amount1 ->
  (* transfer the money to implicit contract *)
  transfer_and_check_balances
    ~with_burn:true
    ~loc:__LOC__
    b
    bootstrap_contract
    src
    amount1
  >>=? fun (b, _) ->
  (* originated contract *)
  Op.origination (I b) contract ~script:Op.dummy_script
  >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  two_over_n_of_balance b bootstrap_contract 4L >>=? fun amount2 ->
  (* transfer from implicit contract to originated contract *)
  transfer_and_check_balances ~loc:__LOC__ b src new_contract amount2
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(********************)
(* Slow tests case *)

(********************)

let multiple_transfer n ?fee amount =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  n_transactions n b ?fee contract_1 contract_2 amount >>=? fun b ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** 1- Create a block with two contracts;
    2- Apply 100 transfers.
*)
let test_block_with_multiple_transfers () =
  multiple_transfer 99 (Tez.of_int 1000)

(** 1- Create a block with two contracts;
    2- Apply 100 transfers with 10tz fee. *)
let test_block_with_multiple_transfers_pay_fee () =
  multiple_transfer 10 ~fee:ten_tez (Tez.of_int 1000)

(* TODO : increase the number of operations and add a `Slow tag to it in `tests` *)

(** 1- Create a block with 8 contracts;
    2- Apply multiple transfers without fees;
    3- Apply multiple transfers with fees. *)
let test_block_with_multiple_transfers_with_without_fee () =
  Context.init 8 >>=? fun (b, contracts) ->
  let contracts = Array.of_list contracts in
  Incremental.begin_construction b >>=? fun b ->
  let hundred = Tez.of_int 100 in
  let ten = Tez.of_int 10 in
  let twenty = Tez.of_int 20 in
  n_transactions 10 b contracts.(0) contracts.(1) Tez.one >>=? fun b ->
  n_transactions 30 b contracts.(1) contracts.(2) hundred >>=? fun b ->
  n_transactions 30 b contracts.(1) contracts.(3) hundred >>=? fun b ->
  n_transactions 30 b contracts.(4) contracts.(3) hundred >>=? fun b ->
  n_transactions 20 b contracts.(0) contracts.(1) hundred >>=? fun b ->
  n_transactions 10 b contracts.(1) contracts.(3) hundred >>=? fun b ->
  n_transactions 10 b contracts.(1) contracts.(3) hundred >>=? fun b ->
  n_transactions 20 ~fee:ten b contracts.(3) contracts.(4) ten >>=? fun b ->
  n_transactions 10 ~fee:twenty b contracts.(4) contracts.(5) ten >>=? fun b ->
  n_transactions 70 ~fee:twenty b contracts.(6) contracts.(0) twenty
  >>=? fun b ->
  n_transactions 550 ~fee:twenty b contracts.(6) contracts.(4) twenty
  >>=? fun b ->
  n_transactions 50 ~fee:ten b contracts.(7) contracts.(5) twenty >>=? fun b ->
  n_transactions 30 ~fee:ten b contracts.(0) contracts.(7) hundred >>=? fun b ->
  n_transactions 20 ~fee:ten b contracts.(1) contracts.(0) twenty >>=? fun b ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Build a chain that has 10 blocks. *)
let test_build_a_chain () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  let ten = Tez.of_int 10 in
  List.fold_left_es
    (fun b _ ->
      Incremental.begin_construction b >>=? fun b ->
      transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 ten
      >>=? fun (b, _) -> Incremental.finalize_block b)
    b
    (1 -- 10)
  >>=? fun _ -> return_unit

(*********************************************************************)
(* Expected error test cases                                         *)
(*********************************************************************)

(** Transferring zero tez is forbidden in implicit contract. *)
let test_empty_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let dest = WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0 in
  let account = Account.new_account () in
  Incremental.begin_construction b >>=? fun incr ->
  let src = Contract.implicit_contract account.Account.pkh in
  two_over_n_of_balance incr dest 3L >>=? fun amount ->
  (* Transfer zero tez from an implicit contract. *)
  Op.transaction (I incr) src dest amount >>=? fun op ->
  Incremental.add_operation incr op >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Contract_storage.Empty_implicit_contract _ -> true
      | _ -> false)

(** Balance is too low to transfer. *)
let test_balance_too_low fee () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance1 ->
  Context.Contract.balance (I i) contract_2 >>=? fun balance2 ->
  (* transfer the amount of tez that is bigger than the balance in the source contract *)
  Op.transaction ~fee (I i) contract_1 contract_2 Tez.max_tez >>=? fun op ->
  let expect_failure = function
    | Environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
        return_unit
    | _ -> failwith "balance too low should fail"
  in
  (* the fee is higher than the balance then raise an error "Balance_too_low" *)
  if fee > balance1 then
    Incremental.add_operation ~expect_failure i op >>= fun _res -> return_unit
    (* the fee is smaller than the balance, then the transfer is accepted
       but it is not processed, and fees are taken *)
  else
    Incremental.add_operation ~expect_failure i op >>=? fun i ->
    (* contract_1 loses the fees *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee
    >>=? fun () ->
    (* contract_2 is not credited *)
    Assert.balance_was_credited ~loc:__LOC__ (I i) contract_2 balance2 Tez.zero

(** 1- Create a block, and three contracts;
    2- Add a transfer that at the end the balance of a contract is
       zero into this block;
    3- Add another transfer that send tez from a zero balance contract;
    4- Catch the expected error: Balance_too_low. *)
let test_balance_too_low_two_transfers fee () =
  Context.init 3 >>=? fun (b, contracts) ->
  let contract_1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  let contract_2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 1
  in
  let contract_3 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 2
  in
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance ->
  Tez.( /? ) balance 3L >>?= fun res ->
  Tez.( *? ) res 2L >>?= fun two_third_of_balance ->
  transfer_and_check_balances
    ~loc:__LOC__
    i
    contract_1
    contract_2
    two_third_of_balance
  >>=? fun (i, _) ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance1 ->
  Context.Contract.balance (I i) contract_3 >>=? fun balance3 ->
  Op.transaction ~fee (I i) contract_1 contract_3 two_third_of_balance
  >>=? fun operation ->
  let expect_failure = function
    | Environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
        return_unit
    | _ -> failwith "balance too low should fail"
  in
  Incremental.add_operation ~expect_failure i operation >>=? fun i ->
  (* contract_1 loses the fees *)
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee
  >>=? fun () ->
  (* contract_3 is not credited *)
  Assert.balance_was_credited ~loc:__LOC__ (I i) contract_3 balance3 Tez.zero

(** The counter is already used for the previous operation. *)
let invalid_counter () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  Op.transaction (I b) contract_1 contract_2 Tez.one >>=? fun op1 ->
  Op.transaction (I b) contract_1 contract_2 Tez.one >>=? fun op2 ->
  Incremental.add_operation b op1 >>=? fun b ->
  Incremental.add_operation b op2 >>= fun b ->
  Assert.proto_error ~loc:__LOC__ b (function
      | Contract_storage.Counter_in_the_past _ -> true
      | _ -> false)

(** Same as before but through a different way to perform this
    error. *)
let test_add_the_same_operation_twice () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 ten_tez
  >>=? fun (b, op_transfer) ->
  Op.transaction (I b) contract_1 contract_2 ten_tez >>=? fun _ ->
  Incremental.add_operation b op_transfer >>= fun b ->
  Assert.proto_error ~loc:__LOC__ b (function
      | Contract_storage.Counter_in_the_past _ -> true
      | _ -> false)

(** Check ownership. *)
let test_ownership_sender () =
  Context.init2 () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  (* get the manager of the contract_1 as a sender *)
  Context.Contract.manager (I b) contract_1 >>=? fun manager ->
  (* create an implicit_contract *)
  let imcontract_1 = Alpha_context.Contract.implicit_contract manager.pkh in
  transfer_and_check_balances ~loc:__LOC__ b imcontract_1 contract_2 Tez.one
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

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
  Context.init 10 >>=? fun (b, contracts) ->
  let contracts = Array.of_list contracts in
  let source = random_contract contracts in
  let dest = random_contract contracts in
  Context.Contract.pkh source >>=? fun source_pkh ->
  (* given that source may not have a sufficient balance for the transfer + to bake,
     make sure it cannot be chosen as baker *)
  Incremental.begin_construction b ~policy:(Block.Excluding [source_pkh])
  >>=? fun b ->
  Context.Contract.balance (I b) source >>=? fun amount ->
  (if source = dest then
   transfer_to_itself_and_check_balances ~loc:__LOC__ b source amount
  else transfer_and_check_balances ~loc:__LOC__ b source dest amount)
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ -> return_unit

(** Transfer random transactions. *)
let test_random_multi_transactions () =
  let n = random_range (1, 100) in
  multiple_transfer n (Tez.of_int 100)

(*********************************************************************)

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
    (* transfer to originated contract *)
    Tztest.tztest
      "transfer to originated contract paying transaction fee"
      `Quick
      test_transfer_to_originate_with_fee;
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
      "transfer from an implicit to implicit contract "
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
      (test_balance_too_low Tez.max_tez);
    Tztest.tztest
      "balance too low with two transfers - transfer zero"
      `Quick
      (test_balance_too_low_two_transfers Tez.zero);
    Tztest.tztest
      "balance too low with two transfers"
      `Quick
      (test_balance_too_low_two_transfers Tez.one);
    Tztest.tztest "invalid_counter" `Quick invalid_counter;
    Tztest.tztest
      "add the same operation twice"
      `Quick
      test_add_the_same_operation_twice;
    Tztest.tztest "ownership sender" `Quick test_ownership_sender;
    (* Random tests *)
    Tztest.tztest "random transfer" `Quick test_random_transfer;
    Tztest.tztest "random multi transfer" `Quick test_random_multi_transactions;
  ]
