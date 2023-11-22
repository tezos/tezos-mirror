(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs. <contact@nomadic-labs.com>          *)
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
    Component:  Protocol (revelation)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_reveal.ml
    Subject:    On the reveal operation.
*)

(** Protocol integration tests for the [Reveal] operation. *)

open Protocol
open Alpha_context
open Test_tez

let ten_tez = of_int 10

let test_simple_reveal () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then Stdlib.failwith "Unexpected revelation"
  in
  (* Reveal the contract *)
  let* operation = Op.revelation (B blk) new_c.pk in
  let* blk = Block.bake blk ~operation in
  let+ is_revealed =
    Context.Contract.is_manager_key_revealed (B blk) new_contract
  in
  if not is_revealed then Stdlib.failwith "New contract revelation failed."

let test_empty_account_on_reveal () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  let amount = Tez.one_mutez in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract amount in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expecting fresh pkh"
  in
  (* Reveal the contract *)
  let* operation = Op.revelation ~fee:amount (B blk) new_c.pk in
  let* inc = Incremental.begin_construction blk in
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract pkh);
      ]
      when pkh = new_c.pkh ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_apply_failure inc operation in
  let* balance = Context.Contract.balance (I inc) new_contract in
  let* () = Assert.equal_tez ~loc:__LOC__ balance Tez.zero in
  let+ is_revealed =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  if is_revealed then
    Stdlib.failwith "Empty account still exists and is revealed."

let test_not_enough_funds_for_reveal () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 () in
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one_mutez in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then Stdlib.failwith "Unexpected revelation"
  in
  (* Reveal the contract *)
  let* operation = Op.revelation ~fee:Tez.fifty_cents (B blk) new_c.pk in
  let*! res = Block.bake blk ~operation in
  Assert.proto_error_with_info ~loc:__LOC__ res "Balance too low"

let test_transfer_fees_emptying_after_reveal_batched () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 () in
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* inc = Incremental.begin_construction blk in
  let* reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* tmp_inc = Incremental.add_operation inc reveal in
  let* transaction =
    Op.transaction ~fee:Tez.one (I tmp_inc) new_contract c Tez.one
  in
  let* op =
    Op.batch_operations ~source:new_contract (I inc) [reveal; transaction]
  in
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract pkh);
      ]
      when pkh = new_c.pkh ->
        return_unit
    | _ -> assert false
  in
  let* (_inc : Incremental.t) =
    Incremental.add_operation ~expect_apply_failure inc op
  in
  return_unit

(* We assert that the changes introduced in !5182, splitting the
   application of Reveal operations into a pre-checking and
   an application phase, do not allow to forge dishonest revelations. *)
let test_reveal_with_fake_account () =
  let open Lwt_result_syntax in
  let* blk, bootstrap = Context.init1 ~consensus_threshold:0 () in
  (* Create two fresh, unrevealed, accounts a and b. *)
  let account_a = Account.new_account () in
  let a_pkh = account_a.pkh in
  let a_contract = Contract.Implicit a_pkh in
  let account_b = Account.new_account () in
  let b_pkh = account_b.pkh in
  let b_contract = Contract.Implicit b_pkh in
  (* Assert a and b are fresh.*)
  (* TODO tezos/tezos#2996

     These preambles are too verbose and boilerplate. We should factor
     out revealing fresh unrevealed accounts. *)
  let* () =
    when_ (Signature.Public_key_hash.equal a_pkh b_pkh) (fun () ->
        failwith
          "Expected different pkhs: got %a %a"
          Signature.Public_key_hash.pp
          a_pkh
          Signature.Public_key_hash.pp
          b_pkh)
  in
  let* oa = Op.transaction (B blk) bootstrap a_contract Tez.one in
  let* ob = Op.transaction (B blk) bootstrap b_contract Tez.one in
  let* batch =
    Op.batch_operations
      ~recompute_counters:true
      ~source:bootstrap
      (B blk)
      [oa; ob]
  in
  let* b = Block.bake blk ~operation:batch in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) a_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) b_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  (* get initial balance of account_a *)
  let* a_balance_before = Context.Contract.balance (B b) a_contract in
  (* We will attempt to forge a reveal with a fake account that
     impersonates account_a but uses account_b's public and secret
     keys, e.g.

     fake_a = Account.{pkh = account_a.pkh; pk = account_b.pk; sk =
     account_b.sk}

     and we will attempt to reveal the public key of b with a's
     pkh. This operation should fail without updating account_a's
     balance *)
  let* operation =
    Op.revelation ~fee:Tez.one_mutez ~forge_pkh:(Some a_pkh) (B b) account_b.pk
  in
  let* i = Incremental.begin_construction b in
  let* i =
    Incremental.add_operation
      ~expect_failure:(function
        | [
            Environment.Ecoproto_error
              (Contract_manager_storage.Inconsistent_hash _);
          ] ->
            return_unit
        | errs ->
            failwith
              "Expected an Contract_manager_storage.Inconsistent_hash error \
               but got %a"
              Error_monad.pp_print_trace
              errs)
      i
      operation
  in
  let* a_balance_after = Context.Contract.balance (I i) a_contract in
  unless (Tez.equal a_balance_after a_balance_before) (fun () ->
      failwith
        "Balance of contract_a should have not changed: expected %atz, got %atz"
        Tez.pp
        a_balance_before
        Tez.pp
        a_balance_after)

(* On the following test, we create an account a, fund it, reveal it,
   and get its balance. Then we attempt to forge a reveal for another
   account b, using a's pkh. *)
let test_reveal_with_fake_account_already_revealed () =
  let open Lwt_result_syntax in
  let* blk, bootstrap = Context.init1 ~consensus_threshold:0 () in
  (* Create two fresh, unrevealed, accounts a and b. *)
  let account_a = Account.new_account () in
  let a_pkh = account_a.pkh in
  let a_contract = Contract.Implicit a_pkh in
  let account_b = Account.new_account () in
  let b_pkh = account_b.pkh in
  let b_contract = Contract.Implicit b_pkh in
  (* Assert a and b are fresh.*)
  (* TODO tezos/tezos#2996

     These preambles are too verbose and boilerplate. We should factor
     out revealing fresh unrevealed accounts. *)
  let* () =
    when_ (Signature.Public_key_hash.equal a_pkh b_pkh) (fun () ->
        failwith
          "Expected different pkhs: got %a %a"
          Signature.Public_key_hash.pp
          a_pkh
          Signature.Public_key_hash.pp
          b_pkh)
  in
  let* oa = Op.transaction (B blk) bootstrap a_contract Tez.one in
  let* ob = Op.transaction (B blk) bootstrap b_contract Tez.one in
  let* batch =
    Op.batch_operations
      ~recompute_counters:true
      ~source:bootstrap
      (B blk)
      [oa; ob]
  in
  let* b = Block.bake blk ~operation:batch in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) a_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) b_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  (* We first reveal a in a block *)
  let* operation = Op.revelation ~fee:Tez.one_mutez (B b) account_a.pk in
  let* b = Block.bake ~operation b in
  let* a_balance_before = Context.Contract.balance (B b) a_contract in
  (* Reveal the public key of b while impersonating account_a. This
     operation should fail without updating account_a's balance *)
  let* operation =
    Op.revelation ~fee:Tez.one_mutez ~forge_pkh:(Some a_pkh) (B b) account_b.pk
  in
  let* i = Incremental.begin_construction b in
  let* i =
    Incremental.add_operation
      ~expect_failure:(function
        | [
            Environment.Ecoproto_error
              (Contract_manager_storage.Inconsistent_hash _);
          ] ->
            return_unit
        | errs ->
            failwith
              "Expected a Previously_revealed_key error but got %a"
              Error_monad.pp_print_trace
              errs)
      i
      operation
  in
  let* a_balance_after = Context.Contract.balance (I i) a_contract in
  unless (Tez.equal a_balance_after a_balance_before) (fun () ->
      failwith
        "Balance of contract_a should have not changed: expected %atz, got %atz"
        Tez.pp
        a_balance_before
        Tez.pp
        a_balance_after)

(* cf: #2386

   On tezos/tezos!5182 we have reverted the behaviour implemented by
   tezos/tezos!587, which explicitly avoided marking reveal operations
   as backtracked to reflect the fact that a reveal in a failing batch
   did still take effect (cf #338).

   We test that backtracked reveals stay backtracked. *)
let test_backtracked_reveal_in_batch () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* inc = Incremental.begin_construction blk in
  let* op_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* op_transfer =
    Op.transaction
      ~force_reveal:false
      ~fee:Tez.zero
      (I inc)
      new_contract
      new_contract
      (Tez.of_mutez_exn 1_000_001L)
  in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [op_reveal; op_transfer]
  in
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Balance_too_low _);
        Environment.Ecoproto_error (Tez_repr.Subtraction_underflow _);
      ] ->
        return_unit
    | err ->
        failwith
          "Error trace:@, %a does not match the expected one"
          Error_monad.pp_print_trace
          err
  in
  let* inc =
    Incremental.add_operation ~expect_apply_failure inc batched_operation
  in
  (* We assert the manager key is still unrevealed, as the batch has failed *)
  let* revelead =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  when_ revelead (fun () ->
      failwith "Unexpected contract revelation: reveal was expected to fail")

(* Asserts that re-revealing an already revealed manager will make the
   whole batch fail. *)
let test_already_revealed_manager_in_batch () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expecting fresh pkh"
  in
  (* Reveal the contract *)
  let* operation = Op.revelation (B blk) new_c.pk in
  let* blk = Block.bake blk ~operation in
  (* We pack a correct batch of operations attempting to re-reveal the contract *)
  let* inc = Incremental.begin_construction blk in
  let* op_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* op_transfer =
    Op.transaction
      ~force_reveal:false
      ~fee:Tez.zero
      (I inc)
      new_contract
      new_contract
      (Tez.of_mutez_exn 1_000_001L)
  in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (B blk)
      [op_reveal; op_transfer]
  in
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error
          (Contract_manager_storage.Previously_revealed_key _);
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc =
    Incremental.add_operation ~expect_apply_failure inc batched_operation
  in
  (* We assert the manager key is still revealed. *)
  let* revelead =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  unless revelead (fun () ->
      Stdlib.failwith
        "Unexpected unrevelation: failing batch shouldn't unreveal the manager")

(* cf: #2386

   We imitate the behaviour of

   https://tzkt.io/ooSocfx3xxzDo7eFyGu6ZDR1svzMrbaJtBikQanXXhwrqMuWfGz

   which provides evidence of a failing reveal with a gas exhaustion
   error due to an incorrect gas limit of 0, which still takes effect
   as witnessed by the subsequent (reveal-less) transfer

   https://tzkt.io/opBQQJQ5senPP5v8PfPFf4uVQqKRE5RVjbwx8uD4SqeRs2JGcVw

   This showcases a bad separation of concerns between pre-checking
   and the application of manager operations per-se within
   [Protocol.Apply.apply_operation]. The situation originated because
   [precheck_manager_contents_lists] would reveal the manager by
   calling [Protocol.Alpha_context.Contract.reveal_manager_key] before
   [prepare_apply_manager_operation_content] has consumed the declared
   gas.

   With !5182 we have fixed this situation by revealing the manager
   contract at application time. The following test isolates the
   failing reveal and asserts that the manager is not revealed after
   the failing op.

   As of !5506, the reveal operation does not pass precheck
   anyway. Unfortunately, this means that this test has lost some of
   its original purpose. Fortunately, {!test_empty_account_on_reveal}
   offers a similar scenario to what this test was supposed to do: a
   reveal fails during application and we check that the contract is
   not revealed afterward. *)
let test_no_reveal_when_gas_exhausted () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Fund the contract with a sufficient balance *)
  let* operation =
    Op.transaction (B blk) c new_contract (Tez.of_mutez_exn 1_000L)
  in
  (* Create the contract *)
  let* blk = Block.bake blk ~operation in
  (* Assert that the account has not been revealed yet *)
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  (* We craft a new (bad) reveal operation with a 0 gas_limit *)
  let* op = Op.revelation ~fee:Tez.zero ~gas_limit:Zero (B blk) new_c.pk in
  let* inc = Incremental.begin_construction blk in
  (* The application of this operation is expected to fail with a
     {! Protocol.Raw_context.Operation_quota_exceeded} error *)
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Insufficient_gas_for_manager;
        Environment.Ecoproto_error Raw_context.Operation_quota_exceeded;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc op in
  (* We assert the manager key is still unrevealed, as the operation has failed *)
  let* revelead =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  when_ revelead (fun () ->
      failwith "Unexpected revelation: reveal operation failed")

(* Fix #2774

   We test that reveals can only succeed if they are placed at the
   first position in a batch of manager operations, and that moreover
   reveal operations occur uniquely in batches.

   - First, [test_reveal_incorrect_position_in_batch] asserts that a
   [[transfer; reveal]] batch where a valid reveal follows another
   valid op (different from a reveal, so here a transfer) fails with
   an [Apply.Incorrect_reveal_position] error.

   - Second, we test a batch consisting of duplicate (potentially)
   valid reveal operations. We assert the second reveal to fail again
   with an [Apply.Incorrect_reveal_position] error, and for the first
   reveal to be backtracked.

   - Then, we test batches with duplicate reveals which follow a
   failing one and we assert again the second reveal fails skipped. We
   do this for the 3 different reasons a well-placed reveal might fail
   (as tested above): gas exhaustion, insolvency, and emptying the
   balance while revealing.
*)
let test_reveal_incorrect_position_in_batch () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* inc = Incremental.begin_construction blk in
  let* op_transfer =
    Op.transaction
      ~force_reveal:false
      ~fee:Tez.zero
      (I inc)
      new_contract
      new_contract
      (Tez.of_mutez_exn 1L)
  in
  let* op_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [op_transfer; op_reveal]
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Incorrect_reveal_position;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc batched_operation in
  (* We assert the manager key is still unrevealed, as the operation has failed *)
  let* revelead =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  when_ revelead (fun () ->
      failwith "Unexpected revelation: reveal operation was expected to fail")

(* Test that a batch [reveal pk; reveal pk] where the first reveal
   succeeds but the second one results in the second one failing, and
   then first reveal being backtracked. *)
let test_duplicate_valid_reveals () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* inc = Incremental.begin_construction blk in
  let* op_rev1 = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* op_rev2 = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [op_rev1; op_rev2]
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Incorrect_reveal_position;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc batched_operation in
  (* We assert the manager key is still unrevealed, as the operation has failed *)
  let* revelead =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  when_ revelead (fun () ->
      failwith "Unexpected contract revelation: backtracking expected")

(* Test that a batch [failed_reveal pk; reveal pk] where the first
   reveal fails with a gas exhaustion results in the second one
   failing due to not being well-placed at the beginnning of the
   batch. *)
let test_valid_reveal_after_gas_exhausted_one () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* inc = Incremental.begin_construction blk in
  (* We first craft a (bad) reveal operation with a 0 gas_limit *)
  let* bad_reveal =
    Op.revelation ~fee:Tez.zero ~gas_limit:Zero (B blk) new_c.pk
  in
  (* While the second is a valid one *)
  let* good_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [bad_reveal; good_reveal]
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Incorrect_reveal_position;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc batched_operation in
  (* We assert the manager key is still unrevealed, as the batch has failed *)
  let+ revealed =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  if revealed then
    Stdlib.failwith "Unexpected contract revelation: no valid reveal in batch"

(* Test that a batch [failed_reveal pk; reveal pk; transfer] where the
   first reveal fails with insufficient funds results in the second
   one failing due to not being well-placed at the beginnning of the
   batch. We add the trailing transfer to ensure covering all branches
   of `check_batch_tail_sanity` in `find_manager_public_key` when
   calling {!Apply.check_manager_signature} to verify the manager's pk
   while processing the second reveal. *)
let test_valid_reveal_after_insolvent_one () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract Tez.one in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then
      Stdlib.failwith "Unexpected revelation: expected fresh pkh"
  in
  let* inc = Incremental.begin_construction blk in
  (* We first craft an insolvent reveal operation *)
  let* bad_reveal = Op.revelation ~fee:ten_tez (B blk) new_c.pk in
  (* While the second is a free valid one *)
  let* good_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* transfer = Op.transaction ~fee:Tez.zero (I inc) new_contract c Tez.one in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [bad_reveal; good_reveal; transfer]
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Incorrect_reveal_position;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc batched_operation in
  (* We assert the manager key is still unrevealed, as the batch has failed *)
  let+ revealed =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  if revealed then
    Stdlib.failwith "Unexpected contract revelation: no valid reveal in batch"

(* Test that a batch [failed_reveal pk; reveal pk] where the first
   reveal fails with insufficient funds results in the second one
   failing due to not being well-placed at the beginnning of the
   batch. *)
let test_valid_reveal_after_emptying_balance () =
  let open Lwt_result_syntax in
  let* blk, c = Context.init1 ~consensus_threshold:0 () in
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  let amount = Tez.one_mutez in
  (* Create the contract *)
  let* operation = Op.transaction (B blk) c new_contract amount in
  let* blk = Block.bake blk ~operation in
  let* () =
    let+ is_revealed =
      Context.Contract.is_manager_key_revealed (B blk) new_contract
    in
    if is_revealed then Stdlib.failwith "Unexpected revelation"
  in
  let* inc = Incremental.begin_construction blk in
  (* Reveal the contract, spending all its balance in fees *)
  let* bad_reveal = Op.revelation ~fee:amount (B blk) new_c.pk in
  (* While the second is a free valid one *)
  let* good_reveal = Op.revelation ~fee:Tez.zero (I inc) new_c.pk in
  let* batched_operation =
    Op.batch_operations
      ~recompute_counters:true
      ~source:new_contract
      (I inc)
      [bad_reveal; good_reveal]
  in
  let expect_failure = function
    | [
        Environment.Ecoproto_error
          Validate_errors.Manager.Incorrect_reveal_position;
      ] ->
        return_unit
    | _ -> assert false
  in
  let* inc = Incremental.add_operation ~expect_failure inc batched_operation in
  (* We assert the manager key is still unrevealed, as the batch has failed *)
  let+ revealed =
    Context.Contract.is_manager_key_revealed (I inc) new_contract
  in
  if revealed then
    Stdlib.failwith "Unexpected contract revelation: no valid reveal in batch"

let tests =
  [
    Tztest.tztest "simple reveal" `Quick test_simple_reveal;
    Tztest.tztest "empty account on reveal" `Quick test_empty_account_on_reveal;
    Tztest.tztest
      "not enough funds for reveal"
      `Quick
      test_not_enough_funds_for_reveal;
    Tztest.tztest
      "transfer fees emptying balance after reveal in batch"
      `Quick
      test_transfer_fees_emptying_after_reveal_batched;
    Tztest.tztest
      "cannot forge reveal with fake keys and signature"
      `Quick
      test_reveal_with_fake_account;
    Tztest.tztest
      "cannot re-reveal an account with fake keys and signature"
      `Quick
      test_reveal_with_fake_account_already_revealed;
    Tztest.tztest
      "a backtracked reveal in a batch doesn't take effect"
      `Quick
      test_backtracked_reveal_in_batch;
    Tztest.tztest
      "cannot re-reveal a manager in a batch"
      `Quick
      test_already_revealed_manager_in_batch;
    Tztest.tztest
      "do not reveal when gas exhausted"
      `Quick
      test_no_reveal_when_gas_exhausted;
    Tztest.tztest
      "incorrect reveal position in batch"
      `Quick
      test_reveal_incorrect_position_in_batch;
    Tztest.tztest
      "cannot duplicate valid reveals in batch"
      `Quick
      test_duplicate_valid_reveals;
    Tztest.tztest
      "cannot batch a good reveal after a gas-exhausted one"
      `Quick
      test_valid_reveal_after_gas_exhausted_one;
    Tztest.tztest
      "cannot batch a good reveal after an insolvent one"
      `Quick
      test_valid_reveal_after_insolvent_one;
    Tztest.tztest
      "cannot batch a good reveal after one emptying account"
      `Quick
      test_valid_reveal_after_emptying_balance;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("revelation", tests)]
  |> Lwt_main.run
