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
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/operations/main.exe \
                -- test "^revelation$"
    Subject:    On the reveal operation.
*)

(** Protocol integration tests for the [Reveal] operation. *)

open Protocol
open Alpha_context
open Test_tez

let ten_tez = of_int 10

let test_simple_reveal () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation (B blk) new_c.pk >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
  | true -> ()
  | false -> Stdlib.failwith "New contract revelation failed."

let test_empty_account_on_reveal () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  let amount = Tez.one_mutez in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract amount >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expecting fresh pkh"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation ~fee:amount (B blk) new_c.pk >>=? fun operation ->
  Incremental.begin_construction blk >>=? fun inc ->
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract pkh);
      ]
      when pkh = new_c.pkh ->
        return_unit
    | _ -> assert false
  in
  Incremental.add_operation ~expect_apply_failure inc operation >>=? fun inc ->
  Context.Contract.balance (I inc) new_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ balance Tez.zero >>=? fun () ->
  Context.Contract.is_manager_key_revealed (I inc) new_contract >|=? function
  | false -> ()
  | true -> Stdlib.failwith "Empty account still exists and is revealed."

let test_not_enough_funds_for_reveal () =
  Context.init1 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one_mutez >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation"
   | false -> ())
  >>=? fun () ->
  (* Reveal the contract *)
  Op.revelation ~fee:Tez.fifty_cents (B blk) new_c.pk >>=? fun operation ->
  Block.bake blk ~operation >>= fun res ->
  Assert.proto_error_with_info ~loc:__LOC__ res "Balance too low"

let test_transfer_fees_emptying_after_reveal_batched () =
  Context.init1 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Alpha_context.Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  Incremental.begin_construction blk >>=? fun inc ->
  Op.revelation ~fee:Tez.zero (I inc) new_c.pk >>=? fun reveal ->
  Incremental.add_operation inc reveal >>=? fun tmp_inc ->
  Op.transaction ~fee:Tez.one (I tmp_inc) new_contract c Tez.one
  >>=? fun transaction ->
  Op.batch_operations ~source:new_contract (I inc) [reveal; transaction]
  >>=? fun op ->
  let expect_apply_failure = function
    | [
        Environment.Ecoproto_error (Contract_storage.Empty_implicit_contract pkh);
      ]
      when pkh = new_c.pkh ->
        return_unit
    | _ -> assert false
  in
  Incremental.add_operation ~expect_apply_failure inc op >>=? fun _inc ->
  return_unit

(* We assert that the changes introduced in !5182, splitting the
   application of Reveal operations into a pre-checking and
   an application phase, do not allow to forge dishonest revelations. *)
let test_reveal_with_fake_account () =
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, bootstrap) ->
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
  when_ (Signature.Public_key_hash.equal a_pkh b_pkh) (fun () ->
      failwith
        "Expected different pkhs: got %a %a"
        Signature.Public_key_hash.pp
        a_pkh
        Signature.Public_key_hash.pp
        b_pkh)
  >>=? fun () ->
  Op.transaction (B blk) bootstrap a_contract Tez.one >>=? fun oa ->
  Op.transaction (B blk) bootstrap b_contract Tez.one >>=? fun ob ->
  Op.batch_operations
    ~recompute_counters:true
    ~source:bootstrap
    (B blk)
    [oa; ob]
  >>=? fun batch ->
  Block.bake blk ~operation:batch >>=? fun b ->
  (Context.Contract.is_manager_key_revealed (B blk) a_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expected fresh pkh"
   | false -> ())
  >>=? fun () ->
  (Context.Contract.is_manager_key_revealed (B blk) b_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expected fresh pkh"
   | false -> ())
  >>=? fun () ->
  (* get initial balance of account_a *)
  Context.Contract.balance (B b) a_contract >>=? fun a_balance_before ->
  (* We will attempt to forge a reveal with a fake account that
     impersonates account_a but uses account_b's public and secret
     keys, e.g.

     fake_a = Account.{pkh = account_a.pkh; pk = account_b.pk; sk =
     account_b.sk}

     and we will attempt to reveal the public key of b with a's
     pkh. This operation should fail without updating account_a's
     balance *)
  Op.revelation ~fee:Tez.one_mutez ~forge_pkh:(Some a_pkh) (B b) account_b.pk
  >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
  Incremental.add_operation
    ~expect_failure:(function
      | [
          Environment.Ecoproto_error
            (Contract_manager_storage.Inconsistent_hash _);
        ] ->
          return_unit
      | errs ->
          failwith
            "Expected an Contract_manager_storage.Inconsistent_hash error but \
             got %a"
            Error_monad.pp_print_trace
            errs)
    i
    operation
  >>=? fun i ->
  Context.Contract.balance (I i) a_contract >>=? fun a_balance_after ->
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
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, bootstrap) ->
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
  when_ (Signature.Public_key_hash.equal a_pkh b_pkh) (fun () ->
      failwith
        "Expected different pkhs: got %a %a"
        Signature.Public_key_hash.pp
        a_pkh
        Signature.Public_key_hash.pp
        b_pkh)
  >>=? fun () ->
  Op.transaction (B blk) bootstrap a_contract Tez.one >>=? fun oa ->
  Op.transaction (B blk) bootstrap b_contract Tez.one >>=? fun ob ->
  Op.batch_operations
    ~recompute_counters:true
    ~source:bootstrap
    (B blk)
    [oa; ob]
  >>=? fun batch ->
  Block.bake blk ~operation:batch >>=? fun b ->
  (Context.Contract.is_manager_key_revealed (B blk) a_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expected fresh pkh"
   | false -> ())
  >>=? fun () ->
  (Context.Contract.is_manager_key_revealed (B blk) b_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expected fresh pkh"
   | false -> ())
  >>=? fun () ->
  (* We first reveal a in a block *)
  Op.revelation ~fee:Tez.one_mutez (B b) account_a.pk >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Context.Contract.balance (B b) a_contract >>=? fun a_balance_before ->
  (* Reveal the public key of b while impersonating account_a. This
     operation should fail without updating account_a's balance *)
  Op.revelation ~fee:Tez.one_mutez ~forge_pkh:(Some a_pkh) (B b) account_b.pk
  >>=? fun operation ->
  Incremental.begin_construction b >>=? fun i ->
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
  >>=? fun i ->
  Context.Contract.balance (I i) a_contract >>=? fun a_balance_after ->
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
  Context.init1 ~consensus_threshold:0 () >>=? fun (blk, c) ->
  let new_c = Account.new_account () in
  let new_contract = Contract.Implicit new_c.pkh in
  (* Create the contract *)
  Op.transaction (B blk) c new_contract Tez.one >>=? fun operation ->
  Block.bake blk ~operation >>=? fun blk ->
  (Context.Contract.is_manager_key_revealed (B blk) new_contract >|=? function
   | true -> Stdlib.failwith "Unexpected revelation: expected fresh pkh"
   | false -> ())
  >>=? fun () ->
  Incremental.begin_construction blk >>=? fun inc ->
  Op.revelation ~fee:Tez.zero (I inc) new_c.pk >>=? fun op_reveal ->
  Op.transaction
    ~force_reveal:false
    ~fee:Tez.zero
    (I inc)
    new_contract
    new_contract
    (Tez.of_mutez_exn 1_000_001L)
  >>=? fun op_transfer ->
  Op.batch_operations
    ~recompute_counters:true
    ~source:new_contract
    (I inc)
    [op_reveal; op_transfer]
  >>=? fun batched_operation ->
  let expect_apply_failure = function
    | [Environment.Ecoproto_error (Contract_storage.Balance_too_low _)] ->
        return_unit
    | _ -> assert false
  in
  Incremental.add_operation ~expect_apply_failure inc batched_operation
  >>=? fun inc ->
  (* We assert the manager key is still unrevealed, as the batch has failed *)
  Context.Contract.is_manager_key_revealed (I inc) new_contract
  >>=? fun revelead ->
  when_ revelead (fun () ->
      failwith "Unexpected contract revelation: reveal was expected to fail")

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
  ]
