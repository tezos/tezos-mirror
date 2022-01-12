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
    Invocation: dune exec src/proto_011_PtHangz2/lib_protocol/test/integration/operations/main.exe -- test "^origination$"
    Subject:    On originating contracts.
*)

open Protocol
open Test_tez

let ten_tez = Tez.of_int 10

(** [register_origination fee credit spendable delegatable] takes four
    optional parameter: fee for the fee need to be paid if set to
    create an originated contract; credit is the amount of tez that
    send to this originated contract; spendable default is set to true
    meaning that this contract is spendable; delegatable default is
    set to true meaning that this contract is able to delegate. *)
let register_origination ?(fee = Tez.zero) ?(credit = Tez.zero) () =
  Context.init 1 >>=? fun (b, contracts) ->
  let source = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  Context.Contract.balance (B b) source >>=? fun source_balance ->
  Op.origination (B b) source ~fee ~credit ~script:Op.dummy_script
  >>=? fun (operation, originated) ->
  Block.bake ~operation b >>=? fun b ->
  (* fee + credit + block security deposit were debited from source *)
  Context.get_constants (B b)
  >>=? fun {
             parametric =
               {origination_size; cost_per_byte; block_security_deposit; _};
             _;
           } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  Tez.( +? ) credit block_security_deposit
  >>? Tez.( +? ) fee
  >>? Tez.( +? ) origination_burn
  >>? Tez.( +? ) Op.dummy_script_cost
  >>?= fun total_fee ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) source source_balance total_fee
  >>=? fun () ->
  (* originated contract has been credited *)
  Assert.balance_was_credited ~loc:__LOC__ (B b) originated Tez.zero credit
  >|=? fun () ->
  (* TODO spendable or not and delegatable or not if relevant for some
     test. Not the case at the moment, cf. uses of
     register_origination *)
  (b, source, originated)

(* [test_origination_balances fee credit spendable delegatable]
   takes four optional parameter: fee is the fee that pay if require to create
   an originated contract; credit is the amount of tez that will send to this
   contract; delegatable default is set to true meaning that this contract is
   able to delegate.
   This function will create a contract, get the balance of this contract, call
   the origination operation to create a new originated contract from this
   contract with all the possible fees; and check the balance before/after
   originated operation valid.
   - the source contract has payed all the fees
   - the originated has been credited correctly *)
let test_origination_balances ~loc:_ ?(fee = Tez.zero) ?(credit = Tez.zero) () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Op.origination (B b) contract ~fee ~credit ~script:Op.dummy_script
  >>=? fun (operation, new_contract) ->
  (* The possible fees are: a given credit, an origination burn fee
     (constants_repr.default.origination_burn = 257 mtez),
     a fee that is paid when creating an originate contract.

     We also take into account a block security deposit. Note that it
     is not related to origination but to the baking done in the
     tests.*)
  Context.get_constants (B b)
  >>=? fun {
             parametric =
               {origination_size; cost_per_byte; block_security_deposit; _};
             _;
           } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size)
  >>?= fun origination_burn ->
  Tez.( +? ) credit block_security_deposit
  >>? Tez.( +? ) fee
  >>? Tez.( +? ) origination_burn
  >>? Tez.( +? ) Op.dummy_script_cost
  >>?= fun total_fee ->
  Block.bake ~operation b >>=? fun b ->
  (* check that after the block has been baked the source contract
     was debited all the fees *)
  Assert.balance_was_debited ~loc:__LOC__ (B b) contract balance total_fee
  >>=? fun _ ->
  (* check the balance of the originate contract is equal to credit *)
  Assert.balance_is ~loc:__LOC__ (B b) new_contract credit

(******************************************************)
(* Tests *)
(******************************************************)

(** compute half of the balance and divided it by nth times *)

let two_nth_of_balance incr contract nth =
  Context.Contract.balance (I incr) contract >>=? fun balance ->
  Lwt.return (Tez.( /? ) balance nth >>? fun res -> Tez.( *? ) res 2L)

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
  test_origination_balances ~loc:__LOC__ ~credit:(Tez.of_int 2) ~fee:ten_tez ()

(** Ask source contract to pay a fee when originating a contract. *)
let test_pay_fee () =
  register_origination ~credit:(Tez.of_int 2) ~fee:ten_tez ()
  >>=? fun (_b, _contract, _new_contract) -> return_unit

(******************************************************)
(** Errors *)

(******************************************************)

(** Create an originate contract where the contract does not have
    enough tez to pay for the fee. *)
let test_not_tez_in_contract_to_pay_fee () =
  Context.init 2 >>=? fun (b, contracts) ->
  let contract_1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 0
  in
  let contract_2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth contracts 1
  in
  Incremental.begin_construction b >>=? fun inc ->
  (* transfer everything but one tez from 1 to 2 and check balance of 1 *)
  Context.Contract.balance (I inc) contract_1 >>=? fun balance ->
  Tez.( -? ) balance Tez.one >>?= fun amount ->
  Op.transaction (I inc) contract_1 contract_2 amount >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Assert.balance_was_debited ~loc:__LOC__ (I inc) contract_1 balance amount
  >>=? fun _ ->
  (* use this source contract to create an originate contract where it requires
     to pay a fee and add an amount of credit into this new contract *)
  Op.origination
    (I inc)
    ~fee:ten_tez
    ~credit:Tez.one
    contract_1
    ~script:Op.dummy_script
  >>=? fun (op, _) ->
  Incremental.add_operation inc op >>= fun inc ->
  Assert.proto_error ~loc:__LOC__ inc (function
      | Contract_storage.Balance_too_low _ -> true
      | _ -> false)

(* Set the endorser of the block as manager/delegate of the originated
   account. *)
let register_contract_get_endorser () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  Incremental.begin_construction b >>=? fun inc ->
  Context.get_endorser (I inc) >|=? fun (account_endorser, _slots) ->
  (inc, contract, account_endorser)

(* Create multiple originated contracts and ask contract to pay the fee. *)
let n_originations n ?credit ?fee () =
  List.fold_left_es
    (fun new_contracts _ ->
      register_origination ?fee ?credit ()
      >|=? fun (_b, _source, new_contract) -> new_contract :: new_contracts)
    []
    (1 -- n)

(** Create 100 originations. *)
let test_multiple_originations () =
  n_originations 100 ~credit:(Tez.of_int 2) ~fee:ten_tez ()
  >>=? fun contracts ->
  Assert.equal_int ~loc:__LOC__ (List.length contracts) 100

(** Cannot originate two contracts with the same context's counter. *)
let test_counter () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  Incremental.begin_construction b >>=? fun inc ->
  Op.origination (I inc) ~credit:Tez.one contract ~script:Op.dummy_script
  >>=? fun (op1, _) ->
  Op.origination (I inc) ~credit:Tez.one contract ~script:Op.dummy_script
  >>=? fun (op2, _) ->
  Incremental.add_operation inc op1 >>=? fun inc ->
  Incremental.add_operation inc op2 >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Contract_storage.Counter_in_the_past _ -> true
      | _ -> false)

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
  ]
