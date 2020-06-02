(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Protocol
open Alpha_context
open Test_tez

let account_pair = function [a1; a2] -> (a1, a2) | _ -> assert false

let wrap e = Lwt.return (Environment.wrap_error e)

let traverse_rolls ctxt head =
  let rec loop acc roll =
    Storage.Roll.Successor.get_option ctxt roll
    >>= wrap
    >>=? function
    | None -> return (List.rev acc) | Some next -> loop (next :: acc) next
  in
  loop [head] head

let get_rolls ctxt baker =
  Storage.Roll.Baker_roll_list.get_option ctxt baker
  >>= wrap
  >>=? function
  | None -> return_nil | Some head_roll -> traverse_rolls ctxt head_roll

let check_rolls b (baker : baker_hash) =
  Context.get_constants (B b)
  >>=? fun constants ->
  Context.Baker.info (B b) baker
  >>=? fun {staking_balance; _} ->
  let token_per_roll = constants.parametric.tokens_per_roll in
  let expected_rolls =
    Int64.div (Tez.to_mutez staking_balance) (Tez.to_mutez token_per_roll)
  in
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  get_rolls ctxt baker
  >>=? fun rolls ->
  Assert.equal_int
    ~loc:__LOC__
    (List.length rolls)
    (Int64.to_int expected_rolls)

let check_no_rolls (b : Block.t) (baker : baker_hash) =
  Raw_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun ctxt ->
  get_rolls ctxt baker
  >>=? fun rolls -> Assert.equal_int ~loc:__LOC__ (List.length rolls) 0

let simple_staking_rights () =
  Context.init 2
  >>=? fun (b, accounts, bakers) ->
  let a1 = List.hd accounts in
  Context.Contract.balance (B b) a1
  >>=? fun balance ->
  let b1 = List.hd bakers in
  Context.Baker.info (B b) b1
  >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance
  >>=? fun () -> check_rolls b b1

let simple_staking_rights_after_baking () =
  Context.init 2
  >>=? fun (b, accounts, bakers) ->
  let a1 = List.hd accounts in
  Context.Contract.balance (B b) a1
  >>=? fun balance ->
  let b1 = List.nth bakers 0 in
  let b2 = List.nth bakers 1 in
  Block.bake_n ~policy:(By_account b2) 5 b
  >>=? fun b ->
  Context.Baker.info (B b) b1
  >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance
  >>=? fun () -> check_rolls b b1 >>=? fun () -> check_rolls b b2

let frozen_deposit (info : Context.Baker.info) =
  Cycle.Map.fold
    (fun _ {Baker.deposit; _} acc -> Test_tez.Tez.(deposit + acc))
    info.frozen_balance_by_cycle
    Tez.zero

let check_activate_staking_balance ~loc ~deactivated
    ?(delegate : Contract.t option) b (baker : baker_hash) =
  Context.Baker.info (B b) baker
  >>=? fun info ->
  Assert.equal_bool ~loc info.deactivated deactivated
  >>=? fun () ->
  Context.Contract.balance (B b) (Contract.baker_contract baker)
  >>=? fun baker_balance ->
  ( match delegate with
  | None ->
      return Tez.zero
  | Some d ->
      Context.Contract.balance (B b) d )
  >>=? fun delegate_balance ->
  let deposit = frozen_deposit info in
  Assert.equal_tez
    ~loc
    Test_tez.Tez.(delegate_balance + deposit + baker_balance)
    info.staking_balance

let run_until_deactivation () =
  Context.init 2
  >>=? fun (b, accounts, bakers) ->
  let (a1, a2) = account_pair accounts in
  Context.Contract.balance (B b) a1
  >>=? fun balance_start ->
  let b1 = List.nth bakers 0 in
  let b2 = List.nth bakers 1 in
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b b1
  >>=? fun () ->
  Context.Baker.info (B b) b1
  >>=? fun info ->
  Block.bake_until_cycle ~policy:(By_account b2) info.grace_period b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b b1
  >>=? fun () ->
  Block.bake_until_cycle_end ~policy:(By_account b2) b
  >>=? fun b ->
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:true b b1
  >|=? fun () -> (b, ((a1, b1), balance_start), (a2, b2))

let deactivation_then_bake () =
  run_until_deactivation ()
  >>=? fun ( b,
             ((_impl_contract, deactivated_baker), _start_balance),
             (_a2, _b2) ) ->
  Block.bake ~policy:(By_account deactivated_baker) b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () -> check_rolls b deactivated_baker

let deactivation_then_reactivation () =
  run_until_deactivation ()
  >>=? fun (b, ((impl_contract, deactivated_baker), start_balance), (_a2, b2)) ->
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_active true)
    impl_contract
    deactivated_baker
  >>=? fun reactivation ->
  Block.bake ~policy:(By_account b2) b ~operation:reactivation
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () ->
  Context.Contract.balance (B b) (Contract.baker_contract deactivated_baker)
  >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ start_balance balance
  >>=? fun () -> check_rolls b deactivated_baker

let deactivation_then_empty_then_reactivation () =
  run_until_deactivation ()
  >>=? fun (b, ((impl_contract, deactivated_baker), _start_balance), (_a2, b2)) ->
  (* empty the baker contract *)
  let baker_contract = Contract.baker_contract deactivated_baker in
  Context.Contract.balance (B b) baker_contract
  >>=? fun balance ->
  let sink_account = Account.new_account () in
  Op.baker_action
    (B b)
    ~action:
      (Client_proto_baker.Transfer_to_implicit (balance, sink_account.pkh))
    impl_contract
    deactivated_baker
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account b2) ~operation:empty_contract b
  >>=? fun b ->
  (* re-activation *)
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_active true)
    impl_contract
    deactivated_baker
  >>=? fun reactivation ->
  Block.bake ~policy:(By_account b2) ~operation:reactivation b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () ->
  Context.Contract.balance (B b) baker_contract
  >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance
  >>=? fun () -> check_rolls b deactivated_baker

let deactivation_then_empty_then_reactivation_then_recredit () =
  run_until_deactivation ()
  >>=? fun (b, ((impl_contract, deactivated_baker), balance), (_a2, b2)) ->
  (* empty the baker contract *)
  let baker_contract = Contract.baker_contract deactivated_baker in
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Op.baker_action
    (B b)
    ~action:
      (Client_proto_baker.Transfer_to_implicit (balance, sink_account.pkh))
    impl_contract
    deactivated_baker
  >>=? fun empty_contract ->
  Block.bake ~policy:(By_account b2) ~operation:empty_contract b
  >>=? fun b ->
  (* re-activation *)
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_active true)
    impl_contract
    deactivated_baker
  >>=? fun reactivation ->
  Block.bake ~policy:(By_account b2) ~operation:reactivation b
  >>=? fun b ->
  (* recredit *)
  Op.transaction (B b) sink_contract baker_contract balance
  >>=? fun recredit_contract ->
  Block.bake ~policy:(By_account b2) ~operation:recredit_contract b
  >>=? fun b ->
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    b
    deactivated_baker
  >>=? fun () ->
  Context.Contract.balance (B b) baker_contract
  >>=? fun new_balance ->
  Assert.equal_tez ~loc:__LOC__ balance new_balance
  >>=? fun () -> check_rolls b deactivated_baker

let delegation () =
  Context.init 3
  >>=? fun (b, contracts, bakers) ->
  let c1 = List.hd contracts in
  let c2 = List.nth contracts 1 in
  let c3 = List.nth contracts 2 in
  Context.Contract.find_account (B b) c3
  >>=? fun a3 ->
  let b1 = List.hd bakers in
  let b2 = List.nth bakers 1 in
  Op.delegation (B b) c1 (Some b1)
  >>=? fun delegation1 ->
  Op.delegation (B b) c2 (Some b2)
  >>=? fun delegation2 ->
  Block.bake ~policy:(By_account b2) b ~operations:[delegation1; delegation2]
  >>=? fun b ->
  Context.Contract.delegate_opt (B b) c1
  >>=? fun delegate ->
  ( match delegate with
  | None ->
      assert false
  | Some baker ->
      assert (Baker_hash.equal baker b1) ) ;
  let consensus_key_acc = Account.new_account () in
  Op.baker_registration
    ~consensus_key:consensus_key_acc.pk
    ~threshold:1
    ~owner_keys:[a3.pk]
    (B b)
    c3
  >>=? fun (registration, new_baker) ->
  Block.bake ~policy:(By_account b2) b ~operation:registration
  >>=? fun b ->
  check_no_rolls b new_baker
  >>=? fun () ->
  Op.delegation (B b) c3 (Some new_baker)
  >>=? fun delegation ->
  Block.bake ~policy:(By_account b2) b ~operation:delegation
  >>=? fun b ->
  Context.Contract.delegate_opt (B b) c3
  >>=? fun delegate ->
  ( match delegate with
  | None ->
      assert false
  | Some baker ->
      assert (Baker_hash.equal baker new_baker) ) ;
  check_activate_staking_balance
    ~loc:__LOC__
    ~deactivated:false
    ~delegate:c3
    b
    new_baker
  >>=? fun () -> check_rolls b new_baker >>=? fun () -> check_rolls b b1

let tests =
  [ Test.tztest "simple staking rights" `Quick simple_staking_rights;
    Test.tztest
      "simple staking rights after baking"
      `Quick
      simple_staking_rights_after_baking;
    Test.tztest "deactivation then bake" `Quick deactivation_then_bake;
    Test.tztest
      "deactivation then re-activation"
      `Quick
      deactivation_then_reactivation;
    Test.tztest
      "deactivation then empty then re-activation"
      `Quick
      deactivation_then_empty_then_reactivation;
    Test.tztest
      "deactivation then empty then re-activation then recredit"
      `Quick
      deactivation_then_empty_then_reactivation_then_recredit;
    Test.tztest "delegation" `Quick delegation ]
