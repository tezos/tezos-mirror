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

(** Testing
    -------
    Component:    Protocol (baking)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/main.exe -- test "^baking$"
    Subject:      Rewards and bakers. Tests based on RPCs.
*)

open Protocol
open Alpha_context

(** Verify the level is correctly computed when the first cycle is
    passed and after baking a certain fixed number of blocks (10 for
    the moment). The result should be [blocks_per_cycle + 10] where
    [blocks_per_cycle] comes from the constants of the selected
    protocol.

    IMPROVEMENTS:
    - Randomize the number of cycle.
    - Randomize the number of accounts created at the beginning
    - Randomize the blocks per cycle.
    - Randomize the number of blocks baked after the n cycles baked
      previously. *)
let test_cycle () =
  Context.init 5
  >>=? fun (b, _, _) ->
  Context.get_constants (B b)
  >>=? fun csts ->
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in
  let pp fmt x = Format.fprintf fmt "%ld" x in
  Block.bake b
  >>=? fun b ->
  Block.bake_until_cycle_end b
  >>=? fun b ->
  Context.get_level (B b)
  >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    blocks_per_cycle
  >>=? fun () ->
  Context.get_level (B b)
  >>?= fun l ->
  Block.bake_n 10 b
  >>=? fun b ->
  Context.get_level (B b)
  >>?= fun curr_level ->
  Assert.equal
    ~loc:__LOC__
    Int32.equal
    "not the right level"
    pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    (Int32.add (Alpha_context.Raw_level.to_int32 l) 10l)

(** After baking and/or endorsing a block, the baker and the endorsers
    get their reward. *)
let test_rewards_retrieval () =
  Context.init 256
  >>=? fun (b, _, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.
             { parametric =
                 { endorsers_per_block;
                   block_security_deposit;
                   endorsement_security_deposit;
                   _ };
               _ } ->
  (* find block with 32 different endorsers *)
  let open Alpha_services.Baker.Endorsing_rights in
  let rec find_block b =
    Context.get_endorsers (B b)
    >>=? fun endorsers ->
    if List.length endorsers = endorsers_per_block then return b
    else Block.bake b >>=? fun b -> find_block b
  in
  let balance_update baker before after =
    Context.Baker.info (B before) baker
    >>=? fun info_before ->
    Context.Baker.info (B after) baker
    >>=? fun info_after ->
    Lwt.return
      Test_tez.Tez.(info_after.frozen_balance -? info_before.frozen_balance)
  in
  find_block b
  >>=? fun good_b ->
  Context.get_endorsers (B good_b)
  >>=? fun endorsers ->
  (* test 3 different priorities, too long otherwise *)
  let block_priorities = 0 -- 10 in
  let included_endorsements = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities included_endorsements in
  List.iter_es
    (fun (priority, endorsing_power) ->
      (* bake block at given priority and with given endorsing_power *)
      let real_endorsers = List.sub endorsers endorsing_power in
      List.map_ep
        (fun endorser ->
          Op.endorsement ~baker:endorser.baker (B good_b) ()
          >|=? fun operation -> Operation.pack operation)
        real_endorsers
      >>=? fun operations ->
      let policy = Block.By_priority priority in
      Block.get_next_baker ~policy good_b
      >>=? fun (baker, _, _) ->
      Block.bake ~policy ~operations good_b
      >>=? fun b ->
      Context.get_baking_reward (B b) ~priority ~endorsing_power
      >>=? fun baking_reward ->
      Test_tez.Tez.(block_security_deposit +? baking_reward)
      >>?= fun baking_frozen_balance ->
      Context.get_endorsing_reward (B b) ~priority ~endorsing_power:1
      >>=? fun endorsing_reward ->
      Test_tez.Tez.(endorsement_security_deposit +? endorsing_reward)
      >>?= fun endorsing_frozen_balance ->
      let baker_is_not_an_endorser =
        List.for_all (fun endorser -> endorser.baker <> baker) real_endorsers
      in
      Test_tez.Tez.(baking_frozen_balance +? endorsing_frozen_balance)
      >>?= fun accumulated_frozen_balance ->
      (* check the baker was rewarded the right amount *)
      balance_update baker good_b b
      >>=? fun baker_frozen_balance ->
      ( if baker_is_not_an_endorser then
        Assert.equal_tez
          ~loc:__LOC__
          baker_frozen_balance
          baking_frozen_balance
      else
        Assert.equal_tez
          ~loc:__LOC__
          baker_frozen_balance
          accumulated_frozen_balance )
      >>=? fun () ->
      (* check the each endorser was rewarded the right amount *)
      List.iter_ep
        (fun endorser ->
          balance_update endorser.baker good_b b
          >>=? fun endorser_frozen_balance ->
          if baker <> endorser.baker then
            Assert.equal_tez
              ~loc:__LOC__
              endorser_frozen_balance
              endorsing_frozen_balance
          else
            Assert.equal_tez
              ~loc:__LOC__
              endorser_frozen_balance
              accumulated_frozen_balance)
        real_endorsers)
    ranges

(** Checks the baking and endorsing rewards formulas against a precomputed
    table. *)
let test_rewards_formulas () =
  Context.init 1
  >>=? fun (b, _, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {endorsers_per_block; _}; _} ->
  let block_priorities = 0 -- 2 in
  let included_endorsements = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities included_endorsements in
  List.iter_ep
    (fun (priority, endorsing_power) ->
      Context.get_baking_reward (B b) ~priority ~endorsing_power
      >>=? fun reward ->
      let expected_reward =
        Test_tez.Tez.of_mutez_exn
          (Int64.of_int Rewards.baking_rewards.(priority).(endorsing_power))
      in
      Assert.equal_tez ~loc:__LOC__ reward expected_reward
      >>=? fun () ->
      Context.get_endorsing_reward (B b) ~priority ~endorsing_power
      >>=? fun reward ->
      let expected_reward =
        Test_tez.Tez.of_mutez_exn
          (Int64.of_int Rewards.endorsing_rewards.(priority).(endorsing_power))
      in
      Assert.equal_tez ~loc:__LOC__ reward expected_reward
      >>=? fun () -> return_unit)
    ranges

let wrap e = Lwt.return (Environment.wrap_tzresult e)

(** Check that the rewards formulas from Context are equivalent with
    the ones from Baking. *)
let test_rewards_formulas_equivalence () =
  Context.init 1
  >>=? fun (b, _, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {endorsers_per_block; _}; _} ->
  Alpha_context.prepare
    b.context
    ~level:b.header.shell.level
    ~predecessor_timestamp:b.header.shell.timestamp
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness
  >>= wrap
  >>=? fun (ctxt, _) ->
  let block_priorities = 0 -- 64 in
  let endorsing_power = 0 -- endorsers_per_block in
  let ranges = List.product block_priorities endorsing_power in
  List.iter_ep
    (fun (block_priority, endorsing_power) ->
      Baking.baking_reward
        ctxt
        ~block_priority
        ~included_endorsements:endorsing_power
      |> wrap
      >>=? fun reward1 ->
      Context.get_baking_reward (B b) ~priority:block_priority ~endorsing_power
      >>=? fun reward2 ->
      Assert.equal_tez ~loc:__LOC__ reward1 reward2
      >>=? fun () ->
      Baking.endorsing_reward ctxt ~block_priority endorsing_power
      |> wrap
      >>=? fun reward1 ->
      Context.get_endorsing_reward
        (B b)
        ~priority:block_priority
        ~endorsing_power
      >>=? fun reward2 -> Assert.equal_tez ~loc:__LOC__ reward1 reward2)
    ranges

(** Test baking [n] cycles in a raw works smoothly. *)
let test_bake_n_cycles n () =
  let open Block in
  let policy = By_priority 0 in
  Context.init 1
  >>=? fun (block, _contracts, _) ->
  Block.bake_until_n_cycle_end ~policy n block >>=? fun _block -> return ()

(** Check the voting power is constant between cycles when number of
    rolls are constant and in presence of one account. *)
let test_voting_power_cache () =
  let open Block in
  let policy = By_priority 0 in
  Context.init 1
  >>=? fun (block, _contracts, _) ->
  Context.get_bakers (B block)
  >>=? fun bakers ->
  let baker = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bakers in
  let assert_voting_power n block =
    let ctxt = Context.B block in
    Context.get_voting_power ctxt baker
    >>=? fun voting_power ->
    Assert.equal_int ~loc:__LOC__ n (Int32.to_int voting_power)
  in
  assert_voting_power 500 block
  >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 2 block
  >>=? fun block ->
  assert_voting_power 500 block
  >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 5 block
  >>=? fun block ->
  assert_voting_power 500 block
  >>=? fun () ->
  Block.bake_until_n_cycle_end ~policy 1 block
  >>=? fun block -> assert_voting_power 500 block

(* Test valid baker registration without credit *)
let test_valid_baker_registration_without_credit () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  Context.Contract.find_account (B b) bootstrap
  >>=? fun account ->
  Incremental.begin_construction b
  >>=? fun i ->
  let consensus_key_acc = Account.new_account () in
  Op.baker_registration
    ~consensus_key:consensus_key_acc.pk
    ~threshold:1
    ~owner_keys:[account.pk]
    ~credit:Tez.zero
    (B b)
    bootstrap
  >>=? fun (registration, new_baker) ->
  Incremental.add_operation i registration
  >>=? fun i ->
  (* check delegation *)
  Context.Contract.delegate (I i) (Contract.baker_contract new_baker)
  >>=? fun contract_delegate ->
  Assert.equal_baker ~loc:__LOC__ contract_delegate new_baker

(** Test valid baker registration:
    - create implicit contract without delegate
    - register baker with some ꜩ + verification of balance
    - delegate implicit contract with baker as delegate + verification of delegation *)
let test_valid_baker_registration_credit amount () =
  (* create an implicit contract with no delegate *)
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  Context.Contract.find_account (I i) bootstrap
  >>=? fun account ->
  (* check no delegate for delegator contract *)
  Context.Contract.delegate (I i) bootstrap
  >>= fun err ->
  Assert.error ~loc:__LOC__ err (function
      | RPC_context.Not_found _ ->
          true
      | _ ->
          false)
  >>=? fun _ ->
  (* baker registration with credit > 0ꜩ + check balance *)
  let consensus_key_acc = Account.new_account () in
  Op.baker_registration
    ~consensus_key:consensus_key_acc.pk
    ~threshold:1
    ~owner_keys:[account.pk]
    ~credit:amount
    (I i)
    bootstrap
  >>=? fun (registration, new_baker) ->
  let baker_contract = Contract.baker_contract new_baker in
  Incremental.add_operation i registration
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) baker_contract amount
  >>=? fun _ ->
  Context.Contract.delegate (I i) baker_contract
  >>=? fun delegate ->
  Assert.equal_baker ~loc:__LOC__ delegate new_baker
  >>=? fun _ ->
  (* delegation to the newly registered key *)
  Op.delegation (I i) bootstrap (Some new_baker)
  >>=? fun delegation ->
  Incremental.add_operation i delegation
  >>=? fun i ->
  (* check delegation *)
  Context.Contract.delegate (I i) bootstrap
  >>=? fun contract_delegate ->
  Assert.equal_baker ~loc:__LOC__ contract_delegate new_baker

(** Test that an empty baker account is not deleted *)
let test_valid_baker_registration_credit_debit amount () =
  (* create an implicit contract with no delegate *)
  Context.init 1
  >>=? fun (b, bootstrap_contracts, _) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  Context.Contract.find_account (I i) bootstrap
  >>=? fun account ->
  (* baker registration with credit > 0ꜩ + check balance *)
  let consensus_key_acc = Account.new_account () in
  Op.baker_registration
    ~consensus_key:consensus_key_acc.pk
    ~threshold:1
    ~owner_keys:[account.pk]
    ~credit:amount
    (I i)
    bootstrap
  >>=? fun (registration, new_baker) ->
  let baker_contract = Contract.baker_contract new_baker in
  Incremental.add_operation i registration
  >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) baker_contract amount
  >>=? fun _ ->
  (* Empty baker contracts are kept. We empty the contract in
     order to verify this. *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Transfer_to_implicit (amount, account.pkh))
    bootstrap
    new_baker
  >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract
  >>=? fun i ->
  (* baker_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (I i) baker_contract Tez.zero
  >>=? fun _ ->
  (* verify self-delegation after contract is emptied *)
  Context.Contract.delegate (I i) baker_contract
  >>=? fun delegate -> Assert.equal_baker ~loc:__LOC__ new_baker delegate

(** Test that an activation on active baker should raise an `Active_baker`
    error *)
let test_active_baker_activation () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  (* activation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active true)
    bootstrap
    baker
  >>=? fun activation ->
  Incremental.add_operation i activation
  >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Baker_storage.Already_active hash when Baker_hash.equal hash baker ->
          true
      | _ ->
          false)

(** Test that a deactivation on active baker should deactivate it *)
let test_active_baker_deactivation () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  (* deactivation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active false)
    bootstrap
    baker
  >>=? fun activation ->
  Incremental.add_operation i activation
  >>=? fun i ->
  Context.Baker.info (I i) baker
  >>=? fun {deactivated; _} ->
  assert deactivated ;
  return_unit

(** Test that an activation on inactive baker should activate it *)
let test_inactive_baker_activation () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  (* deactivation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active false)
    bootstrap
    baker
  >>=? fun activation ->
  Incremental.add_operation i activation
  >>=? fun i ->
  Context.Baker.info (I i) baker
  >>=? fun {deactivated; _} ->
  assert deactivated ;
  (* activation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active true)
    bootstrap
    baker
  >>=? fun activation ->
  Incremental.add_operation i activation
  >>=? fun i ->
  Context.Baker.info (I i) baker
  >>=? fun {deactivated; _} ->
  assert (not deactivated) ;
  return_unit

(** Test that a deactivation on inactive baker should raise an `Inactive_baker`
    error *)
let test_inactive_baker_deactivation () =
  Context.init 1
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  Incremental.begin_construction b
  >>=? fun i ->
  let bootstrap =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let baker =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  (* deactivation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active false)
    bootstrap
    baker
  >>=? fun activation ->
  Incremental.add_operation i activation
  >>=? fun i ->
  Context.Baker.info (I i) baker
  >>=? fun {deactivated; _} ->
  assert deactivated ;
  (* second deactivation *)
  Op.baker_action
    (I i)
    ~action:(Client_proto_baker.Set_active false)
    bootstrap
    baker
  >>=? fun deactivation ->
  Incremental.add_operation i deactivation
  >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Baker_storage.Already_inactive hash when Baker_hash.equal hash baker ->
          true
      | _ ->
          false)

(** Test that when the owner key is changed, baker script can no longer be
    invoked with the previous outdated key.
*)
let test_change_baker_owner_key () =
  Context.init 2
  >>=? fun (b, bootstrap_contracts, bootstrap_bakers) ->
  let baker1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_bakers
  in
  let bootstrap1 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd bootstrap_contracts
  in
  let bootstrap2 =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth bootstrap_contracts 1
  in
  Context.Contract.find_account (B b) bootstrap1
  >>=? fun acc1 ->
  Context.Contract.find_account (B b) bootstrap2
  >>=? fun acc2 ->
  (* Change baker1 owner key to bootstrap2 public key *)
  let threshold = 1 in
  Op.baker_action
    (B b)
    ~action:(Client_proto_baker.Set_owner_keys (Z.of_int threshold, [acc2.pk]))
    bootstrap2
    baker1
  >>=? fun change_key ->
  Incremental.begin_construction b
  >>=? fun i ->
  Incremental.add_operation i change_key
  >>=? fun i ->
  Incremental.finalize_block i
  >>=? fun b ->
  let protocol =
    Protocol_hash.of_b58check_exn
      "ProtoALphaALphaALphaALphaALphaALphaALpha61322gcLUGH"
  in
  let contract = Contract.baker_contract baker1 in
  Op.get_baker_contract_info (B b) contract
  >>=? fun info ->
  Op.origination (B b) bootstrap2 ~script:Op.dummy_script
  >>=? fun (operation, originated) ->
  Block.bake ~operation b
  >>=? fun b ->
  (* Forge baker script calls for all possible actions *)
  let vote = Vote.Yay in
  let actions =
    Client_proto_baker.
      [ Transfer_to_implicit (Tez.one, acc2.pkh);
        Transfer_to_scripted
          {
            amount = Tez.one;
            destination = originated;
            entrypoint = "default";
            parameter =
              Micheline.strip_locations @@ Michelson_v1_helpers.d_unit ~loc:0;
            parameter_type =
              Micheline.strip_locations @@ Michelson_v1_helpers.t_unit ~loc:0;
          };
        Submit_proposals [protocol];
        Submit_ballot (protocol, vote);
        Set_active true;
        Set_active false;
        Set_consensus_key acc2.pk;
        Set_owner_keys (Z.one, [acc2.pk]);
        Generic
          ( Micheline.strip_locations
          @@ Michelson_v1_helpers.generic_baker_noop ~loc:0 ) ]
  in
  List.iter_es
    (fun action ->
      let payload =
        Client_proto_baker.mk_payload ~stored_counter:info.counter ~action
      in
      (* Make parameters bytes, used to create a signature *)
      let bytes =
        Client_proto_baker.mk_bytes_to_sign
          ~chain_id:Chain_id.zero
          ~payload
          contract
      in
      (* Sign the parameter bytes with the outdated key *)
      let signature = Signature.sign acc1.sk bytes in
      (* Turn action into transaction parameters *)
      let parameters =
        Client_proto_baker.mk_singlesig_script_param ~payload ~signature
        |> Script.lazy_expr
      in
      let top =
        Transaction
          {
            amount = Tez.zero;
            parameters;
            destination = contract;
            entrypoint = Client_proto_baker.generic_entrypoint;
          }
      in
      Op.manager_operation ~source:bootstrap1 (B b) top
      >>=? fun sop ->
      let invalid_transfer = Op.sign acc1.sk (B b) sop in
      Incremental.add_operation i invalid_transfer
      >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Script_interpreter.Reject _ ->
              true
          | _ ->
              false))
    actions

let tests =
  [ Test_services.tztest "cycle" `Quick test_cycle;
    Test_services.tztest
      "test rewards are correctly accounted for"
      `Slow
      test_rewards_retrieval;
    Test_services.tztest
      "test rewards formula for various input values"
      `Quick
      test_rewards_formulas;
    Test_services.tztest
      "check equivalence of rewards formulas"
      `Quick
      test_rewards_formulas_equivalence;
    Test_services.tztest
      "test_bake_n_cycles for 12 cycles"
      `Quick
      (test_bake_n_cycles 12);
    Test_services.tztest "voting_power" `Quick test_voting_power_cache;
    Test_services.tztest
      "valid baker registration: no credit"
      `Quick
      test_valid_baker_registration_without_credit;
    Test_services.tztest
      "valid baker registration: credit 1μꜩ"
      `Quick
      (test_valid_baker_registration_credit Tez.one_mutez);
    Test_services.tztest
      "valid baker registration: credit 1μꜩn, debit 1μꜩ"
      `Quick
      (test_valid_baker_registration_credit_debit Tez.one_mutez);
    Test_services.tztest
      "active baker activation"
      `Quick
      test_active_baker_activation;
    Test_services.tztest
      "active baker deactivation"
      `Quick
      test_active_baker_deactivation;
    Test_services.tztest
      "inactive baker activation"
      `Quick
      test_inactive_baker_activation;
    Test_services.tztest
      "inactive baker deactivation"
      `Quick
      test_inactive_baker_deactivation;
    Test_services.tztest
      "change baker owner key"
      `Quick
      test_change_baker_owner_key ]
