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
    Component:  Protocol (delegate_storage)
    Invocation: dune exec src/proto_023_PtSeouLo/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_consensus_key.ml
    Subject:    consistency of the [Drain_delegate] operation
 *)

open Protocol
open Alpha_context

let constants =
  {
    Default_parameters.constants_test with
    issuance_weights =
      {
        Default_parameters.constants_test.issuance_weights with
        base_total_issued_per_minute = Tez.zero;
      };
    consensus_threshold_size = 0;
    origination_size = 0;
  }

(** Checks that staking balance is sum of delegators' stake. *)
let check_delegate_staking_invariant blk delegate_pkh =
  let open Lwt_result_wrap_syntax in
  let* delegate_staking_balance =
    Context.Delegate.staking_balance (B blk) delegate_pkh
  in
  let* self_staking_balance =
    Context.Delegate.full_balance (B blk) delegate_pkh
  in
  let* delegate_info = Context.Delegate.info (B blk) delegate_pkh in
  let delegate_contract = Contract.Implicit delegate_pkh in
  let delegated_contracts =
    List.filter
      (fun c -> Contract.(c <> delegate_contract))
      delegate_info.delegated_contracts
  in
  let* delegators_stake =
    List.fold_left_es
      (fun total pkh ->
        let* staking_balance =
          Context.Contract.balance_and_frozen_bonds (B blk) pkh
        in
        let*?@ t = Tez.(total +? staking_balance) in
        return t)
      self_staking_balance
      delegated_contracts
  in
  Assert.equal_tez ~loc:__LOC__ delegate_staking_balance delegators_stake

let update_consensus_key blk delegate public_key =
  let open Lwt_result_syntax in
  let nb_delay_cycles = constants.consensus_rights_delay + 1 in
  let* update_ck =
    Op.update_consensus_key (B blk) (Contract.Implicit delegate) public_key
  in
  let* blk' = Block.bake ~operation:update_ck blk in
  Block.bake_until_n_cycle_end nb_delay_cycles blk'

let delegate_stake blk source delegate =
  let open Lwt_result_syntax in
  let* delegation =
    Op.delegation (B blk) (Contract.Implicit source) (Some delegate)
  in
  Block.bake ~operation:delegation blk

let transfer_tokens blk source destination amount =
  let open Lwt_result_syntax in
  let* transfer_op =
    Op.transaction
      (B blk)
      (Contract.Implicit source)
      (Contract.Implicit destination)
      amount
  in
  Block.bake ~operation:transfer_op blk

let may_reveal_manager_key blk (pkh, pk) =
  let open Lwt_result_syntax in
  let* is_revealed =
    Context.Contract.is_manager_key_revealed (B blk) (Contract.Implicit pkh)
  in
  if is_revealed then return blk
  else
    let* reveal_op = Op.revelation (B blk) pk in
    Block.bake ~operation:reveal_op blk

let drain_delegate ~policy blk consensus_key delegate destination
    expected_final_balance =
  let open Lwt_result_syntax in
  let* drain_del =
    Op.drain_delegate (B blk) ~consensus_key ~delegate ~destination
  in
  let* blk' = Block.bake ~policy ~operation:drain_del blk in
  let* () = check_delegate_staking_invariant blk' delegate in
  let* final_balance =
    Context.Contract.balance (B blk') (Contract.Implicit delegate)
  in
  Assert.equal_tez ~loc:__LOC__ final_balance expected_final_balance

let get_first_2_accounts_contracts (a1, a2) =
  ((a1, Context.Contract.pkh a1), (a2, Context.Contract.pkh a2))

let test_drain_delegate_scenario f =
  let open Lwt_result_syntax in
  let* genesis, contracts = Context.init_with_constants2 constants in
  let (_contract1, account1_pkh), (_contract2, account2_pkh) =
    get_first_2_accounts_contracts contracts
  in
  let consensus_account = Account.new_account () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  let* blk' =
    transfer_tokens genesis account2_pkh consensus_pkh Tez.one_mutez
  in
  let* blk' = update_consensus_key blk' delegate consensus_pk in
  f blk' consensus_pkh consensus_pk delegate

let test_drain_delegate ~low_balance ~exclude_ck ~ck_delegates () =
  let open Lwt_result_syntax in
  test_drain_delegate_scenario (fun blk consensus_pkh consensus_pk delegate ->
      let policy =
        if exclude_ck then Block.Excluding [consensus_pkh]
        else Block.By_account delegate
      in
      let* blk =
        if ck_delegates then
          let* blk = may_reveal_manager_key blk (consensus_pkh, consensus_pk) in
          delegate_stake blk consensus_pkh delegate
        else return blk
      in
      let* delegate_balance =
        Context.Contract.balance (B blk) (Contract.Implicit delegate)
      in
      let* blk =
        if low_balance then
          let* blk =
            transfer_tokens blk delegate consensus_pkh delegate_balance
          in
          let* blk = may_reveal_manager_key blk (consensus_pkh, consensus_pk) in
          transfer_tokens
            blk
            consensus_pkh
            delegate
            Tez.(of_mutez_exn 1_000_000L)
        else return blk
      in
      let* delegate_balance =
        Context.Contract.balance (B blk) (Contract.Implicit delegate)
      in
      let expected_final_balance =
        if exclude_ck then Tez.zero
        else Tez.(max one) Tez_helpers.(delegate_balance /! 100L)
      in
      drain_delegate
        ~policy
        blk
        consensus_pkh
        delegate
        consensus_pkh
        expected_final_balance)

let test_drain_empty_delegate ~exclude_ck () =
  let open Lwt_result_syntax in
  test_drain_delegate_scenario (fun blk consensus_pkh _consensus_pk delegate ->
      let policy =
        if exclude_ck then Block.Excluding [consensus_pkh]
        else Block.By_account delegate
      in
      let* delegate_balance =
        Context.Contract.balance (B blk) (Contract.Implicit delegate)
      in
      let* blk = transfer_tokens blk delegate consensus_pkh delegate_balance in
      let*! res =
        drain_delegate ~policy blk consensus_pkh delegate consensus_pkh Tez.zero
      in
      Assert.proto_error_with_info
        ~loc:__LOC__
        res
        "Drain delegate without enough balance for allocation burn or drain \
         fees")

let test_tz4_consensus_key ~allow_tz4_delegate_enable () =
  let open Lwt_result_syntax in
  let* genesis, contract =
    Context.init_with_constants1 {constants with allow_tz4_delegate_enable}
  in
  let account1_pkh = Context.Contract.pkh contract in
  let consensus_account = Account.new_account ~algo:Bls () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  let* blk' =
    transfer_tokens genesis account1_pkh consensus_pkh Tez.one_mutez
  in
  let* operation =
    Op.update_consensus_key (B blk') (Contract.Implicit delegate) consensus_pk
  in
  let tz4_pk = match consensus_pk with Bls pk -> pk | _ -> assert false in
  let* inc = Incremental.begin_construction blk' in
  if allow_tz4_delegate_enable then
    let expect_failure =
      Error_helpers.expect_missing_bls_proof
        ~loc:__LOC__
        ~kind_pk:Consensus_pk
        ~pk:consensus_pk
        ~source_pkh:delegate
    in
    let* (_i : Incremental.t) =
      Incremental.validate_operation ~expect_failure inc operation
    in
    let proof_signer = Account.new_account ~algo:Bls () in
    let* operation_with_incorrect_proof =
      Op.update_consensus_key
        ~proof_signer:(Contract.Implicit proof_signer.pkh)
        (B blk')
        (Contract.Implicit delegate)
        consensus_pk
    in
    let expect_apply_failure =
      Error_helpers.expect_incorrect_bls_proof
        ~loc:__LOC__
        ~kind_pk:Consensus_pk
        ~pk:consensus_pk
    in
    let* (_i : Incremental.t) =
      Incremental.add_operation
        ~expect_apply_failure
        inc
        operation_with_incorrect_proof
    in
    (* update_consensus_key with incorrect ciphersuite *)
    let proof_incorrect_ciphersuite =
      match (consensus_account.sk, consensus_account.pk) with
      | Bls sk, Bls pk ->
          let signature =
            Signature.Bls.sign sk (Bls12_381_signature.MinPk.pk_to_bytes pk)
          in
          Some signature
      | _ -> None
    in
    let* operation_with_incorrect_ciphersuite =
      Op.update_consensus_key
        ~forge_proof:proof_incorrect_ciphersuite
        (B blk')
        (Contract.Implicit delegate)
        consensus_pk
    in
    let expect_apply_failure =
      Error_helpers.expect_incorrect_bls_proof
        ~loc:__LOC__
        ~kind_pk:Consensus_pk
        ~pk:consensus_pk
    in
    let* (_i : Incremental.t) =
      Incremental.add_operation
        ~expect_apply_failure
        inc
        operation_with_incorrect_ciphersuite
    in

    (* update_consensus_key with correct proof *)
    let* operation_with_correct_proof =
      Op.update_consensus_key
        ~proof_signer:(Contract.Implicit consensus_account.pkh)
        (B blk')
        (Contract.Implicit delegate)
        consensus_pk
    in
    let* (_i : Incremental.t) =
      Incremental.add_operation inc operation_with_correct_proof
    in
    return_unit
  else
    let expect_failure = function
      | [
          Environment.Ecoproto_error
            (Delegate_consensus_key.Invalid_consensus_key_update_tz4 (pk, kind));
        ]
        when Signature.Bls.Public_key.(pk = tz4_pk) && kind = Consensus ->
          return_unit
      | err ->
          failwith
            "Error trace:@,\
            \ %a does not match the \
             [Delegate_consensus_key.Invalid_consensus_key_update_tz4] error"
            Error_monad.pp_print_trace
            err
    in
    let* inc = Incremental.begin_construction blk' in
    let* (_i : Incremental.t) =
      Incremental.validate_operation ~expect_failure inc operation
    in
    return_unit

let test_consensus_key_with_unused_proof () =
  let open Lwt_result_syntax in
  let* genesis, contract = Context.init_with_constants1 constants in
  let account1_pkh = Context.Contract.pkh contract in
  let consensus_account = Account.new_account () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  let* blk' =
    transfer_tokens genesis account1_pkh consensus_pkh Tez.one_mutez
  in
  let proof_signer = Account.new_account ~algo:Bls () in
  let* operation =
    Op.update_consensus_key
      ~proof_signer:(Contract.Implicit proof_signer.pkh)
      (B blk')
      (Contract.Implicit delegate)
      consensus_pk
  in
  let* inc = Incremental.begin_construction blk' in
  let expect_failure =
    Error_helpers.expect_unused_bls_proof ~loc:__LOC__ ~kind_pk:Consensus_pk
  in
  let* (_i : Incremental.t) =
    Incremental.validate_operation ~expect_failure inc operation
  in
  return_unit

let test_attestation_with_consensus_key () =
  let open Lwt_result_wrap_syntax in
  let* genesis, contract = Context.init_with_constants1 constants in
  let account1_pkh = Context.Contract.pkh contract in
  let consensus_account = Account.new_account () in
  let delegate = account1_pkh in
  let consensus_pk = consensus_account.pk in
  let consensus_pkh = consensus_account.pkh in
  let* blk' =
    transfer_tokens genesis account1_pkh consensus_pkh Tez.one_mutez
  in
  let* b_pre = update_consensus_key blk' delegate consensus_pk in
  let* b = Block.bake b_pre in
  (* There is only one delegate, so its minimal slot is zero. *)
  let*?@ slot = Slot.of_int 0 in
  let* attestation =
    Op.attestation ~attesting_slot:{slot; consensus_pkh = account1_pkh} b
  in
  let*! res = Block.bake ~operation:attestation b in
  let* () =
    Assert.proto_error ~loc:__LOC__ res (function
      | Operation.Invalid_signature -> true
      | _ -> false)
  in
  let* attestation = Op.attestation ~attesting_slot:{slot; consensus_pkh} b in
  let* (_good_block : Block.t) = Block.bake ~operation:attestation b in
  return_unit

let tests =
  Tztest.
    [
      tztest
        "drain delegate high balance, excluding ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:true
           ~ck_delegates:true);
      tztest
        "drain delegate high balance, excluding ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:true
           ~ck_delegates:false);
      tztest
        "drain delegate high balance, with ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:false
           ~ck_delegates:true);
      tztest
        "drain delegate high balance, with ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:false
           ~exclude_ck:false
           ~ck_delegates:false);
      tztest
        "drain delegate low balance, excluding ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:true
           ~ck_delegates:true);
      tztest
        "drain delegate low balance, excluding ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:true
           ~ck_delegates:false);
      tztest
        "drain delegate low balance, with ck, ck delegates"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:false
           ~ck_delegates:true);
      tztest
        "drain delegate low balance, with ck, ck does not delegate"
        `Quick
        (test_drain_delegate
           ~low_balance:true
           ~exclude_ck:false
           ~ck_delegates:false);
      tztest
        "empty drain delegate excluding ck"
        `Quick
        (test_drain_empty_delegate ~exclude_ck:true);
      tztest
        "empty drain delegate with ck"
        `Quick
        (test_drain_empty_delegate ~exclude_ck:false);
      tztest
        "tz4 consensus key (allow_tz4_delegate_enable:false)"
        `Quick
        (test_tz4_consensus_key ~allow_tz4_delegate_enable:false);
      tztest
        "tz4 consensus key (allow_tz4_delegate_enable:true)"
        `Quick
        (test_tz4_consensus_key ~allow_tz4_delegate_enable:true);
      tztest
        "consensus key update with unused proof"
        `Quick
        test_consensus_key_with_unused_proof;
      tztest "attestation with ck" `Quick test_attestation_with_consensus_key;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("consensus key", tests)]
  |> Lwt_main.run
