(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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
    Component:  Protocol (token)
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/main.exe
    Subject:    Token movements in the protocol.
*)

open Protocol
open Alpha_context
open Test_tez

let wrap e = e >|= Environment.wrap_tzresult

(** Creates a context with a single account. Returns the context and the public
    key hash of the account. *)
let create_context () =
  let (Account.{pkh; _} as account) = Account.new_account () in
  let bootstrap_account = Account.make_bootstrap_account account in
  Block.alpha_context [bootstrap_account] >>=? fun ctxt -> return (ctxt, pkh)

let random_amount () =
  match Tez.of_mutez (Int64.add 1L (Random.int64 100L)) with
  | None -> assert false
  | Some x -> x

let nonce = Origination_nonce.Internal_for_tests.initial Operation_hash.zero

let mk_rollup () = Tx_rollup.Internal_for_tests.originated_tx_rollup nonce

(** Check balances for a simple transfer from [bootstrap] to new [Implicit]. *)
let test_simple_balances () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  let src = `Contract (Contract.Implicit pkh) in
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let dest = `Contract (Contract.Implicit pkh) in
  let amount = Tez.one in
  wrap (Token.transfer ctxt src dest amount) >>=? fun (ctxt', _) ->
  wrap (Token.balance ctxt src) >>=? fun (ctxt, bal_src) ->
  wrap (Token.balance ctxt' src) >>=? fun (ctxt', bal_src') ->
  wrap (Token.balance ctxt dest) >>=? fun (_, bal_dest) ->
  wrap (Token.balance ctxt' dest) >>=? fun (_, bal_dest') ->
  bal_src' +? amount >>?= fun add_bal_src'_amount ->
  bal_dest +? amount >>?= fun add_bal_dest_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_src add_bal_src'_amount >>=? fun () ->
  Assert.equal_tez ~loc:__LOC__ bal_dest' add_bal_dest_amount

(** Check balance updates for a simple transfer from [bootstrap] to new
    [Implicit]. *)
let test_simple_balance_updates () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  let src = Contract.Implicit pkh in
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let dest = Contract.Implicit pkh in
  let amount = Tez.one in
  wrap (Token.transfer ctxt (`Contract src) (`Contract dest) amount)
  >>=? fun (_, bal_updates) ->
  Alcotest.(
    check
      bool
      "Missing balance update for source contract."
      (List.mem
         ~equal:( = )
         Receipt.(Contract src, Debited amount, Block_application)
         bal_updates)
      true) ;
  Alcotest.(
    check
      bool
      "Missing balance update for destination contract."
      (List.mem
         ~equal:( = )
         Receipt.(Contract dest, Credited amount, Block_application)
         bal_updates)
      true) ;
  return_unit

let test_allocated_and_deallocated ctxt dest initial_status status_when_empty =
  wrap (Token.allocated ctxt dest) >>=? fun (ctxt, allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated initial_status >>=? fun () ->
  let amount = Tez.one in
  wrap (Token.transfer ctxt `Minted dest amount) >>=? fun (ctxt', _) ->
  wrap (Token.allocated ctxt' dest) >>=? fun (ctxt', allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated true >>=? fun () ->
  wrap (Token.balance ctxt' dest) >>=? fun (ctxt', bal_dest') ->
  wrap (Token.transfer ctxt' dest `Burned bal_dest') >>=? fun (ctxt', _) ->
  wrap (Token.allocated ctxt' dest) >>=? fun (_, allocated) ->
  Assert.equal_bool ~loc:__LOC__ allocated status_when_empty >>=? fun () ->
  return_unit

let test_allocated_and_deallocated_when_empty ctxt dest =
  test_allocated_and_deallocated ctxt dest false false

let test_allocated_and_still_allocated_when_empty ctxt dest initial_status =
  test_allocated_and_deallocated ctxt dest initial_status true

let test_allocated () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  let dest = `Delegate_balance pkh in
  test_allocated_and_still_allocated_when_empty ctxt dest true >>=? fun () ->
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let dest = `Contract (Contract.Implicit pkh) in
  test_allocated_and_deallocated_when_empty ctxt dest >>=? fun () ->
  let dest = `Collected_commitments Blinded_public_key_hash.zero in
  test_allocated_and_deallocated_when_empty ctxt dest >>=? fun () ->
  let dest = `Frozen_deposits pkh in
  test_allocated_and_still_allocated_when_empty ctxt dest false >>=? fun () ->
  let dest = `Block_fees in
  test_allocated_and_still_allocated_when_empty ctxt dest true >>=? fun () ->
  let dest =
    let bond_id = Bond_id.Tx_rollup_bond_id (mk_rollup ()) in
    `Frozen_bonds (Contract.Implicit pkh, bond_id)
  in
  test_allocated_and_deallocated_when_empty ctxt dest

let check_sink_balances ctxt ctxt' dest amount =
  wrap (Token.balance ctxt dest) >>=? fun (_, bal_dest) ->
  wrap (Token.balance ctxt' dest) >>=? fun (_, bal_dest') ->
  bal_dest +? amount >>?= fun add_bal_dest_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_dest' add_bal_dest_amount

(* Accounts of the form (`DelegateBalance pkh) are not allocated when they
   receive funds for the first time. To force allocation, we transfer to
   (`Contract pkh) instead. *)
let force_allocation_if_need_be ctxt account =
  match account with
  | `Delegate_balance pkh ->
      let account = `Contract (Contract.Implicit pkh) in
      wrap (Token.transfer ctxt `Minted account Tez.one_mutez) >|=? fst
  | _ -> return ctxt

let test_transferring_to_sink ctxt sink amount expected_bupds =
  (* Transferring zero must be a noop, and must not return balance updates. *)
  wrap (Token.transfer ctxt `Minted sink Tez.zero) >>=? fun (ctxt', bupds) ->
  Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true
  >>=? fun () ->
  (* Force the allocation of [dest] if need be. *)
  force_allocation_if_need_be ctxt sink >>=? fun ctxt ->
  (* Test transferring a non null amount. *)
  wrap (Token.transfer ctxt `Minted sink amount) >>=? fun (ctxt', bupds) ->
  check_sink_balances ctxt ctxt' sink amount >>=? fun () ->
  let expected_bupds =
    Receipt.(Minted, Debited amount, Block_application) :: expected_bupds
  in
  Alcotest.(
    check bool "Balance updates do not match." (bupds = expected_bupds) true) ;
  (* Test transferring to go beyond capacity. *)
  wrap (Token.balance ctxt' sink) >>=? fun (ctxt', bal) ->
  let amount = Tez.of_mutez_exn Int64.max_int -! bal +! Tez.one_mutez in
  wrap (Token.transfer ctxt' `Minted sink amount) >>= fun res ->
  Assert.proto_error_with_info ~loc:__LOC__ res "Overflowing tez addition"

let test_transferring_to_contract ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let dest = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_to_sink
    ctxt
    (`Contract dest)
    amount
    [(Contract dest, Credited amount, Block_application)]

let test_transferring_to_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_to_sink
    ctxt
    (`Collected_commitments bpkh)
    amount
    [(Commitments bpkh, Credited amount, Block_application)]

let test_transferring_to_delegate_balance ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let dest = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_to_sink
    ctxt
    (`Delegate_balance pkh)
    amount
    [(Contract dest, Credited amount, Block_application)]

let test_transferring_to_frozen_deposits ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let amount = random_amount () in
  test_transferring_to_sink
    ctxt
    (`Frozen_deposits pkh)
    amount
    [(Deposits pkh, Credited amount, Block_application)]

let test_transferring_to_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_to_sink
    ctxt
    `Block_fees
    amount
    [(Block_fees, Credited amount, Block_application)]

let test_transferring_to_burned ctxt =
  let amount = random_amount () in
  let minted_bupd = Receipt.(Minted, Debited amount, Block_application) in
  wrap (Token.transfer ctxt `Minted `Burned amount) >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds = [minted_bupd; (Burned, Credited amount, Block_application)])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Storage_fees amount) >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds = [minted_bupd; (Storage_fees, Credited amount, Block_application)])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Double_signing_punishments amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Double_signing_punishments, Credited amount, Block_application);
      ])
    true
  >>=? fun () ->
  let pkh = Tezos_crypto.Signature.Public_key_hash.zero in
  let p, r = (Random.bool (), Random.bool ()) in
  wrap
    (Token.transfer ctxt `Minted (`Lost_endorsing_rewards (pkh, p, r)) amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Lost_endorsing_rewards (pkh, p, r), Credited amount, Block_application);
      ])
    true
  >>=? fun () ->
  wrap (Token.transfer ctxt `Minted `Sc_rollup_refutation_punishments amount)
  >>=? fun (_, bupds) ->
  Assert.equal_bool
    ~loc:__LOC__
    (bupds
    = [
        minted_bupd;
        (Sc_rollup_refutation_punishments, Credited amount, Block_application);
      ])
    true

let test_transferring_to_frozen_bonds ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let contract = Contract.Implicit pkh in
  let tx_rollup = mk_rollup () in
  let bond_id = Bond_id.Tx_rollup_bond_id tx_rollup in
  let amount = random_amount () in
  test_transferring_to_sink
    ctxt
    (`Frozen_bonds (contract, bond_id))
    amount
    [(Frozen_bonds (contract, bond_id), Credited amount, Block_application)]

let test_transferring_to_sink () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, _) ->
  test_transferring_to_contract ctxt >>=? fun () ->
  test_transferring_to_collected_commitments ctxt >>=? fun () ->
  test_transferring_to_delegate_balance ctxt >>=? fun () ->
  test_transferring_to_frozen_deposits ctxt >>=? fun () ->
  test_transferring_to_collected_fees ctxt >>=? fun () ->
  test_transferring_to_burned ctxt >>=? fun () ->
  test_transferring_to_frozen_bonds ctxt

let check_src_balances ctxt ctxt' src amount =
  wrap (Token.balance ctxt src) >>=? fun (_, bal_src) ->
  wrap (Token.balance ctxt' src) >>=? fun (_, bal_src') ->
  bal_src' +? amount >>?= fun add_bal_src'_amount ->
  Assert.equal_tez ~loc:__LOC__ bal_src add_bal_src'_amount

let test_transferring_from_unbounded_source ctxt src expected_bupds =
  (* Transferring zero must not return balance updates. *)
  wrap (Token.transfer ctxt src `Burned Tez.zero) >>=? fun (_, bupds) ->
  Assert.equal_bool ~loc:__LOC__ (bupds = []) true >>=? fun () ->
  (* Test transferring a non null amount. *)
  let amount = random_amount () in
  wrap (Token.transfer ctxt src `Burned amount) >>=? fun (_, bupds) ->
  let expected_bupds =
    expected_bupds amount
    @ Receipt.[(Burned, Credited amount, Block_application)]
  in
  Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true >>=? fun () ->
  return_unit

(* Returns the balance of [account] if [account] is allocated, and returns
   [Tez.zero] otherwise. *)
let balance_no_fail ctxt account =
  wrap (Token.allocated ctxt account) >>=? fun (ctxt, allocated) ->
  if allocated then wrap (Token.balance ctxt account)
  else return (ctxt, Tez.zero)

let test_transferring_from_bounded_source ctxt src amount expected_bupds =
  balance_no_fail ctxt src >>=? fun (ctxt, balance) ->
  Assert.equal_tez ~loc:__LOC__ balance Tez.zero >>=? fun () ->
  (* Test transferring from an empty account. *)
  wrap (Token.transfer ctxt src `Burned Tez.one) >>= fun res ->
  let error_title =
    match src with
    | `Contract _ -> "Balance too low"
    | `Delegate_balance _ | `Frozen_deposits _ | `Frozen_bonds _ ->
        "Storage error (fatal internal error)"
    | _ -> "Underflowing tez subtraction"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title >>=? fun () ->
  (* Transferring zero must be a noop, and must not return balance updates. *)
  wrap (Token.transfer ctxt src `Burned Tez.zero) >>=? fun (ctxt', bupds) ->
  Assert.equal_bool ~loc:__LOC__ (ctxt == ctxt' && bupds = []) true
  >>=? fun () ->
  (* Force the allocation of [dest] if need be. *)
  force_allocation_if_need_be ctxt src >>=? fun ctxt ->
  (* Test transferring everything. *)
  wrap (Token.transfer ctxt `Minted src amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt src `Burned amount) >>=? fun (ctxt', bupds) ->
  check_src_balances ctxt ctxt' src amount >>=? fun () ->
  let expected_bupds =
    expected_bupds @ Receipt.[(Burned, Credited amount, Block_application)]
  in
  Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true >>=? fun () ->
  (* Test transferring a smaller amount. *)
  wrap (Token.transfer ctxt `Minted src amount) >>=? fun (ctxt, _) ->
  (match src with
  | `Frozen_bonds _ ->
      wrap (Token.transfer ctxt src `Burned amount) >>= fun res ->
      let error_title = "Partial spending of frozen bonds" in
      Assert.proto_error_with_info ~loc:__LOC__ res error_title
  | _ ->
      wrap (Token.transfer ctxt src `Burned amount) >>=? fun (ctxt', bupds) ->
      check_src_balances ctxt ctxt' src amount >>=? fun () ->
      Assert.equal_bool ~loc:__LOC__ (bupds = expected_bupds) true)
  >>=? fun () ->
  (* Test transferring more than available. *)
  wrap (Token.balance ctxt src) >>=? fun (ctxt, balance) ->
  wrap (Token.transfer ctxt src `Burned (balance +! Tez.one)) >>= fun res ->
  let error_title =
    match src with
    | `Contract _ -> "Balance too low"
    | `Frozen_bonds _ -> "Partial spending of frozen bonds"
    | _ -> "Underflowing tez subtraction"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title

let test_transferring_from_contract ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let src = Contract.Implicit pkh in
  let amount = random_amount () in
  test_transferring_from_bounded_source
    ctxt
    (`Contract src)
    amount
    [(Contract src, Debited amount, Block_application)]

let test_transferring_from_collected_commitments ctxt =
  let amount = random_amount () in
  let bpkh = Blinded_public_key_hash.zero in
  test_transferring_from_bounded_source
    ctxt
    (`Collected_commitments bpkh)
    amount
    [(Commitments bpkh, Debited amount, Block_application)]

let test_transferring_from_delegate_balance ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let amount = random_amount () in
  let src = Contract.Implicit pkh in
  test_transferring_from_bounded_source
    ctxt
    (`Delegate_balance pkh)
    amount
    [(Contract src, Debited amount, Block_application)]

let test_transferring_from_frozen_deposits ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let amount = random_amount () in
  test_transferring_from_bounded_source
    ctxt
    (`Frozen_deposits pkh)
    amount
    [(Deposits pkh, Debited amount, Block_application)]

let test_transferring_from_collected_fees ctxt =
  let amount = random_amount () in
  test_transferring_from_bounded_source
    ctxt
    `Block_fees
    amount
    [(Block_fees, Debited amount, Block_application)]

let test_transferring_from_frozen_bonds ctxt =
  let pkh, _pk, _sk = Tezos_crypto.Signature.generate_key () in
  let contract = Contract.Implicit pkh in
  let tx_rollup = mk_rollup () in
  let bond_id = Bond_id.Tx_rollup_bond_id tx_rollup in
  let amount = random_amount () in
  test_transferring_from_bounded_source
    ctxt
    (`Frozen_bonds (contract, bond_id))
    amount
    [(Frozen_bonds (contract, bond_id), Debited amount, Block_application)]

let test_transferring_from_source () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, _) ->
  test_transferring_from_unbounded_source ctxt `Invoice (fun am ->
      [(Invoice, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Bootstrap (fun am ->
      [(Bootstrap, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Initial_commitments (fun am ->
      [(Initial_commitments, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Revelation_rewards (fun am ->
      [(Nonce_revelation_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source
    ctxt
    `Double_signing_evidence_rewards
    (fun am ->
      [(Double_signing_evidence_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Endorsing_rewards (fun am ->
      [(Endorsing_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Baking_rewards (fun am ->
      [(Baking_rewards, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Baking_bonuses (fun am ->
      [(Baking_bonuses, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source ctxt `Minted (fun am ->
      [(Minted, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_unbounded_source
    ctxt
    `Liquidity_baking_subsidies
    (fun am -> [(Liquidity_baking_subsidies, Debited am, Block_application)])
  >>=? fun () ->
  test_transferring_from_contract ctxt >>=? fun () ->
  test_transferring_from_collected_commitments ctxt >>=? fun () ->
  test_transferring_from_delegate_balance ctxt >>=? fun () ->
  test_transferring_from_frozen_deposits ctxt >>=? fun () ->
  test_transferring_from_collected_fees ctxt >>=? fun () ->
  test_transferring_from_frozen_bonds ctxt

let cast_to_container_type x =
  match x with
  | `Burned | `Invoice | `Bootstrap | `Initial_commitments | `Minted
  | `Liquidity_baking_subsidies ->
      None
  | `Contract _ as x -> Some x
  | `Collected_commitments _ as x -> Some x
  | `Delegate_balance _ as x -> Some x
  | `Block_fees as x -> Some x
  | `Frozen_bonds _ as x -> Some x

(** Generates all combinations of constructors. *)
let build_test_cases () =
  create_context () >>=? fun (ctxt, pkh) ->
  let origin = `Contract (Contract.Implicit pkh) in
  let user1, _, _ = Tezos_crypto.Signature.generate_key () in
  let user1c = `Contract (Contract.Implicit user1) in
  let user2, _, _ = Tezos_crypto.Signature.generate_key () in
  let user2c = `Contract (Contract.Implicit user2) in
  let baker1, baker1_pk, _ = Tezos_crypto.Signature.generate_key () in
  let baker1c = `Contract (Contract.Implicit baker1) in
  let baker2, baker2_pk, _ = Tezos_crypto.Signature.generate_key () in
  let baker2c = `Contract (Contract.Implicit baker2) in
  (* Allocate contracts for user1, user2, baker1, and baker2. *)
  wrap (Token.transfer ctxt origin user1c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user2c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin baker1c (random_amount ()))
  >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin baker2c (random_amount ()))
  >>=? fun (ctxt, _) ->
  (* Configure baker1, and baker2 as delegates by self-delegation, for which
     revealing their manager key is a prerequisite. *)
  wrap (Contract.reveal_manager_key ctxt baker1 baker1_pk) >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit baker1) (Some baker1))
  >>=? fun ctxt ->
  wrap (Contract.reveal_manager_key ctxt baker2 baker2_pk) >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit baker2) (Some baker2))
  (* Let user1 delegate to baker2. *)
  >>=? fun ctxt ->
  wrap (Contract.Delegate.set ctxt (Contract.Implicit user1) (Some baker2))
  >>=? fun ctxt ->
  let tx_rollup1 = mk_rollup () in
  let bond_id1 = Bond_id.Tx_rollup_bond_id tx_rollup1 in
  let tx_rollup2 = mk_rollup () in
  let bond_id2 = Bond_id.Tx_rollup_bond_id tx_rollup2 in
  let user1ic = Contract.Implicit user1 in
  let baker2ic = Contract.Implicit baker2 in
  let src_list =
    [
      (`Invoice, random_amount ());
      (`Bootstrap, random_amount ());
      (`Initial_commitments, random_amount ());
      (`Minted, random_amount ());
      (`Liquidity_baking_subsidies, random_amount ());
      (`Collected_commitments Blinded_public_key_hash.zero, random_amount ());
      (`Delegate_balance baker1, random_amount ());
      (`Delegate_balance baker2, random_amount ());
      (`Block_fees, random_amount ());
      (user1c, random_amount ());
      (user2c, random_amount ());
      (baker1c, random_amount ());
      (baker2c, random_amount ());
      (`Frozen_bonds (user1ic, bond_id1), random_amount ());
      (`Frozen_bonds (baker2ic, bond_id2), random_amount ());
    ]
  in
  let dest_list =
    [
      `Collected_commitments Blinded_public_key_hash.zero;
      `Delegate_balance baker1;
      `Delegate_balance baker2;
      `Block_fees;
      user1c;
      user2c;
      baker1c;
      baker2c;
      `Frozen_bonds (user1ic, bond_id1);
      `Frozen_bonds (baker2ic, bond_id2);
      `Burned;
    ]
  in
  return (ctxt, List.product src_list dest_list)

let check_src_balances ctxt ctxt' src amount =
  match cast_to_container_type src with
  | None -> return_unit
  | Some src -> check_src_balances ctxt ctxt' src amount

let check_sink_balances ctxt ctxt' dest amount =
  match cast_to_container_type dest with
  | None -> return_unit
  | Some dest -> check_sink_balances ctxt ctxt' dest amount

let rec check_balances ctxt ctxt' src dest amount =
  match (cast_to_container_type src, cast_to_container_type dest) with
  | None, None -> return_unit
  | ( Some (`Delegate_balance d),
      Some (`Contract (Contract.Implicit c) as contract) )
  | ( Some (`Contract (Contract.Implicit c) as contract),
      Some (`Delegate_balance d) )
    when d = c ->
      (* src and dest are in fact referring to the same contract *)
      check_balances ctxt ctxt' contract contract amount
  | Some src, Some dest when src = dest ->
      (* src and dest are the same contract *)
      wrap (Token.balance ctxt dest) >>=? fun (_, bal_dest) ->
      wrap (Token.balance ctxt' dest) >>=? fun (_, bal_dest') ->
      Assert.equal_tez ~loc:__LOC__ bal_dest bal_dest'
  | Some src, None -> check_src_balances ctxt ctxt' src amount
  | None, Some dest -> check_sink_balances ctxt ctxt' dest amount
  | Some src, Some dest ->
      check_src_balances ctxt ctxt' src amount >>=? fun () ->
      check_sink_balances ctxt ctxt' dest amount

let test_all_combinations_of_sources_and_sinks () =
  Random.init 0 ;
  build_test_cases () >>=? fun (ctxt, cases) ->
  List.iter_es
    (fun ((src, amount), dest) ->
      (match cast_to_container_type src with
      | None -> return ctxt
      | Some src ->
          wrap (Token.transfer ctxt `Minted src amount) >>=? fun (ctxt, _) ->
          return ctxt)
      >>=? fun ctxt ->
      wrap (Token.transfer ctxt src dest amount) >>=? fun (ctxt', _) ->
      check_balances ctxt ctxt' src dest amount)
    cases

(** [coalesce (account, Credited am1, origin) (account, Credited am2, origin)
    = Some (account, Credited (am1+am2), origin)]

    [coalesce (account, Debited am1, origin) (account, Debited am2, origin)
    = Some (account, Debited (am1+am2), origin)]

    Fails if bu1 and bu2 have different accounts or different origins, or
    if one is a credit while the other is a debit. *)
let coalesce_balance_updates bu1 bu2 =
  match (bu1, bu2) with
  | (bu1_bal, bu1_balupd, bu1_origin), (bu2_bal, bu2_balupd, bu2_origin) -> (
      assert (bu1_bal = bu2_bal) ;
      assert (bu1_origin = bu2_origin) ;
      let open Receipt in
      match (bu1_balupd, bu2_balupd) with
      | Credited bu1_am, Credited bu2_am ->
          let bu_am =
            match bu1_am +? bu2_am with Ok am -> am | _ -> assert false
          in
          (bu1_bal, Credited bu_am, bu1_origin)
      | Debited bu1_am, Debited bu2_am ->
          let bu_am =
            match bu1_am +? bu2_am with Ok am -> am | _ -> assert false
          in
          (bu1_bal, Debited bu_am, bu1_origin)
      | Credited _, Debited _ | Debited _, Credited _ -> assert false)

(** Check that elt has the same balance in ctxt1 and ctxt2. *)
let check_balances_are_consistent ctxt1 ctxt2 elt =
  match elt with
  | #Token.container as elt ->
      Token.balance ctxt1 elt >>=? fun (_, elt_bal1) ->
      Token.balance ctxt2 elt >>=? fun (_, elt_bal2) ->
      assert (elt_bal1 = elt_bal2) ;
      return_unit
  | `Invoice | `Bootstrap | `Initial_commitments | `Minted
  | `Liquidity_baking_subsidies | `Burned ->
      return_unit

(** Test that [transfer_n] is equivalent to n debits followed by n credits. *)
let test_transfer_n ctxt src dest =
  (* Run transfer_n. *)
  Token.transfer_n ctxt src dest >>=? fun (ctxt1, bal_updates1) ->
  (* Debit all sources. *)
  List.fold_left_es
    (fun (ctxt, bal_updates) (src, am) ->
      Token.transfer ctxt src `Burned am >>=? fun (ctxt, debit_logs) ->
      return (ctxt, bal_updates @ debit_logs))
    (ctxt, [])
    src
  >>=? fun (ctxt, debit_logs) ->
  (* remove burning balance updates *)
  let debit_logs =
    List.filter
      (fun b -> match b with Receipt.Burned, _, _ -> false | _ -> true)
      debit_logs
  in
  (* Credit the sink for each source. *)
  List.fold_left_es
    (fun (ctxt, bal_updates) (_, am) ->
      Token.transfer ctxt `Minted dest am >>=? fun (ctxt, credit_logs) ->
      return (ctxt, bal_updates @ credit_logs))
    (ctxt, [])
    src
  >>=? fun (ctxt2, credit_logs) ->
  (* remove minting balance updates *)
  let credit_logs =
    List.filter
      (fun b -> match b with Receipt.Minted, _, _ -> false | _ -> true)
      credit_logs
  in
  (* Check equivalence of balance updates. *)
  let credit_logs =
    match credit_logs with
    | [] -> []
    | head :: tail -> [List.fold_left coalesce_balance_updates head tail]
  in
  assert (bal_updates1 = debit_logs @ credit_logs) ;
  (* Check balances are the same in ctxt1 and ctxt2. *)
  List.(iter_es (check_balances_are_consistent ctxt1 ctxt2) (map fst src))
  >>=? fun () -> check_balances_are_consistent ctxt1 ctxt2 dest

let test_transfer_n_with_empty_source () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  wrap (test_transfer_n ctxt [] `Block_fees) >>=? fun () ->
  let dest = `Delegate_balance pkh in
  wrap (test_transfer_n ctxt [] dest)

let test_transfer_n_with_non_empty_source () =
  Random.init 0 ;
  create_context () >>=? fun (ctxt, pkh) ->
  let origin = `Contract (Contract.Implicit pkh) in
  let user1, _, _ = Tezos_crypto.Signature.generate_key () in
  let user1c = `Contract (Contract.Implicit user1) in
  let user2, _, _ = Tezos_crypto.Signature.generate_key () in
  let user2c = `Contract (Contract.Implicit user2) in
  let user3, _, _ = Tezos_crypto.Signature.generate_key () in
  let user3c = `Contract (Contract.Implicit user3) in
  let user4, _, _ = Tezos_crypto.Signature.generate_key () in
  let user4c = `Contract (Contract.Implicit user4) in
  (* Allocate contracts for user1, user2, user3, and user4. *)
  let amount =
    match Tez.of_mutez 1000L with None -> assert false | Some x -> x
  in
  wrap (Token.transfer ctxt origin user1c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user2c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user3c amount) >>=? fun (ctxt, _) ->
  wrap (Token.transfer ctxt origin user4c (random_amount ()))
  >>=? fun (ctxt, _) ->
  let sources =
    [
      (user2c, random_amount ());
      (user3c, random_amount ());
      (user4c, random_amount ());
    ]
  in
  wrap (test_transfer_n ctxt sources user1c) >>=? fun () ->
  wrap (test_transfer_n ctxt ((user1c, random_amount ()) :: sources) user1c)

let tests =
  Tztest.
    [
      tztest "transfer - balances" `Quick test_simple_balances;
      tztest "transfer - balance updates" `Quick test_simple_balance_updates;
      tztest "transfer - test allocated" `Quick test_allocated;
      tztest "transfer - test transfer to sink" `Quick test_transferring_to_sink;
      tztest
        "transfer - test transfer from source"
        `Quick
        test_transferring_from_source;
      tztest
        "transfer - test all (sources x sinks)"
        `Quick
        test_all_combinations_of_sources_and_sinks;
      tztest
        "transfer - test from empty sources to a destination"
        `Quick
        test_transfer_n_with_empty_source;
      tztest
        "transfer - test from n sources to a destination"
        `Quick
        test_transfer_n_with_non_empty_source;
    ]
