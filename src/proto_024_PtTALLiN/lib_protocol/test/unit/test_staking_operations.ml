(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/unit/main.exe \
                  -- --file test_staking_operations.ml
    Subject:      test staking operations
*)

open Protocol
open Alpha_context

let register_test = Tezt_helpers.register_test_es ~__FILE__ ~file_tags:["stake"]

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

let originate_implicit_unrevealed_account b ?(amount = Tez_helpers.of_int 10)
    source =
  let open Lwt_result_syntax in
  let a = Account.new_account () in
  let c = Contract.Implicit a.pkh in
  let* operation = Op.transaction (B b) ~fee:Tez.zero source c amount in
  let* b = Block.bake b ~operation in
  return (b, c)

let bake_set_delegate_parameters_until_activation b ~delegate =
  let open Lwt_result_syntax in
  let init_params =
    Adaptive_issuance_helpers.
      {
        limit_of_staking_over_baking = Q.one;
        edge_of_baking_over_staking = Q.one;
      }
  in
  let* set_delegate_parameters =
    Adaptive_issuance_helpers.set_delegate_parameters
      (B b)
      delegate
      ~parameters:init_params
  in
  let* b = Block.bake ~operation:set_delegate_parameters b in
  let* b =
    Block.bake_until_n_cycle_end
      (constants.delegate_parameters_activation_delay + 1)
      b
  in
  return b

let create_delegate_and_staker ~self_delegate_staker ?amount () =
  let open Lwt_result_syntax in
  let* b, delegate = Context.init_with_constants1 constants in
  let* b, staker = originate_implicit_unrevealed_account ?amount b delegate in
  let* b = bake_set_delegate_parameters_until_activation b ~delegate in
  let* staker_account = Context.Contract.manager (B b) staker in
  let* delegate_account = Context.Contract.manager (B b) delegate in
  let pkh =
    if self_delegate_staker then staker_account.pkh else delegate_account.pkh
  in
  let* set_delegate =
    Op.delegation ~force_reveal:true (B b) staker (Some pkh)
  in
  let* b = Block.bake ~operation:set_delegate b in
  return (b, delegate, staker)

let check_balances b ~staker ~spendable ~staked ?(unstaked_frozen = Tez.zero)
    ?(unstaked_finalizable = Tez.zero) () =
  let open Lwt_result_syntax in
  let equal_tez_option ~loc a b =
    let a = Option.value ~default:Tez.zero a in
    Assert.equal_tez ~loc a b
  in
  let* computed_spendable = Context.Contract.balance (B b) staker in
  let* () = Assert.equal_tez ~loc:__LOC__ spendable computed_spendable in
  let* computed_staked = Context.Contract.staked_balance (B b) staker in
  let* () = equal_tez_option ~loc:__LOC__ computed_staked staked in
  let* computed_unstaked_frozen =
    Context.Contract.unstaked_frozen_balance (B b) staker
  in
  let* () =
    equal_tez_option ~loc:__LOC__ computed_unstaked_frozen unstaked_frozen
  in
  let* computed_unstaked_finalizable =
    Context.Contract.unstaked_finalizable_balance (B b) staker
  in
  let* () =
    equal_tez_option
      ~loc:__LOC__
      computed_unstaked_finalizable
      unstaked_finalizable
  in
  return_unit

(* stake with inconsistent pkh (source <> destination) *)
let () =
  register_test ~title:"stake with inconsistent pkh" @@ fun () ->
  let open Lwt_result_syntax in
  let* b, delegate, staker =
    create_delegate_and_staker ~self_delegate_staker:false ()
  in
  let* stake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
      (B b)
      staker
      delegate
      Tez_helpers.one
  in
  let*! b = Block.bake ~operation:stake b in
  Assert.proto_error ~loc:__LOC__ b (function
    | Protocol.Apply.Invalid_self_transaction_destination -> true
    | _ -> false)

(* low balance for staked amount itself *)
let stake_low_spendable_balance ~self_delegate_staker =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, _delegate, staker =
    create_delegate_and_staker ~self_delegate_staker ~amount ()
  in
  let* () = check_balances b ~staker ~spendable:amount ~staked:Tez.zero () in
  let* stake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount
  in
  if self_delegate_staker then
    let* b = Block.bake ~operation:stake b in
    let* () = check_balances b ~staker ~spendable:Tez.zero ~staked:amount () in
    return_unit
  else
    let*! b = Block.bake ~operation:stake b in
    Assert.proto_error ~loc:__LOC__ b (function
      | Protocol.Contract_storage.Empty_implicit_delegated_contract c ->
          Signature.Public_key_hash.(c = Account.pkh_of_contract_exn staker)
      | _ -> false)

let () =
  register_test ~title:"stake with low spendable balance (external staker)"
  @@ fun () -> stake_low_spendable_balance ~self_delegate_staker:false

let () =
  register_test ~title:"stake with low spendable balance (self delegate)"
  @@ fun () -> stake_low_spendable_balance ~self_delegate_staker:true

(* low balance for fees with non-zero staked balance *)
let fee_low_spendable_balance_non_zero_staked ~self_delegate_staker =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, _delegate, staker =
    create_delegate_and_staker ~self_delegate_staker ~amount ()
  in
  let* () = check_balances b ~staker ~spendable:amount ~staked:Tez.zero () in

  let fee = Tez_helpers.of_int 5 in
  let amount_minus_fee = Tez_helpers.(amount -! fee) in
  let* stake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount_minus_fee
  in
  let* b = Block.bake ~operation:stake b in
  let* () =
    check_balances b ~staker ~spendable:fee ~staked:amount_minus_fee ()
  in

  let* unstake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
      ~fee
      (B b)
      staker
      staker
      amount_minus_fee
  in
  if self_delegate_staker then
    let* b = Block.bake ~operation:unstake b in
    let* () =
      check_balances
        b
        ~staker
        ~spendable:Tez.zero
        ~staked:Tez.zero
        ~unstaked_frozen:amount_minus_fee
        ()
    in
    return_unit
  else
    let*! b = Block.bake ~operation:unstake b in
    Assert.proto_error ~loc:__LOC__ b (function
      | Protocol.Contract_storage.Empty_implicit_delegated_contract c ->
          Signature.Public_key_hash.(c = Account.pkh_of_contract_exn staker)
      | _ -> false)

let () =
  register_test
    ~title:
      "fees with low spendable balance and non-zero staked (external staker)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_staked ~self_delegate_staker:false

let () =
  register_test
    ~title:"fees with low spendable balance and non-zero staked (self delegate)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_staked ~self_delegate_staker:true

(* low balance for fees with non-zero unfinalizable unstake *)
(* non-zero unfinalizable unstake ==> unstaked_frozen <> zero *)
let fee_low_spendable_balance_non_zero_unfinalizable_unstake
    ~self_delegate_staker =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, _delegate, staker =
    create_delegate_and_staker ~self_delegate_staker ~amount ()
  in
  let* () = check_balances b ~staker ~spendable:amount ~staked:Tez.zero () in

  let fee = Tez_helpers.of_int 5 in
  let amount_minus_fee = Tez_helpers.(amount -! fee) in
  let* stake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount_minus_fee
  in
  let* b = Block.bake ~operation:stake b in
  let* () =
    check_balances b ~staker ~spendable:fee ~staked:amount_minus_fee ()
  in

  let* unstake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount_minus_fee
  in
  let* b = Block.bake ~operation:unstake b in
  let* () =
    check_balances
      b
      ~staker
      ~spendable:fee
      ~staked:Tez.zero
      ~unstaked_frozen:amount_minus_fee
      ()
  in

  let* finalize =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
      ~fee
      (B b)
      staker
      staker
      Tez_helpers.zero
  in
  if self_delegate_staker then
    let* b = Block.bake ~operation:finalize b in
    let* () =
      check_balances
        b
        ~staker
        ~spendable:Tez.zero
        ~staked:Tez.zero
        ~unstaked_frozen:amount_minus_fee
        ()
    in
    return_unit
  else
    let*! b = Block.bake ~operation:finalize b in
    Assert.proto_error ~loc:__LOC__ b (function
      | Protocol.Contract_storage.Empty_implicit_delegated_contract c ->
          Signature.Public_key_hash.(c = Account.pkh_of_contract_exn staker)
      | _ -> false)

let () =
  register_test
    ~title:
      "fees with low spendable balance and non-zero unfinalizable unstake \
       (external staker)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_unfinalizable_unstake
    ~self_delegate_staker:false

let () =
  register_test
    ~title:
      "fees with low spendable balance and non-zero unfinalizable unstake \
       (self delegate)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_unfinalizable_unstake
    ~self_delegate_staker:true

(* low balance for fees with non-zero finalizable unstake *)
let fee_low_spendable_balance_non_zero_finalizable_unstake ~self_delegate_staker
    =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, _delegate, staker =
    create_delegate_and_staker ~self_delegate_staker ~amount ()
  in
  let* () = check_balances b ~staker ~spendable:amount ~staked:Tez.zero () in

  let fee = Tez_helpers.of_int 5 in
  let amount_minus_fee = Tez_helpers.(amount -! fee) in
  let* stake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount_minus_fee
  in
  let* b = Block.bake ~operation:stake b in
  let* () =
    check_balances b ~staker ~spendable:fee ~staked:amount_minus_fee ()
  in

  let* unstake =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
      ~fee:Tez.zero
      (B b)
      staker
      staker
      amount_minus_fee
  in
  let* b = Block.bake ~operation:unstake b in
  let* b =
    Block.bake_until_n_cycle_end
      (constants.consensus_rights_delay + Constants.slashing_delay + 1)
      b
  in
  let* () =
    check_balances
      b
      ~staker
      ~spendable:fee
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:amount_minus_fee
      ()
  in

  let* finalize =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
      ~fee
      (B b)
      staker
      staker
      Tez_helpers.zero
  in

  if self_delegate_staker then
    let* b = Block.bake ~operation:finalize b in
    let* () =
      check_balances
        b
        ~staker
        ~spendable:amount_minus_fee
        ~staked:Tez.zero
        ~unstaked_frozen:Tez.zero
        ()
    in
    return_unit
  else
    let*! b = Block.bake ~operation:finalize b in
    Assert.proto_error ~loc:__LOC__ b (function
      | Protocol.Contract_storage.Empty_implicit_delegated_contract c ->
          Signature.Public_key_hash.(c = Account.pkh_of_contract_exn staker)
      | _ -> false)

let () =
  register_test
    ~title:
      "fees with low spendable balance and non-zero finalizable unstake \
       (external staker)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_finalizable_unstake
    ~self_delegate_staker:false

let () =
  register_test
    ~title:
      "fees with low spendable balance and non-zero finalizable unstake (self \
       delegate)"
  @@ fun () ->
  fee_low_spendable_balance_non_zero_finalizable_unstake
    ~self_delegate_staker:true

(* Finn POC: creates 2 stakers whose unstake requests are finalized
   from a different account (finn). Note that we delay unstake
   requests so that they are issued in different cycles. *)
let finn_finalize_batch () =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, delegate = Context.init_with_constants1 constants in
  let* b, finn = originate_implicit_unrevealed_account ~amount b delegate in
  let* finn_account = Context.Contract.manager (B b) finn in
  (* We need to manually reveal finn in order to create a batch later *)
  let* reveal_finn = Op.revelation ~fee:Tez.zero (B b) finn_account.pk in
  let* b = Block.bake ~operation:reveal_finn b in
  let* b = bake_set_delegate_parameters_until_activation b ~delegate in
  let create_staker_and_delegate amount block delegate =
    let* b, staker =
      originate_implicit_unrevealed_account ~amount block delegate
    in
    let* delegate_account = Context.Contract.manager (B b) delegate in
    let* set_delegate =
      Op.delegation ~force_reveal:true (B b) staker (Some delegate_account.pkh)
    in
    let* b = Block.bake ~operation:set_delegate b in
    return (b, staker)
  in
  let* b, staker1 = create_staker_and_delegate amount b delegate in
  let* b, staker2 = create_staker_and_delegate amount b delegate in
  let* b = bake_set_delegate_parameters_until_activation b ~delegate in
  let margin = Tez_helpers.of_int 1 in
  let staked_amount = Tez_helpers.(amount -! margin) in
  let stake_and_bake staker block staked_amount =
    let* stake_op =
      Op.transaction
        ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
        ~fee:Tez.zero
        (B block)
        staker
        staker
        staked_amount
    in
    let* b = Block.bake ~operation:stake_op block in
    let* () =
      check_balances
        b
        ~staker:staker1
        ~spendable:margin
        ~staked:staked_amount
        ()
    in
    return b
  in
  (* Stake 9_999 tez for each staker1 and staker2 with delegate. *)
  let* b = stake_and_bake staker1 b staked_amount in
  let* b = stake_and_bake staker2 b staked_amount in
  let unstake_and_bake staker unstake_amount spendable_balance block =
    let* unstake_op =
      Op.transaction
        ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
        ~fee:Tez.zero
        (B block)
        staker
        staker
        unstake_amount
    in
    let* b = Block.bake ~operation:unstake_op block in
    let* () =
      check_balances
        b
        ~staker
        ~spendable:spendable_balance
        ~staked:Tez.zero
        ~unstaked_frozen:unstake_amount
        ()
    in
    return b
  in
  let* b = unstake_and_bake staker1 staked_amount margin b in
  (* We want to delay the unstake requests so they come from
     different cycles. *)
  let* b = Block.bake_until_n_cycle_end 1 b in
  let* b = unstake_and_bake staker2 staked_amount margin b in
  let* b =
    Block.bake_until_n_cycle_end
      (constants.consensus_rights_delay + Constants.slashing_delay + 1)
      b
  in
  (* Both staker1 and staker 2 should be finalizable now. *)
  let* () =
    check_balances
      b
      ~staker:staker1
      ~spendable:margin
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:staked_amount
      ()
  in
  let* () =
    check_balances
      b
      ~staker:staker2
      ~spendable:margin
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:staked_amount
      ()
  in
  (* Now we need to manually create a batch of finalization ops signed
     by Finn. *)
  let* batch =
    let finalize_staker staker =
      Op.transaction
        ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
        ~fee:Tez.zero
        (B b)
        finn
        staker
        Tez_helpers.zero
    in
    let* finalize1 = finalize_staker staker1 in
    let* finalize2 = finalize_staker staker2 in
    Op.batch_operations
      ~recompute_counters:true
      ~source:finn
      (B b)
      [finalize1; finalize2]
  in
  let* b = Block.bake ~operation:batch b in
  (* we assert finalized work *)
  let* () =
    check_balances
      b
      ~staker:staker1
      ~spendable:amount
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:Tez.zero
      ()
  in
  check_balances
    b
    ~staker:staker2
    ~spendable:amount
    ~staked:Tez.zero
    ~unstaked_frozen:Tez.zero
    ~unstaked_finalizable:Tez.zero
    ()

let () =
  register_test
    ~title:"Finn: Finn finalizes staker1 and staker2 in a single batch."
  @@ fun () -> finn_finalize_batch ()

(* Finn POC: Following Raphael's remark we test what happens if the
   finalize operation is submitted twice in a block. We create 3
   stakers whose unstake requests are finalized from a diffrerent
   account (finn). Note that we delay unstake requests so that they
   are issued in different cycles. *)
let finn_finalize_interferance () =
  let open Lwt_result_syntax in
  let amount = Tez_helpers.of_int 10_000 in
  let* b, delegate = Context.init_with_constants1 constants in
  let* b, finn = originate_implicit_unrevealed_account ~amount b delegate in
  let* finn_account = Context.Contract.manager (B b) finn in
  (* We need to manually reveal finn in order to create a batch later *)
  let* reveal_finn = Op.revelation ~fee:Tez.zero (B b) finn_account.pk in
  let* b = Block.bake ~operation:reveal_finn b in
  let* b = bake_set_delegate_parameters_until_activation b ~delegate in
  let create_staker_and_delegate amount block delegate =
    let* b, staker =
      originate_implicit_unrevealed_account ~amount block delegate
    in
    let* delegate_account = Context.Contract.manager (B b) delegate in
    let* set_delegate =
      Op.delegation ~force_reveal:true (B b) staker (Some delegate_account.pkh)
    in
    let* b = Block.bake ~operation:set_delegate b in
    return (b, staker)
  in
  let* b, staker1 = create_staker_and_delegate amount b delegate in
  let* b, staker2 = create_staker_and_delegate amount b delegate in
  let* b, staker3 = create_staker_and_delegate amount b delegate in
  let* b = bake_set_delegate_parameters_until_activation b ~delegate in
  let margin = Tez_helpers.of_int 1 in
  let staked_amount = Tez_helpers.(amount -! margin) in
  let stake_and_bake staker block staked_amount =
    let* stake_op =
      Op.transaction
        ~entrypoint:Protocol.Alpha_context.Entrypoint.stake
        ~fee:Tez.zero
        (B block)
        staker
        staker
        staked_amount
    in
    let* b = Block.bake ~operation:stake_op block in
    let* () =
      check_balances
        b
        ~staker:staker1
        ~spendable:margin
        ~staked:staked_amount
        ()
    in
    return b
  in
  (* Stake 9_999 tez for each staker1 and staker2 with delegate. *)
  let* b = stake_and_bake staker1 b staked_amount in
  let* b = stake_and_bake staker2 b staked_amount in
  let* b = stake_and_bake staker3 b staked_amount in
  let unstake_and_bake staker unstake_amount spendable_balance block =
    let* unstake_op =
      Op.transaction
        ~entrypoint:Protocol.Alpha_context.Entrypoint.unstake
        ~fee:Tez.zero
        (B block)
        staker
        staker
        unstake_amount
    in
    let* b = Block.bake ~operation:unstake_op block in
    let* () =
      check_balances
        b
        ~staker
        ~spendable:spendable_balance
        ~staked:Tez.zero
        ~unstaked_frozen:unstake_amount
        ()
    in
    return b
  in
  let* b = unstake_and_bake staker1 staked_amount margin b in
  let* b = unstake_and_bake staker2 staked_amount margin b in
  let* b = unstake_and_bake staker3 staked_amount margin b in
  let* b =
    Block.bake_until_n_cycle_end
      (constants.consensus_rights_delay + Constants.slashing_delay + 1)
      b
  in
  (* Both staker1 and staker 2 should be finalizable now. *)
  let* () =
    check_balances
      b
      ~staker:staker1
      ~spendable:margin
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:staked_amount
      ()
  in
  let* () =
    check_balances
      b
      ~staker:staker2
      ~spendable:margin
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:staked_amount
      ()
  in
  let* () =
    check_balances
      b
      ~staker:staker3
      ~spendable:margin
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:staked_amount
      ()
  in

  (* Ensure finalising for an unallocated account doesn't cause the batch to fail. *)
  let non_allocated_contract = Contract.Implicit (Account.new_account ()).pkh in
  let* () =
    check_balances
      b
      ~staker:non_allocated_contract
      ~spendable:Tez.zero
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:Tez.zero
      ()
  in
  (* Now we need to manually create a batch of finalization ops signed
     by Finn. *)
  let finalize_staker source destination =
    Op.transaction
      ~entrypoint:Protocol.Alpha_context.Entrypoint.finalize_unstake
      ~fee:Tez.zero
      (B b)
      source
      destination
      Tez_helpers.zero
  in
  let* batch =
    let* finalize1 = finalize_staker finn staker1 in
    let* finalize2 = finalize_staker finn staker2 in
    let* finalize_non_allocated = finalize_staker finn non_allocated_contract in
    let* finalize3 = finalize_staker finn staker3 in
    Op.batch_operations
      ~recompute_counters:true
      ~source:finn
      (B b)
      [finalize1; finalize2; finalize_non_allocated; finalize3]
  in
  let* finalize_self = finalize_staker staker2 staker2 in
  let* b = Block.bake ~operations:[finalize_self; batch] b in
  (* we assert finalized work *)
  let* () =
    check_balances
      b
      ~staker:staker1
      ~spendable:amount
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:Tez.zero
      ()
  in
  let* () =
    check_balances
      b
      ~staker:staker2
      ~spendable:amount
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:Tez.zero
      ()
  in
  let* () =
    check_balances
      b
      ~staker:staker3
      ~spendable:amount
      ~staked:Tez.zero
      ~unstaked_frozen:Tez.zero
      ~unstaked_finalizable:Tez.zero
      ()
  in
  check_balances
    b
    ~staker:non_allocated_contract
    ~spendable:Tez.zero
    ~staked:Tez.zero
    ~unstaked_frozen:Tez.zero
    ~unstaked_finalizable:Tez.zero
    ()

let () =
  register_test
    ~title:
      "Finn: Finn batch finalizes correctly even if one of the stakers submits \
       a finalize operation in parallel."
  @@ fun () -> finn_finalize_interferance ()
