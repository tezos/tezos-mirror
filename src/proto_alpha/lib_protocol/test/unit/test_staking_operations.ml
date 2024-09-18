(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    protocol
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
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
    consensus_threshold = 0;
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
  let constants =
    constants |> Constants_helpers.Set.Adaptive_issuance.force_activation true
  in
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
