(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [Account_helpers] defines a type abstracting the information of an account
    in the protocol. This includes its pkh, delegate, any funds, staking
    parameters, etc...

    A type [balance] is also defined, as an observed state of funds for a
    given account, i.e balance information that one might get from calling
    RPCs. *)

open Adaptive_issuance_helpers
open Tez_staking_helpers

let join_errors e1 e2 =
  let open Lwt_result_syntax in
  match (e1, e2) with
  | Ok (), Ok () -> return_unit
  | Error e, Ok () | Ok (), Error e -> fail e
  | Error e1, Error e2 -> fail (e1 @ e2)

let fail_account_not_found func_name account_name =
  Log.error "State_account.%s: account %s not found" func_name account_name ;
  assert false

module CycleMap = Map.Make (Cycle)

(** Abstract information of accounts *)
type account_state = {
  pkh : Signature.Public_key_hash.t;
  contract : Protocol.Alpha_context.Contract.t;
  delegate : string option;
  parameters : staking_parameters;
  liquid : Tez.t;
  bonds : Tez.t;
  (* The three following fields contain maps from the account's stakers to,
     respectively, their frozen stake, their unstaked frozen balance, and
     their unstaked finalizable funds. Additionally, [unstaked_frozen] indexes
     the maps with the cycle at which the unstake operation occurred. *)
  frozen_deposits : Frozen_tez.t;
  unstaked_frozen : Unstaked_frozen.t;
  unstaked_finalizable : Unstaked_finalizable.t;
  staking_delegator_numerator : Z.t;
  staking_delegate_denominator : Z.t;
  frozen_rights : Tez.t CycleMap.t;
  slashed_cycles : Cycle.t list;
}

let init_account ?delegate ~pkh ~contract ~parameters ?(liquid = Tez.zero)
    ?(bonds = Tez.zero) ?(frozen_deposits = Frozen_tez.zero)
    ?(unstaked_frozen = Unstaked_frozen.zero)
    ?(unstaked_finalizable = Unstaked_finalizable.zero)
    ?(staking_delegator_numerator = Z.zero)
    ?(staking_delegate_denominator = Z.zero) ?(frozen_rights = CycleMap.empty)
    ?(slashed_cycles = []) () =
  {
    pkh;
    contract;
    delegate;
    parameters;
    liquid;
    bonds;
    frozen_deposits;
    unstaked_frozen;
    unstaked_finalizable;
    staking_delegator_numerator;
    staking_delegate_denominator;
    frozen_rights;
    slashed_cycles;
  }

type account_map = account_state String.Map.t

(** Balance returned by RPCs. Partial tez are rounded down *)
type balance = {
  liquid_b : Tez.t;
  bonds_b : Tez.t;
  staked_b : Partial_tez.t;
  unstaked_frozen_b : Tez.t;
  unstaked_finalizable_b : Tez.t;
  staking_delegator_numerator_b : Z.t;
  staking_delegate_denominator_b : Z.t;
}

let balance_zero =
  {
    liquid_b = Tez.zero;
    bonds_b = Tez.zero;
    staked_b = Partial_tez.zero;
    unstaked_frozen_b = Tez.zero;
    unstaked_finalizable_b = Tez.zero;
    staking_delegator_numerator_b = Z.zero;
    staking_delegate_denominator_b = Z.zero;
  }

let balance_of_account account_name (account_map : account_map) =
  match String.Map.find account_name account_map with
  | None -> fail_account_not_found "balance_of_account.src" account_name
  | Some
      {
        pkh = _;
        contract = _;
        delegate;
        parameters = _;
        liquid;
        bonds;
        frozen_deposits = _;
        unstaked_frozen = _;
        unstaked_finalizable = _;
        staking_delegator_numerator;
        staking_delegate_denominator;
        frozen_rights = _;
        slashed_cycles = _;
      } ->
      let balance =
        {
          balance_zero with
          liquid_b = liquid;
          bonds_b = bonds;
          staking_delegator_numerator_b = staking_delegator_numerator;
          staking_delegate_denominator_b = staking_delegate_denominator;
        }
      in
      let balance =
        match delegate with
        | None -> balance
        | Some d -> (
            match String.Map.find d account_map with
            | None -> fail_account_not_found "balance_of_account.delegate" d
            | Some delegate_account ->
                {
                  balance with
                  staked_b =
                    Frozen_tez.get account_name delegate_account.frozen_deposits;
                })
      in
      (* Because an account can still have frozen or finalizable funds from a delegate
         that is not its own, we iterate over all of them *)
      let unstaked_frozen_b, unstaked_finalizable_b =
        String.Map.fold
          (fun _delegate_name delegate (frozen, finalzbl) ->
            let frozen =
              Tez.(
                frozen
                +! Unstaked_frozen.get_total
                     account_name
                     delegate.unstaked_frozen)
            in
            let finalzbl =
              Tez.(
                finalzbl
                +! Unstaked_finalizable.get
                     account_name
                     delegate.unstaked_finalizable)
            in
            (frozen, finalzbl))
          account_map
          (Tez.zero, Tez.zero)
      in
      {balance with unstaked_frozen_b; unstaked_finalizable_b}

let balance_pp fmt
    {
      liquid_b;
      bonds_b;
      staked_b;
      unstaked_frozen_b;
      unstaked_finalizable_b;
      staking_delegator_numerator_b;
      staking_delegate_denominator_b;
    } =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a@;\
     bonds : %a@;\
     staked : %a@;\
     unstaked_frozen : %a@;\
     unstaked_finalizable : %a@;\
     staking_delegator_numerator : %a@;\
     staking_delegate_denominator : %a@;\
     }@."
    Tez.pp
    liquid_b
    Tez.pp
    bonds_b
    Partial_tez.pp
    staked_b
    Tez.pp
    unstaked_frozen_b
    Tez.pp
    unstaked_finalizable_b
    Z.pp_print
    staking_delegator_numerator_b
    Z.pp_print
    staking_delegate_denominator_b

let balance_update_pp fmt
    ( {
        liquid_b = a_liquid_b;
        bonds_b = a_bonds_b;
        staked_b = a_staked_b;
        unstaked_frozen_b = a_unstaked_frozen_b;
        unstaked_finalizable_b = a_unstaked_finalizable_b;
        staking_delegator_numerator_b = a_staking_delegator_numerator_b;
        staking_delegate_denominator_b = a_staking_delegate_denominator_b;
      },
      {
        liquid_b = b_liquid_b;
        bonds_b = b_bonds_b;
        staked_b = b_staked_b;
        unstaked_frozen_b = b_unstaked_frozen_b;
        unstaked_finalizable_b = b_unstaked_finalizable_b;
        staking_delegator_numerator_b = b_staking_delegator_numerator_b;
        staking_delegate_denominator_b = b_staking_delegate_denominator_b;
      } ) =
  Format.fprintf
    fmt
    "{@;\
     @[<v 2>  liquid : %a -> %a@;\
     bonds : %a -> %a@;\
     staked : %a -> %a@;\
     unstaked_frozen : %a -> %a@;\
     unstaked_finalizable : %a -> %a@;\
     staking_delegator_numerator : %a -> %a@;\
     staking_delegate_denominator : %a -> %a@;\
     }@."
    Tez.pp
    a_liquid_b
    Tez.pp
    b_liquid_b
    Tez.pp
    a_bonds_b
    Tez.pp
    b_bonds_b
    Partial_tez.pp
    a_staked_b
    Partial_tez.pp
    b_staked_b
    Tez.pp
    a_unstaked_frozen_b
    Tez.pp
    b_unstaked_frozen_b
    Tez.pp
    a_unstaked_finalizable_b
    Tez.pp
    b_unstaked_finalizable_b
    Z.pp_print
    a_staking_delegator_numerator_b
    Z.pp_print
    b_staking_delegator_numerator_b
    Z.pp_print
    a_staking_delegate_denominator_b
    Z.pp_print
    b_staking_delegate_denominator_b

let assert_balance_equal ~loc account_name
    {
      liquid_b = a_liquid_b;
      bonds_b = a_bonds_b;
      staked_b = a_staked_b;
      unstaked_frozen_b = a_unstaked_frozen_b;
      unstaked_finalizable_b = a_unstaked_finalizable_b;
      staking_delegator_numerator_b = a_staking_delegator_numerator_b;
      staking_delegate_denominator_b = a_staking_delegate_denominator_b;
    }
    {
      liquid_b = b_liquid_b;
      bonds_b = b_bonds_b;
      staked_b = b_staked_b;
      unstaked_frozen_b = b_unstaked_frozen_b;
      unstaked_finalizable_b = b_unstaked_finalizable_b;
      staking_delegator_numerator_b = b_staking_delegator_numerator_b;
      staking_delegate_denominator_b = b_staking_delegate_denominator_b;
    } =
  let open Lwt_result_syntax in
  let f s = Format.asprintf "%s: %s" account_name s in
  let* () =
    List.fold_left
      (fun a b ->
        let*! a in
        let*! b in
        join_errors a b)
      return_unit
      [
        Assert.equal
          ~loc
          Tez.equal
          (f "Liquid balances do not match")
          Tez.pp
          a_liquid_b
          b_liquid_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Bonds balances do not match")
          Tez.pp
          a_bonds_b
          b_bonds_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Staked balances do not match")
          Tez.pp
          (Partial_tez.to_tez ~round:`Down a_staked_b)
          (Partial_tez.to_tez ~round:`Down b_staked_b);
        Assert.equal
          ~loc
          Tez.equal
          (f "Unstaked frozen balances do not match")
          Tez.pp
          a_unstaked_frozen_b
          b_unstaked_frozen_b;
        Assert.equal
          ~loc
          Tez.equal
          (f "Unstaked finalizable balances do not match")
          Tez.pp
          a_unstaked_finalizable_b
          b_unstaked_finalizable_b;
        Assert.equal
          ~loc
          Z.equal
          (f "Staking delegator numerators do not match")
          Z.pp_print
          a_staking_delegator_numerator_b
          b_staking_delegator_numerator_b;
        Assert.equal
          ~loc
          Z.equal
          (f "Staking delegate denominators do not match")
          Z.pp_print
          a_staking_delegate_denominator_b
          b_staking_delegate_denominator_b;
      ]
  in
  return_unit

let update_account ~f account_name account_map =
  String.Map.update
    account_name
    (function
      | None -> fail_account_not_found "update_account" account_name
      | Some x -> Some (f x))
    account_map

let balance_and_total_balance_of_account account_name account_map =
  let ({
         liquid_b;
         bonds_b;
         staked_b;
         unstaked_frozen_b;
         unstaked_finalizable_b;
         staking_delegator_numerator_b = _;
         staking_delegate_denominator_b = _;
       } as balance) =
    balance_of_account account_name account_map
  in
  ( balance,
    Tez.(
      liquid_b +! bonds_b
      +! Partial_tez.to_tez ~round:`Down staked_b
      +! unstaked_frozen_b +! unstaked_finalizable_b) )

let assert_pseudotokens_consistency ~loc balance account account_name
    account_map =
  let open Lwt_result_syntax in
  let {delegate; staking_delegator_numerator = num_pt; _} = account in
  let exact_staking_balance = balance.staked_b in
  match delegate with
  | None -> return_unit
  | Some delegate_name -> (
      if account_name = delegate_name then return_unit
      else
        match String.Map.find delegate_name account_map with
        | None ->
            fail_account_not_found
              "assert_pseudotokens_consistency"
              delegate_name
        | Some delegate_account ->
            let total_co =
              Frozen_tez.total_co_current_q
                delegate_account.frozen_deposits.co_current
            in
            let den_pt = delegate_account.staking_delegate_denominator in
            if Z.(equal den_pt zero) then
              Assert.equal
                ~loc
                Q.equal
                (Format.asprintf
                   "%s : Delegate should not have external stake with a 0 \
                    staking denominator"
                   account_name)
                Q.pp_print
                total_co
                Q.zero
            else
              let expected = Q.(num_pt /// den_pt * total_co) in
              Assert.equal
                ~loc
                Q.equal
                (Format.asprintf
                   "%s : Pseudotokens do not match exact staking balance"
                   account_name)
                Q.pp_print
                exact_staking_balance
                expected)

let get_balance_from_context ctxt contract =
  let open Lwt_result_syntax in
  let* liquid_b = Context.Contract.balance ctxt contract in
  let* bonds_b = Context.Contract.frozen_bonds ctxt contract in
  let* staked_b = Context.Contract.staked_balance ctxt contract in
  let staked_b =
    Option.value ~default:Tez.zero staked_b |> Partial_tez.of_tez
  in
  let* unstaked_frozen_b =
    Context.Contract.unstaked_frozen_balance ctxt contract
  in
  let unstaked_frozen_b = Option.value ~default:Tez.zero unstaked_frozen_b in
  let* unstaked_finalizable_b =
    Context.Contract.unstaked_finalizable_balance ctxt contract
  in
  let unstaked_finalizable_b =
    Option.value ~default:Tez.zero unstaked_finalizable_b
  in
  let* total_balance = Context.Contract.full_balance ctxt contract in
  let* staking_delegator_numerator_b =
    Context.Contract.staking_numerator ctxt contract
  in
  let*! staking_delegate_denominator_b =
    match (contract : Protocol.Alpha_context.Contract.t) with
    | Implicit pkh ->
        let*! result = Context.Delegate.staking_denominator ctxt pkh in
        Lwt.return
          (match result with
          | Ok v -> v
          | Error _ -> (* Not a delegate *) Z.zero)
    | Originated _ -> Lwt.return Z.zero
  in
  let bd =
    {
      liquid_b;
      bonds_b;
      staked_b;
      unstaked_frozen_b;
      unstaked_finalizable_b;
      staking_delegator_numerator_b;
      staking_delegate_denominator_b;
    }
  in
  return (bd, total_balance)

let assert_balance_check ~loc ctxt account_name account_map =
  let open Lwt_result_syntax in
  match String.Map.find account_name account_map with
  | None -> fail_account_not_found "assert_balance_check" account_name
  | Some account ->
      let* balance_ctxt, total_balance_ctxt =
        get_balance_from_context ctxt account.contract
      in
      let balance, total_balance =
        balance_and_total_balance_of_account account_name account_map
      in
      let*! r0 =
        assert_pseudotokens_consistency
          ~loc
          balance
          account
          account_name
          account_map
      in
      let*! r1 = assert_balance_equal ~loc account_name balance_ctxt balance in
      let*! r1 = join_errors r0 r1 in
      let*! r2 =
        Assert.equal
          ~loc
          Tez.equal
          (Format.asprintf "%s : Total balances do not match" account_name)
          Tez.pp
          total_balance_ctxt
          total_balance
      in
      join_errors r1 r2

let log_debug_balance account_name account_map : unit =
  let balance, total_balance =
    balance_and_total_balance_of_account account_name account_map
  in
  Log.debug
    "Model balance of %s:\n%aTotal balance: %a\n"
    account_name
    balance_pp
    balance
    Tez.pp
    total_balance

let log_debug_rpc_balance name contract block : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* balance, total_balance = get_balance_from_context (B block) contract in
  Log.debug
    "RPC balance of %s:\n%aTotal balance: %a\n"
    name
    balance_pp
    balance
    Tez.pp
    total_balance ;
  return_unit

let log_debug_balance_update account_name old_account_map new_account_map : unit
    =
  let old_balance, old_total_balance =
    balance_and_total_balance_of_account account_name old_account_map
  in
  let new_balance, new_total_balance =
    balance_and_total_balance_of_account account_name new_account_map
  in
  Log.debug
    "Balance update of %s:\n%aTotal balance: %a -> %a\n"
    account_name
    balance_update_pp
    (old_balance, new_balance)
    Tez.pp
    old_total_balance
    Tez.pp
    new_total_balance
