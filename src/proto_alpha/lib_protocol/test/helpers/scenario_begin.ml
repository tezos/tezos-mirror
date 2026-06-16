(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open State_account
open Scenario_dsl
open Scenario_bake
open Scenario_base
open Log_helpers
open Adaptive_issuance_helpers
open Scenario_constants

(** Returns when the number of bootstrap accounts created by [Context.init_n n] is not equal to [n] *)
type error += Inconsistent_number_of_bootstrap_accounts

type starter_constants = Mainnet | Sandbox | Test

let start ~(constants : starter_constants) : (unit, constants) scenarios =
  let constants, name =
    match constants with
    | Mainnet -> (Default_parameters.constants_mainnet, "mainnet")
    | Sandbox -> (Default_parameters.constants_sandbox, "sandbox")
    | Test -> (Default_parameters.constants_test, "test")
  in
  Action
    (fun () ->
      Log.info ~color:begin_end_color "-- Begin test --" ;
      Log.info "Loading constants_%s." name ;
      Lwt_result_syntax.return constants)

let start_with ~(constants : constants) : (unit, constants) scenarios =
  Action
    (fun () ->
      Log.info ~color:begin_end_color "-- Begin test --" ;
      Log.info "Loading custom constants." ;
      Lwt_result_syntax.return constants)

let start_with_list ~(constants : (string * constants) list) :
    (unit, constants) scenarios =
  match constants with
  | [] ->
      Stdlib.failwith
        (Format.asprintf "%s: Cannot build scenarios from empty list" __LOC__)
  | _ -> fold_tag (fun constants -> start_with ~constants) constants

(** Initializes the constants for testing, with well chosen default values.
    Recommended over [start] or [start_with] *)
let init_constants ?(default = Test) ?(reward_per_block = 0L)
    ?(deactivate_dynamic = false) ?blocks_per_cycle
    ?delegate_parameters_activation_delay () =
  let base_total_issued_per_minute = Tez.of_mutez reward_per_block in
  start ~constants:default
  -->
  (* default for tests: 12 *)
  set_opt S.blocks_per_cycle blocks_per_cycle
  --> set_opt
        S.delegate_parameters_activation_delay
        delegate_parameters_activation_delay
  --> set
        S.issuance_weights
        {
          base_total_issued_per_minute;
          baking_reward_fixed_portion_weight = 1;
          baking_reward_bonus_weight = 0;
          attesting_reward_weight = 0;
          seed_nonce_revelation_tip_weight = 0;
          vdf_revelation_tip_weight = 0;
          dal_rewards_weight = 0;
        }
  --> set S.liquidity_baking_subsidy Tez.zero
  --> set S.minimal_block_delay Protocol.Alpha_context.Period.one_minute
  --> set S.cost_per_byte Tez.zero
  --> set S.consensus_threshold_size 0
  (* Make abaab never activate by default *)
  --> set
        S.all_bakers_attest_activation_threshold
        {numerator = 2; denominator = 1}
  -->
  if deactivate_dynamic then
    set
      S.Adaptive_issuance.Adaptive_rewards_params.max_bonus
      (Protocol.Issuance_bonus_repr.max_bonus_parameter_of_Q_exn Q.zero)
  else Empty

type algo =
  | Any_algo
  | Ed25519
  | Secp256k1
  | P256
  | Bls
  | Not_Bls_or_Mldsa44
  | Mldsa44
  | Not_Mldsa44

let algo_to_algo ~rng_state = function
  | Ed25519 -> Some Signature.Ed25519
  | Secp256k1 -> Some Secp256k1
  | P256 -> Some P256
  | Bls -> Some Bls
  | Mldsa44 -> Some Mldsa44
  | Any_algo -> None
  | Not_Bls_or_Mldsa44 -> (
      match Random.State.int rng_state 3 with
      | 0 -> Some Signature.Ed25519
      | 1 -> Some Secp256k1
      | 2 -> Some P256
      | _ -> assert false)
  | Not_Mldsa44 -> (
      match Random.State.int rng_state 4 with
      | 0 -> Some Signature.Ed25519
      | 1 -> Some Secp256k1
      | 2 -> Some P256
      | 3 -> Some Bls
      | _ -> assert false)

(* None = not set, Some None = set to None (not the same) *)
(* type before applying defaults *)
type bootstrap_info = {
  name : string;
  balance_opt : int64 option;
  algo_opt : algo option;
  delegate_opt : string option;
  consensus_key_algo_opt_opt : algo option option;
  companion_key_flag_opt : bool option;
}

(** Used to build bootstrap detailed info.
    @param algo defines the algorithm to use for the given bootstrap.
    Set to [Any_algo] to choose one randomly.
    @param balance is the initial balance of the bootstrap account. Defaults to
    whatever {!Account.make_bootstrap_accounts} sets it to, which should be
    {!Default_parameters.Internal_for_tests.bootstrap_balance}
    @param delegate is the name of the initial delegate of the account. If set, then the
    bootstrap account will not be a self delegate. It cannot be set at the same time as the consensus key.
    @param consensus_key [None] means that the bootstrap account does not have a consensus key.
    [Some algo] will set a consensus key with the given algo.
    @param companion_key is a flag that sets a BLS companion key iff the parameter is set to [true].
*)
let make ?algo ?balance ?delegate ?consensus_key ?companion_key name =
  (match (delegate, consensus_key) with
  | Some _, Some _ ->
      Stdlib.failwith
        "Cannot set both delegate and consensus key for bootstrap account."
  | _ -> ()) ;
  {
    name;
    balance_opt = balance;
    algo_opt = algo;
    delegate_opt = delegate;
    consensus_key_algo_opt_opt = consensus_key;
    companion_key_flag_opt = companion_key;
  }

(* type after applying defaults *)
type bootstrap_full_info = {
  name : string;
  algo : algo;
  balance_opt : int64 option;
  delegate_opt : string option;
  consensus_key_algo_opt : algo option;
  companion_key_flag : bool;
}

(* type after creating accounts *)
type bootstrap_accounts = {
  name : string;
  balance : int64 option;
  account : Account.t;
  delegate : string option;
  consensus_key : Account.t option;
  companion_key : Account.t option;
}

let pp_bootstrap_info fmt (info : bootstrap_accounts) =
  let fs = Format.asprintf in
  let consensus_key =
    Option.map
      (fun (x : Account.t) ->
        fs " (consensus key: %a)" Signature.Public_key_hash.pp x.pkh)
      info.consensus_key
  in
  let delegate = Option.map (fun x -> fs " (delegate: %s)" x) info.delegate in
  Format.fprintf
    fmt
    "%s: %a%a%a"
    info.name
    Signature.Public_key_hash.pp
    info.account.pkh
    Format.(pp_print_option pp_print_string)
    consensus_key
    Format.(pp_print_option pp_print_string)
    delegate

(** Initialize the test, given some initial parameters.
    [algo] defines the algorithm used for the [delegates_name_list].
    If not set, a random algorithm is selected for each.
    To use a different algorithm for each delegate, use [delegates_with_algo] *)
let begin_test ?(bootstrap_info_list = ([] : bootstrap_info list))
    ?(default_algo = (Any_algo : algo))
    ?(default_consensus_key = (None : algo option))
    ?(default_companion_key = false) ?(burn_rewards = false)
    ?(force_attest_all = false) ?(force_preattest_all = false)
    ?(check_finalized_every_block = []) ?(disable_default_checks = false)
    ?(rng_state = Random.State.make_self_init ())
    ?(abaab_activation_levels = []) (default_bootstrap_name_list : string list)
    : (constants, t) scenarios =
  let f abaab_activation_level =
    let g constants =
      let open Lwt_result_syntax in
      let make_bootstrap_full (bootstrap_info : bootstrap_info) :
          bootstrap_full_info =
        {
          name = bootstrap_info.name;
          balance_opt = bootstrap_info.balance_opt;
          delegate_opt = bootstrap_info.delegate_opt;
          algo = Option.value ~default:default_algo bootstrap_info.algo_opt;
          consensus_key_algo_opt =
            Option.value
              ~default:default_consensus_key
              bootstrap_info.consensus_key_algo_opt_opt;
          companion_key_flag =
            Option.value
              ~default:default_companion_key
              bootstrap_info.companion_key_flag_opt;
        }
      in
      let default_bootstrap name =
        make_bootstrap_full
          {
            name;
            balance_opt = None;
            delegate_opt = None;
            algo_opt = None;
            consensus_key_algo_opt_opt = None;
            companion_key_flag_opt = None;
          }
      in
      let bootstrap_info_list =
        List.map default_bootstrap default_bootstrap_name_list
        @ List.map make_bootstrap_full bootstrap_info_list
      in
      (* Please setup at least one bootstrap account *)
      assert (not @@ List.is_empty bootstrap_info_list) ;
      (* Do not disable default checks, unless for a good reason *)
      let check_finalized_every_block =
        if disable_default_checks then check_finalized_every_block
        else
          [check_all_balances; check_misc; check_issuance_rpc]
          @ check_finalized_every_block
      in
      (* Setup bootstraps and their keys *)
      let bootstrap_accounts_list =
        List.map
          (fun x ->
            let account =
              Account.new_account
                ?algo:(algo_to_algo ~rng_state x.algo)
                ~rng_state
                ()
            in
            let consensus_key =
              Option.map
                (fun algo ->
                  Account.new_account
                    ?algo:(algo_to_algo ~rng_state algo)
                    ~rng_state
                    ())
                x.consensus_key_algo_opt
            in
            let companion_key =
              if not x.companion_key_flag then None
              else Some (Account.new_account ~algo:Bls ~rng_state ())
            in
            {
              name = x.name;
              balance = x.balance_opt;
              account;
              delegate = x.delegate_opt;
              consensus_key;
              companion_key;
            })
          bootstrap_info_list
      in
      (* Setup the bootstrap accounts in the parameters *)
      let get_consensus_key_pk_opt =
       fun a -> Option.map (fun x -> x.Account.pk) a.consensus_key
      in
      let get_delegate_pkh_opt =
       fun a ->
        Option.map
          (fun delegate_name ->
            let delegate =
              Stdlib.List.find
                (fun x -> String.equal x.name delegate_name)
                bootstrap_accounts_list
            in
            delegate.account.pkh)
          a.delegate
      in
      let bootstrap_accounts_packed =
        List.map
          (fun a ->
            ( a.account,
              a.balance,
              get_delegate_pkh_opt a,
              get_consensus_key_pk_opt a ))
          bootstrap_accounts_list
      in
      let bootstrap_accounts =
        Account.make_bootstrap_accounts_packed bootstrap_accounts_packed
      in
      let parameters =
        Tezos_protocol_alpha_parameters.Default_parameters
        .parameters_of_constants
          ~bootstrap_accounts
          constants
      in
      Log.info
        "Starting chain with bootstrap accounts:@.%a@."
        (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_bootstrap_info)
        bootstrap_accounts_list ;
      (* Hack-in the companion keys in the context *)
      let set_companion ctxt =
        List.fold_left_es
          (fun ctxt bootstrap ->
            match bootstrap.companion_key with
            | None -> return ctxt
            | Some ck ->
                let key = bootstrap.account.Account.pkh in
                (* Add the companion key to the global cks storage *)
                let*! ctxt = Protocol.Storage.Consensus_keys.add ctxt ck.pkh in
                (* Set the companion key for the bootstrap *)
                let ck_pk = match ck.pk with Bls x -> x | _ -> assert false in
                let ck_pkh =
                  match ck.pkh with Bls x -> x | _ -> assert false
                in
                let*! ctxt =
                  Protocol.Storage.Contract.Companion_key.add
                    ctxt
                    (Implicit key)
                    ck_pk
                in
                (* Clear the cache: we need to update it with the companion key values. *)
                let ctxt = Protocol.Raw_context.Cache.clear ctxt in
                (* Update the [Delegate_sampler_state] with the companion key *)
                let*! ctxt =
                  Protocol.Storage.Delegate_sampler_state.fold
                    ctxt
                    ~order:`Undefined
                    ~init:ctxt
                    ~f:(fun cycle sampler ctxt ->
                      let sampler =
                        Protocol.Sampler.map
                          (fun (x : Protocol.Raw_context.consensus_pk) ->
                            if Signature.Public_key_hash.equal x.delegate key
                            then
                              {
                                x with
                                companion_pk = Some ck_pk;
                                companion_pkh = Some ck_pkh;
                              }
                            else x)
                          sampler
                      in
                      Protocol.Storage.Delegate_sampler_state.add
                        ctxt
                        cycle
                        sampler)
                in
                return ctxt)
          ctxt
          bootstrap_accounts_list
      in
      (* abaab activation level overwrite *)
      let abaab_activation_level_overwrite =
        match abaab_activation_level with
        | None -> return
        | Some level ->
            fun ctxt ->
              let level =
                Protocol.Raw_level_repr.of_int32_exn (Int32.of_int level)
              in
              let cycle_eras = Protocol.Raw_context.cycle_eras ctxt in
              let level =
                Protocol.Level_repr.level_from_raw ~cycle_eras level
              in
              let*! ctxt =
                Protocol.Storage.All_bakers_attest_activation.add ctxt level
              in
              return ctxt
      in
      (* Genesis *)
      let prepare_context ctxt =
        let* ctxt = set_companion ctxt in
        let* ctxt = abaab_activation_level_overwrite ctxt in
        return ctxt
      in
      let* block = Block.genesis_with_parameters ~prepare_context parameters in
      let*? init_level = Context.get_level (B block) in
      (* init state *)
      let account_map =
        List.fold_left
          (fun account_map bootstrap_account ->
            let name = bootstrap_account.name in
            let contract =
              Protocol.Alpha_context.Contract.Implicit
                bootstrap_account.account.pkh
            in
            let initial_full =
              Option.map Tez.of_mutez bootstrap_account.balance
              |> Option.value ~default:Account.default_initial_full_balance
            in
            let delegate =
              Option.value ~default:name bootstrap_account.delegate
            in
            let self_delegate = String.equal delegate name in
            let init_staked =
              if not self_delegate then Tez.zero
              else
                Account.bootstrap_initial_staked_balance
                  ~constants
                  ~initial_full_balance:initial_full
            in
            let liquid = Tez.(initial_full -! init_staked) in
            let frozen_deposits = Frozen_tez.init init_staked name name in
            let frozen_rights =
              List.fold_left
                (fun map cycle -> CycleMap.add cycle init_staked map)
                CycleMap.empty
                Cycle.(root ---> add root constants.consensus_rights_delay)
            in
            let pkh = Context.Contract.pkh contract in
            let consensus_key =
              Option.map
                (fun x -> x.Account.pkh)
                bootstrap_account.consensus_key
            in
            let companion_key =
              Option.map
                (fun x ->
                  x.Account.pkh |> function Bls x -> x | _ -> assert false
                  (* by construction, the companion keys built previously are BLS *))
                bootstrap_account.companion_key
            in
            let last_seen_activity =
              if self_delegate then
                Some Cycle.(add root constants.consensus_rights_delay)
              else None
            in
            let account =
              init_account
                ~name
                ~revealed:true
                ~delegate
                ~pkh
                ~contract
                ~parameters:default_params
                ~liquid
                ~frozen_deposits
                ~frozen_rights
                ?last_seen_activity
                ?consensus_key
                ?companion_key
                ()
            in
            let account_map = String.Map.add name account account_map in
            let balance, total_balance =
              balance_and_total_balance_of_account name account_map
            in
            Log.debug "Initial balance for %s:\n%a" name balance_pp balance ;
            Log.debug "Initial total balance: %a" Tez.pp total_balance ;
            account_map)
          String.Map.empty
          bootstrap_accounts_list
      in
      let* total_supply = Context.get_total_supply (B block) in
      let state =
        State.
          {
            account_map;
            total_supply;
            constants;
            param_requests = [];
            baking_policy = None;
            payload_round = None;
            last_level_rewards = init_level;
            snapshot_balances = String.Map.empty;
            saved_rate = None;
            burn_rewards;
            pending_operations = [];
            pending_batch = [];
            source_batch = None;
            pending_slashes = [];
            double_signings = [];
            force_attest_all;
            force_preattest_all;
            check_finalized_every_block;
            check_finalized_current_block = [];
            previous_metadata = None;
            operation_mode = Bake;
            (* The (grand)grandparent(s) is only used to get the consensus key, so it is
               fine to set it to Genesis here. If needed in the future, an option
               type would be more appropriate. *)
            grandparent = block;
            grandgrandparent = block;
          }
      in
      let* () =
        if not disable_default_checks then check_all_balances () (block, state)
        else return_unit
      in
      return (block, state)
    in
    exec g
  in
  match abaab_activation_levels with
  | [] -> f None
  | _ :: _ ->
      fold_tag_f
        f
        (function
          | None -> "abaab initially off"
          | Some level -> Format.asprintf "abaab activation at level %d" level)
        abaab_activation_levels
