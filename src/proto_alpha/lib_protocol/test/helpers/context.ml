(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

type t = B of Block.t | I of Incremental.t

let get_alpha_ctxt ?(policy = Block.By_round 0) c =
  let open Lwt_result_syntax in
  match c with
  | I i -> return (Incremental.alpha_ctxt i)
  | B b ->
      let* i = Incremental.begin_construction ~policy b in
      return (Incremental.alpha_ctxt i)

let branch = function B b -> b.hash | I i -> (Incremental.predecessor i).hash

let pred_branch = function
  | B b -> b.header.shell.predecessor
  | I i -> (Incremental.predecessor i).hash

let level = function B b -> b.header.shell.level | I i -> Incremental.level i

let get_level ctxt =
  let open Result_wrap_syntax in
  let+@ res = level ctxt |> Raw_level.of_int32 in
  res

let rpc_ctxt =
  object
    method call_proto_service0
        : 'm 'q 'i 'o.
          ( ([< Tezos_rpc.Service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t,
            'q,
            'i,
            'o )
          Tezos_rpc.Service.t ->
          t ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun s pr q i ->
        match pr with
        | B b -> Block.rpc_ctxt#call_proto_service0 s b q i
        | I b -> Incremental.rpc_ctxt#call_proto_service0 s b q i

    method call_proto_service1
        : 'm 'a 'q 'i 'o.
          ( ([< Tezos_rpc.Service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t * 'a,
            'q,
            'i,
            'o )
          Tezos_rpc.Service.t ->
          t ->
          'a ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun s pr a q i ->
        match pr with
        | B bl -> Block.rpc_ctxt#call_proto_service1 s bl a q i
        | I bl -> Incremental.rpc_ctxt#call_proto_service1 s bl a q i

    method call_proto_service2
        : 'm 'a 'b 'q 'i 'o.
          ( ([< Tezos_rpc.Service.meth] as 'm),
            Environment.RPC_context.t,
            (Environment.RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          Tezos_rpc.Service.t ->
          t ->
          'a ->
          'b ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun s pr a b q i ->
        match pr with
        | B bl -> Block.rpc_ctxt#call_proto_service2 s bl a b q i
        | I bl -> Incremental.rpc_ctxt#call_proto_service2 s bl a b q i

    method call_proto_service3
        : 'm 'a 'b 'c 'q 'i 'o.
          ( ([< Tezos_rpc.Service.meth] as 'm),
            Environment.RPC_context.t,
            ((Environment.RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          Tezos_rpc.Service.t ->
          t ->
          'a ->
          'b ->
          'c ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      fun s pr a b c q i ->
        match pr with
        | B bl -> Block.rpc_ctxt#call_proto_service3 s bl a b c q i
        | I bl -> Incremental.rpc_ctxt#call_proto_service3 s bl a b c q i
  end

let get_attesters ctxt = Plugin.RPC.Validators.get rpc_ctxt ctxt

let get_first_different_attesters ctxt =
  let open Lwt_result_syntax in
  let+ attesters = get_attesters ctxt in
  match attesters with x :: y :: _ -> (x, y) | _ -> assert false

let get_attester ctxt =
  let open Lwt_result_syntax in
  let+ attesters = get_attesters ctxt in
  let attester = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd attesters in
  (attester.consensus_key, attester.slots)

let get_attester_slot ctxt pkh =
  let open Lwt_result_syntax in
  let+ attesters = get_attesters ctxt in
  List.find_map
    (function
      | {Plugin.RPC.Validators.consensus_key; slots; _} ->
          if Signature.Public_key_hash.(consensus_key = pkh) then Some slots
          else None)
    attesters

let get_attester_n ctxt n =
  let open Lwt_result_syntax in
  let+ attesters = Plugin.RPC.Validators.get rpc_ctxt ctxt in
  let attester =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth attesters n
  in
  (attester.consensus_key, attester.slots)

let get_attesting_power_for_delegate ctxt ?level pkh =
  let open Lwt_result_syntax in
  let levels = Option.map (fun level -> [level]) level in
  let* attesters = Plugin.RPC.Validators.get rpc_ctxt ?levels ctxt in
  let rec find_slots_for_delegate = function
    | [] -> return 0
    | {Plugin.RPC.Validators.delegate; slots; _} :: t ->
        if Signature.Public_key_hash.equal delegate pkh then
          return (List.length slots)
        else find_slots_for_delegate t
  in
  find_slots_for_delegate attesters

let get_cumulated_attesting_power_for_delegate ctxt ~levels pkh =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun accu level ->
      let+ power = get_attesting_power_for_delegate ctxt ~level pkh in
      accu + power)
    0
    levels

let get_current_voting_power = Delegate_services.current_voting_power rpc_ctxt

let get_voting_power = Delegate_services.voting_power rpc_ctxt

let get_total_voting_power = Alpha_services.Voting.total_voting_power rpc_ctxt

let get_current_baking_power = Delegate_services.current_baking_power rpc_ctxt

let get_bakers ?filter ?cycle ctxt =
  let open Lwt_result_syntax in
  let+ bakers = Plugin.RPC.Baking_rights.get rpc_ctxt ?cycle ctxt in
  (match filter with None -> bakers | Some f -> List.filter f bakers)
  |> List.map (fun p -> p.Plugin.RPC.Baking_rights.delegate)

let get_baker ctxt ~round =
  let open Lwt_result_syntax in
  let* bakers = get_bakers ~filter:(fun x -> x.round = round) ctxt in
  (* there is only one baker for a given round *)
  match bakers with [baker] -> return baker | _ -> assert false

let get_first_different_baker baker bakers =
  WithExceptions.Option.get ~loc:__LOC__
  @@ List.find
       (fun baker' -> Signature.Public_key_hash.( <> ) baker baker')
       bakers

let get_first_different_bakers ctxt =
  let open Lwt_result_syntax in
  let+ bakers = get_bakers ctxt in
  match bakers with
  | [] -> assert false
  | baker_1 :: other_bakers ->
      (baker_1, get_first_different_baker baker_1 other_bakers)

let get_seed_nonce_hash ctxt =
  let open Lwt_result_syntax in
  let header =
    match ctxt with B {header; _} -> header | I i -> Incremental.header i
  in
  match header.protocol_data.contents.seed_nonce_hash with
  | None -> failwith "No committed nonce"
  | Some hash -> return hash

let get_seed ctxt = Alpha_services.Seed.get rpc_ctxt ctxt

let get_seed_computation ctxt =
  Alpha_services.Seed_computation.get rpc_ctxt ctxt

let get_constants ctxt = Alpha_services.Constants.all rpc_ctxt ctxt

let default_test_constants =
  Tezos_protocol_alpha_parameters.Default_parameters.constants_test

let get_issuance_per_minute ctxt =
  Adaptive_issuance_services.current_issuance_per_minute rpc_ctxt ctxt

let get_baking_reward_fixed_portion ctxt =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = csts; _} = get_constants ctxt in
  let*?@ reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Baking_reward_fixed_portion
  in
  return reward

let get_bonus_reward ctxt ~attesting_power =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = {consensus_threshold; _} as csts; _} =
    get_constants ctxt
  in
  let*?@ baking_reward_bonus_per_slot =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Baking_reward_bonus_per_slot
  in
  let multiplier = max 0 (attesting_power - consensus_threshold) in
  return Test_tez.(baking_reward_bonus_per_slot *! Int64.of_int multiplier)

let get_attesting_reward ctxt ~expected_attesting_power =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = csts; _} = get_constants ctxt in
  let*?@ attesting_reward_per_slot =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Attesting_reward_per_slot
  in
  let*?@ t =
    Tez.(attesting_reward_per_slot *? Int64.of_int expected_attesting_power)
  in
  return t

let get_liquidity_baking_subsidy ctxt =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = csts; _} = get_constants ctxt in
  let*?@ reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Liquidity_baking_subsidy
  in
  return reward

let get_liquidity_baking_cpmm_address ctxt =
  Alpha_services.Liquidity_baking.get_cpmm_address rpc_ctxt ctxt

let get_adaptive_issuance_launch_cycle ctxt =
  Adaptive_issuance_services.launch_cycle rpc_ctxt ctxt

let get_total_frozen_stake ctxt =
  Adaptive_issuance_services.total_frozen_stake rpc_ctxt ctxt

let get_total_supply ctxt =
  Adaptive_issuance_services.total_supply rpc_ctxt ctxt

let get_seed_nonce_revelation_tip ctxt =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = csts; _} = get_constants ctxt in
  let*?@ reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Seed_nonce_revelation_tip
  in
  return reward

let get_vdf_revelation_tip ctxt =
  let open Lwt_result_wrap_syntax in
  let* {Constants.parametric = csts; _} = get_constants ctxt in
  let*?@ reward =
    Delegate.Rewards.For_RPC.reward_from_constants
      csts
      ~reward_kind:Vdf_revelation_tip
  in
  return reward

let get_ai_current_yearly_rate ctxt =
  Adaptive_issuance_services.current_yearly_rate rpc_ctxt ctxt

let get_ai_current_yearly_rate_exact ctxt =
  Adaptive_issuance_services.current_yearly_rate_exact rpc_ctxt ctxt

let get_ai_expected_issuance ctxt =
  Adaptive_issuance_services.expected_issuance rpc_ctxt ctxt

let get_denunciations ctxt =
  Alpha_services.Denunciations.denunciations rpc_ctxt ctxt

module Dal = struct
  let shards ctxt = Plugin.RPC.Dal.dal_shards rpc_ctxt ctxt
end

(* Voting *)

module Vote = struct
  let get_ballots ctxt = Alpha_services.Voting.ballots rpc_ctxt ctxt

  let get_ballot_list ctxt = Alpha_services.Voting.ballot_list rpc_ctxt ctxt

  let get_current_period ctxt =
    Alpha_services.Voting.current_period rpc_ctxt ctxt

  let get_current_quorum ctxt =
    Alpha_services.Voting.current_quorum rpc_ctxt ctxt

  let get_listings ctxt = Alpha_services.Voting.listings rpc_ctxt ctxt

  let get_proposals ctxt = Alpha_services.Voting.proposals rpc_ctxt ctxt

  let get_current_proposal ctxt =
    Alpha_services.Voting.current_proposal rpc_ctxt ctxt

  let get_protocol (b : Block.t) =
    Tezos_protocol_environment.Context.get_protocol b.context

  let get_delegate_proposal_count ctxt pkh =
    Alpha_services.Voting.delegate_proposal_count rpc_ctxt ctxt pkh

  let get_participation_ema (b : Block.t) =
    let open Lwt_syntax in
    let+ bytes_opt =
      Environment.Context.find b.context ["votes"; "participation_ema"]
    in
    match bytes_opt with
    | None -> assert false
    | Some bytes -> Ok (TzEndian.get_int32 bytes 0)

  let set_participation_ema (b : Block.t) ema =
    let open Lwt_syntax in
    let bytes = Bytes.make 4 '\000' in
    TzEndian.set_int32 bytes 0 ema ;
    let+ context =
      Environment.Context.add b.context ["votes"; "participation_ema"] bytes
    in
    {b with context}

  type delegate_info = Alpha_context.Vote.delegate_info = {
    voting_power : Int64.t option;
    current_ballot : Alpha_context.Vote.ballot option;
    current_proposals : Protocol_hash.t list;
    remaining_proposals : int;
  }
end

module Contract = struct
  let pp = Alpha_context.Contract.pp

  let equal a b = Alpha_context.Contract.compare a b = 0

  let pkh = function
    | Contract.Implicit p -> p
    | Originated _ -> Stdlib.failwith "pkh: only for implicit contracts"

  let balance ctxt contract =
    Alpha_services.Contract.balance rpc_ctxt ctxt contract

  let frozen_bonds ctxt contract =
    Alpha_services.Contract.frozen_bonds rpc_ctxt ctxt contract

  let balance_and_frozen_bonds ctxt contract =
    Alpha_services.Contract.balance_and_frozen_bonds rpc_ctxt ctxt contract

  let staked_balance ctxt contract =
    Alpha_services.Contract.staked_balance rpc_ctxt ctxt contract

  let unstaked_frozen_balance ctxt contract =
    Alpha_services.Contract.unstaked_frozen_balance rpc_ctxt ctxt contract

  let unstaked_finalizable_balance ctxt contract =
    Alpha_services.Contract.unstaked_finalizable_balance rpc_ctxt ctxt contract

  let full_balance ctxt contract =
    Alpha_services.Contract.full_balance rpc_ctxt ctxt contract

  let staking_numerator ctxt contract =
    let open Lwt_result_syntax in
    let+ pseudotokens =
      Alpha_services.Contract.staking_numerator rpc_ctxt ctxt contract
    in
    Staking_pseudotoken.Internal_for_tests.to_z pseudotokens

  let counter ctxt (contract : Contract.t) =
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.counter"
    | Implicit mgr -> Alpha_services.Contract.counter rpc_ctxt ctxt mgr

  let manager _ (contract : Contract.t) =
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.manager"
    | Implicit pkh -> Account.find pkh

  let is_manager_key_revealed ctxt (contract : Contract.t) =
    let open Lwt_result_syntax in
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.is_manager_key_revealed"
    | Implicit mgr ->
        let+ res = Alpha_services.Contract.manager_key rpc_ctxt ctxt mgr in
        res <> None

  let delegate ctxt contract =
    Alpha_services.Contract.delegate rpc_ctxt ctxt contract

  let delegate_opt ctxt contract =
    Alpha_services.Contract.delegate_opt rpc_ctxt ctxt contract

  let storage ctxt contract =
    Alpha_services.Contract.storage rpc_ctxt ctxt contract

  let script ctxt contract =
    let open Lwt_result_syntax in
    let* {code; storage = _} =
      Alpha_services.Contract.script rpc_ctxt ctxt contract
    in
    match Data_encoding.force_decode code with
    | Some v -> return v
    | None -> invalid_arg "Cannot force lazy script"

  let script_hash ctxt contract =
    let open Lwt_result_syntax in
    let* script = script ctxt contract in
    let bytes = Data_encoding.Binary.to_bytes_exn Script.expr_encoding script in
    return @@ Script_expr_hash.hash_bytes [bytes]
end

module Delegate = struct
  type info = Delegate_services.info = {
    full_balance : Tez.t;
    current_frozen_deposits : Tez.t;
    frozen_deposits : Tez.t;
    staking_balance : Tez.t;
    frozen_deposits_limit : Tez.t option;
    delegated_contracts : Alpha_context.Contract.t list;
    delegated_balance : Tez.t;
    total_delegated_stake : Tez.t;
    staking_denominator : Staking_pseudotoken.t;
    deactivated : bool;
    grace_period : Cycle.t;
    voting_info : Alpha_context.Vote.delegate_info;
    active_consensus_key : Signature.Public_key_hash.t;
    pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
  }

  type stake = {frozen : Tez.t; weighted_delegated : Tez.t}

  let info ctxt pkh = Delegate_services.info rpc_ctxt ctxt pkh

  let full_balance ctxt pkh = Delegate_services.full_balance rpc_ctxt ctxt pkh

  let current_frozen_deposits ctxt pkh =
    Delegate_services.current_frozen_deposits rpc_ctxt ctxt pkh

  let initial_frozen_deposits ctxt pkh =
    Delegate_services.frozen_deposits rpc_ctxt ctxt pkh

  let staking_balance ctxt pkh =
    Delegate_services.staking_balance rpc_ctxt ctxt pkh

  let staking_denominator ctxt pkh =
    let open Lwt_result_syntax in
    let+ pseudotokens =
      Delegate_services.staking_denominator rpc_ctxt ctxt pkh
    in
    Staking_pseudotoken.Internal_for_tests.to_z pseudotokens

  let frozen_deposits_limit ctxt pkh =
    Delegate_services.frozen_deposits_limit rpc_ctxt ctxt pkh

  let deactivated ctxt pkh = Delegate_services.deactivated rpc_ctxt ctxt pkh

  let voting_info ctxt d = Alpha_services.Delegate.voting_info rpc_ctxt ctxt d

  let consensus_key ctxt pkh = Delegate_services.consensus_key rpc_ctxt ctxt pkh

  let participation ctxt pkh = Delegate_services.participation rpc_ctxt ctxt pkh

  let is_forbidden ?policy ctxt pkh =
    let open Lwt_result_syntax in
    let* ctxt = get_alpha_ctxt ?policy ctxt in
    return (Delegate.is_forbidden_delegate ctxt pkh)

  let stake_for_cycle ?policy ctxt cycle pkh =
    let open Lwt_result_wrap_syntax in
    let* alpha_ctxt = get_alpha_ctxt ?policy ctxt in
    let*@ stakes =
      Protocol.Alpha_context.Stake_distribution.Internal_for_tests
      .get_selected_distribution
        alpha_ctxt
        cycle
    in
    let stake_opt =
      List.assoc ~equal:Signature.Public_key_hash.equal pkh stakes
    in
    let Protocol.Stake_repr.{frozen; weighted_delegated} =
      Option.value ~default:Protocol.Stake_repr.zero stake_opt
    in
    let frozen = Protocol.Tez_repr.to_mutez frozen |> Tez.of_mutez_exn in
    let weighted_delegated =
      Protocol.Tez_repr.to_mutez weighted_delegated |> Tez.of_mutez_exn
    in
    return {frozen; weighted_delegated}
end

module Sc_rollup = struct
  let inbox ctxt =
    Environment.RPC_context.make_call0
      Plugin.RPC.Sc_rollup.S.inbox
      rpc_ctxt
      ctxt
      ()
      ()

  let whitelist ctxt rollup =
    Environment.RPC_context.make_call1
      Plugin.RPC.Sc_rollup.S.whitelist
      rpc_ctxt
      ctxt
      rollup
      ()
      ()

  let commitment ctxt sc_rollup hash =
    Environment.RPC_context.make_call2
      Plugin.RPC.Sc_rollup.S.commitment
      rpc_ctxt
      ctxt
      sc_rollup
      hash
      ()
      ()

  let genesis_info ctxt sc_rollup =
    Environment.RPC_context.make_call1
      Plugin.RPC.Sc_rollup.S.genesis_info
      rpc_ctxt
      ctxt
      sc_rollup
      ()
      ()

  let timeout ctxt sc_rollup staker1 staker2 =
    Environment.RPC_context.make_call3
      Plugin.RPC.Sc_rollup.S.timeout
      rpc_ctxt
      ctxt
      sc_rollup
      staker1
      staker2
      ()
      ()

  let ongoing_games_for_staker ctxt sc_rollup staker =
    Environment.RPC_context.make_call2
      Plugin.RPC.Sc_rollup.S.ongoing_refutation_games
      rpc_ctxt
      ctxt
      sc_rollup
      staker
      ()
      ()
end

type (_, _) tup =
  | T1 : ('a, 'a) tup
  | T2 : ('a, 'a * 'a) tup
  | T3 : ('a, 'a * 'a * 'a) tup
  | TList : int -> ('a, 'a list) tup

let tup_hd : type a r. (a, r) tup -> r -> a =
 fun tup elts ->
  match (tup, elts) with
  | T1, v -> v
  | T2, (v, _) -> v
  | T3, (v, _, _) -> v
  | TList _, v :: _ -> v
  | TList _, [] -> assert false

let tup_n : type a r. (a, r) tup -> int = function
  | T1 -> 1
  | T2 -> 2
  | T3 -> 3
  | TList n -> n

let tup_get : type a r. (a, r) tup -> a list -> r =
 fun tup list ->
  match (tup, list) with
  | T1, [v] -> v
  | T2, [v1; v2] -> (v1, v2)
  | T3, [v1; v2; v3] -> (v1, v2, v3)
  | TList _, l -> l
  | _ -> assert false

let init_gen tup ?rng_state ?commitments ?bootstrap_balances
    ?bootstrap_delegations ?bootstrap_consensus_keys ?consensus_threshold
    ?min_proposal_quorum ?bootstrap_contracts ?level ?cost_per_byte
    ?issuance_weights ?origination_size ?blocks_per_cycle
    ?cycles_per_voting_period ?sc_rollup_arith_pvm_enable
    ?sc_rollup_private_enable ?sc_rollup_riscv_pvm_enable ?dal_enable
    ?zk_rollup_enable ?hard_gas_limit_per_block ?nonce_revelation_threshold ?dal
    ?adaptive_issuance () =
  let open Lwt_result_syntax in
  let n = tup_n tup in
  let*? accounts = Account.generate_accounts ?rng_state n in
  let contracts =
    List.map (fun a -> Alpha_context.Contract.Implicit Account.(a.pkh)) accounts
  in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts
      ?bootstrap_balances
      ?bootstrap_delegations
      ?bootstrap_consensus_keys
      accounts
  in
  let+ blk =
    Block.genesis
      ?commitments
      ?consensus_threshold
      ?min_proposal_quorum
      ?bootstrap_contracts
      ?level
      ?cost_per_byte
      ?issuance_weights
      ?origination_size
      ?blocks_per_cycle
      ?cycles_per_voting_period
      ?sc_rollup_arith_pvm_enable
      ?sc_rollup_private_enable
      ?sc_rollup_riscv_pvm_enable
      ?dal_enable
      ?zk_rollup_enable
      ?hard_gas_limit_per_block
      ?nonce_revelation_threshold
      ?dal
      ?adaptive_issuance
      bootstrap_accounts
  in
  (blk, tup_get tup contracts)

let init_n n = init_gen (TList n)

let init1 = init_gen T1

let init2 = init_gen T2

let init3 = init_gen T3

let create_bootstrap_accounts n =
  let open Result_syntax in
  let* accounts = Account.generate_accounts n in
  let contracts =
    List.map (fun a -> Alpha_context.Contract.Implicit Account.(a.pkh)) accounts
  in
  let bootstrap_accounts = Account.make_bootstrap_accounts accounts in
  return (bootstrap_accounts, contracts)

let init_with_constants_gen tup constants =
  let open Lwt_result_syntax in
  let n = tup_n tup in
  let*? bootstrap_accounts, contracts = create_bootstrap_accounts n in
  let parameters =
    Tezos_protocol_alpha_parameters.Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      constants
  in
  let* blk = Block.genesis_with_parameters parameters in
  return (blk, tup_get tup contracts)

let init_with_constants_n constants n =
  init_with_constants_gen (TList n) constants

let init_with_constants1 = init_with_constants_gen T1

let init_with_constants2 = init_with_constants_gen T2

let init_with_parameters_gen tup parameters =
  let open Lwt_result_syntax in
  let n = tup_n tup in
  let*? bootstrap_accounts, contracts = create_bootstrap_accounts n in
  let parameters = Parameters.{parameters with bootstrap_accounts} in
  let* blk = Block.genesis_with_parameters parameters in
  return (blk, tup_get tup contracts)

let init_with_parameters_n params n = init_with_parameters_gen (TList n) params

let init_with_parameters1 = init_with_parameters_gen T1

let init_with_parameters2 = init_with_parameters_gen T2

let default_raw_context () =
  let open Lwt_result_wrap_syntax in
  let open Tezos_protocol_alpha_parameters in
  let initial_account = Account.new_account () in
  let bootstrap_accounts =
    Account.make_bootstrap_account
      ~balance:(Tez.of_mutez_exn 100_000_000_000L)
      initial_account
  in
  let* constants, _, _ = Block.prepare_initial_context_params () in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts:[bootstrap_accounts]
      ~commitments:[]
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  let protocol_param_key = ["protocol_parameters"] in
  let*! context =
    Tezos_protocol_environment.Context.(
      let empty = Tezos_protocol_environment.Memory_context.empty in
      let*! ctxt = add empty ["version"] (Bytes.of_string "genesis") in
      add ctxt protocol_param_key proto_params)
  in
  let typecheck_smart_contract ctxt script_repr =
    return ((script_repr, None), ctxt)
  in
  let typecheck_smart_rollup ctxt _script_repr = Ok ctxt in
  let*@ e =
    Init_storage.prepare_first_block
      Chain_id.zero
      context
      ~level:0l
      ~timestamp:(Time.Protocol.of_seconds 1643125688L)
      ~predecessor:Block_hash.zero
      ~typecheck_smart_contract
      ~typecheck_smart_rollup
  in
  return e
