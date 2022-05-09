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

let branch = function B b -> b.hash | I i -> (Incremental.predecessor i).hash

let level = function B b -> b.header.shell.level | I i -> Incremental.level i

let get_level ctxt =
  level ctxt |> Raw_level.of_int32 |> Environment.wrap_tzresult

let rpc_ctxt =
  object
    method call_proto_service0
        : 'm 'q 'i 'o.
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t,
            'q,
            'i,
            'o )
          RPC_service.t ->
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
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            Environment.RPC_context.t * 'a,
            'q,
            'i,
            'o )
          RPC_service.t ->
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
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            (Environment.RPC_context.t * 'a) * 'b,
            'q,
            'i,
            'o )
          RPC_service.t ->
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
          ( ([< RPC_service.meth] as 'm),
            Environment.RPC_context.t,
            ((Environment.RPC_context.t * 'a) * 'b) * 'c,
            'q,
            'i,
            'o )
          RPC_service.t ->
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

let get_endorsers ctxt = Plugin.RPC.Validators.get rpc_ctxt ctxt

let get_first_different_endorsers ctxt =
  get_endorsers ctxt >|=? function x :: y :: _ -> (x, y) | _ -> assert false

let get_endorser ctxt =
  get_endorsers ctxt >|=? fun endorsers ->
  let endorser = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd endorsers in
  (endorser.delegate, endorser.slots)

let get_endorser_n ctxt n =
  Plugin.RPC.Validators.get rpc_ctxt ctxt >|=? fun endorsers ->
  let endorser =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.nth endorsers n
  in
  (endorser.delegate, endorser.slots)

let get_endorsing_power_for_delegate ctxt ?levels pkh =
  Plugin.RPC.Validators.get rpc_ctxt ?levels ctxt >>=? fun endorsers ->
  let rec find_slots_for_delegate = function
    | [] -> return 0
    | {Plugin.RPC.Validators.delegate; slots; _} :: t ->
        if Signature.Public_key_hash.equal delegate pkh then
          return (List.length slots)
        else find_slots_for_delegate t
  in
  find_slots_for_delegate endorsers

let get_voting_power = Delegate_services.voting_power rpc_ctxt

let get_total_voting_power = Alpha_services.Voting.total_voting_power rpc_ctxt

let get_bakers ?(filter = fun _x -> true) ctxt =
  Plugin.RPC.Baking_rights.get rpc_ctxt ctxt >|=? fun bakers ->
  List.filter filter bakers
  |> List.map (fun p -> p.Plugin.RPC.Baking_rights.delegate)

let get_baker ctxt ~round =
  get_bakers ~filter:(fun x -> x.round = round) ctxt >>=? fun bakers ->
  (* there is only one baker for a given round *)
  match bakers with [baker] -> return baker | _ -> assert false

let get_first_different_baker baker bakers =
  WithExceptions.Option.get ~loc:__LOC__
  @@ List.find
       (fun baker' -> Signature.Public_key_hash.( <> ) baker baker')
       bakers

let get_first_different_bakers ctxt =
  get_bakers ctxt >|=? function
  | [] -> assert false
  | baker_1 :: other_bakers ->
      (baker_1, get_first_different_baker baker_1 other_bakers)

let get_seed_nonce_hash ctxt =
  let header =
    match ctxt with B {header; _} -> header | I i -> Incremental.header i
  in
  match header.protocol_data.contents.seed_nonce_hash with
  | None -> failwith "No committed nonce"
  | Some hash -> return hash

let get_seed ctxt = Alpha_services.Seed.get rpc_ctxt ctxt

let get_constants ctxt = Alpha_services.Constants.all rpc_ctxt ctxt

let default_test_constants =
  Tezos_protocol_alpha_parameters.Default_parameters.constants_test

let get_baking_reward_fixed_portion ctxt =
  get_constants ctxt
  >>=? fun {Constants.parametric = {baking_reward_fixed_portion; _}; _} ->
  return baking_reward_fixed_portion

let get_bonus_reward ctxt ~endorsing_power =
  get_constants ctxt
  >>=? fun {
             Constants.parametric =
               {baking_reward_bonus_per_slot; consensus_threshold; _};
             _;
           } ->
  let multiplier = max 0 (endorsing_power - consensus_threshold) in
  return Test_tez.(baking_reward_bonus_per_slot *! Int64.of_int multiplier)

let get_endorsing_reward ctxt ~expected_endorsing_power =
  get_constants ctxt
  >>=? fun {Constants.parametric = {endorsing_reward_per_slot; _}; _} ->
  Lwt.return
    (Environment.wrap_tzresult
       Tez.(endorsing_reward_per_slot *? Int64.of_int expected_endorsing_power))

let get_liquidity_baking_subsidy ctxt =
  get_constants ctxt
  >>=? fun {Constants.parametric = {liquidity_baking_subsidy; _}; _} ->
  return liquidity_baking_subsidy

let get_liquidity_baking_cpmm_address ctxt =
  Alpha_services.Liquidity_baking.get_cpmm_address rpc_ctxt ctxt

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

  let get_participation_ema (b : Block.t) =
    Environment.Context.find b.context ["votes"; "participation_ema"]
    >|= function
    | None -> assert false
    | Some bytes -> ok (TzEndian.get_int32 bytes 0)

  let set_participation_ema (b : Block.t) ema =
    let bytes = Bytes.make 4 '\000' in
    TzEndian.set_int32 bytes 0 ema ;
    Environment.Context.add b.context ["votes"; "participation_ema"] bytes
    >|= fun context -> {b with context}

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

  let counter ctxt (contract : Contract.t) =
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.counter"
    | Implicit mgr -> Alpha_services.Contract.counter rpc_ctxt ctxt mgr

  let manager _ (contract : Contract.t) =
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.manager"
    | Implicit pkh -> Account.find pkh

  let is_manager_key_revealed ctxt (contract : Contract.t) =
    match contract with
    | Originated _ -> invalid_arg "Helpers.Context.is_manager_key_revealed"
    | Implicit mgr ->
        Alpha_services.Contract.manager_key rpc_ctxt ctxt mgr >|=? fun res ->
        res <> None

  let delegate ctxt contract =
    Alpha_services.Contract.delegate rpc_ctxt ctxt contract

  let delegate_opt ctxt contract =
    Alpha_services.Contract.delegate_opt rpc_ctxt ctxt contract

  let storage ctxt contract =
    let contract = Contract.Originated contract in
    Alpha_services.Contract.storage rpc_ctxt ctxt contract

  let script ctxt contract =
    let contract = Contract.Originated contract in
    Alpha_services.Contract.script rpc_ctxt ctxt contract
    >>=? fun {code; storage = _} ->
    match Data_encoding.force_decode code with
    | Some v -> return v
    | None -> invalid_arg "Cannot force lazy script"

  let script_hash ctxt contract =
    script ctxt contract >>=? fun script ->
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
    deactivated : bool;
    grace_period : Cycle.t;
    voting_info : Alpha_context.Vote.delegate_info;
  }

  let info ctxt pkh = Delegate_services.info rpc_ctxt ctxt pkh

  let full_balance ctxt pkh = Delegate_services.full_balance rpc_ctxt ctxt pkh

  let current_frozen_deposits ctxt pkh =
    Delegate_services.current_frozen_deposits rpc_ctxt ctxt pkh

  let initial_frozen_deposits ctxt pkh =
    Delegate_services.frozen_deposits rpc_ctxt ctxt pkh

  let staking_balance ctxt pkh =
    Delegate_services.staking_balance rpc_ctxt ctxt pkh

  let frozen_deposits_limit ctxt pkh =
    Delegate_services.frozen_deposits_limit rpc_ctxt ctxt pkh

  let deactivated ctxt pkh = Delegate_services.deactivated rpc_ctxt ctxt pkh

  let voting_info ctxt d = Alpha_services.Delegate.voting_info rpc_ctxt ctxt d

  let participation ctxt pkh = Delegate_services.participation rpc_ctxt ctxt pkh
end

module Tx_rollup = struct
  let state ctxt tx_rollup = Tx_rollup_services.state rpc_ctxt ctxt tx_rollup

  let inbox ctxt tx_rollup = Tx_rollup_services.inbox rpc_ctxt ctxt tx_rollup

  let commitment ctxt tx_rollup =
    Tx_rollup_services.commitment rpc_ctxt ctxt tx_rollup
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

let init_gen tup ?rng_state ?commitments ?(initial_balances = [])
    ?consensus_threshold ?min_proposal_quorum ?bootstrap_contracts
    ?bootstrap_delegations ?level ?cost_per_byte ?liquidity_baking_subsidy
    ?endorsing_reward_per_slot ?baking_reward_bonus_per_slot
    ?baking_reward_fixed_portion ?origination_size ?blocks_per_cycle
    ?cycles_per_voting_period ?tx_rollup_enable ?tx_rollup_sunset_level
    ?tx_rollup_origination_size ?sc_rollup_enable () =
  let n = tup_n tup in
  let accounts =
    Account.generate_accounts
      ?rng_state
      ~initial_balances
      ?bootstrap_delegations
      n
  in
  let contracts =
    List.map
      (fun (a, _, _) -> Alpha_context.Contract.Implicit Account.(a.pkh))
      accounts
  in
  Block.genesis
    ?commitments
    ?consensus_threshold
    ?min_proposal_quorum
    ?bootstrap_contracts
    ?level
    ?cost_per_byte
    ?liquidity_baking_subsidy
    ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot
    ?baking_reward_fixed_portion
    ?origination_size
    ?blocks_per_cycle
    ?cycles_per_voting_period
    ?tx_rollup_enable
    ?tx_rollup_sunset_level
    ?tx_rollup_origination_size
    ?sc_rollup_enable
    accounts
  >|=? fun blk -> (blk, tup_get tup contracts)

let init_n n = init_gen (TList n)

let init1 = init_gen T1

let init2 = init_gen T2

let init3 = init_gen T3

let init_with_constants_gen tup constants =
  let n = tup_n tup in
  let accounts = Account.generate_accounts n in
  let contracts =
    List.map
      (fun (a, _, _) -> Alpha_context.Contract.Implicit Account.(a.pkh))
      accounts
  in
  let open Tezos_protocol_alpha_parameters in
  let bootstrap_accounts =
    List.map
      (fun (acc, tez, delegate_to) ->
        Default_parameters.make_bootstrap_account
          (acc.Account.pkh, acc.Account.pk, tez, delegate_to))
      accounts
  in
  let parameters =
    Default_parameters.parameters_of_constants ~bootstrap_accounts constants
  in
  Block.genesis_with_parameters parameters >|=? fun blk ->
  (blk, tup_get tup contracts)

let init_with_constants_n consts n = init_with_constants_gen (TList n) consts

let init_with_constants1 = init_with_constants_gen T1

let init_with_constants2 = init_with_constants_gen T2

let default_raw_context () =
  let initial_accounts =
    Account.generate_accounts ~initial_balances:[100_000_000_000L] 1
  in
  let open Tezos_protocol_alpha_parameters in
  let bootstrap_accounts =
    List.map
      (fun (Account.{pk; pkh; _}, amount, delegate_to) ->
        Default_parameters.make_bootstrap_account (pkh, pk, amount, delegate_to))
      initial_accounts
  in
  Block.prepare_initial_context_params initial_accounts
  >>=? fun (constants, _, _) ->
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~commitments:[]
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  let protocol_param_key = ["protocol_parameters"] in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)
  >>= fun context ->
  let typecheck ctxt script_repr = return ((script_repr, None), ctxt) in
  Init_storage.prepare_first_block
    context
    ~level:0l
    ~timestamp:(Time.Protocol.of_seconds 1643125688L)
    ~typecheck
  >>= fun e -> Lwt.return @@ Environment.wrap_tzresult e
