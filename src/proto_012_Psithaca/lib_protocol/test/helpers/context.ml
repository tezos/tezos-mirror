(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let get_seed_nonce_hash ctxt =
  let header =
    match ctxt with B {header; _} -> header | I i -> Incremental.header i
  in
  match header.protocol_data.contents.seed_nonce_hash with
  | None -> failwith "No committed nonce"
  | Some hash -> return hash

let get_seed ctxt = Alpha_services.Seed.get rpc_ctxt ctxt

let get_constants ctxt = Alpha_services.Constants.all rpc_ctxt ctxt

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
end

module Contract = struct
  let pp = Alpha_context.Contract.pp

  let equal a b = Alpha_context.Contract.compare a b = 0

  let pkh c =
    Alpha_context.Contract.is_implicit c |> function
    | Some p -> return p
    | None -> failwith "pkh: only for implicit contracts"

  let balance ctxt contract =
    Alpha_services.Contract.balance rpc_ctxt ctxt contract

  let counter ctxt contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.counter"
    | Some mgr -> Alpha_services.Contract.counter rpc_ctxt ctxt mgr

  let manager _ contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.manager"
    | Some pkh -> Account.find pkh

  let is_manager_key_revealed ctxt contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.is_manager_key_revealed"
    | Some mgr ->
        Alpha_services.Contract.manager_key rpc_ctxt ctxt mgr >|=? fun res ->
        res <> None

  let delegate ctxt contract =
    Alpha_services.Contract.delegate rpc_ctxt ctxt contract

  let delegate_opt ctxt contract =
    Alpha_services.Contract.delegate_opt rpc_ctxt ctxt contract

  let storage ctxt contract =
    Alpha_services.Contract.storage rpc_ctxt ctxt contract

  let script ctxt contract =
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
    voting_power : int32;
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

  let participation ctxt pkh = Delegate_services.participation rpc_ctxt ctxt pkh
end

let init ?rng_state ?commitments ?(initial_balances = []) ?consensus_threshold
    ?min_proposal_quorum ?bootstrap_contracts ?level ?cost_per_byte
    ?liquidity_baking_subsidy ?endorsing_reward_per_slot
    ?baking_reward_bonus_per_slot ?baking_reward_fixed_portion ?origination_size
    ?blocks_per_cycle n =
  let accounts = Account.generate_accounts ?rng_state ~initial_balances n in
  let contracts =
    List.map
      (fun (a, _) -> Alpha_context.Contract.implicit_contract Account.(a.pkh))
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
    accounts
  >|=? fun blk -> (blk, contracts)

let init_with_constants constants n =
  let accounts = Account.generate_accounts n in
  let contracts =
    List.map
      (fun (a, _) -> Alpha_context.Contract.implicit_contract Account.(a.pkh))
      accounts
  in
  let open Tezos_protocol_012_Psithaca_parameters in
  let bootstrap_accounts =
    List.map
      (fun (acc, tez) ->
        Default_parameters.make_bootstrap_account
          (acc.Account.pkh, acc.Account.pk, tez))
      accounts
  in
  let parameters =
    Default_parameters.parameters_of_constants ~bootstrap_accounts constants
  in
  Block.genesis_with_parameters parameters >|=? fun blk -> (blk, contracts)
