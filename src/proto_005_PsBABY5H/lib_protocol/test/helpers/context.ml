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

type t =
  | B of Block.t
  | I of Incremental.t

let branch = function
  | B b -> b.hash
  | I i -> (Incremental.predecessor i).hash

let level = function
  | B b -> b.header.shell.level
  | I i -> (Incremental.level i)

let get_level ctxt =
  level ctxt
  |> Raw_level.of_int32
  |> Environment.wrap_error
  |> Lwt.return

let rpc_ctxt = object
  method call_proto_service0 :
    'm 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, Environment.RPC_context.t, Environment.RPC_context.t, 'q, 'i, 'o) RPC_service.t ->
    t -> 'q -> 'i -> 'o tzresult Lwt.t =
    fun s pr q i ->
      match pr with
      | B b -> Block.rpc_ctxt#call_proto_service0 s b q i
      | I b -> Incremental.rpc_ctxt#call_proto_service0 s b q i
  method call_proto_service1 :
    'm 'a 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, Environment.RPC_context.t, Environment.RPC_context.t * 'a, 'q, 'i, 'o) RPC_service.t ->
    t -> 'a -> 'q -> 'i -> 'o tzresult Lwt.t =
    fun s pr a q i ->
      match pr with
      | B bl -> Block.rpc_ctxt#call_proto_service1 s bl a q i
      | I bl -> Incremental.rpc_ctxt#call_proto_service1 s bl a q i
  method call_proto_service2 :
    'm 'a 'b 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, Environment.RPC_context.t, (Environment.RPC_context.t * 'a) * 'b, 'q, 'i, 'o) RPC_service.t ->
    t -> 'a -> 'b -> 'q -> 'i -> 'o tzresult Lwt.t =
    fun s pr a b q i ->
      match pr with
      | B bl -> Block.rpc_ctxt#call_proto_service2 s bl a b q i
      | I bl -> Incremental.rpc_ctxt#call_proto_service2 s bl a b q i
  method call_proto_service3 :
    'm 'a 'b 'c 'q 'i 'o.
    ([< RPC_service.meth ] as 'm, Environment.RPC_context.t, ((Environment.RPC_context.t * 'a) * 'b) * 'c, 'q, 'i, 'o) RPC_service.t ->
    t -> 'a -> 'b -> 'c -> 'q -> 'i -> 'o tzresult Lwt.t =
    fun s pr a b c q i ->
      match pr with
      | B bl -> Block.rpc_ctxt#call_proto_service3 s bl a b c q i
      | I bl -> Incremental.rpc_ctxt#call_proto_service3 s bl a b c q i
end

let get_endorsers ctxt =
  Alpha_services.Delegate.Endorsing_rights.get rpc_ctxt ctxt

let get_endorser ctxt =
  Alpha_services.Delegate.Endorsing_rights.get rpc_ctxt ctxt >>=? fun endorsers ->
  let endorser = List.hd endorsers in
  return (endorser.delegate, endorser.slots)

let get_bakers ctxt =
  Alpha_services.Delegate.Baking_rights.get
    ~max_priority:256
    rpc_ctxt ctxt >>=? fun bakers ->
  return (List.map
            (fun p -> p.Alpha_services.Delegate.Baking_rights.delegate)
            bakers)

let get_seed_nonce_hash ctxt =
  let header =
    match ctxt with
    | B { header ; _ } -> header
    | I i -> Incremental.header i in
  match header.protocol_data.contents.seed_nonce_hash with
  | None -> failwith "No committed nonce"
  | Some hash -> return hash

let get_seed ctxt = Alpha_services.Seed.get rpc_ctxt ctxt

let get_constants ctxt =
  Alpha_services.Constants.all rpc_ctxt ctxt

let get_minimal_valid_time ctxt ~priority ~endorsing_power =
  Alpha_services.Delegate.Minimal_valid_time.get rpc_ctxt ctxt priority endorsing_power


let get_baking_reward ctxt ~priority ~endorsing_power =
  get_constants ctxt >>=? fun Constants.
    { parametric = { block_reward ; endorsers_per_block ; _ } ; _  } ->
  let prio_factor_denominator = Int64.(succ (of_int priority)) in
  let endo_factor_numerator = Int64.of_int (8 + 2 * endorsing_power / endorsers_per_block) in
  let endo_factor_denominator = 10L in
  Lwt.return
    Test_tez.Tez.(
      block_reward *? endo_factor_numerator >>? fun val1 ->
      val1 /? endo_factor_denominator >>? fun val2 ->
      val2 /? prio_factor_denominator)

let get_endorsing_reward ctxt ~priority ~endorsing_power =
  get_constants ctxt >>=? fun Constants.
    { parametric = { endorsement_reward ; _ } ; _  } ->
  let open Test_utils in
  Test_tez.Tez.(
    endorsement_reward /? Int64.(succ (of_int priority)) >>?= fun reward_per_slot ->
    reward_per_slot *? (Int64.of_int endorsing_power) >>?= fun reward ->
    return reward)


(* Voting *)

module Vote = struct

  let get_ballots ctxt =
    Alpha_services.Voting.ballots rpc_ctxt ctxt

  let get_ballot_list ctxt =
    Alpha_services.Voting.ballot_list rpc_ctxt ctxt

  let get_voting_period ctxt =
    Alpha_services.Helpers.current_level rpc_ctxt ~offset:1l ctxt >>=? fun l ->
    return l.voting_period

  let get_voting_period_position ctxt =
    Alpha_services.Helpers.current_level rpc_ctxt ~offset:1l ctxt >>=? fun l ->
    return l.voting_period_position

  let get_current_period_kind ctxt =
    Alpha_services.Voting.current_period_kind rpc_ctxt ctxt

  let get_current_quorum ctxt =
    Alpha_services.Voting.current_quorum rpc_ctxt ctxt

  let get_listings ctxt =
    Alpha_services.Voting.listings rpc_ctxt ctxt

  let get_proposals ctxt =
    Alpha_services.Voting.proposals rpc_ctxt ctxt

  let get_current_proposal ctxt =
    Alpha_services.Voting.current_proposal rpc_ctxt ctxt

  let get_protocol (b:Block.t) =
    Tezos_protocol_environment.Context.get b.context ["protocol"] >>= function
    | None -> assert false
    | Some p -> Lwt.return (Protocol_hash.of_bytes_exn p)

  let get_participation_ema (b:Block.t) =
    Environment.Context.get b.context ["votes"; "participation_ema"] >>= function
    | None -> assert false
    | Some bytes -> return (MBytes.get_int32 bytes 0)

  let set_participation_ema (b:Block.t) ema =
    let bytes = MBytes.make 4 '\000' in
    MBytes.set_int32 bytes 0 ema ;
    Environment.Context.set b.context
      ["votes"; "participation_ema"] bytes >>= fun context ->
    Lwt.return { b with context }

end

module Contract = struct

  let pp = Alpha_context.Contract.pp

  let pkh c = Alpha_context.Contract.is_implicit c |> function
    | Some p -> return p
    | None -> failwith "pkh: only for implicit contracts"

  type balance_kind = Main | Deposit | Fees | Rewards

  let balance ?(kind = Main) ctxt contract =
    begin match kind with
      | Main ->
          Alpha_services.Contract.balance rpc_ctxt ctxt contract
      | _ ->
          match Alpha_context.Contract.is_implicit contract with
          | None ->
              invalid_arg
                "get_balance: no frozen accounts for an originated contract."
          | Some pkh ->
              Alpha_services.Delegate.frozen_balance_by_cycle
                rpc_ctxt ctxt pkh >>=? fun map ->
              Lwt.return @@
              Cycle.Map.fold
                (fun _cycle { Delegate.deposit ; fees ; rewards } acc ->
                   acc >>?fun acc ->
                   match kind with
                   | Deposit -> Test_tez.Tez.(acc +? deposit)
                   | Fees -> Test_tez.Tez.(acc +? fees)
                   | Rewards ->  Test_tez.Tez.(acc +? rewards)
                   | _ -> assert false)
                map
                (Ok Tez.zero)
    end

  let counter ctxt contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.counter"
    | Some mgr ->
        Alpha_services.Contract.counter rpc_ctxt ctxt mgr

  let manager _ contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.manager"
    | Some pkh -> Account.find pkh

  let is_manager_key_revealed ctxt contract =
    match Contract.is_implicit contract with
    | None -> invalid_arg "Helpers.Context.is_manager_key_revealed"
    | Some mgr ->
        Alpha_services.Contract.manager_key rpc_ctxt ctxt mgr >>=? fun res ->
        return (res <> None)

  let delegate ctxt contract =
    Alpha_services.Contract.delegate rpc_ctxt ctxt contract

  let delegate_opt ctxt contract =
    Alpha_services.Contract.delegate_opt rpc_ctxt ctxt contract

end

module Delegate = struct

  type info = Delegate_services.info = {
    balance: Tez.t ;
    frozen_balance: Tez.t ;
    frozen_balance_by_cycle: Delegate.frozen_balance Cycle.Map.t ;
    staking_balance: Tez.t ;
    delegated_contracts: Contract_repr.t list ;
    delegated_balance: Tez.t ;
    deactivated: bool ;
    grace_period: Cycle.t ;
  }

  let info ctxt pkh =
    Alpha_services.Delegate.info rpc_ctxt ctxt pkh

end

let init
    ?endorsers_per_block
    ?with_commitments
    ?(initial_balances = [])
    ?initial_endorsers
    ?min_proposal_quorum
    n =
  let accounts = Account.generate_accounts ~initial_balances n in
  let contracts = List.map (fun (a, _) ->
      Alpha_context.Contract.implicit_contract Account.(a.pkh)) accounts in
  Block.genesis
    ?endorsers_per_block
    ?with_commitments
    ?initial_endorsers
    ?min_proposal_quorum
    accounts >>=? fun blk ->
  return (blk, contracts)
