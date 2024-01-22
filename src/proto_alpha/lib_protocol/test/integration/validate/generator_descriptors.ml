(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Validate_helpers

type dbl_attestation_state = {
  temporary : (Block.t * Block.t) option;
  slashable_preattestations :
    (Kind.preattestation operation * Kind.preattestation operation) list;
  slashable_attestations :
    (Kind.attestation operation * Kind.attestation operation) list;
}

type state = {
  block : Block.t;
  pred : Block.t option;
  bootstraps : public_key_hash list;
  delegates : (public_key_hash * public_key_hash option) list;
  voters : Contract.t list;
  seed_nonce_to_reveal : (Raw_level.t * Nonce_hash.t) list;
  commitments : secret_account list;
  protocol_hashes : Protocol_hash.t list;
  slashable_bakes : (block_header * block_header) list;
  vdf : bool;
  dbl_attestation : dbl_attestation_state;
  manager : Manager.infos;
}

let init_manager_state bootstraps block =
  let open Manager in
  let ctxt =
    {
      block;
      bootstraps;
      originated_contract = None;
      sc_rollup = None;
      zk_rollup = None;
    }
  in
  let accounts =
    {sources = []; dest = None; del = None; sc = None; zk = None}
  in
  {ctxt; accounts; flags = all_enabled}

let init_dbl_attestation_state =
  {
    temporary = None;
    slashable_preattestations = [];
    slashable_attestations = [];
  }

(** Initialize the state according to [state] initialisation
    for each operation kind.

    When adding a new operation kind, if such an initialization is
    required, it should occur here. *)
let init_state block ~voters ~(bootstraps : Contract.t list) =
  let bootstraps =
    List.map
      (function Contract.Implicit pkh -> pkh | _ -> assert false)
      bootstraps
  in
  {
    block;
    pred = None;
    bootstraps;
    delegates = List.map (fun pkh -> (pkh, None)) bootstraps;
    voters;
    seed_nonce_to_reveal = [];
    commitments = [];
    protocol_hashes = [];
    slashable_bakes = [];
    vdf = false;
    dbl_attestation = init_dbl_attestation_state;
    manager = init_manager_state bootstraps block;
  }

type cycle_index = On of int | From of int

type descriptor = {
  parameters : Parameters.t -> Parameters.t;
  required_cycle : Parameters.t -> int;
  required_block : Parameters.t -> int;
  prelude :
    cycle_index * (state -> (packed_operation list * state) tzresult Lwt.t);
  opt_prelude :
    (cycle_index * (state -> (packed_operation list * state) tzresult Lwt.t))
    option;
  candidates_generator : state -> packed_operation list tzresult Lwt.t;
}

(** Each voting period lasts a whole cycle in the generation of valid operations. *)
let voting_context_params params =
  let cycles_per_voting_period = 1l in
  let constants = Parameters.{params.constants with cycles_per_voting_period} in
  {params with constants}

let ballot_exploration_prelude state =
  let open Lwt_result_syntax in
  let* ctxt =
    let+ i = Incremental.begin_construction state.block in
    Incremental.alpha_ctxt i
  in
  let blocks_per_cycle = Alpha_context.Constants.blocks_per_cycle ctxt in
  let rem =
    Int32.rem state.block.Block.header.Block_header.shell.level blocks_per_cycle
  in
  if rem = 0l then
    match state.voters with
    | voter :: voters ->
        let* prop = Op.proposals (B state.block) voter [get_n protos 0] in
        let* props =
          List.map_es
            (fun voter ->
              Op.proposals (B state.block) voter [Protocol_hash.zero])
            voters
        in
        return (prop :: props, state)
    | _ -> assert false
  else return ([], state)

let activate_descriptor =
  let open Lwt_result_syntax in
  {
    parameters =
      (fun params ->
        let commitments =
          List.map
            (fun {blinded_public_key_hash; amount; _} ->
              Commitment.{blinded_public_key_hash; amount})
            secrets
        in
        {params with commitments});
    required_cycle = (fun _params -> 1);
    required_block = (fun _params -> 0);
    prelude =
      (On 1, fun state -> return ([], {state with commitments = secrets}));
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let gen s =
          Op.activation (B state.block) (Ed25519 s.account) s.activation_code
        in
        List.map_es gen state.commitments);
  }

(** During the first voting period in the setup of valid operations generations,
    a proposal must win the proposal period -- hence [ballot_exploration_prelude]
    takes place during the first cycle. *)
let ballot_exploration_descriptor =
  {
    parameters = voting_context_params;
    required_cycle =
      (fun params -> Int32.to_int params.constants.cycles_per_voting_period);
    required_block = (fun _params -> 0);
    prelude = (On 1, ballot_exploration_prelude);
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen contract =
          let* voting_period_info =
            Context.Vote.get_current_period (B state.block)
          in
          assert (voting_period_info.voting_period.kind = Exploration) ;
          let ballot = pick_one ballots in
          Op.ballot (B state.block) contract Protocol_hash.zero ballot
        in
        List.map_es gen state.voters);
  }

let proposal_descriptor =
  let open Lwt_result_syntax in
  {
    parameters = voting_context_params;
    required_cycle = (fun _ -> 0);
    required_block = (fun _ -> 0);
    prelude =
      (On 0, fun state -> return ([], {state with protocol_hashes = protos}));
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen contract =
          let* voting_period_info =
            Context.Vote.get_current_period (B state.block)
          in
          assert (voting_period_info.voting_period.kind = Proposal) ;
          Op.proposals (B state.block) contract [Protocol_hash.zero]
        in
        List.map_es gen state.voters);
  }

(** [Promotion] is the 4th voting period, it requires 3 voting period
    to last and be successful. [voting_context_params] set a
    voting_period to 1 cycle. To generate a [Ballot] for this
    promotion period:

    - the first period should conclude in a proposal wining -- 3 cycles
    before generating the [Ballot], the proposal period must succeed:[
    ballot_exploration_prelude],

    - the exploration must conclude in a supermajority for this
    proposal -- 2 cycles before generating the [Ballot], the
    exploration period must succeed., and

    - the cooldown must last -- 1 cycle before generating the
    [Ballot]. *)
let ballot_promotion_descriptor =
  {
    parameters = voting_context_params;
    required_cycle =
      (fun params -> 3 * Int32.to_int params.constants.cycles_per_voting_period);
    required_block = (fun _ -> 0);
    prelude = (On 3, ballot_exploration_prelude);
    opt_prelude =
      Some
        ( On 2,
          fun state ->
            let open Lwt_result_syntax in
            let* ctxt =
              let+ incr = Incremental.begin_construction state.block in
              Incremental.alpha_ctxt incr
            in
            let blocks_per_cycle =
              Alpha_context.Constants.blocks_per_cycle ctxt
            in
            let rem =
              Int32.rem
                state.block.Block.header.Block_header.shell.level
                blocks_per_cycle
            in
            if rem = 0l then
              let* ops =
                List.map_es
                  (fun voter ->
                    Op.ballot (B state.block) voter Protocol_hash.zero Vote.Yay)
                  state.voters
              in
              return (ops, state)
            else return ([], state) );
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen contract =
          let* voting_period_info =
            Context.Vote.get_current_period (B state.block)
          in
          assert (voting_period_info.voting_period.kind = Promotion) ;
          let ballot = Stdlib.List.hd ballots in
          Op.ballot (B state.block) contract Protocol_hash.zero ballot
        in
        List.map_es gen state.voters);
  }

let seed_nonce_descriptor =
  {
    parameters =
      (fun params ->
        assert (params.constants.blocks_per_cycle > 3l) ;
        let blocks_per_commitment =
          Int32.(div params.constants.blocks_per_cycle 3l)
        in
        let constants = {params.constants with blocks_per_commitment} in
        {params with constants});
    required_cycle = (fun _ -> 1);
    required_block = (fun _ -> 0);
    prelude =
      ( On 1,
        fun state ->
          let open Lwt_result_syntax in
          let b = state.block in
          let* seed_nonce_to_reveal =
            match
              b.Block.header.Block_header.protocol_data.contents.seed_nonce_hash
            with
            | None -> return state.seed_nonce_to_reveal
            | Some nonce_hash ->
                let level =
                  Raw_level.of_int32_exn b.Block.header.Block_header.shell.level
                in
                return ((level, nonce_hash) :: state.seed_nonce_to_reveal)
          in
          return ([], {state with seed_nonce_to_reveal}) );
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen (level, nonce_hash) =
          assert (List.length state.seed_nonce_to_reveal >= 3) ;
          let nonce =
            WithExceptions.Option.to_exn ~none:Not_found
            @@ Registered_nonces.get nonce_hash
          in
          return (Op.seed_nonce_revelation (B state.block) level nonce)
        in
        List.map_es gen state.seed_nonce_to_reveal);
  }

(** The heads on which two slashable attestations or preattestation
   should be made are from the previous level. Hence, the temporary
   field of a double_evidence_state is used to transmit them to the
   next level in order to make the slashable operations. *)
let register_temporary ba bb state : (Block.t * Block.t) option * state =
  let pred_forks = state.dbl_attestation.temporary in
  let temporary = Some (ba, bb) in
  let dbl_attestation = {state.dbl_attestation with temporary} in
  (pred_forks, {state with dbl_attestation})

(** During the slashable period, at each level, two different heads
   for the same round are baked by the same baker. At the next level,
   a delegate that either preattests or attests both heads makes a
   pair of slashable pre- or attestations.

   The pair of heads is placed in the temporary of the
   double_evidence_state. If a pair of heads was already in this
   field, hence they were baked at the previous level.

   Consequently, two pairs of slashable operations: two attestations
   and two preattestation, can be made by two distinct attestations. Each
   pair is ordered in operation_hash order. Consequently, each pair
   can appear in a denunciation operation and will be valid. *)
let dbl_attestation_prelude state =
  let open Lwt_result_syntax in
  let* head_A = Block.bake ~policy:(By_round 0) state.block in
  let* addr = pick_addr_attester (B state.block) in
  let ctr = Contract.Implicit addr in
  let* operation = Op.transaction (B state.block) ctr ctr Tez.one_mutez in
  let* head_B = Block.bake ~policy:(By_round 0) state.block ~operation in
  let heads, state = register_temporary head_A head_B state in
  match heads with
  | None -> return ([], state)
  | Some (b1, b2) ->
      let* delegate1, delegate2 = pick_two_attesters (B b1) in
      let* op1 = Op.raw_preattestation ~delegate:delegate1 b1 in
      let* op2 = Op.raw_preattestation ~delegate:delegate1 b2 in
      let op1, op2 =
        let comp =
          Operation_hash.compare (Operation.hash op1) (Operation.hash op2)
        in
        assert (comp <> 0) ;
        if comp < 0 then (op1, op2) else (op2, op1)
      in
      let slashable_preattestations =
        (op1, op2) :: state.dbl_attestation.slashable_preattestations
      in
      let* op3 = Op.raw_attestation ~delegate:delegate2 b1 in
      let* op4 = Op.raw_attestation ~delegate:delegate2 b2 in
      let op3, op4 =
        let comp =
          Operation_hash.compare (Operation.hash op3) (Operation.hash op4)
        in
        assert (comp <> 0) ;
        if comp < 0 then (op3, op4) else (op4, op3)
      in
      let slashable_attestations =
        (op3, op4) :: state.dbl_attestation.slashable_attestations
      in
      let dbl_attestation =
        {
          state.dbl_attestation with
          slashable_preattestations;
          slashable_attestations;
        }
      in
      return ([], {state with dbl_attestation})

let double_consensus_descriptor =
  {
    parameters = Fun.id;
    required_cycle = (fun _params -> 2);
    required_block = (fun _ -> 0);
    prelude = (From 2, dbl_attestation_prelude);
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen_dbl_pre (op1, op2) =
          Op.double_preattestation (Context.B state.block) op1 op2
        in
        let gen_dbl_end (op1, op2) =
          Op.double_attestation (Context.B state.block) op1 op2
        in
        let candidates_pre =
          List.map gen_dbl_pre state.dbl_attestation.slashable_preattestations
        in
        let candidates_end =
          List.map gen_dbl_end state.dbl_attestation.slashable_attestations
        in
        return (candidates_pre @ candidates_end));
  }

let double_baking_descriptor =
  {
    parameters = Fun.id;
    required_cycle = (fun _params -> Constants_repr.max_slashing_period);
    required_block = (fun _ -> 0);
    prelude =
      ( From 2,
        fun state ->
          let open Lwt_result_syntax in
          let order_block_header bh1 bh2 =
            let hash1 = Block_header.hash bh1 in
            let hash2 = Block_header.hash bh2 in
            let c = Block_hash.compare hash1 hash2 in
            if c < 0 then (bh1, bh2) else (bh2, bh1)
          in
          let* ctxt =
            let+ incr = Incremental.begin_construction state.block in
            Incremental.alpha_ctxt incr
          in
          let blocks_per_cycle =
            Alpha_context.Constants.blocks_per_cycle ctxt
          in
          let rem =
            Int32.rem
              state.block.Block.header.Block_header.shell.level
              blocks_per_cycle
          in
          if rem = 0l then return ([], state)
          else
            let* baker1, _baker2 =
              Context.get_first_different_bakers (B state.block)
            in
            let* addr = pick_addr_attester (B state.block) in
            let ctr = Contract.Implicit addr in
            let* operation =
              Op.transaction (B state.block) ctr ctr Tez.one_mutez
            in
            let* ba =
              Block.bake ~policy:(By_account baker1) ~operation state.block
            in
            let* bb = Block.bake ~policy:(By_account baker1) state.block in
            let ba, bb = order_block_header ba.Block.header bb.Block.header in
            let slashable_bakes = (ba, bb) :: state.slashable_bakes in
            return ([], {state with slashable_bakes}) );
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen (bh1, bh2) =
          return (Op.double_baking (B state.block) bh1 bh2)
        in
        List.map_es gen state.slashable_bakes);
  }

(** A drain delegate operation is valid when, preserved_cycle before, the
    delegate has updated its key. This key must then has enough fund in order to
    be revealed.

   At the first level of preserved cycle in the past, the key is funded by a
   bootstrap account. At the second level, it reveals and at the third the
   delegate updates its key to this key. *)
let drain_delegate_prelude state =
  let open Lwt_result_syntax in
  let* ctxt =
    let+ incr = Incremental.begin_construction state.block in
    Incremental.alpha_ctxt incr
  in
  let blocks_per_cycle = Alpha_context.Constants.blocks_per_cycle ctxt in
  let rem =
    Int32.rem state.block.Block.header.Block_header.shell.level blocks_per_cycle
  in
  if rem = 0l then
    (* Create (n / 2) consensus keys *)
    let delegates =
      List.mapi
        (fun i -> function
          | (delegate, None) as del ->
              if i mod 2 = 0 then
                let acc = Account.new_account () in
                (delegate, Some acc.pkh)
              else del
          | del -> del (* should not happen but apparently does...*))
        state.delegates
    in
    let dels =
      List.filter_map
        (function _del, None -> None | del, Some ck -> Some (del, ck))
        delegates
    in
    let* ops =
      List.fold_left_es
        (fun ops (del, ck) ->
          let* {Account.pk; _} = Account.find ck in
          let* op =
            Op.update_consensus_key (B state.block) (Contract.Implicit del) pk
          in
          return (op :: ops))
        []
        dels
    in
    let state = {state with delegates} in
    return (ops, state)
  else return ([], state)

let drain_delegate_descriptor =
  {
    parameters = Fun.id;
    required_cycle = (fun params -> params.constants.consensus_rights_delay + 1);
    required_block = (fun _ -> 0);
    prelude =
      ( On (init_params.constants.consensus_rights_delay + 1),
        drain_delegate_prelude );
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let gen (delegate, consensus_key_opt) =
          let open Lwt_result_syntax in
          match consensus_key_opt with
          | None -> return_none
          | Some consensus_key ->
              let* op =
                Op.drain_delegate
                  (B state.block)
                  ~consensus_key
                  ~delegate
                  ~destination:consensus_key
              in
              return_some op
        in
        List.filter_map_es gen state.delegates);
  }

let vdf_revelation_descriptor =
  let open Lwt_result_syntax in
  {
    parameters =
      (fun params ->
        {params with constants = {params.constants with vdf_difficulty = 750L}});
    required_cycle = (fun _ -> 1);
    required_block =
      (fun params -> Int32.to_int params.constants.nonce_revelation_threshold);
    prelude = (On 2, fun state -> return ([], {state with vdf = true}));
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let* seed_status = Context.get_seed_computation (B state.block) in
        let* csts = Context.get_constants (B state.block) in
        match seed_status with
        | Nonce_revelation_stage | Computation_finished -> assert false
        | Vdf_revelation_stage info ->
            (* generate the VDF discriminant and challenge *)
            let discriminant, challenge =
              Alpha_context.Seed.generate_vdf_setup
                ~seed_discriminant:info.seed_discriminant
                ~seed_challenge:info.seed_challenge
            in
            (* compute the VDF solution (the result and the proof ) *)
            let solution =
              (* generate the result and proof *)
              Environment.Vdf.prove
                discriminant
                challenge
                csts.parametric.vdf_difficulty
            in
            return [Op.vdf_revelation (B state.block) solution]);
  }

let preattestation_descriptor =
  let open Lwt_result_syntax in
  {
    parameters = Fun.id;
    required_cycle = (fun _ -> 1);
    required_block = (fun _ -> 1);
    prelude = (On 1, fun state -> return ([], state));
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen (delegate, ck_opt) =
          let* slots_opt = Context.get_attester_slot (B state.block) delegate in
          let delegate = Option.value ~default:delegate ck_opt in
          match slots_opt with
          | None -> return_none
          | Some slots -> (
              match slots with
              | [] -> return_none
              | _ :: _ ->
                  let* op = Op.preattestation ~delegate state.block in
                  return_some op)
        in
        List.filter_map_es gen state.delegates);
  }

let attestation_descriptor =
  let open Lwt_result_syntax in
  {
    parameters = Fun.id;
    required_cycle = (fun _ -> 1);
    required_block = (fun _ -> 1);
    prelude = (On 1, fun state -> return ([], state));
    opt_prelude = None;
    candidates_generator =
      (fun state ->
        let open Lwt_result_syntax in
        let gen (delegate, ck_opt) =
          let* slots_opt = Context.get_attester_slot (B state.block) delegate in
          let delegate = Option.value ~default:delegate ck_opt in
          match slots_opt with
          | None -> return_none
          | Some slots -> (
              match slots with
              | [] -> return_none
              | _ :: _ ->
                  let* op = Op.attestation ~delegate state.block in
                  return_some op)
        in
        List.filter_map_es gen state.delegates);
  }

module Manager = Manager_operation_helpers

let required_nb_account = 7

(** Convertion from [manager_state] to a {! Manager_operation_helper.infos}. *)
let infos_of_state source block infos : Manager.infos =
  let open Manager in
  let ({ctxt; accounts; flags} : infos) = infos in
  let ctxt : ctxt = {ctxt with block} in
  let accounts = {accounts with sources = [source]} in
  {ctxt; accounts; flags}

(** Updating a [manager_state] according to a  {! Manager_operation_helper.infos}. *)
let update_state_with_infos {Manager.ctxt; accounts; flags}
    {Manager.ctxt = ctxt2; accounts = accounts2; _} =
  let ctxt =
    {
      ctxt with
      originated_contract = ctxt2.originated_contract;
      sc_rollup = ctxt2.sc_rollup;
      zk_rollup = ctxt2.zk_rollup;
    }
  in
  let accounts =
    {
      accounts with
      dest = accounts2.dest;
      del = accounts2.del;
      sc = accounts2.sc;
      zk = accounts2.zk;
    }
  in
  {Manager.ctxt; accounts; flags}

(** According to a [Manager.infos] and a block [b], create and fund
   the required contracts and accounts on [b]. In additions to the
   initiation performed by {! Manager_operation_helper.init_infos}, it
   registers a list of funded sources. *)
let manager_prelude (infos : Manager.infos) b =
  let open Lwt_result_syntax in
  let nb_sources = List.length infos.ctxt.bootstraps in
  let* ops_by_bootstrap =
    List.map_es
      (fun bootstrap ->
        let bootstrap = Contract.Implicit bootstrap in
        let* counter = Context.Contract.counter (B b) bootstrap in
        return (bootstrap, counter, []))
      (List.take_n nb_sources infos.ctxt.bootstraps)
  in
  let add bootstrap counter ops ops_by_bootstrap =
    List.map
      (fun (bootstrap', counter', ops') ->
        if bootstrap' = bootstrap then
          (bootstrap, Manager_counter.succ counter, ops)
        else (bootstrap', counter', ops'))
      ops_by_bootstrap
  in
  let batches block ops_by_bootstrap =
    List.fold_left_es
      (fun acc (source, _counter, operations) ->
        match operations with
        | [] -> return (List.rev acc)
        | _ ->
            let* batch = Op.batch_operations ~source (B block) operations in
            return (batch :: acc))
      []
      ops_by_bootstrap
  in
  let create_and_fund sources ops_by_bootstrap =
    let account = Account.new_account () in
    let n = nb_sources - Stdlib.List.length sources in
    let bootstrap, counter, ops = Stdlib.List.nth ops_by_bootstrap (n - 1) in
    let amount = Tez.of_mutez (Int64.of_int 150000) in
    let+ op, counter =
      Manager.fund_account_op b bootstrap account.pkh amount counter
    in
    (account :: sources, add bootstrap counter (op :: ops) ops_by_bootstrap)
  in
  let* sources, src_operations =
    List.fold_left_es
      (fun (acc_accounts, acc_ops) _ -> create_and_fund acc_accounts acc_ops)
      ([], ops_by_bootstrap)
      (1 -- nb_sources)
  in
  let* operations = batches b src_operations in
  let infos = {infos with accounts = {infos.accounts with sources}} in
  let* infos2 =
    Manager.init_infos
      Manager.ctxt_req_default
      b
      (List.take_n required_nb_account infos.ctxt.bootstraps)
  in
  let state = update_state_with_infos infos infos2 in
  return (operations, state)

(** Build a manager operation according to the information in [infos]
    on [block] for each source in the [manager_state] guaranteeing
    that they are not conflicting. *)
let manager_candidates block infos batch_max_size =
  let open Lwt_result_syntax in
  let params =
    List.map
      (fun src ->
        let m = gen_bounded_int 1 batch_max_size in
        let kd = pick_n m Manager.revealed_subjects in
        (src, kd))
      infos.Manager.accounts.sources
  in
  let gen (source, ks) =
    let infos = infos_of_state source block infos in
    let* reveal =
      Manager.mk_reveal (Manager.operation_req_default Manager.K_Reveal) infos
    in
    let* operations =
      List.map_es
        (fun kd -> Manager.select_op (Manager.operation_req_default kd) infos)
        ks
    in
    let* operations = return (reveal :: operations) in
    Op.batch_operations
      ~recompute_counters:true
      ~source:(Contract.Implicit source.pkh)
      (B block)
      operations
  in
  List.map_es gen params

let manager_descriptor max_batch_size nb_accounts =
  {
    parameters =
      (fun params ->
        let ctxt_req_default = Manager.ctxt_req_default in
        let hard_gas_limit_per_block =
          Some (Gas.Arith.integral_of_int_exn ((nb_accounts + 1) * 5_200_000))
        in
        let ctxt_req = {ctxt_req_default with hard_gas_limit_per_block} in
        Manager.manager_parameters params ctxt_req);
    required_cycle = (fun _ -> 1);
    required_block = (fun _ -> 1);
    prelude =
      ( On 1,
        fun state ->
          let open Lwt_result_syntax in
          let* ops, manager = manager_prelude state.manager state.block in
          let state = {state with manager} in
          return (ops, state) );
    opt_prelude = None;
    candidates_generator =
      (fun state -> manager_candidates state.block state.manager max_batch_size);
  }

type op_kind =
  | KAttestation
  | KPreattestation
  | KBallotExp
  | KBallotProm
  | KProposals
  | KNonce
  | KVdf
  | KActivate
  | KDbl_consensus
  | KDbl_baking
  | KDrain
  | KManager

let op_kind_of_packed_operation op =
  let (Operation_data {contents; _}) = op.protocol_data in
  match contents with
  | Single (Preattestation _) -> KPreattestation
  | Single (Attestation _) -> KAttestation
  | Single (Seed_nonce_revelation _) -> KNonce
  | Single (Vdf_revelation _) -> KVdf
  | Single (Double_attestation_evidence _) -> KDbl_consensus
  | Single (Double_preattestation_evidence _) -> KDbl_consensus
  | Single (Double_baking_evidence _) -> KDbl_baking
  | Single (Activate_account _) -> KActivate
  | Single (Proposals _) -> KProposals
  | Single (Ballot _) -> KBallotExp
  | Single (Drain_delegate _) -> KDrain
  | Single (Manager_operation _) -> KManager
  | Cons (Manager_operation _, _) -> KManager
  | Single (Failing_noop _) -> assert false

let pp_op_kind fmt kind =
  Format.fprintf
    fmt
    (match kind with
    | KManager -> "manager"
    | KAttestation -> "attestation"
    | KPreattestation -> "preattestation"
    | KBallotExp -> "ballot"
    | KBallotProm -> "ballot"
    | KProposals -> "proposals"
    | KNonce -> "nonce"
    | KVdf -> "vdf_revelation"
    | KActivate -> "activate_account"
    | KDbl_consensus -> "double_consensus"
    | KDbl_baking -> "double_baking"
    | KDrain -> "drain_delegate")

let descriptor_of ~nb_bootstrap ~max_batch_size = function
  | KManager -> manager_descriptor max_batch_size nb_bootstrap
  | KAttestation -> attestation_descriptor
  | KPreattestation -> preattestation_descriptor
  | KBallotExp -> ballot_exploration_descriptor
  | KBallotProm -> ballot_promotion_descriptor
  | KProposals -> proposal_descriptor
  | KNonce -> seed_nonce_descriptor
  | KVdf -> vdf_revelation_descriptor
  | KActivate -> activate_descriptor
  | KDbl_consensus -> double_consensus_descriptor
  | KDbl_baking -> double_baking_descriptor
  | KDrain -> drain_delegate_descriptor

let descriptors_of ~nb_bootstrap ~max_batch_size =
  List.map (descriptor_of ~nb_bootstrap ~max_batch_size)

(** A context is in a unique voting period. *)
let voting_kinds = [KProposals; KBallotExp; KBallotProm]

(** A context either wait for nonce revelation or vdf revelation
    but not both at the same time. *)
let nonce_generation_kinds = [KNonce; KVdf]

(** All kind list, used in the sanity check.*)
let non_exclusive_kinds =
  [
    KManager;
    KAttestation;
    KPreattestation;
    KActivate;
    KDbl_consensus;
    KDbl_baking;
    KDrain;
  ]

let all_kinds = voting_kinds @ nonce_generation_kinds @ non_exclusive_kinds
