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

open Protocol.Alpha_context

(* ------------------------------------------------------------------------- *)
(* Mockup protocol parameters *)

(** Protocol constants overriding logic. *)
module Protocol_constants_overrides = struct
  (** Equivalent of [Constants.parametric] with additionally [chain_id] and [timestamp] but each field is wrapped in an [option].
      [Some] is an override, [None] means "Use the default value".
  *)
  type t = {
    preserved_cycles : int option;
    blocks_per_cycle : int32 option;
    blocks_per_commitment : int32 option;
    blocks_per_stake_snapshot : int32 option;
    blocks_per_voting_period : int32 option;
    hard_gas_limit_per_operation : Gas.Arith.integral option;
    hard_gas_limit_per_block : Gas.Arith.integral option;
    proof_of_work_threshold : int64 option;
    tokens_per_roll : Tez.t option;
    seed_nonce_revelation_tip : Tez.t option;
    origination_size : int option;
    baking_reward_fixed_portion : Tez.t option;
    baking_reward_bonus_per_slot : Tez.t option;
    endorsing_reward_per_slot : Tez.t option;
    cost_per_byte : Tez.t option;
    hard_storage_limit_per_operation : Z.t option;
    quorum_min : int32 option;
    quorum_max : int32 option;
    min_proposal_quorum : int32 option;
    liquidity_baking_subsidy : Tez.t option;
    liquidity_baking_sunset_level : int32 option;
    liquidity_baking_escape_ema_threshold : int32 option;
    max_operations_time_to_live : int option;
    minimal_block_delay : Period.t option;
    delay_increment_per_round : Period.t option;
    minimal_participation_ratio : Constants.ratio option;
    consensus_committee_size : int option;
    consensus_threshold : int option;
    delegate_selection : Constants.delegate_selection option;
    max_slashing_period : int option;
    frozen_deposits_percentage : int option;
    double_baking_punishment : Tez.t option;
    ratio_of_frozen_deposits_slashed_per_double_endorsement :
      Constants.ratio option;
    (* Additional, "bastard" parameters (they are not protocol constants but partially treated the same way). *)
    chain_id : Chain_id.t option;
    timestamp : Time.Protocol.t option;
  }

  (** Shamefully copied from [Constants_repr.parametric_encoding] and adapted ([opt] instead of [req]). *)
  let encoding =
    let open Data_encoding in
    conv
      (fun c ->
        ( ( c.preserved_cycles,
            c.blocks_per_cycle,
            c.blocks_per_commitment,
            c.blocks_per_stake_snapshot,
            c.blocks_per_voting_period,
            c.hard_gas_limit_per_operation,
            c.hard_gas_limit_per_block,
            c.proof_of_work_threshold,
            c.tokens_per_roll ),
          ( ( c.seed_nonce_revelation_tip,
              c.origination_size,
              c.baking_reward_fixed_portion,
              c.baking_reward_bonus_per_slot,
              c.endorsing_reward_per_slot,
              c.cost_per_byte,
              c.hard_storage_limit_per_operation,
              c.quorum_min ),
            ( ( c.quorum_max,
                c.min_proposal_quorum,
                c.liquidity_baking_subsidy,
                c.liquidity_baking_sunset_level,
                c.liquidity_baking_escape_ema_threshold,
                c.max_operations_time_to_live,
                c.minimal_block_delay,
                c.delay_increment_per_round,
                c.consensus_committee_size,
                c.consensus_threshold ),
              ( c.delegate_selection,
                c.minimal_participation_ratio,
                c.max_slashing_period,
                c.frozen_deposits_percentage,
                c.double_baking_punishment,
                c.ratio_of_frozen_deposits_slashed_per_double_endorsement,
                c.chain_id,
                c.timestamp ) ) ) ))
      (fun ( ( preserved_cycles,
               blocks_per_cycle,
               blocks_per_commitment,
               blocks_per_stake_snapshot,
               blocks_per_voting_period,
               hard_gas_limit_per_operation,
               hard_gas_limit_per_block,
               proof_of_work_threshold,
               tokens_per_roll ),
             ( ( seed_nonce_revelation_tip,
                 origination_size,
                 baking_reward_fixed_portion,
                 baking_reward_bonus_per_slot,
                 endorsing_reward_per_slot,
                 cost_per_byte,
                 hard_storage_limit_per_operation,
                 quorum_min ),
               ( ( quorum_max,
                   min_proposal_quorum,
                   liquidity_baking_subsidy,
                   liquidity_baking_sunset_level,
                   liquidity_baking_escape_ema_threshold,
                   max_operations_time_to_live,
                   minimal_block_delay,
                   delay_increment_per_round,
                   consensus_committee_size,
                   consensus_threshold ),
                 ( delegate_selection,
                   minimal_participation_ratio,
                   max_slashing_period,
                   frozen_deposits_percentage,
                   double_baking_punishment,
                   ratio_of_frozen_deposits_slashed_per_double_endorsement,
                   chain_id,
                   timestamp ) ) ) )
         ->
        {
          preserved_cycles;
          blocks_per_cycle;
          blocks_per_commitment;
          blocks_per_stake_snapshot;
          blocks_per_voting_period;
          hard_gas_limit_per_operation;
          hard_gas_limit_per_block;
          proof_of_work_threshold;
          tokens_per_roll;
          seed_nonce_revelation_tip;
          origination_size;
          baking_reward_fixed_portion;
          baking_reward_bonus_per_slot;
          endorsing_reward_per_slot;
          cost_per_byte;
          hard_storage_limit_per_operation;
          quorum_min;
          quorum_max;
          min_proposal_quorum;
          liquidity_baking_subsidy;
          liquidity_baking_sunset_level;
          liquidity_baking_escape_ema_threshold;
          max_operations_time_to_live;
          minimal_block_delay;
          delay_increment_per_round;
          minimal_participation_ratio;
          max_slashing_period;
          frozen_deposits_percentage;
          consensus_committee_size;
          consensus_threshold;
          delegate_selection;
          double_baking_punishment;
          ratio_of_frozen_deposits_slashed_per_double_endorsement;
          chain_id;
          timestamp;
        })
      (merge_objs
         (obj9
            (opt "preserved_cycles" uint8)
            (opt "blocks_per_cycle" int32)
            (opt "blocks_per_commitment" int32)
            (opt "blocks_per_stake_snapshot" int32)
            (opt "blocks_per_voting_period" int32)
            (opt "hard_gas_limit_per_operation" Gas.Arith.z_integral_encoding)
            (opt "hard_gas_limit_per_block" Gas.Arith.z_integral_encoding)
            (opt "proof_of_work_threshold" int64)
            (opt "tokens_per_roll" Tez.encoding))
         (merge_objs
            (obj8
               (opt "seed_nonce_revelation_tip" Tez.encoding)
               (opt "origination_size" int31)
               (opt "baking_reward_fixed_portion" Tez.encoding)
               (opt "baking_reward_bonus_per_slot" Tez.encoding)
               (opt "endorsing_reward_per_slot" Tez.encoding)
               (opt "cost_per_byte" Tez.encoding)
               (opt "hard_storage_limit_per_operation" z)
               (opt "quorum_min" int32))
            (merge_objs
               (obj10
                  (opt "quorum_max" int32)
                  (opt "min_proposal_quorum" int32)
                  (opt "liquidity_baking_subsidy" Tez.encoding)
                  (opt "liquidity_baking_sunset_level" int32)
                  (opt "liquidity_baking_escape_ema_threshold" int32)
                  (opt "max_operations_time_to_live" int16)
                  (opt "minimal_block_delay" Period.encoding)
                  (opt "delay_increment_per_round" Period.encoding)
                  (opt "consensus_committee_size" int31)
                  (opt "consensus_threshold" int31))
               (obj8
                  (opt
                     "delegate_selection"
                     Constants.delegate_selection_encoding)
                  (opt "minimal_participation_ratio" Constants.ratio_encoding)
                  (opt "max_slashing_period" int31)
                  (opt "frozen_deposits_percentage" int31)
                  (opt "double_baking_punishment" Tez.encoding)
                  (opt
                     "ratio_of_frozen_deposits_slashed_per_double_endorsement"
                     Constants.ratio_encoding)
                  (opt "chain_id" Chain_id.encoding)
                  (opt "initial_timestamp" Time.Protocol.encoding)))))

  let default_value (cctxt : Tezos_client_base.Client_context.full) :
      t tzresult Lwt.t =
    let cpctxt = new Protocol_client_context.wrap_full cctxt in
    Protocol.Constants_services.all cpctxt (cpctxt#chain, cpctxt#block)
    >>=? fun {parametric; _} ->
    let to_chain_id_opt = function `Hash c -> Some c | _ -> None in
    Shell_services.Blocks.Header.shell_header
      cpctxt
      ~chain:cpctxt#chain
      ~block:cpctxt#block
      ()
    >>=? fun header ->
    return
      {
        preserved_cycles = Some parametric.preserved_cycles;
        blocks_per_cycle = Some parametric.blocks_per_cycle;
        blocks_per_commitment = Some parametric.blocks_per_commitment;
        blocks_per_stake_snapshot = Some parametric.blocks_per_stake_snapshot;
        blocks_per_voting_period = Some parametric.blocks_per_voting_period;
        hard_gas_limit_per_operation =
          Some parametric.hard_gas_limit_per_operation;
        hard_gas_limit_per_block = Some parametric.hard_gas_limit_per_block;
        proof_of_work_threshold = Some parametric.proof_of_work_threshold;
        tokens_per_roll = Some parametric.tokens_per_roll;
        seed_nonce_revelation_tip = Some parametric.seed_nonce_revelation_tip;
        origination_size = Some parametric.origination_size;
        baking_reward_fixed_portion =
          Some parametric.baking_reward_fixed_portion;
        baking_reward_bonus_per_slot =
          Some parametric.baking_reward_bonus_per_slot;
        endorsing_reward_per_slot = Some parametric.endorsing_reward_per_slot;
        cost_per_byte = Some parametric.cost_per_byte;
        hard_storage_limit_per_operation =
          Some parametric.hard_storage_limit_per_operation;
        quorum_min = Some parametric.quorum_min;
        quorum_max = Some parametric.quorum_max;
        min_proposal_quorum = Some parametric.min_proposal_quorum;
        liquidity_baking_subsidy = Some parametric.liquidity_baking_subsidy;
        liquidity_baking_sunset_level =
          Some parametric.liquidity_baking_sunset_level;
        liquidity_baking_escape_ema_threshold =
          Some parametric.liquidity_baking_escape_ema_threshold;
        max_operations_time_to_live =
          Some parametric.max_operations_time_to_live;
        minimal_block_delay = Some parametric.minimal_block_delay;
        delay_increment_per_round = Some parametric.delay_increment_per_round;
        minimal_participation_ratio =
          Some parametric.minimal_participation_ratio;
        consensus_committee_size = Some parametric.consensus_committee_size;
        (* mockup mode does not support endorsing commands *)
        consensus_threshold = Some 0;
        delegate_selection = Some Random;
        max_slashing_period = Some parametric.max_slashing_period;
        frozen_deposits_percentage = Some parametric.frozen_deposits_percentage;
        double_baking_punishment = Some parametric.double_baking_punishment;
        ratio_of_frozen_deposits_slashed_per_double_endorsement =
          Some
            parametric.ratio_of_frozen_deposits_slashed_per_double_endorsement;
        (* Bastard additional parameters. *)
        chain_id = to_chain_id_opt cpctxt#chain;
        timestamp = Some header.timestamp;
      }

  let no_overrides : t =
    {
      preserved_cycles = None;
      blocks_per_cycle = None;
      blocks_per_commitment = None;
      blocks_per_stake_snapshot = None;
      blocks_per_voting_period = None;
      hard_gas_limit_per_operation = None;
      hard_gas_limit_per_block = None;
      proof_of_work_threshold = None;
      tokens_per_roll = None;
      seed_nonce_revelation_tip = None;
      origination_size = None;
      baking_reward_fixed_portion = None;
      baking_reward_bonus_per_slot = None;
      endorsing_reward_per_slot = None;
      cost_per_byte = None;
      hard_storage_limit_per_operation = None;
      quorum_min = None;
      quorum_max = None;
      min_proposal_quorum = None;
      liquidity_baking_subsidy = None;
      liquidity_baking_sunset_level = None;
      liquidity_baking_escape_ema_threshold = None;
      max_operations_time_to_live = None;
      minimal_block_delay = None;
      delay_increment_per_round = None;
      minimal_participation_ratio = None;
      consensus_committee_size = None;
      (* Let consensus threshold be overridable for Tenderbake mockup
         simulator tests. *)
      consensus_threshold = None;
      delegate_selection = None;
      max_slashing_period = None;
      frozen_deposits_percentage = None;
      double_baking_punishment = None;
      ratio_of_frozen_deposits_slashed_per_double_endorsement = None;
      chain_id = None;
      timestamp = None;
    }

  (** Existential wrapper to support heterogeneous lists/maps. *)
  type field =
    | O : {
        name : string;
        override_value : 'a option;
        pp : Format.formatter -> 'a -> unit;
      }
        -> field

  let field_pp ppf (O {name; override_value; pp; _}) =
    match override_value with
    | None -> ()
    | Some value -> Format.fprintf ppf "@[<h>%s: %a@]" name pp value

  let apply_overrides (cctxt : Tezos_client_base.Client_context.printer) (o : t)
      (c : Constants.parametric) : Constants.parametric tzresult Lwt.t =
    let open Format in
    let pp_print_int32 ppf i = fprintf ppf "%li" i in
    let pp_print_int64 ppf i = fprintf ppf "%Li" i in
    let fields : field list =
      [
        O
          {
            name = "preserved_cycles";
            override_value = o.preserved_cycles;
            pp = pp_print_int;
          };
        O
          {
            name = "blocks_per_cycle";
            override_value = o.blocks_per_cycle;
            pp = pp_print_int32;
          };
        O
          {
            name = "blocks_per_commitment";
            override_value = o.blocks_per_commitment;
            pp = pp_print_int32;
          };
        O
          {
            name = "blocks_per_stake_snapshot";
            override_value = o.blocks_per_stake_snapshot;
            pp = pp_print_int32;
          };
        O
          {
            name = "blocks_per_voting_period";
            override_value = o.blocks_per_voting_period;
            pp = pp_print_int32;
          };
        O
          {
            name = "hard_gas_limit_per_operation";
            override_value = o.hard_gas_limit_per_operation;
            pp = Gas.Arith.pp_integral;
          };
        O
          {
            name = "hard_gas_limit_per_block";
            override_value = o.hard_gas_limit_per_block;
            pp = Gas.Arith.pp_integral;
          };
        O
          {
            name = "proof_of_work_threshold";
            override_value = o.proof_of_work_threshold;
            pp = pp_print_int64;
          };
        O
          {
            name = "tokens_per_roll";
            override_value = o.tokens_per_roll;
            pp = Tez.pp;
          };
        O
          {
            name = "seed_nonce_revelation_tip";
            override_value = o.seed_nonce_revelation_tip;
            pp = Tez.pp;
          };
        O
          {
            name = "origination_size";
            override_value = o.origination_size;
            pp = pp_print_int;
          };
        O
          {
            name = "baking_reward_fixed_portion";
            override_value = o.baking_reward_fixed_portion;
            pp = Tez.pp;
          };
        O
          {
            name = "baking_reward_bonus_per_slot";
            override_value = o.baking_reward_bonus_per_slot;
            pp = Tez.pp;
          };
        O
          {
            name = "endorsing_reward_per_slot";
            override_value = o.endorsing_reward_per_slot;
            pp = Tez.pp;
          };
        O
          {
            name = "cost_per_byte";
            override_value = o.cost_per_byte;
            pp = Tez.pp;
          };
        O
          {
            name = "hard_storage_limit_per_operation";
            override_value = o.hard_storage_limit_per_operation;
            pp = Z.pp_print;
          };
        O
          {
            name = "quorum_min";
            override_value = o.quorum_min;
            pp = pp_print_int32;
          };
        O
          {
            name = "quorum_max";
            override_value = o.quorum_max;
            pp = pp_print_int32;
          };
        O
          {
            name = "min_proposal_quorum";
            override_value = o.min_proposal_quorum;
            pp = pp_print_int32;
          };
        O
          {
            name = "liquidity_baking_subsidy";
            override_value = o.liquidity_baking_subsidy;
            pp = Tez.pp;
          };
        O
          {
            name = "liquidity_baking_sunset_level";
            override_value = o.liquidity_baking_sunset_level;
            pp = pp_print_int32;
          };
        O
          {
            name = "liquidity_baking_escape_ema_threshold";
            override_value = o.liquidity_baking_escape_ema_threshold;
            pp = pp_print_int32;
          };
        O
          {
            name = "minimal_block_delay";
            override_value = o.minimal_block_delay;
            pp = Period.pp;
          };
        O
          {
            name = "delay_increment_per_round";
            override_value = o.delay_increment_per_round;
            pp = Period.pp;
          };
        O
          {
            name = "minimal_participation_ratio";
            override_value = o.minimal_participation_ratio;
            pp = Constants.pp_ratio;
          };
        O
          {
            name = "consensus_committee_size";
            override_value = o.consensus_committee_size;
            pp = pp_print_int;
          };
        O
          {
            name = "consensus_threshold";
            override_value = o.consensus_threshold;
            pp = pp_print_int;
          };
        O
          {
            name = "max_slashing_period";
            override_value = o.max_slashing_period;
            pp = pp_print_int;
          };
        O
          {
            name = "frozen_deposits_percentage";
            override_value = o.frozen_deposits_percentage;
            pp = pp_print_int;
          };
        O
          {
            name = "double_baking_punishment";
            override_value = o.double_baking_punishment;
            pp = Tez.pp;
          };
        O
          {
            name = "ratio_of_frozen_deposits_slashed_per_double_endorsement";
            override_value =
              o.ratio_of_frozen_deposits_slashed_per_double_endorsement;
            pp = Constants.pp_ratio;
          };
        O {name = "chain_id"; override_value = o.chain_id; pp = Chain_id.pp};
        O
          {
            name = "timestamp";
            override_value = o.timestamp;
            pp = Time.Protocol.pp_hum;
          };
      ]
    in
    let fields_with_override =
      fields
      |> List.filter (fun (O {override_value; _}) ->
             Option.is_some override_value)
    in
    (if fields_with_override <> [] then
       cctxt#message
         "@[<v>mockup client uses protocol overrides:@,%a@]@?"
         (pp_print_list field_pp)
         fields_with_override
     else Lwt.return_unit)
    >>= fun () ->
    return
      ({
         minimal_block_delay =
           Option.value ~default:c.minimal_block_delay o.minimal_block_delay;
         delay_increment_per_round =
           Option.value
             ~default:c.delay_increment_per_round
             o.delay_increment_per_round;
         consensus_committee_size =
           Option.value
             ~default:c.consensus_committee_size
             o.consensus_committee_size;
         consensus_threshold =
           Option.value ~default:c.consensus_threshold o.consensus_threshold;
         delegate_selection =
           Option.value ~default:c.delegate_selection o.delegate_selection;
         preserved_cycles =
           Option.value ~default:c.preserved_cycles o.preserved_cycles;
         blocks_per_cycle =
           Option.value ~default:c.blocks_per_cycle o.blocks_per_cycle;
         blocks_per_commitment =
           Option.value ~default:c.blocks_per_commitment o.blocks_per_commitment;
         blocks_per_stake_snapshot =
           Option.value
             ~default:c.blocks_per_stake_snapshot
             o.blocks_per_stake_snapshot;
         blocks_per_voting_period =
           Option.value
             ~default:c.blocks_per_voting_period
             o.blocks_per_voting_period;
         hard_gas_limit_per_operation =
           Option.value
             ~default:c.hard_gas_limit_per_operation
             o.hard_gas_limit_per_operation;
         hard_gas_limit_per_block =
           Option.value
             ~default:c.hard_gas_limit_per_block
             o.hard_gas_limit_per_block;
         proof_of_work_threshold =
           Option.value
             ~default:c.proof_of_work_threshold
             o.proof_of_work_threshold;
         tokens_per_roll =
           Option.value ~default:c.tokens_per_roll o.tokens_per_roll;
         seed_nonce_revelation_tip =
           Option.value
             ~default:c.seed_nonce_revelation_tip
             o.seed_nonce_revelation_tip;
         origination_size =
           Option.value ~default:c.origination_size o.origination_size;
         baking_reward_fixed_portion =
           Option.value
             ~default:c.baking_reward_fixed_portion
             o.baking_reward_fixed_portion;
         baking_reward_bonus_per_slot =
           Option.value
             ~default:c.baking_reward_bonus_per_slot
             o.baking_reward_bonus_per_slot;
         endorsing_reward_per_slot =
           Option.value
             ~default:c.endorsing_reward_per_slot
             o.endorsing_reward_per_slot;
         cost_per_byte = Option.value ~default:c.cost_per_byte o.cost_per_byte;
         hard_storage_limit_per_operation =
           Option.value
             ~default:c.hard_storage_limit_per_operation
             o.hard_storage_limit_per_operation;
         quorum_min = Option.value ~default:c.quorum_min o.quorum_min;
         quorum_max = Option.value ~default:c.quorum_max o.quorum_max;
         min_proposal_quorum =
           Option.value ~default:c.min_proposal_quorum o.min_proposal_quorum;
         liquidity_baking_subsidy =
           Option.value
             ~default:c.liquidity_baking_subsidy
             o.liquidity_baking_subsidy;
         liquidity_baking_sunset_level =
           Option.value
             ~default:c.liquidity_baking_sunset_level
             o.liquidity_baking_sunset_level;
         liquidity_baking_escape_ema_threshold =
           Option.value
             ~default:c.liquidity_baking_escape_ema_threshold
             o.liquidity_baking_escape_ema_threshold;
         max_operations_time_to_live =
           Option.value
             ~default:c.max_operations_time_to_live
             o.max_operations_time_to_live;
         minimal_participation_ratio =
           Option.value
             ~default:c.minimal_participation_ratio
             o.minimal_participation_ratio;
         max_slashing_period =
           Option.value ~default:c.max_slashing_period o.max_slashing_period;
         frozen_deposits_percentage =
           Option.value
             ~default:c.frozen_deposits_percentage
             o.frozen_deposits_percentage;
         double_baking_punishment =
           Option.value
             ~default:c.double_baking_punishment
             o.double_baking_punishment;
         ratio_of_frozen_deposits_slashed_per_double_endorsement =
           Option.value
             ~default:c.ratio_of_frozen_deposits_slashed_per_double_endorsement
             o.ratio_of_frozen_deposits_slashed_per_double_endorsement
           (* Notice that the chain_id and the timestamp are not used here
              as they are not protocol constants... *);
       }
        : Constants.parametric)
end

module Parsed_account = struct
  type t = {name : string; sk_uri : Client_keys_v0.sk_uri; amount : Tez.t}

  let pp ppf account =
    let open Format in
    let format_amount ppf value = fprintf ppf "amount:%a" Tez.pp value in
    fprintf
      ppf
      "@[<v>name:%s@,sk_uri:%s@,%a@]"
      account.name
      (Uri.to_string (account.sk_uri :> Uri.t))
      format_amount
      account.amount

  let encoding =
    let open Data_encoding in
    conv
      (fun p -> (p.name, p.sk_uri, p.amount))
      (fun (name, sk_uri, amount) -> {name; sk_uri; amount})
      (obj3
         (req "name" string)
         (req "sk_uri" Client_keys_v0.Secret_key.encoding)
         (req "amount" Tez.encoding))

  let to_bootstrap_account repr =
    Client_keys_v0.neuterize repr.sk_uri >>=? fun pk_uri ->
    Client_keys_v0.public_key pk_uri >>=? fun public_key ->
    let public_key_hash =
      Tezos_crypto.Signature.V0.Public_key.hash public_key
    in
    return
      Parameters.
        {public_key_hash; public_key = Some public_key; amount = repr.amount}

  let default_to_json (cctxt : Tezos_client_base.Client_context.full) :
      string tzresult Lwt.t =
    let rpc_context = new Protocol_client_context.wrap_full cctxt in
    let wallet = (cctxt :> Client_context.wallet) in
    let parsed_account_reprs = ref [] in
    let errors = ref [] in
    Client_keys_v0.list_keys wallet >>=? fun all_keys ->
    List.iter_s
      (function
        | name, pkh, _pk_opt, Some sk_uri -> (
            let contract = Contract.implicit_contract pkh in
            Client_proto_context.get_balance
              rpc_context
              ~chain:cctxt#chain
              ~block:cctxt#block
              contract
            >>= fun tz_balance ->
            match tz_balance with
            | Ok balance -> (
                let tez_repr = Tez.of_mutez @@ Tez.to_mutez balance in
                match tez_repr with
                | None ->
                    (* we're reading the wallet, it's content MUST be valid *)
                    assert false
                | Some amount ->
                    parsed_account_reprs :=
                      {name; sk_uri; amount} :: !parsed_account_reprs ;
                    Lwt.return_unit)
            | Error err ->
                errors := err :: !errors ;
                Lwt.return_unit)
        | _ -> Lwt.return_unit)
      all_keys
    >>= fun () ->
    match !errors with
    | [] ->
        let json =
          Data_encoding.Json.construct
            (Data_encoding.list encoding)
            !parsed_account_reprs
        in
        return @@ Data_encoding.Json.to_string json
    | errs -> Lwt.return_error @@ List.concat errs
end

module Bootstrap_account = struct
  let encoding : Parameters.bootstrap_account Data_encoding.t =
    let open Data_encoding in
    let open Parameters in
    conv
      (fun {public_key_hash; public_key; amount} ->
        (public_key_hash, public_key, amount))
      (fun (public_key_hash, public_key, amount) ->
        {public_key_hash; public_key; amount})
      (obj3
         (req
            "public_key_hash"
            Tezos_crypto.Signature.V0.Public_key_hash.encoding)
         (opt "public_key" Tezos_crypto.Signature.V0.Public_key.encoding)
         (req "amount" Tez.encoding))
end

module Bootstrap_contract = struct
  let encoding : Parameters.bootstrap_contract Data_encoding.t =
    let open Data_encoding in
    let open Parameters in
    conv
      (fun {delegate; amount; script} -> (delegate, amount, script))
      (fun (delegate, amount, script) -> {delegate; amount; script})
      (obj3
         (opt "delegate" Tezos_crypto.Signature.V0.Public_key_hash.encoding)
         (req "amount" Tez.encoding)
         (req "script" Script.encoding))
end

module Protocol_parameters = struct
  type t = {
    initial_timestamp : Time.Protocol.t;
    bootstrap_accounts : Parameters.bootstrap_account list;
    bootstrap_contracts : Parameters.bootstrap_contract list;
    constants : Constants.parametric;
  }

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun p ->
        ( p.initial_timestamp,
          p.bootstrap_accounts,
          p.bootstrap_contracts,
          p.constants ))
      (fun ( initial_timestamp,
             bootstrap_accounts,
             bootstrap_contracts,
             constants )
         ->
        {initial_timestamp; bootstrap_accounts; bootstrap_contracts; constants})
      (obj4
         (req "initial_timestamp" Time.Protocol.encoding)
         (req "bootstrap_accounts" (list Bootstrap_account.encoding))
         (req "bootstrap_contracts" (list Bootstrap_contract.encoding))
         (req "constants" Constants.parametric_encoding))

  let default_value : t =
    let parameters =
      Default_parameters.parameters_of_constants
        Default_parameters.constants_sandbox
    in
    {
      initial_timestamp = Time.Protocol.epoch;
      bootstrap_accounts = parameters.bootstrap_accounts;
      bootstrap_contracts = parameters.bootstrap_contracts;
      constants = parameters.constants;
    }
end

(* This encoding extends [Protocol_constants_overrides.encoding] to allow
   reading json files as produced by lib_parameters. Sadly, this require
   copying partially [bootstrap_account_encoding], which is not exposed
   in parameters_repr.ml. *)
let lib_parameters_json_encoding =
  let bootstrap_account_encoding =
    let open Data_encoding in
    conv
      (function
        | {Parameters.public_key; amount; _} -> (
            match public_key with
            | None -> assert false
            | Some pk -> (pk, amount)))
      (fun (pk, amount) ->
        {
          Parameters.public_key = Some pk;
          public_key_hash = Tezos_crypto.Signature.V0.Public_key.hash pk;
          amount;
        })
      (tup2 Tezos_crypto.Signature.V0.Public_key.encoding Tez.encoding)
  in
  Data_encoding.(
    merge_objs
      (obj2
         (opt "bootstrap_accounts" (list bootstrap_account_encoding))
         (opt "commitments" (list Commitment.encoding)))
      Protocol_constants_overrides.encoding)

(* ------------------------------------------------------------------------- *)
(* Blocks *)

type block = {
  hash : Block_hash.t;
  header : Block_header.t;
  operations : Operation.packed list;
  context : Environment.Context.t;
}

module Forge = struct
  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level = 0;
        validation_passes = 0;
        context = Context_hash.zero;
      }
end

(* ------------------------------------------------------------------------- *)
(* RPC context *)
let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"

let endorsement_branch_data_encoding =
  let open Data_encoding in
  conv
    (fun (block_hash, block_payload_hash) -> (block_hash, block_payload_hash))
    (fun (block_hash, block_payload_hash) -> (block_hash, block_payload_hash))
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_payload_hash" Protocol.Block_payload_hash.encoding))

let initial_context chain_id (header : Block_header.shell_header)
    ({bootstrap_accounts; bootstrap_contracts; constants; _} :
      Protocol_parameters.t) =
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~bootstrap_contracts
      ~commitments:[]
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Tezos_protocol_environment.Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt ["protocol_parameters"] proto_params)
  >>= fun ctxt ->
  Environment.Updater.activate ctxt Protocol.hash >>= fun ctxt ->
  Protocol.Main.init ctxt header >|= Environment.wrap_tzresult
  >>=? fun {context; _} ->
  let ({
         timestamp = predecessor_timestamp;
         level = predecessor_level;
         fitness = predecessor_fitness;
         _;
       }
        : Block_header.shell_header) =
    header
  in
  let timestamp = Time.System.to_protocol (Tezos_base.Time.System.now ()) in
  (*

     We need to forge a predecessor hash to pass it to [value_of_key].
     This initial context is used for RPC, hence this piece of
     information is not important and does not have to be meaningful

  *)
  let predecessor =
    Tezos_base.Block_header.hash {shell = header; protocol_data = Bytes.empty}
  in
  Protocol.Main.value_of_key
    ~chain_id
    ~predecessor_context:context
    ~predecessor_timestamp
    ~predecessor_level
    ~predecessor_fitness
    ~predecessor
    ~timestamp
  >|= Environment.wrap_tzresult
  >>=? fun value_of_key ->
  (*
      In the mockup mode, reactivity is important and there are
      no constraints to be consistent with other nodes. For this
      reason, the mockup mode loads the cache lazily.
      See {!Environment_context.source_of_cache}.
  *)
  Tezos_protocol_environment.Context.load_cache
    predecessor
    context
    `Lazy
    (fun key -> value_of_key key >|= Environment.wrap_tzresult)
  >>=? fun context -> return context

let mem_init :
    cctxt:Tezos_client_base.Client_context.printer ->
    parameters:Protocol_parameters.t ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    Tezos_mockup_registration.Registration.mockup_context tzresult Lwt.t =
 fun ~cctxt ~parameters ~constants_overrides_json ~bootstrap_accounts_json ->
  let hash = genesis_block_hash in
  (* Need to read this Json file before since timestamp modification may be in
     there *)
  (match constants_overrides_json with
  | None -> return Protocol_constants_overrides.no_overrides
  | Some json -> (
      match Data_encoding.Json.destruct lib_parameters_json_encoding json with
      | _, x -> return x
      | exception error ->
          failwith
            "cannot read protocol constants overrides: %a"
            (Data_encoding.Json.print_error ?print_unknown:None)
            error))
  >>=? fun protocol_overrides ->
  let default = parameters.initial_timestamp in
  let timestamp = Option.value ~default protocol_overrides.timestamp in
  (if not @@ Time.Protocol.equal default timestamp then
     cctxt#message "@[<h>initial_timestamp: %a@]" Time.Protocol.pp_hum timestamp
   else Lwt.return_unit)
  >>= fun () ->
  let fitness =
    Protocol.Alpha_context.(
      Fitness.create_without_locked_round
        ~level:Raw_level.root
        ~predecessor_round:Round.zero
        ~round:Round.zero
      |> Fitness.to_raw)
  in
  let shell_header =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp
      ~fitness
      ~operations_hash:Operation_list_list_hash.zero
  in
  Protocol_constants_overrides.apply_overrides
    (cctxt :> Tezos_client_base.Client_context.printer)
    protocol_overrides
    parameters.constants
  >>=? fun protocol_custom ->
  (match bootstrap_accounts_json with
  | None -> return None
  | Some json -> (
      match
        Data_encoding.Json.destruct
          (Data_encoding.list Parsed_account.encoding)
          json
      with
      | accounts ->
          cctxt#message "@[<h>mockup client uses custom bootstrap accounts:@]"
          >>= fun () ->
          let open Format in
          cctxt#message
            "@[%a@]"
            (pp_print_list
               ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
               Parsed_account.pp)
            accounts
          >>= fun () ->
          List.map_es Parsed_account.to_bootstrap_account accounts
          >>=? fun bootstrap_accounts -> return (Some bootstrap_accounts)
      | exception error ->
          failwith
            "cannot read definitions of bootstrap accounts: %a"
            (Data_encoding.Json.print_error ?print_unknown:None)
            error))
  >>=? fun bootstrap_accounts_custom ->
  let chain_id =
    Tezos_mockup_registration.Mockup_args.Chain_id.choose
      ~from_config_file:protocol_overrides.chain_id
  in
  initial_context
    chain_id
    shell_header
    {
      parameters with
      bootstrap_accounts =
        Option.value
          ~default:parameters.bootstrap_accounts
          bootstrap_accounts_custom;
      constants = protocol_custom;
    }
  >>=? fun context ->
  let chain_id =
    Tezos_mockup_registration.Mockup_args.Chain_id.choose
      ~from_config_file:protocol_overrides.chain_id
  in
  let protocol_data =
    let payload_hash =
      Protocol.Block_payload_hash.hash_bytes
        [Block_hash.to_bytes hash; Operation_list_hash.(to_bytes @@ compute [])]
    in
    let open Protocol.Alpha_context.Block_header in
    let _, _, sk = Tezos_crypto.Signature.V0.generate_key () in
    let proof_of_work_nonce =
      Bytes.create Protocol.Alpha_context.Constants.proof_of_work_nonce_size
    in
    let contents =
      {
        payload_round = Round.zero;
        payload_hash;
        seed_nonce_hash = None;
        proof_of_work_nonce;
        (* following Baking_configuration.escape_votes in lib_delegate *)
        liquidity_baking_escape_vote = false;
      }
    in
    let unsigned_bytes =
      Data_encoding.Binary.to_bytes_exn
        Block_header.unsigned_encoding
        (shell_header, contents)
    in
    let signature =
      Tezos_crypto.Signature.V0.sign
        ~watermark:
          Protocol.Alpha_context.Block_header.(
            to_watermark (Block_header chain_id))
        sk
        unsigned_bytes
    in
    Data_encoding.Binary.to_bytes_exn
      Protocol.block_header_data_encoding
      {contents; signature}
  in
  return
    Tezos_mockup_registration.Registration_intf.
      {
        chain = chain_id;
        rpc_context =
          Tezos_protocol_environment.
            {block_hash = hash; block_header = shell_header; context};
        protocol_data;
      }

let migrate :
    Tezos_mockup_registration.Registration.mockup_context ->
    Tezos_mockup_registration.Registration.mockup_context tzresult Lwt.t =
 fun {chain; rpc_context; protocol_data} ->
  let Tezos_protocol_environment.{block_hash; context; block_header} =
    rpc_context
  in
  Environment.Updater.activate context Protocol.hash >>= fun context ->
  Protocol.Main.init context block_header >|= Environment.wrap_tzresult
  >>=? fun {context; _} ->
  let rpc_context =
    Tezos_protocol_environment.{block_hash; block_header; context}
  in
  return
    Tezos_mockup_registration.Registration_intf.
      {chain; rpc_context; protocol_data}

(* ------------------------------------------------------------------------- *)
(* Register mockup *)

module M :
  Tezos_mockup_registration.Registration_intf.MOCKUP
    with module Protocol = Protocol_client_context.Lifted_protocol = struct
  type parameters = Protocol_parameters.t

  type protocol_constants = Protocol_constants_overrides.t

  let parameters_encoding = Protocol_parameters.encoding

  let default_parameters = Protocol_parameters.default_value

  let protocol_constants_encoding = Protocol_constants_overrides.encoding

  let default_protocol_constants = Protocol_constants_overrides.default_value

  let default_bootstrap_accounts = Parsed_account.default_to_json

  let protocol_hash = Protocol.hash

  module Protocol = Protocol_client_context.Lifted_protocol
  module Block_services = Protocol_client_context.Alpha_block_services

  let directory = Plugin.RPC.rpc_services

  let init ~cctxt ~parameters ~constants_overrides_json ~bootstrap_accounts_json
      =
    mem_init
      ~cctxt:(cctxt :> Tezos_client_base.Client_context.printer)
      ~parameters
      ~constants_overrides_json
      ~bootstrap_accounts_json

  let migrate = migrate
end

let () =
  Tezos_mockup_registration.Registration.register_mockup_environment (module M)
