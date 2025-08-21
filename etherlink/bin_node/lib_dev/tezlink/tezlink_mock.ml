(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

include Tezlink_imports

(* Provides mock values necessary for constructing L1 types that contain fields
    that are either irrelevant to the L2 or are not yet supported. *)
let proto_level = 1

let validation_passes = 4

(* TODO #7866
   When blocks are populated, this mock value will be unnecessary,
   being replaced by actual data. *)
let operations_hash =
  Operation_list_list_hash.of_bytes_exn (Bytes.make 32 '\000')

let fitness =
  [
    Bytes.make 4 '\255';
    Bytes.make 4 '\255';
    Bytes.make 4 '\255';
    Bytes.make 4 '\255';
    Bytes.make 4 '\000';
  ]

(* TODO #7866
   When blocks are populated, this mock value will be unnecessary,
   being replaced by actual data. *)
let context = Context_hash.of_bytes_exn (Bytes.make 32 '\255')

(* We are introducing a mock bootstrap account, which will be useful for plugging
   Tzkt on Tezlink. For now, we are using this bootstrap account as the proposer/baker,
   but this should change. If we decide to introduce real Tezlink bootstrap account,
   the baker/proposer should be the address zero or the address of the sequencer). *)
let bootstrap_account =
  let public_key_internal =
    let pk_opt =
      Tezos_crypto.Signature.Ed25519.Public_key.of_bytes_without_validation
        (Bytes.make 32 '\000')
    in
    match pk_opt with None -> (* Unreachable *) assert false | Some pk -> pk
  in
  let public_key : Imported_protocol.Alpha_context.public_key =
    Ed25519 public_key_internal
  in
  let public_key_hash : Imported_protocol.Alpha_context.public_key_hash =
    Ed25519 (Tezos_crypto.Signature.Ed25519.Public_key.hash public_key_internal)
  in
  Imported_protocol_parameters.Default_parameters.make_bootstrap_account
    ( public_key_hash,
      public_key,
      (* This amount was arbitrarly chosen according to bootstrap account
         in L1 sandbox *)
      Alpha_context.Tez.of_mutez_exn 200000000000L,
      None,
      None )

let contents : Alpha_context.Block_header.contents =
  {
    payload_hash = Imported_protocol.Block_payload_hash.zero;
    payload_round = Alpha_context.Round.zero;
    seed_nonce_hash = None;
    proof_of_work_nonce =
      Bytes.make
        Imported_protocol.Constants_repr.proof_of_work_nonce_size
        '\000';
    per_block_votes =
      {
        liquidity_baking_vote = Per_block_vote_pass;
        adaptive_issuance_vote = Per_block_vote_pass;
      };
  }

let signature : Imported_protocol.Alpha_context.signature =
  Unknown (Bytes.make Tezos_crypto.Signature.Ed25519.size '\000')

let protocol_data : Alpha_context.Block_header.protocol_data =
  {contents; signature}

module Operation_metadata = struct
  open Imported_protocol.Apply_results
  open Alpha_context

  type error += Unsupported_operation_kind of string

  let () =
    register_error_kind
      `Permanent
      ~id:"evm_node.dev.tezlink.unsupported_operation_kind"
      ~title:"Unsupported operation kind"
      ~description:"In a RPC call, an operation of unsupported kind was given."
      ~pp:(fun ppf s -> Format.fprintf ppf "Unsupported operation kind: %s" s)
      Data_encoding.(obj1 (req "message" string))
      (function
        | Unsupported_operation_kind message -> Some message | _ -> None)
      (fun message -> Unsupported_operation_kind message)

  let consumed_gas = Gas.Arith.zero

  let manager_op_result (type kind) (contents : kind manager_operation) :
      kind successful_manager_operation_result tzresult =
    let open Result_syntax in
    match contents with
    | Reveal _ -> return (Reveal_result {consumed_gas})
    | Transaction _ ->
        return
          (Transaction_result
             (Transaction_to_contract_result
                {
                  storage = None;
                  lazy_storage_diff = None;
                  balance_updates = [];
                  ticket_receipt = [];
                  originated_contracts = [];
                  consumed_gas;
                  storage_size = Z.zero;
                  paid_storage_size_diff = Z.zero;
                  allocated_destination_contract = true;
                }))
    | _ ->
        tzfail
          (Unsupported_operation_kind
             "only supported kinds are 'reveal' and 'transaction'")

  let contents_result (type kind) (contents : kind contents) :
      kind contents_result tzresult =
    let open Result_syntax in
    match contents with
    | Manager_operation {operation; _} ->
        let* result = manager_op_result operation in
        return
          (Manager_operation_result
             {
               balance_updates = [];
               internal_operation_results = [];
               operation_result = Applied result;
             })
    | _ ->
        tzfail
          (Unsupported_operation_kind "only manager operations are supported")

  let rec contents_list_result :
      type kind. kind contents_list -> kind contents_result_list tzresult =
   fun contents ->
    let open Result_syntax in
    match contents with
    | Single contents ->
        let* result = contents_result contents in
        return (Single_result result)
    | Cons (contents, contents_list) ->
        let* result = contents_result contents in
        let* result_list = contents_list_result contents_list in
        return (Cons_result (result, result_list))

  let operation_metadata (Operation_data op) =
    let open Result_syntax in
    let* contents = contents_list_result op.contents in
    return (Operation_metadata {contents})
end

(* When indexing Tezlink, Tzkt requires Liquidity baking contracts. *)
module Liquidity_baking = struct
  let cpmm_address = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

  let lqt_address = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

  let lq_fallback_token = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN"

  let cpmm_script : Alpha_context.Script.t =
    let cpmm_storage =
      let open Imported_protocol in
      Script_repr.lazy_expr
        (Imported_env.Micheline.strip_locations
           (Prim
              ( 0,
                Michelson_v1_primitives.D_Pair,
                [
                  Int (1, Z.one);
                  Int (2, Z.of_int 100);
                  Int (3, Z.of_int 100);
                  String (4, lq_fallback_token);
                  String (5, lqt_address);
                ],
                [] )))
    in
    {
      code =
        Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_cpmm.script);
      storage = cpmm_storage;
    }

  let token_script : Alpha_context.Script.t =
    let open Imported_protocol in
    let lqt_storage =
      Script_repr.lazy_expr
        (Imported_env.Micheline.strip_locations
           (Prim
              ( 0,
                Michelson_v1_primitives.D_Pair,
                [
                  Int (1, Z.of_int 2);
                  Int (2, Z.of_int 3);
                  String (3, cpmm_address);
                  Int (4, Z.of_int 100);
                ],
                [] )))
    in
    {
      code =
        Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_lqt.script);
      storage = lqt_storage;
    }

  let fallback_script : Alpha_context.Script.t =
    let open Imported_protocol in
    let fallback_token_storage =
      Script_repr.lazy_expr
        (Imported_env.Micheline.strip_locations
           (Prim
              ( 0,
                Michelson_v1_primitives.D_Pair,
                [
                  Int (1, Z.of_int 0);
                  Int (2, Z.of_int 1);
                  String (3, "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx");
                  Int (4, Z.of_int 10_000);
                ],
                [] )))
    in
    {
      code =
        Imported_protocol.(Script_repr.lazy_expr Liquidity_baking_lqt.script);
      storage = fallback_token_storage;
    }
end

(* When indexing Tezlink, Tzkt requires Liquidity baking contracts.
   Instead of really storing it, in the durable_storage, we mock the
   rpc contract as this is the only RPC needed by tzkt for
   Liquidity baking. *)
let mocked_script (contract : Alpha_context.Contract.t) =
  let address = Alpha_context.Contract.to_b58check contract in
  if address = Liquidity_baking.cpmm_address then
    Some Liquidity_baking.cpmm_script
  else if address = Liquidity_baking.lqt_address then
    Some Liquidity_baking.token_script
  else if address = Liquidity_baking.lq_fallback_token then
    Some Liquidity_baking.fallback_script
  else None

let version =
  let open Tezos_version.Octez_node_version in
  Tezos_version.
    {
      version = Tezos_version_parser.default;
      network_version = Network_version.Internal_for_tests.mock ();
      commit_info = Some {commit_hash = ""; commit_date = ""};
    }

(* This module is a fake storage representation, the raw services gives access to the
   real context. Unfortunately, some values are useless for Tezlink, so we don't want
   to store them in the durable storage. So let's do their mock here *)
module Storage_repr = struct
  module Cycle = struct
    let delegate_sampler_state_encoding =
      Imported_protocol.Sampler.encoding
        Imported_protocol.Raw_context.consensus_pk_encoding

    let create_sample_state ~consensus_pks =
      Imported_protocol.Sampler.create consensus_pks

    let selected_stake_distribution_encoding =
      Data_encoding.(
        Variable.list
          (obj2
             (req "baker" Imported_env.Signature.Public_key_hash.encoding)
             (req "active_stake" Imported_protocol.Stake_repr.encoding)))

    let stake ~consensus_pk =
      let lots =
        Option.value ~default:Imported_protocol.Tez_repr.one
        @@ Imported_protocol.Tez_repr.of_mutez 200000000000L
      in
      let selected_stake =
        Imported_protocol.Stake_repr.make
          ~frozen:lots
          ~weighted_delegated:Imported_protocol.Tez_repr.zero
      in
      [
        Imported_protocol.Raw_context.
          (consensus_pk.consensus_pkh, selected_stake);
      ]

    type storage_cycle = {
      selected_stake_distribution :
        (Imported_env.Signature.public_key_hash
        * Imported_protocol.Stake_repr.t)
        list;
      delegate_sampler_state :
        Imported_protocol.Raw_context.consensus_pk Imported_protocol.Sampler.t;
    }

    let sampler_encoding =
      let open Data_encoding in
      conv
        (fun {delegate_sampler_state; selected_stake_distribution} ->
          ( (),
            (),
            delegate_sampler_state,
            (),
            (),
            (),
            (),
            selected_stake_distribution,
            () ))
        (fun ( (),
               (),
               delegate_sampler_state,
               (),
               (),
               (),
               (),
               selected_stake_distribution,
               () ) -> {delegate_sampler_state; selected_stake_distribution})
        (obj9
           (req "already_denounced" empty)
           (req "dal_already_denounced" empty)
           (req "delegate_sampler_state" delegate_sampler_state_encoding)
           (req "nonces" empty)
           (req "pending_consensus_keys" empty)
           (req "pending_staking_parameters" empty)
           (req
              "random_seed"
              (constant
                 "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"))
           (req
              "selected_stake_distribution"
              selected_stake_distribution_encoding)
           (req "total_active_stake" empty))
  end
end

(* Voting period type t in `alpha_context.mli` is a private type so we need to create our own
   and then do the conversion with the serialization. *)
module Voting_period = struct
  type t = {
    index : int32;
    kind : Alpha_context.Voting_period.kind;
    start_position : int32;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {index; kind; start_position} -> (index, kind, start_position))
      (fun (index, kind, start_position) -> {index; kind; start_position})
      (obj3
         (req
            "index"
            ~description:
              "The voting period's index. Starts at 0 with the first block of \
               the Alpha family of protocols."
            int32)
         (req
            ~description:
              "One of the several kinds of periods in the voting procedure."
            "kind"
            Alpha_context.Voting_period.kind_encoding)
         (req
            ~description:
              "The relative position of the first level of the period with \
               respect to the first level of the Alpha family of protocols."
            "start_position"
            int32))

  let convert =
    Tezos_types.convert_using_serialization
      ~name:"voting_period_info"
      ~src:encoding
      ~dst:Alpha_context.Voting_period.encoding
end

let balance_udpdate_bootstrap ~amount ~bootstrap =
  let open Alpha_context.Receipt in
  let migration =
    item
      Bootstrap
      (Debited (Alpha_context.Tez.of_mutez_exn amount))
      Protocol_migration
  in
  let baker = frozen_baker bootstrap in
  let deposit =
    item
      (Deposits baker)
      (Credited (Alpha_context.Tez.of_mutez_exn amount))
      Protocol_migration
  in
  [migration; deposit]

let balance_udpdate_rewards ~(baker : Alpha_context.public_key_hash) ~amount =
  let open Alpha_context.Receipt in
  let debited_rewards =
    item Baking_rewards (Debited amount) Block_application
  in
  let baker = frozen_baker baker in
  let credited_rewards =
    item (Deposits baker) (Credited amount) Block_application
  in
  [debited_rewards; credited_rewards]
