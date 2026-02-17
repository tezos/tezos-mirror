(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Provides mock values necessary for constructing L1 types that contain fields
    that are either irrelevant to the L2 or are not yet supported. *)

open Tezlink_imports

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

(* We are introducing a mock baker account at a fixed address to answer TzKT requests
   about baking rights. This account has all baking rights for all cycles. For now, it
   is at a fixed address; in the future, it could be replaced by the address of the
   sequencer so that it can collect the DA-fee part of operation fees. *)
let baker_initial_balance = 200000000000L

let baker_initial_deposit =
  Imported_context.Tez.of_mutez_exn baker_initial_balance

let faucet_public_key_hash = Signature.V2.Public_key_hash.zero

type account = {
  pkh : Imported_context.public_key_hash;
  pk : Imported_context.public_key;
  balance : int64;
}

let baker_account =
  let public_key_internal =
    let pk_opt =
      Tezos_crypto.Signature.Ed25519.Public_key.of_bytes_without_validation
        (Bytes.make 32 '\000')
    in
    match pk_opt with None -> (* Unreachable *) assert false | Some pk -> pk
  in
  let public_key : Imported_context.public_key = Ed25519 public_key_internal in
  let public_key_hash : Imported_context.public_key_hash =
    Ed25519 (Tezos_crypto.Signature.Ed25519.Public_key.hash public_key_internal)
  in
  {
    pkh = public_key_hash;
    pk = public_key;
    (* This amount was arbitrarly chosen according to bootstrap account
         in L1 sandbox *)
    balance = baker_initial_balance;
  }

let contents : Imported_context.Block_header.contents =
  {
    payload_hash = Imported_protocol.Block_payload_hash.zero;
    payload_round = Imported_context.Round.zero;
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

(* When indexing Tezlink, Tzkt requires Liquidity baking contracts. *)
module Liquidity_baking = struct
  let cpmm_address = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

  let lqt_address = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

  let lq_fallback_token = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN"

  let cpmm_script : Imported_context.Script.t =
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

  let token_script : Imported_context.Script.t =
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

  let fallback_script : Imported_context.Script.t =
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
let mocked_script (contract : Imported_context.Contract.t) =
  let address = Imported_context.Contract.to_b58check contract in
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
        @@ Imported_protocol.Tez_repr.of_mutez baker_initial_balance
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
               () )
           -> {delegate_sampler_state; selected_stake_distribution})
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

(* Voting period type t in `Imported_context.mli` is a private type so we need to create our own
   and then do the conversion with the serialization. *)
module Voting_period = struct
  type t = {
    index : int32;
    kind : Imported_context.Voting_period.kind;
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
            Imported_context.Voting_period.kind_encoding)
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
      ~dst:Imported_context.Voting_period.encoding
end

let balance_udpdate_rewards ~(baker : Imported_context.public_key_hash) ~amount
    =
  let open Imported_context.Receipt in
  let debited_rewards =
    item Baking_rewards (Debited amount) Block_application
  in
  let baker = frozen_baker baker in
  let credited_rewards =
    item (Deposits baker) (Credited amount) Block_application
  in
  [debited_rewards; credited_rewards]

let storage_cycle () =
  let consensus_pk =
    Imported_protocol.Raw_context.
      {
        delegate = baker_account.pkh;
        consensus_pk = baker_account.pk;
        consensus_pkh = baker_account.pkh;
        companion_pk = None;
        companion_pkh = None;
      }
  in
  let delegate_sampler_state =
    Storage_repr.Cycle.create_sample_state
      ~consensus_pks:[(consensus_pk, baker_initial_balance)]
  in
  let selected_stake_distribution = Storage_repr.Cycle.stake ~consensus_pk in
  Lwt_result.return
    Storage_repr.Cycle.{delegate_sampler_state; selected_stake_distribution}

(* This is a way to obtain a context built on genesis with the minimum of
   block. This is not to be used if the context is actually relevant, but just
   as a dummy value for specific cases, for Quick and Dirty wins. *)
let init_dummy_context () =
  let open Imported_protocol_test_helpers in
  let open Lwt_result_syntax in
  let* b, _cs = Context.init1 () in
  let* v = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt v in
  let ctxt = Imported_context.Gas.set_unlimited ctxt in
  return ctxt

(* This an implementation taken straight from the plugin, which assumes the
   context is actually irrelevant. A better solution would be to go through
   rust bindings to the mir implementation, but this a quick win. *)
let list_entrypoints code normalize_types =
  let open Imported_protocol_test_helpers in
  let open Lwt_result_wrap_syntax in
  let open Imported_context in
  let open Imported_protocol in
  let* ctxt = init_dummy_context () in
  let expr = Script.lazy_expr code in
  (* The following is taken verbatim from the implementation of the contract
     services in the plugin:
         src/proto_023_PtSeouLo/lib_plugin/contract_services.ml
  *)
  let legacy = true in
  let open Script_ir_translator in
  let*?@ expr, _ =
    Script.force_decode_in_context
      ~consume_deserialization_gas:When_needed
      ctxt
      expr
  in
  let*@ {arg_type; _}, ctxt = parse_toplevel ctxt expr in
  Lwt.return
  @@ Imported_env.wrap_tzresult (* we wrap the Error_monad result*)
       (let open Result_syntax in
        let* Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _ =
          Script_ir_translator.parse_parameter_ty_and_entrypoints
            ctxt
            ~legacy
            arg_type
        in
        let unreachable_entrypoint, map =
          list_entrypoints_uncarbonated arg_type entrypoints
        in
        let* entrypoint_types, _ctxt =
          Entrypoint.Map.fold_e
            (fun entry
                 (Script_typed_ir.Ex_ty ty, original_type_expr)
                 (acc, ctxt)
               ->
              let* ty_expr, ctxt =
                let open Tezos_micheline in
                if normalize_types then
                  let* ty_node, ctxt =
                    Script_ir_unparser.unparse_ty ~loc:() ctxt ty
                  in
                  return (Micheline.strip_locations ty_node, ctxt)
                else return (Micheline.strip_locations original_type_expr, ctxt)
              in
              return ((Entrypoint.to_string entry, ty_expr) :: acc, ctxt))
            map
            ([], ctxt)
        in
        return_some (unreachable_entrypoint, entrypoint_types))

let pack_data ~data ~ty ~gas =
  let open Imported_protocol_test_helpers in
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_dummy_context () in
  let*@ result =
    Imported_protocol_plugin.RPC.Scripts.pack_data_impl
      ~allow_forged_lazy_storage_id:false
      ctxt
      ~gas
      ~data
      ~ty
  in
  return result
