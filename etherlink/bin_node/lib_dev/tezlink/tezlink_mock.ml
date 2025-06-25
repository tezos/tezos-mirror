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
    Bytes.make 4 '\255';
  ]

(* TODO #7866
   When blocks are populated, this mock value will be unnecessary,
   being replaced by actual data. *)
let context = Context_hash.of_bytes_exn (Bytes.make 32 '\255')

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
