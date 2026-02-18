(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   dune exec etherlink/bin_node/test/test_blueprint_roundtrip.exe
    Subject:      Tests for Blueprints producer and decoder
*)

open Evm_node_lib_dev
open Evm_node_lib_dev_encoding

let hash_typ = Check.(convert Ethereum_types.hash_to_string string)

let block_hash_typ = Check.(convert Ethereum_types.block_hash_to_bytes string)

let timestamp_typ = Check.(convert Time.Protocol.to_seconds int64)

let bytes_typ =
  Check.comparable
    (fun fmt bytes -> Hex.pp fmt (Hex.of_bytes bytes))
    Bytes.compare

let common_tx_typ = Check.(convert Sequencer_blueprint.tag_transaction string)

let register ?(tags = []) =
  Test.register
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
    ~tags:("blueprint" :: "roundtrip" :: tags)

let expect_ok msg = function Ok x -> x | Error _err -> Test.fail msg

let wallet () =
  let open Tezos_client_base in
  let open Tezos_client_base_unix in
  let base_dir = Temp.dir "wallet" in
  let wallet_ctxt =
    new Client_context_unix.unix_io_wallet ~base_dir ~password_filename:None
  in
  let () =
    Client_main_run.register_default_signer
      (wallet_ctxt :> Client_context.io_wallet)
  in
  wallet_ctxt

(* generated with [octez-client]

   - octez-client --endpoint https://rpc.tzkt.io/ghostnet gen keys for-tests
   - octez-client --endpoint https://rpc.tzkt.io/ghostnet show address for-tests -S *)
let sequencer_key =
  expect_ok "Cannot create a sequencer key"
  @@ Tezos_client_base.Client_keys.make_sk_uri
       (Uri.of_string
          "unencrypted:edsk4AHKqBSzpkmZ39EMBJ2Tm1SHu6JUYTnp5vApYPR6cvr9iqbELa")

let zero_hash =
  Evm_node_lib_dev_encoding.Ethereum_types.decode_block_hash
  @@ Bytes.make 32 '\000'

let smart_rollup_address =
  Tezos_crypto.Hashed.Smart_rollup_address.(zero |> to_string)

let make_blueprint ~delayed_transactions ~transactions ~version =
  let cctxt = wallet () in
  let chunks =
    Sequencer_blueprint.make_blueprint_chunks
      ~number:(Qty Z.zero)
      {
        version;
        parent_hash = zero_hash;
        delayed_transactions;
        transactions;
        timestamp = Time.Protocol.epoch;
      }
  in

  let* chunks =
    Sequencer_blueprint.sign ~signer:(Signer.wallet cctxt sequencer_key) ~chunks
  in
  let blueprint =
    Sequencer_blueprint.create_inbox_payload
      ~smart_rollup_address
      ~chunks:(expect_ok "could not prepare a blueprint" chunks)
  in
  return blueprint

let hash_tez_block ~encode block_without_hash =
  let block_bytes =
    Bytes.of_string
    @@ expect_ok "could not encode the tez block"
    @@ encode block_without_hash
  in
  let block_hash = Block_hash.hash_bytes [block_bytes] in
  Ethereum_types.decode_block_hash (Block_hash.to_bytes block_hash)

let test_blueprint_roundtrip ~title ~delayed_transactions ~transactions ~version
    () =
  register ~title:(sf "Blueprint producer decoder roundtrip (%s)" title)
  @@ fun () ->
  let* blueprint =
    make_blueprint ~delayed_transactions ~transactions ~version
  in
  let decoding_result =
    expect_ok "could not decode the blueprint"
    @@ Blueprint_decoder.transactions blueprint
  in
  let delayed_transactions_decoded, transactions_decoded =
    List.partition_either
    @@ List.map
         (function _, Some x -> Either.Right x | x, None -> Left x)
         decoding_result
  in
  Check.(
    (delayed_transactions_decoded = delayed_transactions)
      (list hash_typ)
      ~error_msg:"Wrong decoded of delayed transactions: got %L instead of %R") ;
  Check.(
    (transactions_decoded = transactions)
      (list common_tx_typ)
      ~error_msg:"Wrong decoded of delayed transactions: got %L instead of %R") ;
  unit

(* Bundles a tez block version with a set of utility and check functions *)
module type TEZOS_BLOCK_TOOL = sig
  type t

  val name : string

  val encode_block_for_store : t -> (string, string) result

  val check_match : expected:t -> L2_types.Tezos_block.t -> unit
end

let test_tez_block_roundtrip (type block)
    (module Block_tool : TEZOS_BLOCK_TOOL with type t = block) ~title
    (block : block) =
  register
    ~title:(sf "%s producer decoder roundtrip (%s)" Block_tool.name title)
  @@ fun () ->
  let encoding_result =
    expect_ok "could not encode the tez block"
    @@ Block_tool.encode_block_for_store block
  in
  let decoding_result =
    expect_ok "could not decode the tez block"
    @@ L2_types.Tezos_block.decode_block_for_store encoding_result
  in
  Block_tool.check_match ~expected:block decoding_result ;
  unit

let protocol_typ =
  Check.equalable
    (fun fmt protocol ->
      let string_protocol =
        match protocol with
        | L2_types.Tezos_block.Protocol.S023 -> "S023"
        | L2_types.Tezos_block.Protocol.T024 -> "T024"
      in
      Format.pp_print_string fmt string_protocol)
    ( = )

module Tezos_block_latest_tool = struct
  include L2_types.Tezos_block

  let name = "Tez block"

  let make ~level ~timestamp ~parent_hash ~protocol ~next_protocol ~operations
      () =
    let hash = zero_hash in
    let block_without_hash =
      {level; hash; timestamp; parent_hash; protocol; next_protocol; operations}
    in
    let hash =
      hash_tez_block ~encode:encode_block_for_store block_without_hash
    in
    {block_without_hash with hash}

  let check_match ~(expected : t)
      L2_types.Tezos_block.
        {
          level;
          hash;
          timestamp;
          parent_hash;
          protocol;
          next_protocol;
          operations;
        } =
    Check.(
      (level = expected.level)
        int32
        ~error_msg:"Wrong decoded of number for block: got %L instead of %R") ;
    Check.(
      (timestamp = expected.timestamp)
        timestamp_typ
        ~error_msg:"Wrong decoded of timestamp for block: got %L instead of %R") ;
    Check.(
      (parent_hash = expected.parent_hash)
        block_hash_typ
        ~error_msg:
          "Wrong decoded of parent_hash for block: got %L instead of %R") ;
    Check.(
      (hash = expected.hash)
        block_hash_typ
        ~error_msg:"Wrong decoded of hash for block: got %L instead of %R") ;
    Check.(
      (protocol = expected.protocol)
        protocol_typ
        ~error_msg:"Wrong decoded of protocol for block: got %L instead of %R") ;
    Check.(
      (next_protocol = expected.next_protocol)
        protocol_typ
        ~error_msg:
          "Wrong decoded of next-protocol for block: got %L instead of %R") ;
    Check.(
      (operations = expected.operations)
        bytes_typ
        ~error_msg:"Wrong decoded of operations for block: got %L instead of %R") ;
    ()
end

let test_tez_latest_block_roundtrip =
  test_tez_block_roundtrip (module Tezos_block_latest_tool)

module Tezos_block_v0_tool = struct
  include L2_types.Tezos_block.Internal_for_test.V0

  let name = "Tez V0 block"

  let make ~level ~timestamp ~parent_hash ~operations () =
    let hash = zero_hash in
    let block_without_hash =
      {level; hash; timestamp; parent_hash; operations}
    in
    let hash =
      hash_tez_block ~encode:encode_block_for_store block_without_hash
    in
    {block_without_hash with hash}

  let check_match ~(expected : t)
      L2_types.Tezos_block.
        {
          level;
          hash;
          timestamp;
          parent_hash;
          protocol;
          next_protocol;
          operations;
        } =
    Check.(
      (expected.level = level)
        int32
        ~error_msg:"Wrong decoded of number for block: got %L instead of %R") ;
    Check.(
      (timestamp = expected.timestamp)
        timestamp_typ
        ~error_msg:"Wrong decoded of timestamp for block: got %L instead of %R") ;
    Check.(
      (parent_hash = expected.parent_hash)
        block_hash_typ
        ~error_msg:
          "Wrong decoded of parent_hash for block: got %L instead of %R") ;
    Check.(
      (hash = expected.hash)
        block_hash_typ
        ~error_msg:"Wrong decoded of hash for block: got %L instead of %R") ;
    Check.(
      (protocol = L2_types.Tezos_block.Protocol.S023)
        protocol_typ
        ~error_msg:"Wrong decoded of protocol for block: got %L instead of %R") ;
    Check.(
      (next_protocol = L2_types.Tezos_block.Protocol.S023)
        protocol_typ
        ~error_msg:
          "Wrong decoded of next-protocol for block: got %L instead of %R") ;
    Check.(
      (operations = expected.operations)
        bytes_typ
        ~error_msg:"Wrong decoded of operations for block: got %L instead of %R") ;
    ()
end

let test_tez_v0_block_roundtrip =
  test_tez_block_roundtrip (module Tezos_block_v0_tool)

module Tezos_block_legacy_tool = struct
  include Tezos_block_v0_tool
  include L2_types.Tezos_block.Internal_for_test.Legacy

  let name = "Tez legacy block"

  let make ~level ~timestamp ~parent_hash ~operations () =
    let hash = zero_hash in
    let block_without_hash =
      {level; hash; timestamp; parent_hash; operations}
    in
    let hash =
      hash_tez_block ~encode:encode_block_for_store block_without_hash
    in
    {block_without_hash with hash}
end

let test_tez_legacy_block_roundtrip =
  test_tez_block_roundtrip (module Tezos_block_legacy_tool)

let () =
  test_blueprint_roundtrip
    ~title:"empty legacy blueprint"
    ~delayed_transactions:[]
    ~transactions:[]
    ~version:Legacy
    () ;

  test_blueprint_roundtrip
    ~title:"empty V1 blueprint"
    ~delayed_transactions:[]
    ~transactions:[]
    ~version:V1
    () ;

  test_blueprint_roundtrip
    ~title:"only transactions, legacy"
    ~delayed_transactions:[]
    ~transactions:[Evm "txntxntxn"; Evm "txntxntxntxn"]
    ~version:Legacy
    () ;

  test_blueprint_roundtrip
    ~title:"only delayed transactions, legacy"
    ~delayed_transactions:
      [
        Ethereum_types.hash_raw_tx "txntxntxntxn";
        Ethereum_types.hash_raw_tx "txntxntxn";
      ]
    ~transactions:[]
    ~version:Legacy
    () ;

  test_blueprint_roundtrip
    ~title:"both delayed and regular transactions, legacy"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:[Evm "txntxntxn"; Evm "txntxntxntxn"]
    ~version:Legacy
    () ;

  test_blueprint_roundtrip
    ~title:"both delayed and regular transactions, V1"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:[Evm "txntxntxn"; Michelson "txntxntxntxn"]
    ~version:V1
    () ;

  test_blueprint_roundtrip
    ~title:"large transaction, Legacy"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:
      [Evm "txntxntxn"; Evm "txntxntxntxn"; Evm (String.make 10_000 't')]
    ~version:Legacy
    () ;

  test_blueprint_roundtrip
    ~title:"large transaction, V1"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:
      [Evm "txntxntxn"; Michelson "txntxntxntxn"; Evm (String.make 10_000 't')]
    ~version:V1
    () ;

  test_tez_latest_block_roundtrip ~title:"all zeros tez block"
  @@ Tezos_block_latest_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:zero_hash
       ~protocol:S023
       ~next_protocol:S023
       ~operations:Bytes.empty
       () ;

  test_tez_latest_block_roundtrip ~title:"genesis successor"
  @@ Tezos_block_latest_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~protocol:S023
       ~next_protocol:S023
       ~operations:Bytes.empty
       () ;

  test_tez_latest_block_roundtrip ~title:"with operations"
  @@ Tezos_block_latest_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~protocol:S023
       ~next_protocol:S023
       ~operations:(Bytes.of_string "txntxntxn")
       () ;

  test_tez_latest_block_roundtrip ~title:"T024 protocol"
  @@ Tezos_block_latest_tool.make
       ~level:10l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~protocol:T024
       ~next_protocol:T024
       ~operations:Bytes.empty
       () ;

  test_tez_latest_block_roundtrip ~title:"S023 to T024 transition"
  @@ Tezos_block_latest_tool.make
       ~level:10l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~protocol:S023
       ~next_protocol:T024
       ~operations:Bytes.empty
       () ;

  test_tez_v0_block_roundtrip ~title:"all zeros tez block"
  @@ Tezos_block_v0_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:zero_hash
       ~operations:Bytes.empty
       () ;

  test_tez_v0_block_roundtrip ~title:"genesis successor"
  @@ Tezos_block_v0_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~operations:Bytes.empty
       () ;

  test_tez_v0_block_roundtrip ~title:"with operations"
  @@ Tezos_block_v0_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~operations:(Bytes.of_string "txntxntxn")
       () ;

  test_tez_legacy_block_roundtrip ~title:"all zeros tez block"
  @@ Tezos_block_legacy_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:zero_hash
       ~operations:Bytes.empty
       () ;

  test_tez_legacy_block_roundtrip ~title:"genesis successor"
  @@ Tezos_block_legacy_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~operations:Bytes.empty
       () ;

  test_tez_legacy_block_roundtrip ~title:"with operations"
  @@ Tezos_block_legacy_tool.make
       ~level:0l
       ~timestamp:Time.Protocol.epoch
       ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
       ~operations:(Bytes.of_string "txntxntxn")
       ()

let () = Test.run ()
