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

let make_blueprint ~delayed_transactions ~transactions =
  let cctxt = wallet () in
  let* chunks =
    Sequencer_blueprint.prepare
      ~signer:(Signer.wallet cctxt sequencer_key)
      ~timestamp:Time.Protocol.epoch
      ~number:(Qty Z.zero)
      ~parent_hash:zero_hash
      ~delayed_transactions
      ~transactions
  in
  let blueprint =
    Sequencer_blueprint.create_inbox_payload
      ~smart_rollup_address
      ~chunks:(expect_ok "could not prepare a blueprint" chunks)
  in
  return blueprint

let make_tez_block ~level ~timestamp ~parent_hash () =
  let block_without_hash =
    L2_types.Tezos_block.
      {
        level;
        hash = zero_hash;
        timestamp;
        parent_hash;
        operations = Bytes.empty;
      }
  in
  let block_bytes =
    Bytes.of_string
    @@ expect_ok "could not encode the tez block"
    @@ L2_types.Tezos_block.encode_block block_without_hash
  in
  let block_hash = Block_hash.hash_bytes [block_bytes] in
  let hash =
    Ethereum_types.decode_block_hash (Block_hash.to_bytes block_hash)
  in
  return
    L2_types.Tezos_block.
      {level; hash; timestamp; parent_hash; operations = Bytes.empty}

let test_blueprint_roundtrip ~title ~delayed_transactions ~transactions () =
  register ~title:(sf "Blueprint producer decoder roundtrip (%s)" title)
  @@ fun () ->
  let* blueprint = make_blueprint ~delayed_transactions ~transactions in
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
      (list string)
      ~error_msg:"Wrong decoded of delayed transactions: got %L instead of %R") ;
  unit

let test_tez_block_roundtrip ~title ~level ~timestamp ~parent_hash () =
  register ~title:(sf "Tez block producer decoder roundtrip (%s)" title)
  @@ fun () ->
  let* block = make_tez_block ~level ~timestamp ~parent_hash () in
  let encoding_result =
    expect_ok "could not encode the tez block"
    @@ L2_types.Tezos_block.encode_block block
  in
  let decoding_result =
    expect_ok "could not decode the tez block"
    @@ L2_types.Tezos_block.decode_block encoding_result
  in
  Check.(
    (decoding_result.level = block.level)
      int32
      ~error_msg:"Wrong decoded of number for block: got %L instead of %R") ;
  Check.(
    (decoding_result.timestamp = block.timestamp)
      (convert Time.Protocol.to_seconds int64)
      ~error_msg:"Wrong decoded of timestamp for block: got %L instead of %R") ;
  Check.(
    (Ethereum_types.block_hash_to_bytes decoding_result.parent_hash
    = Ethereum_types.block_hash_to_bytes block.parent_hash)
      string
      ~error_msg:"Wrong decoded of parent_hash for block: got %L instead of %R") ;
  Check.(
    (Ethereum_types.block_hash_to_bytes decoding_result.hash
    = Ethereum_types.block_hash_to_bytes block.hash)
      string
      ~error_msg:"Wrong decoded of hash for block: got %L instead of %R") ;
  unit

let () =
  test_blueprint_roundtrip
    ~title:"empty blueprint"
    ~delayed_transactions:[]
    ~transactions:[]
    () ;

  test_blueprint_roundtrip
    ~title:"only transactions"
    ~delayed_transactions:[]
    ~transactions:["txntxntxn"; "txntxntxntxn"]
    () ;

  test_blueprint_roundtrip
    ~title:"only delayed transactions"
    ~delayed_transactions:
      [
        Ethereum_types.hash_raw_tx "txntxntxntxn";
        Ethereum_types.hash_raw_tx "txntxntxn";
      ]
    ~transactions:[]
    () ;

  test_blueprint_roundtrip
    ~title:"both delayed and regular transactions"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:["txntxntxn"; "txntxntxntxn"]
    () ;

  test_blueprint_roundtrip
    ~title:"large transaction"
    ~delayed_transactions:[Ethereum_types.hash_raw_tx "txntxntxntxn"]
    ~transactions:["txntxntxn"; "txntxntxntxn"; String.make 10_000 't']
    () ;

  test_tez_block_roundtrip
    ~title:"all zeros tez block"
    ~level:0l
    ~timestamp:Time.Protocol.epoch
    ~parent_hash:zero_hash
    () ;

  test_tez_block_roundtrip
    ~title:"genesis successor"
    ~level:0l
    ~timestamp:Time.Protocol.epoch
    ~parent_hash:L2_types.Tezos_block.genesis_parent_hash
    ()

let () = Test.run ()
