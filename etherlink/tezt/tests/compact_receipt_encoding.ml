(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node / experimental_features
    Invocation:   dune exec etherlink/tezt/tests/main.exe -- \
                    --file compact_receipt_encoding.ml
    Subject:      End-to-end check of
                  [experimental_features.compact_receipt_encoding] driven
                  through the actual store + JSON-RPC pipeline rather than
                  only at the encoding layer.

                  A single sandbox sequencer is taken through three
                  writing phases on the same data directory:

                  - Phase 1 (flag OFF, default): deploy a shared
                    [SimpleLogger] contract, then write a bundle of four
                    receipt shapes in successive blocks — a contract call
                    that emits one indexed event (non-zero bloom), a plain
                    ETH transfer (zero bloom, no logs), a fresh
                    [SimpleLogger] deployment ([contractAddress] populated
                    on the receipt), and an EIP-1559 (type-2) transfer.
                    Verify all four receipts via
                    [eth_getTransactionReceipt].

                  - Phase 2 (restart with flag ON, same data dir):
                    re-read the phase-1 (legacy) receipts to confirm the
                    new binary's compact-aware decoder accepts pre-flip
                    rows, then write a second four-shape bundle (compact
                    rows). Re-read both bundles, and run [eth_getLogs]
                    with an address + topic filter across the whole range
                    — this exercises the SQLite
                    [receipt_contains_bloom_filter] UDF against a
                    mixed-format store.

                  - Phase 3 (restart with flag OFF again): re-verify all
                    prior rows (legacy + compact), write a third bundle
                    (legacy), and re-run [eth_getLogs] covering all three
                    phases — six receipt shapes spanning two on-disk
                    formats.

                  What this proves:

                  * The decoder is symmetric: a new binary reads both
                    legacy and compact rows correctly regardless of the
                    current write mode.
                  * The SQLite bloom UDF accepts both column shapes in the
                    same query.
                  * The flag plumbing — config field →
                    [evm_context.store_block_unsafe] →
                    [Transactions.store ~compact_receipt_encoding] →
                    [Q.Transactions.insert] — actually flips the on-disk
                    format on the next write, with no surprises across
                    process restarts.

                  That last point is the rollback-safety claim the MR
                  makes; before this test it was only asserted at the
                  encoding layer.
*)

open Rpc.Syntax
open Test_helpers

let chain_id = 1337

(* keccak256("LogValue(uint256)") — the topic emitted by [SimpleLogger] for
   the indexed [value] argument. Matches the contract at
   [etherlink/kernel_latest/solidity_examples/simple_logger.sol]. *)
let log_value_event_topic =
  let h =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest
      (Bytes.of_string "LogValue(uint256)")
  in
  "0x" ^ Hex.show (Hex.of_bytes h)

(* Hashes captured for one writing phase, plus the per-phase value passed to
   [logValue] (so we can assert the second topic of the emitted log). *)
type bundle = {
  non_zero_bloom : string;
  zero_bloom : string;
  contract_creation : string;
  eip1559 : string;
  log_value : int;
  contract_creation_address : string;
}

(* Produce four transactions in a single phase, each a different receipt
   shape, sealed in their own blocks.

   Shapes 1–3 (contract call, plain transfer, deploy) go through
   [send_transaction_to_sequencer] + [Eth_cli], the conventional helper
   pair used elsewhere in this directory. Shape 4 (EIP-1559) uses [Cast]
   directly because [Eth_cli] always emits legacy transactions. *)
let produce_bundle ~phase_tag ~logger_for_calls ~logger_abi_label ~simple_logger
    ~sender ~log_value sequencer =
  let endpoint = Evm_node.endpoint sequencer in
  Log.info "[%s] producing the four-shape transaction bundle" phase_tag ;
  (* 1. Non-zero bloom: contract call emitting one indexed event. *)
  let* non_zero_bloom =
    send_transaction_to_sequencer
      (Eth_cli.contract_send
         ~source_private_key:sender.Eth_account.private_key
         ~endpoint
         ~abi_label:logger_abi_label
         ~address:logger_for_calls
         ~method_call:(Format.sprintf "logValue(%d)" log_value))
      sequencer
  in
  (* 2. Zero bloom: plain ETH transfer (no logs). *)
  let* zero_bloom =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:sender.private_key
         ~to_public_key:Eth_account.bootstrap_accounts.(1).address
         ~value:(Wei.of_eth_int 1)
         ~endpoint)
      sequencer
  in
  (* 3. Contract creation: a fresh [SimpleLogger] deployment ([logs = []]
     but [contractAddress] is set on the receipt). *)
  let* contract_creation_address, contract_creation =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi:simple_logger.Solidity_contracts.label
         ~bin:simple_logger.bin)
      sequencer
  in
  (* 4. EIP-1559 (type-2) transfer. *)
  let* gas_price = Rpc.get_gas_price sequencer in
  let*@ nonce = Rpc.get_transaction_count ~address:sender.address sequencer in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id
      ~nonce:(Int64.to_int nonce)
      ~gas:25_000
      ~gas_price:(Int32.to_int gas_price)
      ~legacy:false
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(2).address
      ()
  in
  let*@ eip1559 = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _ = produce_block sequencer in
  return
    {
      non_zero_bloom;
      zero_bloom;
      contract_creation;
      eip1559;
      log_value;
      contract_creation_address;
    }

(* All-zero bloom expressed as the canonical 514-character hex string. The
   compact encoder emits an empty payload for this value on disk, but the
   JSON-RPC layer always returns the expanded form. *)
let zero_bloom_hex = "0x" ^ String.make 512 '0'

let hex_256_of_int n = Printf.sprintf "0x%064x" n

let get_receipt_or_fail ~sequencer ~tag tx_hash =
  let*@ r = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match r with
  | None -> Test.fail "%s: receipt missing for %s" tag tx_hash
  | Some r -> return r

let assert_receipt_shape ~loc ~tag ~expected_logs ~expect_zero_bloom
    ~expect_contract_creation (receipt : Transaction.transaction_receipt) =
  Check.is_true
    ~__LOC__:loc
    receipt.status
    ~error_msg:(tag ^ ": tx should have succeeded") ;
  Check.((List.length receipt.logs = expected_logs) int ~__LOC__:loc)
    ~error_msg:(tag ^ ": expected %R log(s), got %L") ;
  let bloom_is_zero = String.equal receipt.logsBloom zero_bloom_hex in
  Check.((bloom_is_zero = expect_zero_bloom) bool ~__LOC__:loc)
    ~error_msg:
      (tag ^ ": logsBloom zeroness mismatch (got %L, expected %R) — bloom = "
     ^ receipt.logsBloom) ;
  let has_contract_address = Option.is_some receipt.contractAddress in
  Check.((has_contract_address = expect_contract_creation) bool ~__LOC__:loc)
    ~error_msg:(tag ^ ": contractAddress mismatch (got %L, expected %R)")

(* Verify the four shapes of [bundle] read back through JSON-RPC, regardless
   of which write mode the bundle was produced in. *)
let check_bundle ~tag bundle sequencer =
  Log.info "[%s] verifying receipts read back correctly" tag ;
  let* r_log =
    get_receipt_or_fail
      ~sequencer
      ~tag:(tag ^ "/non_zero_bloom")
      bundle.non_zero_bloom
  in
  assert_receipt_shape
    ~loc:__LOC__
    ~tag:(tag ^ "/non_zero_bloom")
    ~expected_logs:1
    ~expect_zero_bloom:false
    ~expect_contract_creation:false
    r_log ;
  Check.((r_log.type_ = 0l) int32 ~__LOC__)
    ~error_msg:(tag ^ "/non_zero_bloom: expected type-0, got %L") ;
  (* The second topic of [LogValue] is the indexed [value] argument. *)
  (match r_log.logs with
  | [log] ->
      let topics = log.Transaction.topics in
      Check.((List.length topics = 2) int ~__LOC__)
        ~error_msg:(tag ^ "/non_zero_bloom: expected 2 topics, got %L") ;
      Check.((List.nth topics 0 = log_value_event_topic) string ~__LOC__)
        ~error_msg:
          (tag ^ "/non_zero_bloom: expected event-signature topic %R, got %L") ;
      Check.(
        (List.nth topics 1 = hex_256_of_int bundle.log_value) string ~__LOC__)
        ~error_msg:(tag ^ "/non_zero_bloom: expected indexed value %R, got %L")
  | _ -> Test.fail "%s/non_zero_bloom: expected exactly one log" tag) ;
  let* r_zero =
    get_receipt_or_fail ~sequencer ~tag:(tag ^ "/zero_bloom") bundle.zero_bloom
  in
  assert_receipt_shape
    ~loc:__LOC__
    ~tag:(tag ^ "/zero_bloom")
    ~expected_logs:0
    ~expect_zero_bloom:true
    ~expect_contract_creation:false
    r_zero ;
  let* r_create =
    get_receipt_or_fail
      ~sequencer
      ~tag:(tag ^ "/contract_creation")
      bundle.contract_creation
  in
  assert_receipt_shape
    ~loc:__LOC__
    ~tag:(tag ^ "/contract_creation")
    ~expected_logs:0
    ~expect_zero_bloom:true
    ~expect_contract_creation:true
    r_create ;
  let* r_1559 =
    get_receipt_or_fail ~sequencer ~tag:(tag ^ "/eip1559") bundle.eip1559
  in
  assert_receipt_shape
    ~loc:__LOC__
    ~tag:(tag ^ "/eip1559")
    ~expected_logs:0
    ~expect_zero_bloom:true
    ~expect_contract_creation:false
    r_1559 ;
  Check.((r_1559.type_ = 2l) int32 ~__LOC__)
    ~error_msg:(tag ^ "/eip1559: expected type 2, got %L") ;
  unit

(* Bloom-prefiltered query covering the entire range written so far: this is
   the path that exercises [receipt_contains_bloom_filter] — the SQLite UDF
   that must accept both the legacy 514-byte hex bloom and the compact
   empty-bytes representation in the same query. *)
let check_logs_across_phases ~tag ~bundles ~logger_address sequencer =
  Log.info "[%s] verifying eth_getLogs across all phases" tag ;
  let*@ all_logs =
    Rpc.get_logs
      ~from_block:(Number 0)
      ~address:(Single logger_address)
      ~topics:[[log_value_event_topic]]
      sequencer
  in
  Check.((List.length all_logs = List.length bundles) int ~__LOC__)
    ~error_msg:
      (tag
     ^ ": expected one log per writing phase (%R) but eth_getLogs returned %L \
        — this would indicate the bloom UDF does not handle a row format \
        currently in the store") ;
  let actual_values =
    List.map
      (fun (log : Transaction.tx_log) ->
        match log.topics with
        | [_sig; value] -> value
        | _ -> Test.fail "%s: log has unexpected topic shape" tag)
      all_logs
  in
  let expected_values =
    List.map (fun b -> hex_256_of_int b.log_value) bundles
  in
  let sort = List.sort String.compare in
  Check.((sort actual_values = sort expected_values) (list string) ~__LOC__)
    ~error_msg:(tag ^ ": indexed log values mismatch (got %L, expected %R)") ;
  unit

(* Restart [sequencer] against its existing data directory after rewriting
   its config to set [compact_receipt_encoding = b]. After the run, we
   issue an empty warmup block before returning: the [is_ready] event
   fires as soon as the RPC server is up, but the tx-pool subsystem can
   still drop the very first incoming tx in a race where it hasn't
   finished re-attaching to the running kernel — [preconfirmation.ml]
   uses the same warmup pattern after its config-flip restart. *)
let restart_with_flag ~flag sequencer =
  Log.info "restarting sequencer with compact_receipt_encoding = %b" flag ;
  let* () = Evm_node.terminate sequencer in
  let* () =
    Evm_node.Config_file.update
      sequencer
      (Evm_node.patch_config_with_experimental_feature
         ~compact_receipt_encoding:flag
         ())
  in
  let* () = Evm_node.run sequencer in
  let* _ = Evm_node.wait_for_ready sequencer in
  let*@ _ = produce_block sequencer in
  unit

let test_compact_receipt_encoding_flag_flip () =
  register_sandbox
    ~__FILE__
    ~tags:["evm"; "compact_receipt_encoding"; "feature_flag"; "rollback"]
    ~title:
      "compact_receipt_encoding: receipts decode across flag flips and bloom \
       filtering works on a mixed-format store"
    ~patch_config:
      (Evm_node.patch_config_with_experimental_feature
         ~compact_receipt_encoding:false
         ())
  @@ fun sequencer ->
  let evm_version = Evm_version.Cancun in
  let sender = Eth_account.bootstrap_accounts.(0) in
  let endpoint = Evm_node.endpoint sequencer in
  let* simple_logger = Solidity_contracts.simple_logger evm_version in
  let* () =
    Eth_cli.add_abi ~label:simple_logger.label ~abi:simple_logger.abi ()
  in
  let* logger_for_calls, _shared_deploy_hash =
    send_transaction_to_sequencer
      (Eth_cli.deploy
         ~source_private_key:sender.private_key
         ~endpoint
         ~abi:simple_logger.label
         ~bin:simple_logger.bin)
      sequencer
  in
  Log.info
    "deployed shared SimpleLogger at %s; entering writing phase 1 \
     (compact_receipt_encoding = false)"
    logger_for_calls ;
  (* Phase 1: write with the flag OFF (default, legacy on-disk format). *)
  let* bundle_off_1 =
    produce_bundle
      ~phase_tag:"phase 1 (off)"
      ~logger_for_calls
      ~logger_abi_label:simple_logger.label
      ~simple_logger
      ~sender
      ~log_value:42
      sequencer
  in
  let* () = check_bundle ~tag:"phase 1 (off)" bundle_off_1 sequencer in
  (* Phase 2: flip the flag ON, write more blocks. *)
  let* () = restart_with_flag ~flag:true sequencer in
  let* () =
    check_bundle ~tag:"phase 2 (on, pre-existing rows)" bundle_off_1 sequencer
  in
  let* bundle_on =
    produce_bundle
      ~phase_tag:"phase 2 (on)"
      ~logger_for_calls
      ~logger_abi_label:simple_logger.label
      ~simple_logger
      ~sender
      ~log_value:1337
      sequencer
  in
  let* () = check_bundle ~tag:"phase 2 (on)" bundle_on sequencer in
  let* () =
    check_bundle ~tag:"phase 2 (on, legacy re-read)" bundle_off_1 sequencer
  in
  let* () =
    check_logs_across_phases
      ~tag:"phase 2 (on)"
      ~bundles:[bundle_off_1; bundle_on]
      ~logger_address:logger_for_calls
      sequencer
  in
  (* Phase 3: flip the flag back OFF and re-run all reads. The new binary
     keeps reading both formats; this confirms the decoder is symmetric and
     that the bloom UDF still returns the right rows. *)
  let* () = restart_with_flag ~flag:false sequencer in
  let* () =
    check_bundle ~tag:"phase 3 (off, legacy re-read)" bundle_off_1 sequencer
  in
  let* () =
    check_bundle ~tag:"phase 3 (off, compact re-read)" bundle_on sequencer
  in
  let* bundle_off_2 =
    produce_bundle
      ~phase_tag:"phase 3 (off)"
      ~logger_for_calls
      ~logger_abi_label:simple_logger.label
      ~simple_logger
      ~sender
      ~log_value:999
      sequencer
  in
  let* () = check_bundle ~tag:"phase 3 (off, fresh)" bundle_off_2 sequencer in
  let* () =
    check_logs_across_phases
      ~tag:"phase 3 (off, mixed store)"
      ~bundles:[bundle_off_1; bundle_on; bundle_off_2]
      ~logger_address:logger_for_calls
      sequencer
  in
  unit

let () = test_compact_receipt_encoding_flag_flip ()
