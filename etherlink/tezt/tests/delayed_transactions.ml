(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(* Copyright (c) 2024-2026 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Smart Optimistic Rollups: Etherlink Sequencer
   Requirement:  make -f etherlink.mk build
                 npm install eth-cli solc@0.8.31
                 # Install cast or foundry (see: https://book.getfoundry.sh/getting-started/installation)
                 curl -L https://foundry.paradigm.xyz | bash
                 foundryup
                 make fa-bridge-watchtower
                 ./scripts/install_dal_trusted_setup.sh
                 # Install websocat (see: https://github.com/vi/websocat?tab=readme-ov-file#installation)
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file delayed_transactions.ml
   Subject:      Transactions going through the delayed inbox.
*)

open Rpc.Syntax
open Transaction
open Delayed_inbox
open Test_helpers
open Setup
module Protocol = Test_helpers.Protocol_no_direct_register

let test_send_transaction_to_delayed_inbox =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"]
    ~title:"Send a transaction to the delayed inbox"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; kernel; _}
      _protocol
    ->
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let send ~amount ?expect_failure () =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~amount
      ?expect_failure
      raw_transfer
  in
  (* Test that paying less than 1XTZ is not allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "0.9") ~expect_failure:true ()
  in
  (* Test the correct case where the user burns 1XTZ to send the transaction. *)
  let* hash = send ~amount:Tez.one ~expect_failure:false () in
  (* Assert that the expected transaction hash is found in the delayed inbox
     durable storage path. *)
  let* () =
    Delayed_inbox.assert_mem ~kernel (Sc_rollup_node sc_rollup_node) hash
  in
  (* Test that paying more than 1XTZ is allowed. *)
  let* _hash =
    send ~amount:(Tez.parse_floating "1.1") ~expect_failure:false ()
  in
  unit

let test_send_deposit_to_delayed_inbox =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "deposit"]
    ~title:"Send a deposit to the delayed inbox"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; kernel; _}
      _protocol
    ->
  let amount = Tez.of_int 16 in
  let depositor = Constant.bootstrap5 in
  let receiver =
    Eth_account.
      {
        address = "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
        private_key =
          "0xb7c548b5442f5b28236f0dcd619f65aaaafd952240908adcf9642d8e616587ee";
      }
  in
  let deposit_info =
    {receiver = EthereumAddr receiver.address; chain_id = None}
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount
      ~bridge:l1_contracts.bridge
      ~depositor
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  let* delayed_transactions_hashes =
    Delayed_inbox.content ~kernel (Sc_rollup_node sc_rollup_node)
  in
  let* deposit =
    Delayed_inbox.data
      ~kernel
      (Sc_rollup_node sc_rollup_node)
      (List.hd delayed_transactions_hashes)
  in

  let deposit_bytes = Hex.to_bytes (`Hex (Option.get deposit)) in

  match Evm_node_lib_dev_encoding.Rlp.decode deposit_bytes with
  | Ok
      Evm_node_lib_dev_encoding.Rlp.(
        List (List (Value tag :: List (Value amnt :: Value rcvr :: _) :: _) :: _))
    ->
      let deposit_tag = tag |> Hex.of_bytes |> Hex.show in
      Check.((deposit_tag = "02") string)
        ~error_msg:"Expected tag %R, but got: %L" ;
      let receiver = rcvr |> Hex.of_bytes |> Hex.show in
      Check.((receiver = "1074fd1ec02cbeaa5a90450505cf3b48d834f3eb") string)
        ~error_msg:"Expected receiver %R, but got: %L" ;
      let amount = amnt |> Hex.of_bytes |> Hex.show in
      (* 16000000000000000000 big endian *)
      Check.((amount = "de0b6b3a76400000") string)
        ~error_msg:"Expected amount %R, but got: %L" ;
      unit
  | _ -> failwith "invalid delayed inbox item"

let test_delayed_transfer_is_included =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Delayed transaction is included"
    ~time_between_blocks:Nothing
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        observer;
        _;
      }
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
  let* tx_hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller sender balance (before: %L < after: %R)" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger receiver balance (before: %L > after: %R)" ;
  let*@! (_receipt : Transaction.transaction_receipt) =
    Rpc.get_transaction_receipt ~tx_hash observer
  in
  unit

(* Minimal RLP surgery on a *signed legacy* transaction, used to exhibit EVM
   tx-hash malleability.

   [decode_compressed_h256] (etherlink/kernel_latest/ethereum/src/rlp_helpers.rs)
   decodes the signature scalars [r]/[s] without a minimality check: it accepts a
   zero-padded encoding and zero-extends it back to the same [H256]. The kernel,
   however, sets [tx_hash = Keccak256(raw_bytes_as_received)]. So re-encoding [r]
   (or [s]) one byte longer yields a byte-different transaction that decodes to
   the exact same logical transaction (same [v/r/s], same recovered sender, valid
   signature) but hashes to a different [tx_hash]. *)
module Malleable_rlp = struct
  (* Big-endian encoding of a non-negative int, no leading zeros (empty for 0). *)
  let be_of_int n =
    let rec aux acc n =
      if n = 0 then acc
      else aux (String.make 1 (Char.chr (n land 0xff)) ^ acc) (n lsr 8)
    in
    aux "" n

  let int_of_be s =
    let n = ref 0 in
    String.iter (fun c -> n := (!n lsl 8) lor Char.code c) s ;
    !n

  (* Canonical RLP encoding of a byte-string. *)
  let encode_string s =
    let n = String.length s in
    if n = 1 && Char.code s.[0] <= 0x7f then s
    else if n <= 55 then String.make 1 (Char.chr (0x80 + n)) ^ s
    else
      let lb = be_of_int n in
      String.make 1 (Char.chr (0xb7 + String.length lb)) ^ lb ^ s

  (* RLP encoding of a list from its already-encoded items. *)
  let encode_list items =
    let payload = String.concat "" items in
    let n = String.length payload in
    if n <= 55 then String.make 1 (Char.chr (0xc0 + n)) ^ payload
    else
      let lb = be_of_int n in
      String.make 1 (Char.chr (0xf7 + String.length lb)) ^ lb ^ payload

  (* Split a flat RLP list (a signed legacy tx) into its item *contents*. *)
  let decode_legacy_items s =
    let b0 = Char.code s.[0] in
    let payload_off, payload_len =
      if b0 >= 0xc0 && b0 <= 0xf7 then (1, b0 - 0xc0)
      else if b0 >= 0xf8 then
        let ll = b0 - 0xf7 in
        (1 + ll, int_of_be (String.sub s 1 ll))
      else Test.fail "Malleable_rlp: not an RLP list"
    in
    let stop = payload_off + payload_len in
    let rec items off acc =
      if off >= stop then List.rev acc
      else
        let b = Char.code s.[off] in
        let content, next =
          if b <= 0x7f then (String.sub s off 1, off + 1)
          else if b <= 0xb7 then
            let len = b - 0x80 in
            (String.sub s (off + 1) len, off + 1 + len)
          else if b <= 0xbf then
            let ll = b - 0xb7 in
            let len = int_of_be (String.sub s (off + 1) ll) in
            (String.sub s (off + 1 + ll) len, off + 1 + ll + len)
          else Test.fail "Malleable_rlp: unexpected nested list"
        in
        items next (content :: acc)
    in
    items payload_off []

  (* [malleate hex] takes the canonical hex encoding (no ["0x"]) of a signed
     legacy transaction and returns an alternative encoding of the *same* logical
     transaction with different raw bytes, by prepending a zero byte to whichever
     of [r]/[s] is shorter than 32 bytes (i.e. has a stripped leading zero).
     Returns [None] when both are a full 32 bytes (not paddable within the
     32-byte budget the decoder accepts). *)
  let malleate hex =
    let raw = Bytes.to_string (Hex.to_bytes (`Hex hex)) in
    match decode_legacy_items raw with
    | [nonce; gas_price; gas; to_; value; data; v; r; s] ->
        let pad x = "\000" ^ x in
        let items' =
          if String.length r < 32 then
            Some [nonce; gas_price; gas; to_; value; data; v; pad r; s]
          else if String.length s < 32 then
            Some [nonce; gas_price; gas; to_; value; data; v; r; pad s]
          else None
        in
        Option.map
          (fun items' ->
            let encoded = encode_list (List.map encode_string items') in
            let (`Hex h) = Hex.of_string encoded in
            h)
          items'
    | _ -> Test.fail "Malleable_rlp: expected a 9-field legacy transaction"
end

let test_tx_hash_malleability =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "malleability"]
    ~title:"Non-canonical r/s encoding is rejected"
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  (* 1. Craft a signed legacy transfer whose canonical encoding admits a
        non-canonical (zero-padded [r]/[s]) twin.

        [cast] uses deterministic (RFC-6979) ECDSA, so for these exact parameters
        the signature is fixed. Grinding the transfer value offline shows
        [1 ETH + 138 wei] is the smallest value whose signature has a 31-byte [s]
        scalar (a stripped leading zero) -- i.e. re-encodable one byte longer. We
        hardcode it to avoid searching at runtime; [malleate] re-derives the twin
        and asserts the property still holds. *)
  let* canonical_raw =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:Wei.(one_eth + (one * Z.of_int 138))
      ~address:receiver
      ()
  in
  let twin_raw =
    match Malleable_rlp.malleate canonical_raw with
    | Some twin -> twin
    | None ->
        Test.fail
          "expected the crafted transaction to have a paddable r/s (its s \
           scalar should be 31 bytes); cast's signing may have changed"
  in
  (* The two encodings are genuinely different bytes... *)
  Check.((canonical_raw <> twin_raw) string)
    ~error_msg:"expected the canonical and malleated encodings to differ" ;
  let hash_of raw =
    "0x"
    ^ (`Hex raw |> Hex.to_bytes |> Tezos_crypto.Hacl.Hash.Keccak_256.digest
     |> Hex.of_bytes |> Hex.show)
  in
  let canonical_hash = hash_of canonical_raw in
  let twin_hash = hash_of twin_raw in
  (* ...so their keccak (= the kernel's tx-hash) differ too. *)
  Check.((canonical_hash <> twin_hash) string)
    ~error_msg:"expected the canonical and malleated tx-hashes to differ" ;
  (* 2. Submit the malleated encoding through the (unconditional) delayed inbox.
        Posting the bytes to the L1 bridge always succeeds; what matters is what
        the kernel does with them. *)
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
  let* delayed_hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      twin_raw
  in
  (* Were it ingested, the delayed inbox would key it by keccak(raw bytes), i.e.
     the malleated hash. *)
  Check.(("0x" ^ delayed_hash = twin_hash) string)
    ~error_msg:"the delayed-inbox hash %L should be the malleated hash %R" ;
  (* 3. Regression guard: the kernel fails to decode the non-canonical [s] and
        drops the transaction while parsing the delayed message, so it is never
        added to the delayed inbox. On the buggy (pre-fix) kernel the malleated
        transaction would be ingested here and this assertion would fail. *)
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  (* 4. Consequently nothing executed: balances are untouched and the transaction
        is retrievable under neither hash. *)
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:
      "expected the sender balance to be unchanged (before %L, after %R)" ;
  Check.((receiver_balance_prev = receiver_balance_next) Wei.typ)
    ~error_msg:
      "expected the receiver balance to be unchanged (before %L, after %R)" ;
  let*@ receipt_twin =
    Rpc.get_transaction_receipt ~tx_hash:twin_hash sequencer
  in
  (match receipt_twin with
  | None -> ()
  | Some _ ->
      Test.fail
        "the malleated tx %s must not have been included (it is non-canonical)"
        twin_hash) ;
  let*@ receipt_canonical =
    Rpc.get_transaction_receipt ~tx_hash:canonical_hash sequencer
  in
  (match receipt_canonical with
  | None -> ()
  | Some _ ->
      Test.fail
        "the canonical tx %s was never submitted and must not exist"
        canonical_hash) ;
  unit

let test_delayed_typed_transaction_is_reconstructed =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "reconstruct"; "eip1559"]
    ~title:"Delayed EIP-1559 transaction is reconstructed with its type"
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Craft an EIP-1559 (type [0x2]) transfer and submit it through the delayed
     inbox. The raw transaction of a delayed transaction is *not* part of the
     sequencer blueprint payload, so the EVM node must reconstruct its type from
     the separately stored raw delayed transaction. Without that reconstruction,
     the transaction object is returned as a bare legacy object: no [type] field
     and [v] equal to the raw y_parity ([0x0]/[0x1]), which is rejected by strict
     Ethereum deserializers. *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~legacy:false
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* tx_hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  let tx_hash = "0x" ^ tx_hash in
  let check_typed ~rpc tx_object =
    Check.(
      (JSON.(tx_object |-> "type" |> as_string_opt) = Some "0x2")
        (option string))
      ~error_msg:(rpc ^ ": expected delayed transaction type %R, got %L") ;
    (* EIP-1559 typed transactions legitimately carry [v = 0x0]/[0x1], but they
       must be tagged with their [type]; the EIP-1559 fields must be present. *)
    Check.(
      (JSON.(tx_object |-> "maxFeePerGas" |> as_string_opt) <> None)
        (option string))
      ~error_msg:(rpc ^ ": expected a maxFeePerGas field on the EIP-1559 tx")
  in
  (* [eth_getTransactionByHash] exercises the [find_object] read path. *)
  let* tx_object =
    let* json =
      Evm_node.(
        jsonrpc
          sequencer
          {
            method_ = "eth_getTransactionByHash";
            parameters = `A [`String tx_hash];
          })
    in
    return (Evm_node.extract_result json)
  in
  check_typed ~rpc:"eth_getTransactionByHash" tx_object ;
  (* [eth_getBlockByNumber] with full transaction objects exercises the
     [block_with_objects] read path. *)
  let block_number = JSON.(tx_object |-> "blockNumber" |> as_string) in
  let* block =
    let* json =
      Evm_node.(
        jsonrpc
          sequencer
          {
            method_ = "eth_getBlockByNumber";
            parameters = `A [`String block_number; `Bool true];
          })
    in
    return (Evm_node.extract_result json)
  in
  let tx_object_in_block =
    JSON.(block |-> "transactions" |> as_list)
    |> List.find (fun tx -> JSON.(tx |-> "hash" |> as_string) = tx_hash)
  in
  check_typed ~rpc:"eth_getBlockByNumber" tx_object_in_block ;
  unit

let test_largest_delayed_transfer_is_included =
  register_all
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "inclusion"]
    ~title:"Largest possible delayed transaction is included"
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let _endpoint = Evm_node.endpoint sequencer in
  (* This is the largest ethereum transaction we transfer via the bridge contract. *)
  let max_data = String.make 64896 '0' in
  let* transfer_that_fits =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1
      ~nonce:2323
      ~gas_price:10_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~arguments:[max_data]
      ()
  in
  let len_transfer_that_fits = String.length transfer_that_fits / 2 in
  Log.info "Maximum size allowed is %d" len_transfer_that_fits ;
  (* We assert that this is the largest by sending a transaction that is 1 byte
     larger, the protocol refuses it. *)
  let data_too_big = max_data ^ "00" in
  let* transfer_too_big =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1
      ~nonce:2323
      ~gas_price:10_000_000_000
      ~gas:21_000
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ~arguments:[data_too_big]
      ()
  in
  let len_transfer_too_big = String.length transfer_too_big / 2 in
  assert (len_transfer_that_fits + 1 = len_transfer_too_big) ;
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~expect_failure:true
      transfer_too_big
  in
  (* Now we check that the largest possible transaction is included by the sequencer. *)
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~expect_failure:false
      transfer_that_fits
  in
  let* () =
    wait_for_delayed_inbox_add_tx_and_injected
      ~sequencer
      ~sc_rollup_node
      ~client
  in
  let* () = bake_until_sync ~sc_rollup_node ~sequencer ~client () in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in
  unit

let test_delayed_transaction_peeked =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "failure"]
    ~title:"Delayed deposit is removed only when it is applied"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@
  fun {
        client;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        kernel;
        _;
      }
      _protocol
    ->
  (* This test act as an non-regression test.

     The unwanted behavior was :
       - A blueprint containing a deposit triggers an upgrade
       - The kernel reboots and upgrade
       - The blueprint is popped again and fails to find the deposit

     To mitigate this problem until the kernel is patched we don't put
     transactions in a block that triggers an upgrade. In this test we
     are going to play with this property to force an upgrade with a
     blueprint that contains a deposit.
  *)
  let deposit_info =
    {
      receiver = EthereumAddr "0x1074Fd1EC02cbeaa5A90450505cF3B48D834f3EB";
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* We bake enough blocks for the sequencer to realize there's a deposit. *)
  let* () =
    repeat 2 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Send an upgrade to the rollup node, but don't finalize the block, so the
     sequencer doesn't see the upgrade. *)
  let* _root_hash =
    upgrade
      ~sc_rollup_node
      ~sc_rollup_address
      ~admin:Constant.bootstrap2.public_key_hash
      ~admin_contract:l1_contracts.admin
      ~client
      ~upgrade_to:(Kernel.to_uses kernel)
      ~activation_timestamp:"0"
  in
  (* Produce a block. The sequencer will include the deposit, but won't upgrade.
     The rollup node will upgrade as soon as it receives the block. *)
  let*@ _ = produce_block sequencer in
  bake_until_sync ~sc_rollup_node ~client ~sequencer ()

let test_invalid_delayed_transaction =
  register_all
    ~__FILE__
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "invalid"]
    ~title:"Delayed transaction is removed even when invalid"
    ~enable_fa_bridge:false
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Produces an invalid transaction by setting an invalid nonce. *)
  let* invalid_nonce =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:16
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      ~amount:Tez.one
      invalid_nonce
  in
  (* Assert that the expected transaction hash is found in the delayed inbox
     durable storage path. *)
  let* () = Delayed_inbox.assert_mem (Sc_rollup_node sc_rollup_node) hash in

  (* We bake enough blocks for the sequencer to see the transaction and inject
     it. *)
  let* () =
    bake_until
      ~bake:(fun () ->
        let*@ _ = produce_block sequencer in
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
      ~result_f:(fun () ->
        let* size = Delayed_inbox.size (Sc_rollup_node sc_rollup_node) in
        if size = 0 then return (Some ()) else return None)
      ()
  in
  let* () = Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node) in

  (* There is no receipt for the transaction because it's invalid. *)
  let*@ receipt_opt = Rpc.get_transaction_receipt ~tx_hash:hash sequencer in
  Check.is_true
    (receipt_opt = None)
    ~error_msg:"The transaction should not have a receipt" ;
  unit

let test_init_from_rollup_node_with_delayed_inbox =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~kernels:[Kernel.Latest]
    ~tags:["evm"; "rollup_node"; "init"; "delayed_inbox"; "omit"]
    ~title:
      "Init evm node sequencer data dir from a rollup node data dir with \
       delayed items"
  @@
  fun {
        sc_rollup_node;
        sequencer;
        observer;
        client;
        l1_contracts;
        sc_rollup_address;
        _;
      }
      _protocol
    ->
  (* The sequencer is needed to produce an initial block for the init from
     rollup node to work. *)
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = Evm_node.terminate sequencer in
  let* () = Evm_node.terminate observer in
  (* Sends a deposit to the delayed inbox. *)
  let deposit_info =
    {
      receiver = EthereumAddr "0xB7A97043983f24991398E5a82f63F4C58a417185";
      chain_id = None;
    }
  in
  let* () =
    send_deposit_to_delayed_inbox
      ~amount:Tez.one
      ~bridge:l1_contracts.bridge
      ~depositor:Constant.bootstrap5
      ~deposit_info
      ~sc_rollup_node
      ~sc_rollup_address
      client
  in
  (* Finalize the transaction. *)
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in

  (* Start a new sequencer, the previous sequencer is doomed. *)
  let sequencer =
    Evm_node.create
      ~node_setup:(Evm_node.make_setup ())
      ~mode:(Evm_node.mode sequencer)
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config sequencer in
  let* () = Evm_node.init_from_rollup_node_data_dir sequencer sc_rollup_node in
  let* () = Evm_node.run sequencer in
  (* The sequencer should have items in its delayed inbox. *)
  let* delayed_inbox_size = Delayed_inbox.size (Evm_node sequencer) in
  Check.((delayed_inbox_size = 1) int)
    ~error_msg:"The sequencer should have the delayed inbox in its state" ;
  (* Start a new observer, we will ask it to omit the delayed transactions. *)
  let observer =
    Evm_node.create
      ~mode:
        (Evm_node.Observer
           {
             rollup_node_endpoint = Some (Sc_rollup_node.endpoint sc_rollup_node);
             evm_node_endpoint = Evm_node.endpoint sequencer;
           })
      ()
  in
  let* () = Process.check @@ Evm_node.spawn_init_config observer in
  let* () =
    Evm_node.init_from_rollup_node_data_dir
      ~omit_delayed_tx_events:true
      observer
      sc_rollup_node
  in
  let* () = Evm_node.run observer in
  let* () = Evm_node.wait_for_drift_monitor_ready observer in
  let* () = Delayed_inbox.assert_empty (Evm_node observer) in

  (* Finally produce a block to clear the delayed inbox. *)
  let*@ len = produce_block sequencer in
  Check.((len = 1) int) ~error_msg:"Expected one transaction in the block" ;
  let* _ = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  let* () = check_head_consistency ~left:sequencer ~right:observer () in
  unit

let test_delayed_transfer_timeout =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:1
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed transaction timeout"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        _;
      }
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* () =
    bake_until_blueprint_forced ~sc_rollup_node ~evm_node:sequencer ~client ()
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_forced_blueprint_takes_pred_timestamp =
  register_all
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take predecessor timestamp"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* The head timestamp will be high enough that we don't use the L1 timestamp
     for the forced blueprint and just take the same timestamp. *)
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:04:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Make a delayed transaction and force it by creating L1 blocks. *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* () =
    bake_until_blueprint_forced ~sc_rollup_node ~evm_node:sequencer ~client ()
  in
  (* The sequencer has applied the force blueprint, it's latest is the
     forced blueprint *)
  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  Check.(
    (Tezos_base.Time.Protocol.to_notation sequencer_head.timestamp
    = "2020-01-01T00:04:00Z")
      string)
    ~error_msg:
      "The forced blueprint should have the same timestamp as its predecessor" ;
  let* l1_timestamp = l1_timestamp client in
  Check.(
    (Tezos_base.Time.Protocol.to_seconds sequencer_head.timestamp
    > Tezos_base.Time.Protocol.to_seconds l1_timestamp)
      int64)
    ~error_msg:"The proxy should have taken a timestamp greater than L1 one" ;
  unit

let test_forced_blueprint_takes_l1_timestamp =
  register_all
    ~__FILE__
    ~kernels:[Kernel.Latest]
    ~time_between_blocks:Nothing
    ~genesis_timestamp:Client.(At (Time.of_notation_exn "2020-01-01T00:00:00Z"))
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:1
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Forced blueprint can take l1 timestamp"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  let*@ (_ : int) = produce_block ~timestamp:"2020-01-01T00:00:00Z" sequencer in
  let* () = bake_until_sync ~sc_rollup_node ~client ~sequencer () in
  (* Make a delayed transaction and force it by creating L1 blocks. *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  let* l1_timestamp = l1_timestamp client in
  let* () =
    bake_until_blueprint_forced ~sc_rollup_node ~evm_node:sequencer ~client ()
  in

  let*@ sequencer_head = Rpc.get_block_by_number ~block:"latest" sequencer in
  (* The forced block will have a timestamp of l1_timestamp. *)
  Check.(
    (Tezos_base.Time.Protocol.to_notation sequencer_head.timestamp
    = Tezos_base.Time.Protocol.to_notation l1_timestamp)
      string)
    ~error_msg:
      "Forced blueprint should have timestamp of L1. (proxy has %L, l1 has %R)" ;
  unit

let test_delayed_transfer_timeout_fails_l1_levels =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:3
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"; "min_levels"]
    ~title:"Delayed transaction timeout considers l1 level"
    ~use_dal:ci_enabled_dal_registration
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        _;
      }
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
  (* This is a transfer from Eth_account.bootstrap_accounts.(0) to
     Eth_account.bootstrap_accounts.(1). *)
  let* raw_transfer =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      raw_transfer
  in
  (* Bake a few blocks, should be enough for the tx to time out in terms
     of wall time, but not in terms of L1 levels.
     Note that this test is almost the same as the one where the tx
     times out, only difference being the value of [delayed_inbox_min_levels].
  *)
  let* _ =
    repeat 5 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev = sender_balance_next) Wei.typ)
    ~error_msg:"Sender balance should be the same (prev %L = next %R)" ;
  Check.((receiver_balance_prev = receiver_balance_next) Wei.typ)
    ~error_msg:"Receiver balance should be the same (prev %L = next %R)" ;
  (* Wait until it's forced *)
  let* () =
    bake_until_blueprint_forced ~sc_rollup_node ~evm_node:sequencer ~client ()
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller sender balance (prev %L > next %R)" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger receiver balance (next %L > prev %R)" ;
  unit

let test_external_transaction_to_delayed_inbox_fails =
  (* We have a da_fee set to zero here. This is because the proxy will perform
     validation on the tx before adding it the transaction pool. This will fail
     due to 'gas limit too low' if the da fee is set.

     Since we want to test what happens when the tx is actually submitted, we
     bypass the da fee check here. *)
  register_all
    ~__FILE__
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~time_between_blocks:Nothing
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "external"]
    ~title:"Sending an external transaction to the delayed inbox fails"
  @@ fun {client; sequencer; sc_rollup_node; sc_rollup_address; _} _protocol ->
  let raw_tx, tx_hash = read_tx_from_file () |> List.hd in
  let* _ =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.post_local_batcher_injection
       (* See Message_format.frame_message for the used encoding *)
         ~messages:["\000" ^ sc_rollup_address ^ "\000" ^ tx_hash ^ raw_tx]
         ()
  in
  (* Bake enough levels to make sure the transaction would be processed
     if added *)
  let* () =
    repeat 10 (fun () ->
        let*@ _ = produce_block sequencer in
        let* _ = Rollup.next_rollup_node_level ~client ~sc_rollup_node in
        unit)
  in
  (* Response should be none *)
  let*@ response = Rpc.get_transaction_receipt ~tx_hash sequencer in
  assert (Option.is_none response) ;
  unit

let test_delayed_inbox_flushing =
  (* Setup with a short wall time timeout but a significant lower bound of
     L1 levels needed for timeout.
     The idea is to send 2 transactions to the delayed inbox, having one
     time out and check that the second is also forced.
     We set [delayed_inbox_min_levels] to a value that is large enough
     to give us time to send the second one while the first one is not
     timed out yet.
  *)
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~delayed_inbox_timeout:1
    ~delayed_inbox_min_levels:20
    ~da_fee:arb_da_fee_for_delayed_inbox
    ~tags:["evm"; "sequencer"; "delayed_inbox"; "timeout"]
    ~title:"Delayed inbox flushing"
  @@
  fun {
        client;
        node = _;
        l1_contracts;
        sc_rollup_address;
        sc_rollup_node;
        sequencer;
        _;
      }
      _protocol
    ->
  let endpoint = Evm_node.endpoint sequencer in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let sender = Eth_account.bootstrap_accounts.(0).address in
  let receiver = Eth_account.bootstrap_accounts.(1).address in
  let* sender_balance_prev = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_prev = Eth_cli.balance ~account:receiver ~endpoint () in
  (* Thix crafted tx comes from [100-inputs-for-proxy] (the first one),
     but the signer is not the same so the tx is different on the signature *)
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:21_000
      ~value:(Wei.of_string "10")
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx1
  in
  (* Bake a few blocks but not enough for the first tx to be forced! *)
  let* _ =
    repeat 10 (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in
  (* Send the second transaction, a transfer from
     Eth_account.bootstrap_accounts.(0) to Eth_account.bootstrap_accounts.(1).
  *)
  let* tx2 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:23_300
      ~value:(Wei.of_eth_int 1)
      ~address:Eth_account.bootstrap_accounts.(1).address
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx2
  in
  (* Bake a few more blocks to make sure the first tx times out, but not
     the second one. However, the latter should also be included. *)
  let* () =
    bake_until_blueprint_forced ~sc_rollup_node ~evm_node:sequencer ~client ()
  in
  let* sender_balance_next = Eth_cli.balance ~account:sender ~endpoint () in
  let* receiver_balance_next = Eth_cli.balance ~account:receiver ~endpoint () in
  Check.((sender_balance_prev <> sender_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((receiver_balance_prev <> receiver_balance_next) Wei.typ)
    ~error_msg:"Balance should be updated" ;
  Check.((sender_balance_prev > sender_balance_next) Wei.typ)
    ~error_msg:"Expected a smaller balance" ;
  Check.((receiver_balance_next > receiver_balance_prev) Wei.typ)
    ~error_msg:"Expected a bigger balance" ;
  unit

let test_blueprint_limit_with_delayed_inbox =
  register_all
    ~__FILE__
    ~eth_bootstrap_accounts:Eth_account.lots_of_address
    ~sequencer:Constant.bootstrap1
    ~time_between_blocks:Nothing
    ~max_number_of_chunks:2
    ~minimum_base_fee_per_gas:base_fee_for_hardcoded_tx
    ~tags:["evm"; "sequencer"; "blueprint"; "limit"; "delayed"]
    ~title:
      "Checks the sequencer doesn't produce blueprint bigger than the given \
       maximum number of chunks and count delayed transactions size in the \
       blueprint"
    ~use_dal:ci_enabled_dal_registration
      (* IC set to false because transaction dropping mechanism from IC workflow invalidates it:
      Delayed transactions are included by remaining common transaction are not *)
    ~instant_confirmations:false
  @@
  fun {sc_rollup_node; client; sequencer; sc_rollup_address; l1_contracts; _}
      _protocol
    ->
  let txs = read_tx_from_file () |> List.map (fun (tx, _hash) -> tx) in
  (* The first 3 transactions will be sent to the delayed inbox *)
  let delayed_txs, direct_txs = Tezos_base.TzPervasives.TzList.split_n 3 txs in
  let send_to_delayed_inbox (sender, raw_tx) =
    send_raw_transaction_to_delayed_inbox
      ~wait_for_next_level:false
      ~sender
      ~sc_rollup_node
      ~sc_rollup_address
      ~client
      ~l1_contracts
      raw_tx
  in
  let* delayed_hashes =
    Lwt_list.map_s send_to_delayed_inbox
    @@ List.combine
         [Constant.bootstrap2; Constant.bootstrap3; Constant.bootstrap4]
         delayed_txs
  in
  (* Ensures the transactions are added to the rollup delayed inbox and picked
     by the sequencer *)
  let* () =
    repeat 4 (fun () ->
        let* _l1_level =
          Rollup.next_rollup_node_level ~sc_rollup_node ~client
        in
        unit)
  in
  let* _requests, _hashes =
    batch_n_transactions ~evm_node:sequencer direct_txs
  in
  (* Due to the overapproximation of 4096 bytes per delayed transactions, there
     should be only a single delayed transaction per blueprints with 2 chunks. *)
  let* _ = next_evm_level ~evm_node:sequencer in
  let* _ = next_evm_level ~evm_node:sequencer in
  let* _ = next_evm_level ~evm_node:sequencer in
  (* Checks the delayed transactions and at least the first transaction from the
     batch have been applied *)
  let* block_numbers =
    Lwt_list.map_s
      (fun tx_hash ->
        let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
        match receipt with
        | None -> Test.fail "Delayed transaction hasn't be included"
        | Some receipt -> return receipt.blockNumber)
      delayed_hashes
  in
  let check_block_contains_delayed_transaction_and_transactions
      (delayed_hash, block_number) =
    let*@ block =
      Rpc.get_block_by_number ~block:(Int32.to_string block_number) sequencer
    in
    match block.Block.transactions with
    | Block.Empty -> Test.fail "Block shouldn't be empty"
    | Block.Full _ ->
        Test.fail "Block is supposed to contain only transaction hashes"
    | Block.Hash hashes ->
        if not (List.mem ("0x" ^ delayed_hash) hashes && 2 < List.length hashes)
        then
          Test.fail
            "The delayed transaction %s hasn't been included in the expected \
             block along other transactions from the pool"
            delayed_hash ;
        unit
  in
  Lwt_list.iter_s check_block_contains_delayed_transaction_and_transactions
  @@ List.combine delayed_hashes block_numbers

let test_produce_block_with_no_delayed_transactions =
  register_all
    ~__FILE__
    ~time_between_blocks:Nothing
    ~tags:["evm"; "delayed_transaction"]
    ~title:"Produce block with no delayed transactions"
  @@
  fun {client; l1_contracts; sc_rollup_address; sc_rollup_node; sequencer; _}
      _protocol
    ->
  (* Send a random transaction in the delayed inbox. *)
  let* tx1 =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:21_000
      ~gas:21_000
      ~value:(Wei.of_string "10")
      ~address:"0x0000000000000000000000000000000000000000"
      ()
  in
  let* _hash =
    send_raw_transaction_to_delayed_inbox
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sc_rollup_address
      tx1
  in
  (* Finalize the transaction so the sequencer sees the event. *)
  let* l1_level = Client.level client in
  let wait_for =
    Evm_node.wait_for_processed_l1_level ~level:l1_level sequencer
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  let* _ = wait_for in

  let*@ n = Rpc.produce_block ~with_delayed_transactions:false sequencer in
  Check.((n = 0) int) ~error_msg:"Block should be empty but got %L transactions" ;
  let*@ n = Rpc.produce_block sequencer in
  Check.((n = 1) int) ~error_msg:"Block should have one transaction but got %L" ;

  unit

let protocols = [Protocol.Alpha]

let () =
  test_send_transaction_to_delayed_inbox protocols ;
  test_send_deposit_to_delayed_inbox protocols ;
  test_delayed_transfer_is_included protocols ;
  test_tx_hash_malleability protocols ;
  test_delayed_typed_transaction_is_reconstructed protocols ;
  test_delayed_transaction_peeked protocols ;
  test_invalid_delayed_transaction protocols ;
  test_largest_delayed_transfer_is_included protocols ;
  test_init_from_rollup_node_with_delayed_inbox protocols ;
  test_external_transaction_to_delayed_inbox_fails protocols ;
  test_delayed_transfer_timeout protocols ;
  test_delayed_transfer_timeout_fails_l1_levels protocols ;
  test_forced_blueprint_takes_pred_timestamp protocols ;
  test_forced_blueprint_takes_l1_timestamp protocols ;
  test_delayed_inbox_flushing protocols ;
  test_blueprint_limit_with_delayed_inbox protocols ;
  test_produce_block_with_no_delayed_transactions protocols
