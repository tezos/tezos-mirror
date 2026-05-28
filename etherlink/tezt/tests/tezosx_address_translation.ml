(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file tezosx_address_translation.ml

   Tests for the `originOf` and `resolveAddress` address-translation APIs
   from both call-sites:

   - EVM precompile:
       `originOf(string addr, uint8 sourceRuntime) → (uint8 kind, uint8
        homeRuntime, string nativeAddress)`
       `resolveAddress(string addr, uint8 sourceRuntime, uint8 targetRuntime)
        → (bool classified, uint8 res, string translated)`

   - Michelson synthetic views on the TezosX Gateway:
       `VIEW "originOf"` input `pair string nat`, return `or unit (or nat
        (pair nat string))`
       `VIEW "resolveAddress"` input `pair string (pair nat nat)`, return
        `option (pair nat string)`

   Runtime IDs on the wire: 0 = Tezos, 1 = Ethereum.
*)

open Rpc.Syntax

(* ── Shared constants ─────────────────────────────────────────────────── *)

(** EVM precompile address for cross-runtime gateway calls. *)
let evm_gateway_address = "0xff00000000000000000000000000000000000007"

let runtime_tags = List.map Tezosx_runtime.tag

(* Wire encoding for runtime IDs. *)
let runtime_id_tezos = "0"

let runtime_id_ethereum = "1"

(** 4-byte selector for the Solidity custom error `InvalidRuntimeId(uint8)`.
    Computed as keccak256("InvalidRuntimeId(uint8)")[0..4] = 0xe85c2efe.
    Source: `sol!` macro expansion in
    etherlink/kernel_latest/revm/src/precompiles/runtime_gateway.rs,
    validated by the `invalid_runtime_id_payload_shape` unit test (line ~1551). *)
let invalid_runtime_id_selector = "e85c2efe"

(* ── EVM eth_call helpers ─────────────────────────────────────────────── *)

(** Issue a read-only [eth_call] to the EVM gateway precompile.

    Uses an explicit gas limit of 0xf4240 (1M) so the kernel's gas ↔
    milligas conversion does not overflow the per-operation Michelson limit. *)
let eth_call_gateway ~sequencer ~calldata =
  let* response =
    Evm_node.call_evm_rpc
      sequencer
      Evm_node.
        {
          method_ = "eth_call";
          parameters =
            `A
              [
                `O
                  [
                    ("to", `String evm_gateway_address);
                    ("data", `String calldata);
                    ("gas", `String "0xf4240");
                  ];
                `String "latest";
              ];
        }
  in
  return response

(** [eth_call_gateway_ok ~sequencer ~calldata] issues an [eth_call] and
    returns the ABI-encoded result hex string (without the leading "0x").
    Fails if the call returns a JSONRPC error. *)
let eth_call_gateway_ok ~sequencer ~calldata =
  let* response = eth_call_gateway ~sequencer ~calldata in
  let err = JSON.(response |-> "error") in
  (match JSON.unannotate err with
  | `Null -> ()
  | _ ->
      Test.fail
        "eth_call to gateway reverted unexpectedly: %s"
        (JSON.encode err)) ;
  let hex = Evm_node.extract_result response |> JSON.as_string in
  (* Drop the "0x" prefix. *)
  return (String.sub hex 2 (String.length hex - 2))

(** [eth_call_gateway_err ~sequencer ~calldata] issues an [eth_call] and
    returns the JSON error object.  Fails if the call succeeds. *)
let eth_call_gateway_err ~sequencer ~calldata =
  let* response = eth_call_gateway ~sequencer ~calldata in
  let err = JSON.(response |-> "error") in
  match JSON.unannotate err with
  | `Null ->
      Test.fail
        "eth_call to gateway succeeded but a revert was expected (result: %s)"
        (JSON.encode (Evm_node.extract_result response))
  | _ -> return err

(** [word_at hex pos] extracts the 32-byte ABI word at zero-based [pos] from
    [hex] (without the "0x" prefix) as a lowercase hex string. *)
let word_at hex pos = String.sub hex (pos * 64) 64

(** [uint_at hex pos] reads the last byte of the 32-byte ABI word at [pos]
    as an integer (uint8 ↔ uint256 with leading zeros). *)
let uint_at hex pos = int_of_string ("0x" ^ String.sub (word_at hex pos) 62 2)

(** [bool_at hex pos] reads the last byte of the 32-byte ABI word at [pos]
    as a boolean (0=false, 1=true). *)
let bool_at hex pos = uint_at hex pos <> 0

(** [string_from hex offset_word_pos] decodes an ABI-encoded [string] whose
    offset word is at [offset_word_pos].  Returns the string value. *)
let string_from hex offset_word_pos =
  let offset = int_of_string ("0x" ^ word_at hex offset_word_pos) in
  let len_pos = offset / 32 in
  let length = int_of_string ("0x" ^ word_at hex len_pos) in
  if length = 0 then ""
  else
    let data_start = (len_pos + 1) * 64 in
    Hex.to_bytes (`Hex (String.sub hex data_start (length * 2)))
    |> Bytes.to_string

(* ── Michelson contract helpers ───────────────────────────────────────── *)

let tezlink_foreign_endpoint sequencer =
  let r = Evm_node.rpc_endpoint_record sequencer in
  {r with path = "/tezlink"}

let tezlink_endpoint sequencer =
  Client.Foreign_endpoint (tezlink_foreign_endpoint sequencer)

let tezlink_client sequencer =
  Client.init ~endpoint:(tezlink_endpoint sequencer) ()

let contract_prg script_name =
  Michelson_script.find script_name Michelson_contracts.tezlink_protocol
  |> Michelson_script.path

(** Originate a Michelson contract via the tezlink client and produce a block
    on demand. Returns the originated KT1 address. *)
let originate_contract ~tez_client ~sequencer ~src ~alias ~script_name
    ~init_storage_data () =
  let* kt1 =
    Client.originate_contract
      ~alias
      ~amount:Tez.zero
      ~src:src.Account.public_key_hash
      ~burn_cap:Tez.one
      ~init:init_storage_data
      ~prg:(contract_prg script_name)
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  return kt1

(** Call a Michelson contract via the tezlink client and produce a block on
    demand. *)
let call_contract ~tez_client ~sequencer ~src ~dest ~arg_data () =
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:src.Account.alias
      ~receiver:dest
      ~arg:arg_data
      ~burn_cap:Tez.one
      tez_client
  in
  let*@ _ = Rpc.produce_block sequencer in
  unit

(** [check_contract_storage ~sequencer ~tez_client kt1 expected] reads the
    contract's storage via the typed tezlink RPC and asserts it matches the
    expected Micheline JSON. *)
let check_contract_storage ~sequencer ~tez_client kt1 expected =
  let* storage =
    Client.RPC.call ~endpoint:(tezlink_endpoint sequencer) tez_client
    @@ RPC.get_chain_block_context_contract_storage ~id:kt1 ()
  in
  let expected_json = JSON.annotate ~origin:"check_contract_storage" expected in
  Check.(
    (storage = expected_json)
      json
      ~error_msg:"Expected contract storage %R but got %L") ;
  unit

(* ── EVM contract deployment helper ──────────────────────────────────── *)

(** [deploy_minimal_contract ~sequencer ~sender ~nonce ()] deploys a minimal
    EVM contract (7-byte init code that produces a 1-byte runtime code), produces
    a block, and returns the deployed contract address.

    Deploying bytecode makes the code_hash != KECCAK_EMPTY, which is the
    condition the EVM back-stop checks to classify an address as Native. *)
let deploy_minimal_contract ~sequencer ~sender ~nonce () =
  (* Init code: PUSH1 0x01  PUSH1 0x00  MSTORE8  PUSH1 0x01  PUSH1 0x1F
                RETURN — stores byte 0x01 at mem[0] and returns 1 byte. *)
  let init_code = "0x600160005360016000f3" in
  let* raw_tx =
    Cast.craft_deploy_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce
      ~gas:100_000
      ~gas_price:1_000_000_000
      ~data:init_code
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match receipt with
  | Some {contractAddress = Some addr; status = true; _} -> return addr
  | Some {status = false; _} ->
      Test.fail "Minimal contract deployment transaction failed"
  | _ -> Test.fail "No receipt or no contract address for minimal deployment tx"

(* ── EVM precompile tests — classification paths (commit 1) ───────────── *)

(** Back-stop edge: [originOf] of an EVM address with deployed bytecode returns
    kind=1 (Native) via the EVM code-presence back-stop, without an explicit
    /origin record.  The back-stop checks [code_hash != KECCAK_EMPTY]. *)
let test_evm_origin_of_code_backstop () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:
      "originOf: EVM contract with bytecode classified as Native (code \
       back-stop)"
    ~tags:
      [
        "tezosx"; "address_translation"; "origin_of"; "native"; "evm"; "backstop";
      ]
  @@ fun sandbox ->
  (* Deploying a contract gives it a non-empty code_hash, which triggers the
     back-stop classification as Native without an /origin record. *)
  let deployer = Eth_account.bootstrap_accounts.(0) in
  let* contract_addr =
    deploy_minimal_contract ~sequencer:sandbox ~sender:deployer ~nonce:0 ()
  in
  let* calldata =
    Cast.calldata
      ~args:[contract_addr; runtime_id_ethereum]
      "originOf(string,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let kind = uint_at hex 0 in
  let home_runtime = uint_at hex 1 in
  Check.(
    (kind = 1)
      int
      ~error_msg:"Expected kind=1 (Native) via code back-stop, got %L") ;
  (* homeRuntime should be 1 = Ethereum for a native EVM contract. *)
  Check.(
    (home_runtime = 1)
      int
      ~error_msg:"Expected homeRuntime=1 (Ethereum), got %L") ;
  unit

(** Same-source short-circuit: [resolveAddress] with source = target =
    Ethereum and a valid EVM address returns classified=true, res=0 (Recorded),
    translated = input addr.  No storage read is needed. *)
let test_evm_resolve_address_same_source_ethereum () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"resolveAddress: same-source EVM address returns Recorded=addr"
    ~tags:
      ["tezosx"; "address_translation"; "resolve_address"; "same_source"; "evm"]
  @@ fun sandbox ->
  let addr = "0x1111111111111111111111111111111111111111" in
  let* calldata =
    Cast.calldata
      ~args:[addr; runtime_id_ethereum; runtime_id_ethereum]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let classified = bool_at hex 0 in
  let res = uint_at hex 1 in
  let translated = string_from hex 2 in
  Check.is_true
    ~__LOC__
    classified
    ~error_msg:"Expected classified=true for same-source EVM address" ;
  Check.(
    (res = 0) int ~error_msg:"Expected res=0 (Recorded) for same-source, got %L") ;
  Check.(
    (translated = addr)
      string
      ~error_msg:"Expected translated = input addr, got %L") ;
  unit

(** Same-source with Tezos runtime: [resolveAddress] with source =
    target = Tezos and a valid tz1 address returns Recorded=addr. *)
let test_evm_resolve_address_same_source_tezos () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"resolveAddress: same-source Tezos address returns Recorded=addr"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "resolve_address";
        "same_source";
        "tezos";
      ]
  @@ fun sandbox ->
  let addr = Constant.bootstrap1.public_key_hash in
  let* calldata =
    Cast.calldata
      ~args:[addr; runtime_id_tezos; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let classified = bool_at hex 0 in
  let res = uint_at hex 1 in
  let translated = string_from hex 2 in
  Check.is_true
    ~__LOC__
    classified
    ~error_msg:"Expected classified=true for same-source Tezos address" ;
  Check.(
    (res = 0)
      int
      ~error_msg:"Expected res=0 (Recorded) for same-source Tezos, got %L") ;
  Check.(
    (translated = addr)
      string
      ~error_msg:"Expected translated = input addr, got %L") ;
  unit

(** Unknown: [originOf] of an EVM address with no /origin record and
    nonce = 0 returns kind=0 (Unknown). *)
let test_evm_origin_of_unknown_address () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"originOf: unknown EVM address returns Unknown"
    ~tags:["tezosx"; "address_translation"; "origin_of"; "unknown"; "evm"]
  @@ fun sandbox ->
  let addr = "0x1111111111111111111111111111111111111111" in
  let* calldata =
    Cast.calldata ~args:[addr; runtime_id_ethereum] "originOf(string,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let kind = uint_at hex 0 in
  Check.(
    (kind = 0)
      int
      ~error_msg:
        "Expected kind=0 (Unknown) for unregistered EVM address, got %L") ;
  unit

(** edge: [resolveAddress] of an unclassified (no nonce, no /origin)
    EVM address returns classified=false. *)
let test_evm_resolve_address_unknown_source () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"resolveAddress: unknown source EVM address returns unclassified"
    ~tags:["tezosx"; "address_translation"; "resolve_address"; "unknown"; "evm"]
  @@ fun sandbox ->
  let addr = "0x3333333333333333333333333333333333333333" in
  let* calldata =
    Cast.calldata
      ~args:[addr; runtime_id_ethereum; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let classified = bool_at hex 0 in
  Check.is_false
    ~__LOC__
    classified
    ~error_msg:"Expected classified=false for unknown EVM source address" ;
  unit

(** [originOf] of a deployed EVM contract queried with sourceRuntime=Tezos
    returns kind=0 (Unknown).  This proves [source_runtime] actually routes
    to the Tezos runtime: the Tezos runtime fails to parse the [0x]-hex
    string as base58check and returns Unknown.  If [source_runtime] were
    ignored (defaulting to Ethereum), the code-presence back-stop would
    classify the deployed contract as Native (kind=1), so kind=0 is the
    discriminating assertion. *)
let test_evm_origin_of_cross_source_evm_as_tezos () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:
      "originOf: deployed EVM contract queried as Tezos source returns Unknown"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "origin_of";
        "cross_source";
        "tezos";
        "evm";
      ]
  @@ fun sandbox ->
  let deployer = Eth_account.bootstrap_accounts.(2) in
  let* contract_addr =
    deploy_minimal_contract ~sequencer:sandbox ~sender:deployer ~nonce:0 ()
  in
  let* calldata =
    Cast.calldata
      ~args:[contract_addr; runtime_id_tezos]
      "originOf(string,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let kind = uint_at hex 0 in
  Check.(
    (kind = 0)
      int
      ~error_msg:
        "Expected kind=0 (Unknown) for deployed EVM contract queried as Tezos \
         source, got %L") ;
  unit

(** [resolveAddress] with source=Tezos, target=Ethereum, and a valid
    EVM-format address returns classified=false.  Source≠target, so no
    same-source short-circuit applies.  The Tezos runtime fails to parse
    the [0x]-hex string as base58check → Unknown → classified=false.
    This discriminates cross-source routing: if [source_runtime] were
    ignored (treated as Ethereum), source==target would trigger the
    same-source short-circuit and the valid EVM address would return
    classified=true (Recorded).  So classified=false only holds if
    source routing is correct. *)
let test_evm_resolve_address_cross_source_tezos_to_eth () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:
      "resolveAddress: cross-source Tezos→Ethereum from EVM returns \
       unclassified"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "resolve_address";
        "cross_source";
        "tezos";
        "evm";
      ]
  @@ fun sandbox ->
  let addr = "0x1111111111111111111111111111111111111111" in
  let* calldata =
    Cast.calldata
      ~args:[addr; runtime_id_tezos; runtime_id_ethereum]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let classified = bool_at hex 0 in
  Check.is_false
    ~__LOC__
    classified
    ~error_msg:
      "Expected classified=false for EVM-format address with source=Tezos \
       (cross-source routing returns Unknown)" ;
  unit

(* ── Registration ─────────────────────────────────────────────────────── *)

let () =
  test_evm_origin_of_code_backstop () ;
  test_evm_resolve_address_same_source_ethereum () ;
  test_evm_resolve_address_same_source_tezos () ;
  test_evm_origin_of_unknown_address () ;
  test_evm_resolve_address_unknown_source () ;
  test_evm_origin_of_cross_source_evm_as_tezos () ;
  test_evm_resolve_address_cross_source_tezos_to_eth ()
