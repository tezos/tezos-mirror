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

   Happy-path tests for the EVM precompile selectors at `0xff...07`:

       `originOf(string addr, uint8 sourceRuntime) → (uint8 kind, uint8
        homeRuntime, string nativeAddress)`
       `resolveAddress(string addr, uint8 sourceRuntime, uint8 targetRuntime)
        → (bool classified, uint8 res, string translated)`

   Runtime IDs on the wire: 0 = Tezos, 1 = Ethereum.
*)

open Rpc.Syntax

(* ── Shared constants ─────────────────────────────────────────────────── *)

(** EVM precompile address for cross-runtime gateway calls. *)
let evm_gateway_address = "0xff00000000000000000000000000000000000007"

(* Wire encoding for runtime IDs. *)
let runtime_id_tezos = "0"

let runtime_id_ethereum = "1"

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

(* ── EVM precompile tests (sandbox) ──────────────────────────────────── *)

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

(** [originOf] of an EVM contract with bytecode (code_hash != KECCAK_EMPTY)
    returns Native via the code-presence back-stop.  Then [resolveAddress]
    with sourceRuntime=Ethereum, targetRuntime=Tezos returns a Derived alias.
    Both reads execute under the EVM call-site's own runtime — this exercises
    the same-source (back-stop derivation) path. *)
let test_evm_backstop_derive_same_source () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"originOf + resolveAddress: EVM back-stop derive (same-source path)"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "origin_of";
        "resolve_address";
        "backstop";
        "derive";
        "evm";
        "alias";
      ]
  @@ fun sandbox ->
  (* Deploy a minimal contract — gives it a non-empty code_hash so the back-stop
     classifies it as Native. *)
  let deployer = Eth_account.bootstrap_accounts.(1) in
  let* contract_addr =
    deploy_minimal_contract ~sequencer:sandbox ~sender:deployer ~nonce:0 ()
  in

  (* originOf(contract_addr, Ethereum) → Native, homeRuntime=Ethereum *)
  let* origin_calldata =
    Cast.calldata
      ~args:[contract_addr; runtime_id_ethereum]
      "originOf(string,uint8)"
  in
  let* origin_hex =
    eth_call_gateway_ok ~sequencer:sandbox ~calldata:origin_calldata
  in
  let kind = uint_at origin_hex 0 in
  let home_runtime = uint_at origin_hex 1 in
  Check.(
    (kind = 1) int ~error_msg:"Expected kind=1 (Native) from back-stop, got %L") ;
  Check.(
    (home_runtime = 1)
      int
      ~error_msg:"Expected homeRuntime=1 (Ethereum), got %L") ;

  (* resolveAddress(contract_addr, Ethereum, Tezos) → classified=true, res=1
     (Derived).  The derived KT1 depends on the deployed address, which is
     deterministic but not known here, so we check only the classification. *)
  let* resolve_calldata =
    Cast.calldata
      ~args:[contract_addr; runtime_id_ethereum; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* resolve_hex =
    eth_call_gateway_ok ~sequencer:sandbox ~calldata:resolve_calldata
  in
  let classified = bool_at resolve_hex 0 in
  let res = uint_at resolve_hex 1 in
  let translated = string_from resolve_hex 2 in
  Check.is_true
    ~__LOC__
    classified
    ~error_msg:"Expected classified=true for native EVM contract" ;
  Check.(
    (res = 1)
      int
      ~error_msg:"Expected res=1 (Derived) for EVM contract to Tezos, got %L") ;
  Log.info "EVM contract %s → Tezos alias %s (Derived)" contract_addr translated ;
  unit

(* ── Registration ─────────────────────────────────────────────────────── *)

let () =
  test_evm_origin_of_code_backstop () ;
  test_evm_resolve_address_same_source_ethereum () ;
  test_evm_resolve_address_same_source_tezos () ;
  test_evm_backstop_derive_same_source ()
