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

(* ── EVM precompile tests — back-stop derive + round-trips (commit 2) ─── *)

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

(** Round-trip failure: derive an EVM→Tezos alias (Derived), then ask
    for the inverse Tezos→Ethereum translation.  Since the alias was computed
    but not yet written to durable storage, the inverse lookup returns
    classified=false (Unknown). *)
let test_evm_resolve_address_roundtrip_derive_then_unknown () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:
      "resolveAddress round-trip: derived alias has no materialized inverse"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "resolve_address";
        "round_trip";
        "derived";
        "evm";
      ]
  @@ fun sandbox ->
  (* Deploy a minimal contract — gives it a non-empty code_hash so the back-stop
     classifies it as Native, enabling derivation of a KT1 alias. *)
  let deployer = Eth_account.bootstrap_accounts.(2) in
  let* contract_addr =
    deploy_minimal_contract ~sequencer:sandbox ~sender:deployer ~nonce:0 ()
  in

  (* Step 1: resolveAddress(contract_addr, Ethereum, Tezos) → Derived. *)
  let* calldata_fwd =
    Cast.calldata
      ~args:[contract_addr; runtime_id_ethereum; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex_fwd =
    eth_call_gateway_ok ~sequencer:sandbox ~calldata:calldata_fwd
  in
  let classified_fwd = bool_at hex_fwd 0 in
  let res_fwd = uint_at hex_fwd 1 in
  let derived_kt1 = string_from hex_fwd 2 in
  Check.is_true
    ~__LOC__
    classified_fwd
    ~error_msg:"Expected forward direction to be classified" ;
  Check.(
    (res_fwd = 1) int ~error_msg:"Expected res=1 (Derived) forward, got %L") ;
  (* The derived KT1 is deterministic from the deployed contract address; we
     don't hard-code it here since the address depends on the deployer nonce. *)
  Log.info "Forward: %s → %s (Derived)" contract_addr derived_kt1 ;

  (* Step 2: resolveAddress(derived_kt1, Tezos, Ethereum) → classified=false.
     The alias was computed but never written, so the inverse /origin does
     not exist in durable storage. *)
  let* calldata_rev =
    Cast.calldata
      ~args:[derived_kt1; runtime_id_tezos; runtime_id_ethereum]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex_rev =
    eth_call_gateway_ok ~sequencer:sandbox ~calldata:calldata_rev
  in
  let classified_rev = bool_at hex_rev 0 in
  Check.is_false
    ~__LOC__
    classified_rev
    ~error_msg:
      "Expected classified=false for the derived-only alias (inverse not yet \
       materialized)" ;
  unit

(** Round-trip happy path: after a CRAC transfer, the EVM sender's
    Tezos alias is materialized in durable storage.  Both directions of
    [resolveAddress] should then return [Recorded]. *)
let test_evm_resolve_address_roundtrip_recorded () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "resolveAddress round-trip: both directions Recorded after CRAC transfer"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "round_trip"; "recorded"; "crac"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  (* Step 1: Originate a dummy sender.tz as the CRAC target.  Its [unit]
     parameter accepts the gateway's empty payload; a [string]-parameter
     contract would reject the empty call and revert the CRAC. *)
  let* kt1_target =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:Constant.bootstrap1
      ~alias:"sender"
      ~script_name:["opcodes"; "sender"]
      ~init_storage_data:(sf {|"%s"|} Constant.bootstrap1.public_key_hash)
      ()
  in
  (* Step 2: Send a CRAC from EVM to the Michelson contract.  This writes
     the sender's Tezos alias into /origin on both sides. *)
  let sender = Eth_account.bootstrap_accounts.(0) in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:sender.Eth_account.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas:3_000_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:evm_gateway_address
      ~signature:"callMichelson(string,string,bytes)"
      ~arguments:[kt1_target; ""; "0x"]
      ()
  in
  let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@ _block = Rpc.produce_block sequencer in
  let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  (match receipt with
  | Some r ->
      Check.(
        (r.status = true) bool ~error_msg:"CRAC transaction should succeed")
  | None -> Test.fail "No receipt for CRAC transaction") ;

  (* Step 3: Resolve the sender's Tezos alias via the RPC. *)
  let* alias_result =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let kt1_alias = Result.get_ok alias_result in
  Log.info "Sender %s → Tezos alias %s" sender.address kt1_alias ;

  (* Step 4: resolveAddress(evm_addr, Ethereum, Tezos) should return
     Recorded (the alias was written by the CRAC). *)
  let* calldata_fwd =
    Cast.calldata
      ~args:[sender.address; runtime_id_ethereum; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex_fwd = eth_call_gateway_ok ~sequencer ~calldata:calldata_fwd in
  let classified_fwd = bool_at hex_fwd 0 in
  let res_fwd = uint_at hex_fwd 1 in
  let translated_fwd = string_from hex_fwd 2 in
  Check.is_true
    ~__LOC__
    classified_fwd
    ~error_msg:"Expected forward resolveAddress to be classified" ;
  Check.(
    (res_fwd = 0)
      int
      ~error_msg:"Expected res=0 (Recorded) forward after CRAC, got %L") ;
  Check.(
    (translated_fwd = kt1_alias)
      string
      ~error_msg:
        "Expected forward translated address to match RPC alias, got %L") ;

  (* Step 5: originOf(kt1_alias, Tezos) should return kind=2 (Alias),
     homeRuntime=1 (Ethereum), nativeAddress=evm_sender.  The CRAC wrote the
     inverse /origin entry so the KT1 is classified as an Alias of the EVM
     sender in the Tezos runtime. *)
  let* calldata_origin =
    Cast.calldata ~args:[kt1_alias; runtime_id_tezos] "originOf(string,uint8)"
  in
  let* hex_origin = eth_call_gateway_ok ~sequencer ~calldata:calldata_origin in
  let kind = uint_at hex_origin 0 in
  let home_runtime = uint_at hex_origin 1 in
  let native_address = string_from hex_origin 2 in
  Check.(
    (kind = 2)
      int
      ~error_msg:
        "Expected kind=2 (Alias) for CRAC-classified KT1 from cross-source EVM \
         originOf, got %L") ;
  Check.(
    (home_runtime = 1)
      int
      ~error_msg:
        "Expected homeRuntime=1 (Ethereum) for KT1 alias originOf, got %L") ;
  Check.(
    (String.lowercase_ascii native_address
    = String.lowercase_ascii sender.address)
      string
      ~error_msg:
        "Expected nativeAddress = EVM sender for KT1 alias originOf, got %L") ;

  (* Step 6: resolveAddress(kt1_alias, Tezos, Ethereum) should also return
     Recorded (the inverse /origin was written by the same CRAC). *)
  let* calldata_rev =
    Cast.calldata
      ~args:[kt1_alias; runtime_id_tezos; runtime_id_ethereum]
      "resolveAddress(string,uint8,uint8)"
  in
  let* hex_rev = eth_call_gateway_ok ~sequencer ~calldata:calldata_rev in
  let classified_rev = bool_at hex_rev 0 in
  let res_rev = uint_at hex_rev 1 in
  let translated_rev = string_from hex_rev 2 in
  Check.is_true
    ~__LOC__
    classified_rev
    ~error_msg:"Expected reverse resolveAddress to be classified" ;
  Check.(
    (res_rev = 0)
      int
      ~error_msg:"Expected res=0 (Recorded) reverse after CRAC, got %L") ;
  Check.(
    (String.lowercase_ascii translated_rev
    = String.lowercase_ascii sender.address)
      string
      ~error_msg:
        "Expected reverse translated address to match original EVM sender, got \
         %L") ;
  unit

(* ── Registration ─────────────────────────────────────────────────────── *)

let () =
  test_evm_origin_of_code_backstop () ;
  test_evm_resolve_address_same_source_ethereum () ;
  test_evm_resolve_address_same_source_tezos () ;
  test_evm_origin_of_unknown_address () ;
  test_evm_resolve_address_unknown_source () ;
  test_evm_origin_of_cross_source_evm_as_tezos () ;
  test_evm_resolve_address_cross_source_tezos_to_eth () ;
  test_evm_backstop_derive_same_source () ;
  test_evm_resolve_address_roundtrip_derive_then_unknown () ;
  test_evm_resolve_address_roundtrip_recorded ()
