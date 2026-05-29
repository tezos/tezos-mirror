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
let invalid_runtime_id_selector = "6b95e955"

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
    demand.  When [force] is set the operation is injected even if client-side
    simulation reports a failure, so a call whose Michelson view [FAILWITH]s is
    still included in the block as a failed manager operation (matching the
    delayed-inbox semantics the originated-failure tests rely on).  Forcing
    skips simulation, so explicit [gas_limit] and [storage_limit] are then
    required. *)
let call_contract ~tez_client ~sequencer ~src ~dest ~arg_data ?force ?gas_limit
    ?storage_limit () =
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~fee:Tez.one
      ~giver:src.Account.alias
      ~receiver:dest
      ~arg:arg_data
      ~burn_cap:Tez.one
      ?force
      ?gas_limit
      ?storage_limit
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

(** [check_michelson_op_failed_with ~sequencer ~tez_client expected_rex] reads
    the head block's first manager operation via the typed tezlink RPC, asserts
    the operation failed, and asserts one of its error messages matches the
    regexp [expected_rex].  Confirms a Michelson call rolled back for the
    expected reason rather than silently doing nothing — a strictly stronger
    signal than "storage unchanged", which any failure (gas, wrong view, …)
    would also produce.

    A [FAILWITH] surfaces in the tezlink receipt as an error whose
    [error_message] embeds the failed-with Michelson value, so the match is on
    [error_message] rather than a structured [with] payload.  The operation was
    just included by {!call_contract}'s [produce_block], so it is the head
    block's first manager operation. *)
let check_michelson_op_failed_with ~sequencer ~tez_client expected_rex =
  let* op =
    Client.RPC.call ~endpoint:(tezlink_endpoint sequencer) tez_client
    @@ RPC.get_chain_block_operations_validation_pass
         ~validation_pass:3
         ~operation_offset:0
         ()
  in
  let op_result =
    JSON.(op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let status = JSON.(op_result |-> "status" |> as_string) in
  Check.(
    (status = "failed")
      string
      ~error_msg:"Expected Michelson call to fail (status %R) but got %L") ;
  let error_messages =
    JSON.(op_result |-> "errors" |> as_list)
    |> List.filter_map (fun err ->
           JSON.(err |-> "error_message" |> as_string_opt))
  in
  Check.is_true
    ~__LOC__
    (List.exists (fun msg -> msg =~ rex expected_rex) error_messages)
    ~error_msg:
      (sf
         "Expected an error message matching %S among [%s]"
         expected_rex
         (String.concat "; " error_messages)) ;
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
(* ── EVM precompile tests — error paths (commit 3) ────────────────────── *)

(** Edge: [originOf] with an invalid runtime ID (2) reverts with the ABI-encoded
    [InvalidRuntimeId] Solidity custom error.
    Encoding: 4-byte selector (0x6b95e955) + 32-byte zero-padded uint8 arg. *)
let test_evm_origin_of_invalid_runtime_id () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"originOf: invalid runtime ID reverts"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "origin_of";
        "invalid_runtime";
        "evm";
        "revert";
      ]
  @@ fun sandbox ->
  let addr = "0x1111111111111111111111111111111111111111" in
  let* calldata = Cast.calldata ~args:[addr; "2"] "originOf(string,uint8)" in
  let* err = eth_call_gateway_err ~sequencer:sandbox ~calldata in
  let message = JSON.(err |-> "message" |> as_string) in
  Check.(
    (message =~ rex "execution reverted")
      ~error_msg:"Expected revert for invalid runtimeId, got error message: %L") ;
  (* ABI-encoded InvalidRuntimeId(uint8 received):
     - bytes  0..3  : selector keccak256("InvalidRuntimeId(uint8)")[0..4] = 0x6b95e955
     - bytes  4..35 : received (uint8 2), zero-padded to 32 bytes → last byte = 0x02
     Total revert data: "0x" + 8 + 64 = 74 chars. *)
  let data = JSON.(err |-> "data" |> as_string) in
  (* Strip leading "0x" for structured checks. *)
  let data_hex = String.sub data 2 (String.length data - 2) in
  let selector = String.sub data_hex 0 8 in
  let last_byte = String.sub data_hex (String.length data_hex - 2) 2 in
  Check.(
    (selector = invalid_runtime_id_selector)
      string
      ~error_msg:"Expected InvalidRuntimeId selector 0x6b95e955, got 0x%L") ;
  Check.(
    (last_byte = "02")
      string
      ~error_msg:"Expected received=0x02 in revert data last byte, got 0x%L") ;
  unit

(** Edge: [resolveAddress] with an invalid runtime ID reverts with the
    ABI-encoded [InvalidRuntimeId] Solidity custom error. *)
let test_evm_resolve_address_invalid_runtime_id () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"resolveAddress: invalid runtime ID reverts"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "resolve_address";
        "invalid_runtime";
        "evm";
        "revert";
      ]
  @@ fun sandbox ->
  let addr = "0x1111111111111111111111111111111111111111" in
  let* calldata =
    Cast.calldata
      ~args:[addr; "2"; runtime_id_tezos]
      "resolveAddress(string,uint8,uint8)"
  in
  let* err = eth_call_gateway_err ~sequencer:sandbox ~calldata in
  let message = JSON.(err |-> "message" |> as_string) in
  Check.(
    (message =~ rex "execution reverted")
      ~error_msg:"Expected revert for invalid runtimeId, got error message: %L") ;
  (* ABI-encoded InvalidRuntimeId(uint8 received):
     - bytes  0..3  : selector keccak256("InvalidRuntimeId(uint8)")[0..4] = 0x6b95e955
     - bytes  4..35 : received (uint8 2), zero-padded to 32 bytes → last byte = 0x02 *)
  let data = JSON.(err |-> "data" |> as_string) in
  let data_hex = String.sub data 2 (String.length data - 2) in
  let selector = String.sub data_hex 0 8 in
  let last_byte = String.sub data_hex (String.length data_hex - 2) 2 in
  Check.(
    (selector = invalid_runtime_id_selector)
      string
      ~error_msg:"Expected InvalidRuntimeId selector 0x6b95e955, got 0x%L") ;
  Check.(
    (last_byte = "02")
      string
      ~error_msg:"Expected received=0x02 in revert data last byte, got 0x%L") ;
  unit

(** B1: [originOf] with a non-base58 string and sourceRuntime=Tezos.
    The Tezos runtime's [read_origin] silently returns Unknown for malformed
    addresses (no base58 parse → Classification::Unknown, no charge).
    The precompile therefore returns kind=0 (Unknown) — no revert. *)
let test_evm_origin_of_malformed_tezos_address () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"originOf: malformed Tezos address returns Unknown (no revert)"
    ~tags:
      [
        "tezosx"; "address_translation"; "origin_of"; "malformed"; "tezos"; "evm";
      ]
  @@ fun sandbox ->
  (* A string that is not a valid base58check Tezos address. *)
  let addr = "not_a_valid_tezos_address" in
  let* calldata =
    Cast.calldata ~args:[addr; runtime_id_tezos] "originOf(string,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let kind = uint_at hex 0 in
  (* Malformed addresses are classified Unknown (kind=0), not reverted:
     TezosRuntime::read_origin's base58 parse fails before any storage read
     and yields Classification::Unknown. Reverting is reserved for an
     out-of-range runtime id (InvalidRuntimeId). *)
  Check.(
    (kind = 0)
      int
      ~error_msg:"Expected kind=0 (Unknown) for malformed Tezos address, got %L") ;
  unit

(** B2: [originOf] with a non-hex-prefixed / too-short string and
    sourceRuntime=Ethereum.
    The Ethereum runtime's [read_origin] silently returns Unknown for malformed
    addresses (Address::from_hex fails → Classification::Unknown, no charge).
    The precompile returns kind=0 (Unknown) — no revert. *)
let test_evm_origin_of_malformed_ethereum_address () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~title:"originOf: malformed Ethereum address returns Unknown (no revert)"
    ~tags:
      [
        "tezosx";
        "address_translation";
        "origin_of";
        "malformed";
        "ethereum";
        "evm";
      ]
  @@ fun sandbox ->
  (* A string that is not a valid 0x-prefixed 20-byte hex Ethereum address. *)
  let addr = "0xnothex" in
  let* calldata =
    Cast.calldata ~args:[addr; runtime_id_ethereum] "originOf(string,uint8)"
  in
  let* hex = eth_call_gateway_ok ~sequencer:sandbox ~calldata in
  let kind = uint_at hex 0 in
  (* Malformed addresses are classified Unknown (kind=0), not reverted:
     EthereumRuntime::read_origin's hex parse fails before any storage read
     and yields Classification::Unknown. Reverting is reserved for an
     out-of-range runtime id (InvalidRuntimeId). *)
  Check.(
    (kind = 0)
      int
      ~error_msg:
        "Expected kind=0 (Unknown) for malformed Ethereum address, got %L") ;
  unit

(* ── Michelson VIEW tests — classification paths (commit 4) ──────────── *)

(** [originOf] via Michelson VIEW on an unknown Tezos address.
    Calls the [gateway_origin_of.tz] helper contract.
    Expects storage = Left Unit (Unknown). *)
let test_michelson_origin_of_unknown () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:"Michelson VIEW originOf: unknown Tezos address → Left Unit"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["origin_of"; "unknown"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_origin_of.tz caller contract.
     Initial storage = Right (Left 1) (a sentinel value distinct from the
     expected result Left Unit).  Starting from a sentinel ≠ Left Unit ensures
     that reaching Left Unit proves the VIEW actually ran and returned Unknown,
     rather than the call failing and leaving the initial value unchanged. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"origin_of_unknown"
      ~script_name:["mini_scenarios"; "gateway_origin_of"]
      ~init_storage_data:"Right (Left 1)"
      ()
  in
  (* Step 2: Call with an unknown Tezos address and sourceRuntime=0 (Tezos). *)
  let unknown_addr = Constant.bootstrap5.public_key_hash in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:(sf {|Pair "%s" %s|} unknown_addr runtime_id_tezos)
      ()
  in
  (* Step 3: Storage must be Left Unit (Unknown — no /origin record).
     The storage changed from Right (Left 1) to Left Unit, proving the VIEW
     ran and returned Unknown rather than failing. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (* Micheline JSON for `Left Unit`:
       {"prim":"Left","args":[{"prim":"Unit"}]} *)
    (`O [("prim", `String "Left"); ("args", `A [`O [("prim", `String "Unit")]])])

(** [originOf] via Michelson VIEW: EVM contract with bytecode (code_hash !=
    KECCAK_EMPTY) is classified as Native via the cross-source code-presence
    back-stop (sourceRuntime=1=Ethereum read from the Michelson call-site). *)
let test_michelson_origin_of_evm_native_cross_source () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW originOf: EVM contract with bytecode → Right (Left 1) \
       via back-stop"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["origin_of"; "native"; "evm"; "michelson"; "view"; "backstop"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Deploy a minimal EVM contract.  Its non-empty code_hash causes the
     back-stop to classify it as Native when queried from the Michelson side. *)
  let deployer = Eth_account.bootstrap_accounts.(0) in
  let* evm_contract_addr =
    deploy_minimal_contract ~sequencer ~sender:deployer ~nonce:0 ()
  in
  (* Step 2: Originate the caller contract. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"origin_of_evm_native"
      ~script_name:["mini_scenarios"; "gateway_origin_of"]
      ~init_storage_data:"Left Unit"
      ()
  in
  (* Step 3: Call with sourceRuntime=1 (Ethereum) and the EVM contract address. *)
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:(sf {|Pair "%s" %s|} evm_contract_addr runtime_id_ethereum)
      ()
  in
  (* Step 4: Storage must be Right (Left 1) — Native, homeRuntime=1=Ethereum. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (* `Right (Left 1)`:
       {"prim":"Right","args":[{"prim":"Left","args":[{"int":"1"}]}]} *)
    (`O
       [
         ("prim", `String "Right");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Left");
                   ("args", `A [`O [("int", `String "1")]]);
                 ];
             ] );
       ])

(** [resolveAddress] via Michelson VIEW: unknown Tezos address → None. *)
let test_michelson_resolve_address_unknown () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:"Michelson VIEW resolveAddress: unknown Tezos address → None"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "unknown"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_resolve_address.tz caller contract.
     Initial storage = Some (Pair 1 "sentinel") — a sentinel value distinct
     from the expected result None.  Starting from a sentinel ≠ None ensures
     that reaching None proves the VIEW actually ran and returned None (Unknown),
     rather than the call failing and leaving the initial value unchanged. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_unknown"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:{|Some (Pair 1 "sentinel")|}
      ()
  in
  (* Step 2: Call with an unknown Tezos address, source=Tezos, target=Ethereum.
     No /origin record → result is None. *)
  let unknown_addr = Constant.bootstrap3.public_key_hash in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:
        (sf
           {|Pair "%s" (Pair %s %s)|}
           unknown_addr
           runtime_id_tezos
           runtime_id_ethereum)
      ()
  in
  (* Step 3: Storage must be None.  The storage changed from Some (Pair 1
     "sentinel") to None, proving the VIEW ran and returned None (Unknown)
     rather than failing. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (* Micheline JSON for `None`: {"prim":"None"} *)
    (`O [("prim", `String "None")])

(** Same-source short-circuit via Michelson VIEW: [resolveAddress] with
    sourceRuntime = targetRuntime = Tezos and a valid tz1 address returns
    Some (0, addr) — Recorded, no /origin storage read. *)
let test_michelson_resolve_address_same_source () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW resolveAddress: same-source Tezos address → Some \
       (Recorded, addr)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "same_source"; "tezos"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the caller contract. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_same_source"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:"None"
      ()
  in
  (* Step 2: Call with addr = bootstrap2 and source = target = Tezos. *)
  let addr = Constant.bootstrap2.public_key_hash in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:
        (sf {|Pair "%s" (Pair %s %s)|} addr runtime_id_tezos runtime_id_tezos)
      ()
  in
  (* Step 3: Storage must be Some (Pair 0 addr) — Recorded. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (* `Some (Pair 0 addr)`:
       {"prim":"Some","args":[{"prim":"Pair","args":[{"int":"0"},
         {"string":"<addr>"}]}]} *)
    (`O
       [
         ("prim", `String "Some");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Pair");
                   ( "args",
                     `A
                       [
                         `O [("int", `String "0")]; `O [("string", `String addr)];
                       ] );
                 ];
             ] );
       ])

(** Same-source short-circuit via Michelson VIEW: [resolveAddress] with
    sourceRuntime = targetRuntime = Ethereum and a valid EVM address returns
    Some (Pair 0 addr) — Recorded, no /origin storage read. *)
let test_michelson_resolve_address_same_source_ethereum () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW resolveAddress: same-source Ethereum address → Some \
       (Recorded, addr)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "same_source"; "ethereum"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_resolve_address.tz caller contract. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_same_source_eth"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:"None"
      ()
  in
  (* Step 2: Call with a valid EVM address and source = target = Ethereum. *)
  let addr = "0x1111111111111111111111111111111111111111" in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:
        (sf
           {|Pair "%s" (Pair %s %s)|}
           addr
           runtime_id_ethereum
           runtime_id_ethereum)
      ()
  in
  (* Step 3: Storage must be Some (Pair 0 addr) — Recorded (same-source
     short-circuit, no /origin read).  The dispatcher echoes addr verbatim. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (`O
       [
         ("prim", `String "Some");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Pair");
                   ( "args",
                     `A
                       [
                         `O [("int", `String "0")]; `O [("string", `String addr)];
                       ] );
                 ];
             ] );
       ])

(** Michelson cross-source: [resolveAddress] with source=Ethereum (cross-source
    from the Michelson call-site) via Michelson VIEW.  The EVM contract has
    bytecode (code_hash != KECCAK_EMPTY), so the back-stop fires; the result
    should be Some (Pair 1 derived_kt1) — Derived. *)
let test_michelson_resolve_address_cross_source_ethereum () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW resolveAddress: cross-source Ethereum→Tezos returns Some \
       (Derived)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "cross_source"; "ethereum"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Deploy a minimal EVM contract — code_hash != KECCAK_EMPTY, so the
     back-stop fires for Ethereum source. *)
  let deployer = Eth_account.bootstrap_accounts.(1) in
  let* evm_contract_addr =
    deploy_minimal_contract ~sequencer ~sender:deployer ~nonce:0 ()
  in
  (* Step 2: Originate the gateway_resolve_address.tz caller contract. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_cross_source_eth"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:"None"
      ()
  in
  (* Step 3: Call with source=Ethereum (cross-source) and target=Tezos.
     The EVM code-presence back-stop fires → Native → Derived alias. *)
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:
        (sf
           {|Pair "%s" (Pair %s %s)|}
           evm_contract_addr
           runtime_id_ethereum
           runtime_id_tezos)
      ()
  in
  (* Step 4: Storage must be Some (Pair 1 <derived_kt1>) — Derived.
     The derived KT1 depends on [evm_contract_addr] which is set at deployment
     time.  We check only the classification (res=1, Derived) and that a KT1
     address was produced, without hard-coding the exact hash. *)
  let* storage =
    Client.RPC.call ~endpoint:(tezlink_endpoint sequencer) tez_client
    @@ RPC.get_chain_block_context_contract_storage ~id:kt1_address ()
  in
  (* Expected shape: Some (Pair 1 <kt1_string>).
     In Micheline JSON: {"prim":"Some","args":[{"prim":"Pair",
       "args":[{"int":"1"},{"string":"KT1..."}]}]} *)
  let pair_node = JSON.(storage |-> "args" |> as_list |> List.hd) in
  let args = JSON.(pair_node |-> "args" |> as_list) in
  let res = JSON.(List.nth args 0 |-> "int" |> as_string) in
  let kt1_str = JSON.(List.nth args 1 |-> "string" |> as_string) in
  Check.(
    (res = "1") string ~error_msg:"Expected res=1 (Derived) in storage, got %L") ;
  Check.(
    (String.length kt1_str > 0)
      int
      ~error_msg:"Expected a non-empty KT1 address in storage") ;
  Log.info "EVM contract %s → Derived KT1 %s" evm_contract_addr kt1_str ;
  unit

(* ── Michelson VIEW tests — post-CRAC Recorded/Alias (commit 5) ─── *)

(** [originOf] via Michelson VIEW after a CRAC transfer materialises the
    alias.  The CRAC-classified KT1 is expected to be an Alias of the EVM
    sender: storage = Right (Right (Pair 1 <evm_sender_lowercase>)). *)
let test_michelson_origin_of_alias_after_crac () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW originOf: CRAC-classified KT1 → Right (Right (Pair 1 \
       evm_sender))"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["origin_of"; "alias"; "crac"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate a dummy sender.tz as the CRAC target.  Its [unit]
     parameter accepts the gateway's empty payload; a [string]-parameter
     contract would reject the empty call and revert the CRAC. *)
  let* kt1_target =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"crac_target"
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
  (* Step 4: Originate the gateway_origin_of.tz caller contract. *)
  let* kt1_caller =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"origin_of_caller"
      ~script_name:["mini_scenarios"; "gateway_origin_of"]
      ~init_storage_data:"Left Unit"
      ()
  in
  (* Step 5: Call with kt1_alias and sourceRuntime=0 (Tezos). *)
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_caller
      ~arg_data:(sf {|Pair "%s" %s|} kt1_alias runtime_id_tezos)
      ()
  in
  (* Step 6: Storage must be Right (Right (Pair 1 <evm_sender_lowercase>)).
     The CRAC wrote an Alias record: homeRuntime=1 (Ethereum),
     nativeAddress = the EVM sender address (lowercased by the kernel). *)
  let* storage =
    Client.RPC.call ~endpoint:(tezlink_endpoint sequencer) tez_client
    @@ RPC.get_chain_block_context_contract_storage ~id:kt1_caller ()
  in
  (* Expected shape: Right (Right (Pair 1 <evm_sender_lowercase>)).
     Micheline JSON:
     {"prim":"Right","args":[{"prim":"Right","args":[{"prim":"Pair",
       "args":[{"int":"1"},{"string":"0x..."}]}]}]} *)
  let outer_args = JSON.(storage |-> "args" |> as_list) in
  let inner = List.hd outer_args in
  let inner_args = JSON.(inner |-> "args" |> as_list) in
  let pair_node = List.hd inner_args in
  let pair_args = JSON.(pair_node |-> "args" |> as_list) in
  let home_runtime = JSON.(List.nth pair_args 0 |-> "int" |> as_string) in
  let native_addr = JSON.(List.nth pair_args 1 |-> "string" |> as_string) in
  Check.(
    (home_runtime = "1")
      string
      ~error_msg:"Expected homeRuntime=1 (Ethereum) in Alias storage, got %L") ;
  Check.(
    (String.lowercase_ascii native_addr = String.lowercase_ascii sender.address)
      string
      ~error_msg:"Expected nativeAddress = EVM sender in Alias storage, got %L") ;
  unit

(** [resolveAddress] via Michelson VIEW after a CRAC transfer returns
    Some (Pair 0 kt1_alias) — Recorded, not Derived. *)
let test_michelson_resolve_address_recorded_after_crac () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW resolveAddress: post-CRAC EVM sender → Some (Recorded, \
       kt1_alias)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "recorded"; "crac"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate a dummy sender.tz as the CRAC target.  Its [unit]
     parameter accepts the gateway's empty payload; a [string]-parameter
     contract would reject the empty call and revert the CRAC. *)
  let* kt1_target =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"crac_target"
      ~script_name:["opcodes"; "sender"]
      ~init_storage_data:(sf {|"%s"|} Constant.bootstrap1.public_key_hash)
      ()
  in
  (* Step 2: Send a CRAC from EVM to the Michelson contract. *)
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
  (* Step 4: Originate the gateway_resolve_address.tz caller contract. *)
  let* kt1_caller =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_caller"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:"None"
      ()
  in
  (* Step 5: Call with evm_sender, sourceRuntime=Ethereum, targetRuntime=Tezos. *)
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_caller
      ~arg_data:
        (sf
           {|Pair "%s" (Pair %s %s)|}
           sender.address
           runtime_id_ethereum
           runtime_id_tezos)
      ()
  in
  (* Step 6: Storage must be Some (Pair 0 kt1_alias) — res=0 (Recorded).
     The CRAC wrote the /origin entry so the translation is Recorded rather
     than Derived.  The kt1_alias string must match the RPC result. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_caller
    (`O
       [
         ("prim", `String "Some");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Pair");
                   ( "args",
                     `A
                       [
                         `O [("int", `String "0")];
                         `O [("string", `String kt1_alias)];
                       ] );
                 ];
             ] );
       ])

(* ── Michelson VIEW tests — error paths (commit 6) ────────────────────── *)

(** Michelson invalid-runtime-ID: passing an out-of-range runtime ID to
    [originOf] via Michelson VIEW makes the view FAILWITH with
    [Pair "INVALID_RUNTIME_ID" id].  A FAILWITH inside a view aborts the whole
    call (it does not make VIEW return None), so the caller's own [IF_NONE]
    branch never runs and the operation fails with the view's payload.  This is
    the Michelson counterpart of the EVM precompile reverting [InvalidRuntimeId].
    Two complementary checks: the operation receipt must report a failure
    mentioning [INVALID_RUNTIME_ID] (proving the call rolled back for the
    expected reason), and the sentinel initial storage [Right (Left 1)] — which
    the success path WOULD overwrite — must be preserved (proving no value was
    written). *)
let test_michelson_origin_of_invalid_runtime_id () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW originOf: invalid runtime ID causes FAILWITH (sentinel \
       preserved)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["origin_of"; "invalid_runtime"; "michelson"; "view"; "failwith"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_origin_of.tz caller contract.
     Initial storage = Right (Left 1) — a sentinel that the success path would
     overwrite.  An unchanged sentinel after the call proves FAILWITH occurred. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"origin_of_caller"
      ~script_name:["mini_scenarios"; "gateway_origin_of"]
      ~init_storage_data:"Right (Left 1)"
      ()
  in
  (* Step 2: Call with an invalid runtime ID (99).
     Inside the kernel, runtime_id_from_nat(99) raises
     InterpretError::FailedWith("INVALID_RUNTIME_ID", 99) within the view,
     which aborts the call.  The operation is Failed with that payload and
     storage stays at Right (Left 1). *)
  let invalid_runtime_id = "99" in
  let addr = Constant.bootstrap5.public_key_hash in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:(sf {|Pair "%s" %s|} addr invalid_runtime_id)
      ~force:true
      ~gas_limit:1_000_000
      ~storage_limit:1000
      ()
  in
  let* () =
    check_michelson_op_failed_with ~sequencer ~tez_client "INVALID_RUNTIME_ID"
  in
  (* Step 3: Storage must remain Right (Left 1) — the FAILWITH rolled back the
     call.  The sentinel being preserved proves FAILWITH occurred rather than
     the view returning a value and overwriting storage. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (`O
       [
         ("prim", `String "Right");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Left");
                   ("args", `A [`O [("int", `String "1")]]);
                 ];
             ] );
       ])

(** [resolveAddress] via Michelson VIEW with an invalid runtime ID makes the
    view FAILWITH with [Pair "INVALID_RUNTIME_ID" id], which aborts the call
    (it does not make VIEW return None), so the operation fails with the view's
    payload, leaving storage unchanged.  Two complementary checks: the operation
    receipt must report a failure mentioning [INVALID_RUNTIME_ID] (proving the
    call rolled back for the expected reason), and the sentinel initial storage
    [Some (Pair 1 "sentinel")] — which the success path WOULD overwrite — must
    be preserved (proving no value was written). *)
let test_michelson_resolve_address_invalid_runtime_id () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW resolveAddress: invalid runtime ID causes FAILWITH \
       (sentinel preserved)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["resolve_address"; "invalid_runtime"; "michelson"; "view"; "failwith"]
      )
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_resolve_address.tz caller contract.
     Initial storage = Some (Pair 1 "sentinel") — a sentinel that the success
     path would overwrite.  A preserved sentinel proves FAILWITH occurred. *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"resolve_caller"
      ~script_name:["mini_scenarios"; "gateway_resolve_address"]
      ~init_storage_data:{|Some (Pair 1 "sentinel")|}
      ()
  in
  (* Step 2: Call with an invalid sourceRuntime (99).
     runtime_id_from_nat(99) raises InterpretError::FailedWith("INVALID_RUNTIME_ID", 99)
     inside the view, which aborts the call and rolls back storage. *)
  let invalid_runtime_id = "99" in
  let addr = Constant.bootstrap5.public_key_hash in
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:
        (sf {|Pair "%s" (Pair %s %s)|} addr invalid_runtime_id runtime_id_tezos)
      ~force:true
      ~gas_limit:1_000_000
      ~storage_limit:1000
      ()
  in
  let* () =
    check_michelson_op_failed_with ~sequencer ~tez_client "INVALID_RUNTIME_ID"
  in
  (* Step 3: Storage must remain Some (Pair 1 "sentinel") — the FAILWITH rolled
     back the call.  The sentinel being preserved proves rollback occurred. *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (`O
       [
         ("prim", `String "Some");
         ( "args",
           `A
             [
               `O
                 [
                   ("prim", `String "Pair");
                   ( "args",
                     `A
                       [
                         `O [("int", `String "1")];
                         `O [("string", `String "sentinel")];
                       ] );
                 ];
             ] );
       ])

(** [originOf] via Michelson VIEW with a malformed address.
    The kernel's [read_origin] silently returns Unknown for malformed
    addresses, so the VIEW returns Left Unit (Unknown).  The caller
    contract updates storage to Left Unit (no FAILWITH).
    Initial storage is Left Unit, and it remains Left Unit after the call. *)
let test_michelson_origin_of_malformed_address () =
  Test_helpers.register_sandbox
    ~__FILE__
    ~kernel:Latest
    ~uses_client:true
    ~with_runtimes:[Tezos]
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
    ~title:
      "Michelson VIEW originOf: malformed address → Left Unit (Unknown, no \
       FAILWITH)"
    ~tags:
      (["tezosx"; "address_translation"]
      @ runtime_tags [Tezos]
      @ ["origin_of"; "malformed"; "michelson"; "view"])
  @@ fun sequencer ->
  let* tez_client = tezlink_client sequencer in
  let source = Constant.bootstrap1 in
  (* Step 1: Originate the gateway_origin_of.tz caller contract with a
     distinct, NON-Unknown initial storage [Right (Left 1)] (Native,
     home=Ethereum). The contract overwrites storage with the VIEW result,
     and FAILWITHs if the VIEW returns None — so starting from a value other
     than [Left Unit] is what lets Step 3 distinguish "VIEW returned
     Some (Left Unit)" from "VIEW returned None / call failed" (which would
     leave [Right (Left 1)] in place). *)
  let* kt1_address =
    originate_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~alias:"malformed_caller"
      ~script_name:["mini_scenarios"; "gateway_origin_of"]
      ~init_storage_data:"Right (Left 1)"
      ()
  in
  (* Step 2: Call with a malformed address and sourceRuntime=Tezos.
     TezosRuntime::read_origin returns Unknown silently (no error, no charge).
     The VIEW therefore returns Some (Left Unit), not None, so the caller
     does not FAILWITH and overwrites its storage with Left Unit. *)
  let* () =
    call_contract
      ~tez_client
      ~sequencer
      ~src:source
      ~dest:kt1_address
      ~arg_data:(sf {|Pair "%s" %s|} "not_a_valid_address" runtime_id_tezos)
      ()
  in
  (* Step 3: Storage must have changed from Right (Left 1) to Left Unit —
     proving the malformed address was classified Unknown and returned via
     the VIEW (not a None/FAILWITH, which would have preserved Right (Left 1)). *)
  check_contract_storage
    ~sequencer
    ~tez_client
    kt1_address
    (`O [("prim", `String "Left"); ("args", `A [`O [("prim", `String "Unit")]])])

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
  test_evm_resolve_address_roundtrip_recorded () ;
  test_evm_origin_of_invalid_runtime_id () ;
  test_evm_resolve_address_invalid_runtime_id () ;
  test_evm_origin_of_malformed_tezos_address () ;
  test_evm_origin_of_malformed_ethereum_address () ;
  test_michelson_origin_of_unknown () ;
  test_michelson_origin_of_evm_native_cross_source () ;
  test_michelson_resolve_address_unknown () ;
  test_michelson_resolve_address_same_source () ;
  test_michelson_resolve_address_same_source_ethereum () ;
  test_michelson_resolve_address_cross_source_ethereum () ;
  test_michelson_origin_of_alias_after_crac () ;
  test_michelson_resolve_address_recorded_after_crac () ;
  test_michelson_origin_of_invalid_runtime_id () ;
  test_michelson_resolve_address_invalid_runtime_id () ;
  test_michelson_origin_of_malformed_address ()
