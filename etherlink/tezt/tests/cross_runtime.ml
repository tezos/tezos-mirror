(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------

   Requirement:  make -f etherlink.mk build
                 make octez-node octez-client octez-smart-rollup-node octez-evm-node
   Invocation:   dune exec etherlink/tezt/tests/main.exe -- --file cross_runtime.ml
 *)

open Rpc.Syntax

module EvmContract = struct
  let tezosx_evm_chain_id = 1337

  (** [deploy_contract ~sequencer ~sender ~nonce ~init_code ()] deploys an
   *  EVM contract from [init_code], produces a block, and returns the deployed
   *  contract address.  Fails if the deployment transaction reverts or yields no
   *  contract address. *)
  let deploy_contract ~sequencer ~sender ~nonce ~init_code () =
    let* raw_tx =
      Cast.craft_deploy_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:tezosx_evm_chain_id
        ~nonce
        ~gas:2_000_000
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
        Test.fail "Contract deployment transaction failed"
    | _ -> Test.fail "No receipt or no contract address for deployment tx"

  (** [deploy_solidity_contract ~sequencer ~sender ~nonce ~contract ()]
   *  compiles and deploys a Solidity contract. *)
  let deploy_solidity_contract ?(evm_version = Evm_version.Shanghai) ~sequencer
      ~sender ~nonce ~contract () =
    let* contract = contract evm_version in
    let init_code =
      "0x" ^ Tezt.Base.read_file contract.Solidity_contracts.bin
    in
    deploy_contract ~sequencer ~sender ~nonce ~init_code ()

  (** [craft_and_send_transaction ~sequencer ~sender ~nonce ~value ~address
   *  ~abi_signature ~arguments ()] crafts an EVM transaction, sends it, produces
   *  a block, asserts the receipt status matches [expected_status] (default
    [true]), and returns the receipt. *)
  let craft_and_send_transaction ~sequencer ~sender ~nonce ~value ~address
      ~abi_signature ~arguments ?(expected_status = true) ?access_list ?legacy
      ?(gas = 3_000_000) () =
    let* raw_tx =
      Cast.craft_tx
        ~source_private_key:sender.Eth_account.private_key
        ~chain_id:1337
        ~nonce
        ~gas
        ~gas_price:1_000_000_000
        ~value
        ~address
        ~signature:abi_signature
        ~arguments
        ?access_list
        ?legacy
        ()
    in
    let*@ tx_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
    let*@ _block_number = Rpc.produce_block sequencer in
    let*@ receipt = Rpc.get_transaction_receipt ~tx_hash sequencer in
    match receipt with
    | Some r ->
        Check.(
          (r.status = expected_status)
            bool
            ~error_msg:"Expected receipt status %R but got %L") ;
        return r
    | None -> Test.fail "No receipt for EVM transaction to %s" address
end

module TezContract = struct
  (** Durable storage path where Michelson originated contracts are indexed in
   *  Tezos X. Each subkey is the hex-encoded [Contract_repr.t] of the
   *  Michelson contract. *)
  let tezosx_michelson_contracts_index = "/evm/world_state/contracts/index"

  (** [decode_contract_address hex] decodes a hex-encoded
   *  [Contract_repr.t] into a b58check KT1 address string. *)
  let decode_contract_address hex =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    Hex.to_bytes (`Hex hex)
    |> Data_encoding.Binary.of_bytes_exn C.encoding
    |> C.to_b58check

  (** [send_op_to_delayed_inbox_and_wait] sends a Tezos operation via the
   *  delayed inbox and waits until it is included and the delayed inbox is
   *  empty. *)
  let send_op_to_delayed_inbox_and_wait ~sc_rollup_address ~sc_rollup_node
      ~client ~l1_contracts ~sequencer operation =
    let* _hash =
      Delayed_inbox.send_tezos_operation_to_delayed_inbox
        ~sc_rollup_address
        ~sc_rollup_node
        ~client
        ~l1_contracts
        ~tezosx_format:true
        operation
    in
    let* () =
      Delayed_inbox.wait_for_delayed_inbox_add_tx_and_injected
        ~sequencer
        ~sc_rollup_node
        ~client
    in
    let* () =
      Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
    in
    Delayed_inbox.assert_empty (Sc_rollup_node sc_rollup_node)

  (** [originate_contract_via_delayed_inbox] originates a Michelson
   *  contract via the delayed inbox. Loads the script from
   *  [michelson_test_scripts], converts code and initial storage to JSON,
   *  forges and sends the origination operation, then returns the hex key and
   *  KT1 address of the new contract. *)
  let originate_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node
      ~client ~l1_contracts ~sequencer ~source ~counter ~script_name
      ~init_storage_data ?(init_balance = 0) protocol =
    let* contracts_before =
      Delayed_inbox.subkeys
        tezosx_michelson_contracts_index
        (Sc_rollup_node sc_rollup_node)
    in
    let script_path = Michelson_script.(find script_name protocol |> path) in
    let* code = Client.convert_script_to_json ~script:script_path client in
    let* init_storage =
      Client.convert_data_to_json ~data:init_storage_data client
    in
    let* origination_op =
      Operation.Manager.(
        operation
          [
            make
              ~fee:1000
              ~counter
              ~gas_limit:10000
              ~storage_limit:1000
              ~source
              (origination ~code ~init_storage ~init_balance ());
          ])
        client
    in
    let* () =
      send_op_to_delayed_inbox_and_wait
        ~sc_rollup_address
        ~sc_rollup_node
        ~client
        ~l1_contracts
        ~sequencer
        origination_op
    in
    let* contracts_after =
      Delayed_inbox.subkeys
        tezosx_michelson_contracts_index
        (Sc_rollup_node sc_rollup_node)
    in
    let new_contracts =
      List.filter (fun c -> not (List.mem c contracts_before)) contracts_after
    in
    Check.(
      (List.length new_contracts = 1)
        int
        ~error_msg:"Expected %R new contract but got %L") ;
    let contract_hex = List.hd new_contracts in
    let kt1_address = decode_contract_address contract_hex in
    Log.info "Originated contract: %s" kt1_address ;
    return (contract_hex, kt1_address)

  (** [call_contract_via_delayed_inbox] calls a Michelson contract
   *  via the delayed inbox. Converts the argument from Michelson notation to
   *  JSON, forges and sends the call operation. *)
  let call_contract_via_delayed_inbox ~sc_rollup_address ~sc_rollup_node ~client
      ~l1_contracts ~sequencer ~source ~counter ~dest ~arg_data
      ?(entrypoint = "default") ?(amount = 0) ?(gas_limit = 100_000) () =
    let* arg = Client.convert_data_to_json ~data:arg_data client in
    let* call_op =
      Operation.Manager.(
        operation
          [
            make
              ~fee:1000
              ~counter
              ~gas_limit
              ~storage_limit:1000
              ~source
              (call ~dest ~arg ~entrypoint ~amount ());
          ])
        client
    in
    send_op_to_delayed_inbox_and_wait
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      call_op

  (** [read_michelson_contract_storage sc_rollup_node contract_hex] reads the
   *  storage of an originated Michelson contract from the durable storage,
   *  given its hex-encoded [Contract_repr.t] key. *)
  let read_michelson_contract_storage sc_rollup_node contract_hex =
    let path =
      sf "%s/%s/data/storage" tezosx_michelson_contracts_index contract_hex
    in
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key:path
         ()

  (** [decode_michelson_contract_address hex] decodes a hex-encoded
   *  [Contract_repr.t] into a b58check KT1 address string. *)
  let decode_michelson_contract_address hex =
    let module C = Tezos_protocol_alpha.Protocol.Contract_repr in
    Hex.to_bytes (`Hex hex)
    |> Data_encoding.Binary.of_bytes_exn C.encoding
    |> C.to_b58check

  (** [decode_micheline_storage hex_str] decodes a hex string from the durable
   *  storage into a Micheline [JSON.u] value. Returns [None] if decoding
   *  fails. *)
  let decode_micheline_storage hex_str =
    let module S = Tezos_protocol_alpha.Protocol.Script_repr in
    Data_encoding.Binary.of_bytes_opt
      S.expr_encoding
      (Hex.to_bytes (`Hex hex_str))
    |> Option.map (fun e ->
           Data_encoding.Json.(
             construct S.expr_encoding e
             |> to_string
             |> JSON.parse ~origin:"decode_micheline_storage"))

  (** Reads and decodes the storage of the Michelson contract identified
   *  by [contract_hex].  Fails if the storage is missing or cannot be
   *  decoded. *)
  let get_storage ~sc_rollup_node contract_hex =
    let* storage_raw =
      read_michelson_contract_storage sc_rollup_node contract_hex
    in
    match storage_raw with
    | None ->
        let kt1 = decode_michelson_contract_address contract_hex in
        Test.fail "Storage not found for contract %s" kt1
    | Some hex_str -> (
        match decode_micheline_storage hex_str with
        | None ->
            let kt1 = decode_michelson_contract_address contract_hex in
            Test.fail "Storage failed to be decoded for contract %s" kt1
        | Some storage -> return storage)
end

(** Contracts *)

(** Common [run()] caller for EVM contracts. *)
module EvmRunner = struct
  open EvmContract

  (** Calls [run()] on the given EVM contract and returns [gasUsed]. *)
  let call_run ?expected_status ~sequencer ~sender ~nonce ~value ?access_list
      runner =
    let legacy = if Option.is_some access_list then Some false else None in
    let* receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:runner
        ~abi_signature:"run()"
        ~arguments:[]
        ?expected_status
        ?access_list
        ?legacy
        ()
    in
    return receipt.gasUsed
end

(** EVM contract that forwards [run()] to a Tezos target via the
 *  CRAC gateway.  Increments [counter] before and after the
 *  cross-runtime call. *)
module EvmCrossRuntimeRunnerTez = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.cross_runtime_run_tez

  (** Initialises the contract.  [tez_contract_target_address] is the
   *  KT1 address that will be called on [run()]. *)
  let init ~sequencer ~sender ~nonce ~value ~tez_contract_target_address
      cross_runtime_run_tez =
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:cross_runtime_run_tez
        ~abi_signature:"initialize(string)"
        ~arguments:[tez_contract_target_address]
        ()
    in
    unit

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sequencer ~expected_counter cross_runtime_run_tez =
    let counter_storage_pos = "0x00" in
    let*@ evm_count =
      Rpc.get_storage_at
        ~address:cross_runtime_run_tez
        ~pos:counter_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmCrossRuntimeRunnerTez `count` %R but got %L") ;
    unit
end

(** EVM contract that iterates [run()] over its [callees].  Before
 *  each call, increments [counter].  If a callee's revert is caught,
 *  increments [catches] instead of propagating.  After all calls,
 *  reverts if initialised to do so, otherwise increments [counter]. *)
module EvmMultiRunCaller = struct
  open EvmContract
  include EvmRunner

  let deploy =
    deploy_solidity_contract ~contract:Solidity_contracts.multi_run_caller

  (** Initialises the contract.  [callees] is the list of contracts
   *  that will be called on [run()], together with whether their
   *  revert should be caught.  [revert] controls whether the contract
   *  reverts after all calls. *)
  let init ~sequencer ~sender ~nonce ~value ~revert ~callees multi_run_caller =
    let callees_arg =
      let pp_comma fmt () = Format.fprintf fmt "," in
      let pp_callee fmt (addr, do_catch) =
        Format.fprintf fmt "(%s,%b)" addr do_catch
      in
      Format.asprintf
        "[%a]"
        (Format.pp_print_list ~pp_sep:pp_comma pp_callee)
        callees
    in
    let* _receipt =
      craft_and_send_transaction
        ~sequencer
        ~sender
        ~nonce
        ~value
        ~address:multi_run_caller
        ~abi_signature:"initialize(bool,(address,bool)[])"
        ~arguments:[string_of_bool revert; callees_arg]
        ()
    in
    unit

  (** Asserts that the contract's [catches] and [counter] equal the
   *  expected values. *)
  let check_storage ~sequencer ?(expected_catches = 0) ~expected_counter
      multi_run_caller =
    let catches_storage_pos = "0x00" in
    let*@ evm_catches =
      Rpc.get_storage_at
        ~address:multi_run_caller
        ~pos:catches_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_catches = expected_catches)
        int
        ~error_msg:"Expected EvmMultiRunCaller `catches` %R but got %L") ;
    let counter_storage_pos = "0x01" in
    let*@ evm_count =
      Rpc.get_storage_at
        ~address:multi_run_caller
        ~pos:counter_storage_pos
        sequencer
    in
    Check.(
      (int_of_string evm_count = expected_counter)
        int
        ~error_msg:"Expected EvmMultiRunCaller `count` %R but got %L") ;
    unit
end

(** Common [%run] caller for Tezos contracts. *)
module TezRunner = struct
  open TezContract

  (** Calls [%run] with [Unit] via the delayed inbox. *)
  let call_run ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ?amount ?gas_limit runner =
    call_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ?amount
      ?gas_limit
      ~dest:runner
      ~arg_data:"Unit"
      ~entrypoint:"run"
      ()
end

(** Tezos contract that forwards [%run] to an EVM target via the
 *  CRAC gateway.  Increments [counter] before and after the
 *  cross-runtime call. *)
module TezCrossRuntimeRunnerEvm = struct
  open TezContract
  include TezRunner

  (** Originates the contract.  [evm_contract_target_address] is the
   *  EVM address that will be called on [%run]. *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance
      ~evm_contract_target_address () =
    let script_name = ["mini_scenarios"; "cross_runtime_run_evm"] in
    let init_storage_data = sf {|Pair 0 "%s"|} evm_contract_target_address in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sc_rollup_node ~expected_counter
      (cross_runtime_run_tez_hex, _cross_runtime_run_tez_address) =
    let* storage = get_storage ~sc_rollup_node cross_runtime_run_tez_hex in
    (* Pair(nat %count, string %destination) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezCrossRuntimeRunnerEvm `count` %R but got %L") ;
    unit
end

(** Tezos contract that iterates [%run] over its [callees].  Before
 *  each call, increments [counter].  After all calls, reverts if
 *  initialised to do so, otherwise increments [counter]. *)
module TezMultiRunCaller = struct
  open TezContract
  include TezRunner

  (** Originates the contract.  [callees] is the list of contracts
   *  that will be called on [%run].  [revert] controls whether the
   *  contract reverts after all calls. *)
  let originate ~sc_rollup_address ~sc_rollup_node ~client ~l1_contracts
      ~sequencer ~source ~counter ~protocol ?init_balance ~revert ~callees () =
    let script_name = ["mini_scenarios"; "multi_run_caller"] in
    let init_storage_data =
      let pp_bool fmt b =
        if b then Format.fprintf fmt "True" else Format.fprintf fmt "False"
      in
      let pp_semicolon fmt () = Format.fprintf fmt ";" in
      let pp_callee fmt = Format.fprintf fmt {|"%s"|} in
      Format.asprintf
        {|Pair 0 (Pair %a {%a})|}
        pp_bool
        revert
        (Format.pp_print_list ~pp_sep:pp_semicolon pp_callee)
        callees
    in
    originate_contract_via_delayed_inbox
      ~sc_rollup_address
      ~sc_rollup_node
      ~client
      ~l1_contracts
      ~sequencer
      ~source
      ~counter
      ~script_name
      ~init_storage_data
      ?init_balance
      protocol

  (** Asserts that the contract's [counter] equals [expected_counter]. *)
  let check_storage ~sc_rollup_node ~expected_counter
      (multi_run_caller_hex, _multi_run_caller_address) =
    let* storage = get_storage ~sc_rollup_node multi_run_caller_hex in
    (* Pair(int %counter, Pair(bool %willRevert, list address %callees)) *)
    let counter = JSON.(storage |-> "args" |=> 0 |-> "int" |> as_int) in
    Check.(
      (counter = expected_counter)
        int
        ~error_msg:"Expected TezMultiRunCaller `count` %R but got %L") ;
    unit
end

(** Wraps the CRAC runner contract modules into a first-class module
 *  that manages the shared test state (nonces, counters, sequencer
 *  setup) so that tests only provide scenario-specific parameters.
 *  Contracts are represented as {!evm_runner} and {!tez_runner}
 *  values to distinguish EVM and Tezos contracts at the type level. *)
module CracRunnerWrapper = struct
  type evm_runner = [`Evm_runner of string]

  type tez_runner = [`Tez_runner of string * string]

  (** Simplified interface over the CRAC runner contracts.  Each
   *  sub-module mirrors its namesake but with nonces, counters and
   *  sequencer setup already applied. *)
  module type S = sig
    val sequencer : Evm_node.t

    val sc_rollup_node : Sc_rollup_node.t

    val client : Client.t

    val sender : Eth_account.t

    val source : Account.key

    val evm_nonce : unit -> int

    val tez_counter : unit -> int

    module EvmRunner : sig
      val call_run :
        ?expected_status:bool ->
        ?value:Wei.t ->
        ?access_list:(string * string list) list ->
        evm_runner ->
        int64 Lwt.t
    end

    module EvmCrossRuntimeRunnerTez : sig
      val deploy_and_init : ?value:Wei.t -> tez_runner -> evm_runner Lwt.t

      val check_storage : expected_counter:int -> evm_runner -> unit Lwt.t
    end

    module EvmMultiRunCaller : sig
      val deploy_and_init :
        ?value:Wei.t ->
        ?revert:bool ->
        ?callees:(evm_runner * bool) list ->
        unit ->
        evm_runner Lwt.t

      val check_storage :
        ?expected_catches:int ->
        expected_counter:int ->
        evm_runner ->
        unit Lwt.t
    end

    module TezRunner : sig
      val call_run : ?amount:int -> ?gas_limit:int -> tez_runner -> unit Lwt.t
    end

    module TezCrossRuntimeRunnerEvm : sig
      val originate : ?init_balance:int -> evm_runner -> tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end

    module TezMultiRunCaller : sig
      val originate :
        ?init_balance:int ->
        ?revert:bool ->
        ?callees:tez_runner list ->
        unit ->
        tez_runner Lwt.t

      val check_storage : expected_counter:int -> tez_runner -> unit Lwt.t
    end
  end

  (** Builds a {!S} module from the given test setup.  [nonce] and
   *  [counter] are the initial EVM nonce and Tezos operation counter;
   *  both are auto-incremented on each use. *)
  let build ?(nonce = 0) ?(counter = 0)
      ~sequencer_setup:
        ({
           sc_rollup_address;
           sc_rollup_node;
           client;
           l1_contracts;
           sequencer;
           evm_version;
           _;
         } :
          Tezt_etherlink.Setup.sequencer_setup) ~sender ~source protocol :
      (module S) =
    let ref_nonce = ref nonce in
    let ref_counter = ref counter in

    let evm_nonce () =
      let nonce = !ref_nonce in
      incr ref_nonce ;
      nonce
    in
    let tez_counter () =
      let counter = !ref_counter in
      incr ref_counter ;
      counter
    in
    let module Helper = struct
      let sequencer = sequencer

      let sc_rollup_node = sc_rollup_node

      let client = client

      let sender = sender

      let source = source

      let evm_nonce = evm_nonce

      let tez_counter = tez_counter

      module EvmRunner = struct
        let call_run ?expected_status ?(value = Wei.zero) ?access_list
            (`Evm_runner runner) =
          let* gas_used =
            EvmRunner.call_run
              ?expected_status
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ?access_list
              runner
          in
          let* () =
            Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
          in
          return gas_used
      end

      module EvmCrossRuntimeRunnerTez = struct
        let deploy_and_init ?(value = Wei.zero) (`Tez_runner (_, address)) =
          let* addr =
            EvmCrossRuntimeRunnerTez.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmCrossRuntimeRunnerTez.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~tez_contract_target_address:address
              addr
          in
          return (`Evm_runner addr)

        let check_storage ~expected_counter (`Evm_runner runner) =
          EvmCrossRuntimeRunnerTez.check_storage
            ~sequencer
            ~expected_counter
            runner
      end

      module EvmMultiRunCaller = struct
        let deploy_and_init ?(value = Wei.zero) ?(revert = false)
            ?(callees = []) () =
          let callees =
            List.map
              (fun (`Evm_runner addr, do_catch) -> (addr, do_catch))
              callees
          in
          let* addr =
            EvmMultiRunCaller.deploy
              ~evm_version
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ()
          in
          let* () =
            EvmMultiRunCaller.init
              ~sequencer
              ~sender
              ~nonce:(evm_nonce ())
              ~value
              ~revert
              ~callees
              addr
          in
          return (`Evm_runner addr)

        let check_storage ?expected_catches ~expected_counter
            (`Evm_runner runner) =
          EvmMultiRunCaller.check_storage
            ~sequencer
            ?expected_catches
            ~expected_counter
            runner
      end

      module TezRunner = struct
        let call_run ?amount ?gas_limit (`Tez_runner (_, runner)) =
          TezRunner.call_run
            ~sc_rollup_address
            ~sc_rollup_node
            ~client
            ~l1_contracts
            ~sequencer
            ~source
            ~counter:(tez_counter ())
            ?amount
            ?gas_limit
            runner
      end

      module TezCrossRuntimeRunnerEvm = struct
        let originate ?init_balance (`Evm_runner address) =
          let* contract_hex, address =
            TezCrossRuntimeRunnerEvm.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~evm_contract_target_address:address
              ()
          in
          return (`Tez_runner (contract_hex, address))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezCrossRuntimeRunnerEvm.check_storage
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)
      end

      module TezMultiRunCaller = struct
        let originate ?init_balance ?(revert = false) ?(callees = []) () =
          let callees =
            List.map (fun (`Tez_runner (_runner_hex, runner)) -> runner) callees
          in
          let* runner_hex, runner =
            TezMultiRunCaller.originate
              ~sc_rollup_address
              ~sc_rollup_node
              ~client
              ~l1_contracts
              ~sequencer
              ~source
              ~counter:(tez_counter ())
              ~protocol
              ?init_balance
              ~revert
              ~callees
              ()
          in
          return (`Tez_runner (runner_hex, runner))

        let check_storage ~expected_counter
            (`Tez_runner (runner_hex, runner_address)) =
          TezMultiRunCaller.check_storage
            ~sc_rollup_node
            ~expected_counter
            (runner_hex, runner_address)
      end
    end in
    (module Helper)
end

(** Registers a fullstack CRAC runner test.  Sets up the sequencer,
 *  builds a {!CracRunnerWrapper.S} and passes it to [body]. *)
let register_crac_runner_test ~title ?(tags = []) body =
  let with_runtimes = Tezosx_runtime.[Tezos] in
  let tags =
    ["tezosx"]
    @ List.map Tezosx_runtime.tag with_runtimes
    @ ["crac"; "runner"] @ tags
  in
  Setup.register_test
    ~__FILE__
    ~rpc_server:Evm_node.Resto
    ~title
    ~time_between_blocks:Nothing
    ~tags
    ~kernel:Latest
    ~with_runtimes
    ~enable_dal:false
    ~enable_multichain:false
    ~tez_bootstrap_accounts:Evm_node.tez_default_bootstrap_accounts
  @@ fun sequencer_setup protocol ->
  let sender = Eth_account.bootstrap_accounts.(0) in
  let source = Constant.bootstrap5 in
  let (module Wrapper) =
    CracRunnerWrapper.build ~counter:1 ~sequencer_setup ~sender ~source protocol
  in
  body (module Wrapper : CracRunnerWrapper.S)

(** Simple EVM-to-TEZ cross-runtime call.
 *
 *    EVM[evm_runner]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *
 *)
let test_crac_evm_to_tez =
  register_crac_runner_test ~title:"CRAC: EVM runner calls TEZ runner"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner calling the bridge" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM runner" ;
  let* _ = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  unit

(** Multiple independent CRAC crossings.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *
 *)
let test_crac_evm_multiple_independent_crossings =
  register_crac_runner_test ~title:"CRAC: multiple independent crossings"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_2
  in
  unit

(** Double CRAC crossing: EVM calls EVM inner both directly and through
 *  an EVM->TEZ->EVM round-trip bridge chain.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_inner]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *     |-> EVM[evm_inner]
 *
 *)
let test_crac_evm_double_crossing =
  register_crac_runner_test
    ~title:"CRAC: double crossing EVM via TEZ back to EVM"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM inner runner" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM inner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  Log.debug ~prefix "Deploy EVM bridge to TEZ bridge" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Deploy EVM main calling inner directly and via bridges" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_inner, false); (evm_bridge, false); (evm_inner, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_inner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:4 evm_main in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Shared TEZ leaf called three times: twice directly via the same EVM
 *  bridge, and once through a 3-CRAC chain that also ends at the same leaf.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_direct] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_chain] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_direct] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_evm_shared_leaf_via_direct_and_chain =
  register_crac_runner_test
    ~title:"CRAC: EVM shared TEZ leaf via direct bridge and 3-CRAC chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge_direct to TEZ leaf" ;
  let* evm_bridge_direct = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM bridge_inner to TEZ leaf (for chain)" ;
  let* evm_bridge_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Originate TEZ bridge to EVM bridge_inner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge_inner in
  Log.debug ~prefix "Deploy EVM bridge_chain to TEZ bridge" ;
  let* evm_bridge_chain = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [
          (evm_bridge_direct, false);
          (evm_bridge_chain, false);
          (evm_bridge_direct, false);
        ]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:4 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge_direct
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_chain
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_inner
  in
  unit

(** EVM 5-crossing chain.
 *
 *    EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_evm_5_crossing_chain =
  register_crac_runner_test ~title:"CRAC: EVM 5-crossing chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Call EVM a" ;
  let* _ = EvmRunner.call_run evm_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_e in
  unit

(** Simple TEZ-to-EVM cross-runtime call.
 *
 *     TEZ[tez_runner]
 *      |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *
 *)
let test_crac_tez_to_evm =
  register_crac_runner_test ~title:"CRAC: TEZ runner calls EVM runner"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ runner" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Multiple independent TEZ-to-EVM crossings from a single TEZ caller.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *
 *)
let test_crac_tez_multiple_independent_crossings =
  register_crac_runner_test
    ~title:"CRAC: TEZ multiple independent crossings to EVM"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_2
  in
  unit

(** TEZ-to-EVM-to-TEZ double crossing.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_inner]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *     |-> TEZ[tez_inner]
 *
 *)
let test_crac_tez_double_crossing =
  register_crac_runner_test ~title:"CRAC: TEZ-to-EVM-to-TEZ double crossing"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ inner runner" ;
  let* tez_inner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ inner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  Log.debug ~prefix "Originate TEZ bridge from EVM bridge" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_inner; tez_bridge; tez_inner] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:3 tez_inner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  unit

(** Shared EVM leaf called three times: twice directly via the same TEZ
 *  bridge, and once through a 3-CRAC chain that also ends at the same leaf.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_direct] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_chain] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_direct] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_shared_leaf_via_direct_and_chain =
  register_crac_runner_test
    ~title:"CRAC: TEZ shared EVM leaf via direct bridge and 3-CRAC chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge_direct to EVM leaf" ;
  let* tez_bridge_direct = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ bridge_inner to EVM leaf (for chain)" ;
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM bridge to TEZ bridge_inner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner in
  Log.debug ~prefix "Originate TEZ bridge_chain to EVM bridge" ;
  let* tez_bridge_chain = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_direct; tez_bridge_chain; tez_bridge_direct]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:3 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_direct
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_chain
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_inner
  in
  unit

(** TEZ 5-crossing chain.
 *
 *    TEZ[tez_a] ~CRAC~> EVM[evm_b] ~CRAC~> TEZ[tez_c] ~CRAC~> EVM[evm_d] ~CRAC~> TEZ[tez_e] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_5_crossing_chain =
  register_crac_runner_test ~title:"CRAC: TEZ 5-crossing chain"
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Call TEZ a" ;
  (* The default gas_limit (10_000) is too low for 5 crossings: each
     round-trip costs ~50K EVM gas (~5_000 gas units), exhausting the
     budget after ~2 crossings. 20_000 units (= 200K EVM gas) provides
     enough headroom even with correct cross-runtime gas charge-back. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_leaf in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_e in
  unit

(** Access list preserved across EVM->TEZ->EVM CRAC.
 *
 *  Deploys one double-crossing chain:
 *
 *    EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *
 *  Calls [run()] twice on the same chain: first without an access list,
 *  then with one pre-warming [evm_inner]'s counter storage slot.
 *  The second call should use strictly less gas.
 *)
let test_crac_access_list_preserved =
  register_crac_runner_test
    ~title:"CRAC: access list preserved across EVM->TEZ->EVM"
    ~tags:["access_list"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-AL" in
  Log.debug ~prefix "Deploy chain" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  (* Call 1: without access list *)
  Log.debug ~prefix "Call run() without access list" ;
  let* gas_without = EvmRunner.call_run evm_bridge in
  (* Call 2: with access list pre-warming evm_inner's counter slot *)
  let (`Evm_runner evm_inner_addr) = evm_inner in
  let access_list =
    [
      ( evm_inner_addr,
        ["0x0000000000000000000000000000000000000000000000000000000000000001"]
      );
    ]
  in
  Log.debug ~prefix "Call run() with access list" ;
  let* gas_with = EvmRunner.call_run ~access_list evm_bridge in
  (* The access list warms evm_inner's storage slot before the
     EVM->TEZ->EVM round-trip, so the 3rd EVM execution (evm_inner
     after the round-trip) pays warm-access cost instead of cold. *)
  Log.info "Gas with access list: %Ld, gas without: %Ld" gas_with gas_without ;
  Check.(
    (gas_with < gas_without)
      int64
      ~error_msg:
        "Expected gas with access list (%L) to be strictly less than without \
         (%R)") ;
  (* Verify correctness: counters incremented twice (once per call) *)
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_inner in
  unit

(** TEZ CRAC target reverts.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_evm_to_tez_reverts =
  register_crac_runner_test ~title:"CRAC: EVM to TEZ reverts" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ reverter" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** EVM CRAC target reverts.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_tez_to_evm_reverts =
  register_crac_runner_test ~title:"CRAC: TEZ to EVM reverts" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM reverter" ;
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridge to EVM reverter" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  Log.debug ~prefix "Originate TEZ main caller" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main (BackTracked due to EVM revert)" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters are all zero (state reverted)" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** CRAC: when a Michelson transaction makes a single CRAC into EVM,
 *  the EVM block must contain a fake transaction carrying the logs
 *  emitted during the cross-runtime EVM execution.
 *
 *     TEZ[tez_runner]
 *      |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *
 *  After the Michelson transaction, we fetch the latest EVM block and
 *  assert it contains at least one transaction (the CRAC envelope).
 *)
let test_crac_tez_to_evm_fake_tx_in_block =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM produces fake EVM transaction in block"
    ~tags:["crac_tx"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-TX" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ runner (triggers CRAC into EVM)" ;
  let* () = TezRunner.call_run tez_runner in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Fetch latest EVM block" ;
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_count =
    match block.transactions with
    | Block.Hash txs -> List.length txs
    | Block.Full txs -> List.length txs
    | Block.Empty -> 0
  in
  Log.info "EVM block %ld contains %d transaction(s)" block.number tx_count ;
  Check.(
    (tx_count >= 1)
      int
      ~error_msg:
        "Expected at least 1 transaction in the EVM block (the CRAC envelope), \
         but found %L") ;
  unit

(** Two separate TEZ->EVM CRAC calls in separate blocks must produce fake
 *  transactions with distinct hashes.  Regression test for a bug where the
 *  CRAC transaction hash was computed from the block-local transaction index
 *  only (without the block number), causing a UNIQUE constraint violation in
 *  the EVM node's transactions table on the second insertion.
 *)
let test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM fake transactions have unique hashes across blocks"
    ~tags:["crac_tx"; "regression"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-HASH" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner calling the bridge" ;
  let* tez_runner = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "First CRAC call (TEZ->EVM)" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_runner in
  Log.debug ~prefix "Second CRAC call (TEZ->EVM) — must not crash the node" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  Log.debug ~prefix "Both CRAC fake transactions stored successfully" ;
  unit

(** EVM journal is preserved across CrossRuntime boundaries: TEZ revert must
 *  roll back inner EVM storage changes made via a TEZ->EVM CRAC.
 *
 *    EVM[evm_outer] ~CRAC~> TEZ[tez_runner]
 *                             |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *                             |-> REVERT
 *
 *  evm_inner gets called from within the TEZ execution (TEZ->EVM inner CRAC).
 *  When tez_runner reverts, both the TEZ state and evm_inner's EVM storage
 *  must be rolled back to zero.
 *)
let test_crac_tez_revert_rolls_back_inner_evm_storage =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert rolls back inner EVM storage"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy inner EVM contract" ;
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to inner EVM" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  Log.debug ~prefix "Originate TEZ runner (calls bridge, then reverts)" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM outer bridge to TEZ runner" ;
  let* evm_outer = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Call EVM outer (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_outer in
  Log.debug ~prefix "Verify inner EVM storage was rolled back" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_inner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_outer
  in
  unit

(** TEZ caller reverts after a successful cross-runtime call to EVM.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_propagates_to_evm =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert propagates to EVM"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge] ~revert:true ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** EVM journal state preserved across CRAC sub-calls.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_ok] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_reverter] (pure EVM revert)
 *
 *  The first double-CRAC modifies journal state (logs, checkpoints),
 *  making the outer checkpoint log_i > 0. The caught EVM revert
 *  triggers REVM's logs[checkpoint.log_i..] slice operation which
 *  panics if the prior CRAC drained or reset journal state.
 *)
let test_crac_evm_journal_state_preserved =
  register_crac_runner_test
    ~title:"CRAC: EVM journal state preserved"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_ok = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_ok, false); (evm_reverter, true)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:3
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_ok
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Simplest catch scenario: a TEZ revert behind one CRAC is caught by the
 *  EVM caller. Verifies that Solidity try/catch works across a single
 *  cross-runtime boundary: the caught callee is rolled back while the
 *  outer caller continues execution.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                         |-> REVERT
 *)
let test_crac_catch_tez_revert =
  register_crac_runner_test ~title:"CRAC: catch TEZ revert" ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** EVM caller reverts after a successful cross-runtime call to TEZ.
 *  Tests whether the TEZ-side effects are rolled back.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_propagates_to_tez =
  register_crac_runner_test
    ~title:"CRAC: EVM revert propagates to TEZ"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf runner" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM main calling bridge then reverting" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge, false)]
      ~revert:true
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Second CRAC target TEZ reverts, rolling back the first.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_second_crac_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: second CRAC target TEZ reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** EVM revert rolls back two successful CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_rolls_back_two_cracs =
  register_crac_runner_test
    ~title:"CRAC: EVM revert rolls back two CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runners" ;
  let* tez_runner_1 = TezMultiRunCaller.originate () in
  let* tez_runner_2 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main with revert" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** Deep first branch with TEZ revert in second branch.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_runner_1] ~CRAC~> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_leaf]
 *     |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_deep_branch_with_second_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: deep first branch with TEZ revert in second"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build first branch (deep chain)" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_bridge_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_runner_1 = TezCrossRuntimeRunnerEvm.originate evm_bridge_inner in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  Log.debug ~prefix "Build second branch (TEZ revert)" ;
  let* tez_runner_2 = TezMultiRunCaller.originate ~revert:true () in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_1
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  unit

(** Nested CRAC revert cascade without catch.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *                                 |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *                                 |-> REVERT
 *
 *)
let test_crac_nested_revert_cascade_without_catch =
  register_crac_runner_test
    ~title:"CRAC: nested revert cascade without catch"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runner" ;
  let* evm_runner = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ runner with revert" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** Double-nested CRAC with intermediate EVM revert.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_outer] ~CRAC~> TEZ[tez_runner_outer] ~CRAC~> EVM[evm_runner]
 *                                                                     |-> EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_runner_inner] ~CRAC~> EVM[evm_leaf]
 *                                                                     |-> REVERT
 *
 *)
let test_crac_double_nested_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: double-nested CRAC with intermediate EVM revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build inner chain" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_runner_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_inner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_inner
  in
  Log.debug ~prefix "Deploy EVM runner (calls inner chain then reverts)" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_inner, false)]
      ()
  in
  Log.debug ~prefix "Build outer chain" ;
  let* tez_runner_outer = TezCrossRuntimeRunnerEvm.originate evm_runner in
  let* evm_bridge_outer =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_outer
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_outer, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_outer
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_runner_inner
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_outer
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  unit

(** Deep nesting across 6 CRAC levels.
 *
 *    EVM[a] ~CRAC~> TEZ[b] ~CRAC~> EVM[c] ~CRAC~> TEZ[d] ~CRAC~> EVM[e] ~CRAC~> TEZ[f]
 *                                                                               |-> REVERT
 *
 *)
let test_crac_deep_nesting_6_levels =
  register_crac_runner_test
    ~title:"CRAC: deep nesting across 6 CRAC levels"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 6-level CRAC chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Call EVM a (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  unit

(** EVM revert after nested CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[a] ~CRAC~> TEZ[b] ~CRAC~> EVM[c] ~CRAC~> TEZ[d] ~CRAC~> EVM[e] ~CRAC~> TEZ[f]
 *     |-> REVERT
 *
 *)
let test_crac_evm_revert_after_nested_cracs =
  register_crac_runner_test
    ~title:"CRAC: EVM revert after nested CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with revert" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~revert:true ~callees:[(evm_a, false)] ()
  in
  Log.debug ~prefix "Call EVM main (expected failure)" ;
  let* _ = EvmRunner.call_run ~expected_status:false evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  unit

(** EVM CRAC target reverts.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_reverter]
 *                                 |-> REVERT
 *
 *)
let test_crac_evm_target_reverts =
  register_crac_runner_test
    ~title:"CRAC: EVM CRAC target reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM reverter" ;
  let* evm_reverter = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridge to EVM reverter" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  unit

(** Second CRAC target EVM reverts, rolling back the first.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_second_crac_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: second CRAC target EVM reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** TEZ revert rolls back two successful CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_rolls_back_two_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert rolls back two CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM runners" ;
  let* evm_runner_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridges" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~revert:true
      ~callees:[tez_bridge_1; tez_bridge_2]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** Deep first branch with EVM revert in second branch.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_runner_1] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_runner_2]
 *                                   |-> REVERT
 *
 *)
let test_crac_deep_branch_with_second_evm_revert =
  register_crac_runner_test
    ~title:"CRAC: deep first branch with EVM revert in second"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build first branch (deep chain)" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_runner_1 =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner
  in
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_runner_1 in
  Log.debug ~prefix "Build second branch (EVM revert)" ;
  let* evm_runner_2 = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_runner_2 in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_bridge_1; tez_bridge_2] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner_2 in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  unit

(** TEZ nested revert cascade without catch.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_runner]
 *                                 |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *                                 |-> REVERT
 *
 *)
let test_crac_tez_nested_revert_cascade_without_catch =
  register_crac_runner_test
    ~title:"CRAC: TEZ nested revert cascade without catch"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner with revert" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM runner" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_runner in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Double-nested CRAC with intermediate TEZ revert.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_outer] ~CRAC~> EVM[evm_runner_outer] ~CRAC~> TEZ[tez_runner]
 *                                                                     |-> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_runner_inner] ~CRAC~> TEZ[tez_leaf]
 *                                                                     |-> REVERT
 *
 *)
let test_crac_double_nested_tez_revert =
  register_crac_runner_test
    ~title:"CRAC: double-nested CRAC with intermediate TEZ revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build inner chain" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* evm_runner_inner = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_runner_inner in
  Log.debug ~prefix "Originate TEZ runner (calls inner chain then reverts)" ;
  let* tez_runner =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge_inner] ()
  in
  Log.debug ~prefix "Build outer chain" ;
  let* evm_runner_outer = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let* tez_bridge_outer = TezCrossRuntimeRunnerEvm.originate evm_runner_outer in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_bridge_outer] () in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_outer
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_runner_inner
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_outer
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  unit

(** TEZ deep nesting across 6 CRAC levels.
 *
 *    TEZ[a] ~CRAC~> EVM[b] ~CRAC~> TEZ[c] ~CRAC~> EVM[d] ~CRAC~> TEZ[e] ~CRAC~> EVM[f]
 *                                                                               |-> REVERT
 *
 *)
let test_crac_tez_deep_nesting_6_levels =
  register_crac_runner_test
    ~title:"CRAC: TEZ deep nesting across 6 CRAC levels"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 6-level CRAC chain (inside-out)" ;
  let* evm_f = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_f in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Call TEZ a" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_a in
  Log.debug ~prefix "Verify counters" ;
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_f in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_e in
  unit

(** TEZ revert after nested CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[a] ~CRAC~> EVM[b] ~CRAC~> TEZ[c] ~CRAC~> EVM[d] ~CRAC~> TEZ[e] ~CRAC~> EVM[f]
 *     |-> REVERT
 *
 *)
let test_crac_tez_revert_after_nested_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ revert after nested CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing CRAC chain (inside-out)" ;
  let* evm_f = EvmMultiRunCaller.deploy_and_init () in
  let* tez_e = TezCrossRuntimeRunnerEvm.originate evm_f in
  let* evm_d = EvmCrossRuntimeRunnerTez.deploy_and_init tez_e in
  let* tez_c = TezCrossRuntimeRunnerEvm.originate evm_d in
  let* evm_b = EvmCrossRuntimeRunnerTez.deploy_and_init tez_c in
  let* tez_a = TezCrossRuntimeRunnerEvm.originate evm_b in
  Log.debug ~prefix "Originate TEZ main with revert" ;
  let* tez_main =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_a] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_f in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_a in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_b in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_c in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_d in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_e in
  unit

(** Caught EVM revert between successful cross-runtime calls.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> (Catch) EVM[evm_reverter]
 *     |           |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |           |-> REVERT
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_catch_evm_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch EVM revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling bridge" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge, false); (evm_reverter, true); (evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Caught cross-runtime TEZ revert between successful CRACs.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |-> (Catch) EVM[evm_bridge_rev] ~CRAC~> TEZ[tez_reverter]
 *     |                                       |-> TEZ[tez_leaf]
 *     |                                       |-> REVERT
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_catch_tez_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch cross-runtime TEZ revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Originate TEZ reverter calling leaf" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_leaf] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge_rev = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [(evm_bridge, false); (evm_bridge_rev, true); (evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:4 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_rev
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** Caught TEZ revert with nested CRAC to EVM inside caught block.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_reverter]
 *     |                                     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_leaf]
 *     |                                     |-> REVERT
 *     |-> EVM[evm_leaf]
 *
 *)
let test_crac_catch_tez_revert_with_nested_crac =
  register_crac_runner_test
    ~title:"CRAC: catch TEZ revert with nested CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM bridge_1 to TEZ bridge (double CRAC)" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  Log.debug ~prefix "Originate TEZ reverter calling bridge" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge] ()
  in
  Log.debug ~prefix "Deploy EVM bridge_2 to TEZ reverter" ;
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, true); (evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** Caught deep EVM revert propagated through double CRAC.
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_leaf]
 *     |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_reverter]
 *     |                                                                |-> EVM[evm_leaf]
 *     |                                                                |-> REVERT
 *     |-> EVM[evm_leaf]
 *
 *)
let test_crac_catch_deep_evm_revert_through_double_crac =
  register_crac_runner_test
    ~title:"CRAC: catch deep EVM revert through double CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Build first double CRAC chain to evm_leaf" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_1 in
  Log.debug ~prefix "Deploy EVM reverter calling evm_leaf" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Build second double CRAC chain to evm_reverter" ;
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_2 in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(evm_bridge_1, false); (evm_bridge_2, true); (evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** TEZ-initiated: caught EVM revert between CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_leaf]
 *     |-> TEZ[tez_bridge] ~CRAC~> EVM[evm_catcher]
 *     |                           |-> (Catch) EVM[evm_reverter]
 *     |                           |           |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_leaf]
 *     |                           |           |-> REVERT
 *     |-> TEZ[tez_leaf]
 *
 *)
let test_crac_tez_catch_evm_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch EVM revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ leaf" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling bridge" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_reverter, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate ~callees:[tez_leaf; tez_bridge; tez_leaf] ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  (* The default gas_limit (10_000) is too low: the inner try-catch path
     crosses two runtimes (TEZ->EVM->TEZ) and adds extra EVM contract calls,
     exhausting the budget. 20_000 provides enough headroom. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** TEZ-initiated: caught cross-runtime TEZ revert between CRACs.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_catcher] ~CRAC~> EVM[evm_catcher]
 *     |                                   |-> (Catch) EVM[evm_bridge_inner] ~CRAC~> TEZ[tez_reverter]
 *     |                                                                             |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |                                                                             |-> REVERT
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_catch_tez_revert_between_cracs =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch TEZ revert between CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge_leaf = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Originate TEZ reverter calling bridge to EVM leaf" ;
  let* tez_reverter =
    TezMultiRunCaller.originate ~revert:true ~callees:[tez_bridge_leaf] ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge_inner =
    EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter
  in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_inner, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge_catcher = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_leaf; tez_bridge_catcher; tez_bridge_leaf]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_leaf
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage
      ~expected_counter:2
      tez_bridge_catcher
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_inner
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  unit

(** TEZ-initiated: caught deep EVM revert through double CRAC.
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *     |-> TEZ[tez_bridge_catcher] ~CRAC~> EVM[evm_catcher]
 *     |                                   |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge_inner] ~CRAC~> EVM[evm_reverter]
 *     |                                                                                                     |-> EVM[evm_leaf]
 *     |                                                                                                     |-> REVERT
 *     |-> TEZ[tez_bridge_leaf] ~CRAC~> EVM[evm_leaf]
 *
 *)
let test_crac_tez_catch_deep_revert_through_double_crac =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch deep revert through double CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy EVM leaf" ;
  let* evm_leaf = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate TEZ bridge to EVM leaf" ;
  let* tez_bridge_leaf = TezCrossRuntimeRunnerEvm.originate evm_leaf in
  Log.debug ~prefix "Deploy EVM reverter calling evm_leaf" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_leaf, false)]
      ()
  in
  Log.debug ~prefix "Build double CRAC chain to EVM reverter" ;
  let* tez_bridge_inner = TezCrossRuntimeRunnerEvm.originate evm_reverter in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge_inner in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_bridge_catcher = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Originate TEZ main" ;
  let* tez_main =
    TezMultiRunCaller.originate
      ~callees:[tez_bridge_leaf; tez_bridge_catcher; tez_bridge_leaf]
      ()
  in
  Log.debug ~prefix "Call TEZ main" ;
  (* The default gas_limit (10_000) is too low: the inner try-catch path
     crosses three runtimes (TEZ->EVM->TEZ->EVM) and adds extra EVM contract calls,
     exhausting the budget. 20_000 provides enough headroom. *)
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_main in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezMultiRunCaller.check_storage ~expected_counter:4 tez_main in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:4 tez_bridge_leaf
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_leaf in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage
      ~expected_counter:2
      tez_bridge_catcher
  in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_inner
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  unit

(** Caught EVM revert after 3 sequential CRACs.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_reverter]
 *                 |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_leaf_1]
 *                 |-> EVM[evm_bridge_2] ~CRAC~> TEZ[tez_leaf_2]
 *                 |-> EVM[evm_bridge_3] ~CRAC~> TEZ[tez_leaf_3]
 *                 |-> REVERT
 *
 *)
let test_crac_catch_revert_after_multiple_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch revert after multiple sequential CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate 3 TEZ leaves" ;
  let* tez_leaf_1 = TezMultiRunCaller.originate () in
  let* tez_leaf_2 = TezMultiRunCaller.originate () in
  let* tez_leaf_3 = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy 3 EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_2 in
  let* evm_bridge_3 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf_3 in
  Log.debug ~prefix "Deploy EVM reverter calling 3 bridges" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:
        [(evm_bridge_1, false); (evm_bridge_2, false); (evm_bridge_3, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_reverter, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_3
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_2 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_leaf_3 in
  unit

(** Caught TEZ revert after 3 return CRACs to EVM.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_reverter]
 *                                         |-> TEZ[tez_bridge_1] ~CRAC~> EVM[evm_leaf_1]
 *                                         |-> TEZ[tez_bridge_2] ~CRAC~> EVM[evm_leaf_2]
 *                                         |-> TEZ[tez_bridge_3] ~CRAC~> EVM[evm_leaf_3]
 *                                         |-> REVERT
 *
 *)
let test_crac_catch_tez_revert_after_multiple_return_cracs =
  register_crac_runner_test
    ~title:"CRAC: catch TEZ revert after multiple return CRACs"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Deploy 3 EVM leaves" ;
  let* evm_leaf_1 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_leaf_2 = EvmMultiRunCaller.deploy_and_init () in
  let* evm_leaf_3 = EvmMultiRunCaller.deploy_and_init () in
  Log.debug ~prefix "Originate 3 TEZ bridges to EVM leaves" ;
  let* tez_bridge_1 = TezCrossRuntimeRunnerEvm.originate evm_leaf_1 in
  let* tez_bridge_2 = TezCrossRuntimeRunnerEvm.originate evm_leaf_2 in
  let* tez_bridge_3 = TezCrossRuntimeRunnerEvm.originate evm_leaf_3 in
  Log.debug ~prefix "Originate TEZ reverter calling 3 bridges" ;
  let* tez_reverter =
    TezMultiRunCaller.originate
      ~revert:true
      ~callees:[tez_bridge_1; tez_bridge_2; tez_bridge_3]
      ()
  in
  Log.debug ~prefix "Deploy EVM bridge to TEZ reverter" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_1
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_2
  in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_bridge_3
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_1 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_2 in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_leaf_3 in
  unit

(** Caught EVM revert at end of 4-crossing chain.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e]
 *                                                                                               |-> REVERT
 *
 *)
let test_crac_catch_4_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: catch 4-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 4-crossing chain (inside-out)" ;
  let* evm_e = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_e in
  unit

(** TEZ-initiated: caught 4-crossing chain revert.
 *
 *    TEZ[tez_x] ~CRAC~> EVM[evm_catcher]
 *                       |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e]
 *                                                                                                                 |-> REVERT
 *
 *)
let test_crac_tez_catch_4_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch 4-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 4-crossing chain (inside-out)" ;
  let* evm_e = EvmMultiRunCaller.deploy_and_init ~revert:true () in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_x = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Call TEZ x" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_x in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_x in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_e in
  unit

(** Caught TEZ revert at end of 5-crossing chain.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_f]
 *                                                                                                                   |-> REVERT
 *
 *)
let test_crac_catch_5_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: catch 5-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  unit

(** TEZ-initiated: caught 5-crossing chain revert.
 *
 *    TEZ[tez_x] ~CRAC~> EVM[evm_catcher]
 *                       |-> (Catch) EVM[evm_a] ~CRAC~> TEZ[tez_b] ~CRAC~> EVM[evm_c] ~CRAC~> TEZ[tez_d] ~CRAC~> EVM[evm_e] ~CRAC~> TEZ[tez_f]
 *                                                                                                                                     |-> REVERT
 *
 *)
let test_crac_tez_catch_5_crossing_chain_revert =
  register_crac_runner_test
    ~title:"CRAC: TEZ-initiated catch 5-crossing chain revert"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Build 5-crossing chain (inside-out)" ;
  let* tez_f = TezMultiRunCaller.originate ~revert:true () in
  let* evm_e = EvmCrossRuntimeRunnerTez.deploy_and_init tez_f in
  let* tez_d = TezCrossRuntimeRunnerEvm.originate evm_e in
  let* evm_c = EvmCrossRuntimeRunnerTez.deploy_and_init tez_d in
  let* tez_b = TezCrossRuntimeRunnerEvm.originate evm_c in
  let* evm_a = EvmCrossRuntimeRunnerTez.deploy_and_init tez_b in
  Log.debug ~prefix "Deploy EVM catcher" ;
  let* evm_catcher =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_a, true)] ()
  in
  Log.debug ~prefix "Originate TEZ bridge to EVM catcher" ;
  let* tez_x = TezCrossRuntimeRunnerEvm.originate evm_catcher in
  Log.debug ~prefix "Call TEZ x" ;
  let* () = TezRunner.call_run ~gas_limit:200_000 tez_x in
  Log.debug ~prefix "Verify counters" ;
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_x in
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_catcher
  in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_a in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_b in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_c in
  let* () = TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:0 tez_d in
  let* () = EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_e in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_f in
  unit

(** Caught chained TEZ revert behind CRAC.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge] ~CRAC~> TEZ[tez_runner_1]
 *                                         |-> TEZ[tez_runner_2]
 *                                             |-> TEZ[tez_reverter]
 *                                                 |-> REVERT
 *
 *)
let test_crac_chained_tez_calls_behind_crac =
  register_crac_runner_test
    ~title:"CRAC: chained TEZ calls behind CRAC"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ reverter and chain" ;
  let* tez_reverter = TezMultiRunCaller.originate ~revert:true () in
  let* tez_runner_2 = TezMultiRunCaller.originate ~callees:[tez_reverter] () in
  let* tez_runner_1 = TezMultiRunCaller.originate ~callees:[tez_runner_2] () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ chain" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner_1 in
  Log.debug ~prefix "Deploy EVM main with catch" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, true)] ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:1
      ~expected_counter:2
      evm_main
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_runner_2 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge
  in
  unit

(** Nested catches with multiple reverts at two depths.
 *
 *    EVM[evm_main]
 *     |-> (Catch) EVM[evm_bridge_1] ~CRAC~> TEZ[tez_reverter_1]
 *     |                                     |-> REVERT
 *     |-> (Catch) EVM[evm_reverter]
 *     |           |-> (Catch) EVM[evm_bridge_2] ~CRAC~> TEZ[tez_reverter_2]
 *     |           |                                     |-> REVERT
 *     |           |-> EVM[evm_bridge_3] ~CRAC~> TEZ[tez_leaf]
 *     |           |-> REVERT
 *     |-> EVM[evm_bridge_4] ~CRAC~> TEZ[tez_leaf]
 *
 *)
let test_crac_nested_catches_with_multiple_reverts =
  register_crac_runner_test
    ~title:"CRAC: nested catches with multiple reverts"
    ~tags:["revert"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC" in
  Log.debug ~prefix "Originate TEZ leaf and reverters" ;
  let* tez_leaf = TezMultiRunCaller.originate () in
  let* tez_reverter_1 = TezMultiRunCaller.originate ~revert:true () in
  let* tez_reverter_2 = TezMultiRunCaller.originate ~revert:true () in
  Log.debug ~prefix "Deploy EVM bridges" ;
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_1 in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_reverter_2 in
  let* evm_bridge_3 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  let* evm_bridge_4 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_leaf in
  Log.debug ~prefix "Deploy EVM reverter with inner catch" ;
  let* evm_reverter =
    EvmMultiRunCaller.deploy_and_init
      ~revert:true
      ~callees:[(evm_bridge_2, true); (evm_bridge_3, false)]
      ()
  in
  Log.debug ~prefix "Deploy EVM main" ;
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:
        [(evm_bridge_1, true); (evm_reverter, true); (evm_bridge_4, false)]
      ()
  in
  Log.debug ~prefix "Call EVM main" ;
  let* _ = EvmRunner.call_run evm_main in
  Log.debug ~prefix "Verify counters" ;
  let* () =
    EvmMultiRunCaller.check_storage
      ~expected_catches:2
      ~expected_counter:4
      evm_main
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_1
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_1
  in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:0 evm_reverter in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_2
  in
  let* () =
    TezMultiRunCaller.check_storage ~expected_counter:0 tez_reverter_2
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:0 evm_bridge_3
  in
  let* () =
    EvmCrossRuntimeRunnerTez.check_storage ~expected_counter:2 evm_bridge_4
  in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_leaf in
  unit

let test_crac_gas_model_alias_caching =
  register_crac_runner_test
    ~title:"CRAC: gas model charges less on alias cache hit"
    ~tags:["gas"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "CRAC-gas" in
  Log.debug ~prefix "Originate TEZ runner" ;
  let* tez_runner = TezMultiRunCaller.originate () in
  Log.debug ~prefix "Deploy EVM bridge to TEZ runner" ;
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  Log.debug ~prefix "Deploy EVM runner calling the bridge" ;
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  Log.debug ~prefix "First call (aliases generated)" ;
  let* gas_first = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "Second call (aliases cached)" ;
  let* gas_second = EvmRunner.call_run evm_runner in
  Log.debug ~prefix "gas_first=%Ld gas_second=%Ld" gas_first gas_second ;
  Check.(
    (gas_first > gas_second)
      int64
      ~error_msg:"First CRAC (%L gas) should cost more than second (%R gas)") ;
  let diff = Int64.sub gas_first gas_second in
  (* On cache miss, each alias incurs the Tezos origination gas
     (~105,137 milligas = ~1,051 EVM gas) on top of the lookup cost.
     On cache hit, only ALIAS_CACHE_HIT_COST (2,100) is paid.
     Two aliases are resolved per CRAC, so the minimum difference
     attributable to alias generation alone is 2 * ~1,051. We use a
     conservative lower bound of 1,000 per alias. *)
  let tezos_alias_generation_evm_gas = 1_000 in
  let expected_min_diff = Int64.of_int (2 * tezos_alias_generation_evm_gas) in
  Check.(
    (diff >= expected_min_diff)
      int64
      ~error_msg:
        "Gas difference (%L) should be >= %R (2x alias generation surcharge)") ;
  unit

(* ── Receipt test helpers ──────────────────────────────────────── *)

(** Fetch Tezlink block manager operations (pass 3) as a JSON list.
    Uses [/operations] (all 4 passes) and extracts index 3 (manager ops).
    [block] is a block identifier (level number or ["head"]). *)
let fetch_michelson_manager_ops ~block sequencer =
  let michelson_base = Evm_node.endpoint sequencer ^ "/tezlink" in
  let path = sf "/chains/main/blocks/%s/operations" block in
  let* res =
    Curl.get_raw ~name:"curl#michelson-ops" (michelson_base ^ path)
    |> Runnable.run
  in
  let all_passes = JSON.parse ~origin:"michelson_operations" res in
  return JSON.(all_passes |=> 3)

(** Return the current Michelson runtime head level as a string. *)
let michelson_head_level sequencer =
  let michelson_base = Evm_node.endpoint sequencer ^ "/tezlink" in
  let* res =
    Curl.get_raw
      ~name:"curl#michelson-head"
      (michelson_base ^ "/chains/main/blocks/head/header")
    |> Runnable.run
  in
  let head = JSON.parse ~origin:"michelson_header" res in
  return JSON.(head |-> "level" |> as_int)

(** Fetch manager operations from the most recent non-empty Tezlink block.
    bake_until_sync may advance the head past the block containing the
    CRAC receipt, so we scan backwards from [head]. *)
let fetch_recent_michelson_manager_ops sequencer =
  let* head = michelson_head_level sequencer in
  let rec find_ops level =
    if level < 1 then return (JSON.parse ~origin:"empty" "[]")
    else
      let* ops =
        fetch_michelson_manager_ops ~block:(string_of_int level) sequencer
      in
      let op_list = JSON.(ops |> as_list) in
      if op_list <> [] then return ops else find_ops (level - 1)
  in
  find_ops head

(** Null implicit address — source of all synthetic Michelson operations
    (both CRAC and non-CRAC). *)
let handler_address = "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

(** Enshrined Michelson gateway contract — called by Michelson contracts
    to initiate outgoing CRACs to EVM. *)
let gateway_address = "KT18oDJJKXMKhfE1bSuAPGp92pYcwVDiqsPw"

(** Fetch the latest EVM block, verify it has [expected_tx_count] transactions,
    and return the block.  Uses [prefix] for log messages. *)
let check_evm_block_tx_count ~prefix ~expected_tx_count sequencer =
  let*@ block = Rpc.get_block_by_number ~block:"latest" sequencer in
  let tx_count =
    match block.transactions with
    | Block.Hash txs -> List.length txs
    | Block.Full txs -> List.length txs
    | Block.Empty -> 0
  in
  Log.info "%s: EVM block %ld has %d tx(s)" prefix block.number tx_count ;
  Check.(
    (tx_count = expected_tx_count)
      int
      ~error_msg:"Expected %R EVM transaction(s), got %L") ;
  return block

(** Compute the expected fake CRAC transaction hash for a given [crac_id]
    and [block_number], then verify it is present in the block's tx hashes.
    The kernel computes: hash = keccak256("CRAC-TX" || block_number_be256 || crac_id). *)
let check_fake_crac_tx_hash ~prefix ~expected_crac_id (block : Block.t) =
  let block_number_be256 =
    let buf = Bytes.make 32 '\000' in
    Bytes.set_int64_be buf 24 (Int64.of_int32 block.number) ;
    buf
  in
  let expected_hash_bytes =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest
      (Bytes.cat
         (Bytes.cat (Bytes.of_string "CRAC-TX") block_number_be256)
         (Bytes.of_string expected_crac_id))
  in
  let expected_hash = "0x" ^ (Hex.of_bytes expected_hash_bytes |> Hex.show) in
  Log.info
    "%s: expected fake tx hash for CRAC-ID %s = %s"
    prefix
    expected_crac_id
    expected_hash ;
  let tx_hashes =
    match block.transactions with
    | Block.Hash hs -> List.map String.lowercase_ascii hs
    | Block.Full txs ->
        List.map
          (fun (tx : Transaction.transaction_object) ->
            String.lowercase_ascii tx.hash)
          txs
    | Block.Empty -> []
  in
  Check.is_true
    (List.mem expected_hash tx_hashes)
    ~error_msg:
      (sf
         "%s: Expected fake CRAC tx hash (CRAC-ID %s) in EVM block tx hashes"
         prefix
         expected_crac_id)

(** Verify that [top] is a valid CRAC top-level content item per the RFC.
    Checks source = handler, destination = [expected_destination],
    status = [expected_status], and all synthetic fields = 0.
    Returns the list of internal_operation_results from metadata. *)
let check_crac_top_level ~prefix ~expected_destination ~expected_status top =
  let kind = JSON.(top |-> "kind" |> as_string) in
  Log.info ~prefix "top-level kind = %s" kind ;
  Check.(
    (kind = "transaction")
      string
      ~error_msg:"Expected top-level kind %R, got %L") ;
  let source = JSON.(top |-> "source" |> as_string) in
  Log.info "%s: top-level source = %s" prefix source ;
  Check.(
    (source = handler_address)
      string
      ~error_msg:"Expected top-level source = Handler_M (%R), got %L") ;
  let destination = JSON.(top |-> "destination" |> as_string) in
  Log.info "%s: top-level destination (alias E_0) = %s" prefix destination ;
  Check.(
    (destination = expected_destination)
      string
      ~error_msg:"Expected top-level destination %R, got %L") ;
  Check.(
    (JSON.(top |-> "amount" |> as_string) = "0")
      string
      ~error_msg:"Expected amount %R, got %L") ;
  Check.(
    (JSON.(top |-> "fee" |> as_string) = "0")
      string
      ~error_msg:"Expected fee %R, got %L") ;
  Check.(
    (JSON.(top |-> "counter" |> as_string) = "0")
      string
      ~error_msg:"Expected counter %R, got %L") ;
  Check.(
    (JSON.(top |-> "gas_limit" |> as_string) = "0")
      string
      ~error_msg:"Expected gas_limit %R, got %L") ;
  Check.(
    (JSON.(top |-> "storage_limit" |> as_string) = "0")
      string
      ~error_msg:"Expected storage_limit %R, got %L") ;
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Log.info "%s: top-level status = %s" prefix top_status ;
  Check.(
    (top_status = expected_status)
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  JSON.(metadata |-> "internal_operation_results" |> as_list)

(** Verify that [iop] is a valid CRAC event with the given [expected_crac_id].
    Checks kind = "event", source = handler, tag = "crac",
    payload = expected_crac_id.  When [expected_status] is provided,
    also checks the event result status (omit for failed CRACs where
    the event may be backtracked). *)
let check_crac_event ~prefix ~expected_crac_id ?expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "event")
      string
      ~error_msg:(sf "%s: Expected event kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = handler_address)
      string
      ~error_msg:
        (sf "%s: Expected event source = handler (%%R), got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "tag" |> as_string) = "crac")
      string
      ~error_msg:(sf "%s: Expected event tag %%R, got %%L" prefix)) ;
  let payload = JSON.(iop |-> "payload" |-> "string" |> as_string) in
  Log.info "%s: CRAC-ID = %s" prefix payload ;
  Check.(
    (payload = expected_crac_id)
      string
      ~error_msg:(sf "%s: Expected CRAC-ID %%R, got %%L" prefix)) ;
  match expected_status with
  | Some s ->
      Check.(
        (JSON.(iop |-> "result" |-> "status" |> as_string) = s)
          string
          ~error_msg:(sf "%s: Expected event status %%R, got %%L" prefix))
  | None -> ()

(** Verify that [iop] is a valid internal transaction with the given fields. *)
let check_crac_internal_transaction ~prefix ~expected_nonce ~expected_source
    ~expected_destination ~expected_entrypoint ~expected_status iop =
  Check.(
    (JSON.(iop |-> "kind" |> as_string) = "transaction")
      string
      ~error_msg:(sf "%s: Expected kind %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "source" |> as_string) = expected_source)
      string
      ~error_msg:(sf "%s: Expected source %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "nonce" |> as_int) = expected_nonce)
      int
      ~error_msg:(sf "%s: Expected nonce %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "destination" |> as_string) = expected_destination)
      string
      ~error_msg:(sf "%s: Expected destination %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "parameters" |-> "entrypoint" |> as_string)
    = expected_entrypoint)
      string
      ~error_msg:(sf "%s: Expected entrypoint %%R, got %%L" prefix)) ;
  Check.(
    (JSON.(iop |-> "result" |-> "status" |> as_string) = expected_status)
      string
      ~error_msg:(sf "%s: Expected status %%R, got %%L" prefix))

(* ── Receipt tests ────────────────────────────────────────────── *)

(* CRAC receipt structure test — single EVM→TEZ crossing.
 *
 *  Per the RFC the Michelson runtime block contains a manager operation with:
 *   - Top-level: handler → alias(E_0), synthetic fields = 0, applied
 *   - Internal #0 (event): gateway emits "crac" tag with CRAC-ID payload
 *   - Internal #1 (transaction): alias(E_1) → tez_runner, applied
 *   - Further internal ops from Michelson contract execution
 *
 *     EVM[evm_runner] |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_runner]
 *)
let test_crac_evm_to_tez_receipt =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ receipt matches RFC structure"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-E2T" in
  let* tez_runner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_runner_kt1)) = tez_runner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_runner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* evm_runner =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run evm_runner in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_runner in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_runner in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Check.(
    (List.length op_list = 1)
      int
      ~error_msg:"Expected 1 manager operation, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 2: 3 internal ops — event, alias→tez_runner, self-call *)
  Check.(
    (List.length internals = 3)
      int
      ~error_msg:"Expected 3 internal operations, got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(E_1) → tez_runner (%run) ───────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_runner → tez_runner (%_incrementWitness) ─ *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_runner_kt1
    ~expected_destination:tez_runner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  unit

(* Two EVM→TEZ CRACs from the SAME EVM transaction (RFC Example 5).
 *
 *  Per the RFC, the Michelson runtime block should have ONE manager operation
 *  with at least TWO internal transactions (one per gateway call, plus
 *  additional ops from Michelson execution) and ONE CRAC event (first).
 *
 *    EVM[evm_main]
 *     |-> EVM[bridge_1] ~CRAC~> TEZ[tez_1]
 *     |-> EVM[bridge_2] ~CRAC~> TEZ[tez_2]
 *)
let test_crac_receipt_two_independent =
  register_crac_runner_test
    ~title:"CRAC: two EVM->TEZ from same tx produce one Michelson op"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-2IND" in
  let* tez_1 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_1_kt1)) = tez_1 in
  let* tez_2 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_2_kt1)) = tez_2 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_2 in
  let (`Evm_runner bridge_2_addr) = bridge_2 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init
      ~callees:[(bridge_1, false); (bridge_2, false)]
      ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_1_addr sequencer
  in
  let*@ bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_2_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_2 in
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 operation, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 5: 5 internal ops — event, then two pairs of
     alias(bridge) → tez_runner (%run) + tez_runner → tez_runner
     (%_incrementWitness), in execution order (tez_1 before tez_2). *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal operations (RFC Example 5), got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(bridge_1) → tez_1 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:bridge_1_alias
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_1 → tez_1 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_1_kt1
    ~expected_destination:tez_1_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: alias(bridge_2) → tez_2 (%run) ──────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:bridge_2_alias
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_2 → tez_2 (%_incrementWitness) ──────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_2_kt1
    ~expected_destination:tez_2_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  unit

(* Two truly independent EVM→TEZ CRACs from SEPARATE EVM transactions
 * in the same block (RFC Example 6).
 *
 *  Each EVM transaction gets a different tx_index → different CRAC-ID.
 *  The Michelson runtime block should have TWO manager operations, each with its
 *  own CRAC event and a different CRAC-ID.
 *
 *   EVM tx 0: EVM[runner_1] → EVM[bridge_1] ~CRAC~> TEZ[tez_1]  (CRAC-ID "1-0")
 *   EVM tx 1: EVM[runner_2] → EVM[bridge_2] ~CRAC~> TEZ[tez_2]  (CRAC-ID "1-1")
 *)
let test_crac_receipt_separate_tx_two_cracs =
  register_crac_runner_test
    ~title:
      "CRAC: two separate EVM txs produce two Michelson ops with different \
       CRAC-IDs"
    ~tags:["crac_receipt"; "crac_id"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-SEP" in
  (* Deploy infrastructure via the wrapper (each deploy produces a block). *)
  let* tez_1 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_1_kt1)) = tez_1 in
  let* tez_2 = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_2_kt1)) = tez_2 in
  let* bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_1 in
  let (`Evm_runner bridge_1_addr) = bridge_1 in
  let* bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_2 in
  let (`Evm_runner bridge_2_addr) = bridge_2 in
  let* runner_1 =
    EvmMultiRunCaller.deploy_and_init ~callees:[(bridge_1, false)] ()
  in
  let (`Evm_runner runner_1_addr) = runner_1 in
  let* runner_2 =
    EvmMultiRunCaller.deploy_and_init ~callees:[(bridge_2, false)] ()
  in
  let (`Evm_runner runner_2_addr) = runner_2 in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_1_addr sequencer
  in
  let*@ bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress bridge_2_addr sequencer
  in
  (* Send both run() transactions to the mempool without producing a
     block, then produce one block so both land with different tx_index. *)
  let* raw_tx_1 =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:runner_1_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ _tx_hash_1 = Rpc.send_raw_transaction ~raw_tx:raw_tx_1 sequencer in
  let* raw_tx_2 =
    Cast.craft_tx
      ~source_private_key:sender.private_key
      ~chain_id:1337
      ~nonce:(evm_nonce ())
      ~gas:300_000
      ~gas_price:1_000_000_000
      ~value:Wei.zero
      ~address:runner_2_addr
      ~signature:"run()"
      ~arguments:[]
      ()
  in
  let*@ _tx_hash_2 = Rpc.send_raw_transaction ~raw_tx:raw_tx_2 sequencer in
  let*@ _block_number = Rpc.produce_block sequencer in
  let* () =
    Test_helpers.bake_until_sync ~sc_rollup_node ~sequencer ~client ()
  in
  (* Verify both Tez contracts were called *)
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_1 in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_2 in
  (* Fetch Michelson runtime manager operations *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info "%s: %d manager operation(s)" prefix (List.length op_list) ;
  Check.(
    (List.length op_list = 2) int ~error_msg:"Expected 2 operations, got %L") ;
  (* Verify each operation has correct CRAC structure:
     event + bridge_alias → tez (%run) + tez → tez (%_incrementWitness) *)
  let check_op ~prefix ~expected_destination ~expected_bridge_alias
      ~expected_dest ~expected_crac_id ~nonce_offset op =
    let top = JSON.(op |-> "contents" |=> 0) in
    let internals =
      check_crac_top_level
        ~prefix
        ~expected_destination
        ~expected_status:"applied"
        top
    in
    Check.(
      (List.length internals = 3)
        int
        ~error_msg:(sf "%s: expected 3 internal operations, got %%L" prefix)) ;
    check_crac_event
      ~prefix
      ~expected_crac_id
      ~expected_status:"applied"
      (List.nth internals 0) ;
    (* Internal operation nonces are block-global (L1 semantics):
       event gets nonce_offset, first tx gets nonce_offset+1, etc. *)
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 1)
      ~expected_source:expected_bridge_alias
      ~expected_destination:expected_dest
      ~expected_entrypoint:"run"
      ~expected_status:"applied"
      (List.nth internals 1) ;
    check_crac_internal_transaction
      ~prefix
      ~expected_nonce:(nonce_offset + 2)
      ~expected_source:expected_dest
      ~expected_destination:expected_dest
      ~expected_entrypoint:"_incrementWitness"
      ~expected_status:"applied"
      (List.nth internals 2)
  in
  (* First operation: nonces 0,1,2 (event=0, run=1, _incrementWitness=2) *)
  check_op
    ~prefix:(prefix ^ "#0")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_1_alias
    ~expected_dest:tez_1_kt1
    ~expected_crac_id:"1-0"
    ~nonce_offset:0
    JSON.(ops |=> 0) ;
  (* Second operation: nonces continue at 3,4,5 (block-global counter) *)
  check_op
    ~prefix:(prefix ^ "#1")
    ~expected_destination:sender_alias
    ~expected_bridge_alias:bridge_2_alias
    ~expected_dest:tez_2_kt1
    ~expected_crac_id:"1-1"
    ~nonce_offset:3
    JSON.(ops |=> 1) ;
  unit

(* EVM→TEZ→EVM double crossing — verify receipts on BOTH sides.
 *
 *  The EVM transaction triggers a CRAC into Michelson, which then CRACs
 *  back into EVM.  We verify:
 *   - Tezlink block: one manager op with handler source, event, internals
 *   - EVM block: at least one additional fake transaction (from TEZ→EVM)
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge] ~CRAC~> TEZ[tez_bridge] ~CRAC~> EVM[evm_inner]
 *)
let test_crac_receipt_evm_tez_evm =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ->EVM double crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-ETE" in
  let* evm_inner = EvmMultiRunCaller.deploy_and_init () in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge, false)] ()
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_main in
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:1 evm_inner in
  let* () =
    TezCrossRuntimeRunnerEvm.check_storage ~expected_counter:2 tez_bridge
  in
  (* ── Michelson runtime side: EVM→TEZ leg ─────────────────────────────── *)
  Log.debug ~prefix "Verify Michelson runtime receipt" ;
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected = 1 Michelson op, got %L") ;
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* RFC Example 3: 5 internal ops — event, alias(evm_bridge) → tez_bridge (%run),
     tez_bridge → tez_bridge (%_incrementWitness) (pre),
     tez_bridge → GW_M (%call_evm), tez_bridge → tez_bridge (%_incrementWitness) (post). *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 5)
      int
      ~error_msg:"Expected 5 internal operations (RFC Example 3), got %L") ;
  (* ── Internal #0: CRAC event ────────────────────────────────── *)
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  (* ── Internal #1: alias(evm_bridge) → tez_bridge (%run) ────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:1
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  (* ── Internal #2: tez_bridge → tez_bridge (%_incrementWitness) (pre) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:2
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  (* ── Internal #3: tez_bridge → GW_M (%call_evm) ─────────────── *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:3
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  (* ── Internal #4: tez_bridge → tez_bridge (%_incrementWitness) (post) *)
  check_crac_internal_transaction
    ~prefix
    ~expected_nonce:4
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  (* ── EVM side: re-entrant TEZ→EVM leg ─────────────────────── *)
  (* Per RFC principle 6, the re-entrant EVM execution appears as
     internal transactions under the ORIGINAL EVM transaction, not as
     a separate top-level fake CRAC tx. *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

(* TEZ→EVM→TEZ double crossing — verify receipts on BOTH sides.
 * This covers RFC Example 7 (Michelson → EVM → Michelson multi-hop).
 *
 *    TEZ[tez_main]
 *     |-> TEZ[tez_outer_bridge] ~CRAC~> EVM[evm_bridge] ~CRAC~> TEZ[tez_inner]
 *
 *  We verify:
 *   - EVM block: fake CRAC transaction whose hash matches CRAC-ID "0-0"
 *   - Tezlink block: manager operation from the second EVM→TEZ leg
 *)
let test_crac_receipt_tez_evm_tez =
  register_crac_runner_test
    ~title:"CRAC: TEZ->EVM->TEZ double crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-TET" in
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_addr) = evm_bridge in
  let* tez_outer_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge in
  let (`Tez_runner (_, tez_outer_bridge_kt1)) = tez_outer_bridge in
  let* tez_main = TezMultiRunCaller.originate ~callees:[tez_outer_bridge] () in
  let (`Tez_runner (_, tez_main_kt1)) = tez_main in
  let*@ evm_bridge_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_addr sequencer
  in
  let* () = TezRunner.call_run tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:2 tez_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  (* ── EVM side: TEZ→EVM leg ─────────────────────────────────── *)
  let* block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  check_fake_crac_tx_hash ~prefix ~expected_crac_id:"0-0" block ;
  (* ── Michelson runtime side: EVM→TEZ return leg ────────────────────── *)
  (* Origin is TEZ (runtime_id=0), so re-entering TEZ should NOT
     produce a CRAC event — the event was already emitted on the
     first crossing out of the origin runtime. *)
  (* ── Michelson runtime side: TEZ→EVM→TEZ ────────────────────────────── *)
  (* Per RFC principle 6 (one top-level per runtime per CRAC-ID),
     the re-entrant TEZ execution (EVM→TEZ return leg) appears as
     internal operations within the ORIGINAL TEZ transaction, not
     as a separate manager operation.  Verify the Michelson runtime block has
     1 manager operation with the return leg visible as internal ops. *)
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  (* The original TEZ transaction is an outgoing CRAC (TEZ→EVM), so the
     top-level source is the injected sender, not the handler.  Re-entrant
     EVM→TEZ effects are merged as internal ops per RFC principle 6.
     No CRAC event is emitted (origin runtime = TEZ). *)
  let top = JSON.(ops |=> 0 |-> "contents" |=> 0) in
  let metadata = JSON.(top |-> "metadata") in
  let top_status =
    JSON.(metadata |-> "operation_result" |-> "status" |> as_string)
  in
  Check.(
    (top_status = "applied")
      string
      ~error_msg:"Expected top-level status %R, got %L") ;
  let internals = JSON.(metadata |-> "internal_operation_results" |> as_list) in
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  (* Expected internal ops (all transactions, no CRAC event):
     #0: tez_main → tez_main (%_incrementWitness)
     #1: tez_main → tez_outer_bridge (%run)
     #2: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (pre)
     #3: tez_outer_bridge → GW_M (%call_evm)
     #4: alias(evm_bridge) → tez_inner (%run)          (re-entrant)
     #5: tez_inner → tez_inner (%_incrementWitness)     (re-entrant)
     #6: tez_outer_bridge → tez_outer_bridge (%_incrementWitness) (post)
     #7: tez_main → tez_main (%_incrementWitness) (final) *)
  Check.(
    (List.length internals = 8)
      int
      ~error_msg:"Expected 8 internal operations, got %L") ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#0")
    ~expected_nonce:0
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_bridge_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_outer_bridge_kt1
    ~expected_destination:tez_outer_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#7")
    ~expected_nonce:7
    ~expected_source:tez_main_kt1
    ~expected_destination:tez_main_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 7) ;
  unit

(* EVM→TEZ→EVM→TEZ triple crossing — nested CRAC that re-enters TEZ.
 * Mirrors test_crac_receipt_tez_evm_tez but starts from EVM, exercising
 * a nested CRAC within the same runtime (TEZ appears twice).
 *
 *    EVM[evm_main]
 *     |-> EVM[evm_bridge_1] ~CRAC~> TEZ[tez_bridge] ~CRAC~>
 *         EVM[evm_bridge_2] ~CRAC~> TEZ[tez_inner]
 *
 *  We verify:
 *   - Michelson runtime block: 1 manager op with 1 content item,
 *     handler source, KT1 alias destination, synthetic fields = 0,
 *     internal transaction(s) all "applied", CRAC event with
 *     tag "crac" and CRAC-ID "1-0", event status "applied"
 *   - EVM block: 1 transaction (original tx; re-entrant TEZ→EVM leg
 *     is internal per RFC principle 6)
 *)
let test_crac_receipt_evm_tez_evm_tez =
  register_crac_runner_test
    ~title:"CRAC: EVM->TEZ->EVM->TEZ triple crossing receipts"
    ~tags:["crac_receipt"]
  @@ fun (module Wrapper) ->
  let open Wrapper in
  let prefix = "RCPT-ETET" in
  (* Build chain: evm_main -> evm_bridge_1 ~> tez_bridge ~> evm_bridge_2 ~> tez_inner *)
  let* tez_inner = TezMultiRunCaller.originate () in
  let (`Tez_runner (_, tez_inner_kt1)) = tez_inner in
  let* evm_bridge_2 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_inner in
  let (`Evm_runner evm_bridge_2_addr) = evm_bridge_2 in
  let* tez_bridge = TezCrossRuntimeRunnerEvm.originate evm_bridge_2 in
  let (`Tez_runner (_, tez_bridge_kt1)) = tez_bridge in
  let* evm_bridge_1 = EvmCrossRuntimeRunnerTez.deploy_and_init tez_bridge in
  let (`Evm_runner evm_bridge_1_addr) = evm_bridge_1 in
  let* evm_main =
    EvmMultiRunCaller.deploy_and_init ~callees:[(evm_bridge_1, false)] ()
  in
  let*@ evm_bridge_1_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_1_addr sequencer
  in
  let*@ evm_bridge_2_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress evm_bridge_2_addr sequencer
  in
  let*@ sender_alias =
    Rpc.Tezosx.tez_getEthereumTezosAddress sender.address sequencer
  in
  let* _ = EvmRunner.call_run evm_main in
  (* Verify execution reached both ends *)
  let* () = EvmMultiRunCaller.check_storage ~expected_counter:2 evm_main in
  let* () = TezMultiRunCaller.check_storage ~expected_counter:1 tez_inner in
  (* ── Michelson runtime side ──────────────────────────────────── *)
  Log.debug ~prefix "Verify Michelson runtime receipt" ;
  let* ops = fetch_recent_michelson_manager_ops sequencer in
  let op_list = JSON.(ops |> as_list) in
  Log.info
    "%s: Michelson runtime has %d manager op(s)"
    prefix
    (List.length op_list) ;
  Check.(
    (List.length op_list = 1) int ~error_msg:"Expected 1 Michelson op, got %L") ;
  let first_op = JSON.(ops |=> 0) in
  let contents = JSON.(first_op |-> "contents" |> as_list) in
  Check.((List.length contents = 1) int ~error_msg:"Expected 1 content, got %L") ;
  let top = JSON.(first_op |-> "contents" |=> 0) in
  let internals =
    check_crac_top_level
      ~prefix
      ~expected_destination:sender_alias
      ~expected_status:"applied"
      top
  in
  (* EVM→TEZ→EVM→TEZ: 7 internal ops — event + 6 transactions.
     #0: event (CRAC-ID "1-0")
     #1: alias(evm_bridge_1) → tez_bridge (%run)
     #2: tez_bridge → tez_bridge (%_incrementWitness) (pre)
     #3: tez_bridge → GW_M (%call_evm)
     #4: alias(evm_bridge_2) → tez_inner (%run)          (re-entrant)
     #5: tez_inner → tez_inner (%_incrementWitness)       (re-entrant)
     #6: tez_bridge → tez_bridge (%_incrementWitness) (post) *)
  Log.info "%s: %d internal op(s)" prefix (List.length internals) ;
  Check.(
    (List.length internals = 7)
      int
      ~error_msg:"Expected 7 internal operations, got %L") ;
  check_crac_event
    ~prefix
    ~expected_crac_id:"1-0"
    ~expected_status:"applied"
    (List.nth internals 0) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#1")
    ~expected_nonce:1
    ~expected_source:evm_bridge_1_alias
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 1) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#2")
    ~expected_nonce:2
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 2) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#3")
    ~expected_nonce:3
    ~expected_source:tez_bridge_kt1
    ~expected_destination:gateway_address
    ~expected_entrypoint:"call_evm"
    ~expected_status:"applied"
    (List.nth internals 3) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#4")
    ~expected_nonce:4
    ~expected_source:evm_bridge_2_alias
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"run"
    ~expected_status:"applied"
    (List.nth internals 4) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#5")
    ~expected_nonce:5
    ~expected_source:tez_inner_kt1
    ~expected_destination:tez_inner_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 5) ;
  check_crac_internal_transaction
    ~prefix:(prefix ^ "#6")
    ~expected_nonce:6
    ~expected_source:tez_bridge_kt1
    ~expected_destination:tez_bridge_kt1
    ~expected_entrypoint:"_incrementWitness"
    ~expected_status:"applied"
    (List.nth internals 6) ;
  (* ── EVM side ──────────────────────────────────────────────── *)
  (* 1 original EVM tx; re-entrant TEZ→EVM leg is internal per RFC principle 6 *)
  let* _block =
    check_evm_block_tx_count ~prefix ~expected_tx_count:1 sequencer
  in
  unit

let () =
  test_crac_evm_to_tez [Alpha] ;
  test_crac_evm_multiple_independent_crossings [Alpha] ;
  test_crac_evm_double_crossing [Alpha] ;
  test_crac_evm_shared_leaf_via_direct_and_chain [Alpha] ;
  test_crac_evm_5_crossing_chain [Alpha] ;
  test_crac_tez_to_evm [Alpha] ;
  test_crac_tez_multiple_independent_crossings [Alpha] ;
  test_crac_tez_double_crossing [Alpha] ;
  test_crac_tez_shared_leaf_via_direct_and_chain [Alpha] ;
  test_crac_tez_5_crossing_chain [Alpha] ;
  test_crac_access_list_preserved [Alpha] ;
  test_crac_evm_to_tez_reverts [Alpha] ;
  test_crac_tez_to_evm_reverts [Alpha] ;
  test_crac_tez_to_evm_fake_tx_in_block [Alpha] ;
  test_crac_tez_to_evm_fake_tx_unique_hash_across_blocks [Alpha] ;
  test_crac_tez_revert_rolls_back_inner_evm_storage [Alpha] ;
  test_crac_tez_revert_propagates_to_evm [Alpha] ;
  test_crac_evm_journal_state_preserved [Alpha] ;
  test_crac_catch_tez_revert [Alpha] ;
  test_crac_evm_revert_propagates_to_tez [Alpha] ;
  test_crac_second_crac_tez_revert [Alpha] ;
  test_crac_evm_revert_rolls_back_two_cracs [Alpha] ;
  test_crac_deep_branch_with_second_tez_revert [Alpha] ;
  test_crac_nested_revert_cascade_without_catch [Alpha] ;
  test_crac_double_nested_evm_revert [Alpha] ;
  test_crac_deep_nesting_6_levels [Alpha] ;
  test_crac_evm_revert_after_nested_cracs [Alpha] ;
  test_crac_evm_target_reverts [Alpha] ;
  test_crac_second_crac_evm_revert [Alpha] ;
  test_crac_tez_revert_rolls_back_two_cracs [Alpha] ;
  test_crac_deep_branch_with_second_evm_revert [Alpha] ;
  test_crac_tez_nested_revert_cascade_without_catch [Alpha] ;
  test_crac_double_nested_tez_revert [Alpha] ;
  test_crac_tez_deep_nesting_6_levels [Alpha] ;
  test_crac_tez_revert_after_nested_cracs [Alpha] ;
  test_crac_catch_evm_revert_between_cracs [Alpha] ;
  test_crac_catch_tez_revert_between_cracs [Alpha] ;
  test_crac_catch_tez_revert_with_nested_crac [Alpha] ;
  test_crac_catch_deep_evm_revert_through_double_crac [Alpha] ;
  test_crac_tez_catch_evm_revert_between_cracs [Alpha] ;
  test_crac_tez_catch_tez_revert_between_cracs [Alpha] ;
  test_crac_tez_catch_deep_revert_through_double_crac [Alpha] ;
  test_crac_catch_revert_after_multiple_cracs [Alpha] ;
  test_crac_catch_tez_revert_after_multiple_return_cracs [Alpha] ;
  test_crac_catch_4_crossing_chain_revert [Alpha] ;
  test_crac_tez_catch_4_crossing_chain_revert [Alpha] ;
  test_crac_catch_5_crossing_chain_revert [Alpha] ;
  test_crac_tez_catch_5_crossing_chain_revert [Alpha] ;
  test_crac_chained_tez_calls_behind_crac [Alpha] ;
  test_crac_nested_catches_with_multiple_reverts [Alpha] ;
  test_crac_gas_model_alias_caching [Alpha] ;
  test_crac_evm_to_tez_receipt [Alpha] ;
  test_crac_receipt_two_independent [Alpha] ;
  test_crac_receipt_separate_tx_two_cracs [Alpha] ;
  test_crac_receipt_evm_tez_evm [Alpha] ;
  test_crac_receipt_tez_evm_tez [Alpha] ;
  test_crac_receipt_evm_tez_evm_tez [Alpha]
