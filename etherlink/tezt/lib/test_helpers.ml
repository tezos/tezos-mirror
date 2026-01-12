(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Sc_rollup_helpers

let u16_to_bytes n =
  let bytes = Bytes.make 2 'a' in
  Bytes.set_uint16_le bytes 0 n ;
  Bytes.to_string bytes

let leftPad32 s =
  let s = Durable_storage_path.no_0x s in
  let len = String.length s in
  String.make (64 - len) '0' ^ s

let add_0x s = "0x" ^ s

let remove_0x s =
  if String.starts_with ~prefix:"0x" s then String.sub s 2 (String.length s - 2)
  else s

let mapping_position index map_position =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest
    (Hex.to_bytes
       (`Hex (leftPad32 index ^ leftPad32 (string_of_int map_position))))
  |> Hex.of_bytes |> Hex.show |> add_0x

let hex_string_to_int x = `Hex x |> Hex.to_string |> Z.of_bits |> Z.to_int

let hex_256_of_int n = Printf.sprintf "%064x" n

let hex_256_of_address acc =
  let s = acc.Eth_account.address in
  (* strip 0x and convert to lowercase *)
  let n = String.length s in
  let s = String.lowercase_ascii @@ String.sub s 2 (n - 2) in
  (* prepend 24 leading zeros *)
  String.("0x" ^ make 24 '0' ^ s)

let genesis_time = Client.Time.of_notation_exn "2020-01-01T00:00:00Z"

let genesis_timestamp = Client.At genesis_time

let days n = Ptime.Span.of_int_s (n * 86400)

let get_timestamp i =
  Ptime.add_span genesis_time (days (i + 1))
  |> Option.get |> Client.Time.to_notation

let produce_block ?timestamp evm_node =
  match Evm_node.mode evm_node with
  | Sandbox _ | Tezlink_sandbox _ | Sequencer _ ->
      Rpc.produce_block ?timestamp evm_node
  | _ -> assert false

let check_chain_id ~expected_chain_id ~chain_id =
  let expected_chain_id =
    let bytes = Bytes.create 4 in
    Bytes.set_int32_be bytes 0 @@ Int32.of_int expected_chain_id ;
    bytes |> Tezos_crypto.Hashed.Chain_id.of_bytes_exn
    |> Tezos_crypto.Hashed.Chain_id.to_b58check
  in
  Check.(
    (chain_id = expected_chain_id)
      string
      ~error_msg:"Expected %R as chain_id but got %L")

let check_header ~previous_header ~current_header ~chain_id ~current_timestamp =
  Check.(
    ((JSON.(current_header |-> "level" |> as_int)
     - JSON.(previous_header |-> "level" |> as_int))
    = 1)
      int)
    ~error_msg:
      "The difference in level between consecutive blocks should be %R but got \
       %L" ;

  Check.(
    (JSON.(previous_header |-> "hash") = JSON.(current_header |-> "predecessor"))
      json)
    ~error_msg:"Expected predecessor hash of current header to be %L but got %R" ;

  (match chain_id with
  | Some chain_id ->
      Check.(
        (JSON.(previous_header |-> "chain_id")
        = JSON.(current_header |-> "chain_id"))
          json)
        ~error_msg:
          "Expected blocks to be in the same chain, but the current block is \
           in chain %R, while the previous block is in chain %L" ;

      check_chain_id
        ~expected_chain_id:chain_id
        ~chain_id:JSON.(current_header |-> "chain_id" |> as_string)
  | _ -> ()) ;

  match current_timestamp with
  | Some current_timestamp ->
      Check.(
        (JSON.(current_header |-> "timestamp" |> as_string) = current_timestamp)
          string)
        ~error_msg:
          "Expected the timestamp of the current_block to be %R, but got %L"
  | _ -> ()

(* Block info contains a raw header that differs slightly from the header returned by
   the header RPC.
   We need to append the hash and chain_id fields to our raw_header so that the function
   check_header doesn't fail. *)
let block_info_to_header block_info =
  let raw_header = JSON.(block_info |-> "header") in
  let hash_raw_header = JSON.(put ("hash", block_info |-> "hash") raw_header) in
  let header =
    JSON.(put ("chain_id", block_info |-> "chain_id") hash_raw_header)
  in
  header

let check_block_info ~previous_block_info ~current_block_info ~chain_id
    ~current_timestamp ~expected_operations =
  let () =
    check_header
      ~previous_header:(block_info_to_header previous_block_info)
      ~current_header:(block_info_to_header current_block_info)
      ~chain_id
      ~current_timestamp
  in
  (* For now operations are converted to dummy string.
     We convert to string operations to use the Check module *)
  let operations =
    JSON.(
      current_block_info |-> "operations" |> as_list
      |> List.map (fun operations ->
             let ops = as_list operations in
             List.map as_string ops))
  in
  Check.((operations = expected_operations) (list (list string)))
    ~error_msg:"List of operations is expected to be empty for now"

let next_evm_level ~evm_node ~sc_rollup_node ~client =
  match Evm_node.mode evm_node with
  | Proxy ->
      let* _l1_level = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
      unit
  | Sequencer _ | Sandbox _ | Tezlink_sandbox _ ->
      let open Rpc.Syntax in
      let*@ _l2_level = produce_block evm_node in
      unit
  | Observer _ -> Test.fail "Cannot create a new level with an Observer node"
  | Rpc _ -> Test.fail "Cannot create a new level with a Rpc node"

let kernel_inputs_path = "etherlink/tezt/tests/evm_kernel_inputs"

let read_tx_from_file () =
  read_file (kernel_inputs_path ^ "/100-inputs-for-proxy")
  |> String.trim |> String.split_on_char '\n'
  |> List.map (fun line ->
         match String.split_on_char ' ' line with
         | [tx_raw; tx_hash] -> (tx_raw, tx_hash)
         | _ -> failwith "Unexpected tx_raw and tx_hash.")

let force_kernel_upgrade ~sc_rollup_address ~sc_rollup_node ~client =
  let force_kernel_upgrade_payload =
    (* Framed protocol tag. *)
    "\000"
    (* Smart rollup address bytes. *)
    ^ Tezos_crypto.Hashed.Smart_rollup_address.(
        of_b58check_exn sc_rollup_address |> to_string)
    ^
    (* Force kernel upgrade tag.
         See [FORCE_KERNEL_UPGRADE_TAG] in [etherlink/kernel_latest/kernel/src/parsing.rs] *)
    "\255"
    |> Hex.of_string |> Hex.show
  in
  let* () =
    Sc_rollup_helpers.send_message
      client
      (sf "hex:[%S]" force_kernel_upgrade_payload)
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  unit

let upgrade ~sc_rollup_node ~sc_rollup_address ~admin ~admin_contract ~client
    ~upgrade_to ~activation_timestamp =
  let preimages_dir =
    Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0"
  in
  let* {root_hash; _} =
    Sc_rollup_helpers.prepare_installer_kernel ~preimages_dir upgrade_to
  in
  let* payload = Evm_node.upgrade_payload ~root_hash ~activation_timestamp in
  (* Sends the upgrade to L1. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:admin
      ~receiver:admin_contract
      ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
      ~burn_cap:Tez.one
      client
  in
  let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
  return root_hash

let check_block_consistency ~left ~right ?error_msg ~block () =
  let open Rpc.Syntax in
  let block =
    match block with
    | `Latest -> "latest"
    | `Level l -> Int32.to_string l
    | `Finalized -> "finalized"
  in
  let error_msg =
    Option.value
      ~default:
        Format.(
          sprintf
            "Nodes do not have the same head (%s is %%L while %s is %%R)"
            (Evm_node.name left)
            (Evm_node.name right))
      error_msg
  in
  let*@ left_head = Rpc.get_block_by_number ~block left in
  let*@ right_head = Rpc.get_block_by_number ~block right in
  Check.((left_head.number = right_head.number) int32) ~error_msg ;
  Check.((left_head.hash = right_head.hash) string) ~error_msg ;
  unit

let check_head_consistency ~left ~right ?error_msg () =
  check_block_consistency ~left ~right ?error_msg ~block:`Latest ()

type network = Etherlink | Tezlink

let rollup_level_opt ?(network = Etherlink) sc_rollup_node =
  let key =
    match network with
    | Etherlink -> "/evm/world_state/blocks/current/number"
    | Tezlink -> "/tezlink/blocks/current/number"
  in
  let* block_number =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_durable_state_value
         ~pvm_kind:"wasm_2_0_0"
         ~operation:Sc_rollup_rpc.Value
         ~key
         ()
  in
  match block_number with
  | None -> return None
  | Some bytes ->
      return
        (Some
           (Hex.to_bytes (`Hex bytes)
           |> Bytes.to_string |> Z.of_bits |> Z.to_int32))

let rollup_level ?(network = Etherlink) sc_rollup_node =
  let* level_opt = rollup_level_opt ~network sc_rollup_node in
  match level_opt with
  | None ->
      return (Error Rpc.{code = 404; message = "Level not found"; data = None})
  | Some level -> return (Ok level)

let check_rollup_head_consistency ~evm_node ~sc_rollup_node ?error_msg () =
  let open Rpc.Syntax in
  let*@ evm_node_head = Rpc.get_block_by_number ~block:"latest" evm_node in
  let*@ sc_rollup_node_block_number = rollup_level sc_rollup_node in

  Check.((evm_node_head.number = sc_rollup_node_block_number) int32)
    ~error_msg:
      (Option.value
         ~default:
           Format.(
             sprintf
               "EVM node head number (%ld) is different from the rollup node \
                head number (%ld)"
               evm_node_head.number
               sc_rollup_node_block_number)
         error_msg) ;

  unit

let sequencer_upgrade ~sc_rollup_address ~sequencer_admin
    ~sequencer_governance_contract ~client ~upgrade_to ~pool_address
    ~activation_timestamp =
  let* payload =
    Evm_node.sequencer_upgrade_payload
      ~client
      ~public_key:upgrade_to
      ~pool_address
      ~activation_timestamp
      ()
  in
  (* Sends the upgrade to L1. *)
  let* () =
    Client.transfer
      ~amount:Tez.zero
      ~giver:sequencer_admin
      ~receiver:sequencer_governance_contract
      ~arg:(sf {|Pair "%s" 0x%s|} sc_rollup_address payload)
      ~burn_cap:Tez.one
      client
  in
  Client.bake_for_and_wait ~keys:[] client

let bake_until ?__LOC__ ?(timeout_in_blocks = 20) ?(timeout = 30.) ~bake
    ~result_f () =
  let res = ref None in
  let rec go counter_block =
    let* opt = result_f () in
    match opt with
    | Some x ->
        res := Some x ;
        unit
    | None ->
        if counter_block > timeout_in_blocks then
          Test.fail
            ?__LOC__
            "Bake until has baked more than %d blocks but the condition is \
             still false."
            timeout_in_blocks
        else
          let* _ = bake () in
          go (counter_block + 1)
  in
  let* () = Lwt.pick [go 0; Lwt_unix.sleep timeout] in
  match !res with
  | Some x -> return x
  | None -> Test.fail ?__LOC__ "Bake until failed with a timeout"

let bake_until_sync ?__LOC__ ?timeout_in_blocks ?timeout ?(network = Etherlink)
    ~sc_rollup_node ~sequencer ~client () =
  let bake () =
    let* _l1_lvl = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
    unit
  in
  let result_f () =
    let open Rpc.Syntax in
    let* sc_rollup_node_block_number_opt =
      rollup_level_opt ~network sc_rollup_node
    in
    let*@ sequencer_level = Rpc.generic_block_number sequencer in
    match sc_rollup_node_block_number_opt with
    | None -> Lwt.return_none
    | Some proxy_level ->
        if sequencer_level < proxy_level then
          Test.fail
            ~loc:__LOC__
            "rollup node has more recent block. Not supposed to happened "
        else if sequencer_level = proxy_level then return @@ Some ()
        else Lwt.return_none
  in
  bake_until ?__LOC__ ?timeout_in_blocks ?timeout ~bake ~result_f ()

(** [wait_for_transaction_receipt ~evm_node ~transaction_hash] takes an
    transaction_hash and returns only when the receipt is non null, or [count]
    blocks have passed and the receipt is still not available. *)
let wait_for_transaction_receipt ?websocket ?(count = 3) ~evm_node
    ~transaction_hash () =
  let rec loop count =
    let* () = Lwt_unix.sleep 5. in
    let* receipt =
      Evm_node.(
        jsonrpc
          ?websocket
          evm_node
          {
            method_ = "eth_getTransactionReceipt";
            parameters = `A [`String transaction_hash];
          })
    in
    if receipt |> Evm_node.extract_result |> JSON.is_null then
      if count > 0 then loop (count - 1)
      else Test.fail "Transaction still hasn't been included"
    else
      receipt |> Evm_node.extract_result
      |> Transaction.transaction_receipt_of_json |> return
  in
  loop count

let wait_for_application ?(time_between_blocks = 5.) ?(max_blocks = 10)
    ~produce_block apply =
  let open Rpc.Syntax in
  let max_iteration = max_blocks in
  let application_result = apply () in
  let rec loop current_iteration =
    let* () = Lwt_unix.sleep time_between_blocks in
    let*@ _ = produce_block () in
    if max_iteration < current_iteration then
      Test.fail
        "Baked more than %d blocks and the operation's application is still \
         pending"
        max_iteration ;
    match Lwt.state application_result with
    | Lwt.Return value -> Lwt.return value
    | Lwt.Fail exn -> raise exn
    | Lwt.Sleep -> loop (current_iteration + 1)
  in
  Lwt.pick [application_result; loop 0]

let wait_for_event ?(timeout = 30.) ?(levels = 10) event_watcher ~sequencer
    ~sc_rollup_node ~client ~error_msg =
  let open Rpc.Syntax in
  let event_value = ref None in
  let _ =
    let* return_value = event_watcher in
    event_value := Some return_value ;
    unit
  in
  let rec rollup_node_loop n =
    if n = 0 then Test.fail error_msg
    else
      let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
      let*@ _ = produce_block sequencer in
      if Option.is_some !event_value then unit else rollup_node_loop (n - 1)
  in
  let* () = Lwt.pick [rollup_node_loop levels; Lwt_unix.sleep timeout] in
  match !event_value with
  | Some value -> return value
  | None -> Test.fail ~loc:__LOC__ "Waiting for event failed"

let batch_n_transactions ?websocket ~evm_node txs =
  let requests =
    List.map
      (fun tx ->
        Evm_node.
          {method_ = "eth_sendRawTransaction"; parameters = `A [`String tx]})
      txs
  in
  let* hashes = Evm_node.batch_jsonrpc ?websocket evm_node requests in
  let hashes =
    hashes
    |> List.map (fun json -> Evm_node.extract_result json |> JSON.as_string)
  in
  return (requests, hashes)

(* sending more than ~300 tx could fail, because or curl *)
let send_n_transactions ?websocket ~produce_block ~evm_node ?wait_for_blocks txs
    =
  let* requests, hashes = batch_n_transactions ?websocket ~evm_node txs in
  let first_hash = List.hd hashes in
  (* Let's wait until one of the transactions is injected into a block, and
      test this block contains the `n` transactions as expected. *)
  let* receipt =
    wait_for_application
      ~produce_block
      (wait_for_transaction_receipt
         ?websocket
         ?count:wait_for_blocks
         ~evm_node
         ~transaction_hash:first_hash)
  in
  return (requests, receipt, hashes)

let default_bootstrap_account_balance = Wei.of_eth_int 9999

let l1_timestamp client =
  let* l1_header = Client.RPC.call client @@ RPC.get_chain_block_header () in
  return
    JSON.(
      l1_header |-> "timestamp" |> as_string
      |> Tezos_base.Time.Protocol.of_notation_exn)

let find_and_execute_withdrawal ?(outbox_lookup_depth = 10) ~withdrawal_level
    ~commitment_period ~challenge_window ~evm_node ~sc_rollup_node
    ~sc_rollup_address ~client () =
  (* Bake enough levels to have a commitment and cement it. *)
  let* _ =
    repeat
      ((commitment_period * challenge_window) + 3)
      (fun () ->
        let* _ = Rollup.next_rollup_node_level ~sc_rollup_node ~client in
        unit)
  in

  (* Construct and execute the outbox proof. *)
  let find_outbox level =
    let rec aux level' =
      if level' > level + outbox_lookup_depth then
        Test.fail
          "Looked for an outbox for %d levels, stopping the loop"
          outbox_lookup_depth
      else
        let* outbox =
          Sc_rollup_node.RPC.call sc_rollup_node
          @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level:level' ()
        in
        if
          JSON.is_null outbox
          || (JSON.is_list outbox && JSON.as_list outbox = [])
        then aux (level' + 1)
        else return (JSON.as_list outbox |> List.length, level')
    in
    aux level
  in
  let* size, withdrawal_level = find_outbox withdrawal_level in
  let execute_withdrawal withdrawal_level message_index =
    let* outbox_proof =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.outbox_proof_simple
           ~message_index
           ~outbox_level:withdrawal_level
           ()
    in
    let Sc_rollup_rpc.{proof; commitment_hash} =
      match outbox_proof with
      | Some r -> r
      | None -> Test.fail "No outbox proof found for the withdrawal"
    in
    let*! () =
      Client.Sc_rollup.execute_outbox_message
        ~hooks
        ~burn_cap:(Tez.of_int 10)
        ~rollup:sc_rollup_address
          (* Execute with bootstrap5 as tests in general use bootstrap1 for the
             rollup node operators. We might break the 1M rule if we use
             the same key. *)
        ~src:Constant.bootstrap5.alias
        ~commitment_hash
        ~proof
        client
    in
    let* _ = next_evm_level ~evm_node ~sc_rollup_node ~client in
    unit
  in
  let* () =
    Lwt_list.iter_s
      (fun message_index -> execute_withdrawal withdrawal_level message_index)
      (List.init size Fun.id)
  in
  return withdrawal_level

let init_sequencer_sandbox ?maximum_gas_per_transaction ?genesis_timestamp
    ?tx_queue_max_lifespan ?tx_queue_max_size ?tx_queue_tx_per_addr_limit
    ?set_account_code ?da_fee_per_byte ?minimum_base_fee_per_gas ?history_mode
    ?patch_config ?websockets ?(kernel = Constant.WASM.evm_kernel) ?evm_version
    ?(eth_bootstrap_accounts =
      List.map
        (fun account -> account.Eth_account.address)
        (Array.to_list Eth_account.bootstrap_accounts))
    ?(tez_bootstrap_accounts = Evm_node.tez_default_bootstrap_accounts)
    ?(sequencer_keys = []) ?with_runtimes () =
  let patch_config =
    Option.map
      (fun input_patch json ->
        json
        |> Evm_node.patch_config_with_experimental_feature
             ~preconfirmation_stream_enabled:true
             ()
        |> input_patch)
      patch_config
  in
  let wallet_dir = Temp.dir "wallet" in
  let output_config = Temp.file "config.yaml" in
  let preimages_dir = Temp.dir "wasm_2_0_0" in
  let tez_bootstrap_accounts =
    (* tezos bootstrap accounts only relevant if tezos runtime is activated *)
    if not Tezosx_runtime.(mem Tezos with_runtimes) then []
    else tez_bootstrap_accounts
  in
  let*! () =
    Evm_node.make_kernel_installer_config
      ?maximum_gas_per_transaction
      ?set_account_code
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ~output:output_config
      ~eth_bootstrap_accounts
      ~tez_bootstrap_accounts
      ?evm_version
      ?with_runtimes
      ?sequencer:
        (Option.map (fun k -> k.Account.public_key)
        @@ List.nth_opt sequencer_keys 0)
      ()
  in
  let* {output; _} =
    Sc_rollup_helpers.prepare_installer_kernel
      ~preimages_dir
      ~config:(`Path output_config)
      kernel
  in
  let () = Account.write Constant.all_secret_keys ~base_dir:wallet_dir in
  let sequencer_mode =
    Evm_node.Sandbox
      {
        initial_kernel = Some output;
        network = None;
        preimage_dir = Some preimages_dir;
        private_rpc_port = Some (Port.fresh ());
        time_between_blocks = Some Nothing;
        genesis_timestamp;
        max_number_of_chunks = None;
        wallet_dir = Some wallet_dir;
        funded_addresses = [];
        tx_queue_max_lifespan;
        tx_queue_max_size;
        tx_queue_tx_per_addr_limit;
        sequencer_keys =
          List.map
            (fun k -> Account.uri_of_secret_key k.Account.secret_key)
            sequencer_keys;
      }
  in
  Evm_node.init
    ?history_mode
    ?patch_config
    ?websockets
    ~mode:sequencer_mode
    Uri.(empty |> to_string)

(* Send the transaction but doesn't wait to be mined and does not
   produce a block after sending the transaction. *)
let send_transaction_to_sequencer_dont_produce_block
    (transaction : unit -> 'a Lwt.t) sequencer =
  let wait_for = Evm_node.wait_for_tx_queue_add_transaction sequencer in
  let transaction = transaction () in
  let* _ = wait_for in
  Lwt.return transaction

let send_transactions_to_sequencer ~(sends : (unit -> 'a Lwt.t) list) sequencer
    =
  let open Rpc.Syntax in
  let* res =
    Lwt_list.map_s
      (fun tx -> send_transaction_to_sequencer_dont_produce_block tx sequencer)
      sends
  in
  (* Once the transactions are in the transaction pool the next block
     will include them. *)
  let*@ n = produce_block sequencer in
  (* Resolve the transactions sends to make sure they were included. *)
  let* transactions = Lwt.all res in
  Lwt.return (n, transactions)

let send_transaction_to_sequencer ?timestamp (transaction : unit -> 'a Lwt.t)
    sequencer =
  let open Rpc.Syntax in
  let* () =
    match timestamp with
    | Some timestamp ->
        (* We must propose a timestamp else the sequencer set it to now *)
        let*@ () = Rpc.propose_next_block_timestamp ~timestamp sequencer in
        unit
    | None -> unit
  in

  let* transaction =
    send_transaction_to_sequencer_dont_produce_block transaction sequencer
  in
  (* Once the transaction us in the transaction pool the next block
     will include it. *)
  let*@ _ = produce_block ?timestamp sequencer in
  (* Resolve the transaction sends to make sure they were included. *)
  transaction

let deposit ?env ?hooks ?log_output ?endpoint ?wait ?burn_cap ?fee ?gas_limit
    ?safety_guard ?storage_limit ?counter ?simulation ?force ?expect_failure
    ~amount ~giver ~sr_address ~bridge ~l2_address client =
  let arg = Printf.sprintf "(Pair \"%s\" %s)" sr_address l2_address in
  Client.spawn_transfer
    ?env
    ?log_output
    ?endpoint
    ?hooks
    ?wait
    ?burn_cap
    ?fee
    ?gas_limit
    ?safety_guard
    ?storage_limit
    ?counter
    ?simulation
    ?force
    ~entrypoint:"deposit"
    ~arg
    ~amount
    ~giver
    ~receiver:bridge
    client
  |> Process.check ?expect_failure

let check_operations ~client ~block ~expected =
  let* block = Client.RPC.call ~hooks client @@ RPC.get_chain_block ~block () in
  let operations = JSON.(block |-> "operations" |> as_list) in
  let managers = JSON.(List.nth operations 3 |> as_list) in
  let hashes = List.map (fun o -> JSON.(o |-> "hash" |> as_string)) managers in
  Check.((hashes = expected) (list string) ~error_msg:"Expected %R Actual %L") ;
  unit

let produce_block_and_wait_for ~sequencer n =
  let open Rpc.Syntax in
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* () = Evm_node.wait_for_blueprint_applied sequencer n in
  return ()

let register_sandbox ~__FILE__ ?(uses_client = false) ?kernel
    ?tx_queue_tx_per_addr_limit ~title ?tez_bootstrap_accounts ?set_account_code
    ?da_fee_per_byte ?minimum_base_fee_per_gas ~tags ?patch_config ?websockets
    ?sequencer_keys ?with_runtimes body =
  Test.register
    ~__FILE__
    ~title
    ~tags:
      (tags
      @ (Option.map (fun k -> Kernel.to_uses_and_tags k |> fst) kernel
        |> Option.to_list))
    ~uses_admin_client:
      (* using the client requires to use the admin client *)
      uses_client
    ~uses_client
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let* sequencer =
    init_sequencer_sandbox
      ?kernel:(Option.map (fun k -> Kernel.to_uses_and_tags k |> snd) kernel)
      ?tx_queue_tx_per_addr_limit
      ?set_account_code
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ?patch_config
      ?websockets
      ?sequencer_keys
      ?tez_bootstrap_accounts
      ?with_runtimes
      ()
  in
  body sequencer

type sandbox_test = {sandbox : Evm_node.t; observer : Evm_node.t}

let register_sandbox_with_observer ~__FILE__ ?kernel ?tx_queue_tx_per_addr_limit
    ~title ?set_account_code ?da_fee_per_byte ?minimum_base_fee_per_gas ~tags
    ?patch_config ?websockets ?(sequencer_keys = [Constant.bootstrap1]) body =
  Test.register
    ~__FILE__
    ~title
    ~tags:
      (tags
      @ (Option.map (fun k -> Kernel.to_uses_and_tags k |> fst) kernel
        |> Option.to_list))
    ~uses_admin_client:false
    ~uses_client:false
    ~uses_node:false
    ~uses:
      [
        Constant.octez_evm_node;
        Constant.WASM.evm_kernel;
        Constant.smart_rollup_installer;
      ]
  @@ fun () ->
  let* sandbox =
    init_sequencer_sandbox
      ?kernel:(Option.map (fun k -> Kernel.to_uses_and_tags k |> snd) kernel)
      ?tx_queue_tx_per_addr_limit
      ?set_account_code
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ?patch_config
      ?websockets
      ~sequencer_keys
      ()
  in
  let* observer =
    Setup.run_new_observer_node ?patch_config ~sc_rollup_node:None sandbox
  in
  body {sandbox; observer}
