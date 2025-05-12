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

let evm_type =
  "or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes"

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

let next_rollup_node_level ~sc_rollup_node ~client =
  let* l1_level = Client.bake_for_and_wait_level ~keys:[] client in
  Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node l1_level

let produce_block ?(wait_on_blueprint_applied = true) ?timestamp evm_node =
  match Evm_node.mode evm_node with
  | Sandbox _ | Sequencer _ -> Rpc.produce_block ?timestamp evm_node
  | Threshold_encryption_sequencer {time_between_blocks; _} -> (
      let open Rpc.Syntax in
      let*@ current_number = Rpc.block_number evm_node in
      let wait_blueprint =
        if wait_on_blueprint_applied then
          Evm_node.wait_for_blueprint_applied
            evm_node
            (Int32.to_int current_number + 1)
        else unit
      in
      let* res =
        (* if time_between_blocks is not Nothing, then it is unsafe
           so make a produce_proposal request as the proposal handler might
           be locked, in which case the threshold encryption sequencer will fail. *)
        if time_between_blocks = Some Nothing then
          Rpc.produce_proposal ?timestamp evm_node
        else return @@ Ok ()
      and* () = wait_blueprint in
      match res with Ok () -> return (Ok 0) | Error res -> return (Error res))
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

let next_evm_level ~evm_node ~sc_rollup_node ~client =
  match Evm_node.mode evm_node with
  | Proxy ->
      let* _l1_level = next_rollup_node_level ~sc_rollup_node ~client in
      unit
  | Sequencer _ | Sandbox _ | Threshold_encryption_sequencer _ ->
      let open Rpc.Syntax in
      let*@ _l2_level = produce_block evm_node in
      unit
  | Observer _ -> Test.fail "Cannot create a new level with an Observer node"
  | Rpc _ -> Test.fail "Cannot create a new level with a Rpc node"
  | Threshold_encryption_observer _ ->
      Test.fail
        "Cannot create a new level with a Threshold encryption observer node"

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
    ^ (* Force kernel upgrade tag.
         See [FORCE_KERNEL_UPGRADE_TAG] in [etherlink/kernel_latest/kernel/src/parsing.rs] *)
    "\255"
    |> Hex.of_string |> Hex.show
  in
  let* () =
    Sc_rollup_helpers.send_message
      client
      (sf "hex:[%S]" force_kernel_upgrade_payload)
  in
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
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
  let* _ = next_rollup_node_level ~sc_rollup_node ~client in
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

let bake_until_sync ?__LOC__ ?timeout_in_blocks ?timeout ~sc_rollup_node ~proxy
    ~sequencer ~client () =
  let bake () =
    let* _l1_lvl = next_rollup_node_level ~sc_rollup_node ~client in
    unit
  in
  let result_f () =
    let open Rpc.Syntax in
    let* proxy_level_opt = Rpc.block_number_opt proxy in
    let*@ sequencer_level = Rpc.block_number sequencer in
    match proxy_level_opt with
    | Error _ | Ok None -> Lwt.return_none
    | Ok (Some proxy_level) ->
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

let wait_for_application ~produce_block apply =
  let open Rpc.Syntax in
  let max_iteration = 10 in
  let application_result = apply () in
  let rec loop current_iteration =
    let* () = Lwt_unix.sleep 5. in
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
        let* _ = next_rollup_node_level ~sc_rollup_node ~client in
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
    ?tx_pool_tx_per_addr_limit ?set_account_code ?da_fee_per_byte
    ?minimum_base_fee_per_gas ?history_mode ?patch_config
    ?(kernel = Constant.WASM.evm_kernel) ?evm_version
    ?(eth_bootstrap_accounts =
      List.map
        (fun account -> account.Eth_account.address)
        (Array.to_list Eth_account.bootstrap_accounts)) () =
  let wallet_dir = Temp.dir "wallet" in
  let output_config = Temp.file "config.yaml" in
  let preimages_dir = Temp.dir "wasm_2_0_0" in

  let*! () =
    Evm_node.make_kernel_installer_config
      ?maximum_gas_per_transaction
      ?set_account_code
      ?da_fee_per_byte
      ?minimum_base_fee_per_gas
      ~output:output_config
      ~eth_bootstrap_accounts
      ?evm_version
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
        initial_kernel = output;
        preimage_dir = Some preimages_dir;
        private_rpc_port = Some (Port.fresh ());
        time_between_blocks = Some Nothing;
        genesis_timestamp;
        max_number_of_chunks = None;
        wallet_dir = Some wallet_dir;
        tx_pool_timeout_limit = None;
        tx_pool_addr_limit = None;
        tx_pool_tx_per_addr_limit;
      }
  in
  Evm_node.init
    ?history_mode
    ?patch_config
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
