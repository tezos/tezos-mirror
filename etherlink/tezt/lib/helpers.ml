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

let mapping_position index map_position =
  Tezos_crypto.Hacl.Hash.Keccak_256.digest
    (Hex.to_bytes
       (`Hex (leftPad32 index ^ leftPad32 (string_of_int map_position))))
  |> Hex.of_bytes |> Hex.show |> add_0x

let hex_string_to_int x = `Hex x |> Hex.to_string |> Z.of_bits |> Z.to_int

let hex_256_of_int n = Printf.sprintf "%064x" n

let next_rollup_node_level ~sc_rollup_node ~client =
  let* () = Client.bake_for_and_wait ~keys:[] client in
  Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node

let produce_block ?(wait_on_blueprint_applied = true) ?timestamp evm_node =
  match Evm_node.mode evm_node with
  | Sequencer _ -> Rpc.produce_block ?timestamp evm_node
  | Threshold_encryption_sequencer _ -> (
      let open Rpc.Syntax in
      let*@ current_number = Rpc.block_number evm_node in
      let wait_blueprint =
        if wait_on_blueprint_applied then
          Evm_node.wait_for_blueprint_applied
            evm_node
            (Int32.to_int current_number + 1)
        else unit
      in
      let* res = Rpc.produce_proposal ?timestamp evm_node
      and* () = wait_blueprint in
      match res with Ok () -> return (Ok 0) | Error res -> return (Error res))
  | _ -> assert false

let next_evm_level ~evm_node ~sc_rollup_node ~client =
  match Evm_node.mode evm_node with
  | Proxy ->
      let* _l1_level = next_rollup_node_level ~sc_rollup_node ~client in
      unit
  | Sequencer _ | Threshold_encryption_sequencer _ ->
      let open Rpc.Syntax in
      let*@ _l2_level = produce_block evm_node in
      unit
  | Observer _ -> Test.fail "Cannot create a new level with an Observer node"
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
         See [FORCE_KERNEL_UPGRADE_TAG] in [etherlink/kernel_evm/kernel/src/parsing.rs] *)
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
  unit

let check_block_consistency ~left ~right ?error_msg ~block () =
  let open Rpc.Syntax in
  let block =
    match block with `Latest -> "latest" | `Level l -> Int32.to_string l
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

let bake_until ?__LOC__ ?(timeout_in_blocks = 5) ?(timeout = 30.) ~bake
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
let wait_for_transaction_receipt ?(count = 3) ~evm_node ~transaction_hash () =
  let rec loop count =
    let* () = Lwt_unix.sleep 5. in
    let* receipt =
      Evm_node.(
        call_evm_rpc
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

let wait_for_application ~evm_node ~sc_rollup_node ~client apply =
  let max_iteration = 10 in
  let application_result = apply () in
  let rec loop current_iteration =
    let* () = Lwt_unix.sleep 5. in
    let* () = next_evm_level ~evm_node ~sc_rollup_node ~client in
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

let batch_n_transactions ~evm_node txs =
  let requests =
    List.map
      (fun tx ->
        Evm_node.
          {method_ = "eth_sendRawTransaction"; parameters = `A [`String tx]})
      txs
  in
  let* hashes = Evm_node.batch_evm_rpc evm_node requests in
  let hashes =
    hashes |> JSON.as_list
    |> List.map (fun json -> Evm_node.extract_result json |> JSON.as_string)
  in
  return (requests, hashes)

(* sending more than ~300 tx could fail, because or curl *)
let send_n_transactions ~sc_rollup_node ~client ~evm_node ?wait_for_blocks txs =
  let* requests, hashes = batch_n_transactions ~evm_node txs in
  let first_hash = List.hd hashes in
  (* Let's wait until one of the transactions is injected into a block, and
      test this block contains the `n` transactions as expected. *)
  let* receipt =
    wait_for_application
      ~evm_node
      ~sc_rollup_node
      ~client
      (wait_for_transaction_receipt
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

let find_and_execute_withdrawal ~withdrawal_level ~commitment_period
    ~challenge_window ~evm_node ~sc_rollup_node ~sc_rollup_address ~client =
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
      if level' > level + 10 then
        Test.fail "Looked for an outbox for 10 levels, stopping the loop"
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
