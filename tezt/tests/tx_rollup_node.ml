(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Tx_rollup_node
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup_node.ml
   Subject:      Various test scenarios for the Tx rollup node
*)

module Rollup = Rollup.Tx_rollup
module Rollup_node = Rollup_node.Tx_node

let get_block_hash block_json = JSON.(block_json |-> "hash" |> as_string)

let node_rpc node service =
  let* opt_get = RPC.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
      let url = Format.asprintf "%s/%s" (Rollup_node.endpoint node) service in
      let* result = get ~url in
      return (Some result)

(* This function computes an inbox from the messages stored by the
   rollup node.  The way the inbox is computed is to use helpers
   exposed by the protocol. *)
let get_node_inbox ?(block = "head") node client =
  let* json = node_rpc node @@ "block/" ^ block ^ "/inbox" in
  match json with
  | None ->
      return Rollup.{inbox_length = 0; cumulated_size = 0; merkle_root = ""}
  | Some json ->
      let parse_message json =
        if JSON.(is_null (json |-> "batch")) then
          Test.fail "This case is not handled yet"
        else JSON.(json |-> "batch" |> as_string |> fun x -> `Batch (`Hex x))
      in
      let messages =
        JSON.(
          json |-> "contents" |> as_list
          |> List.map (fun x -> x |-> "message" |> parse_message))
      in
      Rollup.compute_inbox_from_messages messages client

let get_rollup_parameter_file ~protocol =
  let enable_tx_rollup = [(["tx_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  Protocol.write_parameter_file ~base enable_tx_rollup

(* Checks that the configuration is stored and that the required
   fields are present. *)
let test_node_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: configuration"
    ~tags:["tx_rollup"; "configuration"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      (* Originate a rollup with a given operator *)
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* block_hash = RPC.get_block_hash client in
      let tx_rollup_node =
        Rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* filename =
        Rollup_node.config_init tx_rollup_node tx_rollup_hash block_hash
      in
      Log.info "Tx_rollup configuration file was successfully created" ;
      let () =
        let open Ezjsonm in
        let req = ["operator"; "rollup_id"; "rpc_addr"] in
        (* TODO: add optional args checks *)
        match from_channel @@ open_in filename with
        | `O fields ->
            List.iter
              (fun k ->
                if List.exists (fun (key, _v) -> String.equal k key) fields then
                  ()
                else Test.fail "unexpected configuration field")
              req
        | _ -> Test.fail "Unexpected configuration format"
      in
      unit)

let init_and_run_rollup_node ~operator node client =
  let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  Log.info "Tx_rollup %s was successfully originated" tx_rollup_hash ;
  let* block_hash = RPC.get_block_hash client in
  let tx_node =
    Rollup_node.create
      ~rollup_id:tx_rollup_hash
      ~rollup_genesis:block_hash
      ~operator
      client
      node
  in
  let* _ = Rollup_node.config_init tx_node tx_rollup_hash block_hash in
  let* () = Rollup_node.run tx_node in
  Log.info "Tx_rollup node is now running" ;
  let* () = Rollup_node.wait_for_ready tx_node in
  Lwt.return (tx_rollup_hash, tx_node)

(* Checks that the tx_node is ready after originating an associated
   rollup key. *)
let test_tx_node_origination =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test if the node is ready"
    ~tags:["tx_rollup"; "ready"; "originate"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* _tx_node = init_and_run_rollup_node ~operator node client in
      unit)

(* Checks that an inbox received by the tx_rollup node is well stored
   and available in a percistent way. *)
let test_tx_node_store_inbox =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: store inbox"
    ~tags:["tx_rollup"; "store"; "inbox"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! rollup = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* block_hash = RPC.get_block_hash client in
      let tx_node =
        Rollup_node.create
          ~rollup_id:rollup
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Rollup_node.config_init tx_node rollup block_hash in
      let* () = Rollup_node.run tx_node in
      (* Submit a batch *)
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_1" in
      let*! () =
        Client.Tx_rollup.submit_batch ~content ~rollup ~src:operator client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 3 in
      let* node_inbox_1 = get_node_inbox ~block:"0" tx_node client in
      let*! expected_inbox_1 = Rollup.get_inbox ~rollup ~level:0 client in
      (* Ensure that stored inboxes on daemon's side are equivalent of
         inboxes returned by the rpc call. *)
      Check.(node_inbox_1 = expected_inbox_1)
        Rollup.Check.inbox
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      let (`Batch content) = Rollup.make_batch "tezos_l2_batch_2" in
      let*! () =
        Client.Tx_rollup.submit_batch ~content ~rollup ~src:operator client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* node_inbox_2 = get_node_inbox ~block:"1" tx_node client in
      let*! expected_inbox_2 = Rollup.get_inbox ~rollup ~level:1 client in
      (* Ensure that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(node_inbox_2 = expected_inbox_2)
        Rollup.Check.inbox
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      (* Stop the node and try to get the inbox once again*)
      let* () = Rollup_node.terminate tx_node in
      let* () = Rollup_node.run tx_node in
      let* () = Rollup_node.wait_for_ready tx_node in
      let*! inbox_after_restart = Rollup.get_inbox ~rollup ~level:1 client in
      Check.(node_inbox_2 = inbox_after_restart)
        Rollup.Check.inbox
        ~error_msg:
          "Unexpected inbox computed from the rollup node. Expected %R. \
           Computed %L" ;
      unit)

(* FIXME/TORU: This is a temporary way of querying the node without
   tx_rollup_client. This aims to be replaced as soon as possible by
   the dedicated client's RPC. *)
let raw_tx_node_rpc node ~url =
  let* rpc = RPC.Curl.get () in
  match rpc with
  | None -> assert false
  | Some curl ->
      let url = Printf.sprintf "%s/%s" (Rollup_node.rpc_addr node) url in
      curl ~url

let raw_tx_node_rpc_post node ~url data =
  let* rpc = RPC.Curl.post () in
  match rpc with
  | None -> assert false
  | Some curl ->
      let url = Printf.sprintf "%s/%s" (Rollup_node.rpc_addr node) url in
      curl ~url data

(* FIXME/TORU: Must be replaced by the Tx_client.get_balance command *)
let tx_client_get_balance ~tx_node ~block ~ticket_id ~tz4_address =
  let* json =
    raw_tx_node_rpc
      tx_node
      ~url:
        ("context/" ^ block ^ "/tickets/" ^ ticket_id ^ "/balance/"
       ^ tz4_address)
  in
  match JSON.(json |> as_int_opt) with
  | Some level -> Lwt.return level
  | None -> Test.fail "Cannot retrieve balance of tz4 address %s" tz4_address

let tx_client_inject_transaction ~tx_node ?failswith transaction signature =
  let open Tezos_protocol_alpha.Protocol in
  let signed_tx_json =
    JSON.annotate ~origin:"signed_l2_transaction"
    @@ `O
         [
           ( "transaction",
             Data_encoding.Json.construct
               Tx_rollup_l2_batch.V1.transaction_encoding
               transaction );
           ( "signature",
             Data_encoding.Json.construct
               Tx_rollup_l2_context_sig.signature_encoding
               signature );
         ]
  in
  let* json =
    raw_tx_node_rpc_post
      tx_node
      ~url:"queue/injection/transaction"
      signed_tx_json
  in
  match failswith with
  | None -> (
      try return (JSON.as_string json)
      with _ ->
        Test.fail "Transaction injection failed with: %s" (JSON.encode json))
  | Some expected_id ->
      let error_id = JSON.(json |> as_list |> List.hd |-> "id" |> as_string) in
      Check.((error_id = expected_id) string)
        ~error_msg:"Injection should have failed with %R but failed with %L" ;
      (* Dummy value for operation hash *)
      return ""

let tx_client_get_queue ~tx_node = raw_tx_node_rpc tx_node ~url:"queue"

let tx_client_get_transaction_in_queue ~tx_node txh =
  raw_tx_node_rpc tx_node ~url:("queue/transaction/" ^ txh)

let tx_client_get_block ~tx_node ~block =
  raw_tx_node_rpc tx_node ~url:("block/" ^ block)

(* FIXME/TORU: Must be replaced by the Tx_client.get_inbox command *)
let tx_client_get_inbox ~tx_node ~block =
  raw_tx_node_rpc tx_node ~url:("block/" ^ block ^ "/inbox")

(* Returns the ticket hash, if any, of a given operation. *)
let get_ticket_hash_from_op op =
  let metadata = JSON.(op |-> "contents" |=> 0 |-> "metadata") in
  let result =
    JSON.(metadata |-> "internal_operation_results" |=> 0 |-> "result")
  in
  let kind =
    JSON.(
      metadata |-> "internal_operation_results" |=> 0 |-> "parameters"
      |-> "entrypoint" |> as_string)
  in
  let deposit = "deposit" in
  if not String.(equal kind deposit) then
    Test.fail
      "The internal operation was expected to be a %s but is a %s"
      deposit
      kind ;
  let status = JSON.(result |-> "status" |> as_string_opt) in
  match status with
  | Some v when String.(equal v "applied") ->
      JSON.(result |-> "ticket_hash" |> as_string)
  | None | Some _ -> Test.fail "The contract origination failed"

let get_ticket_hash_from_deposit d =
  JSON.(d |-> "message" |-> "deposit" |-> "ticket_hash" |> as_string)

(* The contract is expecting a parameter of the form:
   (Pair tx_rollup_txr1_address tx_rollup_tz4_address) *)
let make_tx_rollup_deposit_argument txr1 tz4 =
  "( Pair " ^ "\"" ^ txr1 ^ "\" \"" ^ tz4 ^ "\")"

(* FIXME/TORU: we should merge this into Tezos_crypto.Bls *)
module Bls_public_key_hash = struct
  include
    Tezos_crypto.Blake2B.Make
      (Tezos_crypto.Base58)
      (struct
        let name = "Bls12_381.Public_key_hash"

        let title = "A Bls12_381 public key hash"

        let b58check_prefix = "\006\161\166" (* tz4(36) *)

        let size = Some 20
      end)
end

let generate_bls_addr ?alias:_ _client =
  (* FIXME/TORU: we should use the the client to generate keys *)
  let seed =
    let rng_state = Random.State.make_self_init () in
    Bytes.init 32 (fun _ -> char_of_int @@ Random.State.int rng_state 255)
  in
  let sk = Bls12_381.Signature.generate_sk seed in
  let pk = Bls12_381.Signature.MinSig.derive_pk sk in
  let pkh =
    Bls_public_key_hash.hash_bytes [Bls12_381.Signature.MinSig.pk_to_bytes pk]
  in
  Log.info
    "A new BLS key was generated: %s"
    (Bls_public_key_hash.to_b58check pkh) ;
  (pkh, pk, sk)

let check_tz4_balance ~tx_node ~block ~ticket_id ~tz4_address ~expected_balance
    =
  let* tz4_balance =
    tx_client_get_balance ~tx_node ~block ~ticket_id ~tz4_address
  in
  Check.(
    ( = )
      tz4_balance
      expected_balance
      int
      ~error_msg:
        (Format.sprintf
           "The balance of %s was expected to be %d instead of %d."
           tz4_address
           expected_balance
           tz4_balance)) ;
  unit

(* Checks that the a ticket can be transfered from the L1 to the rollup. *)
let test_ticket_deposit_from_l1_to_l2 =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: deposit ticket from l1 to l2"
    ~tags:["tx_rollup"; "deposit"; "ticket"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      let (bls_pkh, _, _) = generate_bls_addr client in
      let bls_pkh_str = Bls_public_key_hash.to_b58check bls_pkh in
      let arg = make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_str in
      (* This smart contract call will transfer 10 tickets to the
         given address. *)
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      (* Get the operation containing the ticket transfer. We assume
         that only one operation is issued in this block. *)
      let* inbox = tx_client_get_inbox ~tx_node ~block:"head" in
      let ticket_id =
        get_ticket_hash_from_deposit JSON.(inbox |-> "contents" |=> 0)
      in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_str
          ~expected_balance:10
      in
      unit)

let sign_transaction sks txs =
  let open Tezos_protocol_alpha.Protocol in
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  List.map (fun sk -> Bls12_381.Signature.MinSig.Aug.sign sk buf) sks

let sign_one_transaction sk txs =
  let open Tezos_protocol_alpha.Protocol in
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  Bls12_381.Signature.MinSig.Aug.sign sk buf

let craft_tx ~counter ~signer ~dest ~ticket qty =
  let open Tezos_protocol_alpha.Protocol in
  let qty = Tx_rollup_l2_qty.of_int64_exn qty in
  let l2_addr = Tx_rollup_l2_address.of_b58check_exn dest in
  let destination = Indexable.from_value l2_addr in
  let ticket_hash =
    Indexable.from_value
      (Tezos_protocol_alpha.Protocol.Alpha_context.Ticket_hash.of_b58check_exn
         ticket)
  in
  let content =
    Tx_rollup_l2_batch.V1.Transfer {destination; ticket_hash; qty}
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents = [content]}

let aggregate_signature_exn signatures =
  match Bls12_381.Signature.MinSig.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> invalid_arg "aggregate_signature_exn"

let batch signatures contents =
  let open Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 in
  let aggregated_signature = aggregate_signature_exn signatures in
  {aggregated_signature; contents}

let craft_batch
    (transactions :
      ( 'signer,
        'content )
      Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1.transaction
      list) sks =
  let signatures =
    List.map2 (fun txs sk -> sign_transaction sk txs) transactions sks
    |> List.concat
  in
  batch signatures transactions

(* Checks that the a ticket can be transfered within the rollup. *)
let test_l2_to_l2_transaction =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: l2 to l2 transaction"
    ~tags:["tx_rollup"; "rollup"; "internal"; "transaction"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Genarating some identities *)
      let (bls_1_pkh, bls_pk_1, bls_sk_1) = generate_bls_addr client in
      let bls_pkh_1_str = Bls_public_key_hash.to_b58check bls_1_pkh in
      (* FIXME/TORU: Use the client *)
      let (bls_2_pkh, _, _) = generate_bls_addr client in
      let bls_pkh_2_str = Bls_public_key_hash.to_b58check bls_2_pkh in
      let arg_1 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_1_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = tx_client_get_inbox ~tx_node ~block:"head" in
      let ticket_id =
        get_ticket_hash_from_deposit JSON.(inbox |-> "contents" |=> 0)
      in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:10
      in
      let arg_2 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_2_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_2
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:10
      in
      Log.info "Crafting a l2 transaction" ;
      (* FIXME/TORU: Use the client *)
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1L
      in
      Log.info "Crafting a batch" ;
      let batch = craft_batch [[tx]] [[bls_sk_1]] in
      let content =
        Hex.of_string
          (Data_encoding.Binary.to_string_exn
             Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.encoding
             (Tezos_protocol_alpha.Protocol.Tx_rollup_l2_batch.V1 batch))
      in
      Log.info "Submiting a batch" ;
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content
          ~rollup:tx_rollup_hash
          ~src:operator
          client
      in
      Log.info "Baking the batch" ;
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 6 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 6 in
      (* The decoding fails because of the buggy JSON encoding. This
         line can be uncommented once it is fixed.

         let* _node_inbox = get_node_inbox tx_node client in *)
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:9
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:11
      in
      unit)

(* Checks that the rollup node can receive L2 transactions in its queue, batch
   them and inject them in the Tezos node. *)
let test_batcher =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: L2 batcher"
    ~tags:["tx_rollup"; "node"; "batcher"; "transaction"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* (tx_rollup_hash, tx_node) =
        init_and_run_rollup_node ~operator node client
      in
      let* contract_id =
        Client.originate_contract
          ~alias:"rollup_deposit"
          ~amount:Tez.zero
          ~src:"bootstrap1"
          ~prg:"file:./tezt/tests/contracts/proto_alpha/tx_rollup_deposit.tz"
          ~init:"Unit"
          ~burn_cap:Tez.(of_int 1)
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      Log.info
        "The tx_rollup_deposit %s contract was successfully originated"
        contract_id ;
      (* Genarating some identities *)
      let (bls_1_pkh, bls_pk_1, bls_sk_1) = generate_bls_addr client in
      let bls_pkh_1_str = Bls_public_key_hash.to_b58check bls_1_pkh in
      (* FIXME/TORU: Use the client *)
      let (bls_2_pkh, bls_pk_2, bls_sk_2) = generate_bls_addr client in
      let bls_pkh_2_str = Bls_public_key_hash.to_b58check bls_2_pkh in
      let arg_1 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_1_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_1
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* inbox = tx_client_get_inbox ~tx_node ~block:"head" in
      let ticket_id =
        get_ticket_hash_from_deposit JSON.(inbox |-> "contents" |=> 0)
      in
      Log.info "Ticket %s was successfully emitted" ticket_id ;
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:10
      in
      let arg_2 =
        make_tx_rollup_deposit_argument tx_rollup_hash bls_pkh_2_str
      in
      let* () =
        Client.transfer
          ~gas_limit:100_000
          ~fee:Tez.one
          ~amount:Tez.zero
          ~burn_cap:Tez.one
          ~storage_limit:10_000
          ~giver:"bootstrap1"
          ~receiver:contract_id
          ~arg:arg_2
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 5 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 5 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:10
      in
      Log.info
        "Crafting a l2 transaction: %s transfers 1 to %s"
        bls_pkh_1_str
        bls_pkh_2_str ;
      (* FIXME/TORU: Use the client *)
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1L
      in
      let signature = sign_one_transaction bls_sk_1 [tx] in
      Log.info "Submitting the L2 transaction" ;
      let* txh1 = tx_client_inject_transaction ~tx_node [tx] signature in
      Log.info "Successfully submitted L2 transaction %s" txh1 ;
      Log.info
        "Crafting a l2 transaction: %s transfers 5 to %s"
        bls_pkh_2_str
        bls_pkh_1_str ;
      let tx =
        craft_tx
          ~counter:1L
          ~signer:(Bls_pk bls_pk_2)
          ~dest:bls_pkh_1_str
          ~ticket:ticket_id
          5L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the L2 transaction" ;
      let* txh2 = tx_client_inject_transaction ~tx_node [tx] signature in
      Log.info "Successfully submitted L2 transaction %s" txh2 ;

      Log.info "Crafting a l2 transaction with wrong counter" ;
      let tx =
        craft_tx
          ~counter:5L
          ~signer:(Bls_pk bls_pk_2)
          ~dest:bls_pkh_1_str
          ~ticket:ticket_id
          5L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the bad counter L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_operation_counter_mismatch"
          signature
      in

      Log.info "Crafting a l2 transaction with wrong signature" ;
      let tx =
        craft_tx
          ~counter:2L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          1L
      in
      let signature = sign_one_transaction bls_sk_2 [tx] in
      Log.info "Submitting the bad signature L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_incorrect_aggregated_signature"
          signature
      in

      Log.info "Crafting a l2 transaction with too big amount" ;
      let tx =
        craft_tx
          ~counter:2L
          ~signer:(Bls_pk bls_pk_1)
          ~dest:bls_pkh_2_str
          ~ticket:ticket_id
          15L
      in
      let signature = sign_one_transaction bls_sk_1 [tx] in
      Log.info "Submitting the wrong amount L2 transaction" ;
      let* _ =
        tx_client_inject_transaction
          ~tx_node
          [tx]
          ~failswith:"proto.alpha.tx_rollup_balance_too_low"
          signature
      in
      Log.info "Checking rollup node queue" ;
      let* q = tx_client_get_queue ~tx_node in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = 2) int) ~error_msg:"Queue length is %L but should be %R" ;
      Log.info "Checking rollup node queue transactions" ;
      let* _t1 = tx_client_get_transaction_in_queue ~tx_node txh1
      and* _t2 = tx_client_get_transaction_in_queue ~tx_node txh2 in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 6 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 6 in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 7 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 7 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:14
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:6
      in

      let inject_tx ~counter ~from ~dest ?(amount = 1L) sk =
        let tx =
          craft_tx ~counter ~signer:from ~dest ~ticket:ticket_id amount
        in
        let signature = sign_one_transaction sk [tx] in
        tx_client_inject_transaction ~tx_node [tx] signature
      in
      let nbtxs1 = 13 in
      Log.info "Injecting %d transactions to queue" nbtxs1 ;
      let* () =
        Lwt_list.iter_s
          (fun counter ->
            let* _ =
              inject_tx
                ~counter
                ~from:(Bls_pk bls_pk_1)
                ~dest:bls_pkh_2_str
                bls_sk_1
            in
            unit)
          (List.init nbtxs1 (fun i -> Int64.of_int (i + 2)))
      in
      let nbtxs2 = 6 in
      Log.info "Injecting %d transactions to queue" nbtxs2 ;
      let* () =
        Lwt_list.iter_s
          (fun counter ->
            let* _ =
              inject_tx
                ~counter
                ~from:(Bls_pk bls_pk_2)
                ~dest:bls_pkh_1_str
                bls_sk_2
            in
            unit)
          (List.init nbtxs2 (fun i -> Int64.of_int (i + 2)))
      in
      let* q = tx_client_get_queue ~tx_node in
      let len_q = JSON.(q |> as_list |> List.length) in
      Check.((len_q = nbtxs1 + nbtxs2) int)
        ~error_msg:"Queue length is %L but should be %R" ;
      let* () = Client.bake_for client in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 8 in
      let* () = Client.bake_for client in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 9 in
      let* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_1_str
          ~expected_balance:7
      and* () =
        check_tz4_balance
          ~tx_node
          ~block:"head"
          ~ticket_id
          ~tz4_address:bls_pkh_2_str
          ~expected_balance:13
      in
      unit)

let register ~protocols =
  test_node_configuration protocols ;
  test_tx_node_origination protocols ;
  test_tx_node_store_inbox protocols ;
  test_ticket_deposit_from_l1_to_l2 protocols ;
  test_l2_to_l2_transaction protocols ;
  test_batcher protocols
