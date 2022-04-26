(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Clic

let l2_addr_param =
  Clic.parameter (fun _ s ->
      match Tx_rollup_l2_address.of_b58check_opt s with
      | Some addr -> return addr
      | None -> failwith "The given rollup address is invalid")

let block_id_param =
  let open Lwt_result_syntax in
  Clic.parameter (fun _ s ->
      match RPC.destruct_block_id s with
      | Ok v -> return v
      | Error e -> failwith "%s" e)

let parse_ticket =
  Clic.parameter (fun _ s ->
      return (Alpha_context.Ticket_hash.of_b58check_exn s))

let get_tx_address_balance_command () =
  command
    ~desc:"returns the balance associated to a given tz4 address and ticket"
    (args1
       (default_arg
          ~long:"block"
          ~placeholder:"block"
          ~doc:"block from which the balance is expected"
          ~default:"head"
          block_id_param))
    (prefixes ["get"; "balance"; "for"]
    @@ param
         ~name:"tz4"
         ~desc:"tz4 address from which the balance is queried"
         l2_addr_param
    @@ prefixes ["of"]
    @@ param
         ~name:"ticket-hash"
         ~desc:"ticket from which the balance is expected"
         parse_ticket
    @@ stop)
    (fun block tz4 ticket (cctxt : #Configuration.tx_client_context) ->
      RPC.balance cctxt block ticket tz4 >>=? fun value ->
      cctxt#message "@[%a@]" Tx_rollup_l2_qty.pp value >>= fun () -> return_unit)

let get_tx_inbox () =
  command
    ~desc:"returns the inbox for a given block identifier"
    no_options
    (prefixes ["get"; "inbox"; "for"]
    @@ param
         ~name:"block"
         ~desc:"block from which the inbox is requested"
         block_id_param
    @@ stop)
    (fun () block (cctxt : #Configuration.tx_client_context) ->
      RPC.inbox cctxt block >>=? fun inbox ->
      let json = Data_encoding.(Json.construct (option Inbox.encoding)) inbox in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let get_tx_block () =
  command
    ~desc:"returns the tx rollup block for a given block identifier"
    no_options
    (prefixes ["get"; "block"]
    @@ param ~name:"block" ~desc:"block requested" block_id_param
    @@ stop)
    (fun () block (cctxt : #Configuration.tx_client_context) ->
      RPC.block cctxt block >>=? fun block ->
      let json =
        Data_encoding.(Json.construct (option RPC.Encodings.block)) block
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let sign_transaction sks txs =
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  List.map (fun sk -> Bls.sign sk buf) sks

let craft_tx ~counter ~signer ~destination ~ticket_hash ~qty =
  let content =
    Tx_rollup_l2_batch.V1.(
      Transfer
        {
          destination = Indexable.from_value destination;
          ticket_hash = Indexable.from_value ticket_hash;
          qty;
        })
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents = [content]}

let aggregate_signature_exn signatures =
  match Bls.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> invalid_arg "aggregate_signature_exn"

let batch signatures contents =
  let open Tx_rollup_l2_batch.V1 in
  let aggregated_signature = aggregate_signature_exn signatures in
  {aggregated_signature; contents}

let craft_batch
    (transactions : ('signer, 'content) Tx_rollup_l2_batch.V1.transaction list)
    sks =
  let signatures =
    List.map2
      ~when_different_lengths:()
      (fun txs sk -> sign_transaction sk txs)
      transactions
      sks
    |> (function
         | Ok r -> r
         | Error () ->
             (* assumed valid thanks to preconditions *)
             assert false)
    |> List.concat
  in
  batch signatures transactions

let conv_pk =
  Clic.parameter (fun _ pk_str ->
      return (Bls.Public_key.of_b58check_exn pk_str))

let conv_qty =
  Clic.parameter (fun _ qty ->
      match Tx_rollup_l2_qty.of_string qty with
      | Some qty -> return qty
      | None -> failwith "The given qty is invalid")

let conv_counter =
  Clic.parameter (fun _ counter -> return (Int64.of_string counter))

let craft_tx_transaction () =
  command
    ~desc:"WIP: craft a transaction"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "transferring"]
    @@ param ~name:"qty" ~desc:"qty to transfer" conv_qty
    @@ prefixes ["from"]
    @@ param ~name:"signer_pk" ~desc:"public key of the signer" conv_pk
    @@ prefixes ["to"]
    @@ param ~name:"dest" ~desc:"tz4 destination address" l2_addr_param
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to transfer" parse_ticket
    @@ stop)
    (fun counter
         qty
         signer_pk
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      (match counter with
      | Some c -> return c
      | None ->
          (* Retrieve the counter of the current head and increments it
             by one. *)
          (match RPC.destruct_block_id "head" with
          | Ok v -> return v
          | Error e -> failwith "Cannot get counter of current head (%s)" e)
          >>=? fun head ->
          let pkh = Tx_rollup_l2_address.of_bls_pk signer_pk in
          RPC.counter cctxt head pkh >>=? fun counter ->
          return (Int64.succ counter))
      >>=? fun counter ->
      let signer = Tx_rollup_l2_batch.Bls_pk signer_pk in
      let op = craft_tx ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          (Data_encoding.list Tx_rollup_l2_batch.V1.transaction_encoding)
          [[op]]
      in
      cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
      return_unit)

let batch_of_json ~txs:tx_json ~sks:sks_json =
  let transactions =
    Data_encoding.(
      Json.destruct (list Tx_rollup_l2_batch.V1.transaction_encoding) tx_json)
  in
  let sks =
    Data_encoding.(Json.destruct (list (list Bls.Secret_key.encoding)) sks_json)
  in
  craft_batch transactions sks

let craft_tx_batch () =
  command
    ~desc:"craft a transactional rollup batch"
    no_options
    (prefixes ["craft"; "batch"; "with"]
    @@ string ~name:"json" ~desc:"JSON containing a list of operations"
    @@ prefixes ["for"]
    @@ string
         ~name:"secret-keys"
         ~desc:"[UNSAFE] JSON list of secret keys to sign"
    @@ stop)
    (fun () json_str sks_str (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* txs =
        match Data_encoding.Json.from_string json_str with
        | Ok json -> return json
        | Error _ -> failwith "Cannot decode the given json"
      in
      let* sks =
        match Data_encoding.Json.from_string sks_str with
        | Ok json -> return json
        | Error _ -> failwith "Cannot decode the given json"
      in
      let batch = batch_of_json ~txs ~sks in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.encoding
          (Tx_rollup_l2_batch.V1 batch)
      in
      cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
      return_unit)

let get_batcher_queue () =
  command
    ~desc:"returns the batcher's queue of pending operations"
    no_options
    (prefixes ["get"; "batcher"; "queue"] @@ stop)
    (fun () (cctxt : #Configuration.tx_client_context) ->
      RPC.get_queue cctxt >>=? fun queue ->
      let json =
        Data_encoding.(Json.construct (list L2_transaction.encoding) queue)
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let valid_transaction_hash =
  Clic.parameter (fun _ s ->
      match L2_transaction.Hash.of_b58check_opt s with
      | Some addr -> return addr
      | None -> failwith "The L2 transaction hash is invalid")

let get_batcher_transaction () =
  command
    ~desc:"returns a batcher transaction for a given hash"
    no_options
    (prefixes ["get"; "batcher"; "transaction"]
    @@ param
         ~name:"hash"
         ~desc:"requested transaction hash"
         valid_transaction_hash
    @@ stop)
    (fun () hash (cctxt : #Configuration.tx_client_context) ->
      RPC.get_transaction cctxt hash >>=? fun tx ->
      let json =
        Data_encoding.(Json.construct (option L2_transaction.encoding)) tx
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let inject_batcher_transaction () =
  let open Lwt_result_syntax in
  command
    ~desc:"injects the given transaction into the batcher's transaction queue"
    no_options
    (prefixes ["inject"; "batcher"; "transaction"]
    @@ string
         ~name:"signed_tx_json"
         ~desc:"injects a signed transaction into the batcher"
    @@ stop)
    (fun () signed_tx_json (cctxt : #Configuration.tx_client_context) ->
      let* json =
        match Data_encoding.Json.from_string signed_tx_json with
        | Ok json -> return json
        | Error _ -> failwith "cannot decode signed transactions"
      in
      let signed_tx =
        Data_encoding.Json.destruct L2_transaction.encoding json
      in
      RPC.inject_transaction cctxt signed_tx >>=? fun txh ->
      let json =
        Data_encoding.(Json.construct L2_transaction.Hash.encoding txh)
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let all () =
  [
    get_tx_address_balance_command ();
    get_tx_inbox ();
    get_tx_block ();
    craft_tx_transaction ();
    craft_tx_batch ();
    get_batcher_queue ();
    get_batcher_transaction ();
    inject_batcher_transaction ();
  ]
