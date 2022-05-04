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
open Tezos_client_base

let l1_destination_parameter =
  parameter (fun _ s ->
      match Signature.Public_key_hash.of_b58check_opt s with
      | Some addr -> return addr
      | None -> failwith "cannot parse %s to get a valid destination" s)

let parse_file parse path =
  Lwt_utils_unix.read_file path >>= fun contents -> parse contents

let file_or_text_parameter ~from_text
    ?(from_path = parse_file (from_text ~heuristic:false)) () =
  parameter @@ fun _ p ->
  match String.split ~limit:1 ':' p with
  | ["text"; text] -> from_text ~heuristic:false text
  | ["file"; path] -> from_path path
  | _ -> if Sys.file_exists p then from_path p else from_text ~heuristic:true p

let json_file_or_text_parameter =
  let from_text ~heuristic s =
    try return (Ezjsonm.from_string s)
    with Ezjsonm.Parse_error _ when heuristic ->
      failwith "Neither an existing file nor valid JSON: '%s'" s
  in
  let from_path = Lwt_utils_unix.Json.read_file in
  file_or_text_parameter ~from_text ~from_path ()

let json_parameter =
  let from_text ~heuristic s =
    try return (Ezjsonm.from_string s)
    with Ezjsonm.Parse_error _ when heuristic ->
      failwith "Neither an existing file nor valid JSON: '%s'" s
  in
  file_or_text_parameter ~from_text ()

let alias_or_literal ~from_alias ~from_key =
  Client_aliases.parse_alternatives [("alias", from_alias); ("key", from_key)]

type wallet_entry = {
  alias : string;
  public_key_hash : Bls.Public_key_hash.t;
  public_key : Bls.Public_key.t option;
  secret_key_uri : Client_keys.aggregate_sk_uri option;
}

let wallet_parameter () =
  parameter (fun cctxt alias ->
      let open Lwt_result_syntax in
      let open Aggregate_signature in
      let* (Bls12_381 public_key_hash) =
        Client_keys.Aggregate_alias.Public_key_hash.find cctxt alias
      in
      let* (_, pk_opt) =
        Client_keys.Aggregate_alias.Public_key.find cctxt alias
      in
      let public_key =
        Option.map (fun (Bls12_381 pk : public_key) -> pk) pk_opt
      in
      let+ secret_key_uri =
        Client_keys.Aggregate_alias.Secret_key.find_opt cctxt alias
      in
      {alias; public_key_hash; public_key; secret_key_uri})

let wallet_param ?(name = "an alias for a tz4 address")
    ?(desc = "an alias for a tz4 address") =
  param ~name ~desc @@ wallet_parameter ()

let bls_pkh_parameter () =
  parameter
    ~autocomplete:Client_keys.Aggregate_alias.Public_key_hash.autocomplete
    (fun cctxt s ->
      let open Lwt_result_syntax in
      let from_alias s =
        let* (Bls12_381 pkh) =
          Client_keys.Aggregate_alias.Public_key_hash.find cctxt s
        in
        return pkh
      in
      let from_key s = Bls.Public_key_hash.of_b58check s |> Lwt.return in
      alias_or_literal ~from_alias ~from_key s)

let conv_bls_pkh_to_l2_addr pkh =
  Bls.Public_key_hash.to_b58check pkh |> Tx_rollup_l2_address.of_b58check_exn

let bls_pkh_param ?(name = "public key hash")
    ?(desc = "bls public key hash to use") =
  let desc =
    String.concat
      "\n"
      [
        desc; "Can be an alias or a key.\nUse 'alias:name', 'key:name' to force.";
      ]
  in
  param
    ~name
    ~desc
    (map_parameter ~f:conv_bls_pkh_to_l2_addr (bls_pkh_parameter ()))

let bls_pk_parameter () =
  parameter
    ~autocomplete:Client_keys.Aggregate_alias.Public_key.autocomplete
    (fun cctxt s ->
      let open Lwt_result_syntax in
      let from_alias s =
        let* pk_opt = Client_keys.Aggregate_alias.Public_key.find cctxt s in
        match pk_opt with
        | (_pk_uri, Some (Bls12_381 pk)) -> return pk
        | (_pk_uri, None) -> failwith "it is not a valid bls public key"
      in
      let from_key s = Bls.Public_key.of_b58check s |> Lwt.return in
      alias_or_literal ~from_alias ~from_key s)

let bls_pk_param ?(name = "public key") ?(desc = "Bls public key to use.") =
  let desc =
    String.concat
      "\n"
      [
        desc; "Can be an alias or a key.\nUse 'alias:name', 'key:name' to force.";
      ]
  in
  param ~name ~desc (bls_pk_parameter ())

let bls_sk_uri_parameter () =
  parameter
    ~autocomplete:Client_keys.Aggregate_alias.Secret_key.autocomplete
    Client_keys.Aggregate_alias.Secret_key.find

let bls_sk_uri_param ?(name = "secret key") ?(desc = "Bls secret key to use.") =
  let desc =
    String.concat
      "\n"
      [
        desc; "Can be an alias or a key.\nUse 'alias:name', 'key:name' to force.";
      ]
  in
  param ~name ~desc (bls_sk_uri_parameter ())

let signature_parameter () =
  parameter (fun _cctxt s -> Bls.of_b58check s |> Lwt.return)

let signature_arg =
  arg
    ~doc:"aggregated signature"
    ~long:"aggregated-signature"
    ~placeholder:"current aggregated signature"
    (signature_parameter ())

let transaction_parameter =
  map_parameter
    ~f:(fun json ->
      try
        Data_encoding.Json.destruct
          Tx_rollup_l2_batch.V1.transaction_encoding
          json
      with Data_encoding.Json.Cannot_destruct (_path, exn) ->
        Stdlib.failwith
          (Format.asprintf
             "Invalid JSON for a message result path: %a"
             (fun ppf -> Data_encoding.Json.print_error ppf)
             exn))
    json_parameter

let transaction_param next =
  param ~name:"transaction" ~desc:"Transaction" transaction_parameter next

let l2_transaction_parameter =
  map_parameter
    ~f:(fun json ->
      try Data_encoding.Json.destruct L2_transaction.encoding json
      with Data_encoding.Json.Cannot_destruct (_path, exn) ->
        Stdlib.failwith
          (Format.asprintf
             "Invalid JSON for a l2 transaction: %a"
             (fun ppf -> Data_encoding.Json.print_error ppf)
             exn))
    json_parameter

let l2_transaction_param next =
  param
    ~name:"signed l2 transaction"
    ~desc:
      "Signed l2 transaction. Must be a valid json with the following format: \
       {\"transaction\": <transaction>; \"signatures\": [<list of signature>]}"
    l2_transaction_parameter
    next

let block_id_param =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match RPC.destruct_block_id s with
      | Ok v -> return v
      | Error e -> failwith "%s" e)

let ticket_hash_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Alpha_context.Ticket_hash.of_b58check_opt s with
      | Some tkh -> return tkh
      | None -> failwith "cannot parse %s to get a valid ticket_hash" s)

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
    @@ bls_pkh_param
         ~name:"tz4"
         ~desc:"tz4 address from which the balance is queried"
    @@ prefixes ["of"]
    @@ param
         ~name:"ticket-hash"
         ~desc:"ticket from which the balance is expected"
         ticket_hash_parameter
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

let craft_transfers ~counter ~signer transfers =
  let contents =
    List.map
      (fun (qty, destination, ticket_hash) ->
        Tx_rollup_l2_batch.V1.(
          Transfer
            {
              destination = Indexable.from_value destination;
              ticket_hash = Indexable.from_value ticket_hash;
              qty;
            }))
      transfers
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents}

let craft_tx ~counter ~signer ~destination ~ticket_hash ~qty =
  craft_transfers ~counter ~signer [(qty, destination, ticket_hash)]

let sign_transaction cctxt sks_uri txs =
  let open Lwt_result_syntax in
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  List.map_ep
    (fun sk ->
      let* signature = Client_keys.aggregate_sign cctxt sk buf in
      match signature with
      | Bls12_381 signature -> return signature
      | Unknown _signature -> failwith "failed to sign")
    sks_uri

let craft_withdraw ~counter ~signer ~destination ~ticket_hash ~qty =
  let content =
    Tx_rollup_l2_batch.V1.(Withdraw {destination; ticket_hash; qty})
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents = [content]}

let aggregate_signature signatures =
  let open Result_syntax in
  match Bls.aggregate_signature_opt signatures with
  | Some res -> return res
  | None -> error_with "aggregate_signature"

let craft_batch ~transactions =
  let open Result_syntax in
  let (transactions, signatures) =
    List.split
      (List.map
         (fun L2_transaction.{transaction; signatures} ->
           (transaction, signatures))
         transactions)
  in
  let* aggregated_signature = aggregate_signature (List.concat signatures) in
  return Tx_rollup_l2_batch.V1.{aggregated_signature; contents = transactions}

let conv_qty =
  parameter (fun _ qty ->
      match Tx_rollup_l2_qty.of_string qty with
      | Some qty -> return qty
      | None -> failwith "The given qty is invalid")

let conv_counter = parameter (fun _ counter -> return (Int64.of_string counter))

let signer_next_counter cctxt signer_pk counter =
  match counter with
  | Some c -> return c
  | None ->
      (* Retrieve the counter of the current head and increments it
         by one. *)
      (match RPC.destruct_block_id "head" with
      | Ok v -> return v
      | Error e ->
          let pkh_str =
            Bls12_381.Signature.MinPk.pk_to_bytes signer_pk
            |> Data_encoding.Binary.of_bytes_exn
                 Tezos_crypto.Bls.Public_key.encoding
            |> Tezos_crypto.Bls.Public_key.to_b58check
          in
          failwith "Cannot get counter of %s (%s)" pkh_str e)
      >>=? fun head ->
      let pkh = Tx_rollup_l2_address.of_bls_pk signer_pk in
      RPC.counter cctxt head pkh >>=? fun counter -> return (Int64.succ counter)

let craft_tx_transfers () =
  command
    ~desc:"WIP: craft a transaction with transfers"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "transfers"; "from"]
    @@ bls_pk_param ~name:"signer_pk" ~desc:"public key of the signer"
    @@ prefix "using"
    @@ param
         ~name:"transfers.json"
         ~desc:
           "List of transfers from the signer in JSON format (from a file or \
            directly inlined). The input JSON must be an array of objects of \
            the form '[ {\"destination\": dst, \"qty\" : val, \"ticket\" : \
            ticket_hash} ]'"
         json_file_or_text_parameter
    @@ stop)
    (fun counter
         signer_pk
         transfers_json
         (cctxt : #Configuration.tx_client_context) ->
      let transfers_encoding =
        let open Data_encoding in
        let transfer_encoding =
          obj3
            (req "qty" string)
            (req "destination" string)
            (req "ticket" string)
        in
        list transfer_encoding
      in
      match Data_encoding.Json.destruct transfers_encoding transfers_json with
      | [] -> failwith "Empty transfer list"
      | transfers ->
          let transfers =
            List.map
              (fun (qty, destination, ticket) ->
                ( Int64.of_string qty |> Tx_rollup_l2_qty.of_int64_exn,
                  Tx_rollup_l2_address.of_b58check_exn destination,
                  Alpha_context.Ticket_hash.of_b58check_exn ticket ))
              transfers
          in
          signer_next_counter cctxt signer_pk counter >>=? fun counter ->
          let signer = Tx_rollup_l2_batch.Bls_pk signer_pk in
          let op = craft_transfers ~counter ~signer transfers in
          let json =
            Data_encoding.Json.construct
              Tx_rollup_l2_batch.V1.transaction_encoding
              [op]
          in
          cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
          return_unit)

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
    @@ bls_pk_param ~name:"signer_pk" ~desc:"public key of the signer"
    @@ prefixes ["to"]
    @@ bls_pkh_param ~name:"dest" ~desc:"tz4 destination address"
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to transfer" ticket_hash_parameter
    @@ stop)
    (fun counter
         qty
         signer_pk
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      signer_next_counter cctxt signer_pk counter >>=? fun counter ->
      let signer = Tx_rollup_l2_batch.Bls_pk signer_pk in
      let op = craft_tx ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.V1.transaction_encoding
          [op]
      in
      cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
      return_unit)

let craft_tx_withdrawal () =
  command
    ~desc:"WIP: craft a withdrawal from L2 to L1"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "withdrawing"]
    @@ param ~name:"qty" ~desc:"qty to withdraw" conv_qty
    @@ prefixes ["from"]
    @@ bls_pk_param ~name:"signer_pk" ~desc:"public key of the signer"
    @@ prefixes ["to"]
    @@ param
         ~name:"dest"
         ~desc:"L1 destination address"
         l1_destination_parameter
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to withdraw" ticket_hash_parameter
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
      let op = craft_withdraw ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.V1.transaction_encoding
          [op]
      in
      cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
      return_unit)

let craft_tx_batch () =
  command
    ~desc:"craft a batch from a list of signed layer-2 transactions"
    (args1
       (switch
          ~doc:"Bytes representation of the batch encoded in hexadecimal"
          ~long:"bytes"
          ()))
    (prefixes ["craft"; "batch"; "with"] @@ seq_of_param l2_transaction_param)
    (fun show_bytes transactions (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let*? batch = craft_batch ~transactions in
      if show_bytes then
        let bytes =
          Data_encoding.Binary.to_bytes_exn
            Tx_rollup_l2_batch.encoding
            (Tx_rollup_l2_batch.V1 batch)
        in
        let*! () = cctxt#message "@[%a@]" Hex.pp (Hex.of_bytes bytes) in
        return_unit
      else
        let json =
          Data_encoding.Json.construct
            Tx_rollup_l2_batch.encoding
            (Tx_rollup_l2_batch.V1 batch)
        in
        let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
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
  parameter (fun _ s ->
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
    @@ l2_transaction_param @@ stop)
    (fun () transaction_and_sig (cctxt : #Configuration.tx_client_context) ->
      RPC.inject_transaction cctxt transaction_and_sig >>=? fun txh ->
      let json =
        Data_encoding.(Json.construct L2_transaction.Hash.encoding txh)
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
      return_unit)

let transfer () =
  let open Lwt_result_syntax in
  command
    ~desc:"submit a layer-2 transfer to a rollup node’s batcher"
    (args1
       (arg
          ~long:"counter"
          ~short:'c'
          ~placeholder:"counter"
          ~doc:"The counter associated to the signer address"
          conv_counter))
    (prefix "transfer"
    @@ param ~name:"qty" ~desc:"quantity to transfer" conv_qty
    @@ prefix "of"
    @@ param ~name:"ticket" ~desc:"A ticket hash" ticket_hash_parameter
    @@ prefix "from"
    @@ wallet_param ~name:"source"
    @@ prefix "to"
    @@ bls_pkh_param
         ~name:"destination"
         ~desc:"A BLS public key hash or an alias"
    @@ stop)
    (fun counter qty ticket_hash signer destination cctxt ->
      let open Tx_rollup_l2_batch in
      let open Tx_rollup_l2_batch.V1 in
      let*? signer_pk =
        match signer.public_key with
        | Some pk -> ok pk
        | None -> error_with "missing signer public key in the wallet"
      in
      let signer_addr = Tx_rollup_l2_address.of_bls_pk signer_pk in
      let* counter =
        match counter with
        | Some counter -> return counter
        | None ->
            let+ counter = RPC.counter cctxt `Head signer_addr in
            Int64.succ counter
      in
      let*? sk_uri =
        match signer.secret_key_uri with
        | None -> error_with "missing secret key in wallet"
        | Some sk_uri -> ok sk_uri
      in
      (* For the very first operation sent by a given account, we need
         to provide the full public key; otherwise, sending the public
         key hash is fine. *)
      let signer =
        if Compare.Int64.(counter = 1L) then Bls_pk signer_pk
        else L2_addr signer_addr
      in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let signer = Indexable.from_value signer in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let destination = Indexable.from_value destination in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let ticket_hash = Indexable.from_value ticket_hash in
      let contents = [Transfer {destination; ticket_hash; qty}] in
      let operation = Tx_rollup_l2_batch.V1.{counter; signer; contents} in
      let transaction = [operation] in
      let* signatures = sign_transaction cctxt [sk_uri] transaction in
      let* hash = RPC.inject_transaction cctxt {transaction; signatures} in
      let*! () =
        cctxt#message "Transaction hash: %a" L2_transaction.Hash.pp hash
      in
      return_unit)

let sign_transaction () =
  command
    ~desc:"sign a transaction"
    (args2
       (switch ~doc:"aggregate signature" ~long:"aggregate" ())
       signature_arg)
    (prefixes ["sign"; "transaction"]
    @@ transaction_param @@ prefix "with"
    @@ seq_of_param bls_sk_uri_param)
    (fun (aggregate, aggregated_signature)
         transactions
         sks_uri
         (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* signatures = sign_transaction cctxt sks_uri transactions in
      if aggregate then
        let signatures =
          match aggregated_signature with
          | Some aggregated_signature -> aggregated_signature :: signatures
          | None -> signatures
        in
        let*? aggregated_signature = aggregate_signature signatures in

        let*! () = cctxt#message "@[%a@]" Bls.pp aggregated_signature in
        return_unit
      else
        let*! () =
          cctxt#message
            "@[%a@]"
            (Format.pp_print_list
               ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";")
               Bls.pp)
            signatures
        in
        return_unit)

let all () =
  [
    get_tx_address_balance_command ();
    get_tx_inbox ();
    get_tx_block ();
    craft_tx_transaction ();
    craft_tx_transfers ();
    craft_tx_withdrawal ();
    craft_tx_batch ();
    sign_transaction ();
    get_batcher_queue ();
    get_batcher_transaction ();
    inject_batcher_transaction ();
    transfer ();
  ]
