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
  let open Lwt_syntax in
  let* contents = Lwt_utils_unix.read_file path in
  parse contents

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
      let* _, pk_opt =
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

let non_negative_param =
  Clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

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
      let open Lwt_result_syntax in
      let* value = RPC.balance cctxt block ticket tz4 in
      let*! () = cctxt#message "@[%a@]" Tx_rollup_l2_qty.pp value in
      return_unit)

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
      let open Lwt_result_syntax in
      let* inbox = RPC.inbox cctxt block in
      let json = Data_encoding.(Json.construct (option Inbox.encoding)) inbox in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let get_tx_block () =
  command
    ~desc:"returns the tx rollup block for a given block identifier"
    no_options
    (prefixes ["get"; "block"]
    @@ param ~name:"block" ~desc:"block requested" block_id_param
    @@ stop)
    (fun () block (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* block = RPC.block cctxt block in
      let json =
        Data_encoding.(Json.construct (option RPC.Encodings.block)) block
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
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
  let transactions, signatures =
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

let signer_to_address : Tx_rollup_l2_batch.signer -> Tx_rollup_l2_address.t =
  function
  | Bls_pk pk -> Tx_rollup_l2_address.of_bls_pk pk
  | L2_addr addr -> addr

let signer_next_counter cctxt signer counter =
  let open Lwt_result_syntax in
  match counter with
  | Some counter -> return counter
  | None ->
      (* Retrieve the counter of the current head and increments it
         by one. *)
      let* counter = RPC.counter cctxt `Head @@ signer_to_address signer in
      return (Int64.succ counter)

let signer_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Tx_rollup_l2_address.of_b58check_opt s with
      | Some pkh -> return @@ Tx_rollup_l2_batch.L2_addr pkh
      | None -> (
          match Bls.Public_key.of_b58check_opt s with
          | Some pk -> return @@ Tx_rollup_l2_batch.Bls_pk pk
          | None -> failwith "cannot parse %s to get a valid signer" s))

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
    @@ param
         ~name:"signer"
         ~desc:"public key or public key hash of the signer"
         signer_parameter
    @@ prefix "using"
    @@ param
         ~name:"transfers.json"
         ~desc:
           "List of transfers from the signer in JSON format (from a file or \
            directly inlined). The input JSON must be an array of objects of \
            the form '[ {\"destination\": dst, \"qty\" : val, \"ticket_hash\" \
            : ticket_hash} ]'"
         json_file_or_text_parameter
    @@ stop)
    (fun counter
         signer
         transfers_json
         (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let transfers_encoding =
        let open Data_encoding in
        let transfer_encoding =
          obj3
            (req "qty" string)
            (req "destination" string)
            (req "ticket_hash" string)
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
          let* counter = signer_next_counter cctxt signer counter in
          let op = craft_transfers ~counter ~signer transfers in
          let json =
            Data_encoding.Json.construct
              Tx_rollup_l2_batch.V1.transaction_encoding
              [op]
          in
          let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
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
    @@ param
         ~name:"signer"
         ~desc:"public key or public key hash of the signer"
         signer_parameter
    @@ prefixes ["to"]
    @@ bls_pkh_param ~name:"dest" ~desc:"tz4 destination address"
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to transfer" ticket_hash_parameter
    @@ stop)
    (fun counter
         qty
         signer
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* counter = signer_next_counter cctxt signer counter in
      let op = craft_tx ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.V1.transaction_encoding
          [op]
      in
      let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
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
    @@ param
         ~name:"signer"
         ~desc:"public key or public key hash of the signer"
         signer_parameter
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
         signer
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* counter = signer_next_counter cctxt signer counter in
      let op = craft_withdraw ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.V1.transaction_encoding
          [op]
      in
      let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
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
      let open Lwt_result_syntax in
      let* queue = RPC.get_queue cctxt in
      let json =
        Data_encoding.(Json.construct (list L2_transaction.encoding) queue)
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
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
      let open Lwt_result_syntax in
      let* tx = RPC.get_transaction cctxt hash in
      let json =
        Data_encoding.(Json.construct (option L2_transaction.encoding)) tx
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let inject_batcher_transaction () =
  command
    ~desc:"injects the given transaction into the batcher's transaction queue"
    no_options
    (prefixes ["inject"; "batcher"; "transaction"]
    @@ l2_transaction_param @@ stop)
    (fun () transaction_and_sig (cctxt : #Configuration.tx_client_context) ->
      let open Lwt_result_syntax in
      let* txh = RPC.inject_transaction cctxt transaction_and_sig in
      let json =
        Data_encoding.(Json.construct L2_transaction.Hash.encoding txh)
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let prepare_operation_parameters cctxt signer counter =
  let open Tx_rollup_l2_batch in
  let open Lwt_result_syntax in
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

  return (signer, sk_uri, counter)

let transfer () =
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
      let open Lwt_result_syntax in
      let open Tx_rollup_l2_batch.V1 in
      let* signer, sk_uri, counter =
        prepare_operation_parameters cctxt signer counter
      in
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

let withdraw () =
  command
    ~desc:"submit a layer-2 withdraw to a rollup node’s batcher"
    (args1
       (arg
          ~long:"counter"
          ~short:'c'
          ~placeholder:"counter"
          ~doc:"The counter associated to the signer address"
          conv_counter))
    (prefix "withdraw"
    @@ param ~name:"qty" ~desc:"quantity to withdraw" conv_qty
    @@ prefix "of"
    @@ param ~name:"ticket" ~desc:"A ticket hash" ticket_hash_parameter
    @@ prefix "from"
    @@ wallet_param ~name:"source" ~desc:"An alias for a tz4 address"
    @@ prefix "to"
    @@ param
         ~name:"destination"
         ~desc:"A L1 public key hash"
         l1_destination_parameter
    @@ stop)
    (fun counter qty ticket_hash signer destination cctxt ->
      let open Lwt_result_syntax in
      let open Tx_rollup_l2_batch.V1 in
      let* signer, sk_uri, counter =
        prepare_operation_parameters cctxt signer counter
      in
      let contents = [Withdraw {destination; ticket_hash; qty}] in
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

let display_answer (cctxt : #Configuration.tx_client_context) :
    RPC_context.generic_call_result -> unit Lwt.t = function
  | `Json (`Ok json) -> cctxt#answer "%a" Json_repr.(pp (module Ezjsonm)) json
  | `Binary (`Ok binary) -> cctxt#answer "%a" Hex.pp (Hex.of_string binary)
  | `Json (`Error (Some error)) ->
      cctxt#error
        "@[<v 2>Command failed: @[%a@]@]@."
        (Format.pp_print_list Error_monad.pp)
        (Data_encoding.Json.destruct
           (Data_encoding.list Error_monad.error_encoding)
           error)
  | `Binary (`Error (Some error)) -> (
      match Data_encoding.Binary.of_string Error_monad.trace_encoding error with
      | Ok trace ->
          cctxt#error
            "@[<v 2>Command failed: @[%a@]@]@."
            Error_monad.pp_print_trace
            trace
      | Error msg ->
          cctxt#error
            "@[<v 2>Error whilst decoding the server response: @[%a@]@]@."
            Data_encoding.Binary.pp_read_error
            msg)
  | `Json (`Not_found _) | `Binary (`Not_found _) | `Other (_, `Not_found _) ->
      cctxt#error "No service found at this URL\n%!"
  | `Json (`Gone _) | `Binary (`Gone _) | `Other (_, `Gone _) ->
      cctxt#error
        "Requested data concerns a pruned block and target resource is no \
         longer available\n\
         %!"
  | `Json (`Unauthorized _)
  | `Binary (`Unauthorized _)
  | `Other (_, `Unauthorized _) ->
      cctxt#error "@[<v 2>[HTTP 403] Access denied to: %a@]@." Uri.pp cctxt#base
  | _ -> cctxt#error "Unexpected server answer\n%!"

let call ?body meth raw_url (cctxt : #Configuration.tx_client_context) =
  let open Lwt_result_syntax in
  let uri = Uri.of_string raw_url in
  let body =
    (* This code is similar to a piece of code in [fill_in]
       function. An RPC is declared as POST, PATCH or PUT, but the
       body is not given. In that case, the body should be an empty
       JSON object. *)
    match (meth, body) with
    | _, Some _ -> body
    | `DELETE, None | `GET, None -> None
    | `PATCH, None | `PUT, None | `POST, None -> Some (`O [])
  in
  let* answer = cctxt#generic_media_type_call ?body meth uri in
  let*! () = display_answer cctxt answer in
  return_unit

let call_with_json meth raw_url json (cctxt : #Configuration.tx_client_context)
    =
  match Data_encoding.Json.from_string json with
  | exception Assert_failure _ ->
      (* Ref : https://github.com/mirage/ezjsonm/issues/31 *)
      cctxt#error "Failed to parse the provided json: unwrapped JSON value.\n%!"
  | Error err -> cctxt#error "Failed to parse the provided json: %s\n%!" err
  | Ok body -> call meth ~body raw_url cctxt

let call_with_file_or_json meth url maybe_file
    (cctxt : #Configuration.tx_client_context) =
  let open Lwt_result_syntax in
  let* json =
    match TzString.split ':' ~limit:1 maybe_file with
    | ["file"; filename] ->
        Lwt.catch
          (fun () ->
            Lwt_result.ok @@ Lwt_io.(with_file ~mode:Input filename read))
          (fun exn -> failwith "cannot read file (%s)" (Printexc.to_string exn))
    | _ -> return maybe_file
  in
  call_with_json meth url json cctxt

let rpc_commands () =
  let group =
    {Clic.name = "rpc"; title = "Commands for the low level RPC layer"}
  in
  [
    command
      ~group
      ~desc:"Call an RPC with the GET method."
      no_options
      (prefixes ["rpc"; "get"] @@ string ~name:"url" ~desc:"the RPC URL" @@ stop)
      (fun () -> call `GET);
    command
      ~group
      ~desc:"Call an RPC with the POST method."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ stop)
      (fun () -> call `POST);
    command
      ~group
      ~desc:
        "Call an RPC with the POST method, providing input data via the \
         command line."
      no_options
      (prefixes ["rpc"; "post"]
      @@ string ~name:"url" ~desc:"the RPC URL"
      @@ prefix "with"
      @@ string
           ~name:"input"
           ~desc:
             "the raw JSON input to the RPC\n\
              For instance, use `{}` to send the empty document.\n\
              Alternatively, use `file:path` to read the JSON data from a file."
      @@ stop)
      (fun () -> call_with_file_or_json `POST);
  ]

let get_message_proof () =
  let open Lwt_result_syntax in
  command
    ~desc:
      "returns the proof for a given block identifier and a message position \
       for the according inbox"
    no_options
    (prefixes ["get"; "proof"; "for"; "message"; "at"; "position"]
    @@ param
         ~name:"position"
         ~desc:"message position in the inbox"
         non_negative_param
    @@ prefixes ["in"; "block"]
    @@ param
         ~name:"block"
         ~desc:"block from which the message's proof is requested"
         block_id_param
    @@ stop)
    (fun () message_position block (cctxt : #Configuration.tx_client_context) ->
      RPC.get_message_proof cctxt block ~message_position >>=? fun proof ->
      let json =
        Data_encoding.(
          Json.construct (option Tx_rollup_l2_proof.encoding) proof)
      in
      cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) >>= fun () ->
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
    withdraw ();
    get_message_proof ();
  ]
  @ rpc_commands ()
