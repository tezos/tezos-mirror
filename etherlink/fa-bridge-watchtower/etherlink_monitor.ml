(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type ctx = {
  db : Sqlite.t;
  ws_client : Websocket_client.t;
  max_fee_per_gas : Z.t;
  public_key : Ethereum_types.address;
  sk : Libsecp256k1.External.Key.secret Libsecp256k1.External.Key.t;
  chain_id : L2_types.chain_id;
  gas_limit : Z.t;
}

module Craft = struct
  let prepare_and_forge_tx
      {max_fee_per_gas; sk; gas_limit; chain_id = L2_types.Chain_id chain_id; _}
      ?to_ ?data ~nonce ~value () =
    let open Efunc_core in
    let unsigned =
      Eth.
        {
          ti_max_priority_fee = Z.succ max_fee_per_gas;
          ti_max_fee = max_fee_per_gas;
          ti_value = value;
          ti_data = data;
          ti_chain_id = Z.to_int chain_id;
          ti_nonce = Z.to_int nonce;
          ti_gas_limit = Z.to_int gas_limit;
          ti_access_list = [];
          ti_signature = None;
          ti_to = to_;
          ti_max_fee_per_blob_gas = None;
          ti_blob_versioned_hashes = [];
          ti_blobs = [];
        }
    in
    let ti_signature =
      Some (Crypto.sign sk (Rope.to_string (Forge.transaction unsigned)))
    in
    Evm.of_rope @@ Forge.transaction {unsigned with ti_signature}

  let transfer ctx ~nonce ?to_ ?data ~value () =
    let txn = prepare_and_forge_tx ctx ?to_ ?data ~value ~nonce () in
    Ethereum_types.Hex (txn :> string)
end

module Tx_queue = struct
  include Tx_queue

  let ( let**? ) v f =
    let open Lwt_result_syntax in
    match v with Ok v -> f v | Error err -> return (Error err)

  (* as found in etherlink/bin_floodgate/tx_queue.ml *)
  let transfer ctx ?to_ ?(value = Z.zero) ~nonce ~data () =
    let txn = Craft.transfer ctx ~nonce ?to_ ~value ~data () in
    let tx_raw = Ethereum_types.hex_to_bytes txn in
    let hash = Ethereum_types.hash_raw_tx tx_raw in
    let**? tx = Transaction.decode tx_raw in
    let**? tx_object = Transaction.to_transaction_object ~hash tx in
    inject tx_object txn ~next_nonce:(Ethereum_types.Qty nonce)
end

module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

let system_address =
  Ethereum_types.Address (Hex "0000000000000000000000000000000000000000")

let kecack_topic s =
  let (`Hex h) =
    Tezos_crypto.Hacl.Hash.Keccak_256.digest (Bytes.of_string s) |> Hex.of_bytes
  in
  Ethereum_types.Hash (Hex h)

let extract_32 i ?padding data =
  let start = i * 32 in
  let start, length =
    match padding with
    | None -> (start, 32)
    | Some (`Left_padded l) -> (start + (32 - l), l)
    | Some (`Right_padded l) -> (start, l)
  in
  Bytes.sub data start length

let extract_32_end i ?padding data =
  let start = Bytes.length data - ((i + 1) * 32) in
  let start, length =
    match padding with
    | None -> (start, 32)
    | Some (`Left_padded l) -> (start + (32 - l), l)
    | Some (`Right_padded l) -> (start, l)
  in
  Bytes.sub data start length

let mk_filter address topic =
  let bloom =
    Filter_helpers.make_bloom_address_topics
      (Some (Vec [system_address; address]))
      (Some [Some (One topic)])
  in
  Filter_helpers.
    {bloom; address = [system_address; address]; topics = [Some (One topic)]}

module Event = struct
  include Internal_event.Simple

  let section = ["fa_bridge_watchtower"; "etherlink"]

  let transaction_log =
    declare_1
      ~section
      ~name:"transaction_log"
      ~msg:"Received withdrawal log {log}"
      ~level:Debug
      ("log", Ethereum_types.transaction_log_encoding)

  let deposit_log =
    declare_7
      ~section
      ~name:"deposit_log"
      ~msg:
        "Deposit {nonce}: {amount} {token} to {receiver} in transaction \
         {transactionHash}({transactionIndex}) of block {blockNumber}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ("amount", Data_encoding.float)
      ("token", Data_encoding.string)
      ("receiver", Ethereum_types.address_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("transactionIndex", Db.quantity_hum_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp3:Format.pp_print_string
      ~pp4:(fun fmt a ->
        Format.pp_print_string fmt (Ethereum_types.Address.to_string a))
      ~pp5:Ethereum_types.pp_hash
      ~pp6:Ethereum_types.pp_quantity
      ~pp7:Ethereum_types.pp_quantity

  let emit_deposit_log ws_client (d : Db.deposit) (l : Db.log_info) =
    let open Lwt_result_syntax in
    let* amount, symbol =
      Token_info.get_for_display ws_client d.proxy d.amount
    in
    Lwt_result.ok
    @@ emit
         deposit_log
         ( d.nonce,
           amount,
           symbol,
           d.receiver,
           l.transactionHash,
           l.transactionIndex,
           l.blockNumber )

  let unclaimed_deposits =
    declare_1
      ~section
      ~name:"unclaimed_deposits"
      ~msg:"There are {number} unclaimed deposits"
      ~level:Notice
      ("number", Data_encoding.int31)

  let claiming_deposit =
    declare_1
      ~section
      ~name:"claiming_deposit"
      ~msg:"Claiming deposit {nonce}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity

  let claimed_deposit =
    declare_4
      ~section
      ~name:"claimed_deposit"
      ~msg:
        "Claimed deposit {nonce} in transaction \
         {transactionHash}({transactionIndex}) of block {blockNumber}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("transactionIndex", Db.quantity_hum_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_hash
      ~pp3:Ethereum_types.pp_quantity
      ~pp4:Ethereum_types.pp_quantity

  let parsing_error =
    declare_1
      ~section
      ~name:"parsing_error"
      ~msg:"Deposit log parsing error: {error}"
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let tx_queue_error =
    declare_1
      ~section
      ~name:"tx_queue_error"
      ~msg:"Tx queue error: {error}"
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let tx_queue_beacon_error =
    declare_1
      ~section
      ~name:"tx_queue_beacon_error"
      ~msg:"Tx queue beacon error: {error}"
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let injection_error =
    declare_1
      ~section
      ~name:"injection_error"
      ~msg:"Injection error: {error}"
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let new_etherlink_head =
    declare_2
      ~section
      ~name:"new_etherlink_head"
      ~msg:"New etherlink head {level} ({txs} txs)"
      ~level:Notice
      ("level", Db.quantity_hum_encoding)
      ("txs", Data_encoding.int31)
      ~pp1:Ethereum_types.pp_quantity

  let catch_up =
    declare_1
      ~section
      ~name:"catch_up"
      ~msg:"Catching up on {levels} levels"
      ~level:Notice
      ("levels", Data_encoding.z)
      ~pp1:Z.pp_print

  let empty_l1_level =
    declare_1
      ~section
      ~name:"empty_l1_level"
      ~msg:"Empty L1 level {l1_level}"
      ~level:Info
      ("l1_level", Data_encoding.int32)

  let ws_reconnection =
    declare_1
      ~section
      ~name:"ws_reconnection"
      ~msg:"Disconnected from websocket, reconnecting in {delay}s."
      ~level:Error
      ("delay", Data_encoding.float)
end

type error +=
  | Too_many_deposits_in_one_block of {
      block : Ethereum_types.quantity;
      limit : int;
    }

let () =
  register_error_kind
    `Temporary
    ~id:"fa_bridge_watchtower.too_many_depositgs_in_one_block"
    ~title:"Too many deposits in one block"
    ~description:"Too many deposits in one block to fetch."
    ~pp:(fun ppf (block, limit) ->
      Format.fprintf
        ppf
        "There are more than %d deposits logs in the block %a. Change the \
         max_nb_logs config in the EVM node to recover."
        limit
        Ethereum_types.pp_quantity
        block)
    Data_encoding.(
      obj2
        (req "blockNumber" Db.quantity_hum_encoding)
        (req "limit" Data_encoding.int31))
    (function
      | Too_many_deposits_in_one_block {block; limit} -> Some (block, limit)
      | _ -> None)
    (fun (block, limit) -> Too_many_deposits_in_one_block {block; limit})

module Deposit = struct
  (*
    event QueuedEvent (
        uint256 nonce,
        address receiver,
        uint256 amount,
        uint256 inbox_level,
        uint256 inbox_msg_id,
    );

   topics  = keccak selector + ticket_hash + proxy
  *)

  type data = Db.deposit = {
    nonce : Ethereum_types.quantity;
    proxy : Ethereum_types.Address.t;
    ticket_hash : Ethereum_types.hash;
    receiver : Ethereum_types.Address.t;
    amount : Ethereum_types.quantity;
  }

  (* TODO: same precompile as withdrawals at first *)
  let address_hex = "ff00000000000000000000000000000000000002"

  let address_0x = "0x" ^ address_hex

  let address = Ethereum_types.Address (Hex address_hex)

  let topic =
    kecack_topic "QueuedDeposit(uint256,address,uint256,uint256,uint256)"

  let filter = mk_filter address topic

  let decode_event_data Ethereum_types.{topics; data = Hex hex_data; _} =
    let open Result_syntax in
    let* ticket_hash, proxy =
      match topics with
      | [_selector; th; Hash (Hex proxy)] ->
          let proxy =
            Hex.to_bytes_exn (`Hex proxy)
            |> extract_32 0 ~padding:(`Left_padded 20)
            |> Ethereum_types.decode_address
          in
          return (th, proxy)
      | _ ->
          error_with "Missing ticket hash and/or proxy from FA deposit topics"
    in
    let data = Hex.to_bytes (`Hex hex_data) in
    let* data =
      match data with
      | None -> error_with "Invalid hex data in deposit event"
      | Some d -> return d
    in
    let* () =
      if Bytes.length data < 3 * 32 then
        error_with "Invalid length for data of deposit event"
      else return_unit
    in
    let nonce = extract_32 0 data |> Ethereum_types.decode_number_be in
    let receiver =
      extract_32 1 data ~padding:(`Left_padded 20)
      |> Ethereum_types.decode_address
    in
    let amount = extract_32 2 data |> Ethereum_types.decode_number_be in
    return {nonce; proxy; ticket_hash; receiver; amount}
end

let parsed_log_to_db (log : Ethereum_types.transaction_log) event =
  match log with
  | {
   blockNumber = Some blockNumber;
   transactionHash = Some transactionHash;
   transactionIndex = Some transactionIndex;
   blockHash = Some blockHash;
   logIndex = Some logIndex;
   removed;
   _;
  } ->
      Some
        Db.
          {
            deposit = event;
            log_info =
              {
                transactionHash;
                transactionIndex;
                logIndex;
                blockHash;
                blockNumber;
                removed = Option.value removed ~default:false;
              };
            claimed = None;
          }
  | _ -> None

let parse_log (log : Ethereum_types.transaction_log) =
  let open Result_syntax in
  let* deposit_data = Deposit.decode_event_data log in
  return (parsed_log_to_db log deposit_data)

let precompiled_contract_address = Efunc_core.Private.a Deposit.address_hex

let claim ctx ~deposit_id =
  let open Lwt_result_syntax in
  let* (Ethereum_types.Qty nonce) =
    Websocket_client.send_jsonrpc
      ctx.ws_client
      (Call
         ( (module Rpc_encodings.Get_transaction_count),
           (ctx.public_key, Block_parameter Latest) ))
  in
  let data =
    Efunc_core.Evm.encode ~name:"claim" [`uint 256] [`int deposit_id]
  in
  let _ : unit Lwt.t =
    let open Lwt_syntax in
    let* res =
      Tx_queue.transfer ctx ~nonce ~to_:precompiled_contract_address ~data ()
    in
    match res with
    | Ok (Ok ()) -> return_unit
    | Error trace ->
        Format.kasprintf Event.(emit tx_queue_error) "%a" pp_print_trace trace
    | Ok (Error error) -> Event.(emit injection_error) error
  in
  return_unit

let handle_one_log {ws_client; db; _} (log : Ethereum_types.transaction_log) =
  let open Lwt_result_syntax in
  let*! () = Event.(emit transaction_log) log in
  let deposit = parse_log log in
  match deposit with
  | Error e ->
      let*! () =
        Format.kasprintf Event.(emit parsing_error) "%a" pp_print_trace e
      in
      fail e
  | Ok None ->
      let*! () =
        Event.(emit parsing_error) "Log did not match deposit filter"
      in
      return_unit
  | Ok (Some deposit) ->
      let* () =
        Event.emit_deposit_log ws_client deposit.deposit deposit.log_info
      in
      Db.Deposits.store db deposit.deposit deposit.log_info

let lwt_stream_iter_es f stream =
  let open Lwt_result_syntax in
  let rec loop () =
    let*! elt = Lwt_stream.get stream in
    match elt with
    | None -> return_unit
    | Some elt ->
        let* () = f elt in
        loop ()
  in
  loop ()

let filter_address = Ethereum_types.Filter.Vec [Deposit.address]

(* TODO: add filters on ticket hash and proxy for known contracts. *)
let filter_topics = [Some (Ethereum_types.Filter.One Deposit.topic)]

(** Retrieve log events that happened between [from_block] and [to_block] and
    register them in the DB. Uses a recursive strategy to handle the case where
    there are too many logs in the requested range.

    @param from_block Starting block number (inclusive)
    @param to_block Ending block number (inclusive)
*)
let rec get_logs ?(n = 1) ctx ~from_block ~to_block =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let Qty from_z, Qty to_z = (from_block, to_block) in
  if Z.Compare.(from_z > to_z) then
    (* There are no blocks for which to fetch logs *)
    return_unit
  else
    (* Query logs for the specified block range *)
    let*! logs =
      Websocket_client.send_jsonrpc
        ctx.ws_client
        (Call
           ( (module Rpc_encodings.Get_logs),
             Filter.
               {
                 from_block = Some (Number from_block);
                 to_block = Some (Number to_block);
                 address = Some filter_address;
                 topics = Some filter_topics;
                 block_hash = None;
               } ))
    in
    match logs with
    | Ok logs ->
        (* Process each log in the range *)
        List.iter_es
          (function
            | Filter.Block_filter _ | Pending_transaction_filter _ ->
                return_unit
            | Log log -> handle_one_log ctx log)
          logs
    | Error (Filter_helpers.Too_many_logs {limit} :: _ as e)
      when Z.equal from_z to_z ->
        (* If we're querying a single block and it has too many logs, this is a
           fatal error - the node's max_nb_logs config needs to be increased *)
        fail
          (TzTrace.cons
             (Too_many_deposits_in_one_block {block = from_block; limit})
             e)
    | Error (Filter_helpers.Too_many_logs _ :: _) ->
        (* If there are too many logs in the range, split it in half and try
           each half separately. This handles cases where the total number of
           logs exceeds the node's limit but each half is within bounds. *)
        let middle = Z.ediv (Z.add from_z to_z) (Z.of_int 2) in
        let* () = get_logs ctx ~from_block ~to_block:(Qty middle) in
        get_logs ctx ~from_block:(Qty (Z.succ middle)) ~to_block
    | Error _ when n < 10 ->
        (* It's possible for the `getLogs` request to fail if the receipt has not
           been stored yet. We retry at most 10 times to allow for the node to
           compute it. *)
        let*! () = Lwt_unix.sleep (0.1 *. float n) in
        get_logs ~n:(n + 1) ctx ~from_block ~to_block
    | Error e -> fail e

let claim_selector =
  (Efunc_core.Evm.method_id ~name:"claim" [`uint 256] :> string)

let is_claim_input = String.starts_with ~prefix:claim_selector

let handle_confirmed_txs {db; ws_client; _}
    (b : Transaction_object.t Ethereum_types.block) =
  let open Lwt_result_syntax in
  let* txs =
    match b.transactions with
    | TxFull [] | TxHash [] -> return_nil
    | TxFull txs -> return txs
    | TxHash _ -> (
        let* block =
          Websocket_client.send_jsonrpc
            ws_client
            (Call
               ( (module Rpc_encodings.Get_block_by_number),
                 (Number b.number, true) ))
        in
        match block.transactions with
        | TxHash _ -> assert false
        | TxFull txs -> return txs)
  in
  txs
  |> List.iteri_es @@ fun index tx ->
     let (Hex input) = Transaction_object.input tx in
     match Transaction_object.to_ tx with
     | Some to_
       when Ethereum_types.Address.compare to_ Deposit.address = 0
            && is_claim_input input ->
         let tx_hash = Transaction_object.hash tx in
         let input =
           Hex.to_string (`Hex input) |> WithExceptions.Option.get ~loc:__LOC__
         in
         let input_rope = Rope.of_string input in
         let value = Efunc_core.Evm.decode_value (`uint 256) (input_rope, 4) in
         let nonce =
           match value with
           | `int v -> Ethereum_types.quantity_of_z v
           | _ -> assert false
         in
         let exec =
           Db.
             {
               transactionHash = tx_hash;
               transactionIndex = Qty (Z.of_int index);
               blockHash = b.hash;
               blockNumber = b.number;
             }
         in
         let*! () =
           Event.(emit claimed_deposit)
             ( nonce,
               exec.transactionHash,
               exec.transactionIndex,
               exec.blockNumber )
         in
         let* () = Db.Deposits.set_claimed db nonce exec in
         let* () = Tx_queue.confirm tx_hash in
         return_unit
     | _ -> return_unit

let claim_deposits ctx =
  let open Lwt_result_syntax in
  let* deposits = Db.Deposits.get_unclaimed ctx.db in
  let*! () =
    let number = List.length deposits in
    if number > 0 then Event.(emit unclaimed_deposits) number
    else Lwt.return_unit
  in
  let* () =
    List.iter_es
      (fun deposit ->
        let (Qty deposit_id) = deposit.Db.nonce in
        let*! () = Event.(emit claiming_deposit) deposit.Db.nonce in
        claim ctx ~deposit_id)
      deposits
  in
  return_unit

let on_new_block ctx ~catch_up (b : _ Ethereum_types.block) =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let nb_txs =
    match b.transactions with
    | TxHash l -> List.length l
    | TxFull l -> List.length l
  in
  let*! () = Event.(emit new_etherlink_head) (b.number, nb_txs) in
  (* Process logs for this block *)
  let* () = get_logs ctx ~from_block:b.number ~to_block:b.number in
  (* Notify tx queue and register claimed deposits in DB *)
  let* () = handle_confirmed_txs ctx b in
  let* () = Db.Pointers.L2_head.set ctx.db b.number in
  unless catch_up @@ fun () -> claim_deposits ctx

let rec catch_up ctx ~from_block ~end_block =
  let open Lwt_result_syntax in
  let Ethereum_types.Qty from_, Ethereum_types.Qty end_ =
    (from_block, end_block)
  in
  if Z.gt from_ end_ then return_unit
  else
    let* block =
      Websocket_client.send_jsonrpc
        ctx.ws_client
        (Call
           ( (module Rpc_encodings.Get_block_by_number),
             (Number from_block, true) ))
    in
    let* () = on_new_block ctx block ~catch_up:true in
    catch_up ctx ~from_block:(Ethereum_types.Qty.next from_block) ~end_block

let monitor_heads ctx =
  let open Lwt_result_syntax in
  let* head =
    Websocket_client.send_jsonrpc
      ctx.ws_client
      (Call ((module Rpc_encodings.Get_block_by_number), (Latest, true)))
  and* heads_subscription = Websocket_client.subscribe_newHeads ctx.ws_client in
  let* () =
    lwt_stream_iter_es
      (fun (b : Transaction_object.t Ethereum_types.block) ->
        let* last_l2_head = Db.Pointers.L2_head.get ctx.db in
        let expected_level = Ethereum_types.Qty.next last_l2_head in
        let* () =
          unless Ethereum_types.Qty.(b.number = expected_level) @@ fun () ->
          catch_up
            ctx
            ~from_block:expected_level
            ~end_block:(Ethereum_types.Qty.pred b.number)
        in
        on_new_block ctx b ~catch_up:false)
      (Lwt_stream.append (Lwt_stream.return head) heads_subscription.stream)
  in
  return_unit

let init_db_pointers db ws_client ~first_block =
  let open Lwt_result_syntax in
  let* l2_head = Db.Pointers.L2_head.find db in
  let* () =
    match l2_head with
    | Some _ -> return_unit
    | None -> (
        match first_block with
        | Some first_block ->
            Db.Pointers.L2_head.set db (Ethereum_types.Qty.pred first_block)
        | None ->
            (* TODO: log *)
            let* latest =
              Websocket_client.send_jsonrpc
                ws_client
                (Call
                   ((module Rpc_encodings.Get_block_by_number), (Latest, false)))
            in
            Db.Pointers.L2_head.set db latest.number)
  in
  return_unit

let reconnection_delay = 10.

let get_chain_id ws_client =
  Websocket_client.send_jsonrpc
    ws_client
    (Call ((module Rpc_encodings.Chain_id), ()))

module Public_key = struct
  open Libsecp256k1.External

  type t = Key.public Key.t

  let ctxt = Efunc_core.Eth.Crypto.context ()

  let from_sk sk = Key.neuterize_exn ctxt sk

  let to_address pk =
    let addr = (Efunc_core.Crypto.to_address pk :> string) in
    Ethereum_types.Address (Ethereum_types.hex_of_string addr)
end

let start db ~evm_node_endpoint ~first_block ~secret_key ~max_fee_per_gas
    ~gas_limit =
  let open Lwt_result_syntax in
  let tx_queue_endpoint = ref (Tx_queue.Rpc evm_node_endpoint) in
  let run () =
    let*! ws_client =
      Websocket_client.connect
        ~monitoring:{ping_timeout = 60.; ping_interval = 10.}
        Media_type.json
        (Uri.with_path evm_node_endpoint (Uri.path evm_node_endpoint ^ "/ws"))
    in
    tx_queue_endpoint := Tx_queue.Websocket ws_client ;
    let* () = init_db_pointers db ws_client ~first_block in
    let* chain_id = get_chain_id ws_client in
    let public_key = Public_key.(to_address (from_sk secret_key)) in
    let ctx =
      {
        db;
        ws_client;
        max_fee_per_gas;
        public_key;
        sk = secret_key;
        chain_id;
        gas_limit;
      }
    in
    monitor_heads ctx
  in
  let rec loop ?(first = false) () =
    let* () =
      Lwt.catch run (function
          | Unix.(Unix_error (ECONNREFUSED, _, _)) when not first -> return_unit
          | e -> Lwt.reraise e)
    in
    let*! () = Event.(emit ws_reconnection) reconnection_delay in
    let*! () = Lwt_unix.sleep reconnection_delay in
    loop ()
  in
  let rec tx_queue_beacon () =
    let open Lwt_syntax in
    let* res =
      protect @@ fun () -> Tx_queue.tick ~evm_node_endpoint:!tx_queue_endpoint
    in
    let* () =
      match res with
      | Ok () -> return_unit
      | Error e ->
          Format.kasprintf Event.(emit tx_queue_error) "%a" pp_print_trace e
    in
    let*! () = Lwt_unix.sleep 0.05 in
    tx_queue_beacon ()
  in
  let* () =
    Tx_queue.start
      ~config:
        {
          max_size = 1000;
          max_transaction_batch_length = None;
          max_lifespan_s = 5;
          tx_per_addr_limit = 10000L;
        }
      ~keep_alive:true
      ()
  in
  Lwt.dont_wait tx_queue_beacon ignore ;
  loop ~first:true ()
