(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025-2026 Functori, <contact@functori.com>                  *)
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
  whitelist : Config.whitelist_item list option;
  mutable nonce : Ethereum_types.quantity;
  block_timeout : float;
  rpc_timeout : Websocket_client.timeout option;
}

module Craft = struct
  let strip_0x s =
    if String.starts_with ~prefix:"0x" s then
      String.sub s 2 (String.length s - 2)
    else s

  let amount_to_float_1e6 (Ethereum_types.Qty z) = Z.to_float z *. 1e-6

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

  let start, Services_backend_sig.Evm_tx_container tx_container =
    tx_container ~chain_family:EVM

  let ( let**? ) v f =
    let open Lwt_result_syntax in
    match v with Ok v -> f v | Error err -> return (Error err)

  (* as found in etherlink/bin_floodgate/tx_queue.ml *)
  let transfer ctx ?to_ ?(value = Z.zero) ~data () =
    let open Lwt_result_syntax in
    let (module Tx_container) = tx_container in
    let (Ethereum_types.Qty nonce as qnonce) = ctx.nonce in
    let txn = Craft.transfer ctx ~nonce ?to_ ~value ~data () in
    let tx_raw = Ethereum_types.hex_to_bytes txn in
    let*? tx_object = Transaction_object.decode tx_raw in
    let+ res = Tx_container.add tx_object ~raw_tx:txn ~next_nonce:qnonce in
    match res with
    | Ok _hash ->
        ctx.nonce <- Ethereum_types.Qty.next ctx.nonce ;
        Ok ()
    | Error _ as res -> res
end

module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

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

(* Helper to convert an Ethereum address to a topic format *)
let addr_to_topic address =
  let (Ethereum_types.Address (Hex addr_hex)) = address in
  let padded_hex = String.make (64 - String.length addr_hex) '0' ^ addr_hex in
  Ethereum_types.Hash (Hex padded_hex)

(** [mk_filter address selectors whitelist] creates an Ethereum log filter for
    events.

    This function builds a filter to match logs from a specific contract address
    with a specific event selector, optionally filtered by whitelist items.

    The filter's topics are constructed based on the whitelist configuration:
    - With no whitelist: only filter by the event selector
    - With a single whitelist item: filter by selector, ticket hashes,
      and proxy address
    - With multiple items that only have proxy addresses: filter by selector
      and a list of proxies
    - With multiple items that only have ticket hashes: filter by selector and
      a list of ticket hashes
    - With mixed items: filter by selector, all ticket hashes, and all proxy
      addresses

    @param address The contract address to filter logs from
    @param selectors The event selectors (keccak hashes of the events signatures)
    @param whitelist Optional list of whitelist items containing proxy
      addresses and ticket hashes
    @return A filter configuration for Ethereum log queries
*)
let mk_filter addresses selectors whitelist =
  (* First topic is always the event selector *)
  let selector_topic = Some (Ethereum_types.Filter.Or selectors) in
  (* Construct topics array based on whitelist configuration *)
  let topics =
    match whitelist with
    | None ->
        (* No whitelist: only filter by event selector *)
        [selector_topic]
    | Some [{Config.proxy; ticket_hashes}] ->
        (* Single whitelist item: create specific filter for this item *)
        let proxy_topic =
          Option.map
            (fun p -> Ethereum_types.Filter.One (addr_to_topic p))
            proxy
        in
        let ticket_hashes_topic =
          Option.map (fun th -> Ethereum_types.Filter.Or th) ticket_hashes
        in
        [selector_topic; ticket_hashes_topic; proxy_topic]
    | Some whitelist
      when List.for_all (fun w -> w.Config.ticket_hashes = None) whitelist ->
        (* Multiple items with only proxy addresses: filter by list of
           proxies *)
        let proxy_topics =
          List.filter_map
            (fun {Config.proxy; _} -> Option.map addr_to_topic proxy)
            whitelist
        in
        [selector_topic; None; Some (Or proxy_topics)]
    | Some whitelist
      when List.for_all (fun w -> w.Config.proxy = None) whitelist ->
        (* Multiple items with only ticket hashes: filter by list of ticket
           hashes *)
        let ticket_hashes_topics =
          List.concat_map
            (fun {Config.ticket_hashes; _} ->
              Option.value ticket_hashes ~default:[])
            whitelist
        in
        [selector_topic; Some (Or ticket_hashes_topics)]
    | Some whitelist ->
        (* Mixed items: filter by all proxies and all ticket hashes. This is
           inaccurate because we can only filter with a single Or for each
           topic. *)
        let proxy_topics =
          List.filter_map
            (fun {Config.proxy; _} -> Option.map addr_to_topic proxy)
            whitelist
        in
        let ticket_hashes_topics =
          List.concat_map
            (fun {Config.ticket_hashes; _} ->
              Option.value ticket_hashes ~default:[])
            whitelist
        in
        [selector_topic; Some (Or ticket_hashes_topics); Some (Or proxy_topics)]
  in
  (* Create bloom filter for efficient filtering *)
  let bloom =
    Filter_helpers.make_bloom_address_topics
      (Some (Vec addresses))
      (Some topics)
  in
  (* Return the complete filter configuration *)
  Filter_helpers.{bloom; address = addresses; topics}

module Event = struct
  include Internal_event.Simple

  let section = ["fa_bridge_watchtower"; "etherlink"]

  let started =
    declare_0
      ~section
      ~name:"bridge_watchtower_started"
      ~msg:"watchtower has been started"
      ~level:Notice
      ()

  let transaction_log =
    declare_1
      ~section
      ~name:"transaction_log"
      ~msg:"Received withdrawal log {log}"
      ~level:Debug
      ("log", Ethereum_types.transaction_log_encoding)

  let fa_deposit_log =
    declare_6
      ~section
      ~name:"fa_deposit_log"
      ~msg:
        "FA Deposit {nonce}: {amount} {token} to {receiver} in transaction \
         {transactionHash} of block {blockNumber}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ("amount", Data_encoding.float)
      ("token", Data_encoding.string)
      ("receiver", Ethereum_types.address_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp3:Format.pp_print_string
      ~pp4:(fun fmt a ->
        Format.pp_print_string fmt (Ethereum_types.Address.to_string a))
      ~pp5:Ethereum_types.pp_hash
      ~pp6:Ethereum_types.pp_quantity

  let xtz_deposit_log =
    declare_5
      ~section
      ~name:"xtz_deposit_log"
      ~msg:
        "XTZ Deposit {nonce}: {amount} to {receiver} in transaction \
         {transactionHash} of block {blockNumber}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ("amount", Data_encoding.float)
      ("receiver", Ethereum_types.address_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp3:(fun fmt a ->
        Format.pp_print_string fmt (Ethereum_types.Address.to_string a))
      ~pp4:Ethereum_types.pp_hash
      ~pp5:Ethereum_types.pp_quantity

  let emit_deposit_log ws_client (deposit : Db.deposit) (l : Db.log_info) =
    let open Lwt_result_syntax in
    match deposit.token with
    | XTZ ->
        Lwt_result.ok
        @@ emit
             xtz_deposit_log
             ( deposit.nonce,
               Craft.amount_to_float_1e6 deposit.amount,
               deposit.receiver,
               l.transactionHash,
               l.blockNumber )
    | FA {proxy; _} ->
        let* amount, symbol =
          Token_info.get_for_display ws_client proxy deposit.amount
        in
        Lwt_result.ok
        @@ emit
             fa_deposit_log
             ( deposit.nonce,
               amount,
               symbol,
               deposit.receiver,
               l.transactionHash,
               l.blockNumber )

  let unclaimed_deposits =
    declare_1
      ~section
      ~name:"unclaimed_deposits"
      ~msg:"There are {number} unclaimed deposits"
      ~level:Notice
      ("number", Data_encoding.int31)

  let claiming_deposit =
    declare_2
      ~section
      ~name:"claiming_deposit"
      ~msg:"Claiming {deposit_type} deposit {nonce}"
      ~level:Notice
      ("deposit_type", Data_encoding.string)
      ("nonce", Db.quantity_hum_encoding)
      ~pp1:Format.pp_print_string
      ~pp2:Ethereum_types.pp_quantity

  let claimed_deposit =
    declare_3
      ~section
      ~name:"claimed_deposit"
      ~msg:
        "Claimed deposit {nonce} in transaction {transactionHash} of block \
         {blockNumber}"
      ~level:Notice
      ("nonce", Db.quantity_hum_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_hash
      ~pp3:Ethereum_types.pp_quantity

  let claiming_deposit_status_fail =
    declare_3
      ~section
      ~name:"claiming_deposit_status_fail"
      ~msg:
        "Claiming deposit {nonce} transaction {transactionHash} failed with \
         status error {status}"
      ~level:Warning
      ("nonce", Db.quantity_hum_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("status", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_hash
      ~pp3:Ethereum_types.pp_quantity

  let claiming_deposit_receipt_not_found =
    declare_2
      ~section
      ~name:"claiming_deposit_receipt_not_found"
      ~msg:
        "Claiming deposit {nonce} transaction {transactionHash} receipt not \
         found"
      ~level:Warning
      ("nonce", Db.quantity_hum_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_hash

  let parsing_error =
    declare_1
      ~section
      ~name:"parsing_error"
      ~msg:"Deposit log parsing error: {error}"
      ~level:Error
      ("error", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let transfered_to_tx_queue =
    declare_0
      ~section
      ~name:"transfered_to_tx_queue"
      ~msg:"Claim was transfered to tx queue"
      ~level:Notice
      ()

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
    declare_1
      ~section
      ~name:"new_etherlink_head"
      ~msg:"New etherlink head {level}"
      ~level:Notice
      ("level", Db.quantity_hum_encoding)
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
      ~level:Warning
      ("delay", Data_encoding.float)

  let monitor_heads_timeout =
    declare_1
      ~section
      ~name:"monitor_heads_timeout"
      ~msg:
        "Timeout after {timeout}s while waiting for a new head. Disconnecting \
         from websocket."
      ~level:Warning
      ("timeout", Data_encoding.float)

  let monitor_error =
    declare_1
      ~section
      ~name:"monitor_error"
      ~msg:"Error in monitoring: {error}"
      ~level:Error
      ("error", trace_encoding)
      ~pp1:pp_print_top_error_of_trace
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

module Address = struct
  (* See [kernel]/revm/src/precompiles/constants.rs *)

  let xtz_bridge_hex = "ff00000000000000000000000000000000000001"

  let xtz_bridge_0x = "0x" ^ xtz_bridge_hex

  let xtz_bridge = Ethereum_types.Address (Hex xtz_bridge_hex)

  let fa_bridge_hex = "ff00000000000000000000000000000000000002"

  let fa_bridge_0x = "0x" ^ fa_bridge_hex

  let fa_bridge = Ethereum_types.Address (Hex fa_bridge_hex)

  let bridge_addresses = [xtz_bridge; fa_bridge]
end

module Deposit = struct
  type data = Db.deposit

  module Dionysus = struct
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

    let fa_topic =
      kecack_topic "QueuedDeposit(uint256,address,uint256,uint256,uint256)"
  end

  module Farfadet = struct
    (*
      event QueuedDeposit(
        uint256 indexed ticketHash,
        address indexed proxy,
        uint256 nonce,
        address receiver,
        uint256 amount,
        uint256 inboxLevel,
        uint256 inboxMsgId
      );
    *)

    let fa_topic =
      kecack_topic
        "QueuedDeposit(uint256,address,uint256,address,uint256,uint256,uint256)"
  end

  module FarfadetR2 = struct
    (*
      event QueuedDeposit(
          uint256 amount,
          uint256 nonce,
          address receiver,
          uint256 inbox_level,
          uint256 inbox_msg_id
      );
    *)

    let xtz_topic =
      kecack_topic "QueuedDeposit(uint256,uint256,address,uint256,uint256)"
  end

  let filter ~addresses whitelist =
    mk_filter
      addresses
      [Dionysus.fa_topic; Farfadet.fa_topic; FarfadetR2.xtz_topic]
      whitelist

  let whitelist_filter whitelist topics =
    let open Result_syntax in
    let* log_ticket_hash, log_proxy =
      match topics with
      | [_selector; th; Ethereum_types.Hash (Hex proxy)] ->
          let proxy =
            Hex.to_bytes_exn (`Hex proxy)
            |> extract_32 0 ~padding:(`Left_padded 20)
            |> Ethereum_types.decode_address
          in
          return (th, proxy)
      | _ ->
          error_with "Missing ticket hash and/or proxy from FA deposit topics"
    in
    let matched_by_whitelist =
      match whitelist with
      | None | Some [_] -> true
      | Some whitelist ->
          List.for_all
            (fun Config.{proxy; ticket_hashes} ->
              proxy = None || ticket_hashes = None)
            whitelist
          || List.exists
               (fun Config.{proxy; ticket_hashes} ->
                 (match proxy with
                 | None -> true
                 | Some p -> Ethereum_types.Address.equal log_proxy p)
                 &&
                 match ticket_hashes with
                 | None -> true
                 | Some ticket_hashes ->
                     List.mem
                       ~equal:Ethereum_types.equal_hash
                       log_ticket_hash
                       ticket_hashes)
               whitelist
    in
    if matched_by_whitelist then return_some (log_ticket_hash, log_proxy)
    else return_none

  let decode_xtz_event_data Ethereum_types.{topics; data = Hex hex_data; _} =
    let open Result_syntax in
    let* is_xtz_event_deposit =
      match topics with
      | [selector] -> Ok (selector = FarfadetR2.xtz_topic)
      | _ ->
          error_with
            "Something went wrong while parsing XTZ deposit event topics, \
             should contain exactly one topic"
    in
    if not is_xtz_event_deposit then return_none
    else
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
      let amount = extract_32 0 data |> Ethereum_types.decode_number_be in
      let nonce = extract_32 1 data |> Ethereum_types.decode_number_be in
      let receiver =
        extract_32 2 data ~padding:(`Left_padded 20)
        |> Ethereum_types.decode_address
      in
      return_some @@ Db.{nonce; token = XTZ; receiver; amount}

  let decode_fa_event_data whitelist
      Ethereum_types.{topics; data = Hex hex_data; _} =
    let open Result_syntax in
    let* th_proxy = whitelist_filter whitelist topics in
    match th_proxy with
    | None -> return_none
    | Some (ticket_hash, proxy) ->
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
        return_some
        @@ Db.{nonce; token = FA {proxy; ticket_hash}; receiver; amount}
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

let parse_log whitelist (log : Ethereum_types.transaction_log) =
  let open Result_syntax in
  let* deposit_data =
    if log.address = Address.xtz_bridge then Deposit.decode_xtz_event_data log
    else Deposit.decode_fa_event_data whitelist log
  in
  return (Option.map (parsed_log_to_db log) deposit_data)

let param_address address =
  let (Ethereum_types.Address (Hex address_hex)) = address in
  Efunc_core.Private.a (Craft.strip_0x address_hex)

let xtz_bridge_address = Efunc_core.Private.a Address.xtz_bridge_hex

let fa_bridge_address = Efunc_core.Private.a Address.fa_bridge_hex

let claim_fa_call_data deposit_id =
  Efunc_core.Evm.encode ~name:"claim" [`uint 256] [`int deposit_id]

let claim_xtz_call_data deposit_id =
  Efunc_core.Evm.encode ~name:"claim_xtz" [`uint 256] [`int deposit_id]

let claim ctx ~is_native ~deposit_id =
  let open Lwt_result_syntax in
  let _ : unit Lwt.t =
    let open Lwt_syntax in
    let data, bridge_address =
      if is_native then (claim_xtz_call_data deposit_id, Address.xtz_bridge)
      else (claim_fa_call_data deposit_id, Address.fa_bridge)
    in
    let* res =
      Tx_queue.transfer ctx ~to_:(param_address bridge_address) ~data ()
    in
    match res with
    | Ok (Ok ()) -> Event.(emit transfered_to_tx_queue) ()
    | Error trace ->
        Format.kasprintf Event.(emit tx_queue_error) "%a" pp_print_trace trace
    | Ok (Error error) -> Event.(emit injection_error) error
  in
  return_unit

let handle_one_log {ws_client; db; whitelist; _}
    (log : Ethereum_types.transaction_log) =
  let open Lwt_result_syntax in
  let*! () = Event.(emit transaction_log) log in
  let deposit = parse_log whitelist log in
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
  | Ok (Some None) ->
      (* Matched get_logs filter but not whitelist filter *)
      return_unit
  | Ok (Some (Some deposit)) ->
      let* () =
        Event.emit_deposit_log ws_client deposit.deposit deposit.log_info
      in
      Db.Deposits.store db deposit.deposit deposit.log_info

type lwt_stream_iter_with_timeout_ended = Closed | Timeout of float

type 'a lwt_stream_get_result =
  | Get_none
  | Get_timeout of float
  | Get_elt of 'a

let lwt_stream_iter_es_with_timeout ~timeout f stream =
  let open Lwt_syntax in
  let rec loop () =
    let get_promise =
      let+ res = Lwt_stream.get stream in
      match res with None -> Get_none | Some e -> Get_elt e
    in
    let timeout_promise =
      let+ () = Lwt_unix.sleep timeout in
      Get_timeout timeout
    in
    let* res = Lwt.pick [get_promise; timeout_promise] in
    match res with
    | Get_none -> return_ok Closed
    | Get_timeout t -> return_ok (Timeout t)
    | Get_elt elt -> (
        let* res = protect @@ fun () -> f elt in
        match res with
        | Ok () -> (loop [@ocaml.tailcall]) ()
        | Error trace -> return_error trace)
  in
  loop ()

(** Retrieve log events that happened in [block] and register them in the
    DB. Uses a recursive strategy to handle the case where there are too many
    logs in the requested range.

    @param ctx Context with websocket client
    @param block Block number in which to look for log events
*)
let rec get_logs ?(n = 1) ctx ~block =
  let open Lwt_result_syntax in
  (* Query logs for the specified block range *)
  let filter =
    Deposit.filter ~addresses:Address.bridge_addresses ctx.whitelist
  in
  let*! logs =
    Websocket_client.send_jsonrpc
      ?timeout:ctx.rpc_timeout
      ctx.ws_client
      (Call
         ( (module Rpc_encodings.Get_logs),
           Ethereum_types.Filter.
             {
               from_block = Some (Number block);
               to_block = Some (Number block);
               address = Some (Vec Address.bridge_addresses);
               topics = Some filter.topics;
               block_hash = None;
             } ))
  in
  match logs with
  | Ok logs ->
      (* Process each log in the range *)
      List.iter_es
        (fun log -> handle_one_log ctx (Ethereum_types.decode_pre log))
        logs
  | Error (Filter_helpers.Too_many_logs {limit} :: _ as e) ->
      (* If we're querying a single block and it has too many logs, this is a
         fatal error - the node's max_nb_logs config needs to be increased *)
      fail (TzTrace.cons (Too_many_deposits_in_one_block {block; limit}) e)
  | Error _ when n < 10 ->
      (* It's possible for the `getLogs` request to fail if the receipt has not
         been stored yet. We retry at most 10 times to allow for the node to
         compute it. *)
      let*! () = Lwt_unix.sleep (0.1 *. float n) in
      get_logs ~n:(n + 1) ctx ~block
  | Error e -> fail e

let claim_fa_selector =
  (Efunc_core.Evm.method_id ~name:"claim" [`uint 256] :> string)

let claim_xtz_selector =
  (Efunc_core.Evm.method_id ~name:"claim_xtz" [`uint 256] :> string)

let is_claim_input ~bridge_address =
  if bridge_address = Address.fa_bridge then
    String.starts_with ~prefix:claim_fa_selector
  else String.starts_with ~prefix:claim_xtz_selector

let handle_confirmed_txs {db; ws_client; rpc_timeout; _} number =
  let open Lwt_result_syntax in
  let* b =
    Websocket_client.send_jsonrpc
      ?timeout:rpc_timeout
      ws_client
      (Call ((module Rpc_encodings.Get_block_by_number), (Number number, true)))
  in
  let* txs =
    match b.transactions with
    | TxHash [] -> return []
    | TxHash _ -> assert false
    | TxFull txs -> return txs
  in
  txs
  |> List.iteri_es @@ fun index tx ->
     let (Hex input) = Transaction_object.input tx in
     match Transaction_object.to_ tx with
     | Some to_
       when List.mem
              ~equal:Ethereum_types.Address.equal
              to_
              Address.bridge_addresses
            && is_claim_input ~bridge_address:to_ input -> (
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
         let* receipt =
           Websocket_client.send_jsonrpc
             ?timeout:rpc_timeout
             ws_client
             (Call ((module Rpc_encodings.Get_transaction_receipt), tx_hash))
         in
         match receipt with
         (* when "status": "0x1" *)
         | Some {status = Qty z; _} when Z.equal z Z.one ->
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
                 (nonce, exec.transactionHash, exec.blockNumber)
             in
             let* () = Db.Deposits.set_claimed db nonce exec in
             let (module Tx_container) = Tx_queue.tx_container in
             let* () =
               Tx_container.confirm_transactions
                 ~clear_pending_queue_after:false
                 ~confirmed_txs:(Seq.cons tx_hash Seq.empty)
             in
             return_unit
         | Some {status; _} ->
             let*! () =
               Event.(emit claiming_deposit_status_fail) (nonce, tx_hash, status)
             in
             return_unit
         | None ->
             let*! () =
               Event.(emit claiming_deposit_receipt_not_found) (nonce, tx_hash)
             in
             return_unit)
     | None | Some _ -> return_unit

let claim_deposits ctx =
  let open Lwt_result_syntax in
  let* deposits = Db.Deposits.get_unclaimed ctx.db in
  match deposits with
  | [] -> return_unit
  | _ ->
      let*! () = Event.(emit unclaimed_deposits) (List.length deposits) in
      let* nonce =
        Websocket_client.send_jsonrpc
          ?timeout:ctx.rpc_timeout
          ctx.ws_client
          (Call
             ( (module Rpc_encodings.Get_transaction_count),
               (ctx.public_key, Block_parameter Latest) ))
      in
      ctx.nonce <- nonce ;
      (* Clear queue because we reinject all missing claims. *)
      let (module Tx_container) = Tx_queue.tx_container in
      let* () = Tx_container.clear () in
      let handle_deposits deposit =
        let is_native = Db.(deposit.token = XTZ) in
        let deposit_id_qty = deposit.nonce in
        let (Qty deposit_id) = deposit_id_qty in
        let deposit_type = if is_native then "xtz" else "fa" in
        let*! () =
          Event.(emit claiming_deposit) (deposit_type, deposit_id_qty)
        in
        claim ctx ~is_native ~deposit_id
      in
      List.iter_es handle_deposits deposits

let on_new_block ctx ~catch_up number =
  let open Lwt_result_syntax in
  let*! () = Event.(emit new_etherlink_head) number in
  (* Process logs for this block *)
  let* () = get_logs ctx ~block:number in
  (* Notify tx queue and register claimed deposits in DB *)
  let* () = handle_confirmed_txs ctx number in
  let* () = Db.Pointers.L2_head.set ctx.db number in
  unless catch_up @@ fun () -> claim_deposits ctx

let rec catch_up ctx ~from_block ~end_block =
  let open Lwt_result_syntax in
  let Ethereum_types.Qty from_, Ethereum_types.Qty end_ =
    (from_block, end_block)
  in
  if Z.gt from_ end_ then return_unit
  else
    let* () = on_new_block ctx from_block ~catch_up:true in
    catch_up ctx ~from_block:(Ethereum_types.Qty.next from_block) ~end_block

let monitor_heads ctx =
  let open Lwt_result_syntax in
  let* head =
    Websocket_client.send_jsonrpc
      ?timeout:ctx.rpc_timeout
      ctx.ws_client
      (Call ((module Rpc_encodings.Block_number), ()))
  and* heads_subscription =
    Websocket_client.subscribe_newHeadNumbers
      ?timeout:ctx.rpc_timeout
      ctx.ws_client
  in
  let* stopped =
    lwt_stream_iter_es_with_timeout
      ~timeout:ctx.block_timeout
      (fun number ->
        let*? number in
        let* last_l2_head = Db.Pointers.L2_head.get ctx.db in
        let expected_level = Ethereum_types.Qty.next last_l2_head in
        let* () =
          unless Ethereum_types.Qty.(number = expected_level) @@ fun () ->
          let*! () =
            Event.(emit catch_up)
              (Z.sub
                 (Ethereum_types.Qty.to_z number)
                 (Ethereum_types.Qty.to_z expected_level))
          in
          catch_up
            ctx
            ~from_block:expected_level
            ~end_block:(Ethereum_types.Qty.pred number)
        in
        on_new_block ctx number ~catch_up:false)
      (Lwt_stream.append
         (Lwt_stream.return (Ok head))
         heads_subscription.stream)
  in
  match stopped with
  | Closed -> return_unit
  | Timeout timeout ->
      (* We didn't receive a new head within ctx.block_timeout so we close the
         connection (and will reconnect after). *)
      let*! () = Event.(emit monitor_heads_timeout) timeout in
      let*! () = Websocket_client.disconnect ctx.ws_client in
      return_unit

let init_db_pointers db ws_client timeout ~first_block =
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
                ?timeout
                ws_client
                (Call
                   ((module Rpc_encodings.Get_block_by_number), (Latest, false)))
            in
            Db.Pointers.L2_head.set db latest.number)
  in
  return_unit

let reconnection_delay = 10.

let is_connection_exception = function
  | Unix.(Unix_error (ECONNREFUSED, _, _))
  | Websocket_lwt_unix.HTTP_Error "404 Not Found"
  | Websocket_client.Connection_closed | Lwt_io.Channel_closed _ ->
      true
  | _ -> false

let is_connection_error trace =
  List.exists
    (function
      | Exn e -> is_connection_exception e
      | RPC_client_errors.(
          Request_failed
            {
              error =
                ( Connection_failed _
                | Unexpected_status_code {code = `Not_found; _} );
              _;
            }) ->
          true
      | _ -> false)
    trace

let get_chain_id ?timeout ws_client =
  Websocket_client.send_jsonrpc
    ?timeout
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

let ws_uri uri =
  match Uri.scheme uri with
  | Some "ws" | Some "wss" ->
      (* This is already a websocket URI which has the correct path so we leave
         it as is. *)
      uri
  | None | Some "http" ->
      let uri = Uri.with_scheme uri (Some "ws") in
      Uri.with_path uri (Uri.path uri ^ "/ws")
  | Some "https" ->
      let uri = Uri.with_scheme uri (Some "wss") in
      Uri.with_path uri (Uri.path uri ^ "/ws")
  | _ -> uri

let rpc_uri uri =
  match Uri.scheme uri with
  | None | Some "http" | Some "https" ->
      (* This is already an RPC URI so we leave it as is. *)
      uri
  | Some "ws" -> (
      let uri = Uri.with_scheme uri (Some "http") in
      match String.remove_suffix ~suffix:"/ws" (Uri.path uri) with
      | Some p -> Uri.with_path uri p
      | None -> uri)
  | Some "wss" -> (
      let uri = Uri.with_scheme uri (Some "https") in
      match String.remove_suffix ~suffix:"/ws" (Uri.path uri) with
      | Some p -> Uri.with_path uri p
      | None -> uri)
  | _ -> uri

let start db ~config ~notify_ws_change ~first_block =
  let open Lwt_result_syntax in
  let evm_node_endpoint = config.Config.evm_node_endpoint in
  let tx_queue_endpoint =
    ref (Services_backend_sig.Rpc (rpc_uri evm_node_endpoint))
  in
  let connected_once = ref false in
  let run () =
    let ws_client =
      Websocket_client.create
        ~monitoring:{ping_timeout = 60.; ping_interval = 10.}
        ~keep_alive:false
          (* We want a connection drop to retrigger a reconnection and a new
             head subscription. *)
        Media_type.json
        (ws_uri evm_node_endpoint)
    in
    let* () = Websocket_client.connect ws_client in
    connected_once := true ;
    tx_queue_endpoint := Services_backend_sig.Websocket ws_client ;
    notify_ws_change ws_client ;
    let* () = init_db_pointers db ws_client config.rpc_timeout ~first_block in
    let* chain_id = get_chain_id ?timeout:config.rpc_timeout ws_client in
    (* We checked that it exists in main.ml *)
    let secret_key = Stdlib.Option.get config.Config.secret_key in
    let public_key = Public_key.(to_address (from_sk secret_key)) in
    let ctx =
      {
        db;
        ws_client;
        max_fee_per_gas = Z.of_int64 config.max_fee_per_gas;
        public_key;
        sk = secret_key;
        chain_id;
        gas_limit = Z.of_int64 config.gas_limit;
        whitelist =
          (if config.monitor_all_deposits then None else config.whitelist);
        nonce = Ethereum_types.Qty.zero;
        block_timeout = config.block_timeout;
        rpc_timeout = config.rpc_timeout;
      }
    in
    monitor_heads ctx
  in
  let rec loop () =
    let*! stopped = protect run in
    let* () =
      match stopped with
      | Error e ->
          let*! () = Event.(emit monitor_error) e in
          Format.eprintf "connection error: %b@." (is_connection_error e) ;
          (match e with
          | Exn e :: _ ->
              Format.eprintf "exn: %s@." (Printexc.to_string_default e)
          | _ -> ()) ;
          if is_connection_error e && !connected_once then return_unit
          else fail e
      | Ok () -> return_unit
    in
    let*! () = Event.(emit ws_reconnection) reconnection_delay in
    let*! () = Lwt_unix.sleep reconnection_delay in
    loop ()
  in
  let rec tx_queue_beacon () =
    let open Lwt_syntax in
    let (module Tx_container) = Tx_queue.tx_container in
    let* res =
      protect @@ fun () ->
      Tx_container.tx_queue_tick ~evm_node_endpoint:!tx_queue_endpoint
    in
    let* () =
      match res with
      | Ok () -> return_unit
      | Error e ->
          Format.kasprintf Event.(emit tx_queue_error) "%a" pp_print_trace e
    in
    let*! () = Lwt_unix.sleep 0.5 in
    tx_queue_beacon ()
  in
  let* () =
    let timeout =
      match config.rpc_timeout with Some t -> t.timeout | None -> 10.
    in
    Tx_queue.start
      ~config:
        {
          max_size = 1000;
          max_transaction_batch_length = None;
          max_lifespan_s = 5;
          tx_per_addr_limit = 10000L;
        }
      ~keep_alive:true
      ~timeout
      ()
  in
  Lwt.dont_wait tx_queue_beacon ignore ;
  Lwt.dont_wait (fun () -> Internal_event.Simple.emit Event.started ()) ignore ;
  loop ()
