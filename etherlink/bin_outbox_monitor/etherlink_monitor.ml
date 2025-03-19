(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

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

  let section = ["outbox_monitor"; "etherlink"]

  let transaction_log =
    declare_1
      ~section
      ~name:"transaction_log"
      ~msg:"Received withdrawal log {log}"
      ~level:Debug
      ("log", Ethereum_types.transaction_log_encoding)

  let pp_withdrawal_kind fmt (k : Db.withdrawal_kind) =
    match k with
    | Xtz -> Format.pp_print_string fmt "XTZ"
    | FA ticket_owner ->
        Format.fprintf
          fmt
          "FA(%s)"
          (Ethereum_types.Address.to_string ticket_owner)

  let withdrawal_log =
    declare_8
      ~section
      ~name:"withdrawal_log"
      ~msg:
        "Withdrawal of {amount} {token} from {sender} to {receiver} in \
         transaction {transactionHash}({transactionIndex}/{logIndex}) of block \
         {blockNumber}"
      ~level:Notice
      ("amount", Db.quantity_hum_encoding)
      ("token", Db.withdrawal_kind_encoding)
      ("sender", Ethereum_types.address_encoding)
      ("receiver", Contract.encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("transactionIndex", Db.quantity_hum_encoding)
      ("logIndex", Db.quantity_hum_encoding)
      ("blockNumber", Db.quantity_hum_encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:pp_withdrawal_kind
      ~pp3:(fun fmt a ->
        Format.pp_print_string fmt (Ethereum_types.Address.to_string a))
      ~pp4:Contract.pp
      ~pp5:Ethereum_types.pp_hash
      ~pp6:Ethereum_types.pp_quantity
      ~pp7:Ethereum_types.pp_quantity
      ~pp8:Ethereum_types.pp_quantity

  let emit_withdrawal_log (w : Db.withdrawal_log) =
    emit
      withdrawal_log
      ( w.withdrawal.amount,
        w.withdrawal.kind,
        w.withdrawal.sender,
        w.withdrawal.receiver,
        w.transactionHash,
        w.transactionIndex,
        w.logIndex,
        w.blockNumber )

  let parsing_error =
    declare_1
      ~section
      ~name:"parsing_error"
      ~msg:"Withdrawal log parsing error: {error}"
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
end

type error +=
  | Too_many_withdrawals_in_one_block of {
      block : Ethereum_types.quantity;
      limit : int;
    }

let () =
  register_error_kind
    `Temporary
    ~id:"etherlink_monitor.too_many_withdrawals_in_one_block"
    ~title:"Too many withdrawals in one block"
    ~description:"Too many withdrawals in one block to fetch."
    ~pp:(fun ppf (block, limit) ->
      Format.fprintf
        ppf
        "There are more than %d withdrawal logs in the block %a. Change the \
         max_nb_logs config in the EVM node to recover."
        limit
        Ethereum_types.pp_quantity
        block)
    Data_encoding.(
      obj2
        (req "blockNumber" Db.quantity_hum_encoding)
        (req "limit" Data_encoding.int31))
    (function
      | Too_many_withdrawals_in_one_block {block; limit} -> Some (block, limit)
      | _ -> None)
    (fun (block, limit) -> Too_many_withdrawals_in_one_block {block; limit})

module Withdrawal = struct
  type data = {
    amount : Ethereum_types.quantity;
    sender : Ethereum_types.Address.t;
    receiver : Contract.t;
    withdrawal_id : Ethereum_types.quantity;
  }

  let address =
    Ethereum_types.Address (Hex "ff00000000000000000000000000000000000001")

  let topic =
    (* 2d5ad793181f5b6bd727c0c216461e019ecfe466353fdde9ccf813f45b84fa82 *)
    kecack_topic "Withdrawal(uint256,address,bytes22,uint256)"

  let filter = mk_filter address topic

  let decode_event_data (Ethereum_types.Hex hex_data) =
    let open Result_syntax in
    let data = Hex.to_bytes (`Hex hex_data) in
    let* data =
      match data with
      | None -> error_with "Invalid hex data in withdrawal event"
      | Some d -> return d
    in
    let* () =
      if Bytes.length data <> 4 * 32 then
        error_with "Invalid length for data of withdrawal event"
      else return_unit
    in
    let amount_b = extract_32 0 data in
    let amount = Ethereum_types.decode_number_be amount_b in
    let sender_b = extract_32 1 data ~padding:(`Left_padded 20) in
    let sender = Ethereum_types.decode_address sender_b in
    let receiver_b = extract_32 2 data ~padding:(`Right_padded 22) in
    let receiver =
      Data_encoding.Binary.of_bytes_exn Contract.encoding receiver_b
    in
    let withdrawal_id_b = extract_32 3 data in
    let withdrawal_id = Ethereum_types.decode_number_be withdrawal_id_b in
    return {amount; sender; receiver; withdrawal_id}
end

module FA_withdrawal = struct
  type event = {
    sender : Ethereum_types.Address.t;
    ticket_hash : Ethereum_types.hash;
    ticket_owner : Ethereum_types.Address.t;
    receiver : Contract.t;
    proxy : Contract.t;
    amount : Ethereum_types.quantity;
    withdrawal_id : Ethereum_types.quantity;
  }

  let address =
    Ethereum_types.Address (Hex "ff00000000000000000000000000000000000002")

  let topic =
    kecack_topic
      "Withdrawal(uint256,address,address,bytes22,bytes22,uint256,uint256)"

  let filter = mk_filter address topic

  let decode_event_log Ethereum_types.{topics; data = Hex hex_data; _} =
    let open Result_syntax in
    let* ticket_hash =
      match topics with
      | [_addr; th] -> return th
      | _ -> error_with "Missing ticket hash from FA withdrawal topics"
    in
    let data = Hex.to_bytes (`Hex hex_data) in
    let* data =
      match data with
      | None -> error_with "Invalid hex data in FA withdrawal event"
      | Some d -> return d
    in
    let* () =
      if Bytes.length data <> 6 * 32 then
        error_with "Invalid length for data of FA withdrawal event"
      else return_unit
    in
    let sender =
      extract_32 0 data ~padding:(`Left_padded 20)
      |> Ethereum_types.decode_address
    in
    let ticket_owner =
      extract_32 1 data ~padding:(`Left_padded 20)
      |> Ethereum_types.decode_address
    in
    let receiver =
      extract_32 2 data ~padding:(`Right_padded 22)
      |> Data_encoding.Binary.of_bytes_exn Contract.encoding
    in
    let proxy =
      extract_32 3 data ~padding:(`Right_padded 22)
      |> Data_encoding.Binary.of_bytes_exn Contract.encoding
    in
    let amount = extract_32 4 data |> Ethereum_types.decode_number_be in
    let withdrawal_id = extract_32 5 data |> Ethereum_types.decode_number_be in
    return
      {
        sender;
        ticket_hash;
        ticket_owner;
        receiver;
        proxy;
        amount;
        withdrawal_id;
      }
end

type parsed =
  | Withdrawal of Withdrawal.data
  | FA_withdrawal of FA_withdrawal.event

let parsed_to_db = function
  | Withdrawal {amount; sender; receiver; withdrawal_id} ->
      Db.{kind = Xtz; amount; sender; receiver; withdrawal_id}
  | FA_withdrawal
      {
        sender;
        ticket_hash = _;
        ticket_owner;
        receiver;
        proxy = _;
        amount;
        withdrawal_id;
      } ->
      Db.{kind = FA ticket_owner; amount; sender; receiver; withdrawal_id}

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
            transactionHash;
            transactionIndex;
            logIndex;
            blockHash;
            blockNumber;
            removed = Option.value removed ~default:false;
            withdrawal = parsed_to_db event;
          }
  | _ -> None

let parse_log (log : Ethereum_types.transaction_log) =
  let open Result_syntax in
  match Filter_helpers.filter_one_log Withdrawal.filter log with
  | Some _ ->
      let* withdraw_data = Withdrawal.decode_event_data log.data in
      return (parsed_log_to_db log (Withdrawal withdraw_data))
  | None -> (
      match Filter_helpers.filter_one_log FA_withdrawal.filter log with
      | Some _ ->
          let* withdraw_data = FA_withdrawal.decode_event_log log in
          return (parsed_log_to_db log (FA_withdrawal withdraw_data))
      | None -> return_none)

let handle_one_log db (log : Ethereum_types.transaction_log) =
  let open Lwt_result_syntax in
  let*! () = Event.(emit transaction_log) log in
  let withdrawal = parse_log log in
  match withdrawal with
  | Error e ->
      let*! () =
        Format.kasprintf Event.(emit parsing_error) "%a" pp_print_trace e
      in
      fail e
  | Ok None ->
      let*! () =
        Event.(emit parsing_error) "Log did not match withdraw filter"
      in
      return_unit
  | Ok (Some withdrawal) ->
      let*! () = Event.emit_withdrawal_log withdrawal in
      Db.Withdrawals.store db withdrawal

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

let filter_address =
  Ethereum_types.Filter.Vec
    [
      (* Older versions of etherlink emit events with address 0 *)
      system_address;
      Withdrawal.address;
      FA_withdrawal.address;
    ]

let filter_topics =
  [Some (Ethereum_types.Filter.Or [Withdrawal.topic; FA_withdrawal.topic])]

let monitor_withdrawals db ws_client =
  let open Lwt_result_syntax in
  let* logs_subscription =
    Websocket_client.subscribe_logs
      ws_client
      ~address:filter_address
      ~topics:filter_topics
  in
  let* () = lwt_stream_iter_es (handle_one_log db) logs_subscription.stream in
  return_unit

(** Retrieve log events that happened between [from_block] and [to_block] and
    register them in the DB. Uses a recursive strategy to handle the case where
    there are too many logs in the requested range.

    @param db The database to store the logs in
    @param ws_client The websocket client to query logs from
    @param from_block Starting block number (inclusive)
    @param to_block Ending block number (inclusive)
*)
let rec get_logs db ws_client ~from_block ~to_block =
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
        ws_client
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
            | Log log -> handle_one_log db log)
          logs
    | Error (Filter_helpers.Too_many_logs {limit} :: _ as e)
      when Z.equal from_z to_z ->
        (* If we're querying a single block and it has too many logs, this is a
           fatal error - the node's max_nb_logs config needs to be increased *)
        fail
          (TzTrace.cons
             (Too_many_withdrawals_in_one_block {block = from_block; limit})
             e)
    | Error (Filter_helpers.Too_many_logs _ :: _) ->
        (* If there are too many logs in the range, split it in half and try
           each half separately. This handles cases where the total number of
           logs exceeds the node's limit but each half is within bounds. *)
        let middle = Z.ediv (Z.add from_z to_z) (Z.of_int 2) in
        let* () = get_logs db ws_client ~from_block ~to_block:(Qty middle) in
        get_logs db ws_client ~from_block:(Qty (Z.succ middle)) ~to_block
    | Error e -> fail e

let max_nb_blocks_pred = Z.of_int 99

(** Process withdrawal logs in chunks of up to [max_nb_blocks = 100] blocks at a
    time.  This function recursively processes blocks from [from_block] to
    [end_block], updating the L2 head after each chunk is processed.

    @param db The database to store withdrawal logs in
    @param ws_client The websocket client to query logs from
    @param from_block Starting block number (inclusive)
    @param end_block Ending block number (inclusive)
*)
let rec catch_up db ws_client ~from_block ~end_block =
  let open Lwt_result_syntax in
  let open Ethereum_types in
  let Qty from_z, Qty end_z = (from_block, end_block) in
  if Z.Compare.(from_z > end_z) then (* No more blocks to process *)
    return_unit
  else
    (* Calculate the end of this chunk, limited by max_nb_blocks_pred *)
    let to_block = Qty (Z.min end_z (Z.add from_z max_nb_blocks_pred)) in
    (* Process logs for this chunk of blocks *)
    let* () = get_logs db ws_client ~from_block ~to_block in
    let new_head = Qty.next to_block in
    (* We have handled logs up to [new_head - 1] now. *)
    let* () = Db.Pointers.L2_head.set db new_head in
    (* Recursively process the next chunk *)
    catch_up db ws_client ~from_block:new_head ~end_block

(** Catch up on withdrawal logs that occurred while the monitor was offline.
    This function fetches logs between the last processed block and the current
    L2 head.

    @param db The database to store withdrawal logs in
    @param ws_client The websocket client to query logs from
    @param last_l2_head The last L2 block height that was processed (if any)
*)
let catch_up_withdrawals db ws_client ~last_l2_head =
  let open Lwt_result_syntax in
  match last_l2_head with
  | None ->
      (* No previous state - first time running the monitor *)
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7778:
         Fetch 2 weeks of withdrawals *)
      return_unit
  | Some last_l2_head ->
      (* Get current L2 head from the node *)
      let* current_l2_head =
        Websocket_client.send_jsonrpc
          ws_client
          (Call ((module Rpc_encodings.Get_block_by_number), (Latest, false)))
      in
      (* We want to process up to HEAD-1 to avoid partially fetched logs *)
      let pred_l2_head = Ethereum_types.Qty.pred current_l2_head.number in
      (* Calculate how many blocks we need to catch up on *)
      let missing =
        Z.sub
          (Ethereum_types.Qty.to_z pred_l2_head)
          (Ethereum_types.Qty.to_z last_l2_head)
      in
      (* Log if we need to catch up *)
      let*! () =
        if Z.(Compare.(missing > zero)) then Event.(emit catch_up) missing
        else Lwt.return_unit
      in
      (* Process all missing blocks in chunks *)
      catch_up db ws_client ~from_block:last_l2_head ~end_block:pred_l2_head

let monitor_heads db ws_client =
  let open Lwt_result_syntax in
  let* heads_subscription = Websocket_client.subscribe_newHeads ws_client in
  let* () =
    lwt_stream_iter_es
      (fun (b : _ Ethereum_types.block) ->
        let*! () = Event.(emit new_etherlink_head) b.number in
        Db.Pointers.L2_head.set db b.number)
      heads_subscription.stream
  in
  return_unit

let start db ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let*! ws_client =
    Websocket_client.connect Media_type.json evm_node_endpoint
  in
  let* last_l2_head = Db.Pointers.L2_head.get db in
  let monitor_withdrawals = monitor_withdrawals db ws_client in
  let* () = catch_up_withdrawals db ws_client ~last_l2_head in
  let* () = Lwt.pick [monitor_withdrawals; monitor_heads db ws_client] in
  return_unit
