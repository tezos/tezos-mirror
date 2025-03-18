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
end

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

let start db ~evm_node_endpoint =
  let open Lwt_result_syntax in
  let*! ws_client =
    Websocket_client.connect Media_type.json evm_node_endpoint
  in
  let* logs_subscription =
    Websocket_client.subscribe_logs
      ws_client
      ~address:
        (Vec
           [
             (* Older versions of etherlink emit events with address 0 *)
             system_address;
             Withdrawal.address;
             FA_withdrawal.address;
           ])
      ~topics:[Some (Or [Withdrawal.topic; FA_withdrawal.topic])]
  in
  let* () =
    lwt_stream_iter_es
      (fun (log : Ethereum_types.transaction_log) ->
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
            Db.Withdrawals.store db withdrawal)
      logs_subscription.stream
  in
  return_unit
