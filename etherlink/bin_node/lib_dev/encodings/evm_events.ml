(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module Delayed_transaction = struct
  type kind = Transaction | Deposit | Fa_deposit

  type t = {
    kind : kind;
    hash : hash;
    raw : string;
        (* Binary string, so that it integrates smoothly with the tx-pool. *)
  }

  let encoding_kind =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"transaction"
          (constant "transaction")
          (function Transaction -> Some () | _ -> None)
          (function () -> Transaction);
        case
          (Tag 1)
          ~title:"deposit"
          (constant "deposit")
          (function Deposit -> Some () | _ -> None)
          (function () -> Deposit);
        case
          (Tag 2)
          ~title:"fa_deposit"
          (constant "fa_deposit")
          (function Fa_deposit -> Some () | _ -> None)
          (function () -> Fa_deposit);
      ]

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun {kind; hash; raw} -> (kind, hash, raw))
      (fun (kind, hash, raw) -> {kind; hash; raw})
      (tup3 encoding_kind hash_encoding (string' Hex))

  let of_rlp_content ?(transaction_tag = "\x03") ?(fa_deposit_tag = "\x04") hash
      rlp_content =
    match rlp_content with
    | Rlp.(List [Value tag; content]) -> (
        match (Bytes.to_string tag, content) with
        (* The new delayed transaction event actually contains the
           TransactionContent, which is Ethereum|Deposit|DelayedTransaction.
           Transaction cannot be in the delayed inbox by construction, therefore
           we care only about Deposit and DelayedTransaction.

           However, we use this function to decode actual delayed inbox item
           when we initialize from a rollup-node. They contain the same
           payload but have a different tag for transaction.
        *)
        | tag, Rlp.Value raw_tx when tag = transaction_tag ->
            Some {kind = Transaction; hash; raw = Bytes.to_string raw_tx}
        | tag, fa_deposit when tag = fa_deposit_tag ->
            (* Delayed inbox item has tag 3, inbox::transaction has tag 4. Event
               uses the inbox::transaction tag. *)
            let raw = Rlp.encode fa_deposit |> Bytes.to_string in
            Some {kind = Fa_deposit; hash; raw}
        | "\x02", deposit ->
            let raw = Rlp.encode deposit |> Bytes.to_string in
            Some {kind = Deposit; hash; raw}
        | _ -> None)
    | _ -> None

  let to_rlp {kind; raw; hash} =
    let open Rlp in
    let tag =
      (match kind with
      | Transaction -> "\x03"
      | Deposit -> "\x02"
      | Fa_deposit -> "\x04")
      |> Bytes.of_string
    in
    let hash = hash_to_bytes hash |> Bytes.of_string in
    let content =
      match kind with
      | Transaction -> Value (Bytes.of_string raw)
      | Deposit -> decode_exn (Bytes.of_string raw)
      | Fa_deposit -> decode_exn (Bytes.of_string raw)
    in
    let rlp = List [Value hash; List [Value tag; content]] in
    encode rlp

  let pp_kind fmt = function
    | Transaction -> Format.pp_print_string fmt "Transaction"
    | Deposit -> Format.pp_print_string fmt "Deposit"
    | Fa_deposit -> Format.pp_print_string fmt "FA_Deposit"

  let pp fmt {raw; kind; _} =
    Format.fprintf fmt "%a: %a" pp_kind kind Hex.pp (Hex.of_string raw)

  let pp_short fmt {kind; hash; _} =
    Format.fprintf fmt "%a: %a" pp_kind kind pp_hash hash
end

module Upgrade = struct
  type t = {hash : hash; timestamp : Time.Protocol.t}

  let of_rlp = function
    | Rlp.List [Value hash_bytes; Value timestamp] ->
        let hash =
          hash_bytes |> Bytes.to_string |> Hex.of_string |> Hex.show
          |> hash_of_string
        in
        let timestamp = timestamp_of_bytes timestamp in
        Some {hash; timestamp}
    | _ -> None

  let of_bytes bytes =
    match bytes |> Rlp.decode with Ok rlp -> of_rlp rlp | _ -> None

  let to_bytes {hash; timestamp} =
    let hash = hash_to_bytes hash |> String.to_bytes in
    let timestamp = timestamp_to_bytes timestamp in
    Rlp.(encode (List [Value hash; Value timestamp]))

  let encoding =
    let open Data_encoding in
    conv
      (fun {hash = Hash (Hex hash); timestamp} -> (hash, timestamp))
      (fun (hash, timestamp) -> {hash = Hash (Hex hash); timestamp})
      (tup2 string Time.Protocol.encoding)
end

module Sequencer_upgrade = struct
  type t = {
    sequencer : Signature.public_key;
    pool_address : address;
    timestamp : Time.Protocol.t;
  }

  let of_rlp = function
    | Rlp.List [Value sequencer; Value pool_address; Value timestamp] ->
        let sequencer =
          Signature.Public_key.of_b58check_exn (String.of_bytes sequencer)
        in
        let timestamp = timestamp_of_bytes timestamp in
        let pool_address = decode_address pool_address in
        Some {sequencer; pool_address; timestamp}
    | _ -> None

  let to_rlp {sequencer; pool_address; timestamp} =
    let sequencer =
      Signature.Public_key.to_b58check sequencer |> String.to_bytes
    in
    let timestamp = timestamp_to_bytes timestamp in
    let pool_address = encode_address pool_address in
    Rlp.List [Value sequencer; Value pool_address; Value timestamp]

  let of_bytes bytes =
    match bytes |> Rlp.decode with Ok rlp -> of_rlp rlp | _ -> None

  let to_bytes sequencer_upgrade = Rlp.encode @@ to_rlp sequencer_upgrade

  let encoding =
    let open Data_encoding in
    conv
      (fun {sequencer; pool_address = Address (Hex pool_address); timestamp} ->
        (sequencer, pool_address, timestamp))
      (fun (sequencer, pool_address, timestamp) ->
        {sequencer; pool_address = Address (Hex pool_address); timestamp})
      (tup3 Signature.Public_key.encoding string Time.Protocol.encoding)
end

module Blueprint_applied = struct
  type t = {number : quantity; hash : block_hash}

  let of_rlp = function
    | Rlp.List [Value number; Value hash] ->
        let number = decode_number_le number in
        let hash = decode_block_hash hash in
        Some {number; hash}
    | _ -> None

  let encoding =
    let open Data_encoding in
    conv
      (fun {number = Qty number; hash = Block_hash (Hex hash)} ->
        (number, hash))
      (fun (number, hash) ->
        {number = Qty number; hash = Block_hash (Hex hash)})
      (tup2 z string)
end

type t =
  | Upgrade_event of Upgrade.t
  | Sequencer_upgrade_event of Sequencer_upgrade.t
  | Blueprint_applied of Blueprint_applied.t
  | New_delayed_transaction of Delayed_transaction.t

let of_bytes bytes =
  match bytes |> Rlp.decode with
  | Ok (Rlp.List [Value tag; rlp_content]) -> (
      match Bytes.to_string tag with
      | "\x01" ->
          let upgrade = Upgrade.of_rlp rlp_content in
          Option.map (fun u -> Upgrade_event u) upgrade
      | "\x02" ->
          let sequencer_upgrade = Sequencer_upgrade.of_rlp rlp_content in
          Option.map (fun u -> Sequencer_upgrade_event u) sequencer_upgrade
      | "\x03" ->
          let blueprint_applied = Blueprint_applied.of_rlp rlp_content in
          Option.map (fun u -> Blueprint_applied u) blueprint_applied
      | "\x04" -> (
          match rlp_content with
          | List [Value hash; transaction_content] ->
              let hash = decode_hash hash in
              let transaction =
                Delayed_transaction.of_rlp_content hash transaction_content
              in
              Option.map (fun u -> New_delayed_transaction u) transaction
          | _ -> None)
      | _ -> None)
  | _ -> None

let pp fmt = function
  | Upgrade_event {hash; timestamp} ->
      Format.fprintf
        fmt
        "Upgrade:@ hash %a,@ timestamp: %a"
        pp_hash
        hash
        Time.Protocol.pp_hum
        timestamp
  | Sequencer_upgrade_event
      {sequencer; pool_address = Address (Hex address); timestamp} ->
      Format.fprintf
        fmt
        "Sequencer upgrade:@ sequencer:@ %a,pool_address %s,@ timestamp: %a"
        Signature.Public_key.pp
        sequencer
        address
        Time.Protocol.pp_hum
        timestamp
  | Blueprint_applied {number = Qty number; hash = Block_hash (Hex hash)} ->
      Format.fprintf
        fmt
        "Blueprint applied:@,number:%a@ hash: %s"
        Z.pp_print
        number
        hash
  | New_delayed_transaction delayed_transaction ->
      Format.fprintf
        fmt
        "New delayed transaction:@ %a"
        Delayed_transaction.pp_short
        delayed_transaction

let encoding =
  let open Data_encoding in
  let case ~kind ~tag ~event_encoding ~proj ~inj =
    case
      ~title:kind
      (Tag tag)
      (obj2 (req "kind" string) (req "event" event_encoding))
      (fun x -> match proj x with None -> None | Some x -> Some (kind, x))
      (fun (_, x) -> inj x)
  in
  union
    [
      case
        ~kind:"kernel_upgrade"
        ~tag:0
        ~event_encoding:Upgrade.encoding
        ~proj:(function Upgrade_event upgrade -> Some upgrade | _ -> None)
        ~inj:(fun upgrade -> Upgrade_event upgrade);
      case
        ~kind:"sequencer_upgrade"
        ~tag:1
        ~event_encoding:Sequencer_upgrade.encoding
        ~proj:(function
          | Sequencer_upgrade_event upgrade -> Some upgrade | _ -> None)
        ~inj:(fun upgrade -> Sequencer_upgrade_event upgrade);
      case
        ~kind:"blueprint_applied"
        ~tag:2
        ~event_encoding:Blueprint_applied.encoding
        ~proj:(function Blueprint_applied info -> Some info | _ -> None)
        ~inj:(fun info -> Blueprint_applied info);
      case
        ~kind:"new_delayed_transaction"
        ~tag:3
        ~event_encoding:Delayed_transaction.encoding
        ~proj:(function
          | New_delayed_transaction delayed_transaction ->
              Some delayed_transaction
          | _ -> None)
        ~inj:(fun delayed_transaction ->
          New_delayed_transaction delayed_transaction);
    ]
