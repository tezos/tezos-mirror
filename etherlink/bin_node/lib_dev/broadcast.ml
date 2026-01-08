(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Stream on which only blueprints are broadcasted. *)
let blueprint_watcher : Blueprint_types.Legacy.with_events Lwt_watcher.input =
  Lwt_watcher.create_input ()

let create_blueprint_stream () = Lwt_watcher.create_stream blueprint_watcher

type common_transaction = Evm of string | Michelson of string

let common_transaction =
  let open Data_encoding in
  union
    [
      case
        ~title:"Evm"
        (Tag 0)
        (obj2 (req "kind" (constant "evm")) (req "transaction" (string' Hex)))
        (function Evm ts -> Some ((), ts) | _ -> None)
        (fun ((), ts) -> Evm ts);
      case
        ~title:"Michelson"
        (Tag 1)
        (obj2
           (req "kind" (constant "michelson"))
           (req "transaction" (string' Hex)))
        (function Michelson ts -> Some ((), ts) | _ -> None)
        (fun ((), ts) -> Michelson ts);
    ]

type transaction =
  | Common of common_transaction
  | Delayed of Evm_events.Delayed_transaction.t

let transaction_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Common_transaction"
        (Tag 0)
        (obj2
           (req "kind" (constant "Common_transaction"))
           (req "transaction" common_transaction))
        (function Common txn -> Some ((), txn) | _ -> None)
        (fun ((), txn) -> Common txn);
      case
        ~title:"Delayed_transaction"
        (Tag 1)
        (obj2
           (req "kind" (constant "Delayed_transaction"))
           (req "transaction" Evm_events.Delayed_transaction.encoding))
        (function Delayed txn -> Some ((), txn) | _ -> None)
        (fun ((), txn) -> Delayed txn);
    ]

type message =
  | Blueprint of Blueprint_types.with_events
  | Finalized_levels of {
      l1_level : int32;
      start_l2_level : Ethereum_types.quantity;
      end_l2_level : Ethereum_types.quantity;
    }
  | Next_block_info of {
      timestamp : Time.Protocol.t;
      number : Ethereum_types.quantity;
    }
  | Included_transaction of {tx : transaction; hash : Ethereum_types.hash}
  | Dropped_transaction of {hash : Ethereum_types.hash; reason : string}

let message_encoding =
  let open Data_encoding in
  Data_encoding.union
    [
      case
        ~title:"Blueprint"
        (Tag 0)
        (obj2
           (req "kind" (constant "blueprint"))
           (req "blueprint" Blueprint_types.Legacy.with_events_encoding))
        (fun _ ->
          (* We don't produce legacy blueprints anymore but we can still decode
             them. *)
          None)
        (fun ((), bp) -> Blueprint (Blueprint_types.of_legacy bp));
      case
        ~title:"Blueprint.v2"
        (Tag 1)
        (obj2
           (req "kind" (constant "blueprint_v2"))
           (req "blueprint" Blueprint_types.with_events_encoding))
        (function Blueprint bp -> Some ((), bp) | _ -> None)
        (fun ((), bp) -> Blueprint bp);
      case
        ~title:"Finalized_levels"
        (Tag 2)
        (obj4
           (req "kind" (constant "finalized_levels"))
           (req "l1_level" int32)
           (req "start_l2_level" Ethereum_types.quantity_encoding)
           (req "end_l2_level" Ethereum_types.quantity_encoding))
        (function
          | Finalized_levels {l1_level; start_l2_level; end_l2_level} ->
              Some ((), l1_level, start_l2_level, end_l2_level)
          | _ -> None)
        (fun ((), l1_level, start_l2_level, end_l2_level) ->
          Finalized_levels {l1_level; start_l2_level; end_l2_level});
      case
        ~title:"Next_block_info"
        (Tag 3)
        (obj3
           (req "kind" (constant "block_timestamp"))
           (req "timestamp" Time.Protocol.encoding)
           (req "number" Ethereum_types.quantity_encoding))
        (function
          | Next_block_info {timestamp; number} -> Some ((), timestamp, number)
          | _ -> None)
        (fun ((), timestamp, number) -> Next_block_info {timestamp; number});
      case
        ~title:"Included_transaction"
        (Tag 4)
        (obj3
           (req "kind" (constant "included_transaction"))
           (req "transaction" transaction_encoding)
           (req "hash" Ethereum_types.hash_encoding))
        (function
          | Included_transaction {tx; hash} -> Some ((), tx, hash) | _ -> None)
        (fun ((), tx, hash) -> Included_transaction {tx; hash});
      case
        ~title:"Dropped_transaction"
        (Tag 5)
        (obj3
           (req "kind" (constant "dropped_transaction"))
           (req "hash" Ethereum_types.hash_encoding)
           (req "reason" string))
        (function
          | Dropped_transaction {hash; reason} -> Some ((), hash, reason)
          | _ -> None)
        (fun ((), hash, reason) -> Dropped_transaction {hash; reason});
    ]

(** Stream on which all messages are broadcasted *)
let message_watcher : message Lwt_watcher.input = Lwt_watcher.create_input ()

let create_broadcast_stream () = Lwt_watcher.create_stream message_watcher

let notify_blueprint b =
  let legacy = Blueprint_types.make_legacy b in
  let () = Lwt_watcher.notify blueprint_watcher legacy in
  Lwt_watcher.notify message_watcher (Blueprint b)

let notify_finalized_levels ~l1_level ~start_l2_level ~end_l2_level =
  let message = Finalized_levels {l1_level; start_l2_level; end_l2_level} in
  Lwt_watcher.notify message_watcher message

let notify_next_block_info timestamp number =
  Lwt_watcher.notify message_watcher (Next_block_info {timestamp; number})

let notify_inclusion tx hash =
  Lwt_watcher.notify message_watcher (Included_transaction {tx; hash})

let notify_dropped ~hash ~reason =
  Lwt_watcher.notify message_watcher (Dropped_transaction {hash; reason})

type transaction_result = {
  hash : Ethereum_types.hash;
  result : (Transaction_receipt.t, string) result;
}

(** Stream on which only pre-confirmed results are streamed  *)
let transaction_result_watcher : transaction_result Lwt_watcher.input =
  Lwt_watcher.create_input ()

let create_transaction_result_stream () =
  Lwt_watcher.create_stream transaction_result_watcher

let notify_transaction_result res =
  Lwt_watcher.notify transaction_result_watcher res
