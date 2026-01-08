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

type 'b encoding_wrapper = {
  fn : 'a. 'a Data_encoding.t -> ('b * 'a) Data_encoding.t;
}

let message_cases (type extra) ~(wrap_enc : extra encoding_wrapper) ~ofs
    ~(from :
       message -> extra -> message Octez_telemetry.Traceparent.instrumented)
    ~to_ =
  let from f (extra, x) = from (f x) extra in
  let to_ f x =
    match to_ x with
    | Some (b, x) -> Option.map (fun x -> (b, x)) (f x)
    | None -> None
  in
  Data_encoding.
    [
      case
        ~title:"Blueprint"
        (Tag (ofs + 0))
        (wrap_enc.fn
           (obj2
              (req "kind" (constant "blueprint"))
              (req "blueprint" Blueprint_types.Legacy.with_events_encoding)))
        (fun _ ->
          (* We don't produce legacy blueprints anymore but we can still decode
             them. *)
          None)
        (from (fun ((), bp) -> Blueprint (Blueprint_types.of_legacy bp)));
      case
        ~title:"Blueprint.v2"
        (Tag (ofs + 1))
        (wrap_enc.fn
           (obj2
              (req "kind" (constant "blueprint_v2"))
              (req "blueprint" Blueprint_types.with_events_encoding)))
        (to_ (function Blueprint bp -> Some ((), bp) | _ -> None))
        (from (fun ((), bp) -> Blueprint bp));
      case
        ~title:"Finalized_levels"
        (Tag (ofs + 2))
        (wrap_enc.fn
           (obj4
              (req "kind" (constant "finalized_levels"))
              (req "l1_level" int32)
              (req "start_l2_level" Ethereum_types.quantity_encoding)
              (req "end_l2_level" Ethereum_types.quantity_encoding)))
        (to_ (function
          | Finalized_levels {l1_level; start_l2_level; end_l2_level} ->
              Some ((), l1_level, start_l2_level, end_l2_level)
          | _ -> None))
        (from (fun ((), l1_level, start_l2_level, end_l2_level) ->
             Finalized_levels {l1_level; start_l2_level; end_l2_level}));
      case
        ~title:"Next_block_info"
        (Tag (ofs + 3))
        (wrap_enc.fn
           (obj3
              (req "kind" (constant "block_timestamp"))
              (req "timestamp" Time.Protocol.encoding)
              (req "number" Ethereum_types.quantity_encoding)))
        (to_ (function
          | Next_block_info {timestamp; number} -> Some ((), timestamp, number)
          | _ -> None))
        (from (fun ((), timestamp, number) ->
             Next_block_info {timestamp; number}));
      case
        ~title:"Included_transaction"
        (Tag (ofs + 4))
        (wrap_enc.fn
           (obj3
              (req "kind" (constant "included_transaction"))
              (req "transaction" transaction_encoding)
              (req "hash" Ethereum_types.hash_encoding)))
        (to_ (function
          | Included_transaction {tx; hash} -> Some ((), tx, hash)
          | _ -> None))
        (from (fun ((), tx, hash) -> Included_transaction {tx; hash}));
      case
        ~title:"Dropped_transaction"
        (Tag (ofs + 5))
        (wrap_enc.fn
           (obj3
              (req "kind" (constant "dropped_transaction"))
              (req "hash" Ethereum_types.hash_encoding)
              (req "reason" string)))
        (to_ (function
          | Dropped_transaction {hash; reason} -> Some ((), hash, reason)
          | _ -> None))
        (from (fun ((), hash, reason) -> Dropped_transaction {hash; reason}));
    ]

let encoding =
  let open Data_encoding in
  let open Octez_telemetry.Traceparent in
  union
    (message_cases
       ~ofs:0
       ~wrap_enc:{fn = (fun enc -> conv snd (fun x -> ((), x)) enc)}
       ~from:(fun data () -> {data; origin = None})
       ~to_:(function {data; origin = None} -> Some ((), data) | _ -> None)
    @ message_cases
        ~ofs:0x80
        ~wrap_enc:
          {
            fn =
              (fun enc ->
                merge_objs
                  (obj1
                     (req "traceparent" Octez_telemetry.Traceparent.encoding))
                  enc);
          }
        ~from:(fun data traceparent -> {data; origin = Some traceparent})
        ~to_:(function
          | {data; origin = Some traceparent} -> Some (traceparent, data)
          | _ -> None))

(** Stream on which all messages are broadcasted *)
let message_watcher :
    message Octez_telemetry.Traceparent.instrumented Lwt_watcher.input =
  Lwt_watcher.create_input ()

let notify message =
  Lwt_watcher.notify
    message_watcher
    (Octez_telemetry.Traceparent.instrument message)

let create_broadcast_stream () = Lwt_watcher.create_stream message_watcher

let notify_blueprint b =
  let legacy = Blueprint_types.make_legacy b in
  let () = Lwt_watcher.notify blueprint_watcher legacy in
  notify (Blueprint b)

let notify_finalized_levels ~l1_level ~start_l2_level ~end_l2_level =
  let message = Finalized_levels {l1_level; start_l2_level; end_l2_level} in
  notify message

let notify_next_block_info timestamp number =
  notify (Next_block_info {timestamp; number})

let notify_inclusion tx hash = notify (Included_transaction {tx; hash})

let notify_dropped ~hash ~reason = notify (Dropped_transaction {hash; reason})

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
