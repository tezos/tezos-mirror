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

type message =
  | Blueprint of Blueprint_types.with_events
  | Finalized_levels of {
      l1_level : int32;
      start_l2_level : Ethereum_types.quantity;
      end_l2_level : Ethereum_types.quantity;
    }

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

type preconfirmation_message =
  | Block_timestamp of Time.Protocol.t
  | Preconfirmed_transaction of Transaction_object.t

let preconfirmation_message_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Block_timestamp"
        (Tag 0)
        (obj2
           (req "kind" (constant "block_timestamp"))
           (req "next_block_timestamp" Time.Protocol.encoding))
        (function Block_timestamp ts -> Some ((), ts) | _ -> None)
        (fun ((), ts) -> Block_timestamp ts);
      case
        ~title:"Preconfirmed transaction"
        (Tag 1)
        (obj2
           (req "kind" (constant "preconfirmed_transaction"))
           (req "transaction" Transaction_object.encoding))
        (function Preconfirmed_transaction txn -> Some ((), txn) | _ -> None)
        (fun ((), txn) -> Preconfirmed_transaction txn);
    ]

let preconfirmation_watcher : preconfirmation_message Lwt_watcher.input =
  Lwt_watcher.create_input ()

let create_preconfirmation_stream () =
  Lwt_watcher.create_stream preconfirmation_watcher

let notify_new_block_timestamp timestamp =
  Lwt_watcher.notify preconfirmation_watcher (Block_timestamp timestamp)

let notify_preconfirmation txn =
  Lwt_watcher.notify preconfirmation_watcher (Preconfirmed_transaction txn)
