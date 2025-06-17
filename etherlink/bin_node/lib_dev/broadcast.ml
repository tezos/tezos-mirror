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
           (req "blueprint" Blueprint_types.with_events_encoding))
        (function Blueprint bp -> Some ((), bp) | _ -> None)
        (fun ((), bp) -> Blueprint bp);
      case
        ~title:"Finalized_levels"
        (Tag 1)
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
