(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Stream on which only blueprints are broadcasted. *)
let blueprint_watcher : Blueprint_types.with_events Lwt_watcher.input =
  Lwt_watcher.create_input ()

let notify_blueprint = Lwt_watcher.notify blueprint_watcher

let create_blueprint_stream () = Lwt_watcher.create_stream blueprint_watcher

type message = Blueprint of Blueprint_types.with_events

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
        (function Blueprint bp -> Some ((), bp))
        (fun ((), bp) -> Blueprint bp);
    ]

(** Stream on which all messages are broadcasted *)
let message_watcher : message Lwt_watcher.input = Lwt_watcher.create_input ()

let create_broadcast_stream () = Lwt_watcher.create_stream message_watcher

let notify = function
  | Blueprint b ->
      let () = notify_blueprint b in
      Lwt_watcher.notify message_watcher (Blueprint b)
