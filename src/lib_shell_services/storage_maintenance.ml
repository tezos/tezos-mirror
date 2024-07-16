(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type context_pruning = Enabled | Disabled

let context_pruning_encoding =
  let open Data_encoding in
  def
    "context_pruning"
    ~title:"context_pruning"
    ~description:"Context pruning status"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"disabled"
           ~description:
             "When disabled, the storage maintenance won't be triggered"
           (Tag 0)
           (constant "disabled")
           (function Disabled -> Some () | _ -> None)
           (fun () -> Disabled);
         case
           ~title:"enabled"
           ~description:
             "When enabled, the storage maintenance is triggered as soon as a \
              cycle dawn is encountered. This is the default value."
           (Tag 1)
           (constant "enabled")
           (function Enabled -> Some () | _ -> None)
           (fun () -> Enabled);
       ])

let pp_context_pruning fmt = function
  | Disabled -> Format.fprintf fmt "disabled"
  | Enabled -> Format.fprintf fmt "enabled"

type delay = Disabled

let delay_encoding =
  let open Data_encoding in
  def
    "storage_maintenance_delay"
    ~title:"storage maintenance delay"
    ~description:"Delay prior to the storage maintenance trigger"
    (union
       ~tag_size:`Uint8
       [
         case
           ~title:"disabled"
           ~description:
             "When disabled, the storage maintenance is triggered without any \
              delay, as soon as a new cycle starts."
           (Tag 0)
           (constant "disabled")
           (function Disabled -> Some ())
           (fun () -> Disabled);
       ])

let pp_delay fmt = function Disabled -> Format.fprintf fmt "disabled"
