(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type delay = Disabled | Custom of Int32.t

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
           (function Disabled -> Some () | _ -> None)
           (fun () -> Disabled);
         case
           ~title:"custom"
           ~description:
             "When \"custom <N>\" is set, storage maintenance is triggered \
              \"N\" blocks after the start of a new cycle."
           (Tag 1)
           (obj1 (req "custom" int32))
           (function Custom delay -> Some delay | _ -> None)
           (fun delay -> Custom delay);
       ])

let pp_delay fmt = function
  | Disabled -> Format.fprintf fmt "disabled"
  | Custom delay -> Format.fprintf fmt "custom %ld" delay
