(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type delay = Disabled | Custom of Int32.t | Auto

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
         case
           ~title:"auto"
           ~description:
             "When \"auto\" is set, storage maintenance is triggered after a \
              delay that is determined automatically."
           (Tag 2)
           (constant "auto")
           (function Auto -> Some () | _ -> None)
           (fun () -> Auto);
       ])

let default_auto_delay ~blocks_per_cycle =
  let exclusion = Int32.(max 1l (div blocks_per_cycle 20l)) in
  let limit = Int32.div blocks_per_cycle 2l in
  let delay =
    Int32.add (Random.int32 (max 1l (Int32.sub limit exclusion))) exclusion
  in
  delay

let pp_delay fmt = function
  | Disabled -> Format.fprintf fmt "disabled"
  | Custom delay -> Format.fprintf fmt "custom %ld" delay
  | Auto -> Format.fprintf fmt "auto"
