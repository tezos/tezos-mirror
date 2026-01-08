(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Catching_up of {levels_to_process : int32}
  | Synced
  | Lagging of {levels_to_process : int32}
  | L1_bootstrapping
  | L1_unreachable
  | Unknown

let encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"Catching_up"
        (Tag 0)
        (obj2
           (req "status" (constant "Catching_up"))
           (req "levels_to_process" int32))
        (function
          | Catching_up {levels_to_process} -> Some ((), levels_to_process)
          | _ -> None)
        (fun ((), levels_to_process) -> Catching_up {levels_to_process});
      case
        ~title:"Synced"
        (Tag 1)
        (obj1 (req "status" (constant "Synced")))
        (function Synced -> Some () | _ -> None)
        (fun () -> Synced);
      case
        ~title:"Lagging"
        (Tag 2)
        (obj2
           (req "status" (constant "Lagging"))
           (req "levels_to_process" int32))
        (function
          | Lagging {levels_to_process} -> Some ((), levels_to_process)
          | _ -> None)
        (fun ((), levels_to_process) -> Lagging {levels_to_process});
      case
        ~title:"L1_bootstrapping"
        (Tag 3)
        (obj1 (req "status" (constant "L1_bootstrapping")))
        (function L1_bootstrapping -> Some () | _ -> None)
        (fun () -> L1_bootstrapping);
      case
        ~title:"L1_unreachable"
        (Tag 4)
        (obj1 (req "status" (constant "L1_unreachable")))
        (function L1_unreachable -> Some () | _ -> None)
        (fun () -> L1_unreachable);
      case
        ~title:"Unknown"
        (Tag 5)
        (obj1 (req "status" (constant "Unknown")))
        (function Unknown -> Some () | _ -> None)
        (fun () -> Unknown);
    ]

let catching_up_lagging_or_synced_status ~delta_kind ~head_level
    ~last_processed_level =
  let last_finalized = Int32.sub head_level 2l in
  let levels_to_process = Int32.(sub last_finalized last_processed_level) in
  if levels_to_process > 0l then
    match delta_kind with
    | `Catching_up -> Catching_up {levels_to_process}
    | `Lagging -> Lagging {levels_to_process}
  else Synced

let catching_up_or_synced_status =
  catching_up_lagging_or_synced_status ~delta_kind:`Catching_up

let lagging_or_synced_status =
  catching_up_lagging_or_synced_status ~delta_kind:`Lagging
