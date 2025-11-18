(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** If the consensus is distributed randomly for 7000 slots,
    the (relative) power is the number of slots of the given delegate,
    noted in [slots]. Otherwise, if all bakers attest, then its power is its baking_power
    for the cycle in which the rights were distributed, noted in [baking_power]. *)
type t = {slots : int; baking_power : int64}

type result = {slots : int; baking_power_opt : int64 option}

let encoding =
  let open Data_encoding in
  conv
    (fun {slots; baking_power} -> (slots, baking_power))
    (fun (slots, baking_power) -> {slots; baking_power})
    (obj2 (req "slots" int31) (req "baking_power" int64))

let op_result_encoding =
  let open Data_encoding in
  conv
    (fun {slots; baking_power_opt} -> (slots, baking_power_opt))
    (fun (slots, baking_power_opt) -> {slots; baking_power_opt})
    (obj2 (req "slots" int31) (req "baking_power" (option int64)))

let to_result ~abaab_activated {slots; baking_power} =
  let baking_power_opt = if abaab_activated then Some baking_power else None in
  {slots; baking_power_opt}

let pp ppf {slots; baking_power} =
  Format.fprintf ppf "(slots:%d, baking power:%Ld)" slots baking_power

let pp_result ppf {slots; baking_power_opt} =
  match baking_power_opt with
  | None -> Format.fprintf ppf "slots:%d" slots
  | Some baking_power ->
      Format.fprintf ppf "(slots:%d, baking power:%Ld)" slots baking_power

let zero = {slots = 0; baking_power = 0L}

let make ~slots ~baking_power = {slots; baking_power}

let add (a : t) (b : t) =
  {
    slots = a.slots + b.slots;
    baking_power = Int64.add a.baking_power b.baking_power;
  }

let get_slots_from_result ({slots; _} : result) = slots

module Internal_for_tests = struct
  let get_from_result {slots; baking_power_opt} =
    match baking_power_opt with
    | None -> Int64.of_int slots
    | Some baking_power -> baking_power
end
