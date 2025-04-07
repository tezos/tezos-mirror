(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Smart constructor for wait and interval duration. *)
type t = {repr : string; ms : int}

(* Conversion factors for each time unit to milliseconds *)
let unit_to_ms = function
  | "ms" -> 1
  | "s" -> 1000
  | "m" -> 60 * 1000
  | "h" -> 60 * 60 * 1000
  | "d" -> 24 * 60 * 60 * 1000
  | "w" -> 7 * 24 * 60 * 60 * 1000
  | "y" -> 365 * 24 * 60 * 60 * 1000
  | _ -> failwith "Unknown time unit"

(* Parse a duration string into milliseconds *)
let of_string str =
  (* Regex to match components like "1h30m" *)
  let rex = Re.Pcre.regexp "([0-9]+)(ms|s|m|h|d|w|y)" in
  (* Function to sum up the total milliseconds from matches *)
  let rec parse acc pos =
    try
      let groups = Re.Pcre.exec ~rex ~pos str in
      let value = int_of_string (Re.Pcre.get_substring groups 1) in
      let unit = Re.Pcre.get_substring groups 2 in
      let _, next_pos = Re.Pcre.get_substring_ofs groups 0 in
      parse ((value * unit_to_ms unit) + acc) next_pos
    with Not_found -> acc
  in
  let ms = parse 0 0 in
  {repr = str; ms}

let to_string {repr; _} = repr

let compare {ms = left; _} {ms = right; _} = compare left right
