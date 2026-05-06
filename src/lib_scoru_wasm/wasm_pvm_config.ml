(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type feature = Nds_host_functions

let feature_compare : feature -> feature -> int =
 fun a b -> match (a, b) with Nds_host_functions, Nds_host_functions -> 0

module Feature_map = Map.Make (struct
  type t = feature

  let compare = feature_compare
end)

type t = {features : int32 Feature_map.t}

let empty = {features = Feature_map.empty}

let is_enabled {features} f ~current_level =
  match Feature_map.find_opt f features with
  | None -> false
  | Some activation_level -> Compare.Int32.(activation_level < current_level)

let activation_level {features} f = Feature_map.find_opt f features

let equal a b = Feature_map.equal Int32.equal a.features b.features

let nds_host_functions_signal = "nds_host_functions"

let signal_name_of_feature : feature -> string = function
  | Nds_host_functions -> nds_host_functions_signal

let feature_of_signal_name = function
  | s when String.equal s nds_host_functions_signal -> Some Nds_host_functions
  | _ -> None

let nds_host_functions_enabled t ~current_level =
  is_enabled t Nds_host_functions ~current_level

let of_signals signals =
  let features =
    List.fold_left
      (fun acc (name, level) ->
        match feature_of_signal_name name with
        | None -> acc
        | Some f ->
            (* Keep the oldest (first-seen) activation level on
               duplicates. Protocol-side [enable_signal] rejects
               duplicates, so this case does not arise in practice; the
               guard keeps the module robust independently. *)
            Feature_map.update
              f
              (function
                | Some prev_level -> Some (Int32.min prev_level level)
                | None -> Some level)
              acc)
      Feature_map.empty
      signals
  in
  {features}
