(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t = Scatter of int * int | Map of int * int * int | Disabled

let encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 1)
        ~title:"scatter"
        (tup2 int31 int31)
        (function
          | Scatter (nb_keys, nb_daemons) -> Some (nb_keys, nb_daemons)
          | _ -> None)
        (fun (nb_keys, nb_daemons) -> Scatter (nb_keys, nb_daemons));
      case
        (Tag 2)
        ~title:"map"
        (tup3 int31 int31 int31)
        (function
          | Map (nb_keys, nb_alone_bakers, nb_additional_daemons) ->
              Some (nb_keys, nb_alone_bakers, nb_additional_daemons)
          | _ -> None)
        (fun (nb_keys, nb_alone_bakers, nb_additional_daemons) ->
          Map (nb_keys, nb_alone_bakers, nb_additional_daemons));
      case
        (Tag 3)
        ~title:"disabled"
        empty
        (function Disabled -> Some () | _ -> None)
        (fun () -> Disabled);
    ]

let to_string = function
  | Scatter (x, y) -> Format.sprintf "scatter(%d,%d)" x y
  | Map (x, y, z) -> Format.sprintf "map(%d,%d,%d)" x y z
  | Disabled -> Format.sprintf "disabled"

let parse simulate_network_arg =
  let is_positive_param p =
    if p > 0 then p
    else
      Test.fail
        "Unexpected value provided, [%d], from argument [%s]. Values must be \
         positive integers.@."
        p
        simulate_network_arg
  in
  let is_arg1_sup_eq_arg2 arg1 arg2 =
    if arg1 >= arg2 then ()
    else
      Test.fail
        "Unexpected value provided for argument [%s]. %d must be greater or \
         equal to %d."
        simulate_network_arg
        arg1
        arg2
  in
  let re_scatter = Str.regexp "\\(scatter\\)(\\([^,]+\\),\\([^)]*\\))" in
  let re_map = Str.regexp "\\(map\\)(\\([^,]+\\),\\([^)]*\\),\\([^)]*\\))" in
  if Str.string_match re_scatter simulate_network_arg 0 then
    let arg1 =
      Str.matched_group 2 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg2 =
      Str.matched_group 3 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let () = is_arg1_sup_eq_arg2 arg1 arg2 in
    Some (Scatter (arg1, arg2))
  else if Str.string_match re_map simulate_network_arg 0 then
    let arg1 =
      Str.matched_group 2 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg2 =
      Str.matched_group 3 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let arg3 =
      Str.matched_group 4 simulate_network_arg
      |> int_of_string |> is_positive_param
    in
    let () = is_arg1_sup_eq_arg2 arg1 (arg2 + arg3) in
    Some (Map (arg1, arg2, arg3))
  else
    Test.fail
      "Unexpected network simulation config (--simulation) [%s]"
      simulate_network_arg

let typ =
  Clap.typ ~name:"simulate_network" ~dummy:Disabled ~parse ~show:to_string
