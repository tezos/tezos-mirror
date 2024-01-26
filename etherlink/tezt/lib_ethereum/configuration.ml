(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let default_bootstrap_account_balance = Wei.of_eth_int 9999

let make_config ?bootstrap_accounts ?ticketer ?administrator ?sequencer
    ?delayed_bridge ?(flat_fee = Wei.zero) () =
  let open Sc_rollup_helpers.Installer_kernel_config in
  let ticketer =
    Option.fold
      ~some:(fun ticketer ->
        let value = Hex.(of_string ticketer |> show) in
        let to_ = Durable_storage_path.ticketer in
        [Set {value; to_}])
      ~none:[]
      ticketer
  in
  let bootstrap_accounts =
    Option.fold
      ~some:
        (Array.fold_left
           (fun acc Eth_account.{address; _} ->
             let value =
               Wei.(to_le_bytes default_bootstrap_account_balance)
               |> Hex.of_bytes |> Hex.show
             in
             let to_ = Durable_storage_path.balance address in
             Set {value; to_} :: acc)
           [])
      ~none:[]
      bootstrap_accounts
  in
  let administrator =
    Option.fold
      ~some:(fun administrator ->
        let to_ = Durable_storage_path.admin in
        let value = Hex.(of_string administrator |> show) in
        [Set {value; to_}])
      ~none:[]
      administrator
  in
  let sequencer =
    match sequencer with
    | Some secret_key ->
        let value = Hex.(of_string secret_key |> show) in
        [Set {value; to_ = Durable_storage_path.sequencer}]
    | None -> []
  in

  let delayed_bridge =
    Option.fold
      ~some:(fun delayed_bridge ->
        let to_ = Durable_storage_path.delayed_bridge_path in
        let value = Hex.(of_string delayed_bridge |> show) in
        [Set {value; to_}])
      ~none:[]
      delayed_bridge
  in
  let flat_fee =
    let to_ = Durable_storage_path.flat_fee_path in
    let value = Wei.(to_le_bytes flat_fee) |> Hex.of_bytes |> Hex.show in
    [Set {value; to_}]
  in
  match
    ticketer @ bootstrap_accounts @ administrator @ sequencer @ delayed_bridge
    @ flat_fee
  with
  | [] -> None
  | res -> Some (`Config res)
