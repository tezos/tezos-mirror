(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let default_bootstrap_account_balance = Wei.of_eth_int 9999

let make_config ?bootstrap_accounts ?ticketer ?administrator
    ?sequencer_administrator ?sequencer ?delayed_bridge
    ?(da_fee_per_byte = Wei.zero) ?delayed_inbox_timeout
    ?delayed_inbox_min_levels () =
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
  let sequencer_administrator =
    Option.fold
      ~some:(fun sequencer_administrator ->
        let to_ = Durable_storage_path.sequencer_admin in
        let value = Hex.(of_string sequencer_administrator |> show) in
        [Set {value; to_}])
      ~none:[]
      sequencer_administrator
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
  let da_fee_per_byte =
    let to_ = Durable_storage_path.da_fee_per_byte_path in
    let value = Wei.(to_le_bytes da_fee_per_byte) |> Hex.of_bytes |> Hex.show in
    [Set {value; to_}]
  in
  let delayed_inbox_timeout =
    match delayed_inbox_timeout with
    | Some timeout ->
        let bytes = Z.of_int timeout |> Z.to_bits in
        let value = Hex.(of_string bytes |> show) in
        [Set {value; to_ = Durable_storage_path.delayed_inbox_timeout}]
    | None -> []
  in
  let delayed_inbox_min_levels =
    match delayed_inbox_min_levels with
    | Some min_levels ->
        let bytes = Z.of_int min_levels |> Z.to_bits in
        let value = Hex.(of_string bytes |> show) in
        [Set {value; to_ = Durable_storage_path.delayed_inbox_min_levels}]
    | None -> []
  in
  match
    ticketer @ bootstrap_accounts @ administrator @ sequencer_administrator
    @ sequencer @ delayed_bridge @ da_fee_per_byte @ delayed_inbox_timeout
    @ delayed_inbox_min_levels
  with
  | [] -> None
  | res -> Some (`Config res)
