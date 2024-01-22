(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_protocol_alpha.Protocol

let prepare_mint_and_deposit_contract ?hooks client protocol =
  let* _, mint_and_deposit_contract =
    Client.originate_contract_at
      ?hooks
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "smart_rollup_mint_and_deposit_ticket"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  return Contract_hash.(of_b58check_exn mint_and_deposit_contract)

let prepare_receive_withdrawn_tickets_contract ?hooks client protocol =
  let* _, receive_tickets_contract =
    Client.originate_contract_at
      ?hooks
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~init:"{}"
      ~burn_cap:Tez.(of_int 1)
      client
      ["mini_scenarios"; "smart_rollup_receive_tickets"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in
  return Contract_hash.(of_b58check_exn receive_tickets_contract)

let deposit_string_tickets ?hooks client ~mint_and_deposit_contract
    ~sc_rollup_address ~destination_l2_addr ~ticket_content ~amount =
  let* () =
    let arg =
      sf
        {|Pair (Pair %S %S) (Pair %d %S)|}
        sc_rollup_address
        destination_l2_addr
        amount
        ticket_content
    in
    Client.transfer
      ?hooks
      client
      ~amount:Tez.zero
      ~giver:Constant.bootstrap1.alias
      ~receiver:mint_and_deposit_contract
      ~arg
      ~burn_cap:(Tez.of_int 1000)
  in
  let* () = Client.bake_for_and_wait client in
  Lwt.return ()
