(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** A set of helper functions for creating and managing L1 smart contracts
    that are used for interacting with the tx-kernel. *)

open Tezos_protocol_alpha.Protocol

(** Originate a contract that will mint and transfer tickets to the tx kernel.
    Uses the contract [mini_scenarios/smart_rollup_mint_and_deposit_ticket.tz]. *)
val prepare_mint_and_deposit_contract :
  ?hooks:Tezt.Process.hooks -> Client.t -> Protocol.t -> Contract_hash.t Lwt.t

(** Originate ticket receiver contract that will receive withdrawals.
    Uses the contract [mini_scenarios/smart_rollup_receive_tickets.tz]. *)
val prepare_receive_withdrawn_tickets_contract :
  ?hooks:Tezt.Process.hooks -> Client.t -> Protocol.t -> Contract_hash.t Lwt.t

(** [deposit_string_tickets ?hooks client ~sc_rollup_address ~ticket_content
    ~mint_and_deposit_contract ~destination_l2_addr ~amount] deposits a string
    ticket into a rollup.

    @param client The L1 client.

    @param mint_and_deposit_contract The contract for minting and depositing tickets.
    This contract can be created by calling [prepare_receive_withdrawn_tickets_contract].

    @param sc_rollup_address The address of the smart-rollup to which we deposit the ticket.

    @param destination_l2_addr The L2 address to which we deposit the ticket.

    @param ticket_content The content of the ticket we deposit.

    @param amount The amount of the ticket we deposit.
*)
val deposit_string_tickets :
  ?hooks:Tezt.Process.hooks ->
  Client.t ->
  mint_and_deposit_contract:string ->
  sc_rollup_address:string ->
  destination_l2_addr:string ->
  ticket_content:string ->
  amount:int ->
  unit Lwt.t
