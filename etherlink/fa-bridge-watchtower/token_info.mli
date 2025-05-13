(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Token information retrieval for both native and FA tokens *)

(** [get_for_display ws_client token_addr amount] retrieves token information and
    formats the amount for display.

    The amount is converted from raw units to display units using the token's
    decimals (if available).

    @param ws_client Websocket client for querying token contracts
    @param token_addr The address for the FA token
    @param amount The amount in raw units
    @return Tuple of (formatted amount, token symbol)
*)
val get_for_display :
  Websocket_client.t ->
  Ethereum_types.address ->
  Ethereum_types.quantity ->
  (float * string) tzresult Lwt.t

(** [get_for_rpc ws_client token_addr amount] retrieves token information for
    RPC responses.

    The amount is converted from raw units to display units using the token's
    decimals (if available). Unlike [get_for_display], this doesn't use the
    address as the token symbol when it cannot be retrieved.

    @param ws_client Websocket client for querying token contracts
    @param token_addr The address for the FA token
    @param amount The amount in raw units
    @return Tuple of (formatted amount, optional token symbol)
*)
val get_for_rpc :
  Websocket_client.t ->
  Ethereum_types.address ->
  Ethereum_types.quantity ->
  (float * string option) tzresult Lwt.t
