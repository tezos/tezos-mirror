(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Token information retrieval for both native and FA tokens *)

(** Type representing token information *)
type t = {
  symbol : string;  (** Token symbol (e.g., "XTZ", "USDC") *)
  decimals : int option;  (** Number of decimals for the token, if available *)
}

(** [get ws_client kind] retrieves token information for the given withdrawal
    kind.
    For XTZ, returns hardcoded values.
    For FA tokens, queries the token contract for symbol and decimals. If the
    symbol cannot be retrieved, it returns the address of the contract instead.
    Caches results to avoid repeated queries.

    @param ws_client Websocket client for querying token contracts
    @param kind The withdrawal kind (XTZ or FA token)
    @return Tuple of (Token information, fast withdrawal)
*)
val get : Websocket_client.t -> Db.withdrawal_kind -> (t * bool) tzresult Lwt.t

(** [get_for_display ws_client kind amount] retrieves token information and
    formats the amount for display.

    The amount is converted from raw units to display units using the token's
    decimals (if available). For example, 1000000000000000000 native tokens
    becomes 1.0 XTZ.

    @param ws_client Websocket client for querying token contracts
    @param kind The withdrawal kind (XTZ or FA token)
    @param amount The amount in raw units
    @return Tuple of (formatted amount, token symbol, fast), [fast = true] for
      fast withdrawals
*)
val get_for_display :
  Websocket_client.t ->
  Db.withdrawal_kind ->
  Ethereum_types.quantity ->
  (float * string * bool) tzresult Lwt.t
