(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

type t = {
  from : address;
  to_ : address;
  gas : quantity;
  gasPrice : quantity;
  value : quantity option;
  data : hash;
  nonce : quantity option;
}

val encoding : t Data_encoding.t

val hash_raw_tx : string -> string

(** [nonce_of_rlp_raw_tx bytes] returns the nonce of a given raw
    transaction. *)
val nonce_of_rlp_raw_tx : string -> Z.t tzresult

(** [gas_price_of_rlp_raw_tx base_fee bytes] returns the maximum gas
    price the user can pay for the tx. *)
val gas_price_of_rlp_raw_tx : string -> Z.t tzresult

(** [gas_limit_of_rlp_raw_tx bytes] returns the gas limit of a given
    raw transaction. *)
val gas_limit_of_rlp_raw_tx : string -> Z.t tzresult

(** [data_of_rlp_raw_tx bytes] returns the data of a given raw
    transaction. *)
val data_of_rlp_raw_tx : string -> bytes tzresult
