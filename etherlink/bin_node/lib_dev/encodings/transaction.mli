(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type transaction_type = Legacy | Eip2930 | Eip1559

(** Payload of an Ethereum transaction.

    Note that nonces, signature and all are not wrapped in usual
    {!quantity}, {!hash} types etc. It is not relevant to wrap them
    as this type is not returned in any RPC and we do not need to make sure
    they are hexadecimal values. On the contrary, we prefer to keep binary bytes
    to facilitate signature verification.
 *)

type access_list_item = bytes * bytes list

type transaction = {
  transaction_type : transaction_type;
  chain_id : Z.t option;
  nonce : Z.t;
  max_priority_fee_per_gas : Z.t;
  max_fee_per_gas : Z.t;
  gas_limit : Z.t;
  to_ : bytes option;
  value : Z.t;
  data : bytes;
  access_list : access_list_item list;
      (** Access list are not yet supported. Even if the kernel does not
          support them, we need to decode them to verify the signature. *)
  v : Z.t;
  r : bytes;
  s : bytes;
}

(** [decode_legacy bytes] tries to decode [bytes] into a {!transaction}. *)
val decode_legacy : bytes -> (transaction, string) result

(** [decode_eip1559 bytes] tries to decode [bytes] into a {!transaction}. *)
val decode_eip1559 : bytes -> (transaction, string) result

(** [decode_eip2930 bytes] tries to decode [bytes] into a {!transaction}. *)
val decode_eip2930 : bytes -> (transaction, string) result

(** [decode bytes] tries to decode [bytes] into a {!transaction},
    using {!decode_eip1559}, {!decode_eip2930} or {!decode_legacy}
    depending of the first byte of [bytes]. *)
val decode : string -> (transaction, string) result

(** [to_transaction_object ~hash transaction] transforms a [transaction] and
    its [hash] to a {!Ethereum_types.transaction_object}. *)
val to_transaction_object :
  hash:Ethereum_types.hash ->
  transaction ->
  (Ethereum_types.legacy_transaction_object, string) result
