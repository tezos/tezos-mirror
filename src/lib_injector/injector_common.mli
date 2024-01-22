(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(*****************************************************************************)

(** The type of signers for operations injected by the injector *)
type signer = {
  alias : string;
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Client_keys.sk_uri;
}

(** Retrieve a signer from the client wallet. *)
val get_signer :
  #Client_context.wallet -> Signature.public_key_hash -> signer tzresult Lwt.t

(** tez representation *)
type tez = {mutez : int64}

(** fee parameters for each operation type *)
type fee_parameter = {
  minimal_fees : tez;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : tez;
  burn_cap : tez;
}

(** [fee_parameter_encoding default_parameter_encoding] generates the
    encoding using default value from [default_fee_parameter] *)
val fee_parameter_encoding :
  default_fee_parameter:fee_parameter -> fee_parameter Data_encoding.t
