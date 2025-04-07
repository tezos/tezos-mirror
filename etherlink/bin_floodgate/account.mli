(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Libsecp256k1.External

module Secret_key : sig
  type t = Key.secret Key.t

  val from_hex_string : string -> t tzresult

  val fresh : unit -> t
end

module Public_key : sig
  type t = Key.public Key.t

  val from_sk : Secret_key.t -> t

  val to_address : t -> Ethereum_types.address
end

type t = {
  mutable nonce : Z.t;
      (** The nonce of the account, as far as Floodgate is concerned. The nonce
          of an account is incremented by the {!Tx_queue} every time a
          transaction it has injected is accepted. *)
  mutable balance : Z.t;
      (** An underapproximation of the balance of an account, based on its
      initial balance minus the fees and the funds it has transfered to
      other addresses. This value is updated by the {!Tx_queue} every time
      a transaction it has injected is accepted. *)
  public_key : Public_key.t;
  secret_key : Secret_key.t;
}

val from_secret_key :
  evm_node_endpoint:Uri.t -> Secret_key.t -> t tzresult Lwt.t

val fresh : unit -> t

val increment_nonce : t -> unit

val credit : t -> Z.t -> unit

val debit : t -> Z.t -> unit

val address : t -> Efunc_core.Types.address

val address_et : t -> Ethereum_types.address

val pp_pkh : Format.formatter -> t -> unit
