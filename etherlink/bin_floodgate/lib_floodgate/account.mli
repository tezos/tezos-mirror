(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

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
  signer : Signer.t;
}

val from_signer : evm_node_endpoint:Uri.t -> Signer.t -> t tzresult Lwt.t

val fresh : unit -> t

val increment_nonce : t -> unit

val credit : t -> Z.t -> unit

val debit : t -> Z.t -> unit

val address : t -> Efunc_core.Types.address

val address_et : t -> Ethereum_types.address

val pp_pkh : Format.formatter -> t -> unit
