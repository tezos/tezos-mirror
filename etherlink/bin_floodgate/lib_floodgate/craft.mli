(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Craft a transaction from [from] to [to_] with amount [value]. The
    returned value is a signed transaction ready to be injected with
    [Rpc.send_transaction_batch]. *)
val transfer :
  ?nonce:Z.t ->
  ?to_:Efunc_core.Eth.address ->
  ?data:Efunc_core.Private.b ->
  value:Z.t ->
  gas_limit:Z.t ->
  infos:Network_info.t ->
  from:Account.t ->
  unit ->
  Ethereum_types.hex tzresult Lwt.t

(** Same as {!transfer}, but raises [Failure] instead of returning an
    [Error] when the transaction cannot be crafted. *)
val transfer_exn :
  ?nonce:Z.t ->
  ?to_:Efunc_core.Eth.address ->
  ?data:Efunc_core.Private.b ->
  value:Z.t ->
  gas_limit:Z.t ->
  infos:Network_info.t ->
  from:Account.t ->
  unit ->
  Ethereum_types.hex Lwt.t

(** Same as {!transfer_exn}, but also returns the decoded
  {!Transaction_object.t} corresponding to the signed transaction.
  Returns [Error _] if decoding the raw transaction fails, and may
  raise [Failure] if the transaction cannot be crafted. *)
val transfer_with_obj_exn :
  ?nonce:Z.t ->
  ?to_:Efunc_core.Eth.address ->
  ?data:Efunc_core.Private.b ->
  value:Z.t ->
  gas_limit:Z.t ->
  infos:Network_info.t ->
  from:Account.t ->
  unit ->
  (Ethereum_types.hex * Transaction_object.t) tzresult Lwt.t
