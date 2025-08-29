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
