(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** BLS commands are used by rollup clients to handle their keys directly. *)
module Bls_commands : sig
  (** [generate_keys ~force ~encrypted alias cctxt] generates a BLS
      based pair of keys with a fresh mnemonic with [alias] as
      alias. If [force] is [true], it will replace the alias if it
      already exists. If [encrypted] is [true], then it will ask for a
      passphrase, and encrypt the generated key. *)
  val generate_keys :
    force:bool ->
    encrypted:bool ->
    Client_keys.Aggregate_alias.Secret_key.fresh_param ->
    #Client_context.io_wallet ->
    unit tzresult Lwt.t

  (** [list_keys cctxt] lists the BLS keys known by the wallet. *)
  val list_keys : #Client_context.io_wallet -> unit tzresult Lwt.t

  (** [show_address alias] shows the address corresponding to given [alias]. *)
  val show_address : string -> #Client_context.io_wallet -> unit tzresult Lwt.t

  (** [import_secret_key ~force alias uri cctxt] imports a secret key from [uri]
      as [alias] in the wallet. If [force] is [true], it will replace the alias
      if it already exists. *)
  val import_secret_key :
    force:bool ->
    Client_keys.Aggregate_alias.Secret_key.fresh_param ->
    Client_keys.aggregate_sk_uri ->
    #Client_context.io_wallet ->
    unit tzresult Lwt.t
end

val commands :
  [`Mainnet | `Testnet] option -> Client_context.full Clic.command list
