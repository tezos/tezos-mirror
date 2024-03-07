(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2024 Trilitech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context

module Raw_contract_alias : Client_aliases.Alias with type t = Contract.t

(** Like [Contract_alias] below but restricted to originated contracts. *)
module Originated_contract_alias : sig
  val find_destination :
    #Client_context.wallet -> string -> Contract_hash.t tzresult Lwt.t

  val destination_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (Contract_hash.t -> 'a, 'wallet) Tezos_clic.params

  val destination_arg :
    ?name:string ->
    ?doc:string ->
    unit ->
    (Contract_hash.t option, #Client_context.wallet) Tezos_clic.arg
end

module Contract_alias : sig
  val get_contract :
    #Client_context.wallet -> string -> Contract.t tzresult Lwt.t

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (Contract.t -> 'a, 'wallet) Tezos_clic.params

  val find_destination :
    #Client_context.wallet -> string -> Contract.t tzresult Lwt.t

  val destination_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (Contract.t -> 'a, 'wallet) Tezos_clic.params

  val destination_arg :
    ?name:string ->
    ?doc:string ->
    unit ->
    (Contract.t option, #Client_context.wallet) Tezos_clic.arg

  val rev_find :
    #Client_context.wallet -> Contract.t -> string option tzresult Lwt.t

  val name : #Client_context.wallet -> Contract.t -> string tzresult Lwt.t

  val autocomplete : #Client_context.wallet -> string list tzresult Lwt.t
end

module Destination_alias : sig
  val destination_parameter :
    unit -> (Destination.t, #Client_context.wallet) Tezos_clic.parameter

  val destination_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (Destination.t -> 'a, 'wallet) Tezos_clic.params
end

(** [list_contracts cctxt] returns the concatenation of [contracts] and [accounts]
    where [contracts] is the result of [Raw_contract_alias.load cctxt]
    and [accounts] the list of accounts (implicit contracts of identities)
*)
val list_contracts :
  #Client_context.wallet ->
  (string * string * Raw_contract_alias.t) list tzresult Lwt.t

(** Calls {!Alpha_services.Contract.delegate_opt} *)
val get_delegate :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  public_key_hash option tzresult Lwt.t
