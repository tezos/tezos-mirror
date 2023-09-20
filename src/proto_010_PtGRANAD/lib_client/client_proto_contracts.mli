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

open Protocol
open Alpha_context

module Raw_contract_alias : Client_aliases.Alias with type t = Contract.t

module Contract_alias : sig
  val get_contract :
    #Client_context.wallet -> string -> (string * Contract.t) tzresult Lwt.t

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (string * Contract.t -> 'a, 'wallet) Tezos_clic.params

  val find_destination :
    #Client_context.wallet -> string -> (string * Contract.t) tzresult Lwt.t

  val destination_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) Tezos_clic.params ->
    (string * Contract.t -> 'a, 'wallet) Tezos_clic.params

  val destination_arg :
    ?name:string ->
    ?doc:string ->
    unit ->
    ((string * Contract.t) option, #Client_context.wallet) Tezos_clic.arg

  val rev_find :
    #Client_context.wallet -> Contract.t -> string option tzresult Lwt.t

  val name : #Client_context.wallet -> Contract.t -> string tzresult Lwt.t

  val autocomplete : #Client_context.wallet -> string list tzresult Lwt.t
end

val list_contracts :
  #Client_context.wallet ->
  (string * string * Raw_contract_alias.t) list tzresult Lwt.t

val get_delegate :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  public_key_hash option tzresult Lwt.t
