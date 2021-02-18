(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
open Clic

module Raw_contract_alias : Client_aliases.Alias with type t = Contract.t

module Contract_alias : sig
  val get_contract :
    #Client_context.wallet -> string -> (string * Contract.t) tzresult Lwt.t

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) params ->
    (string * Contract.t -> 'a, 'wallet) params

  val find_destination :
    #Client_context.wallet -> string -> (string * Contract.t) tzresult Lwt.t

  val destination_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'wallet)) params ->
    (string * Contract.t -> 'a, 'wallet) params

  val destination_arg :
    ?name:string ->
    ?doc:string ->
    unit ->
    ((string * Contract.t) option, #Client_context.wallet) Clic.arg

  val rev_find :
    #Client_context.wallet -> Contract.t -> string option tzresult Lwt.t

  val name : #Client_context.wallet -> Contract.t -> string tzresult Lwt.t

  val autocomplete : #Client_context.wallet -> string list tzresult Lwt.t
end

module Baker_alias : sig
  type t = Baker_hash.t

  val load : #Client_context.wallet -> (string * t) list tzresult Lwt.t

  val rev_find : #Client_context.wallet -> t -> string option tzresult Lwt.t

  val name : #Client_context.wallet -> t -> string tzresult Lwt.t

  val to_source : t -> string tzresult Lwt.t

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Clic.params ->
    (string * t -> 'a, 'b) Clic.params

  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'obj)) Clic.params ->
    (t -> 'a, 'obj) Clic.params

  val source_arg :
    ?long:string ->
    ?placeholder:string ->
    ?doc:string ->
    unit ->
    (t option, (#Client_context.wallet as 'obj)) Clic.arg
end

(** This helper alias accepts both `Signature.Public_key_hash` and `Baker_hash`
    aliases for backward compatibility.
*)
module Baker_or_pkh_alias : sig
  type t = Contract.t

  val load : #Client_context.wallet -> (string * t) list tzresult Lwt.t

  val name : #Client_context.wallet -> t -> string tzresult Lwt.t

  val alias_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'b)) Clic.params ->
    (string * t -> 'a, 'b) Clic.params

  val source_param :
    ?name:string ->
    ?desc:string ->
    ('a, (#Client_context.wallet as 'obj)) Clic.params ->
    (t -> 'a, 'obj) Clic.params

  val source_arg :
    ?long:string ->
    ?placeholder:string ->
    ?doc:string ->
    unit ->
    (Contract.t option, (#Client_context.wallet as 'obj)) Clic.arg
end

val list_contracts :
  #Client_context.wallet ->
  (string * string * Raw_contract_alias.t) list tzresult Lwt.t

val get_delegate :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  baker_hash option tzresult Lwt.t

val get_baker_consensus_key :
  ?level:Raw_level.t ->
  ?offset:int32 ->
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  baker_hash ->
  (string * public_key_hash * public_key * Client_keys.sk_uri) tzresult Lwt.t

(** Baker accounts are now identified by baker hashes instead of PKHs.
    For backward compatibility with the previous protocol, the client accepts
    any contract and attempts to convert it to baker hash using this function.
*)
val baker_of_contract :
  #Protocol_client_context.full ->
  Contract.t ->
  Baker_hash.t Error_monad.tzresult Lwt.t

val get_source_keys :
  #Protocol_client_context.full ->
  Contract.t ->
  (string * public_key_hash * public_key * Client_keys.sk_uri) option tzresult
  Lwt.t
