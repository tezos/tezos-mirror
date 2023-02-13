(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** [Protocol.Sc_protocol_reveal_hash.t] is unknown to  modules outside the 
    protocol and only known at runtime. To avoid the proliferation of functors
    in the dac node, [Dac_hash.t] hides the dynamic [Protocol.Sc_protocol_reveal_hash.t]
    behind an abstract static type. An instance of [Dac_plugin.T] implement
    the [Dac_plugin.Dac_hash] which are the functions that can operate on [Dac_hash.t].
  *)
module Dac_hash : sig
  type t

  val to_bytes : t -> bytes
end

module type T = sig
  (** [Dac_hash.t] operations that need to be derived from the protocol *)
  module Dac_hash : sig
    val encoding : Dac_hash.t Data_encoding.t
  end

  module Proto : Registered_protocol.T

  module RPC : sig
    val rpc_services :
      reveal_data_dir:string ->
      #Client_context.wallet ->
      Tezos_crypto.Aggregate_signature.public_key option list ->
      Client_keys.aggregate_sk_uri option list ->
      int ->
      unit Tezos_rpc.Directory.directory
  end
end

(** [register make_plugin] derives and registers a new [Dac_plugin.T] given an 
    [of_bytes]. Implementers of plugin are responsible for providing the 
    definition of this derivation. Functions that expose 
    [Protocol.Sc_protocol_reveal_hash.t] can be wrapped into [Dac_hash.t] via 
    [Dac_hash.to_bytes] and [of_bytes].
*)
val register : ((bytes -> Dac_hash.t) -> (module T)) -> unit

val get : Protocol_hash.Table.key -> (module T) option
