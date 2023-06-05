(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

(** This is the first version of the [Certificate_repr.t]. In the future
    if it is needed to add a field of modify type of an existing field,
      one must simply create a [V1] module with the new definition 
    of [type t]. *)
module V0 : sig
  (** Representation of a Data Availibility Committee Certificate.
     Type is private to make sure correct [version] is used.
     Use [make] function to create a [Certificate_repr.V0.t]. *)
  type t

  (** Create a [Certificate_repr.V0.t] from given [Dac_plugin.raw_hash],
     [Tezos_crypto.Aggregate_signature.signature] and [Z.t].
     This function is in charge to add the correct [version]. *)
  val make :
    Dac_plugin.raw_hash ->
    Tezos_crypto.Aggregate_signature.signature ->
    Z.t ->
    t

  (** All the following functions are related to the Protocol.
      They are used only by the `command_handlers` module
      exposing CLI commands for Kernel SDK. *)
  module Protocol_dependant : sig
    (** Serialize the provided [Dac_plugin.hash], 
        [Tezos_crypto.Aggregate_signature.signature] and [Z.t] 
        with the correct encoding. [Dac_plugin.t] is needed to provide 
        the correct encoding related to the correct protocol. *)
    val serialize_certificate :
      Dac_plugin.hash Data_encoding.t ->
      root_hash:Dac_plugin.hash ->
      aggregate_signature:Tezos_crypto.Aggregate_signature.signature ->
      witnesses:Z.t ->
      Bytes.t
  end
end

type t = V0 of V0.t

(** Used to return any version of [Certificate_repr] on 
     DAC RPC endpoints. *)
val encoding : t Data_encoding.t

(** Helper to get [root_hash] from any given version of [Certificate_repr]. *)
val get_root_hash : t -> Dac_plugin.raw_hash

(** Helper to get [aggregate_signature] from any given version of [Certificate_repr]. *)
val get_aggregate_signature : t -> Tezos_crypto.Aggregate_signature.signature

(** Helper to get [witnesses] from any given version of [Certificate_repr]. *)
val get_witnesses : t -> Z.t

(** Helper to get [version] from any given version of [Certificate_repr]. *)
val get_version : t -> int

(** [Certificate_repr.all_committee_members_have_signed committee_members certificate] 
     will return [true] if all members have signed, otherwise it will return [false]. *)
val all_committee_members_have_signed : 'a list -> t -> bool
