(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module Cryptobox = Tezos_crypto_dal.Cryptobox

module Parameters : sig
  type t = {
    feature_enabled : bool;
    cryptobox : Cryptobox.parameters;
    number_of_slots : int;
    attestation_lag : int;
    attestation_threshold : int;
    blocks_per_epoch : int;
  }

  val parameter_file : Protocol.t -> string Lwt.t

  val from_protocol_parameters : JSON.t -> t

  val from_client : Client.t -> t Lwt.t
end

module Helpers : sig
  val endpoint : Dal_node.t -> string

  (** Abstract version of a slot to deal with messages content which
     are smaller than the expected size of a slot. *)
  type slot

  (** [make_slot ?paddming ~slot_size content] produces a slot. If [padding=true]
      (which is the default), then the content is padded to reach the expected
      size given by [slot_size] (which is usually obtained from
      {!type:Cryptobox.parameters}). *)
  val make_slot : ?padding:bool -> slot_size:int -> string -> slot

  (** [content_of_slot slot] retrieves the original content of a slot
     by removing the padding. *)
  val content_of_slot : slot -> string

  val make_cryptobox :
    ?on_error:(string -> Cryptobox.t) -> Cryptobox.parameters -> Cryptobox.t

  val publish_slot_header :
    ?counter:int ->
    ?force:bool ->
    ?source:Account.key ->
    ?fee:int ->
    ?error:rex ->
    index:int ->
    commitment:Cryptobox.commitment ->
    proof:Cryptobox.commitment_proof ->
    Client.t ->
    [`OpHash of string] Lwt.t

  (** This function builds a slot with the given content, and makes the given
      DAL node to compute and store the corresponding commitment and shards by
      calling relevant RPCs. It returns the commitment and its proof. *)
  val store_slot :
    (Dal_node.t, Endpoint.t) Either.t ->
    ?with_proof:bool ->
    slot ->
    (string * string) Lwt.t
end

module RPC_legacy : sig
  type default_uri_provider = (Dal_node.t, Endpoint.t) Either.t

  type local_uri_provider = Dal_node.t

  type remote_uri_provider = Endpoint.t

  (** [slot_pages slot_header] gets slot/pages of [slot_header] *)
  val slot_pages : string -> string list RPC_core.t

  (** [shard ~slot_header ~shard_id] gets a shard from
        a given slot header and shard id *)
  val shard : slot_header:string -> shard_id:int -> string RPC_core.t

  (** [shards ~slot_header shard_ids] gets a subset of shards from a given
        slot header *)
  val shards : slot_header:string -> int list -> string list RPC_core.t
end

module RPC : sig
  include module type of RPC_legacy

  type commitment = string

  (** Profiles that operate on shards/slots. *)
  type operator_profile = Attester of string | Producer of int

  (** List of operator profiles.  *)
  type operator_profiles = operator_profile list

  (* Profiles tracked by the DAL node. *)
  type profiles = Bootstrap | Operator of operator_profiles

  (** Information contained in a slot header fetched from the DAL node. *)
  type slot_header = {
    slot_level : int;
    slot_index : int;
    commitment : string;
    status : string;
  }

  (** [slot_header_of_json json] decodes [json] as a slot header. The function
        fails if the given [json] cannot be decoded. *)
  val slot_header_of_json : JSON.t -> slot_header

  (** [slot_header_of_json json_] similar to {!slot_header_of_json}, but
        the input (and output) is expected to be a list. *)
  val slot_headers_of_json : JSON.t -> slot_header list

  (** Call RPC "POST /commitments" to store a slot and retrun the commitment
        in case of success. *)
  val post_commitment : Helpers.slot -> commitment RPC_core.t

  (** Call RPC "PATCH /commitments" to associate the given level and index to the slot
        whose commitment is given. *)
  val patch_commitment :
    commitment -> slot_level:int -> slot_index:int -> unit RPC_core.t

  (** Call RPC "GET /commitments/<commitment>/slot" to retrieve the slot
        content associated with the given commitment. *)
  val get_commitment_slot : commitment -> Helpers.slot RPC_core.t

  (** Call RPC "PUT /commitments/<commitment>/shards" to compute and store the
        shards of the slot whose commitment is given, using the current DAL
        parameters. Note that [with_proof], whose default value is [false], is
        provided as input to the RPC. *)
  val put_commitment_shards : ?with_proof:bool -> commitment -> unit RPC_core.t

  type commitment_proof = string

  (** Call RPC "GET /commitments/<commitment>/proof" to get the proof
       associated to a commitment. *)
  val get_commitment_proof : commitment -> commitment_proof RPC_core.t

  (** Call RPC "GET
        /levels/<published_level>/slot_indices/<slot_index>/commitment" to get
        the commitment associated to the given level and index. *)
  val get_level_index_commitment :
    slot_level:int -> slot_index:int -> commitment RPC_core.t

  (**  Call RPC "PATCH /profiles" to update the list of profiles tracked by
         the DAL node. *)
  val patch_profiles : operator_profiles -> unit RPC_core.t

  (**  Call RPC "GET /profiles" to retrieve the list of profiles tracked by
         the DAL node. *)
  val get_profiles : unit -> profiles RPC_core.t

  (** Call RPC "GET /commitments/<commitment>/headers" to get the headers and
        statuses known about the given commitment. The resulting list can be filtered by a
        given header publication level and slot index. *)
  val get_commitment_headers :
    ?slot_level:int ->
    ?slot_index:int ->
    commitment ->
    slot_header list RPC_core.t

  (** Call RPC "GET
        /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
        to get shard ids assigned to the given public key hash at the given
        level. *)
  val get_assigned_shard_indices :
    level:int -> pkh:string -> int list RPC_core.t

  (** Call RPC "GET /levels/<published_level>/headers?status" to get the known
        headers with the given published level. *)
  val get_published_level_headers :
    ?status:string -> int -> slot_header list RPC_core.t

  type slot_set = bool list

  type attestable_slots = Not_in_committee | Attestable_slots of slot_set

  (** Call RPC "GET
        /profiles/<public_key_hash>/attested_levels/<level>/attestable_slots" to
        get the slots currently attestable by the given public key hash at the
        given attested level. The result is either a [Not_in_committee] or a
        [Attestable_slots flags], where [flags] is a boolean list of length
        [num_slots]. A slot is attestable if it is published at level [level -
        attestation_lag]) and all the shards assigned to the given attester at
        level [level] are available in the DAL node's store. *)
  val get_attestable_slots :
    attester:Account.key -> attested_level:int -> attestable_slots RPC_core.t

  (**  Call RPC "DELETE /p2p/peers/disconnect" to disconnect the node whose
       identity is given. *)
  val delete_p2p_peer_disconnect : peer_id:string -> unit RPC_core.t

  module Local : RPC_core.CALLERS with type uri_provider := local_uri_provider

  module Remote : RPC_core.CALLERS with type uri_provider := remote_uri_provider
end

module Commitment : sig
  val dummy_commitment :
    ?on_error:
      ([ `Invalid_degree_strictly_less_than_expected of
         (int, int) Cryptobox.error_container
       | `Slot_wrong_size of Helpers.slot ] ->
      Cryptobox.commitment * Cryptobox.commitment_proof) ->
    Cryptobox.t ->
    string ->
    Cryptobox.commitment * Cryptobox.commitment_proof

  val to_string : Cryptobox.commitment -> string

  val of_string : string -> Cryptobox.commitment

  val proof_to_string : Cryptobox.commitment_proof -> string

  val proof_of_string : string -> Cryptobox.commitment_proof
end

module Committee : sig
  type member = {attester : string; first_shard_index : int; power : int}

  type t = member list

  val typ : t Check.typ

  val at_level : Node.t -> level:int -> t Lwt.t
end

module Check : sig
  val profiles_typ : RPC.profiles Check.typ
end
