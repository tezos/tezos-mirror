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

module Dal : sig
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

    val from_client : Client.t -> t Lwt.t

    (** [cryptobox_config_to_json config] returns the Json encoding of
     the record {!type:Cryptobox.Config.t}. *)
    val cryptobox_config_to_json : Cryptobox.Config.t -> JSON.t
  end

  val endpoint : Dal_node.t -> string

  (** Abstract version of a slot to deal with messages content which
     are smaller than the expected size of a slot. *)
  type slot

  (** [make_slot ?padding ~slot_size content] produces a slot. If [padding=true]
      (which is the default), then the content is padded to reach the expected
      size given by [slot_size] (which is usually obtained from
      {!type:Cryptobox.parameters}). *)
  val make_slot : ?padding:bool -> slot_size:int -> string -> slot

  (** [content_of_slot slot] retrieves the original content of a slot
     by removing the padding. *)
  val content_of_slot : slot -> string

  module RPC_legacy : sig
    (** [slot_pages slot_header] gets slot/pages of [slot_header] *)
    val slot_pages : string -> (Dal_node.t, string list) RPC_core.t

    (** [shard ~slot_header ~shard_id] gets a shard from
        a given slot header and shard id *)
    val shard :
      slot_header:string -> shard_id:int -> (Dal_node.t, string) RPC_core.t

    (** [shards ~slot_header shard_ids] gets a subset of shards from a given
        slot header *)
    val shards :
      slot_header:string -> int list -> (Dal_node.t, string list) RPC_core.t
  end

  module RPC : sig
    include module type of RPC_legacy

    type commitment = string

    type profile = Attestor of string

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
    val post_commitment : slot -> (Dal_node.t, commitment) RPC_core.t

    (** Call RPC "PATCH /commitments" to associate the given level and index to the slot
        whose commitment is given. *)
    val patch_commitment :
      commitment ->
      slot_level:int ->
      slot_index:int ->
      (Dal_node.t, unit) RPC_core.t

    (** Call RPC "GET /commitments/<commitment>/slot" to retrieve the slot
        content associated with the given commitment. *)
    val get_commitment_slot : commitment -> (Dal_node.t, slot) RPC_core.t

    (** Call RPC "PUT /commitments/<commitment>/shards" to compute and store the
        shards of the slot whose commitment is given, using the current DAL
        parameters. Note that [with_proof], whose default value is [false], is
        provided as input to the RPC. *)
    val put_commitment_shards :
      ?with_proof:bool -> commitment -> (Dal_node.t, unit) RPC_core.t

    type commitment_proof = string

    (** Call RPC "GET /commitments/<commitment>/proof" to get the proof
       associated to a commitment. *)
    val get_commitment_proof :
      commitment -> (Dal_node.t, commitment_proof) RPC_core.t

    (** Call RPC "GET
        /levels/<published_level>/slot_indices/<slot_index>/commitment" to get
        the commitment associated to the given level and index. *)
    val get_level_index_commitment :
      slot_level:int -> slot_index:int -> (Dal_node.t, commitment) RPC_core.t

    (**  Call RPC "PATCH /profiles" to update the list of profiles tracked by
         the DAL node. *)
    val patch_profile : profile -> (Dal_node.t, unit) RPC_core.t

    (**  Call RPC "GET /profiles" to retrieve the list of profiles tracked by
         the DAL node. *)
    val get_profiles : unit -> (Dal_node.t, profile list) RPC_core.t

    (** Call RPC "GET /commitments/<commitment>/headers" to get the headers and
        statuses know about the given commitment. The resulting list can be filtered by a
        given header publication level and slot index. *)
    val get_commitment_headers :
      ?slot_level:int ->
      ?slot_index:int ->
      commitment ->
      (Dal_node.t, slot_header list) RPC_core.t

    (** Call RPC "GET
        /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
        to get shard ids assigned to the given public key hash at the given
        level. *)
    val get_assigned_shard_indices :
      level:int -> pkh:string -> (Dal_node.t, int list) RPC_core.t

    (** Call RPC "GET /levels/<published_level>/headers?status" to get the known
        headers with the given published level. *)
    val get_published_level_headers :
      ?status:string -> int -> (Dal_node.t, slot_header list) RPC_core.t

    type slot_set = bool list

    type attestable_slots = Not_in_committee | Attestable_slots of slot_set

    (** Call RPC "GET
        /profiles/<public_key_hash>/attested_levels/<level>/attestable_slots" to
        get the slots currently attestable by the given public key hash at the
        given attested level. The result is either a [Not_in_committee] or a
        [Attestable_slots flags], where [flags] is a boolean list of length
        [num_slots]. A slot is attestable if it is published at level [level -
        attestation_lag]) and all the shards assigned to the given attestor at
        level [level] are available in the DAL node's store. *)
    val get_attestable_slots :
      attestor:Account.key ->
      attested_level:int ->
      (Dal_node.t, attestable_slots) RPC_core.t
  end

  val make :
    ?on_error:(string -> Cryptobox.t) -> Cryptobox.parameters -> Cryptobox.t

  module Commitment : sig
    val dummy_commitment :
      ?on_error:
        ([ `Invalid_degree_strictly_less_than_expected of
           (int, int) Cryptobox.error_container
         | `Slot_wrong_size of slot ] ->
        Cryptobox.commitment * Cryptobox.commitment_proof) ->
      Cryptobox.t ->
      string ->
      Cryptobox.commitment * Cryptobox.commitment_proof

    val to_string : Cryptobox.commitment -> string
  end

  module Committee : sig
    type member = {attestor : string; first_shard_index : int; power : int}

    type t = member list

    val typ : t Check.typ

    val at_level : Node.t -> level:int -> t Lwt.t
  end

  module Check : sig
    type profiles = RPC.profile list

    val profiles_typ : profiles Check.typ
  end
end
