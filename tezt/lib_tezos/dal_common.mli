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
    incentives_enabled : bool;
    cryptobox : Cryptobox.parameters;
    number_of_slots : int;
    attestation_lag : int;
    attestation_threshold : int;
    blocks_per_epoch : int;
  }

  val parameter_file : Protocol.t -> string Lwt.t

  val from_protocol_parameters : JSON.t -> t

  val from_client : Client.t -> t Lwt.t

  val from_endpoint : Endpoint.t -> t Lwt.t

  (* This function computes the period (in cycles) during which the node stores
     data about attested slots assuming the node supports refutations and it has
     been start sufficiently far in the past. See the functions
     [Profile_manager.get_attested_data_default_store_period] and
     [Daemon.get_storage_period] in src/bin_dal_node/. *)
  val full_storage_period_with_refutation_in_cycles :
    proto_parameters:JSON.t -> int

  (* This function computes the period (in cycles) during which the node stores
     data about attested slots assuming the node supports refutations and it
     hash just been started. See the functions
     [Profile_manager.get_attested_data_default_store_period] and
     [Daemon.get_storage_period] in src/bin_dal_node/. *)
  val initial_storage_period_with_refutation_in_cycles :
    proto_parameters:JSON.t -> int

  (* This function computes the period (in cycles) during which the node stores
     data about attested slots assuming the node does not supports
     refutations. See the functions
     [Profile_manager.get_attested_data_default_store_period] and
     [Daemon.get_proto_plugins] src/bin_dal_node/. *)
  val storage_period_without_refutation_in_cycles :
    proto_parameters:JSON.t -> int
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

  (** [slot_of_pages ~slot_size pages] produces a slot from a list of
      pages. The length of the result is checked against the given
      [slot_size]. *)
  val slot_of_pages : slot_size:int -> string list -> slot

  val make_cryptobox :
    ?on_error:(string -> Cryptobox.t Lwt.t) ->
    Cryptobox.parameters ->
    Cryptobox.t Lwt.t

  val publish_commitment :
    ?dont_wait:bool ->
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
    Dal_node.t -> slot_index:int -> slot -> (string * string) Lwt.t

  val store_slot_uri :
    Endpoint.t -> slot_index:int -> slot -> (string * string) Lwt.t

  (* Publish and store the slot on the corresponding DAL node. *)
  val publish_and_store_slot :
    ?dont_wait:bool ->
    ?counter:int ->
    ?force:bool ->
    ?fee:int ->
    Client.t ->
    Dal_node.t ->
    Account.key ->
    index:int ->
    slot ->
    string Lwt.t

  val pp_cryptobox_error :
    Format.formatter ->
    [ `Fail of string
    | `Invalid_degree_strictly_less_than_expected of 'a
    | `Invalid_page
    | `Invalid_shard_length of string
    | `Not_enough_shards of string
    | `Page_index_out_of_range
    | `Page_length_mismatch
    | `Shard_index_out_of_range of string
    | `Slot_wrong_size of string
    | `Shard_length_mismatch
    | `Prover_SRS_not_loaded
    | `Invalid_shard ] ->
    unit

  (** Wait for a connection event between [main_node] and
    [other_node]. The optional argument [other_peer_id] can be used to
    ignore the connection events which are not between these two
    nodes. When this optional argument is given, it must be the peer
    of [other_node]; this assumption is checked by this function
    after the reception of the connection event. *)
  val check_new_connection_event :
    main_node:Dal_node.t ->
    ?other_peer_id:string ->
    other_node:Dal_node.t ->
    is_trusted:bool ->
    unit ->
    unit Lwt.t

  (** Connect [dal_node1] and [dal_node2] using the bootstrap peer mechanism.
    [dal_node2] will use [dal_node1] as a bootstrap peer.
    For this to work, [dal_node1] must already be running.
    If [init_config] (false by default) is set to true, [Dal_node.init_config]
    will be performed for [dal_node2] with [dal_node1] as peer *)
  val connect_nodes_via_p2p :
    ?init_config:bool -> Dal_node.t -> Dal_node.t -> unit Lwt.t
end

module RPC : sig
  type default_uri_provider = (Dal_node.t, Endpoint.t) Either.t

  type local_uri_provider = Dal_node.t

  type remote_uri_provider = Endpoint.t

  type commitment = string

  (** Profiles that operate on shards/slots. *)
  type operator_profile =
    | Attester of string
    | Producer of int
    | Observer of int

  (** List of operator profiles.  *)
  type operator_profiles = operator_profile list

  (* Profiles tracked by the DAL node. *)
  type profile = Bootstrap | Operator of operator_profiles

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

  type commitment_proof = string

  (** Call RPC "POST /slots" to store a slot and return the commitment
        and its proof in case of success. *)
  val post_slot :
    ?slot_index:int ->
    Helpers.slot ->
    (commitment * commitment_proof) RPC_core.t

  (** Call RPC "GET /levels/<slot_level>/slot/<slot_index>/content" to retrieve
      the slot content associated with the given level and index. *)
  val get_level_slot_content :
    slot_level:int -> slot_index:int -> Helpers.slot RPC_core.t

  (** [get_level_slot_pages ~published_level ~slot_index] gets the pages
      of the slot published at level [published_level] on slot index
      [slot_index]. *)
  val get_level_slot_pages :
    published_level:int -> slot_index:int -> string list RPC_core.t

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
  val get_profiles : unit -> profile RPC_core.t

  (** Call RPC "GET /levels/<slot_level>/slots/<slot_index>/status" to
      get the status known about the given slot. *)
  val get_level_slot_status :
    slot_level:int -> slot_index:int -> string RPC_core.t

  (** Call RPC "GET
        /profiles/<public_key_hash>/attested_levels/<level>/assigned_shard_indices"
        to get shard ids assigned to the given public key hash at the given
        level. *)
  val get_assigned_shard_indices :
    level:int -> pkh:string -> int list RPC_core.t

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

  (**  Call RPC "PATCH /p2p/peers/by-id/<peer_id>" to patch the ACL of the node
       whose identity is given. Ignores the output of the RPC. *)
  val patch_p2p_peers_by_id :
    peer_id:string -> ?acl:string -> unit -> unit RPC_core.t

  type topic = {topic_slot_index : int; topic_pkh : string}

  (** Call RPC "GET /p2p/gossipsub/topics" to list the topics *)
  val get_topics : unit -> topic list RPC_core.t

  (** Call RPC "GET /p2p/gossipsub/topics/peers" to list the peers on
     each (subscribed) topic. *)
  val get_topics_peers :
    subscribed:bool -> (topic * string list) list RPC_core.t

  (** Call RPC "GET /p2p/gossipsub/slot_indexes/peers" to list the peers on
      each slot index part of a (subscribed) topic. *)
  val get_slot_indexes_peers :
    subscribed:bool -> (int * string list) list RPC_core.t

  (** Call RPC "GET /p2p/gossipsub/pkhs/peers" to list the peers on
     each pkh part of a (subscribed) topic. *)
  val get_pkhs_peers : subscribed:bool -> (string * string list) list RPC_core.t

  val get_gossipsub_connections : unit -> JSON.t RPC_core.t

  type peer_score = {peer : string; score : float}

  (** Call RPC "GET /p2p/gossipsub/scores" to list the scores of peers with a
     known score. *)
  val get_scores : unit -> peer_score list RPC_core.t

  (** Call RPC /plugin/[proto_hash]/<commitments_history/hash/[hash]. *)
  val get_plugin_commitments_history_hash :
    proto_hash:string -> hash:string -> unit -> JSON.t RPC_core.t

  (** Call /levels/<slot_level>/slots/<slot_index>/shards/<shard_index>/content *)
  val get_level_slot_shard_content :
    slot_level:int -> slot_index:int -> shard_index:int -> string RPC_core.t

  (** [unistring_to_json s] converts a possibly invalid UTF-8 string
      into a JSON object using Data-encoding's unistring
      representation. *)
  val unistring_to_json : string -> JSON.u

  module Local : RPC_core.CALLERS with type uri_provider := local_uri_provider

  module Remote : RPC_core.CALLERS with type uri_provider := remote_uri_provider
end

module Commitment : sig
  val dummy_commitment :
    ?on_error:
      ([ `Invalid_degree_strictly_less_than_expected of
         (int, int) Cryptobox.error_container
       | `Prover_SRS_not_loaded
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
  type member = {attester : string; indexes : int list}

  type t = member list

  val typ : t Check.typ

  val at_level :
    Node.t -> ?level:int -> ?delegates:string list -> unit -> t Lwt.t

  (* [check_is_in ~__LOC__ node ?inside ?level delegate] checks that [delegate]
     is part of the DAL committee at the given [level] if and only if [inside]
     is [true] (which is the default). The test fails if not. *)
  val check_is_in :
    __LOC__:string ->
    Node.t ->
    ?inside:bool ->
    ?level:int ->
    string ->
    unit Lwt.t
end

module Check : sig
  val profiles_typ : RPC.profile Check.typ

  val topics_peers_typ : (RPC.topic * string list) list Check.typ

  val slot_header_typ : RPC.slot_header Check.typ

  val slot_headers_typ : RPC.slot_header list Check.typ
end
