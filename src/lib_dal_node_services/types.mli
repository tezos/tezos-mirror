(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** A Tezos level. *)
type level = int32

(** An index of a DAL slot header. *)
type slot_index = int

(** An index of a DAL page. *)
type page_index = int

(** An ID associated to a slot or to its commitment. *)
module Slot_id : sig
  type t = {slot_level : level; slot_index : slot_index}

  module Comparable : Stdlib.Set.OrderedType with type t = t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

type slot_id = Slot_id.t

(** A topic is defined by a public key hash of an attester and a slot index.
    - A slot producer tracks the topic associated to a given slot index for all
    the public key-hashes;
    - The attester tracks its own public key hash for all the slot indices;
    - A slot consumer tracks topics associated to a given slot index and enough
    public key-hashes so that the number of covered shards is enough to recover
    the slot data. *)
module Topic : sig
  (** Definition of a topic used for gossipsub. *)
  type t = {slot_index : int; pkh : Signature.Public_key_hash.t}

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t

  include COMPARABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** A message id uniquely identifies a share whose commitment is included in an
    L1 block. It is defined by a tuple containing the commitment, the level at
    which the commitment is successfully included in an L1 block, the
    corresponding slot index, the shard index, as well as the public key hash
    [pkh] of the delegate expected to attest it.

    Note that [pkh] is used to be able to directly infer a topic from a message id. It
    could be retrieved from L1 using the level. But, we decide to provide it
    directly in this first version. *)
module Message_id : sig
  type t = {
    commitment : Cryptobox.Commitment.t;
    level : int32;
    slot_index : int;
    shard_index : int;
    pkh : Signature.Public_key_hash.t;
  }

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t

  include COMPARABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val get_topic : t -> Topic.t
end

(** A message is a portion of an encoded slot's data. It's basically a shard
    without the corresponding index. The proof that the corresponding shard
    belong to the commitment (part of the message id) is also part of the
    message. *)
module Message : sig
  type t = {share : Cryptobox.share; shard_proof : Cryptobox.shard_proof}

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t
end

(** From the Gossipsub point of view, a peer is given by a cryptographic node
    identity {!P2p_peer.Id.t}. It's up to the caller to associate the
    {!P2p_peer.Id.t} to a {!P2p_point.Id.t} if needed (to e.g. implement peers
    exchange, which needs addresses and ports instead of cryptographic
    identities). *)
module Peer : sig
  type t = P2p_peer.Id.t

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t

  include COMPARABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

(** A point is made of an IP address and a port. Only the worker knows about
    the notion. The automaton only sees peers (i.e. cryptographic identities of
    nodes). *)
module Point : sig
  type t = P2p_point.Id.t

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t

  include COMPARABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module Span : sig
  type t = Ptime.Span.t

  include PRINTABLE with type t := t

  include ENCODABLE with type t := t

  include COMPARABLE with type t := t

  val zero : t

  val of_int_s : int -> t

  val to_int_s : t -> int

  val of_float_s : float -> t

  val to_float_s : t -> float

  (** [mul s n] returns [n * s]. *)
  val mul : t -> int -> t
end

module Time : sig
  type t = Ptime.t

  include COMPARABLE with type t := t

  include ENCODABLE with type t := t

  include PRINTABLE with type t := t

  type span = Span.t

  val now : unit -> t

  val add : t -> span -> t

  val sub : t -> span -> t

  val to_span : t -> span
end

module Score : sig
  type t = float

  include ENCODABLE with type t := t
end

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4562
   Use a bitset instead, when available in the standard library. *)

(** A set of slots, represented by a list of booleans (false for not in the
      set). It is used for instance to record which slots are deemed available
      by an attester. The level at which the slots have been published is also
      given. *)
type slot_set = {slots : bool list; published_level : int32}

(** The set of attestable slots of an attester (which may not necessarily be
      in the committee for a given level). *)
type attestable_slots = Attestable_slots of slot_set | Not_in_committee

(** An index of a DAL shard *)
type shard_index = int

(** The status of a header a DAL node is aware of: *)
type header_status =
  [ `Waiting_attestation
    (** The slot header was included and applied in a finalized L1 block
          but remains to be attested. *)
  | `Attested  (** The slot header was included in an L1 block and attested. *)
  | `Unattested
    (** The slot header was included in an L1 block but not timely attested. *)
  ]

(** DAL node can track one or many profiles that correspond to various modes
      that the DAL node would operate in. *)
type profile =
  | Bootstrap
      (** The bootstrap profile facilitates peer discovery in the DAL
          network.  Note that bootstrap nodes are incompatible with
          attester/producer/observer profiles as bootstrap nodes are
          expected to connect to all the meshes with degree 0. *)
  | Operator of Operator_profile.t
  | Random_observer

(** Information associated to a slot header in the RPC services of the DAL
      node. *)
type slot_header = {
  slot_id : slot_id;
  commitment : Cryptobox.Commitment.t;
  status : header_status;
}

(** The [with_proof] flag is associated to shards computation. It indicates
      whether we also compute shards' proofs or not. *)
type with_proof = {with_proof : bool}

val slot_id_query : (level option * shard_index option) Resto.Query.t

val slot_query :
  < padding : char ; slot_index : slot_index option > Resto.Query.t

val wait_query : < wait : bool > Resto.Query.t

val connected_query : < connected : bool > Resto.Query.t

val subscribed_query : < subscribed : bool > Resto.Query.t

val opt_header_status_query : header_status option Resto.Query.t

val slot_encoding : Cryptobox.slot Data_encoding.t

val slot_header_encoding : slot_header Data_encoding.t

val slot_id_encoding : slot_id Data_encoding.t

val header_status_encoding : header_status Data_encoding.t

val profile_encoding : profile Data_encoding.t

val with_proof_encoding : with_proof Data_encoding.t

val attestable_slots_encoding : attestable_slots Data_encoding.t

module Store : sig
  (** [stored_data] is the kind of data being encoded/decoded. This
    datatype is used to get better events UX. *)
  type kind = Commitment | Header_status | Slot_id | Slot | Shard | Profile

  val encoding : kind Data_encoding.t

  val to_string : kind -> string
end

module P2P : sig
  module Metadata : sig
    module Peer : sig
      (** Peer metadata is not used. So, its value is [unit]. *)
      type t = unit

      val config : t P2p_params.peer_meta_config
    end

    module Connection : sig
      (** {!connection_metadata} type.

          A value of this type is exchanged through the handshake
          protocol of the P2P.

          The {!advertised_net_port} is not mandatory, as it is
          already sent via the first P2P message after a connection is
          authenticated. But, we decide to duplicate the information
          here for consistency. The [is_bootstrap_peer] indicates
          whether the remote peer has a bootstrap profile or not. *)
      type t = {
        advertised_net_addr : P2p_addr.t option;
            (** The public address for which the local node can be
                reached from the outside. This is useful if the node
                is behind a NAT or a load balancer for example. *)
        advertised_net_port : int option;
            (** The port at which the local node can be reached. It is
                a bit redundant since the handshaking protocol already
                exchange this piece of information. *)
        is_bootstrap_peer : bool;
            (** [true] if the node advertises itself as a bootstrap
                node. This is to prevent a race condition from
                Gossipsub where a node may send full messages to a
                bootstrap node while this is not necessary. *)
      }

      include ENCODABLE with type t := t

      val config : t -> t P2p_params.conn_meta_config
    end
  end

  module Peer : sig
    module Info : sig
      type t = (Metadata.Peer.t, Metadata.Connection.t) P2p_peer.Info.t

      include ENCODABLE with type t := t
    end
  end
end

module Gossipsub : sig
  (** See {!Tezos_gossipsub.Introspection.connection}. Ideally we should reuse
      that type, but that would require a new dependency to be added. *)
  type connection = {
    topics : Topic.t list;
    direct : bool;
    outbound : bool;
    bootstrap : bool;
  }

  val connection_encoding : connection Data_encoding.t
end

module Version : sig
  type t = private {network_version : Network_version.t}

  (** [make ~network_version] makes a version out of a [network_version]. *)
  val make : network_version:Network_version.t -> t

  include ENCODABLE with type t := t
end

module Health : sig
  type status = Up | Degraded | Down | Ok | Ko

  type t = {status : status; checks : (string * status) list}

  val encoding : t Data_encoding.t
end
