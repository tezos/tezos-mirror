(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Services_legacy
open Types

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Tezos_rpc.Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

let post_commitment :
    < meth : [`POST]
    ; input : Cryptobox.slot
    ; output : Cryptobox.commitment
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.post_service
    ~description:
      "Add a slot in the node's context if not already present. The \
       corresponding commitment is returned."
    ~query:Tezos_rpc.Query.empty
    ~input:slot_encoding
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(open_root / "commitments")

let post_slot :
    < meth : [`POST]
    ; input : string
    ; output : Cryptobox.commitment * Cryptobox.commitment_proof
    ; prefix : unit
    ; params : unit
    ; query : < padding : char > >
    service =
  Tezos_rpc.Service.post_service
    ~description:
      "Post a slot to the DAL node, computes its commitment and commitment \
       proof, then computes the correspoding shards with their proof. The \
       result of this RPC can be directly used to publish a slot header."
    ~query:Types.slot_query
      (* With [Data_encoding.string], the body of the HTTP request contains
         two length prefixes: one for the full body, and one for the string.
         Using [Variable.string] instead fixes this. *)
    ~input:Data_encoding.Variable.string
    ~output:
      Data_encoding.(
        obj2
          (req "commitment" Cryptobox.Commitment.encoding)
          (req "commitment_proof" Cryptobox.Commitment_proof.encoding))
    Tezos_rpc.Path.(open_root / "slot")

let patch_commitment :
    < meth : [`PATCH]
    ; input : slot_id
    ; output : unit
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:"Associate a commitment to a level and a slot index."
    ~query:Tezos_rpc.Query.empty
    ~input:slot_id_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "commitments" /: Cryptobox.Commitment.rpc_arg)

let get_commitment_slot :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.slot
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve the content of the slot associated with the given commitment."
    ~query:Tezos_rpc.Query.empty
    ~output:slot_encoding
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "slot")

let get_commitment_proof :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.commitment_proof
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Compute the proof associated with a commitment."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment_proof.encoding
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "proof")

let put_commitment_shards :
    < meth : [`PUT]
    ; input : with_proof
    ; output : unit
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : unit >
    service =
  Tezos_rpc.Service.put_service
    ~description:
      "Compute and save the shards of the slot associated to the given \
       commitment. If the input's flag is true, the shard proofs are also \
       computed."
    ~query:Tezos_rpc.Query.empty
    ~input:with_proof_encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "shards")

let get_commitment_by_published_level_and_index :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.commitment
    ; prefix : unit
    ; params : (unit * level) * slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the accepted commitment associated to the given slot index and \
       published at the given level."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slot_indices"
      /: Tezos_rpc.Arg.int / "commitment")

let get_commitment_headers :
    < meth : [`GET]
    ; input : unit
    ; output : slot_header list
    ; prefix : unit
    ; params : unit * Cryptobox.commitment
    ; query : level option * slot_index option >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the known headers for the slot whose commitment is given."
    ~query:slot_id_query
    ~output:(Data_encoding.list slot_header_encoding)
    Tezos_rpc.Path.(
      open_root / "commitments" /: Cryptobox.Commitment.rpc_arg / "headers")

let get_published_level_headers :
    < meth : [`GET]
    ; input : unit
    ; output : slot_header list
    ; prefix : unit
    ; params : unit * level
    ; query : header_status option >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the known headers for the given published level."
    ~query:opt_header_status_query
    ~output:(Data_encoding.list slot_header_encoding)
    Tezos_rpc.Path.(open_root / "levels" /: Tezos_rpc.Arg.int32 / "headers")

let patch_profiles :
    < meth : [`PATCH]
    ; input : operator_profiles
    ; output : unit
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.patch_service
    ~description:
      "Update the list of profiles tracked by the DAL node. Note that it does \
       not take the bootstrap profile as it is incompatible with other \
       profiles."
    ~query:Tezos_rpc.Query.empty
    ~input:(Data_encoding.list operator_profile_encoding)
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "profiles")

let get_profiles :
    < meth : [`GET]
    ; input : unit
    ; output : profiles
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the list of current profiles tracked by the DAL node."
    ~query:Tezos_rpc.Query.empty
    ~output:profiles_encoding
    Tezos_rpc.Path.(open_root / "profiles")

let get_assigned_shard_indices :
    < meth : [`GET]
    ; input : unit
    ; output : shard_index list
    ; prefix : unit
    ; params : (unit * Tezos_crypto.Signature.public_key_hash) * level
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the shard indexes assigned to the given public key hash at the \
       given level."
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.(list int16)
    Tezos_rpc.Path.(
      open_root / "profiles" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "assigned_shard_indices")

let get_attestable_slots :
    < meth : [`GET]
    ; input : unit
    ; output : attestable_slots
    ; prefix : unit
    ; params : (unit * Tezos_crypto.Signature.public_key_hash) * level
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the currently attestable slots at the given attested level by \
       the given public key hash. A slot is attestable at level [l] if it is \
       published at level [l - attestation_lag] and *all* the shards assigned \
       at level [l] to the given public key hash are available in the DAL \
       node's store."
    ~query:Tezos_rpc.Query.empty
    ~output:attestable_slots_encoding
    Tezos_rpc.Path.(
      open_root / "profiles" /: Tezos_crypto.Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "attestable_slots")

let monitor_shards :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.Commitment.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Monitor put shards."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(open_root / "monitor_shards")

let version :
    < meth : [`GET]
    ; input : unit
    ; output : Types.Version.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"version"
    ~query:Tezos_rpc.Query.empty
    ~output:Types.Version.encoding
    Tezos_rpc.Path.(open_root / "version")

module P2P = struct
  open Tezos_rpc.Path

  let open_root = open_root / "p2p"

  let post_connect :
      < meth : [`POST]
      ; input : P2p_point.Id.t
      ; output : unit
      ; prefix : unit
      ; params : unit
      ; query : < timeout : Ptime.Span.t option > >
      service =
    Tezos_rpc.Service.post_service
      ~description:"Connect to a new peer."
      ~query:
        (let open Tezos_rpc.Query in
        query (fun timeout ->
            object
              method timeout = timeout
            end)
        |+ opt_field "timeout" Tezos_base.Time.System.Span.rpc_arg (fun t ->
               t#timeout)
        |> seal)
      ~input:P2p_point.Id.encoding
      ~output:Data_encoding.unit
      (open_root / "connect")

  let delete_disconnect_point :
      < meth : [`DELETE]
      ; input : unit
      ; output : unit
      ; prefix : unit
      ; params : unit * P2p_point.Id.t
      ; query : < wait : bool > >
      service =
    Tezos_rpc.Service.delete_service
      ~description:"Disconnect from a point."
      ~query:wait_query
      ~output:Data_encoding.unit
      (open_root / "points" / "disconnect" /: P2p_point.Id.rpc_arg)

  let delete_disconnect_peer :
      < meth : [`DELETE]
      ; input : unit
      ; output : unit
      ; prefix : unit
      ; params : unit * P2p_peer.Id.t
      ; query : < wait : bool > >
      service =
    Tezos_rpc.Service.delete_service
      ~description:"Disconnect from a peer."
      ~query:wait_query
      ~output:Data_encoding.unit
      (open_root / "peers" / "disconnect" /: P2p_peer.Id.rpc_arg)

  let get_points :
      < meth : [`GET]
      ; input : unit
      ; output : P2p_point.Id.t list
      ; prefix : unit
      ; params : unit
      ; query : < connected : bool > >
      service =
    Tezos_rpc.Service.get_service
      ~description:
        "By default, get the list of known points. When the 'connected' flag \
         is given, only get the connected points."
      ~query:connected_query
      ~output:Data_encoding.(list (obj1 (req "point" P2p_point.Id.encoding)))
      (open_root / "points")

  let get_points_info :
      < meth : [`GET]
      ; input : unit
      ; output : (P2p_point.Id.t * P2p_point.Info.t) list
      ; prefix : unit
      ; params : unit
      ; query : < connected : bool > >
      service =
    Tezos_rpc.Service.get_service
      ~description:
        "By default, get the list of known points and their corresponding \
         info. When the 'connected' flag is given, then only get the connected \
         points."
      ~query:connected_query
      ~output:
        Data_encoding.(
          list
            (obj2
               (req "point" P2p_point.Id.encoding)
               (req "info" P2p_point.Info.encoding)))
      (open_root / "points" / "info")

  module Points = struct
    let open_root = open_root / "points"

    let get_point_info :
        < meth : [`GET]
        ; input : unit
        ; output : P2p_point.Info.t
        ; prefix : unit
        ; params : unit * P2p_point.Id.t
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get info of the requested point"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.(obj1 (req "info" P2p_point.Info.encoding))
        (open_root / "by-id" /: P2p_point.Id.rpc_arg)
  end

  let get_peers :
      < meth : [`GET]
      ; input : unit
      ; output : P2p_peer.Id.t list
      ; prefix : unit
      ; params : unit
      ; query : < connected : bool > >
      service =
    Tezos_rpc.Service.get_service
      ~description:
        "By default, get the list of known peers. When the 'connected' flag is \
         given, then only get the connected peers."
      ~query:connected_query
      ~output:Data_encoding.(list (obj1 (req "peer" P2p_peer.Id.encoding)))
      (open_root / "peers" / "list")

  let get_peers_info :
      < meth : [`GET]
      ; input : unit
      ; output : (P2p_peer.Id.t * P2P.Peer.Info.t) list
      ; prefix : unit
      ; params : unit
      ; query : < connected : bool > >
      service =
    Tezos_rpc.Service.get_service
      ~description:"Get list of known peers and their corresponding info."
      ~query:connected_query
      ~output:
        Data_encoding.(
          list
            (obj2
               (req "point" P2p_peer.Id.encoding)
               (req "info" P2P.Peer.Info.encoding)))
      (open_root / "peers" / "info")

  module Peers = struct
    let open_root = open_root / "peers"

    let get_peer_info :
        < meth : [`GET]
        ; input : unit
        ; output : Types.P2P.Peer.Info.t
        ; prefix : unit
        ; params : unit * P2p_peer.Id.t
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get info of the requested peer"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.(obj1 (req "info" Types.P2P.Peer.Info.encoding))
        (open_root / "by-id" /: P2p_peer.Id.rpc_arg)

    let patch_peer :
        < meth : [`PATCH]
        ; input : [`Ban | `Trust | `Open] option
        ; output : Types.P2P.Peer.Info.t
        ; prefix : unit
        ; params : unit * P2p_peer.Id.t
        ; query : unit >
        service =
      Tezos_rpc.Service.patch_service
        ~description:
          "Change the permissions of a given peer. With `{acl: ban}`: \
           blacklist the given peer and remove it from the whitelist if \
           present. With `{acl: open}`: removes the peer from the blacklist \
           and whitelist. With `{acl: trust}`: trust the given peer \
           permanently and remove it from the blacklist if present. The peer \
           cannot be blocked (but its host IP still can)."
        ~query:Tezos_rpc.Query.empty
        ~input:
          Data_encoding.(
            obj1
              (opt
                 "acl"
                 (string_enum
                    [("ban", `Ban); ("trust", `Trust); ("open", `Open)])))
        ~output:Data_encoding.(obj1 (req "info" Types.P2P.Peer.Info.encoding))
        (open_root / "by-id" /: P2p_peer.Id.rpc_arg)
  end

  module Gossipsub = struct
    let open_root = open_root / "gossipsub"

    let get_topics :
        < meth : [`GET]
        ; input : unit
        ; output : Topic.t list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get the topics this node is currently subscribed to."
        ~query:Tezos_rpc.Query.empty
        ~output:(Data_encoding.list Topic.encoding)
        (open_root / "topics")

    let get_topics_peers :
        < meth : [`GET]
        ; input : unit
        ; output : (Types.Topic.t * Types.Peer.t list) list
        ; prefix : unit
        ; params : unit
        ; query : < subscribed : bool > >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "Get an association list between each topic subscribed to by the \
           connected peers and the remote peers subscribed to that topic. If \
           the 'subscribed' flag is given, then restrict the output to the \
           topics this node is subscribed to."
        ~query:subscribed_query
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "topic" Types.Topic.encoding)
                 (req "peers" (list Types.Peer.encoding))))
        (open_root / "topics" / "peers")

    let get_connections :
        < meth : [`GET]
        ; input : unit
        ; output : (Peer.t * Gossipsub.connection) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get this node's currently active connections."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "peer" Peer.encoding)
                 (req "connection" Gossipsub.connection_encoding)))
        (open_root / "connections")

    let get_scores :
        < meth : [`GET]
        ; input : unit
        ; output : (Peer.t * Score.t) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get the scores of the peers with a known score."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list (obj2 (req "peer" Peer.encoding) (req "score" Score.encoding)))
        (open_root / "scores")

    let get_backoffs :
        < meth : [`GET]
        ; input : unit
        ; output : (Topic.t * (Peer.t * Time.t) list) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get the backoffs of the peers with a backoff, per topic."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "topic" Topic.encoding)
                 (req
                    "backoffs"
                    (list
                       (obj2
                          (req "peer" Peer.encoding)
                          (req "backoff" Tezos_base.Time.System.encoding))))))
        (open_root / "backoffs")

    let get_message_cache :
        < meth : [`GET]
        ; input : unit
        ; output : (int64 * (Types.Topic.t * int) list) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "Get the number of message ids in the message cache, grouped by \
           heartbeat tick and topic."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "tick" int64)
                 (req
                    "per_topic_cache_size"
                    (list
                       (obj2
                          (req "topic" Types.Topic.encoding)
                          (req "num_ids" int31))))))
        (open_root / "message_cache")
  end
end
