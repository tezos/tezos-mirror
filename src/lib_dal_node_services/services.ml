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

open Types

(* String parameters queries. *)

let char =
  Resto.Arg.make
    ~name:"char"
    ~destruct:(fun str ->
      if String.length str = 1 then Ok (String.get str 0)
      else Error "A single character string is expected")
    ~construct:(fun c -> String.make 1 c)
    ()

let option_int =
  Resto.Arg.make
    ~name:"optional int"
    ~destruct:(function
      | "" -> Ok None
      | str -> (
          try Ok (Some (int_of_string str))
          with Failure _ -> Error "int expected"))
    ~construct:(function None -> "" | Some i -> string_of_int i)
    ()

let slot_query =
  let open Tezos_rpc.Query in
  query (fun padding slot_index ->
      object
        method padding = padding

        method slot_index = slot_index
      end)
  |+ field "padding" char '\x00' (fun obj -> obj#padding)
  |+ field "slot_index" option_int None (fun obj -> obj#slot_index)
  |> seal

let wait_query =
  let open Tezos_rpc.Query in
  query (fun wait ->
      object
        method wait = wait
      end)
  |+ flag "wait" (fun t -> t#wait)
  |> seal

let connected_query =
  let open Tezos_rpc.Query in
  query (fun connected ->
      object
        method connected = connected
      end)
  |+ flag "connected" (fun t -> t#connected)
  |> seal

let all_query =
  let open Tezos_rpc.Query in
  query (fun all ->
      object
        method all = all
      end)
  |+ flag "all" (fun t -> t#all)
  |> seal

let level_query =
  let open Tezos_rpc in
  let open Query in
  query (fun level -> level) |+ opt_field "level" Arg.int32 (fun t -> t) |> seal

let topic_query =
  let open Tezos_rpc in
  let open Query in
  query (fun delegate slot_index ->
      object
        method delegate = delegate

        method slot_index = slot_index
      end)
  |+ opt_field "delegate" Signature.Public_key_hash.rpc_arg (fun obj ->
         obj#delegate)
  |+ opt_field "slot_index" Arg.int (fun obj -> obj#slot_index)
  |> seal

(* Service declarations *)

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

let health :
    < meth : [`GET]
    ; input : unit
    ; output : Health.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Performs health checks on the DAL node, evaluating key components of \
       the DAL node. Returns a health status indicating whether the DAL node \
       is 'Up', 'Down', or 'Degraded' based on the results of these checks."
    ~query:Tezos_rpc.Query.empty
    ~output:Health.encoding
    Tezos_rpc.Path.(open_root / "health")

let synchronized :
    < meth : [`GET]
    ; input : unit
    ; output : L1_crawler_status.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Returns the current synchronization status of the DAL node with the L1 \
       node."
    ~query:Tezos_rpc.Query.empty
    ~output:L1_crawler_status.encoding
    Tezos_rpc.Path.(open_root / "synchronized")

let monitor_synchronized :
    < meth : [`GET]
    ; input : unit
    ; output : L1_crawler_status.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Returns the stream of synchronization statuses of the DAL node with the \
       L1 node."
    ~query:Tezos_rpc.Query.empty
    ~output:L1_crawler_status.encoding
    Tezos_rpc.Path.(open_root / "monitor" / "synchronized")

let post_slot :
    < meth : [`POST]
    ; input : string
    ; output : Cryptobox.commitment * Cryptobox.commitment_proof
    ; prefix : unit
    ; params : unit
    ; query : < padding : char ; slot_index : slot_index option > >
    service =
  Tezos_rpc.Service.post_service
    ~description:
      "Post a slot to the DAL node, computes its commitment and commitment \
       proof, then computes the correspoding shards with their proof. The \
       result of this RPC can be directly used to publish a slot header. If \
       the sent data is smaller than the size of a DAL slot, it is padded with \
       the character provided as padding query parameter (defaults to \\000). \
       If the slot_index query parameter is provided, the DAL node checks that \
       its profile allows to publish data on the given slot index. However, \
       slot_index is optional and has NO SEMANTIC EFFECT on the produced \
       commitment. It exists solely to help reverse proxies route POST /slots \
       requests to a DAL node subscribed to the corresponding topics."
    ~query:slot_query
      (* With [Data_encoding.string], the body of the HTTP request contains
         two length prefixes: one for the full body, and one for the string.
         Using [Variable.string] instead fixes this. *)
    ~input:Data_encoding.Variable.string
    ~output:
      Data_encoding.(
        obj2
          (req "commitment" Cryptobox.Commitment.encoding)
          (req "commitment_proof" Cryptobox.Commitment_proof.encoding))
    Tezos_rpc.Path.(open_root / "slots")

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

let get_slot_content :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.slot
    ; prefix : unit
    ; params : (unit * level) * slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Retrieve the content of the slot whose id is given."
    ~query:Tezos_rpc.Query.empty
    ~output:slot_encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "content")

let get_slot_pages :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.page list
    ; prefix : unit
    ; params : (unit * level) * slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Fetch slot as list of pages"
    ~query:Tezos_rpc.Query.empty
    ~output:(Data_encoding.list Data_encoding.bytes)
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "pages")

let get_slot_page_proof :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.page_proof
    ; prefix : unit
    ; params : ((unit * level) * slot_index) * page_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Compute the proof associated with a page of a given slot."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.page_proof_encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "pages" /: Tezos_rpc.Arg.int / "proof")

let get_slot_commitment :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.commitment
    ; prefix : unit
    ; params : (unit * level) * slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the commitment associated to the given slot index and published \
       at the given level, if any. The commitment is fetched from the \
       skip-list storage. Note that the commitment is not present in the \
       storage immediately after publication, but only when its attestation \
       status is known and final."
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.Commitment.encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "commitment")

let get_slot_status :
    < meth : [`GET]
    ; input : unit
    ; output : header_status
    ; prefix : unit
    ; params : (unit * level) * slot_index
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the status for the given slot. For operator nodes only."
    ~query:Tezos_rpc.Query.empty
    ~output:header_status_encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "status")

let get_last_processed_level :
    < meth : [`GET]
    ; input : unit
    ; output : int32
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Returns the last (finalized) L1 level which was processed by the DAL \
       node."
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.int32
    Tezos_rpc.Path.(open_root / "last_processed_level")

let get_protocol_parameters :
    < meth : [`GET]
    ; input : unit
    ; output : proto_parameters
    ; prefix : unit
    ; params : unit
    ; query : int32 option >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Returns the protocol parameters as known by the DAL node. An optional \
       'level' argument can specify for which level to retrieve them."
    ~query:level_query
    ~output:proto_parameters_encoding
    Tezos_rpc.Path.(open_root / "protocol_parameters")

let patch_profiles :
    < meth : [`PATCH]
    ; input : Controller_profiles.t
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
    ~input:Controller_profiles.encoding
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(open_root / "profiles")

let get_profiles :
    < meth : [`GET]
    ; input : unit
    ; output : profile
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Return the list of current profiles tracked by the DAL node."
    ~query:Tezos_rpc.Query.empty
    ~output:profile_encoding
    Tezos_rpc.Path.(open_root / "profiles")

let get_assigned_shard_indices :
    < meth : [`GET]
    ; input : unit
    ; output : shard_index list
    ; prefix : unit
    ; params : (unit * Signature.public_key_hash) * level
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Return the shard indexes assigned to the given public key hash at the \
       given level."
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.(list int16)
    Tezos_rpc.Path.(
      open_root / "profiles" /: Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "assigned_shard_indices")

let get_attestable_slots :
    < meth : [`GET]
    ; input : unit
    ; output : attestable_slots
    ; prefix : unit
    ; params : (unit * Signature.public_key_hash) * level
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
      open_root / "profiles" /: Signature.Public_key_hash.rpc_arg
      / "attested_levels" /: Tezos_rpc.Arg.int32 / "attestable_slots")

let monitor_attestable_slots :
    < meth : [`GET]
    ; input : unit
    ; output : Types.Attestable_event.t
    ; prefix : unit
    ; params : unit * Signature.public_key_hash
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "Stream attestable slot ids for a given public key hash [pkh]. A slot is \
       attestable for attested level L if it was published at (L - \
       attestation_lag) and *all* shards assigned at level L to [pkh] are \
       available in the DAL node's store. If some shards of the slot are \
       detected as traps for the baker, the slot should not be attested, so \
       the id is not sent via the stream."
    ~query:Tezos_rpc.Query.empty
    ~output:Types.Attestable_event.encoding
    Tezos_rpc.Path.(
      open_root / "profiles" /: Signature.Public_key_hash.rpc_arg / "monitor"
      / "attestable_slots")

let get_traps :
    < meth : [`GET]
    ; input : unit
    ; output : Types.trap list
    ; prefix : unit
    ; params : unit * level
    ; query :
        < delegate : Signature.public_key_hash option
        ; slot_index : slot_index option > >
    service =
  Tezos_rpc.Service.get_service
    ~description:
      "For a given published level, return all the traps known by the node. \
       Optional arguments allow to restrict the output to a given delegate or \
       slot index."
    ~query:topic_query
    ~output:(Data_encoding.list Types.trap_encoding)
    Tezos_rpc.Path.(
      open_root / "published_levels" /: Tezos_rpc.Arg.int32 / "known_traps")

let get_slot_shard :
    < meth : [`GET]
    ; input : unit
    ; output : Cryptobox.shard
    ; prefix : unit
    ; params : ((unit * level) * slot_index) * int
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"Fetch shard as bytes"
    ~query:Tezos_rpc.Query.empty
    ~output:Cryptobox.shard_encoding
    Tezos_rpc.Path.(
      open_root / "levels" /: Tezos_rpc.Arg.int32 / "slots" /: Tezos_rpc.Arg.int
      / "shards" /: Tezos_rpc.Arg.int / "content")

let version :
    < meth : [`GET]
    ; input : unit
    ; output : Version.t
    ; prefix : unit
    ; params : unit
    ; query : unit >
    service =
  Tezos_rpc.Service.get_service
    ~description:"version"
    ~query:Tezos_rpc.Query.empty
    ~output:Version.encoding
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

  module Points = struct
    let open_root = open_root / "points"

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
        (open_root / "disconnect" /: P2p_point.Id.rpc_arg)

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
        open_root

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
           info. When the 'connected' flag is given, then only get the \
           connected points."
        ~query:connected_query
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "point" P2p_point.Id.encoding)
                 (req "info" P2p_point.Info.encoding)))
        (open_root / "info")

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

  module Peers = struct
    let open_root = open_root / "peers"

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
        (open_root / "disconnect" /: P2p_peer.Id.rpc_arg)

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
          "By default, get the list of known peers. When the 'connected' flag \
           is given, then only get the connected peers."
        ~query:connected_query
        ~output:Data_encoding.(list (obj1 (req "peer" P2p_peer.Id.encoding)))
        open_root

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
                 (req "peer" P2p_peer.Id.encoding)
                 (req "info" P2P.Peer.Info.encoding)))
        (open_root / "info")

    let get_peer_info :
        < meth : [`GET]
        ; input : unit
        ; output : P2P.Peer.Info.t
        ; prefix : unit
        ; params : unit * P2p_peer.Id.t
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:"Get info of the requested peer"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.(obj1 (req "info" P2P.Peer.Info.encoding))
        (open_root / "by-id" /: P2p_peer.Id.rpc_arg)

    let patch_peer :
        < meth : [`PATCH]
        ; input : [`Ban | `Trust | `Open] option
        ; output : P2P.Peer.Info.t
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
           cannot be blocked (but its host IP still can). In all cases, the \
           updated information for the peer is returned. If input is omitted, \
           this is equivalent to using the `GET` version of this RPC."
        ~query:Tezos_rpc.Query.empty
        ~input:
          Data_encoding.(
            obj1
              (opt
                 "acl"
                 (string_enum
                    [("ban", `Ban); ("trust", `Trust); ("open", `Open)])))
        ~output:Data_encoding.(obj1 (req "info" P2P.Peer.Info.encoding))
        (open_root / "by-id" /: P2p_peer.Id.rpc_arg)
  end

  module Gossipsub = struct
    let topic_with_peers =
      Data_encoding.(
        obj2 (req "topic" Topic.encoding) (req "peers" (list Peer.encoding)))

    let open_root = open_root / "gossipsub"

    let get_mesh :
        < meth : [`GET]
        ; input : unit
        ; output : (Topic.t * Peer.t list) list
        ; prefix : unit
        ; params : unit
        ; query :
            < delegate : Signature.public_key_hash option
            ; slot_index : slot_index option > >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "Get the mesh of the peer. Concretely, the RPC returns a list of \
           topics, where each topic is associated to the remote peers with \
           which the current node shares a full connection (on that topic). \
           Optional arguments allow to restrict the output to a given delegate \
           or slot index."
        ~query:topic_query
        ~output:(Data_encoding.list topic_with_peers)
        (open_root / "mesh")

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
        ; output : (Topic.t * Peer.t list) list
        ; prefix : unit
        ; params : unit
        ; query : < all : bool > >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "When the 'all' flag is given, get an association list between each \
           topic subscribed to by the connected peers and the remote peers \
           subscribed to that topic. If the 'all' flag is not given, then \
           restrict the output to the topics this peer is subscribed to."
        ~query:all_query
        ~output:Data_encoding.(list topic_with_peers)
        (open_root / "topics" / "peers")

    let get_fanout :
        < meth : [`GET]
        ; input : unit
        ; output : (Topic.t * Peer.t list * Time.t) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "Returns the fanout peers per topic alongside the last publication \
           time on the topic."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list
              (obj3
                 (req "topic" Topic.encoding)
                 (req "peers" (list Peer.encoding))
                 (req "last_publication_time" Time.encoding)))
        (open_root / "fanout")

    let get_slot_indexes_peers :
        < meth : [`GET]
        ; input : unit
        ; output : (slot_index * Peer.t list) list
        ; prefix : unit
        ; params : unit
        ; query : < all : bool > >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "When the 'all' flag is given, get an association list between each \
           public key hash part of a topic subscribed to by the connected \
           peers and the remote peers subscribed to such topics. If the 'all' \
           flag is not given, then restrict the output to the topics this peer \
           is subscribed to."
        ~query:all_query
        ~output:
          Data_encoding.(
            list
              (obj2 (req "slot_index" uint8) (req "peers" (list Peer.encoding))))
        (open_root / "slot_indexes" / "peers")

    let get_pkhs_peers :
        < meth : [`GET]
        ; input : unit
        ; output : (Signature.public_key_hash * Peer.t list) list
        ; prefix : unit
        ; params : unit
        ; query : < all : bool > >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "When the 'all' flag is given, get an association list between each \
           topic subscribed to by the connected peers and the remote peers \
           subscribed to that topic. If the 'all' flag is not given, then \
           restrict the output to the topics this peer is subscribed to."
        ~query:all_query
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "pkh" Signature.Public_key_hash.encoding)
                 (req "peers" (list Peer.encoding))))
        (open_root / "pkhs" / "peers")

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

    let get_reconnection_delays :
        < meth : [`GET]
        ; input : unit
        ; output : (Point.t * Span.t) list
        ; prefix : unit
        ; params : unit
        ; query : unit >
        service =
      Tezos_rpc.Service.get_service
        ~description:
          "For each unreachable point, retrieve the time remaining until the \
           next reconnection attempt."
        ~query:Tezos_rpc.Query.empty
        ~output:
          Data_encoding.(
            list
              (obj2
                 (req "point" Point.encoding)
                 (req "delay" Span.rpc_encoding)))
        (open_root / "reconnection_delays")

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
        ; output : (int64 * (Topic.t * int) list) list
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
                       (obj2 (req "topic" Topic.encoding) (req "num_ids" int31))))))
        (open_root / "message_cache")
  end
end
