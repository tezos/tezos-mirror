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

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4966

   We should decide whether we want to implement a tracing mechanism
   as in the go implementation.
*)

module Gossipsub_intf = Gossipsub_intf
open Gossipsub_intf

module Make (C : AUTOMATON_CONFIG) :
  AUTOMATON
    with type Time.t = C.Time.t
     and module Span = C.Span
     and module Peer = C.Peer
     and module Topic = C.Topic
     and module Message_id = C.Message_id
     and module Message = C.Message = struct
  module Peer = C.Peer
  module Topic = C.Topic
  module Message_id = C.Message_id
  module Message = C.Message
  module Span = C.Span
  module Time = C.Time

  (** This module allows to compute a score for each peers. *)
  module Score : SCORE = struct
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4967

       This is incomplete *)
    type t = {behaviour_penalty : int}

    let zero = {behaviour_penalty = 0}

    let float {behaviour_penalty} = -behaviour_penalty |> float_of_int

    let penalty {behaviour_penalty} penalty =
      {behaviour_penalty = behaviour_penalty + penalty}

    let compare s1 s2 =
      let f1 = float s1 in
      let f2 = float s2 in
      Float.compare f1 f2

    include Compare.Make (struct
      type nonrec t = t

      let compare = compare
    end)
  end

  type message = Message.t

  type time = Time.t

  type span = Time.span

  type nonrec limits = (Peer.t, Message_id.t, span) limits

  type nonrec parameters = (Peer.t, Message_id.t) parameters

  (* FIXME not sure subtyping for output is useful. If it is, it is
     probably for few ouputs and could be removed. *)
  type _ output =
    | Negative_peer_score : Score.t -> [`IHave] output
    | Too_many_recv_ihave_messages : {count : int; max : int} -> [`IHave] output
    | Too_many_sent_iwant_messages : {count : int; max : int} -> [`IHave] output
    | Message_topic_not_tracked : [`IHave] output
    | Message_requested_message_ids : Message_id.t list -> [`IHave] output
    | On_iwant_messages_to_route : {
        peer : Peer.t;
        routed_message_ids :
          [`Ignored | `Not_found | `Message of message] Message_id.Map.t;
      }
        -> [`IWant] output
    | Peer_filtered : [`Graft] output
    | Unknown_topic : [`Graft] output
    | Peer_already_in_mesh : [`Graft] output
    | Grafting_direct_peer : [`Graft] output
    | Unexpected_grafting_peer : [`Graft] output
    | Grafting_peer_with_negative_score : [`Graft] output
    | Grafting_successfully : [`Graft] output
    | Peer_backed_off : [`Graft] output
    | No_peer_in_mesh : [`Prune] output
    | Ignore_PX_score_too_low : Score.t -> [`Prune] output
    | No_PX : [`Prune] output
    | PX : Peer.Set.t -> [`Prune] output
    | Publish_message : Peer.Set.t -> [`Publish] output
    | Already_subscribed : [`Join] output
    | Joining_topic : Peer.Set.t -> [`Join] output
    | Not_subscribed : [`Leave] output
    | Leaving_topic : {to_prune : Peer.Set.t} -> [`Leave] output
    | Heartbeat : {
        to_graft : Topic.Set.t Peer.Map.t;
        to_prune : Topic.Set.t Peer.Map.t;
        noPX_peers : Peer.Set.t;
      }
        -> [`Heartbeat] output
    | Peer_added : [`Add_peer] output
    | Peer_already_known : [`Add_peer] output
    | Removing_peer : [`Remove_peer] output
    | Subscribed : [`Subscribe] output
    | Subscribe_to_unknown_peer : [`Subscribe] output
    | Unsubscribed : [`Unsubscribe] output
    | Unsubscribe_from_unknown_peer : [`Unsubscribe] output

  type connection = {
    topics : Topic.Set.t;
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4980

       When does this field should be updated? *)
    direct : bool;
        (** A direct (aka explicit) connection is a connection to which we forward all the messages. *)
    outbound : bool;
        (** An outbound connection is a connection we initiated. *)
    backoff : time Topic.Map.t;
        (** The backoff times associated to this peer for each topic *)
    score : Score.t;  (** The score associated to this peer. *)
    expire : time option;
        (** The expiring time after having being disconnected from this peer. *)
  }

  type connections = connection Peer.Map.t

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4982

     This module is incomplete. *)
  module Memory_cache = struct
    type value = {message : message; access : int Peer.Map.t}

    type t = {messages : value Message_id.Map.t}

    let create () = {messages = Message_id.Map.empty}

    let record_message_access peer message_id t =
      match Message_id.Map.find message_id t.messages with
      | None -> None
      | Some {message; access} ->
          let access =
            Peer.Map.update
              peer
              (function None -> Some 1 | Some x -> Some (x + 1))
              access
          in
          let t =
            {
              messages =
                Message_id.Map.add message_id {message; access} t.messages;
            }
          in
          Some (t, message)

    let add_message message_id message t =
      let value = {message; access = Peer.Map.empty} in
      {messages = Message_id.Map.add message_id value t.messages}
  end

  type fanout_peers = {peers : Peer.Set.t; last_published_time : time}

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4983

      This data-structure should be documented. *)
  type state = {
    limits : limits;
    parameters : parameters;
    connections : connections;
    ihave_per_heartbeat : int Peer.Map.t;
    iwant_per_heartbeat : int Peer.Map.t;
    mesh : Peer.Set.t Topic.Map.t;
    fanout : fanout_peers Topic.Map.t;
    seen_messages : Message_id.Set.t;
    memory_cache : Memory_cache.t;
    rng : Random.State.t;
    heartbeat_ticks : int64;
  }
  (* Invariants:

     - Forall t set, Topic.Map.find t mesh = Some set -> Peer.Set set <> Peer.Set.empty

     - Forall t set, Topic.Map.find t fanout = Some set -> Peer.Set set <> Peer.Set.empty

     - Forall t p c, Peer.Map.find connections p = Some c && c.expired = Some _ ->
         Topic.Map.find t mesh = None &&
         Topic.Map.find t fanout = None
  *)

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4984

     Test the those invariants
  *)
  module Monad = struct
    type 'a t = (state, 'a) State_monad.t

    type ('pass, 'fail) check = (state, 'pass, 'fail) State_monad.check

    include State_monad.M
  end

  let check_limits l =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5129
       Replace the asserts by something more informative. *)
    assert (l.degree_low > 0) ;
    assert (l.degree_out >= 0) ;
    assert (l.degree_score >= 0) ;
    assert (l.degree_low < l.degree_optimal) ;
    assert (l.degree_high > l.degree_optimal) ;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
       This requirement is not imposed in the spec/Go implementation. Relax this
       requirement or delete the todo. *)
    assert (l.degree_score + l.degree_out <= l.degree_optimal) ;
    assert (l.degree_out < l.degree_low) ;
    assert (l.degree_out <= l.degree_optimal / 2)

  let make : Random.State.t -> limits -> parameters -> state =
   fun rng limits parameters ->
    check_limits limits ;
    {
      limits;
      parameters;
      connections = Peer.Map.empty;
      ihave_per_heartbeat = Peer.Map.empty;
      iwant_per_heartbeat = Peer.Map.empty;
      mesh = Topic.Map.empty;
      fanout = Topic.Map.empty;
      seen_messages = Message_id.Set.empty;
      memory_cache = Memory_cache.create ();
      rng;
      heartbeat_ticks = 0L;
    }

  module Helpers = struct
    (* These projections enable let-punning. *)
    let max_recv_ihave_per_heartbeat state =
      state.limits.max_recv_ihave_per_heartbeat

    let max_sent_iwant_per_heartbeat state =
      state.limits.max_sent_iwant_per_heartbeat

    let degree_optimal state = state.limits.degree_optimal

    let gossip_publish_threshold state = state.limits.gossip_publish_threshold

    let accept_px_threshold state = state.limits.accept_px_threshold

    let prune_backoff state = state.limits.prune_backoff

    let unsubscribe_backoff state = state.limits.unsubscribe_backoff

    let graft_flood_backoff state = state.limits.graft_flood_backoff

    let retain_duration state = state.limits.retain_duration

    let fanout_ttl state = state.limits.fanout_ttl

    let heartbeat_interval state = state.limits.heartbeat_interval

    let backoff_cleanup_ticks state = state.limits.backoff_cleanup_ticks

    let degree_low state = state.limits.degree_low

    let degree_high state = state.limits.degree_high

    let degree_score state = state.limits.degree_score

    let degree_out state = state.limits.degree_out

    let mesh state = state.mesh

    let fanout state = state.fanout

    let connections state = state.connections

    let seen_messages state = state.seen_messages

    let peer_filter state = state.parameters.peer_filter

    let memory_cache state = state.memory_cache

    let rng state = state.rng

    let update ?(delta = 1) key map =
      Peer.Map.update
        key
        (function None -> Some delta | Some n -> Some (n + delta))
        map

    let update_and_get ?(delta = 1) key map =
      let res = ref delta in
      Peer.Map.update
        key
        (function
          | None -> Some delta
          | Some n ->
              let value = n + delta in
              res := value ;
              Some value)
        map
      |> fun x -> (x, !res)

    let update_and_get_ihave_per_heartbeat ?delta key state =
      let ihave_per_heartbeat, res =
        update_and_get ?delta key state.ihave_per_heartbeat
      in
      let state = {state with ihave_per_heartbeat} in
      (state, res)

    let update_iwant_per_heartbeat ?delta key state =
      let iwant_per_heartbeat = update ?delta key state.ihave_per_heartbeat in
      let state = {state with iwant_per_heartbeat} in
      (state, ())

    let find ?(default = 0) key map =
      match Peer.Map.find key map with None -> default | Some n -> n

    let find_iwant_per_heartbeat ?default key state =
      find ?default key state.iwant_per_heartbeat

    let reset_ihave_per_heartbeat state =
      ({state with ihave_per_heartbeat = Peer.Map.empty}, ())

    let reset_iwant_per_heartbeat state =
      ({state with iwant_per_heartbeat = Peer.Map.empty}, ())

    let heartbeat_ticks state = state.heartbeat_ticks

    let set_heartbeat_ticks heartbeat_ticks state =
      ({state with heartbeat_ticks}, ())

    let set_connections connections state = ({state with connections}, ())

    let topic_is_tracked topic state =
      let {mesh; _} = state in
      match Topic.Map.find topic mesh with None -> false | Some _ -> true

    let set_memory_cache memory_cache state = ({state with memory_cache}, ())

    let get_connections_score connections ~default peer =
      match Peer.Map.find peer connections with
      | None -> default
      | Some connection -> connection.score

    let get_score ~default peer state =
      get_connections_score state.connections ~default peer

    let peer_has_outbound_connection connections ~default peer =
      Peer.Map.find_opt peer connections
      |> Option.map (fun c -> c.outbound)
      |> Option.value ~default

    let select_connections_peers connections rng topic ~filter ~max =
      Peer.Map.bindings connections
      |> List.filter_map (fun (peer, connection) ->
             let topics = connection.topics in
             if filter peer connection && Topic.Set.mem topic topics then
               Some peer
             else None)
      |> List.shuffle ~rng |> List.take_n max

    let select_peers topic ~filter ~max =
      let open Monad.Syntax in
      let*! connections in
      let*! rng in
      select_connections_peers connections rng topic ~filter ~max
      |> Peer.Set.of_list |> return

    let set_mesh_topic topic peers state =
      let state = {state with mesh = Topic.Map.add topic peers state.mesh} in
      (state, ())

    let set_mesh mesh state =
      let state = {state with mesh} in
      (state, ())

    let find_mesh topic state = Topic.Map.find topic state.mesh

    let find_fanout topic state = Topic.Map.find topic state.fanout

    let set_fanout_topic topic last_published_time peers state =
      if Peer.Set.is_empty peers then (state, ())
      else
        let state =
          {
            state with
            fanout =
              Topic.Map.add topic {peers; last_published_time} state.fanout;
          }
        in
        (state, ())

    let set_fanout fanout state =
      let state = {state with fanout} in
      (state, ())

    let delete_mesh topic state =
      let state = {state with mesh = Topic.Map.remove topic state.mesh} in
      (state, ())

    let delete_fanout topic state =
      let state = {state with fanout = Topic.Map.remove topic state.fanout} in
      (state, ())

    let put_message_in_cache message_id message state =
      let state =
        {
          state with
          memory_cache =
            Memory_cache.add_message message_id message state.memory_cache;
        }
      in
      (state, ())

    let update_backoff peer topic expire connections =
      Peer.Map.update
        peer
        (function
          | None -> None
          | Some connection ->
              let backoff =
                Topic.Map.update
                  topic
                  (function
                    | None -> Some expire
                    | Some old_backoff ->
                        if Time.(old_backoff < expire) then Some expire
                        else Some old_backoff)
                  connection.backoff
              in
              Some {connection with backoff})
        connections

    let add_connections_backoff time topic peer connections =
      let now = Time.now () in
      let expire = Time.add now time in
      update_backoff peer topic expire connections

    let add_backoff time topic peer =
      let open Monad.Syntax in
      let*! connections in
      let connections = add_connections_backoff time topic peer connections in
      set_connections connections

    let add_connections_score peer score =
      Peer.Map.update
        peer
        (Option.map (fun connection -> {connection with score}))

    let add_score peer score =
      let open Monad.Syntax in
      let*! connections in
      let connections = add_connections_score peer score connections in
      set_connections connections

    let _check_peer_score peer =
      let open Monad.Syntax in
      let*! peer_score = get_score ~default:Score.zero peer in
      if Score.(peer_score < zero) then Negative_peer_score peer_score |> fail
      else unit
  end

  include Helpers

  module IHave = struct
    let check_too_many_recv_ihave_message count =
      let open Monad.Syntax in
      let*! max_recv_ihave_per_heartbeat in
      if count > max_recv_ihave_per_heartbeat then
        Too_many_recv_ihave_messages {count; max = max_recv_ihave_per_heartbeat}
        |> fail
      else unit

    (* FIXME https://gitlab.com/tezos/tezos/-/issues/5016

       This check is not correct if the distant peer uses a different
       value for [max_recv_ihave_per_heartbeat] then our value for
       [max_sent_iwant_per_heartbeat]. *)
    let check_too_many_sent_iwant_message count =
      let open Monad.Syntax in
      let*! max_sent_iwant_per_heartbeat in
      if count > max_sent_iwant_per_heartbeat then
        Too_many_sent_iwant_messages {count; max = max_sent_iwant_per_heartbeat}
        |> fail
      else unit

    let check_topic_tracked topic =
      let open Monad.Syntax in
      let*! is_topic_tracked = topic_is_tracked topic in
      if not is_topic_tracked then Message_topic_not_tracked |> fail else unit

    let check_not_empty iwant_message_ids =
      let open Monad.Syntax in
      match iwant_message_ids with
      | [] -> Message_requested_message_ids [] |> fail
      | _ -> unit

    let filter peer message_ids : Message_id.t list Monad.t =
      let open Monad.Syntax in
      let*! peer_filter in
      let*! seen_messages in
      let should_handle_message_id message_id : bool =
        (not (Message_id.Set.mem message_id seen_messages))
        && peer_filter peer (`IHave message_id)
      in
      List.filter should_handle_message_id message_ids |> return

    let shuffle_and_trunc message_ids ~limit : (int * Message_id.t list) Monad.t
        =
      let open Monad.Syntax in
      let*! rng in
      let iwant_message_ids_len = List.length message_ids in
      (* Do not send more messages than [max_sent_iwant_per_heartbeat] *)
      let iwant_ids_to_send_n = min iwant_message_ids_len limit in
      let shuffle_iwant_ids = List.shuffle ~rng message_ids in
      let requested_message_ids =
        List.take_n iwant_ids_to_send_n shuffle_iwant_ids
      in
      return (iwant_ids_to_send_n, requested_message_ids)

    let handle peer topic message_ids : [`IHave] output Monad.t =
      let open Monad.Syntax in
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/5009

         Score check is missing.
      *)
      let* count_ihave_received = update_and_get_ihave_per_heartbeat peer in
      let*! count_iwant_sent = find_iwant_per_heartbeat peer in
      let*? () = check_too_many_recv_ihave_message count_ihave_received in
      let*? () = check_too_many_sent_iwant_message count_iwant_sent in
      let*? () = check_topic_tracked topic in
      let* iwant_message_ids = filter peer message_ids in
      let*? () = check_not_empty iwant_message_ids in
      let*! max_sent_iwant_per_heartbeat in
      let limit = max_sent_iwant_per_heartbeat - count_iwant_sent in
      (* Invariant: limit > 0 *)
      let* length, requested_message_ids =
        shuffle_and_trunc iwant_message_ids ~limit
      in
      let* () = update_iwant_per_heartbeat ~delta:length peer in
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/4966

         The go implementation traces some of the messages
         requested. *)
      Message_requested_message_ids requested_message_ids |> return
  end

  let handle_ihave :
      Peer.t -> Topic.t -> Message_id.t list -> [`IHave] output Monad.t =
    IHave.handle

  module IWant = struct
    let handle peer message_ids : [`IWant] output Monad.t =
      let open Monad.Syntax in
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/5008

         Score check is missing.
      *)
      let routed_message_ids = Message_id.Map.empty in
      let*! memory_cache in
      let*! peer_filter in
      let memory_cache, routed_message_ids =
        (* FIXME https://gitlab.com/tezos/tezos/-/issues/5011

           A check should ensure that the number of accesses do not
           exceed some pre-defined limit. *)
        List.fold_left
          (fun (memory_cache, messages) message_id ->
            match
              Memory_cache.record_message_access peer message_id memory_cache
            with
            | None ->
                (memory_cache, Message_id.Map.add message_id `Not_found messages)
            | Some (memory_cache, message) ->
                let messages =
                  if peer_filter peer (`IWant message_id) then
                    Message_id.Map.add message_id (`Message message) messages
                  else Message_id.Map.add message_id `Ignored messages
                in
                (memory_cache, messages))
          (memory_cache, routed_message_ids)
          message_ids
      in
      let* () = set_memory_cache memory_cache in
      On_iwant_messages_to_route {peer; routed_message_ids} |> return
  end

  let handle_iwant : Peer.t -> Message_id.t list -> [`IWant] output Monad.t =
    IWant.handle

  module Graft = struct
    let check_filter peer =
      let open Monad.Syntax in
      let*! peer_filter in
      if peer_filter peer `Graft then unit else Peer_filtered |> fail

    let check_topic_known mesh_opt =
      let open Monad.Syntax in
      match mesh_opt with
      | None -> Unknown_topic |> fail
      | Some mesh -> pass mesh

    let check_not_in_mesh mesh peer =
      let open Monad.Syntax in
      if Peer.Set.mem peer mesh then Peer_already_in_mesh |> fail else unit

    let check_not_direct peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> Unexpected_grafting_peer |> fail
      | Some ({direct; _} as connection) ->
          if direct then Grafting_direct_peer |> fail else pass connection

    let check_score peer topic {score; _} =
      let open Monad.Syntax in
      let*! prune_backoff in
      if Score.(score >= zero) then unit
      else
        let* () = add_backoff prune_backoff topic peer in
        Grafting_peer_with_negative_score |> fail

    let check_backoff peer topic {backoff; score; _} =
      let open Monad.Syntax in
      let*! prune_backoff in
      match Topic.Map.find topic backoff with
      | None -> unit
      | Some backoff ->
          let current = Time.now () in
          if Time.(current >= backoff) then unit
          else
            let score = Score.penalty score 1 in
            let*! graft_flood_backoff in
            let score =
              if Time.(current < add backoff graft_flood_backoff) then
                Score.penalty score 1
              else score
            in
            let* () = add_backoff prune_backoff topic peer in
            let* () = add_score peer score in
            fail Peer_backed_off

    let handle peer topic =
      let open Monad.Syntax in
      let*? () = check_filter peer in
      let*! mesh_opt = find_mesh topic in
      let*? mesh = check_topic_known mesh_opt in
      let*? () = check_not_in_mesh mesh peer in
      let*? connection = check_not_direct peer in
      let*? () = check_backoff peer topic connection in
      let*? () = check_score peer topic connection in
      let* () = set_mesh_topic topic (Peer.Set.add peer mesh) in
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/4980
         Probably the [topics] field needs to be updated here.
      *)
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/5007

         Handle negative score  and the size check is missing *)
      Grafting_successfully |> return
  end

  let handle_graft : Peer.t -> Topic.t -> [`Graft] output Monad.t = Graft.handle

  module Prune = struct
    let check_px_score peer =
      let open Monad.Syntax in
      let*! accept_px_threshold in
      let*! score = get_score ~default:Score.zero peer in
      if Compare.Float.(score |> Score.float < accept_px_threshold) then
        Ignore_PX_score_too_low score |> fail
      else unit

    let handle peer topic ~px ~backoff =
      let open Monad.Syntax in
      let*! mesh_opt = find_mesh topic in
      match mesh_opt with
      | None -> return No_peer_in_mesh
      | Some mesh ->
          (* FIXME https://gitlab.com/tezos/tezos/-/issues/5006

             backoff computation. *)
          let mesh = Peer.Set.remove peer mesh in
          let* () = set_mesh_topic topic mesh in
          let* _ = add_backoff backoff topic peer in
          let px = Peer.Set.of_seq px in
          if Peer.Set.is_empty px then No_PX |> return
          else
            let*? () = check_px_score peer in
            return (PX px)
  end

  let handle_prune :
      Peer.t ->
      Topic.t ->
      px:Peer.t Seq.t ->
      backoff:span ->
      [`Prune] output Monad.t =
    Prune.handle

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5148
     Consider merging subcribe/unsubscribe with join/leave. *)
  module Subscribe = struct
    let handle topic peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> return @@ Subscribe_to_unknown_peer
      | Some connection ->
          let connection =
            {connection with topics = Topic.Set.add topic connection.topics}
          in
          let connections = Peer.Map.add peer connection connections in
          let* () = set_connections connections in
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5143
             rust-libp2p adds the peer to the mesh if needed here. *)
          return Subscribed
  end

  let handle_subscribe : Topic.t -> Peer.t -> [`Subscribe] output Monad.t =
    Subscribe.handle

  module Unsubscribe = struct
    let handle topic peer =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None -> return @@ Unsubscribe_from_unknown_peer
      | Some connection ->
          let connection =
            {connection with topics = Topic.Set.remove topic connection.topics}
          in
          let connections = Peer.Map.add peer connection connections in
          let* () = set_connections connections in
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5143
             rust-libp2p removes unsubscribed peers from the mesh here. *)
          return Unsubscribed
  end

  let handle_unsubscribe : Topic.t -> Peer.t -> [`Unsubscribe] output Monad.t =
    Unsubscribe.handle

  module Publish = struct
    let get_peers_for_unsubscribed_topic topic =
      let open Monad.Syntax in
      let*! gossip_publish_threshold in
      let*! degree_optimal in
      let now = Time.now () in
      let*! fanout_opt = find_fanout topic in
      match fanout_opt with
      | None ->
          let filter _peer {direct; score; _} =
            (not direct)
            && Compare.Float.(score |> Score.float >= gossip_publish_threshold)
          in
          let* not_direct_peers =
            select_peers topic ~filter ~max:degree_optimal
          in
          let* () = set_fanout_topic topic now not_direct_peers in
          (* FIXME https://gitlab.com/tezos/tezos/-/issues/5010

             We could avoid this call by directly having a map
             of direct peers in the state. *)
          let filter peer ({direct; _} as connection) =
            filter peer connection || direct
          in
          select_peers topic ~filter ~max:Int.max_int
      | Some fanout ->
          let* () = set_fanout_topic topic now fanout.peers in
          return fanout.peers

    let handle ~sender topic message_id message : [`Publish] output Monad.t =
      let open Monad.Syntax in
      let* () = put_message_in_cache message_id message in
      let*! mesh_opt = find_mesh topic in
      let* peers =
        match mesh_opt with
        | Some peers -> return peers
        | None -> get_peers_for_unsubscribed_topic topic
      in
      let peers =
        Option.fold
          ~none:peers
          ~some:(fun peer -> Peer.Set.remove peer peers)
          sender
      in
      Publish_message peers |> return
  end

  let publish :
      sender:Peer.t option ->
      Topic.t ->
      Message_id.t ->
      message ->
      [`Publish] output Monad.t =
    Publish.handle

  module Join = struct
    let check_is_not_subscribed topic : (unit, [`Join] output) Monad.check =
      let open Monad.Syntax in
      let*! mesh in
      match Topic.Map.find topic mesh with
      | None -> unit
      | Some _ -> Already_subscribed |> fail

    let get_more_peers topic ~max =
      let filter _peer {backoff; score; direct; _} =
        not (direct || Topic.Map.mem topic backoff || Score.(score < zero))
      in
      select_peers topic ~filter ~max

    let init_mesh topic : [`Join] output Monad.t =
      let open Monad.Syntax in
      let*! degree_optimal in
      let*! connections in
      let is_valid peer =
        match Peer.Map.find peer connections with
        | None ->
            (* FIXME https://gitlab.com/tezos/tezos/-/issues/5005

               Not supposed to happen. But maybe it is better to
               return a value for defensive programming. *)
            false
        | Some {backoff; score; _} ->
            not (Topic.Map.mem topic backoff || Score.(score < zero))
      in
      let*! fanout in
      let valid_fanout_peers =
        match Topic.Map.find topic fanout with
        | None -> Peer.Set.empty
        | Some fanout_peers -> Peer.Set.filter is_valid fanout_peers.peers
      in
      let* peers =
        (* We prioritize fanout peers to be in the mesh for this
           topic. If we need more peers, we look at all our peers
           subscribed to this topic. *)
        if Peer.Set.cardinal valid_fanout_peers >= degree_optimal then
          return valid_fanout_peers
        else
          let max =
            max 0 (degree_optimal - Peer.Set.cardinal valid_fanout_peers)
          in
          let* more_peers = get_more_peers topic ~max in
          return (Peer.Set.union more_peers valid_fanout_peers)
      in
      let* () = set_mesh_topic topic peers in
      let* () = delete_fanout topic in
      Joining_topic peers |> return

    let handle topic : [`Join] output Monad.t =
      let open Monad.Syntax in
      let*? () = check_is_not_subscribed topic in
      init_mesh topic
  end

  let join : Topic.t -> [`Join] output Monad.t = Join.handle

  module Leave = struct
    type mesh = Peer.Set.t

    let check_already_subscribed topic : (mesh, [`Leave] output) Monad.check =
      let open Monad.Syntax in
      let*! mesh in
      match Topic.Map.find topic mesh with
      | None -> Not_subscribed |> fail
      | Some mesh -> pass mesh

    let handle_mesh topic mesh : [`Leave] output Monad.t =
      let open Monad.Syntax in
      let*! unsubscribe_backoff in
      let*! connections in
      let connections =
        Peer.Set.fold
          (fun peer connections ->
            add_connections_backoff unsubscribe_backoff topic peer connections)
          mesh
          connections
      in
      let* () = set_connections connections in
      Leaving_topic {to_prune = mesh} |> return

    let handle topic : [`Leave] output Monad.t =
      let open Monad.Syntax in
      let*? mesh = check_already_subscribed topic in
      let* () = delete_mesh topic in
      handle_mesh topic mesh
  end

  let leave : Topic.t -> [`Leave] output Monad.t = Leave.handle

  module Heartbeat = struct
    let clear_backoff =
      let open Monad.Syntax in
      let*! heartbeat_ticks in
      let*! heartbeat_interval in
      let*! backoff_cleanup_ticks in
      (* NOTE: Probably the cleanup can also be done lazily: at use, if a
         backoff time is expired, then remove it *)
      (* We only clear once every [backoff_cleanup_ticks] ticks to avoid
         iterating over the map(s) too much *)
      if Int64.(rem heartbeat_ticks (of_int backoff_cleanup_ticks)) != 0L then
        return ()
      else
        let current = Time.now () in
        let current_with_slack =
          (* Subtract some slack time to the current time to account for
             the message latency; for details, see
             https://github.com/libp2p/go-libp2p-pubsub/issues/368 *)
          Time.sub current (Time.mul_span heartbeat_interval 2)
        in
        let*! connections in
        Peer.Map.filter_map
          (fun _peer connection ->
            let backoff =
              Topic.Map.filter
                (fun _topic expire -> Time.(expire > current_with_slack))
                connection.backoff
            in
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/5095
               Check the reasoning *)
            match connection.expire with
            | Some expire
              when Time.(expire > current)
                   && Topic.Map.is_empty connection.backoff ->
                None
            | _ -> Some {connection with backoff})
          connections
        |> set_connections

    let cleanup =
      let open Monad.Syntax in
      (* Clean up expired backoffs *)
      let* () = clear_backoff in

      (* Clean up IHave and IWant counters *)
      let* () = reset_ihave_per_heartbeat in
      let* () = reset_iwant_per_heartbeat in

      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4967
         Apply IWANT request penalties *)
      return ()

    (* Mesh maintenance. For each topic, do in order:

       - Prune all peers with negative score, do not enable peer exchange for
       these peers.

       - If the number of remaining peers in the topic mesh is less than
       [degree_low], then select as many random peers (not already in the mesh
       topic) to graft as possible so that to have [degree_optimal] in the topic
       mesh. The selected peers should have a non-negative score, should not
       backedoff, and should not be direct peers.

       - If the number of remaining peers in the topic mesh is higher than
       [degree_high], then select as many peers (not already in the mesh topic)
       to prune as possible so that to have [degree_optimal] in the topic mesh.
       See [select_peers_to_prune] to see how the selection is performed.

       - If the number of remaining peers in the topic mesh is higher than
       [degree_low] and the number of outbound peers therein is smaller than
       [degree_out], then select an additional number of peers to graft (with
       the same conditions for grafting) to ensure that here at least
       [degree_out] outbound peers in the mesh.

       Finally, for pruned peers, back them off for [prune_backoff] time.
    *)
    let maintain_mesh =
      let open Monad.Syntax in
      let*! connections in
      let*! rng in
      let*! mesh in
      let*! prune_backoff in
      let*! degree_optimal in
      let*! degree_low in
      let*! degree_high in
      let*! degree_score in
      let*! degree_out in

      let has_outbound_connection =
        peer_has_outbound_connection connections ~default:false
      in

      let get_score = get_connections_score connections ~default:Score.zero in

      (* [add_to_peers_topic_set peers_topicset topic peers] adds [topic] to
         each [peer] from the [peers] list into the [peers_topicset], which is a
         peer to topicset map. It returns the new map.

         Note that the following invariant is maintained by the caller: the
         topicset for [peer] does not already contain [topic]. *)
      let add_to_peers_topic_set map topic peers =
        List.fold_left
          (fun acc peer ->
            Peer.Map.update
              peer
              (function
                | None -> Some (Topic.Set.singleton topic)
                | Some topics -> Some (Topic.Set.add topic topics))
              acc)
          map
          peers
      in

      (* Update [to_prune] and [old_peers] as follows:
         - In [to_prune], add [topic] for each of the peers in [new_peers].
         - Remove [new_peers] from [old_peers]. *)
      let prune topic to_prune ~old_peers new_peers =
        let to_prune = add_to_peers_topic_set to_prune topic new_peers in
        let peers =
          List.fold_left
            (fun acc peer -> Peer.Set.remove peer acc)
            old_peers
            new_peers
        in
        (to_prune, peers)
      in

      (* Keep the first [degree_score] peers by score and the remaining up to
         [degree_optimal] randomly, under the constraint that we keep
         [degree_out] peers in the mesh (if we have that many). *)
      let select_peers_to_prune peers =
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5052
           Consider first selecting [degree_out] peers and then ordering on the score. *)
        (* Sort descendingly by score, but shuffle first for the case we
           don't use the score. Head of list has highest score. *)
        let peers =
          peers |> Peer.Set.elements |> List.shuffle ~rng
          |> List.sort (fun peer1 peer2 ->
                 let s1 = get_score peer1 in
                 let s2 = get_score peer2 in
                 Score.compare s2 s1)
        in

        let peers_high_score, peers_low_score =
          List.split_n degree_score peers
        in
        let peers_low_score = List.shuffle ~rng peers_low_score in
        let peers_to_keep, peers_to_prune =
          let to_keep, peers_to_prune =
            (* Recall that [degree_score <= degree_optimal] *)
            List.split_n (degree_optimal - degree_score) peers_low_score
          in
          (peers_high_score @ to_keep, peers_to_prune)
        in

        (* Count the outbound peers we are keeping. *)
        let outbound_peers_to_keep, inbound_peers_to_keep =
          List.partition has_outbound_connection peers_to_keep
        in
        let num_outbound_to_keep = List.length outbound_peers_to_keep in

        (* If [num_outbound] is less than [degree_out], swap some
           outbound peers from the peers to prune with inbound peers
           from the peers to keep. *)
        if num_outbound_to_keep < degree_out then
          let outbound_peers_to_prune, inbound_peers_to_prune =
            List.partition has_outbound_connection peers_to_prune
          in
          let num_outbound_to_prune = List.length outbound_peers_to_prune in
          let num_inbound_to_keep = List.length inbound_peers_to_keep in
          let num_to_swap =
            min
              (max 0 (degree_out - num_outbound_to_keep))
              (min num_outbound_to_prune num_inbound_to_keep)
          in
          if num_to_swap > 0 then
            (* We additionally prune the [num_to_swap] inbound peers among the ones with
               a low score (or were shuffled); that's why we revert [inbound_peers_to_keep]. *)
            let inbound_peers_to_prune =
              List.take_n num_to_swap (List.rev inbound_peers_to_keep)
              @ inbound_peers_to_prune
            in
            (* Actually keep [num_to_swap] outbound peers. *)
            let outbound_peers_to_prune =
              List.drop_n num_to_swap outbound_peers_to_prune
            in
            inbound_peers_to_prune @ outbound_peers_to_prune
          else peers_to_prune
        else peers_to_prune
      in

      (* [maintain_topic_mesh topic peers (to_prune, to_graft, noPX_peers)]
         maintains the mesh for [topic] where [peers] are the original [peers]
         in this topic. [to_prune], [to_graft], [noPX_peers] are the peers to be
         pruned, grafted, and those for which no peer exchange should be done
         performed, accumulated from the maintenance for other mesh topics. *)
      let maintain_topic_mesh topic peers (to_prune, to_graft, noPX_peers) =
        let to_prune, peers, noPX_peers =
          (* Drop all peers with negative score, without PX *)
          Peer.Set.fold
            (fun peer (to_prune, peers, noPX_peers) ->
              if Score.(get_score peer < zero) then
                let to_prune, peers =
                  prune topic to_prune ~old_peers:peers [peer]
                in
                let noPX_peers = Peer.Set.add peer noPX_peers in
                (to_prune, peers, noPX_peers)
              else (to_prune, peers, noPX_peers))
            peers
            (to_prune, peers, noPX_peers)
        in

        (* Do we have too few peers? *)
        let num_peers = Peer.Set.cardinal peers in
        let to_graft =
          if num_peers < degree_low then
            let max = degree_optimal - num_peers in
            (* Filter out our current and direct peers, peers we are backing
               off, and peers with negative score. *)
            let filter peer connection =
              let in_mesh = Peer.Set.mem peer peers in
              let backedoff =
                Option.is_some (Topic.Map.find_opt topic connection.backoff)
              in
              (not in_mesh) && (not backedoff) && (not connection.direct)
              && Score.(connection.score >= zero)
            in
            select_connections_peers connections rng topic ~filter ~max
            |> add_to_peers_topic_set to_graft topic
          else to_graft
        in

        (* Do we have too many peers? *)
        let to_prune, peers =
          if num_peers > degree_high then
            (* We'll prune [num_peers - degree_optimal] peers. *)
            select_peers_to_prune peers |> prune topic to_prune ~old_peers:peers
          else (to_prune, peers)
        in

        (* Do we have enough outbound peers? *)
        let num_peers = Peer.Set.cardinal peers in
        let to_graft =
          if num_peers >= degree_low then
            let num_outbound =
              Peer.Set.fold
                (fun peer count ->
                  if has_outbound_connection peer then count + 1 else count)
                peers
                0
            in
            if num_outbound < degree_out then
              let max = degree_out - num_outbound in
              (* Filter out our current and direct peers, peers we are backing
                 off, and peers with negative score *)
              let filter peer connection =
                let in_mesh = Peer.Set.mem peer peers in
                let backedoff =
                  Option.is_some (Topic.Map.find_opt topic connection.backoff)
                in
                (not in_mesh) && (not backedoff) && (not connection.direct)
                && Score.(connection.score >= zero)
                && has_outbound_connection peer
              in
              select_connections_peers connections rng topic ~filter ~max
              |> add_to_peers_topic_set to_graft topic
            else to_graft
          else to_graft
        in

        (* TODO: https://gitlab.com/tezos/tezos/-/issues/4967
           Try to improve the mesh with opportunistic grafting *)

        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5066
           Prepare gossip messages, that is, IHave control messages
           referring to recently seen messages, to be sent to a random
           selection of peers.*)
        (to_graft, to_prune, noPX_peers)
      in

      let to_graft, to_prune, noPX_peers =
        Topic.Map.fold
          maintain_topic_mesh
          mesh
          (Peer.Map.empty, Peer.Map.empty, Peer.Set.empty)
      in

      (* Update backoff for pruned peers. *)
      let* () =
        Peer.Map.fold
          (fun peer topicset connections ->
            Topic.Set.fold
              (fun topic connections ->
                add_connections_backoff prune_backoff topic peer connections)
              topicset
              connections)
          to_prune
          connections
        |> set_connections
      in
      return (to_graft, to_prune, noPX_peers)

    let expire_fanout =
      let open Monad.Syntax in
      let*! fanout in
      let*! fanout_ttl in
      let current = Time.now () in
      let fanout =
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/5184
           Optimize by having a min and a max last published time to avoid
           traversing the map when not needed? *)
        Topic.Map.filter
          (fun _topic {last_published_time; peers = _} ->
            Time.(add last_published_time fanout_ttl >= current))
          fanout
      in
      set_fanout fanout

    let handle =
      let open Monad.Syntax in
      let*! heartbeat_ticks in
      let* () = set_heartbeat_ticks (Int64.succ heartbeat_ticks) in

      (* cleaning up *)
      let* () = cleanup in

      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4967
         Cache scores throughout the heartbeat *)

      (* Maintain the mesh for topics we have joined. Concretely, in case the
         number of peers per topic (in the mesh) is lower than [degree_low] or
         higher than [degree_high], then select peers to graft or prune
         respectively, so that the new number of peers per topic becomes
         [degree_optimal]. *)
      let* to_graft, to_prune, noPX_peers = maintain_mesh in

      (* Expire fanout for topics we haven't published to in a while *)
      let* () = expire_fanout in

      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5066
         Maintain our fanout for topics we are publishing to, but we have not
         joined. *)

      (* FIXME https://gitlab.com/tezos/tezos/-/issues/4982
         Advance the message history window *)
      Heartbeat {to_graft; to_prune; noPX_peers} |> return
  end

  let heartbeat : [`Heartbeat] output Monad.t = Heartbeat.handle

  module Add_peer = struct
    let handle ~direct ~outbound peer : [`Add_peer] output Monad.t =
      let open Monad.Syntax in
      let*! connections in
      match Peer.Map.find peer connections with
      | None ->
          let connection =
            {
              direct;
              score = Score.zero;
              backoff = Topic.Map.empty;
              topics = Topic.Set.empty;
              outbound;
              expire = None;
            }
          in
          let connections = Peer.Map.add peer connection connections in
          let* () = set_connections connections in
          return Peer_added
      | Some _ -> return Peer_already_known
  end

  let add_peer :
      direct:bool -> outbound:bool -> Peer.t -> [`Add_peer] output Monad.t =
    Add_peer.handle

  module Remove_peer = struct
    let handle peer : [`Remove_peer] output Monad.t =
      let open Monad.Syntax in
      let*! mesh in
      let mesh = Topic.Map.map (fun peers -> Peer.Set.remove peer peers) mesh in
      let* () = set_mesh mesh in
      let*! fanout in
      let fanout =
        Topic.Map.map
          (fun fanout_peers ->
            {fanout_peers with peers = Peer.Set.remove peer fanout_peers.peers})
          fanout
      in
      let* () = set_fanout fanout in
      let*! connections in
      let*! retain_duration in
      let connections =
        Peer.Map.update
          peer
          (function
            | None -> None
            | Some connection ->
                let now = Time.now () in
                let expire = Some (Time.add now retain_duration) in
                Some {connection with expire})
          connections
      in
      let* () = set_connections connections in
      Removing_peer |> return
  end

  let remove_peer : Peer.t -> [`Remove_peer] output Monad.t = Remove_peer.handle

  module Internal_for_tests = struct
    let get_subscribed_topics peer state =
      match Peer.Map.find peer state.connections with
      | None -> []
      | Some connection -> Topic.Set.elements connection.topics

    type nonrec connection = connection = {
      topics : Topic.Set.t;
      direct : bool;
      outbound : bool;
      backoff : Time.t Topic.Map.t;
      score : Score.t;
      expire : Time.t option;
    }

    let connections state = state.connections

    let limits state = state.limits
  end
end
