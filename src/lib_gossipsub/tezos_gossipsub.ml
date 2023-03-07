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

module type PRINTABLE = sig
  type t

  val pp : Format.formatter -> t -> unit
end

module type ITERABLE = sig
  type t

  include Compare.S with type t := t

  include PRINTABLE with type t := t

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type CONFIGURATION = sig
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : ITERABLE

  module Message : PRINTABLE

  module Span : PRINTABLE

  module Time : sig
    include Compare.S

    include PRINTABLE with type t := t

    type span = Span.t

    val now : unit -> t

    val add : t -> span -> t
  end
end

type ('peer, 'message_id, 'span) limits = {
  max_recv_ihave_per_heartbeat : int;
  max_sent_iwant_per_heartbeat : int;
  expected_peers_per_topic : int;
  gossip_publish_threshold : float;
  accept_px_threshold : float;
  unsubscribe_backoff : 'span;
  graft_flood_backoff : 'span;
      (* WRT to go implementation, the value of this constant is
         actually [graft_flood_threshold - prune_backoff] *)
  prune_backoff : 'span;
  retain_duration : 'span;
}

type ('peer, 'message_id) parameters = {
  peer_filter :
    'peer -> [`IHave of 'message_id | `IWant of 'message_id | `Graft] -> bool;
}

(** This module allows to compute a score for each peers. *)
module Score : sig
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4967

     This is incomplete *)
  include Compare.S

  val float : t -> float

  val zero : t

  val penalty : t -> int -> t
end = struct
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

module type S = sig
  (* FIXME https://gitlab.com/tezos/tezos/-/issues/5012

     Maybe it would be better if the interface would be more generic
     and would look like an automaton? *)
  module Peer : ITERABLE

  module Topic : ITERABLE

  module Message_id : ITERABLE

  module Message : PRINTABLE

  module Time : PRINTABLE

  module Span : PRINTABLE

  type message = Message.t

  type span = Span.t

  type state

  type limits := (Peer.t, Message_id.t, span) limits

  type parameters := (Peer.t, Message_id.t) parameters

  val make : Random.State.t -> limits -> parameters -> state

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
    | Peer_added : [`Add_peer] output
    | Peer_already_known : [`Add_peer] output
    | Removing_peer : [`Remove_peer] output

  type 'a monad := state -> state * 'a output

  val add_peer : direct:bool -> outbound:bool -> Peer.t -> [`Add_peer] monad

  val remove_peer : Peer.t -> [`Remove_peer] monad

  val handle_ihave : Peer.t -> Topic.t -> Message_id.t list -> [`IHave] monad

  val handle_iwant : Peer.t -> Message_id.t list -> [`IWant] monad

  val handle_graft : Peer.t -> Topic.t -> [`Graft] monad

  val handle_prune :
    Peer.t -> Topic.t -> px:Peer.t Seq.t -> backoff:span -> [`Prune] monad

  val publish :
    sender:Peer.t option ->
    Topic.t ->
    Message_id.t ->
    message ->
    [`Publish] monad

  val heartbeat : [`Heartbeat] monad

  val join : Topic.t -> [`Join] monad

  val leave : Topic.t -> [`Leave] monad
end

module Make (C : CONFIGURATION) :
  S
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
    | Peer_added : [`Add_peer] output
    | Peer_already_known : [`Add_peer] output
    | Removing_peer : [`Remove_peer] output

  type connection = {
    topics : Topic.Set.t;
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/4980

       When does this field should be updated? *)
    direct : bool;
        (** A direct connection is a connection to which we forward all the messages. *)
    outbound : bool;
        (** An outbound connection is a connection we
                          connected to. *)
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

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4983

      This data-structure should be documented. *)
  type state = {
    limits : limits;
    parameters : parameters;
    connections : connections;
    ihave_per_heartbeat : int Peer.Map.t;
    iwant_per_heartbeat : int Peer.Map.t;
    mesh : Peer.Set.t Topic.Map.t;
    fanout : Peer.Set.t Topic.Map.t;
    last_published_time : time Topic.Map.t;
    seen_messages : Message_id.Set.t;
    memory_cache : Memory_cache.t;
    rng : Random.State.t;
  }
  (* Invariants:

     - Forall t set, Topic.Map.find t mesh = Some set -> Peer.Set set <> Peer.Set.empty

     - Forall t set, Topic.Map.find t fanout = Some set -> Peer.Set set <> Peer.Set.empty
  *)

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/4984

     Test the those invariants
  *)

  module Monad = struct
    type 'a t = (state, 'a) State_monad.t

    type ('pass, 'fail) check = (state, 'pass, 'fail) State_monad.check

    include State_monad.M
  end

  let make : Random.State.t -> limits -> parameters -> state =
   fun rng limits parameters ->
    {
      limits;
      parameters;
      connections = Peer.Map.empty;
      ihave_per_heartbeat = Peer.Map.empty;
      iwant_per_heartbeat = Peer.Map.empty;
      mesh = Topic.Map.empty;
      fanout = Topic.Map.empty;
      last_published_time = Topic.Map.empty;
      seen_messages = Message_id.Set.empty;
      memory_cache = Memory_cache.create ();
      rng;
    }

  module Helpers = struct
    (* These projections enable let-punning. *)
    let max_recv_ihave_per_heartbeat state =
      state.limits.max_recv_ihave_per_heartbeat

    let max_sent_iwant_per_heartbeat state =
      state.limits.max_sent_iwant_per_heartbeat

    let expected_peers_per_topic state = state.limits.expected_peers_per_topic

    let gossip_publish_threshold state = state.limits.gossip_publish_threshold

    let accept_px_threshold state = state.limits.accept_px_threshold

    let prune_backoff state = state.limits.prune_backoff

    let unsubscribe_backoff state = state.limits.unsubscribe_backoff

    let graft_flood_backoff state = state.limits.graft_flood_backoff

    let retain_duration state = state.limits.retain_duration

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

    let set_connections connections state = ({state with connections}, ())

    let topic_is_tracked topic state =
      let {mesh; _} = state in
      match Topic.Map.find topic mesh with None -> false | Some _ -> true

    let set_memory_cache memory_cache state = ({state with memory_cache}, ())

    let get_score ~default peer state =
      match Peer.Map.find peer state.connections with
      | None -> default
      | Some connection -> connection.score

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

    let set_fanout_topic topic peers state =
      if Peer.Set.is_empty peers then (state, ())
      else
        let state =
          {state with fanout = Topic.Map.add topic peers state.fanout}
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

    let set_last_published_time topic time state =
      let state =
        {
          state with
          last_published_time =
            Topic.Map.add topic time state.last_published_time;
        }
      in
      (state, ())

    let delete_last_published_time topic state =
      let state =
        {
          state with
          last_published_time = Topic.Map.remove topic state.last_published_time;
        }
      in
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

  module Publish = struct
    let get_peers_for_unsubscribed_topic topic =
      let open Monad.Syntax in
      let*! fanout_opt = find_fanout topic in
      let now = Time.now () in
      let* () = set_last_published_time topic now in
      let*! gossip_publish_threshold in
      let*! expected_peers_per_topic in
      match fanout_opt with
      | None ->
          let filter _peer {direct; score; _} =
            (not direct)
            && Compare.Float.(score |> Score.float >= gossip_publish_threshold)
          in
          let* not_direct_peers =
            select_peers topic ~filter ~max:expected_peers_per_topic
          in
          let* () = set_fanout_topic topic not_direct_peers in
          (* FIXME https://gitlab.com/tezos/tezos/-/issues/5010

             We could avoid this call by directly having a map
             of direct peers in the state. *)
          let filter peer ({direct; _} as connection) =
            filter peer connection || direct
          in
          let* peers = select_peers topic ~filter ~max:Int.max_int in
          (* FIXME https://gitlab.com/tezos/tezos/-/issues/4988

             lastpub field *)
          return peers
      | Some peers -> return peers

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
      let*! expected_peers_per_topic in
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
        | Some fanout_peers -> Peer.Set.filter is_valid fanout_peers
      in
      let* peers =
        (* We prioritize fanout peers to be in the mesh for this
           topic. If we need more peers, we look at all our peers
           subscribed to this topic. *)
        if Peer.Set.cardinal valid_fanout_peers >= expected_peers_per_topic then
          return valid_fanout_peers
        else
          let max =
            max
              0
              (expected_peers_per_topic - Peer.Set.cardinal valid_fanout_peers)
          in
          let* more_peers = get_more_peers topic ~max in
          return (Peer.Set.union more_peers valid_fanout_peers)
      in
      let* () = set_mesh_topic topic peers in
      let* () = delete_fanout topic in
      let* () = delete_last_published_time topic in
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
    let handle _ =
      (* FIXME https://gitlab.com/tezos/tezos/-/issues/4949

         Implement this. *)
      assert false
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
        Topic.Map.map (fun peers -> Peer.Set.remove peer peers) fanout
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
end
