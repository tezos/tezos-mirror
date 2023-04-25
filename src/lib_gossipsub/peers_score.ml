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

open Gossipsub_intf

(** This module allows to compute a score for each peers. *)

(* FIXME https://gitlab.com/tezos/tezos/-/issues/4967

   This is incomplete *)

module Make
    (Span : SPAN)
    (Time : TIME with type span = Span.t)
    (Topic : ITERABLE) =
struct
  type span = Span.t

  type time = Time.t

  type topic = Topic.t

  type value = float

  type peer_status =
    | Connected
    | Disconnected of {
        expires : Time.t;  (** The time after which the score can be cleared. *)
      }

  type mesh_status =
    | Active of {
        since : Time.t;
            (** The time at which the peer was added to the mesh. *)
        during : Span.t;
            (** The time spent by a peer in the mesh since [since]. *)
      }
        (** If the status is [Active], the peer is in the mesh for the associated topic. *)
    | Inactive
        (** If the status is [Inactive], the peer is not in the mesh for the associated topic. *)

  type topic_status = {
    mesh_status : mesh_status;
        (** [mesh_status] tracks whether the associated peer is in the mesh or not.
            This field is updated when pruning, grafting or removing the associated peer. *)
    first_message_deliveries : int;
        (** The number of messages on this topic that the associated peer was the first to deliver. *)
  }

  type stats = {
    behaviour_penalty : int;  (** The score associated to a peer. *)
    topic_status : topic_status Topic.Map.t;
    peer_status : peer_status;
    parameters : topic score_parameters;
  }

  type t = {
    stats : stats;  (** [stats] contains the gossipsub score counters. *)
    score : float Lazy.t;
        (** [score] is the score obtained from [stats]. We lazify it in order to compute it at most once. *)
  }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5451
     Add printers for [t] *)

  let get_topic_params parameters topic =
    match parameters.topics with
    | Topic_score_parameters_single tp -> tp
    | Topic_score_parameters_family {parameters; _} -> parameters topic

  let get_topic_weight parameters topic =
    match parameters.topics with
    | Topic_score_parameters_single _ -> 1.0
    | Topic_score_parameters_family {weights; _} -> weights topic

  (* Please refer to the `SCORE` module type documentation for the meaning of each
     score function. *)

  let p1 {parameters; topic_status; _} =
    Topic.Map.fold
      (fun topic {mesh_status; _} acc ->
        match mesh_status with
        | Inactive -> acc
        | Active {since = _; during} ->
            let topic_parameters = get_topic_params parameters topic in
            let topic_weight = get_topic_weight parameters topic in
            let seconds_in_mesh = Span.seconds during |> float_of_int in
            let weighted_time =
              topic_parameters.time_in_mesh_weight *. seconds_in_mesh
              /. topic_parameters.time_in_mesh_quantum
            in
            acc
            +. topic_weight
               *. Float.min weighted_time topic_parameters.time_in_mesh_cap)
      topic_status
      0.0

  let p2 {parameters; topic_status; _} =
    Topic.Map.fold
      (fun topic {first_message_deliveries; _} acc ->
        let topic_parameters = get_topic_params parameters topic in
        let topic_weight = get_topic_weight parameters topic in
        let weighted_deliveries =
          topic_weight *. topic_parameters.first_message_deliveries_weight
          *. float_of_int first_message_deliveries
        in
        acc +. weighted_deliveries)
      topic_status
      0.0

  let p7 {behaviour_penalty; parameters; _} =
    let penalty = float_of_int behaviour_penalty in
    if penalty > parameters.behaviour_penalty_threshold then
      let excess = penalty -. parameters.behaviour_penalty_threshold in
      excess *. excess *. parameters.behaviour_penalty_weight
    else 0.0

  let float ps =
    let p1 = p1 ps in
    let p2 = p2 ps in
    let p7 = p7 ps in
    p1 +. p2 +. p7

  let make stats = {stats; score = Lazy.from_fun (fun () -> float stats)}

  let value ps = Lazy.force ps.score

  let newly_connected parameters : t =
    make
      {
        behaviour_penalty = 0;
        topic_status = Topic.Map.empty;
        peer_status = Connected;
        parameters;
      }

  let penalty {stats; _} penalty =
    make {stats with behaviour_penalty = stats.behaviour_penalty + penalty}

  let set_connected {stats; _} = make {stats with peer_status = Connected}

  let fresh_mesh_status () =
    let since = Time.now () in
    let during = Span.zero in
    Active {since; during}

  let fresh_topic_stats () =
    let mesh_status = fresh_mesh_status () in
    {mesh_status; first_message_deliveries = 0}

  let expires {stats; _} =
    match stats.peer_status with
    | Connected -> None
    | Disconnected {expires} -> Some expires

  let graft ({stats; _} : t) topic =
    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> Some (fresh_topic_stats ())
          | Some status ->
              (* This should not happen: the automaton prevents grafting
                 a peer twice on the same topic. *)
              let mesh_status = fresh_mesh_status () in
              Some {status with mesh_status})
        stats.topic_status
    in
    make {stats with topic_status}

  let prune ({stats; _} : t) topic =
    let topic_status =
      Topic.Map.update
        topic
        (Option.map (fun status -> {status with mesh_status = Inactive}))
        stats.topic_status
    in
    make {stats with topic_status}

  let remove_peer ({stats; score} : t) ~retain_duration =
    let score = Lazy.force score in
    if Compare.Float.(score > 0.0) then
      (* We only retain non-positive scores to
         dissuade attacks on the score function. *)
      None
    else
      let now = Time.now () in
      let expires = Time.add now retain_duration in
      (* Update per-topic statistics *)
      let topic_status =
        Topic.Map.mapi
          (fun _topic status -> {status with mesh_status = Inactive})
          stats.topic_status
      in
      (* Mark peer as disconnected *)
      let stats =
        {stats with peer_status = Disconnected {expires}; topic_status}
      in
      (* TODO https://gitlab.com/tezos/tezos/-/issues/5447
         apply score penalties due to mesh message deliveries deficit. *)
      make stats |> Option.some

  let refresh_graft_status now {stats; _} =
    let topic_status =
      Topic.Map.map
        (fun status ->
          match status.mesh_status with
          | Inactive -> status
          | Active {since; during = _} ->
              let during = Time.sub now (Time.to_span since) |> Time.to_span in
              {status with mesh_status = Active {since; during}})
        stats.topic_status
    in
    make {stats with topic_status}

  let first_message_delivered ({stats; _} : t) (topic : Topic.t) =
    let increment topic ts =
      let cap =
        let topic_parameters = get_topic_params stats.parameters topic in
        topic_parameters.first_message_deliveries_cap
      in
      {
        mesh_status = ts.mesh_status;
        first_message_deliveries = Int.min cap (ts.first_message_deliveries + 1);
      }
    in
    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> fresh_topic_stats () |> increment topic |> Option.some
          | Some ts -> increment topic ts |> Option.some)
        stats.topic_status
    in
    make {stats with topic_status}

  let refresh ps =
    let current = Time.now () in
    let refresh () = Some (refresh_graft_status current ps) in
    match ps.stats.peer_status with
    | Connected -> refresh ()
    | Disconnected {expires = at} when Time.(at > current) -> refresh ()
    | Disconnected _ -> None

  let zero = 0.0

  let of_float = Fun.id

  include Compare.Make (struct
    type t = value

    let compare = Float.compare
  end)

  let pp_mesh_status fmtr status =
    match status with
    | Active {since; during} ->
        let open Fmt in
        let open Dump in
        let p = (since, during) in
        record [field "since" fst Time.pp; field "during" snd Span.pp] fmtr p
    | Inactive -> Format.pp_print_string fmtr "Inactive"

  let pp_topic_status =
    let open Fmt in
    Dump.record [field "mesh_status" (fun ts -> ts.mesh_status) pp_mesh_status]

  let pp_topic_status_map =
    let open Fmt in
    Dump.iter_bindings Topic.Map.iter nop Topic.pp pp_topic_status

  let pp_peer_status fmtr status =
    match status with
    | Connected -> Format.pp_print_string fmtr "Connected"
    | Disconnected {expires} ->
        Fmt.Dump.(record [field "expires" Fun.id Time.pp]) fmtr expires

  let pp_stats =
    let open Fmt in
    let open Dump in
    record
      [
        field "behaviour_penalty" (fun s -> s.behaviour_penalty) int;
        field "topic_status" (fun s -> s.topic_status) pp_topic_status_map;
        field "peer_status" (fun s -> s.peer_status) pp_peer_status;
        (* Don't print parameters back *)
      ]

  let pp =
    let open Fmt in
    let open Dump in
    record
      [
        field "stats" (fun x -> x.stats) pp_stats;
        field "score" (fun x -> Lazy.force x.score) float;
      ]

  let pp_value = Fmt.float
end
