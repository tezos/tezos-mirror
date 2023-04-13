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
  }

  type t = {
    behaviour_penalty : int;  (** The score associated to a peer. *)
    topic_status : topic_status Topic.Map.t;
    peer_status : peer_status;
    parameters : topic score_parameters;
  }

  let newly_connected parameters =
    {
      behaviour_penalty = 0;
      topic_status = Topic.Map.empty;
      peer_status = Connected;
      parameters;
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

  let p1 {parameters; topic_status; _} =
    Topic.Map.fold
      (fun topic {mesh_status} acc ->
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

  let p7 {behaviour_penalty; parameters; _} =
    let penalty = float_of_int behaviour_penalty in
    if penalty > parameters.behaviour_penalty_threshold then
      let excess = penalty -. parameters.behaviour_penalty_threshold in
      excess *. excess *. parameters.behaviour_penalty_weight
    else 0.0

  let float ps =
    let p1 = p1 ps in
    let p7 = p7 ps in
    p1 +. p7

  let penalty score penalty =
    {score with behaviour_penalty = score.behaviour_penalty + penalty}

  let set_connected score = {score with peer_status = Connected}

  let expires ps =
    match ps.peer_status with
    | Connected -> None
    | Disconnected {expires} -> Some expires

  let graft (ps : t) topic =
    let topic_status =
      Topic.Map.add
        topic
        {mesh_status = Active {since = Time.now (); during = Span.zero}}
        ps.topic_status
    in
    {ps with topic_status}

  let prune (ps : t) topic =
    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> None
          | Some {mesh_status = Inactive} as v -> v
          | Some {mesh_status = Active _} -> Some {mesh_status = Inactive})
        ps.topic_status
    in
    {ps with topic_status}

  let remove_peer (stats : t) ~retain_duration =
    let score = float stats in
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
          (fun _topic {mesh_status = _} -> {mesh_status = Inactive})
          stats.topic_status
      in
      (* Mark peer as disconnected *)
      let stats =
        {stats with peer_status = Disconnected {expires}; topic_status}
      in
      (* TODO https://gitlab.com/tezos/tezos/-/issues/5447
         apply score penalties due to mesh message deliveries deficit. *)
      stats |> Option.some

  let refresh_graft_status now ps =
    let topic_status =
      Topic.Map.map
        (fun ({mesh_status} as v) ->
          match mesh_status with
          | Inactive -> v
          | Active {since; during = _} ->
              let during = Time.sub now (Time.to_span since) |> Time.to_span in
              {mesh_status = Active {since; during}})
        ps.topic_status
    in
    {ps with topic_status}

  let refresh ps =
    let current = Time.now () in
    let refresh () = Some (refresh_graft_status current ps) in
    match ps.peer_status with
    | Connected -> refresh ()
    | Disconnected {expires = at} when Time.(at > current) -> refresh ()
    | Disconnected _ -> None

  let compare s1 s2 =
    let f1 = float s1 in
    let f2 = float s2 in
    Float.compare f1 f2

  include Compare.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end
