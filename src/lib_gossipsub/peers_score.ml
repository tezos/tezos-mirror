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
    first_message_deliveries : float;
        (** The number of messages on this topic that the associated peer was the first to deliver. *)
    mesh_message_deliveries_active : bool;
        (** A flag indicating whether we started evaluating the score associated to mesh messages deliveries. *)
    mesh_message_deliveries : float;
        (** The number of first or near-first messages delivered on this topic by
            the associated mesh peer. *)
    mesh_failure_penalty : float;
        (** The score penalty induced on the associated peer when pruned or removed due
            to a mesh message delivery deficit. *)
    invalid_messages : float;
        (** The number of invalid messages sent by the associated mesh peer. *)
  }

  type stats = {
    behaviour_penalty : float;
        (** P7: The behavioural score associated to a peer. *)
    application_score : float;  (** P5: Application-specific score. *)
    topic_status : topic_status Topic.Map.t;
    peer_status : peer_status;
    parameters : (topic, span) score_parameters;
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

  let p1 topic_parameters {mesh_status; _} =
    match mesh_status with
    | Inactive -> 0.0
    | Active {since = _; during} ->
        let seconds_in_mesh = Span.to_float_s during in
        let weighted_time =
          topic_parameters.time_in_mesh_weight *. seconds_in_mesh
          /. topic_parameters.time_in_mesh_quantum
        in
        Float.min weighted_time topic_parameters.time_in_mesh_cap

  let p2 topic_parameters {first_message_deliveries; _} =
    let weighted_deliveries =
      topic_parameters.first_message_deliveries_weight
      *. first_message_deliveries
    in
    weighted_deliveries

  let mesh_message_delivery_penalty {mesh_message_deliveries_threshold; _}
      {mesh_message_deliveries; mesh_message_deliveries_active; _} =
    if
      mesh_message_deliveries_active
      && mesh_message_deliveries
         < float_of_int mesh_message_deliveries_threshold
    then
      let deficit =
        float_of_int mesh_message_deliveries_threshold
        -. mesh_message_deliveries
      in
      deficit *. deficit
    else 0.

  let p3 params status =
    let penalty = mesh_message_delivery_penalty params status in
    penalty *. params.mesh_message_deliveries_weight

  let p3b {mesh_failure_penalty_weight; _} {mesh_failure_penalty; _} =
    mesh_failure_penalty *. mesh_failure_penalty_weight

  let p4 {invalid_message_deliveries_weight; _} {invalid_messages; _} =
    invalid_messages *. invalid_message_deliveries_weight

  let topic_scores {parameters; topic_status; _} =
    Topic.Map.fold
      (fun topic status acc ->
        let topic_parameters = get_topic_params parameters topic in
        let topic_weight = get_topic_weight parameters topic in
        let p1 = p1 topic_parameters status in
        let p2 = p2 topic_parameters status in
        let p3 = p3 topic_parameters status in
        let p3b = p3b topic_parameters status in
        let p4 = p4 topic_parameters status in
        let total = topic_weight *. (p1 +. p2 +. p3 +. p3b +. p4) in
        acc +. total)
      topic_status
      0.0

  let p5 {application_score; parameters; _} =
    application_score *. parameters.app_specific_weight

  let p7 {behaviour_penalty; parameters; _} =
    let penalty = behaviour_penalty in
    if penalty > parameters.behaviour_penalty_threshold then
      let excess = penalty -. parameters.behaviour_penalty_threshold in
      excess *. excess *. parameters.behaviour_penalty_weight
    else 0.0

  let float ps =
    let topic_scores =
      match ps.parameters.topic_score_cap with
      | None -> topic_scores ps
      | Some cap -> Float.min cap (topic_scores ps)
    in
    let p5 = p5 ps in
    let p7 = p7 ps in
    topic_scores +. p5 +. p7

  let decay ~decay_zero ~decay_rate value =
    let decayed = value *. decay_rate in
    if decayed < decay_zero then 0.0 else decayed

  let make stats = {stats; score = Lazy.from_fun (fun () -> float stats)}

  let value ps = Lazy.force ps.score

  let newly_connected parameters : t =
    make
      {
        behaviour_penalty = 0.0;
        application_score = 0.0;
        topic_status = Topic.Map.empty;
        peer_status = Connected;
        parameters;
      }

  let penalty {stats; _} penalty =
    make
      {
        stats with
        behaviour_penalty = stats.behaviour_penalty +. float_of_int penalty;
      }

  let set_connected {stats; _} = make {stats with peer_status = Connected}

  let fresh_mesh_status () =
    let since = Time.now () in
    let during = Span.zero in
    Active {since; during}

  let fresh_topic_stats () =
    let mesh_status = fresh_mesh_status () in
    {
      mesh_status;
      first_message_deliveries = 0.0;
      mesh_message_deliveries_active = false;
      mesh_message_deliveries = 0.0;
      mesh_failure_penalty = 0.0;
      invalid_messages = 0.0;
    }

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
        (Option.map (fun status ->
             let topic_parameters = get_topic_params stats.parameters topic in
             let penalty =
               mesh_message_delivery_penalty topic_parameters status
             in
             {
               status with
               mesh_status = Inactive;
               mesh_message_deliveries_active = false;
               mesh_failure_penalty = status.mesh_failure_penalty +. penalty;
             }))
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
          (fun topic status ->
            let mesh_failure_penalty =
              match status.mesh_status with
              | Active _ ->
                  let topic_parameters =
                    get_topic_params stats.parameters topic
                  in
                  status.mesh_failure_penalty
                  +. mesh_message_delivery_penalty topic_parameters status
              | Inactive -> status.mesh_failure_penalty
            in
            {
              status with
              mesh_status = Inactive;
              first_message_deliveries = 0.0;
              mesh_message_deliveries_active = false;
              mesh_failure_penalty;
            })
          stats.topic_status
      in
      (* Mark peer as disconnected *)
      let stats =
        {stats with peer_status = Disconnected {expires}; topic_status}
      in
      make stats |> Option.some

  let increment_first_message_deliveries parameters topic ts =
    let cap =
      let topic_parameters = get_topic_params parameters topic in
      topic_parameters.first_message_deliveries_cap |> float_of_int
    in
    Float.min cap (ts.first_message_deliveries +. 1.)

  let increment_mesh_message_deliveries parameters topic ts =
    match ts.mesh_status with
    | Active _ ->
        let cap =
          let topic_parameters = get_topic_params parameters topic in
          topic_parameters.mesh_message_deliveries_cap |> float_of_int
        in
        Float.min cap (ts.mesh_message_deliveries +. 1.)
    | Inactive -> ts.mesh_message_deliveries

  let first_message_delivered ({stats; _} : t) (topic : Topic.t) =
    let increment ts =
      let first_message_deliveries =
        increment_first_message_deliveries stats.parameters topic ts
      in
      let mesh_message_deliveries =
        increment_mesh_message_deliveries stats.parameters topic ts
      in
      {
        mesh_status = ts.mesh_status;
        mesh_message_deliveries_active = ts.mesh_message_deliveries_active;
        mesh_failure_penalty = ts.mesh_failure_penalty;
        invalid_messages = ts.invalid_messages;
        mesh_message_deliveries;
        first_message_deliveries;
      }
    in

    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> fresh_topic_stats () |> increment |> Option.some
          | Some ts -> increment ts |> Option.some)
        stats.topic_status
    in
    make {stats with topic_status}

  let duplicate_message_delivered ({stats; _} : t) (topic : Topic.t)
      (validated : Time.t) =
    let increment ts =
      let topic_parameters = get_topic_params stats.parameters topic in
      let window_upper_bound =
        Time.add validated topic_parameters.mesh_message_deliveries_window
      in
      if Time.(now () <= window_upper_bound) then
        let mesh_message_deliveries =
          increment_mesh_message_deliveries stats.parameters topic ts
        in
        {ts with mesh_message_deliveries}
      else ts
    in
    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> fresh_topic_stats () |> increment |> Option.some
          | Some ts -> increment ts |> Option.some)
        stats.topic_status
    in
    make {stats with topic_status}

  let invalid_message_delivered ({stats; _} : t) (topic : Topic.t) =
    let increment ts = {ts with invalid_messages = ts.invalid_messages +. 1.} in
    let topic_status =
      Topic.Map.update
        topic
        (function
          | None -> fresh_topic_stats () |> increment |> Option.some
          | Some ts -> increment ts |> Option.some)
        stats.topic_status
    in
    make {stats with topic_status}

  let set_application_score ({stats; _} : t) application_score =
    let stats = {stats with application_score} in
    make stats

  let refresh_mesh_status now mesh_status =
    match mesh_status with
    | Inactive -> mesh_status
    | Active {since; during = _} ->
        let during = Time.sub now (Time.to_span since) |> Time.to_span in
        Active {since; during}

  let time_active_in_mesh mesh_status =
    match mesh_status with
    | Inactive -> Span.zero
    | Active {during; _} -> during

  let refresh_stats now {stats; _} =
    let parameters = stats.parameters in
    let decay_zero = parameters.decay_zero in
    let topic_status =
      Topic.Map.mapi
        (fun topic status ->
          let topic_parameters = get_topic_params stats.parameters topic in
          let mesh_status = refresh_mesh_status now status.mesh_status in
          let mesh_message_deliveries_active =
            Span.(
              topic_parameters.mesh_message_deliveries_activation
              <= time_active_in_mesh mesh_status)
          in
          (* Apply decay to counters *)
          let first_message_deliveries =
            decay
              ~decay_zero
              ~decay_rate:topic_parameters.first_message_deliveries_decay
              status.first_message_deliveries
          in
          let mesh_message_deliveries =
            decay
              ~decay_zero
              ~decay_rate:topic_parameters.mesh_message_deliveries_decay
              status.mesh_message_deliveries
          in
          let mesh_failure_penalty =
            decay
              ~decay_zero
              ~decay_rate:topic_parameters.mesh_failure_penalty_decay
              status.mesh_failure_penalty
          in
          let invalid_messages =
            decay
              ~decay_zero
              ~decay_rate:topic_parameters.invalid_message_deliveries_decay
              status.invalid_messages
          in
          {
            mesh_status;
            mesh_message_deliveries_active;
            first_message_deliveries;
            mesh_message_deliveries;
            mesh_failure_penalty;
            invalid_messages;
          })
        stats.topic_status
    in
    let behaviour_penalty =
      decay
        ~decay_zero
        ~decay_rate:parameters.behaviour_penalty_decay
        stats.behaviour_penalty
    in
    make {stats with topic_status; behaviour_penalty}

  let refresh ps =
    let current = Time.now () in
    let refresh () = Some (refresh_stats current ps) in
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
        field "behaviour_penalty" (fun s -> s.behaviour_penalty) float;
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

  module Internal_for_tests = struct
    let get_topic_params = get_topic_params
  end
end
