(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module implements a generic disconnection scheduler, designed to
    periodically "disable" and "re-enable" indexed actors (e.g., nodes,
    bakers, DAL nodes) based on the current level of the chain. 

    It is suitable for simulating faults in a controlled and repeatable way. *)

open Scenarios_helpers
module IMap = Map.Make (Int)

type t = {
  disconnected_actors : int IMap.t;
      (** A map of actor indexes that have been disconnected, along with the level at
          which they were disconnected. *)
  frequency : int;
      (** The frequency (in levels) at which disconnections occur. *)
  reconnection_delay : int;
      (** The number of levels after which a disconnected actor should be reconnected. *)
  next_to_disconnect : int;
      (** The next actor index to disconnect, used for round-robin selection.
          It is 0 when no actor has been disconnected yet. *)
}

(** [init (frequency, reconnection_delay)] initializes a new disconnection
    scheduler. Every [frequency] levels, the next actor will be disconnected,
    and it will be reconnected after [reconnection_delay] levels. *)
let init (frequency, reconnection_delay) =
  if frequency <= 0 then
    Test.fail
      "Unexpected error: The disconnection frequency must be strictly \
       positive, rather than %d"
      frequency ;
  {
    disconnected_actors = IMap.empty;
    frequency;
    reconnection_delay;
    next_to_disconnect = 0;
  }

(** [disconnect t level f] attempts to disconnect the actor at index
    [t.next_to_disconnect] if [level] is a multiple of [t.frequency] and
    the actor is not already disconnected. Applies the function [f] to that index. *)
let disconnect t level f =
  if level mod t.frequency <> 0 then Lwt.return t
  else
    match IMap.find_opt t.next_to_disconnect t.disconnected_actors with
    | Some _ ->
        toplog
          "disconnect: all actors have been disconnected, waiting for next \
           actor to reconnect." ;
        Lwt.return t
    | None ->
        let* () = f t.next_to_disconnect in
        Lwt.return
          {
            t with
            disconnected_actors =
              IMap.add t.next_to_disconnect level t.disconnected_actors;
            next_to_disconnect = t.next_to_disconnect + 1;
          }

(** [reconnect t level f] applies the function [f] to each actor whose disconnection
    started [t.reconnection_delay] or more levels ago, i.e., they are eligible
    for reconnection. *)
let reconnect t level f =
  let actors_to_reconnect, actors_to_keep_disconnected =
    IMap.partition
      (fun _ disconnected_level ->
        level >= disconnected_level + t.reconnection_delay)
      t.disconnected_actors
  in
  let* () =
    IMap.to_seq actors_to_reconnect
    |> List.of_seq
    |> Lwt_list.iter_p (fun (b, _) -> f b)
  in
  Lwt.return {t with disconnected_actors = actors_to_keep_disconnected}
