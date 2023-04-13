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

module Time = struct
  let t = ref 0

  let now () = !t

  let elapse dt =
    assert (dt >= 0) ;
    t := !t + dt

  let set now = t := now

  let reset () = t := 0
end

module Automaton_config :
  AUTOMATON_CONFIG
    with type Time.t = int
     and type Span.t = int
     and type Subconfig.Peer.t = int
     and type Subconfig.Topic.t = string
     and type Subconfig.Message_id.t = int
     and type Subconfig.Message.t = string = struct
  module Span = struct
    type t = int

    let zero = 0

    let seconds = Fun.id

    let pp = Format.pp_print_int
  end

  module Time = struct
    type span = Span.t

    let pp = Format.pp_print_int

    let now = Time.now

    let add = ( + )

    let sub = ( - )

    let mul_span = ( * )

    let to_span = Fun.id

    include Compare.Int
  end

  module Int_iterable = struct
    include Compare.Int

    let pp = Format.pp_print_int

    module Map = Map.Make (Int)
    module Set = Set.Make (Int)
  end

  module String_iterable = struct
    include Compare.String

    let pp = Format.pp_print_string

    module Map = Map.Make (String)
    module Set = Set.Make (String)
  end

  module Subconfig = struct
    module Peer = Int_iterable
    module Topic = String_iterable

    module Message_id = struct
      include Int_iterable

      let get_topic i = string_of_int (i mod 10)
    end

    module Message = String_iterable
  end
end

module C = Automaton_config
module GS = Tezos_gossipsub.Make (C)

let pp_per_topic_score_parameters =
  Fmt.Dump.record
    [
      Fmt.Dump.field
        "time_in_mesh_weight"
        (fun l -> l.time_in_mesh_weight)
        Fmt.float;
      Fmt.Dump.field "time_in_mesh_cap" (fun l -> l.time_in_mesh_cap) Fmt.float;
      Fmt.Dump.field
        "time_in_mesh_quantum"
        (fun l -> l.time_in_mesh_quantum)
        Fmt.float;
      Fmt.Dump.field
        "first_message_deliveries_weight"
        (fun l -> l.first_message_deliveries_weight)
        Fmt.float;
      Fmt.Dump.field
        "first_message_deliveries_cap"
        (fun l -> l.first_message_deliveries_cap)
        Fmt.int;
    ]

let pp_topic_score_parameters fmtr tsp =
  match tsp with
  | Topic_score_parameters_single p -> pp_per_topic_score_parameters fmtr p
  | Topic_score_parameters_family _ -> Format.fprintf fmtr "<...>"

let pp_score_parameters =
  Fmt.Dump.(
    record
      [
        field "topics" (fun sp -> sp.topics) pp_topic_score_parameters;
        field
          "behaviour_penalty_weight"
          (fun sp -> sp.behaviour_penalty_weight)
          Fmt.float;
        field
          "behaviour_penalty_threshold"
          (fun sp -> sp.behaviour_penalty_threshold)
          Fmt.float;
      ])

let pp_limits fmtr
    (l : (GS.Topic.t, GS.Peer.t, GS.Message_id.t, GS.span) limits) =
  let {
    max_recv_ihave_per_heartbeat;
    max_sent_iwant_per_heartbeat;
    max_gossip_retransmission;
    degree_optimal;
    publish_threshold;
    gossip_threshold;
    do_px;
    peers_to_px;
    accept_px_threshold;
    unsubscribe_backoff;
    graft_flood_backoff;
    prune_backoff;
    retain_duration;
    fanout_ttl;
    heartbeat_interval;
    backoff_cleanup_ticks;
    score_cleanup_ticks;
    degree_low;
    degree_high;
    degree_score;
    degree_out;
    degree_lazy;
    gossip_factor;
    history_length;
    history_gossip_length;
    score_parameters;
  } =
    l
  in
  let open Format in
  fprintf
    fmtr
    "@[<v 2>{ max_recv_ihave_per_heartbeat = %d;@;\
     max_sent_iwant_per_heartbeat = %d;@;\
     max_gossip_retransmission = %d;@;\
     degree_optimal = %d;@;\
     publish_threshold = %f;@;\
     gossip_threshold = %f;@;\
     do_px = %b;@;\
     peers_to_px = %d;@;\
     accept_px_threshold = %f;@;\
     unsubscribe_backoff = %a;@;\
     graft_flood_backoff = %a;@;\
     prune_backoff = %a;@;\
     retain_duration = %a;@;\
     fanout_ttl = %a;@;\
     heartbeat_interval = %a;@;\
     backoff_cleanup_ticks = %d;@;\
     score_cleanup_ticks = %d;@;\
     degree_low = %d;@;\
     degree_high = %d;@;\
     degree_score = %d;@;\
     degree_out = %d;@;\
     degree_lazy = %d;@;\
     gossip_factor = %f;@;\
     history_length = %d;@;\
     history_gossip_length = %d;@;\
     score_parameters= %a }@]"
    max_recv_ihave_per_heartbeat
    max_sent_iwant_per_heartbeat
    max_gossip_retransmission
    degree_optimal
    publish_threshold
    gossip_threshold
    do_px
    peers_to_px
    accept_px_threshold
    GS.Span.pp
    unsubscribe_backoff
    GS.Span.pp
    graft_flood_backoff
    GS.Span.pp
    prune_backoff
    GS.Span.pp
    retain_duration
    GS.Span.pp
    fanout_ttl
    GS.Span.pp
    heartbeat_interval
    backoff_cleanup_ticks
    score_cleanup_ticks
    degree_low
    degree_high
    degree_score
    degree_out
    degree_lazy
    gossip_factor
    history_length
    history_gossip_length
    pp_score_parameters
    score_parameters

(** Instantiate the worker functor *)
module Worker_config = struct
  module GS = GS

  module Monad = struct
    type 'a t = 'a Lwt.t

    let ( let* ) = Lwt.bind

    let return = Lwt.return

    let sleep i = Lwt_unix.sleep @@ float_of_int i
  end

  module Stream = struct
    type 'a t = {stream : 'a Lwt_stream.t; pusher : 'a option -> unit}

    let empty () =
      let stream, pusher = Lwt_stream.create () in
      {stream; pusher}

    let push e t = t.pusher (Some e)

    let pop t =
      let open Lwt_syntax in
      let* r = Lwt_stream.get t.stream in
      match r with
      | Some r -> Lwt.return r
      | None ->
          Stdlib.failwith "Invariant: we don't push None values in the stream"

    let get_available t = Lwt_stream.get_available t.stream
  end
end

module Worker = Tezos_gossipsub.Worker (Worker_config)
