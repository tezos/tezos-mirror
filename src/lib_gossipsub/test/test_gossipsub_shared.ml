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

module Milliseconds = struct
  type t = {ms : int}

  let pp fmtr {ms} = Format.fprintf fmtr "%dms" ms

  let of_int_ms ms = {ms}

  let of_float_s f = {ms = int_of_float @@ (f *. 1000.)}

  let of_int_s n = {ms = n * 1000}

  let to_int_ms {ms} = ms

  let zero = {ms = 0}

  let add m1 m2 = of_int_ms (m1.ms + m2.ms)

  let sub m1 m2 = of_int_ms (m1.ms - m2.ms)

  let to_float_s {ms} = float ms /. 1000.

  let to_int_s ms = to_float_s ms |> int_of_float

  let compare m1 m2 = Int.compare m1.ms m2.ms

  module Compare = Compare.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module Span = struct
    type nonrec t = t

    let zero = zero

    let one_second = {ms = 1000}

    let to_int_s = to_int_s

    let to_int_ms = to_int_ms

    let to_float_s = to_float_s

    let of_int_s = of_int_s

    let of_float_s = of_float_s

    let add = add

    let sub = sub

    let mul m s = of_int_ms (m.ms * s)

    let pp = pp

    include Compare
  end

  type span = Span.t

  let mul_span = Span.mul

  let to_span = Fun.id

  include Compare
end

module Time = struct
  let t = ref Milliseconds.zero

  let now () = !t

  let elapse ms =
    assert (Milliseconds.(ms >= zero)) ;
    t := Milliseconds.add !t ms

  let set now = t := now

  let reset () = t := Milliseconds.zero
end

(* Hook used to check message validity. By default,
   all messages are valid. *)
module Validity_hook = struct
  let validity = ref (fun _msg _msg_id -> `Valid)

  let set f = validity := f

  let apply msg msg_id = !validity msg msg_id
end

module Automaton_config :
  AUTOMATON_CONFIG
    with type Time.t = Milliseconds.t
     and type Span.t = Milliseconds.span
     and type Subconfig.Peer.t = int
     and type Subconfig.Topic.t = string
     and type Subconfig.Message_id.t = int
     and type Subconfig.Message.t = string = struct
  module Span = Milliseconds.Span

  module Time = struct
    include Milliseconds

    let now = Time.now
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

    module Message = struct
      include String_iterable

      let valid msg msg_id = Validity_hook.apply msg msg_id
    end
  end
end

module C = Automaton_config
module GS = Tezos_gossipsub.Make (C)

let pp_per_topic_score_limits =
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
        GS.Span.pp;
      Fmt.Dump.field
        "first_message_deliveries_weight"
        (fun l -> l.first_message_deliveries_weight)
        Fmt.float;
      Fmt.Dump.field
        "first_message_deliveries_cap"
        (fun l -> l.first_message_deliveries_cap)
        Fmt.int;
    ]

let pp_topic_score_limits fmtr tsp =
  match tsp with
  | Topic_score_limits_single p -> pp_per_topic_score_limits fmtr p
  | Topic_score_limits_family _ -> Format.fprintf fmtr "<...>"

let pp_score_limits =
  Fmt.Dump.(
    record
      [
        field "topics" (fun sp -> sp.topics) pp_topic_score_limits;
        field "app_specific_weight" (fun sp -> sp.app_specific_weight) Fmt.float;
        field
          "behaviour_penalty_weight"
          (fun sp -> sp.behaviour_penalty_weight)
          Fmt.float;
        field
          "behaviour_penalty_threshold"
          (fun sp -> sp.behaviour_penalty_threshold)
          Fmt.float;
        field
          "behaviour_penalty_decay"
          (fun sp -> sp.behaviour_penalty_decay)
          Fmt.float;
        field "decay_zero" (fun sp -> sp.decay_zero) Fmt.float;
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
    graft_flood_threshold;
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
    opportunistic_graft_ticks;
    opportunistic_graft_peers;
    opportunistic_graft_threshold;
    seen_history_length;
    score_limits;
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
     graft_flood_threshold = %a;@;\
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
     opportunistic_graft_ticks = %Ld;@;\
     opportunistic_graft_peers = %d;@;\
     opportunistic_graft_threshold = %f;@;\
     seen_history_length = %d;@;\
     score_limits= %a }@]"
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
    graft_flood_threshold
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
    opportunistic_graft_ticks
    opportunistic_graft_peers
    opportunistic_graft_threshold
    seen_history_length
    pp_score_limits
    score_limits

(** Instantiate the worker functor *)
module Worker_config = struct
  module GS = GS

  module Monad = struct
    type 'a t = 'a Lwt.t

    let ( let* ) = Lwt.bind

    let return = Lwt.return

    let sleep (span : GS.Span.t) =
      Lwt_unix.sleep @@ Milliseconds.Span.to_float_s span
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
module Message_cache =
  Tezos_gossipsub.Internal_for_tests.Message_cache
    (Automaton_config.Subconfig)
    (Automaton_config.Time)
