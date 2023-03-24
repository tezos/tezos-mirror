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

  let reset () = t := 0
end

module Automaton_config :
  AUTOMATON_CONFIG
    with type Time.t = int
     and type Span.t = int
     and type Peer.t = int
     and type Topic.t = string
     and type Message_id.t = int
     and type Message.t = string = struct
  module Span = struct
    type t = int

    let pp = Format.pp_print_int
  end

  module Time = struct
    type span = Span.t

    let pp = Format.pp_print_int

    let now = Time.now

    let add = ( + )

    let sub = ( - )

    let mul_span = ( * )

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

  module Peer = Int_iterable
  module Topic = String_iterable
  module Message_id = Int_iterable
  module Message = String_iterable
end

module C = Automaton_config
module GS = Tezos_gossipsub.Make (C)

let pp_limits fmtr (l : (GS.Peer.t, GS.Message_id.t, GS.span) limits) =
  let {
    max_recv_ihave_per_heartbeat;
    max_sent_iwant_per_heartbeat;
    degree_optimal;
    gossip_publish_threshold;
    accept_px_threshold;
    unsubscribe_backoff;
    graft_flood_backoff;
    prune_backoff;
    retain_duration;
    fanout_ttl;
    heartbeat_interval;
    backoff_cleanup_ticks;
    degree_low;
    degree_high;
    degree_score;
    degree_out;
    history_length;
    history_gossip_length;
  } =
    l
  in
  let open Format in
  fprintf
    fmtr
    "@[<v 2>{ max_recv_ihave_per_heartbeat = %d;@;\
     max_sent_iwant_per_heartbeat = %d;@;\
     degree_optimal = %d;@;\
     gossip_publish_threshold = %f;@;\
     accept_px_threshold = %f;@;\
     unsubscribe_backoff = %a;@;\
     graft_flood_backoff = %a;@;\
     prune_backoff = %a;@;\
     retain_duration = %a;@;\
     fanout_ttl = %a;@;\
     heartbeat_interval = %a;@;\
     backoff_cleanup_ticks = %d;@;\
     degree_low = %d;@;\
     degree_high = %d;@;\
     degree_score = %d;@;\
     degree_out = %d;@;\
     history_length = %d;@;\
     history_gossip_length = %d }@]"
    max_recv_ihave_per_heartbeat
    max_sent_iwant_per_heartbeat
    degree_optimal
    gossip_publish_threshold
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
    degree_low
    degree_high
    degree_score
    degree_out
    history_length
    history_gossip_length
