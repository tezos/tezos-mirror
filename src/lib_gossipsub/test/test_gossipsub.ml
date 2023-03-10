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

include Tezos_gossipsub

module Configuration :
  CONFIGURATION
    with type Time.t = int
     and type Time.span = int
     and type Peer.t = int
     and type Topic.t = int
     and type Message_id.t = int
     and type Message.t = int = struct
  module Time = struct
    type span = int

    let now =
      let cpt = ref (-1) in
      fun () ->
        incr cpt ;
        !cpt

    let add = ( + )

    include Compare.Int
  end

  module Peer = struct
    type t = int

    module Map = Map.Make (Int)
    module Set = Set.Make (Int)
  end

  module Topic = Peer
  module Message_id = Peer
  module Message = Peer
end

module GS = Make (Configuration)

let limits =
  {
    max_recv_ihave_per_heartbeat = 0;
    max_sent_iwant_per_heartbeat = 0;
    expected_peers_per_topic = 0;
    gossip_publish_threshold = 0.;
    accept_px_threshold = 0.;
    unsubscribe_backoff = 0;
    graft_flood_backoff = 0;
    prune_backoff = 0;
    retain_duration = 0;
  }

let parameters = {peer_filter = (fun _peer _action -> true)}

(* This is to use a seed with Tezt. *)
let _seed =
  match
    Tezt_core.Cli.get
      ~default:None
      (fun x ->
        try int_of_string x |> Option.some |> Option.some
        with _ -> Option.none)
      "seed"
  with
  | None ->
      Random.self_init () ;
      Random.bits ()
  | Some seed -> seed

(* This dummy test can be removed once the first unit tests are
   registered. *)
let () =
  Tezt_core.Test.register ~__FILE__ ~title:"simple" ~tags:["simple"]
  @@ fun () ->
  let rng = Random.get_state () in
  let _state = GS.make rng limits parameters in
  Tezt_core.Base.unit

let () = Tezt.Test.run ()
