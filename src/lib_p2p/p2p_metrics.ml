(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let namespace = Tezos_version.Octez_node_version.namespace

let subsystem = "p2p"

(* we add this function to impose the correct namespace *)
let metric_counter ~component ~help ~name () =
  Prometheus.Counter.v
    ~help
    ~namespace
    ~subsystem
    (String.concat "_" [component; name])

module Messages = struct
  let component = "messages"

  let metric_counter = metric_counter ~component

  let broadcast_message_sent =
    metric_counter
      ~help:"Number of user message sent by broadcasting"
      ~name:"broadcast_message_sent"
      ()

  let user_message_sent =
    metric_counter
      ~help:"Number of user message sent"
      ~name:"user_message_sent"
      ()

  let user_message_received =
    metric_counter
      ~help:"Number of user message received"
      ~name:"user_message_received"
      ()

  let user_message_received_error =
    metric_counter
      ~help:"Number of user message received that resulted in error"
      ~name:"user_message_received_error"
      ()

  let advertise_received =
    metric_counter
      ~help:"Number of advertise received"
      ~name:"advertise_received"
      ()

  let advertise_sent =
    metric_counter ~help:"Number of advertise sent" ~name:"advertise_sent" ()

  let bootstrap_received =
    metric_counter
      ~help:"Number of bootstrap received"
      ~name:"bootstrap_received"
      ()

  let bootstrap_sent =
    metric_counter ~help:"Number of bootstrap sent" ~name:"bootstrap_sent" ()

  let swap_request_sent =
    metric_counter ~help:"Number of swap sent" ~name:"swap_requests_sent" ()

  let swap_request_received =
    metric_counter
      ~help:"Number of swap received"
      ~name:"swap_requests_received"
      ()

  let swap_ack_sent =
    metric_counter ~help:"Number of swap acks sent" ~name:"swap_ack_sent" ()

  let swap_ack_received =
    metric_counter
      ~help:"Number of swap acks received"
      ~name:"swap_ack_received"
      ()
end

module Swap = struct
  let component = "swap"

  let metric_counter = metric_counter ~component

  let ignored = metric_counter ~help:"Number of ignored swap" ~name:"ignored" ()

  let success =
    metric_counter ~help:"Number of successful swap" ~name:"success" ()

  let fail = metric_counter ~help:"Number of failed swap" ~name:"fail" ()
end
