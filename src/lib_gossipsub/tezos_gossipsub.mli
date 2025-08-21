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

module Gossipsub_intf = Gossipsub_intf

module Automaton (C : Gossipsub_intf.AUTOMATON_CONFIG) :
  Gossipsub_intf.AUTOMATON
    with type Time.t = C.Time.t
     and type Span.t = C.Time.span
     and module Peer = C.Subconfig.Peer
     and module Topic = C.Subconfig.Topic
     and module Message_id = C.Subconfig.Message_id
     and module Message = C.Subconfig.Message

module Worker (C : Gossipsub_intf.WORKER_CONFIGURATION) :
  Gossipsub_intf.WORKER
    with module GS = C.GS
     and module GS.Topic = C.GS.Topic
     and module GS.Peer = C.GS.Peer
     and module Monad = C.Monad
     and module Stream = C.Stream
     and module Point = C.Point

module Profiler = Gossipsub_profiler
