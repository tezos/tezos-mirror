(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

module Make (C : Gossipsub_intf.AUTOMATON_CONFIG) :
  Gossipsub_intf.AUTOMATON
    with type Time.t = C.Time.t
     and type Span.t = C.Time.span
     and module Peer = C.Subconfig.Peer
     and module Topic = C.Subconfig.Topic
     and module Message_id = C.Subconfig.Message_id
     and module Message = C.Subconfig.Message
