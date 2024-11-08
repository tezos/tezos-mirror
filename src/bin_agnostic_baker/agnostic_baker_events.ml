(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["agnostic-baker"]

let alternative_color = Internal_event.Green

(* Notice *)
let starting_baker =
  declare_2
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"starting_baker"
    ~msg:"starting baker for protocol {proto} with arguments: {args}"
    ("proto", Protocol_hash.encoding)
    ("args", string)
    ~pp1:Protocol_hash.pp_short

let baker_running =
  declare_1
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"baker_running"
    ~msg:"baker for protocol {proto} is now running"
    ("proto", Protocol_hash.encoding)
    ~pp1:Protocol_hash.pp_short

let stopping_baker =
  declare_1
    ~section
    ~level:Notice
    ~name:"stopping_baker"
    ~msg:"stopping baker for protocol {proto}"
    ("proto", Protocol_hash.encoding)
    ~pp1:Protocol_hash.pp_short

let starting_daemon =
  declare_0
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"starting_daemon"
    ~msg:"agnostic baker started"
    ()

let stopping_daemon =
  declare_0
    ~section
    ~level:Notice
    ~name:"stopping_daemon"
    ~msg:"stopping agnostic daemon"
    ()

let protocol_encountered =
  declare_2
    ~section
    ~level:Notice
    ~name:"protocol_encountered"
    ~msg:"the {status} protocol {proto_hash} was encountered"
    ("status", Parameters.status_encoding)
    ~pp1:Parameters.pp_status
    ("proto_hash", Protocol_hash.encoding)
    ~pp2:Protocol_hash.pp_short

let waiting_for_active_protocol =
  declare_0
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"waiting_for_active_protocol"
    ~msg:"waiting for active protocol"
    ()

let period_status =
  declare_2
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"period_status"
    ~msg:"new block on {period} period (remaining period duration {remaining})"
    ("period", string)
    ("remaining", int31)
