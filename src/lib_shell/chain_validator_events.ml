(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

include Internal_event.Simple
module Request = Chain_validator_worker_state.Request

let section = ["validator"; "chain"]

let updated_to_checkpoint =
  declare_2
    ~section
    ~name:"updated_to_checkpoint"
    ~msg:"updated to checkpoint {block_hash} (running in mode {history_mode})"
    ~level:Notice
    ~pp1:Block_hash.pp
    ~pp2:History_mode.Legacy.pp
    ("block_hash", Block_hash.encoding)
    ("history_mode", History_mode.Legacy.encoding)

let prevalidator_filter_not_found =
  declare_1
    ~section
    ~name:"prevalidator_filter_not_found"
    ~msg:"no prevalidator filter found for protocol {protocol_hash}"
    ~level:Warning
    ~pp1:Protocol_hash.pp
    ("protocol_hash", Protocol_hash.encoding)

let prevalidator_reinstantiation_failure =
  declare_1
    ~section
    ~name:"prevalidator_reinstantiation_failure"
    ~msg:"failed to reinstantiate prevalidator error {trace}"
    ~level:Error
    ~pp1:pp_print_top_error_of_trace
    ("trace", trace_encoding)

let prevalidator_instantiation_failure =
  declare_1
    ~section
    ~name:"prevalidator_instantiation_failure"
    ~msg:"failed to instantiate the prevalidator: {trace}"
    ~level:Error
    ~pp1:pp_print_top_error_of_trace
    ("trace", trace_encoding)

let loading_protocol =
  declare_1
    ~section
    ~name:"loading_protocol"
    ~level:Notice
    ~msg:"loading non-embedded protocol {protocol} from disk"
    ~pp1:Protocol_hash.pp
    ("protocol", Protocol_hash.encoding)

let bootstrapped =
  declare_0
    ~section
    ~name:"bootstrapped"
    ~msg:"chain is bootstrapped"
    ~level:Notice
    ()

let synchronisation_status =
  declare_1
    ~section
    ~name:"synchronisation_status"
    ~msg:"synchronisation status: {status}"
    ~level:Notice
    ~pp1:Chain_validator_worker_state.sync_status_pp
    ("status", Chain_validator_worker_state.sync_status_encoding)

let could_not_switch_testchain =
  declare_1
    ~section
    ~name:"could_not_switch_testchain"
    ~msg:"error while switching testchina: {trace}"
    ~level:Error
    ~pp1:pp_print_top_error_of_trace
    ("trace", Error_monad.trace_encoding)

let request_failure =
  declare_3
    ~section
    ~name:"request_failure"
    ~msg:"chain validator request {view} failed ({worker_status}): {errors}"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:Worker_types.pp_status
    ~pp3:pp_print_top_error_of_trace
    ("view", Request.encoding)
    ("worker_status", Worker_types.request_status_encoding)
    ("errors", Error_monad.trace_encoding)

let notify_head =
  declare_2
    ~section
    ~name:"notify_head"
    ~msg:"head {block_hash} from {peer_id} processed"
    ~level:Debug
    ~pp1:P2p_peer.Id.pp_short
    ~pp2:Block_hash.pp_short
    ("peer_id", P2p_peer.Id.encoding)
    ("block_hash", Block_hash.encoding)

let notify_branch =
  declare_2
    ~section
    ~name:"notify_branch"
    ~msg:"branch up to {head_hash} from {peer_id} processed"
    ~level:Info
    ~pp1:P2p_peer.Id.pp_short
    ~pp2:Block_hash.pp_short
    ("peer_id", P2p_peer.Id.encoding)
    ("head_hash", Block_hash.encoding)

let connection =
  declare_1
    ~section
    ~name:"connection"
    ~msg:"connection of {peer_id}"
    ~level:Info
    ~pp1:P2p_peer.Id.pp
    ("peer_id", P2p_peer.Id.encoding)

let disconnection =
  declare_1
    ~section
    ~name:"disconnection"
    ~msg:"disconnection of {peer_id}"
    ~level:Info
    ~pp1:P2p_peer.Id.pp
    ("peer_id", P2p_peer.Id.encoding)

let ignore_head =
  declare_2
    ~section
    ~name:"ignore_head"
    ~msg:"current head is better than {view} (level {level}), we do not switch"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:(fun fmt -> Format.fprintf fmt "%li")
    ("view", Request.encoding)
    ("level", Data_encoding.int32)

let branch_switch =
  declare_2
    ~section
    ~name:"branch_switch"
    ~msg:"switching branch to {view} ({level})"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:(fun fmt -> Format.fprintf fmt "%li")
    ("view", Request.encoding)
    ("level", Data_encoding.int32)

let head_increment =
  declare_2
    ~section
    ~name:"head_increment"
    ~msg:"head is now {view} ({level})"
    ~level:Notice
    ~pp1:Request.pp
    ~pp2:(fun fmt -> Format.fprintf fmt "%li")
    ("view", Request.encoding)
    ("level", Data_encoding.int32)

let bootstrap_head_increment =
  declare_2
    ~section
    ~name:"bootstrap_time_remaining"
    ~msg:"synchronizing: current head is {timediff} old (level: {level})"
    ~level:Notice
    ("level", Data_encoding.int32)
    ~pp1:(fun fmt -> Format.fprintf fmt "%li")
    ("timediff", Time.System.Span.encoding)
    ~pp2:Time.System.Span.pp_hum

let block_info =
  declare_2
    ~section
    ~name:"block_info"
    ~msg:"treated block has timestamp {timestamp} with fitness {fitness}"
    ~level:Info
    ~pp1:Time.Protocol.pp_hum
    ~pp2:Fitness.pp
    ("timestamp", Time.Protocol.encoding)
    ("fitness", Fitness.encoding)

let bootstrap_active_peers =
  declare_2
    ~section
    ~name:"bootstrap_active_peers"
    ~msg:"bootstrap peers: active {active} needed {needed}"
    ~level:Debug
    ~pp1:Format.pp_print_int
    ~pp2:Format.pp_print_int
    ("active", Data_encoding.int31)
    ("needed", Data_encoding.int31)

let bootstrap_active_peers_heads_time =
  declare_2
    ~section
    ~name:"bootstrap_active_peers_heads_time"
    ~msg:"bootstrap peers: active {active} needed {needed}"
    ~level:Debug
    ~pp1:Format.pp_print_int
    ~pp2:Format.pp_print_int
    ("active", Data_encoding.int31)
    ("needed", Data_encoding.int31)
