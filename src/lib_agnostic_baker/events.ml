(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = ["agnostic-agent"]

let alternative_color = Internal_event.Green

(* Notice *)
let starting_agent =
  declare_2
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"starting_agent"
    ~msg:"starting {agent} for protocol {proto}"
    ("agent", string)
    ("proto", Protocol_hash.encoding)
    ~pp2:Protocol_hash.pp_short

let agent_running =
  declare_2
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"agent_running"
    ~msg:"{agent} for protocol {proto} is now running"
    ("agent", string)
    ("proto", Protocol_hash.encoding)
    ~pp2:Protocol_hash.pp_short

let stopping_agent =
  declare_2
    ~section
    ~level:Notice
    ~name:"stopping_agent"
    ~msg:"stopping {agent} for protocol {proto}"
    ("agent", string)
    ("proto", Protocol_hash.encoding)
    ~pp2:Protocol_hash.pp_short

let starting_daemon =
  declare_1
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"starting_daemon"
    ~msg:"agnostic {agent} started"
    ("agent", string)

let stopping_daemon =
  declare_1
    ~section
    ~level:Notice
    ~name:"stopping_daemon"
    ~msg:"stopping agnostic {agent} daemon"
    ("agent", string)

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

let become_old_agent =
  declare_3
    ~section
    ~level:Notice
    ~name:"become_old_agent"
    ~msg:
      "The old {agent} for protocol {proto_hash} will be shut down at level \
       {level_to_kill}."
    ("agent", string)
    ("proto_hash", Protocol_hash.encoding)
    ~pp2:Protocol_hash.pp_short
    ("level_to_kill", int31)

let waiting_for_active_protocol =
  declare_0
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"waiting_for_active_protocol"
    ~msg:"waiting for active protocol"
    ()

let period_status =
  declare_3
    ~section
    ~alternative_color
    ~level:Notice
    ~name:"period_status"
    ~msg:
      "new block ({block}) on {period} period (remaining period duration \
       {remaining})"
    ("block", Block_hash.encoding)
    ("period", string)
    ("remaining", int31)

(* Error *)
let cannot_connect =
  declare_1
    ~section
    ~alternative_color
    ~level:Error
    ~name:"cannot_connect"
    ~msg:"Cannot connect to node. {message}"
    ~pp1:Format.pp_print_string
    ("message", Data_encoding.string)

(* Warning *)
let node_version_check_bypass =
  declare_0
    ~section
    ~name:"node_version_check_bypass"
    ~level:Warning
    ~msg:"Compatibility between node version and baker version by passed"
    ()

(* Debug *)
let node_version_check =
  declare_4
    ~section
    ~name:"node_version_check"
    ~level:Debug
    ~msg:
      "Checking compatibility between node version {node_version} \
       ({node_commit}) and baker version {baker_version} ({baker_commit})"
    ~pp1:Tezos_version.Version.pp_simple
    ("node_version", Tezos_version.Octez_node_version.version_encoding)
    ~pp2:
      (Format.pp_print_option
         Tezos_version.Octez_node_version.commit_info_pp_short)
    ( "node_commit",
      Data_encoding.option Tezos_version.Octez_node_version.commit_info_encoding
    )
    ~pp3:Tezos_version.Version.pp_simple
    ("baker_version", Tezos_version.Octez_node_version.version_encoding)
    ~pp4:
      (Format.pp_print_option
         Tezos_version.Octez_node_version.commit_info_pp_short)
    ( "baker_commit",
      Data_encoding.option Tezos_version.Octez_node_version.commit_info_encoding
    )

module Per_block_votes = struct
  include Internal_event.Simple

  let reading_per_block_votes =
    declare_1
      ~section
      ~name:"reading_per_block_votes"
      ~level:Notice
      ~msg:"reading votes file: {path}"
      ("path", Data_encoding.string)

  let liquidity_baking_toggle_vote =
    declare_1
      ~section
      ~name:"read_liquidity_baking_toggle_vote"
      ~level:Notice
      ~msg:"read liquidity baking toggle vote = {value}"
      ("value", Per_block_votes.liquidity_baking_vote_encoding)

  let per_block_vote_file_fail =
    declare_1
      ~section
      ~name:"per_block_vote_file_error"
      ~level:Error
      ~msg:"Error reading the block vote file: {errors}"
      ~pp1:pp_print_top_error_of_trace
      ("errors", Error_monad.(TzTrace.encoding error_encoding))

  let adaptive_issuance_vote =
    declare_1
      ~section
      ~name:"read_adaptive_issuance_vote"
      ~level:Notice
      ~msg:"read adaptive issuance vote = {value}"
      ("value", Per_block_votes.adaptive_issuance_vote_encoding)
end
