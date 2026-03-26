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
    ~msg:"starting {agent} daemon"
    ("agent", string)

let stopping_daemon =
  declare_1
    ~section
    ~level:Notice
    ~name:"stopping_daemon"
    ~msg:"stopping {agent} daemon"
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

let head_stream_ended =
  declare_1
    ~section
    ~level:Notice
    ~name:"head_stream_ended"
    ~msg:"agnostic daemon lost stream connection with node {node}"
    ("node", string)

let head_stream_reconnected =
  declare_1
    ~section
    ~level:Notice
    ~name:"head_stream_reconnected"
    ~msg:"agnostic daemon managed to reconnect to node {node}"
    ("node", string)

let old_baker_stopped =
  declare_1
    ~section
    ~level:Notice
    ~name:"old_baker_stopped"
    ~msg:"agnostic daemon witnessed shutdown of protocol {protocol} baker"
    ~pp1:Protocol_hash.pp
    ("protocol", Protocol_hash.encoding)

(* Warning *)
let node_version_check_bypass =
  declare_0
    ~section
    ~name:"node_version_check_bypass"
    ~level:Warning
    ~msg:"Compatibility between node version and baker version by passed"
    ()

let retry_on_disconnection =
  declare_1
    ~section
    ~alternative_color
    ~level:Warning
    ~name:"retry_on_disconnection"
    ~msg:"{msg}"
    ~pp1:Format.pp_print_string
    ("msg", Data_encoding.string)

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

let new_head =
  declare_0
    ~section
    ~name:"new_head"
    ~level:Debug
    ~msg:"agnostic daemon received a new head"
    ()

let node_connection_lost =
  declare_1
    ~section
    ~name:"agnostic_daemon_node_connection_lost"
    ~level:Warning
    ~msg:"connection lost to node {uri}, will retry in 5 seconds"
    ("uri", Data_encoding.string)

let node_connection_restored =
  declare_1
    ~section
    ~name:"agnostic_daemon_node_connection_restored"
    ~level:Notice
    ~msg:"connection restored to node {uri}"
    ("uri", Data_encoding.string)

let node_connection_retry_failed =
  declare_2
    ~section
    ~name:"agnostic_daemon_node_connection_retry_failed"
    ~level:Warning
    ~msg:"failed to reconnect to node {uri}: {error}, retrying in 5 seconds"
    ("uri", Data_encoding.string)
    ~pp2:pp_print_top_error_of_trace
    ("error", trace_encoding)

let monitoring_head_from_node =
  declare_3
    ~section
    ~name:"agnostic_daemon_monitoring_head_from_node"
    ~level:Debug
    ~msg:"monitoring head at level {level} from node {uri} (block {block})"
    ~pp1:Format.pp_print_int
    ("level", Data_encoding.int31)
    ("uri", Data_encoding.string)
    ~pp3:Block_hash.pp_short
    ("block", Block_hash.encoding)

let skipping_lower_or_equal_level =
  declare_3
    ~section
    ~name:"agnostic_daemon_skipping_lower_or_equal_level"
    ~level:Debug
    ~msg:
      "skipping monitoring for head at level {level} from node {uri} (block \
       {block}): level is not greater than last seen level"
    ~pp1:Format.pp_print_int
    ("level", Data_encoding.int31)
    ("uri", Data_encoding.string)
    ~pp3:Block_hash.pp_short
    ("block", Block_hash.encoding)

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
end

module Commands = struct
  include Internal_event.Simple

  let section = section @ ["commands"]

  let no_dal_node_provided =
    declare_0
      ~section
      ~name:"no_dal_node_provided"
      ~level:Warning
      ~msg:
        "No DAL node endpoint has been provided.\n\
         Not running a DAL node might result in losing a share of the \
         participation rewards.\n\
         For instructions on how to run a DAL node, please visit \
         https://docs.tezos.com/tutorials/join-dal-baker."
      ()

  let healthy_dal_node =
    declare_0
      ~section
      ~name:"healthy_dal_node"
      ~level:Notice
      ~msg:"The DAL node is healthy."
      ()

  let unhealthy_dal_node =
    declare_2
      ~section
      ~name:"unhealthy_dal_node"
      ~level:Error
      ~msg:
        "The DAL node running on {endpoint} is not healthy. DAL attestations \
         cannot be sent. Its health is {health}. Please check that your DAL \
         node is configured correctly."
      ~pp1:Uri.pp
      ("endpoint", Tezos_rpc.Encoding.uri_encoding)
      ~pp2:Tezos_dal_node_services.Types.Health.pp
      ("health", Tezos_dal_node_services.Types.Health.encoding)

  let unreachable_dal_node =
    declare_1
      ~section
      ~name:"unreachable_dal_node"
      ~level:Error
      ~msg:
        "The DAL node cannot be reached on endpoint: {endpoint}.\n\
         Please check your DAL node and possibly restart it."
      ~pp1:Uri.pp
      ("endpoint", Tezos_rpc.Encoding.uri_encoding)
end
