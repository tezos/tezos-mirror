(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Parameters = struct
  type mode = Bundler of {endpoint : string} | Sequencer

  type persistent_state = {
    arguments : string list;
    rpc_addr : string;
    rpc_port : int;
    mode : mode;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "dsn_node"

  let default_colors = Log.Color.[|FG.red; FG.blue; FG.yellow; FG.green|]
end

include Daemon.Make (Parameters)

let bundler ?runner ?(rpc_addr = Constant.default_host)
    ?(rpc_port = Port.fresh ()) ~endpoint () =
  let server_addr = Format.asprintf "%s:%d" rpc_addr rpc_port in
  let arguments =
    ["bundler"; "--rpc-address"; server_addr; "--sequencer-url"; endpoint]
  in
  let persistent_state =
    Parameters.
      {arguments; rpc_addr; rpc_port; mode = Bundler {endpoint}; runner}
  in
  let bundler =
    create ~path:(Uses.path Constant.octez_dsn_node) persistent_state
  in
  bundler

let sequencer ?runner ?(rpc_addr = Constant.default_host)
    ?(rpc_port = Port.fresh ()) () =
  let server_addr = Format.asprintf "%s:%d" rpc_addr rpc_port in
  let arguments = ["sequencer"; "--rpc-address"; server_addr] in
  let persistent_state =
    Parameters.{arguments; rpc_addr; rpc_port; mode = Sequencer; runner}
  in
  let sequencer =
    create ~path:(Uses.path Constant.octez_dsn_node) persistent_state
  in
  sequencer

let endpoint (dsn_node : t) =
  Format.sprintf
    "http://%s:%d"
    dsn_node.persistent_state.rpc_addr
    dsn_node.persistent_state.rpc_port

let start daemon =
  run daemon Parameters.{ready = true} daemon.persistent_state.arguments
