(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Parameters = struct
  type persistent_state = {
    arguments : string list;
    rpc_addr : string;
    rpc_port : int;
    endpoint : string;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "evm_node"

  let default_colors = Log.Color.[|FG.green; FG.yellow; FG.cyan; FG.magenta|]
end

include Daemon.Make (Parameters)

let bundler ?runner ?(rpc_addr = Constant.default_host) ?(rpc_port = 3000)
    ~endpoint () =
  let open Tezos_base.TzPervasives.Lwt_syntax in
  let server_addr = Format.asprintf "%s:%d" rpc_addr rpc_port in
  let arguments =
    ["bundler"; "--rpc-address"; server_addr; "--sequencer-url"; endpoint]
  in
  let persistent_state =
    Parameters.{arguments; rpc_addr; rpc_port; endpoint; runner}
  in
  let bundler =
    create ~path:(Uses.path Constant.octez_dsn_node) persistent_state
  in
  let* () = run bundler Parameters.{ready = true} arguments in
  return bundler

let endpoint (dsn_node : t) =
  Format.sprintf
    "http://%s:%d"
    dsn_node.persistent_state.rpc_addr
    dsn_node.persistent_state.rpc_port
