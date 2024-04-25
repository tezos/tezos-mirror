(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Prometheus

(* The registry and the callback needs to be redefined because
   Prometheus always use the {!CollectRegistry.default}. However, if
   we use the default one, we link the metrics of the octez-node, the
   solution is to create a new registry and implement the callback
   that serves this new registry specifically. *)

let registry = CollectorRegistry.create ()

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* data = CollectorRegistry.collect registry in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
        in
        let headers =
          Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let namespace = Tezos_version.Octez_node_version.namespace

let subsystem = "evm_node"

module Chain = struct
  type t = {head : Gauge.t; confirmed_head : Gauge.t}

  let init name =
    let head =
      Gauge.v_label
        ~registry
        ~label_name:"head"
        ~help:"Level of the node's head"
        ~namespace
        ~subsystem
        "head"
        name
    in
    let confirmed_head =
      Gauge.v_label
        ~registry
        ~label_name:"confirmed_head"
        ~help:"Confirmed level (smart rollup node's head)"
        ~namespace
        ~subsystem
        "confirmed_head"
        name
    in
    {head; confirmed_head}
end

type t = {chain : Chain.t}

let metrics =
  let name = "Etherlink" in
  let chain = Chain.init name in
  {chain}

let set_level ~level = Gauge.set metrics.chain.head (Z.to_float level)

let set_confirmed_level ~level =
  Gauge.set metrics.chain.confirmed_head (Z.to_float level)
