(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Cmdliner

type discovery_options = {
  bind_addr : string option;
  broadcast_addr : string option;
  timeout : float option;
  single_search_timeout : float option;
}

type mapping_options = {
  local_addr : string option;
  lease_duration : int option;
  description : string option;
  any_net_port : bool;
}

let docs = "UPNP OPTIONS"

let exclusive_parameters =
  "--any-net-port and --advertised-net-port are exclusive."

let bind_addr =
  let doc = "The URL at which this instance can be reached." in
  Arg.(
    value
    & opt (some string) None
    & info ~docs ~doc ~docv:"ADDR:PORT" ["listen-addr"])

let broadcast_addr =
  let doc = "The address used for broadcasting the UPNP discovery requests." in
  Arg.(
    value
    & opt (some string) None
    & info ~docs ~doc ~docv:"ADDR:PORT" ["broadcast-addr"])

let discovery_timeout =
  let doc = "Timeout until UPNP discovery stops looking for a gateway." in
  Arg.(
    value
    & opt (some float) None
    & info ~docs ~doc ~docv:"SEC" ["discovery-timeout"])

let single_search_timeout =
  let doc =
    "Timeout until the node stops waiting for the first discovery response."
  in
  Arg.(
    value
    & opt (some float) None
    & info ~docs ~doc ~docv:"SEC" ["single-search-timeout"])

let local_addr =
  let doc = "Local network IP address on which the redirection is done." in
  Arg.(
    value & opt (some string) None & info ~docs ~doc ~docv:"ADDR" ["local-addr"])

let lease_duration =
  let doc =
    "Lease duration of the mapping, in seconds. Defaults to `0` which either \
     corresponds to an unlimited lease if the gateway implements UPNP/IGD v1, \
     or 604800 seconds for UPNP/IGD v2."
  in
  Arg.(
    value & opt (some int) None & info ~docs ~doc ~docv:"SEC" ["lease-duration"])

let any_net_port =
  let doc =
    "Asks the gateway to map any available port instead, and update the \
     configuration according to the result. This option should be used in \
     priority at first invocation of the command, as it ensures an available \
     port is selected by the gateway." ^ exclusive_parameters
  in
  Arg.(value & flag & info ~docs ~doc ~docv:"SEC" ["any-net-port"])

let description =
  let doc =
    "Name of the mapping, used by UPNP clients/routers to document the \
     redirection."
  in
  Arg.(
    value
    & opt (some string) None
    & info ~docs ~doc ~docv:"DESC" ["mapping-description"])

let search_options =
  let open Term.Syntax in
  let+ bind_addr
  and+ broadcast_addr
  and+ discovery_timeout
  and+ single_search_timeout in
  {
    bind_addr;
    broadcast_addr;
    timeout = discovery_timeout;
    single_search_timeout;
  }

let mapping_options =
  let open Term.Syntax in
  let+ local_addr and+ lease_duration and+ description and+ any_net_port in
  {local_addr; lease_duration; description; any_net_port}
