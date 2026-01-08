(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** This module allows to configure NginX as a reverse proxy in
      front of a collection of DAL nodes. This allows to balance the
      load on the DAL nodes based on which DAL slot index is used
      while complying with the interface of the rollup node, which
      expects a single DAL node endpoint.

      The NginX reverse-proxy configuration is added as an NginX site
      (in directory /etc/nginx/sites-available/ which is symlinked in
      /etc/nginx/sites-enabled/). This module makes no assumption
      about the existence of other NginX sites and should be invisible
      to them except that:

      - it disables the "default" NginX site (the file
        /etc/nginx/sites-enabled/default is removed if it exists),
      - it overrides the "reverse_proxy" NgninX site if there is some.
  *)

(** Launch a DAL node per slot index and set up an NginX reverse proxy in front of them. *)
val init_dal_reverse_proxy_observers :
  external_rpc:bool ->
  network:Network.t ->
  snapshot:Snapshot_helpers.t ->
  ppx_profiling_verbosity:string option ->
  ppx_profiling_backends:string list ->
  memtrace:bool ->
  simulate_network:Network_simulation.t ->
  name_of:(int -> string) ->
  default_endpoint:string option ->
  node_p2p_endpoint:string ->
  dal_node_p2p_endpoint:string option ->
  dal_slots:int list ->
  index:int ->
  next_agent:(name:string -> Agent.t Lwt.t) ->
  otel:string option ->
  cloud:Cloud.t ->
  Dal_node.t Lwt.t
