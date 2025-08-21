(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [map_port ?bind_addr ?broadcast_addr ?timeout ?single_search_timeout
    ?local_addr ?lease_duration ~description ~local_port ~external_port
    ~any_net_port ()] will look for the gateway and ask for a redirection on
    [external_port] to [local_port]. If [any_net_port] is true, a port will be
    randomly selected by the gateway, otherwise it will use [external_port]. It
    returns the current machine address and the external port that serves for
    redirection.

    [bind_addr], [broadcast_addr], [timeout] and [single_search_timeout] are
    UPNP/IGD discovery parameters, default parameters should be enough and
    should be changed carefully (see documentation of `rust_igd_next`).

    [local_addr] is the local network address sent to the gateway for
    redirection. [map_port] can find it automatically, but it can be overridden
    in case it didn't find it automatically.

    [lease_duration] is the duration in seconds the redirection is kept by the
    gateway. `0` by default, meaning either an infinite duration for UPNP/IGD v1
    or 604800 seconds (two weeks) with UPNP/IGD v2.
*)
val map_port :
  ?bind_addr:string ->
  ?broadcast_addr:string ->
  ?timeout:float ->
  ?single_search_timeout:float ->
  ?local_addr:string ->
  ?lease_duration:int ->
  description:string ->
  local_port:int ->
  external_port:int ->
  any_net_port:bool ->
  unit ->
  (string * int) tzresult Lwt.t
