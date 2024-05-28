(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** A proxy server instance *)
type t

(** Command-line arguments of [octez-proxy-server].

    Not all arguments are available here, because it was not needed so far. *)
type argument =
  | Data_dir
      (** Whether to pass [--data-dir], to read the node's data-dir from disk,
          instead of using a RPC. This case doesn't need a parameter, because it is computed
          automatically from the {!Node.t} value in {!init}. Think
          of this argument as a Boolean flag (not an argument) whether to pass
          [--data-dir] to [octez-proxy-server]. *)
  | Symbolic_block_caching_time of int
      (** Time interval (in seconds) during which data for a symbolic block
          identifier (like HEAD) is kept. A symbolic identifier
          is a block identifier that it not a hash and which hence
          cannot be safely used as a key in any form of cache. *)

(** Get the RPC port of a proxy server. It's the port to
    do requests to. *)
val rpc_port : t -> int

(** Get the RPC host of a proxy server. It's the host to
    do requests to. Its value is [Constant.default_host]. *)
val rpc_host : string

(** Get the RPC scheme of a proxy server. Its value is ["http"]. *)
val rpc_scheme : string

(** Get the runner associated to a proxy server.

    Return [None] if the proxy server runs on the local machine. *)
val runner : t -> Runner.t option

(** [spawn ?rpc_port node] spawns a new proxy server that serves
    the given port and delegates its queries to [node].

    This function is meant to be used by callers that need finer control
    than what {!init} allows. *)
val spawn : ?rpc_port:int -> ?args:string list -> Node.t -> Process.t

(** [init ?runner ?name ?rpc_port ?event_level ?args node] creates and starts a proxy server
    that serves the given port and delegates its queries to [node].

    [event_level] specifies the verbosity of the file descriptor sink.

    [event_sections_levels] specifies the verbosity for events in sections whose
    prefix is in the list. See {!Node.run} for description. *)
val init :
  ?runner:Runner.t ->
  ?name:string ->
  ?rpc_port:int ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?args:argument list ->
  Node.t ->
  t Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t; timestamp : float}

(** Add a callback to be called whenever the proxy_server emits an event.

    This callback is never removed.

    You can have multiple [on_event] handlers, although
    the order in which they trigger is unspecified. *)
val on_event : t -> (event -> unit) -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Expose the RPC server address of this proxy server as a foreign endpoint. *)
val as_rpc_endpoint : t -> Endpoint.t
