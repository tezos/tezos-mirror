(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Worker that handles the RPC server spawned as an external process
   by the node.

   This module defines an RPC process worker type [t] that is
   responsible of dealing with the process' lifetime. *)

(** Type of the RPC process worker*)
type t

(** [create ~comm_socket_path config node_version internal_event_config] creates
    the worker initial state. [comm_socket_path] is a socket path that will be
    used to communicate with the node (note that the socket is created
    automatically, but the cleaning of it is not handled). [config] contains all
    the RPC server configuration (such as ACLs, cors_headers, …). [node_version]
    contains informations regarding version of the node to show when answering
    related rpcs. *)
val create :
  comm_socket_path:string ->
  Config_file.t ->
  Tezos_version.Octez_node_version.t ->
  Internal_event_config.t ->
  t

(** Starts the external RPC process using fork+exec calls. It
    implements a watch dog that is responsible of restarting the
    process if it dies at some point. Additionally, if the process
    fails to be restarted, it retries with an exponential back-off
    until the restart is successful.
    The promise is blocking until the RPC server is not fully
    available to answer to PCs. *)
val start : t -> unit tzresult Lwt.t

(** Stops gracefully the RPC process worker*)
val stop : t -> unit Lwt.t
