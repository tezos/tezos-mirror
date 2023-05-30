(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

(** Spawn Data-availability-committee (DAC) nodes and control them. *)

(** DAC Node state. *)
type t

(** Creates a DAC node to run in legacy mode, using the specified threshold
    and list of dac members. *)
val create_legacy :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?reveal_data_dir:string ->
  threshold:int ->
  committee_members:string list ->
  ?committee_member_address:string ->
  node:Node.t ->
  client:Client.t ->
  unit ->
  t

(** Creates a DAC node to run in coordinator mode registered with
    the specified committee members. *)
val create_coordinator :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?reveal_data_dir:string ->
  ?allow_v1_api:bool ->
  committee_members:string list ->
  node:Node.t ->
  client:Client.t ->
  unit ->
  t

(** Creates a DAC node to run in committee_member mode, using the specified address,
    coordinator rpc host and port. *)
val create_committee_member :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?reveal_data_dir:string ->
  ?coordinator_rpc_host:string ->
  ?coordinator_rpc_port:int ->
  ?allow_v1_api:bool ->
  address:string ->
  node:Node.t ->
  client:Client.t ->
  unit ->
  t

(** Creates a DAC node to run in observer mode, using the specified coordinator
    rpc host and port and set the committee member endpoints to 
    [committee_member_rpcs]. *)
val create_observer :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?rpc_host:string ->
  ?rpc_port:int ->
  ?reveal_data_dir:string ->
  ?coordinator_rpc_host:string ->
  ?coordinator_rpc_port:int ->
  ?timeout:int ->
  ?allow_v1_api:bool ->
  committee_member_rpcs:(string * int) list ->
  node:Node.t ->
  client:Client.t ->
  unit ->
  t

(** Get the name of an dac node. *)
val name : t -> string

(** Get the mode in which a dac node is configured to run. Returned values can
    be either "Legacy", "Coordinator", "Commitee_member" or "Observer". *)
val mode : t -> string

(** Get the RPC host given as [--rpc-addr] to an dac node. *)
val rpc_host : t -> string

(** Get the RPC port given as [--rpc-addr] to an dac node. *)
val rpc_port : t -> int

(** Return the endpoint of the dac node, i.e., http://rpc_host:rpc_port. *)
val endpoint : t -> string

(** Get the data-dir of an dac node. *)
val data_dir : t -> string

(** Get the reveal-data-dir of an dac node. *)
val reveal_data_dir : t -> string

(** [allow_v1_api dac_node] is [true] if current node allows running [V1] API. *)
val allow_v1_api : t -> bool

(** Calls [ls] on reveal data dir. *)
val ls_reveal_data_dir : t -> string list Lwt.t

(** [run ?wait_ready ?env node] launches the given dac
    node where env is a map of environment variable.

    If [wait_ready] is [true], the promise waits for the dac node to be ready.
    [true] by default.
*)
val run : ?wait_ready:bool -> ?env:string String_map.t -> t -> unit Lwt.t

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Shows in stdout every events sent by the node *)
val log_events : t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** [is_running_not_ready dac_node] returns true if the given node is
    running but its status is not ready *)
val is_running_not_ready : t -> bool

(** Wait until a node terminates and return its status. If the node is not
    running, make the test fail. *)
val wait : t -> Unix.process_status Lwt.t

(** Run [octez-dac-node init-config]. Returns the name of the resulting
    configuration file.
*)
val init_config : t -> string Lwt.t

module Config_file : sig
  (** C node configuration files. *)

  (** Read the configuration file ([config.json]) of a DAC node. *)
  val read : t -> JSON.t

  (** Write the configuration file of a DAC node, replacing the existing one. *)
  val write : t -> JSON.t -> unit

  (** Update the configuration file of a DAC node. If the DAC node is already
      running, it needs to be restarted manually. *)
  val update : t -> (JSON.t -> JSON.t) -> unit
end

(** [with_sleeping_node] creates and runs an embedded node that sleeps for [timeout] 
    seconds upon receiving any request then returns "ok". It is used to test 
    timeout capabilities of clients. *)
val with_sleeping_node :
  ?rpc_port:int ->
  ?rpc_address:string ->
  timeout:float ->
  (string * int -> unit Lwt.t) ->
  unit Lwt.t
