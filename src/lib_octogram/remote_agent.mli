(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module introduces the means to spawn and control agents on remote
    machine. *)

(** Specify how the agent gets the octogram binary. *)
type octogram_binary =
  | Push of {local_path : string}
      (** The orchestrator has to copy a local executable to the remote machine. *)
  | Pull of {url : string}
      (** The orchestrator retreives the executable from a HTTP server. *)
  | Already_installed of {bin : string}
      (** The remote machine already provides the octogram binary. *)

val octogram_binary_encoding : octogram_binary Data_encoding.t

(** A handler controlling an agent remotely. *)
type t

(** A handler identifying an asynchronous procedure sent to an agent, to be
    used to fetch its response. *)
type 'a request_handler

(** [wait agent] returns when the remote agent has successfully terminated. *)
val wait : t -> Unix.process_status Lwt.t

val name : t -> Agent_name.t

val runner : t -> Runner.t

val color : t -> Log.Color.t

val scope : t -> string -> string

val create :
  runner:Runner.t ->
  name:Agent_name.t ->
  on_new_metrics_source:
    (Agent_name.t -> string -> Services_cache.node_kind -> int -> unit) ->
  octogram_binary:octogram_binary ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  unit ->
  t

val run :
  ?on_terminate:(Unix.process_status -> unit Lwt.t) ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  t ->
  unit Lwt.t

(** [wait_for_ready agent] returns when the remote agent has been through its
    initialization process and is ready for receiving requests. *)
val wait_for_ready : t -> unit Lwt.t

(** [start_request state proc] creates a new request for the execution of
    [proc] and returns its handler. Use {!wait_for_request} to get the response
    back, when ready. *)
val start_request :
  t -> ('a, Uri.agent_uri) Remote_procedure.t -> 'a request_handler Lwt.t

(** [wait_for_request state handler] waits for the request identified by
    [handler] to complete, and the agent responsible for executing it to return
    its response. *)
val wait_for_request : t -> 'a request_handler -> 'a Lwt.t

val get_service_info :
  Services_cache.node_kind -> Services_cache.service_kind -> t -> string -> int

val to_tvalue : t -> Jingoo.Jg_types.tvalue
