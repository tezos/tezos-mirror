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

(** A baker instance *)
type t

(** Initialize a baker.

    This creates a baker, waits for it to be ready, and then returns it.

    As the baker usually relies on a node, we first
    wait for the node to be ready and then, run the baker.

    The path to the baker binary is chosen from the [protocol].

    The standard output and standard error output of the baker will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that baker
    events can be received.

    The [Node.t] parameter is the node used by the baker. The baker
    is configured to use the node's data dir.

    The [Client.t] parameter is the client used by the baker. The baker
    is configured to use the client's base directory.

    If [runner] is specified, the baker will be spawned on this
    runner using SSH. *)
val init :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?delegates:string list ->
  Node.t ->
  Client.t ->
  t Lwt.t

(** Specialised version of [Daemon.Make.terminate]. *)
val terminate : ?kill:bool -> t -> unit Lwt.t

(** Specialised version of [Daemon.Make.log_events]. *)
val log_events : t -> unit

(** Specialised version of [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t}

(** Specialised version of [Daemon.Make.on_event]. *)
val on_event : t -> (event -> unit) -> unit
