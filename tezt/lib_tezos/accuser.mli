(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Spawn Tezos accuser and control them. *)

(** Tezos accuser states. *)
type t

(** Raw events. *)
type event = {name : string; value : JSON.t}

(** Create an accuser.

    This function just creates the [t] value, it does not call [run].

    The path to accuser binary is chosen from the [protocol].

    The standard output and standard error output of the accuser will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that accuser
    events can be received.

    [base_dir] corresponds to the (useless) "--base-dir" argument of
    the tezos-accuser command.

    The [Node.t] parameter is the accuser's node target. The accuser
    will be configured to be synchronised with the given node, and
    will communicate with it.

    If [runner] is specified, the accuser will be spawned on this
    runner using SSH. *)
val create :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?base_dir:string ->
  ?runner:Runner.t ->
  Node.t ->
  t

(** Get the name of an accuser. *)
val name : t -> string

(** Get the RPC port of the associated node. *)
val node_rpc_port : t -> int

(** Send SIGTERM (or SIGKILL) to an accuser and wait for it to terminate. *)
val terminate : ?kill:bool -> t -> unit Lwt.t

(** {2 Commands} *)

(** Spawn [tezos-accuser run].

    The resulting promise is fulfilled as soon as the accuser has been spawned.
    It continues running in the background. *)
val run : t -> unit Lwt.t

(** {2 Events} *)

(** Exception raised by [wait_for] functions if the node terminates
    before the event.

    You may catch or let it propagate to cause the test to fail.
    [daemon] is the name of the accuser.
    [event] is the name of the event.
    [where] is an additional optional constraint, such as ["level >= 10"]. *)
exception
  Terminated_before_event of {
    daemon : string;
    event : string;
    where : string option;
  }

(** Wait until the accuser is ready.

    More precisely, wait until the node on which the accuser is
    connected to is bootstrapped, and then, the accuser is ready. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait for a custom event to occur.

    Usage: [wait_for accuser name filter]

    If an event named [name] occurs, apply [filter] to its value.
    If [filter] returns [None], continue waiting.
    If [filter] returns [Some x], return [x].

    [where] is used as the [where] field of the
    [Terminated_before_event] exception if the accuser terminates. It
    should describe the constraint that [filter] applies, such as
    ["field level exists"].

    It is advised to register such event handlers before starting the accuser,
    as if they occur before being registered, they will not trigger your handler.
    For instance, you can define a promise with
    [let x_event = wait_for accuser "x" (fun x -> Some x)]
    and bind it later with [let* x = x_event]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Add a callback to be called whenever the accuser emits an event.

    Contrary to [wait_for] functions, this callback is never removed.

    Listening to events with [on_event] will not prevent [wait_for] promises
    to be fulfilled. You can also have multiple [on_event] handlers, although
    the order in which they trigger is unspecified. *)
val on_event : t -> (event -> unit) -> unit

(** Register an event handler that logs all events.

    Use this when you need to debug or reverse engineer incoming events.
    Usually you do not want to keep that in the final versions of your tests. *)
val log_events : t -> unit

(** {2 High-Level Functions} *)

(** Initialize an accuser.

    This {!create}s an accuser and {!run}, then waits for the accuser
    to be ready, and finally returns the accuser.

    As the accuser usually needs to be connected to a node, we first
    wait for the node to be ready and then, run the accuser. If one
    does not want to wait for the node to be ready, it is necessary to
    use [create] and then [run].

    The path to accuser binary is chosen from the [protocol].

    The standard output and standard error output of the accuser will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that accuser
    events can be received.

    [base_dir] corresponds to the (useless) "--base-dir" argument of
    the tezos-accuser command.

    The [Node.t] parameter is the accuser's node target. The accuser
    will be configured to be synchronised with the given node, and
    will communicate with it.

    If [runner] is specified, the accuser will be spawned on this
    runner using SSH. *)
val init :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?base_dir:string ->
  ?runner:Runner.t ->
  Node.t ->
  t Lwt.t

(** Restart an accuser.

    This {!terminate}s an accuser, then {!run}s it again and waits for
    it to be ready. *)
val restart : t -> unit Lwt.t
