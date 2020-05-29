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

(** Spawn Tezos nodes and control them. *)

(** Convention: in this module, some functions implement node commands;
    those functions are named after those commands.
    For instance, [Node.config_init] corresponds to [tezos-node config init],
    and [Node.run] corresponds to [tezos-node run].

    The arguments of those functions are also named after the actual arguments.
    For instance, [?network] is named after [--network], to make
    [Node.config_init ~network:"carthagenet"] look as close as possible
    to [tezos-node config init --network carthagenet].

    Most options have default values which are not necessarily the default values
    of [tezos-node]. Indeed, the latter are tailored for Mainnet, but here we
    use defaults which are tailored for the sandbox. In particular, the default
    value for [?network] is ["sandbox"].
    However, if you specify an option such as [~network] or [~history_mode],
    they are passed to the node unchanged, to reduce surprises.

    These conventions are also followed in the [Client] module. *)

(** Tezos node states. *)
type t

(** Create a node.

    This function just creates the [t] value, it does not call
    [identity_generate] nor [config_init] nor [run].

    The standard output and standard error output of the node will
    be logged with prefix [name] and color [color].

    Default [data_dir] is a temporary directory
    which is always the same for each [name].

    Default [event_pipe] is a temporary file
    whose name is derived from [name]. It will be created
    as a named pipe so that node events can be received.

    Default values for [net_port] or [rpc_port] are chosen automatically
    with values starting from 19732. They are used by [config_init]
    and by functions from the [Client] module. They are not used by [run],
    so if you do not call [config_init] or generate the configuration file
    through some other means, your node will not listen. *)
val create :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?net_port:int ->
  ?rpc_port:int ->
  unit ->
  t

(** Get the name of a node. *)
val name : t -> string

(** Get the network port given as [--net-addr] to a node. *)
val net_port : t -> int

(** Get the RPC port given as [--rpc-addr] to a node. *)
val rpc_port : t -> int

(** Send SIGTERM to a node and wait for it to terminate. *)
val terminate : t -> unit Lwt.t

(** {2 Commands} *)

(** Run [tezos-node identity generate]. *)
val identity_generate : ?expected_pow:int -> t -> unit Lwt.t

(** History modes for the node. *)
type history_mode = Archive | Full | Rolling

(** Convert an history mode into a string.

    The result is suitable to be passed to the node on the command-line. *)
val show_history_mode : history_mode -> string

(** Run [tezos-node config init].

    [net_port] and [rpc_port] can be used to override the port
    which was chosen by [create]. They will change
    the result of the {!net_port} and {!rpc_port} functions. *)
val config_init :
  ?network:string ->
  ?net_port:int ->
  ?rpc_port:int ->
  ?history_mode:history_mode ->
  t ->
  unit Lwt.t

(** Spawn [tezos-node run]. *)
val run :
  ?expected_pow:int ->
  ?single_process:bool ->
  ?bootstrap_threshold:int ->
  ?connections:int ->
  t ->
  unit

(** {2 Events} *)

(** Exception raised by [wait_for] functions if the node terminates before the event.

    You may catch or let it propagate to cause the test to fail.
    [node] is the name of the node.
    [event] is the name of the event.
    [where] is an additional optional constraint, such as ["level >= 10"]. *)
exception
  Terminated_before_event of {
    node : string;
    event : string;
    where : string option;
  }

(** Wait until the node is ready.

    More precisely, wait until a [node_is_ready] event occurs.
    If such an event already occurred, return immediately. *)
val wait_for_ready : t -> unit Lwt.t

(** Wait for a given chain level.

    More precisely, wait until a [node_chain_validator] with a [level]
    greater or equal to the requested level occurs.
    If such an event already occurred, return immediately. *)
val wait_for_level : t -> int -> int Lwt.t

(** Wait for the node to read its identity.

    More precisely, wait until a [read_identity] event occurs.
    If such an event already occurred, return immediately.

    Return the identity. *)
val wait_for_identity : t -> string Lwt.t

(** Wait for a custom event to occur.

    Usage: [wait_for node name filter]

    If an event named [name] occurs, apply [filter] to its value.
    If [filter] returns [None], continue waiting.
    If [filter] returns [Some x], return [x].

    [where] is used as the second field of the [Terminated_before_event] exception
    if the node terminates. It should describe the constraint that [filter] applies,
    such as ["field level exists"].

    It is advised to register such event handlers before starting the node,
    as if they occur before being registered, they will not trigger your handler.
    For instance, you can define a promise with
    [let x_event = wait_for node "x" (fun x -> Some x)]
    and bind it later with [let* x = x_event]. *)
val wait_for :
  ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t}

(** Add a callback to be called whenever the node emits an event.

    Contrary to [wait_for] functions, this callback is never removed.

    Listening to events with [on_event] will not prevent [wait_for] promises
    to be fulfilled. You can also have multiple [on_event] handlers, although
    the order in which they trigger is unspecified. *)
val on_event : t -> (event -> unit) -> unit

(** {2 High-Level Functions} *)

(** Initialize a node.

    This {!create}s a node, runs {!identity_generate}, {!config_init} and {!run},
    then waits for the node to be ready, and finally returns the node. *)
val init :
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?expected_pow:int ->
  ?network:string ->
  ?net_port:int ->
  ?rpc_port:int ->
  ?history_mode:history_mode ->
  ?single_process:bool ->
  ?bootstrap_threshold:int ->
  ?connections:int ->
  unit ->
  t Lwt.t
