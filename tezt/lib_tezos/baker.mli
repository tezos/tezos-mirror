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

(** See [Daemon.Make.name] *)
val name : t -> string

(** Send SIGTERM and wait for the process to terminate.

    Default [timeout] is 30 seconds, after which SIGKILL is sent. *)
val terminate : ?timeout:float -> t -> unit Lwt.t

(** Send SIGKILL and wait for the process to terminate. *)
val kill : t -> unit Lwt.t

(** Send SIGSTOP to the process. *)
val stop : t -> unit Lwt.t

(** Send SIGCONT to the process. *)
val continue : t -> unit Lwt.t

(** See [Daemon.Make.log_events]. *)
val log_events : t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Wait until the baker is ready.

    More precisely, wait until a "Baker started" event occurs.
    If this event alreay happened, return immediately.
 *)
val wait_for_ready : t -> unit Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t; timestamp : float}

(** See [Daemon.Make.on_event]. *)
val on_event : t -> (event -> unit) -> unit

(** Spawn [octez-baker run].

    The resulting promise is fulfilled as soon as the baker has been spawned.  It
    continues running in the background.*)
val run :
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  t ->
  unit Lwt.t

(** Liquidity baking vote values. *)
type liquidity_baking_vote = Off | On | Pass

(** Returns the [liquidity_baking_vote] corresponding to a string, or None if the
    string is not a valid liquidity baking vote. *)
val liquidity_baking_vote_of_string_opt : string -> liquidity_baking_vote option

(** Returns the string representation of a [liquidity_baking_vote]. *)
val liquidity_baking_vote_to_string : liquidity_baking_vote -> string

(** Writes a liquidity baking votefile, as read by the bakers [--votefile]
    argument.

    If [path] is set, the vote file is written there. Otherwise, it is written
    to a temporary file.

    Returns the path to the file that was written. *)
val liquidity_baking_votefile : ?path:string -> liquidity_baking_vote -> string

(** Create a baker.

    This function just creates a value of type [t], it does not call {!val:run}.

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
    runner using SSH.

    [delegates] is a list of account aliases (see {!val:Account.key.alias}), e.g.,
    bootstrap accounts (see {!val:Constant.bootstrap_keys}), delegated to this
    baker. This defaults to the empty list, which is a shortcut for "every known
    account".

    [votefile] and [liquidity_baking_toggle_vote] are passed to the baker daemon
    through the flags [--votefile] and [--liquidity-baking-toggle-vote]. If
    [--liquidity-baking-toggle-vote] is [None], then
    [--liquidity-baking-toggle-vote]
    is not passed. If it is [Some x] then [--liquidity-baking-toggle-vote x] is
    passed. The default value is [Some Pass].

    [operations_pool] and [force_apply] are passed to the baker daemon through
    the flag [--operations-pool] and [--force_apply].

    If [dal_node] is specified, then it is the DAL node that the baker queries
    in order to determine the attestations it sends to the L1 node. A
    [--dal_node] argument is passed to specify the DAL node's endpoint. *)
val create :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  ?force_apply:bool ->
  ?operations_pool:string ->
  ?dal_node:Dal_node.t ->
  Node.t ->
  Client.t ->
  t

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
    runner using SSH.

    [delegates] is a list of account aliases (see {!val:Account.key.alias}), e.g.,
    bootstrap accounts (see {!val:Constant.bootstrap_keys}), delegated to this
    baker. This defaults to the empty list, which is a shortcut for "every known
    account".

    [votefile], [liquidity_baking_toggle_vote], [force_apply] respectively
    [operations_pool] are passed to the baker daemon through the flags
    [--votefile], [--liquidity-baking-toggle-vote], [--should-apply]
    respectively [--operations-pool].

    If [dal_node] is specified, then it is the DAL node that the baker queries
    in order to determine the attestations it sends to the L1 node. A
    [--dal_node] argument is passed to specify the DAL node's endpoint. *)
val init :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  ?force_apply:bool ->
  ?operations_pool:string ->
  ?dal_node:Dal_node.t ->
  Node.t ->
  Client.t ->
  t Lwt.t

(** Log block injection events.

    Show the baker daemon name, level and round of the block, and
    delegate for which it was injected.

    This log is relatively lightweight and a good indicator of chain
    progress during a test. It can also be useful to observe baking
    rights at the levels and rounds featured in a test. *)
val log_block_injection : ?color:Log.Color.t -> t -> unit

(** Log all baker events with [Log.info] on a single line each, which
    should be easily readable by a human.

    This function should not be called by any test on a permanent
    basis, but is available for debugging.

    If an event has an unexpected format, the anomaly is signaled with
    [Log.warn].

    If you want to see all events, don't forget to launch the baker with
{[
~event_sections_levels:
  [(String.concat "." [Protocol.encoding_prefix protocol; "baker"], `Debug)]
]} *)
val log_shortened_events : t -> unit
