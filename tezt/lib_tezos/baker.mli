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

(** See [Daemon.Make.terminate]. *)
val terminate : ?kill:bool -> t -> unit Lwt.t

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
type event = {name : string; value : JSON.t}

(** See [Daemon.Make.on_event]. *)
val on_event : t -> (event -> unit) -> unit

(** Spawn [tezos-baker run].

    The resulting promise is fulfilled as soon as the baker has been spawned.  It
    continues running in the background.*)
val run : t -> unit Lwt.t

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
    passed. The default value is [Some Pass]. *)
val create :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
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

    [votefile] and [liquidity_baking_toggle_vote] are passed to the baker daemon
    through the flags [--votefile] and [--liquidity-baking-toggle-vote]. *)
val init :
  protocol:Protocol.t ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?runner:Runner.t ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  Node.t ->
  Client.t ->
  t Lwt.t
