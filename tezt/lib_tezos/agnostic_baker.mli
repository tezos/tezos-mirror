(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** An agnostic baker instance *)
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
val log_events : ?max_length:int -> t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for : ?where:string -> t -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Wait until the agnostic baker is ready.

    More precisely, wait until a "Agnostic Baker started" event occurs.
    If this event alreay happened, return immediately.
 *)
val wait_for_ready : t -> unit Lwt.t

(** Spawn [octez-agnostic-baker run].

    The resulting promise is fulfilled as soon as the agnostic baker has been
    spawned. It continues running in the background. *)
val run :
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  t ->
  unit Lwt.t

(** Liquidity baking vote values. *)
type liquidity_baking_vote = Off | On | Pass

(** Returns the string representation of a [liquidity_baking_vote]. *)
val liquidity_baking_vote_to_string : liquidity_baking_vote -> string

(** Create a agnostic baker.

    This function just creates a value of type [t], it does not call {!val:run}.

    [path] provides the path to the agnostic baker binary, the default being the one
    derived from the [protocol].

    The standard output and standard error output of the agnostic baker will
    be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that agnostic baker
    events can be received.

    The [Node.t] parameter is the node used by the agnostic baker. The agnostic baker
    is configured to use the node's data dir.

    The [Client.t] parameter is the client used by the agnostic baker. The agnostic baker
    is configured to use the client's base directory.

    If [runner] is specified, the agnostic baker will be spawned on this
    runner using SSH.

    [delegates] is a list of account aliases (see {!val:Account.key.alias}), e.g.,
    bootstrap accounts (see {!val:Constant.bootstrap_keys}), delegated to this
    agnostic baker. This defaults to the empty list, which is a shortcut for "every known
    account".

    [liquidity_baking_toggle_vote] is passed to the agnostic baker
    daemon through the flags [--liquidity-baking-toggle-vote]. If
    [--liquidity-baking-toggle-vote] is [None], then
    [--liquidity-baking-toggle-vote] is not passed. If it is [Some x]
    then [--liquidity-baking-toggle-vote x] is passed. The default
    value is [Some Pass].

    If [remote_mode] is specified, the agnostic baker will run in RPC-only mode.
 *)
val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?delegates:string list ->
  ?remote_mode:bool ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  Node.t ->
  Client.t ->
  t

(** Similar to {!create}, but nodes RPCs addresses, wallet base directory and L1
    data directory are directly provided instead of a {!Client.t}, a {!Node.t}
    and optionally a {!Dal_node.t}.

    The [node_data_dir] parameter provides the (local) node's data directory
    used for baking.

    The [node_rpc_endpoint] parameter provides the (local) node's RPC server
    endpoint to which the agnostic baker will connect to.

    The [base_dir] parameter contains needed information about the wallets used
    by the agnostic baker.

 *)
val create_from_uris :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?delegates:string list ->
  ?remote_mode:bool ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  base_dir:string ->
  node_data_dir:string ->
  node_rpc_endpoint:Endpoint.t ->
  unit ->
  t

(** Initialize an agnositc baker.

    This creates agnostic baker, waits for it to be ready, and then
    returns it.

    As the agnostic baker usually relies on a node, we first wait for
    the node to be ready and then, run the agnostic baker.

    The path to the agnostic baker binary is chosen from the
    [protocol].

    The standard output and standard error output of the agnostic
    baker will be logged with prefix [name] and color [color].

    Default [event_pipe] is a temporary file whose name is derived
    from [name]. It will be created as a named pipe so that agnostic
    baker events can be received.

    The [Node.t] parameter is the node used by the agnostic baker. The
    agnostic baker is configured to use the node's data dir.

    The [Client.t] parameter is the client used by the agnostic
    baker. The agnostic baker is configured to use the client's base
    directory.

    If [runner] is specified, the agnostic baker will be spawned on
    this runner using SSH.

    [delegates] is a list of account aliases (see
    {!val:Account.key.alias}), e.g., bootstrap accounts (see
    {!val:Constant.bootstrap_keys}), delegated to this agnostic
    baker. This defaults to the empty list, which is a shortcut for
    "every known account".

    If [remote_mode] is specified, the agnostic baker will run in
    RPC-only mode. *)
val init :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_level:Daemon.Level.default_level ->
  ?event_pipe:string ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?delegates:string list ->
  ?remote_mode:bool ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  Node.t ->
  Client.t ->
  t Lwt.t
