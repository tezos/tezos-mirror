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

(** See [Daemon.Make.pid] *)
val pid : t -> int option

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
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

(** Wait until the agnostic baker is ready.

    More precisely, wait until a "Agnostic Baker started" event occurs.
    If this event alreay happened, return immediately.
 *)
val wait_for_ready : t -> unit Lwt.t

(* Wait for agnostic baker termination. *)
val wait_for_termination : t -> unit Lwt.t

(** Raw events. *)
type event = {name : string; value : JSON.t; timestamp : float}

(** See [Daemon.Make.on_event]. *)
val on_event : t -> (event -> unit) -> unit

(** Spawn [octez-baker run].

    The resulting promise is fulfilled as soon as the agnostic baker has been
    spawned. It continues running in the background. *)
val run :
  ?env:string String_map.t ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?extra_arguments:string list ->
  t ->
  unit Lwt.t

(** Spawn [octez-baker run] similarly to {!run} but returns the process. *)
val spawn_run :
  ?env:string String_map.t -> ?extra_arguments:string list -> t -> Process.t

(** Spawn [octez-baker].

    Similar to {!run} but takes all its arguments from [arguments]. *)
val raw : arguments:string list -> t -> unit Lwt.t

(** Spawn [octez-baker] similarly to {!spawn_run} but doesn't have any arguments
    other than [arguments]. *)
val spawn_raw : arguments:string list -> t -> Process.t

(** Liquidity baking vote values. *)
type liquidity_baking_vote = Off | On | Pass

(** Returns the string representation of a [liquidity_baking_vote]. *)
val liquidity_baking_vote_to_string : liquidity_baking_vote -> string

(** Returns the [liquidity_baking_vote] corresponding to a string, or None if the
    string is not a valid liquidity baking vote. *)
val liquidity_baking_vote_of_string_opt : string -> liquidity_baking_vote option

(** Writes a liquidity baking votefile, as read by the bakers [--votefile]
    argument.

    If [path] is set, the vote file is written there. Otherwise, it is written
    to a temporary file.

    Returns the path to the file that was written. *)
val liquidity_baking_votefile : ?path:string -> liquidity_baking_vote -> string

(** Number of extra levels to keep the old baker alive before shutting it down.
   This extra time is used to avoid halting the chain in cases such as
   reorganization or high round migration blocks. *)
val extra_levels_for_old_baker : int

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

    [votefile], [force_apply_from_round], [operations_pool], [state_recorder],
    [node_version_check_bypass], [node_version_allowed] and [liquidity_baking_toggle_vote] are
    passed to the baker daemon through the flags [--votefile], [--force-apply-from-round],
    [--operations-pool], [--record-state], [--node-version-check-bypass], [--node-version-allowed]
    and [--liquidity-baking-toggle-vote]. If [--liquidity-baking-toggle-vote]
    is [None], then [--liquidity-baking-toggle-vote] is not passed. If it is [Some x] then
    [--liquidity-baking-toggle-vote x] is passed. The default value is [Some Pass].

    If [remote_mode] is specified, the agnostic baker will run in RPC-only mode.

    If a [dal_node_rpc_endpoint] is specified, then the baker queries it in
    order to determine the DAL content of the attestations it sends to the L1
    node.  *)
val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  ?force_apply_from_round:int ->
  ?remote_mode:bool ->
  ?operations_pool:string ->
  ?dal_node_rpc_endpoint:Endpoint.t ->
  ?dal_node_timeout_percentage:int ->
  ?state_recorder:bool ->
  ?node_version_check_bypass:bool ->
  ?node_version_allowed:string ->
  ?keep_alive:bool ->
  ?allow_fixed_random_seed:bool ->
  ?allow_signing_delay:bool ->
  Node.t ->
  Client.t ->
  t

(** Similar to {!create}, but nodes RPCs addresses, wallet base directory and L1
    data directory are directly provided instead of a {!Client.t} and a {!Node.t}.

    The [node_data_dir] parameter provides the (local) node's data directory
    used for baking.

    The [node_rpc_endpoint] parameter provides the (local) node's RPC server
    endpoint to which the agnostic baker will connect to.

    The [base_dir] parameter contains needed information about the wallets used
    by the agnostic baker.

    If a [dal_node_rpc_endpoint] is specified, then the baker queries it in
    order to determine the DAL content of the attestations it sends to the L1
    node. *)
val create_from_uris :
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_pipe:string ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  ?force_apply_from_round:int ->
  ?remote_mode:bool ->
  ?operations_pool:string ->
  ?dal_node_rpc_endpoint:Endpoint.t ->
  ?dal_node_timeout_percentage:int ->
  ?state_recorder:bool ->
  ?node_version_check_bypass:bool ->
  ?node_version_allowed:string ->
  base_dir:string ->
  node_data_dir:string ->
  node_rpc_endpoint:Endpoint.t ->
  ?keep_alive:bool ->
  ?allow_fixed_random_seed:bool ->
  ?allow_signing_delay:bool ->
  unit ->
  t

(** Initialize a protocol-agnostic baker.

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
  ?env:string String_map.t ->
  ?runner:Runner.t ->
  ?path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?event_level:Daemon.Level.default_level ->
  ?event_pipe:string ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?delegates:string list ->
  ?votefile:string ->
  ?liquidity_baking_toggle_vote:liquidity_baking_vote option ->
  ?force_apply_from_round:int ->
  ?remote_mode:bool ->
  ?operations_pool:string ->
  ?dal_node_rpc_endpoint:Endpoint.t ->
  ?dal_node_timeout_percentage:int ->
  ?state_recorder:bool ->
  ?node_version_check_bypass:bool ->
  ?node_version_allowed:string ->
  ?keep_alive:bool ->
  ?allow_fixed_random_seed:bool ->
  ?allow_signing_delay:bool ->
  ?extra_arguments:string list ->
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
