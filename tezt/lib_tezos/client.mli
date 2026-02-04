(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

(** Run Tezos client commands. *)

module Time = Tezos_base.Time.System

(** Values that can be passed to the client's [--endpoint] argument *)
type endpoint =
  | Node of Node.t  (** A full-fledged node *)
  | Foreign_endpoint of Endpoint.t  (** A service not managed by Tezt *)

(** Values that can be passed to the client's [--adaptive-issuance-vote] argument *)
type ai_vote = On | Off | Pass

(** A string representation of an endpoint suitable to be used as a CLI
    argument (e.g., [http://127.0.0.1:5893]). *)
val string_of_endpoint : ?hostname:bool -> endpoint -> string

(** A string representation of an endpoint suitable to be used inside URLs
    argument (e.g., [http%3A%2F%2F127.0.0.1%3A5893]). *)
val url_encoded_string_of_endpoint : ?hostname:bool -> endpoint -> string

(** Values that can be passed to the client's [--media-type] argument *)
type media_type = Json | Binary | Any

(* Default delay used for the activation of a protocol *)
val default_protocol_activation_delay : Ptime.span

(** Values that can be passed to the client's [--timestamp] argument *)
type timestamp = Now | Ago of Time.Span.t | At of Time.t

(** Convert [timestamp] into a concrete [Time.t], relative to
    [Time.now ()]. *)
val time_of_timestamp : timestamp -> Time.t

(** [rpc_port endpoint] returns the port on which to reach [endpoint]
    when doing RPC calls. *)
val rpc_port : endpoint -> int

(** [address ?from endpoint] returns the address at which [endpoint] can be
    contacted. If [from] is provided, and if [from] and [endpoint] live in the
    same address, then ["127.0.0.1"] is returned (or ["localhost"] if
    [hostname] is [true]. *)
val address : ?hostname:bool -> ?from:endpoint -> endpoint -> string

(** [scheme endpoint] returns “http” or “https” depending on the configuration
    of the endpoint. *)
val scheme : endpoint -> string

(** Mode of the client *)
type mode =
  | Client of endpoint option * media_type option
  | Mockup
  | Light of float * endpoint list
  | Proxy of endpoint

(** [mode_to_endpoint mode] returns the {!endpoint} within a {!mode}
    (if any) *)
val mode_to_endpoint : mode -> endpoint option

(** The synchronization mode of the client.

    - [Asynchronous] mode is when transfer doesn't bake the block.
    - [Synchronous] is the default mode (no flag passed to [create mockup]). *)
type mockup_sync_mode = Asynchronous | Synchronous

(** The mode argument of the client's 'normalize data' command *)
type normalize_mode = Readable | Optimized | Optimized_legacy

(** Tezos client states. *)
type t

(** Get the name of a client (e.g. ["client1"]). *)
val name : t -> string

(** Get the base directory of a client.

    The base directory is the location where clients store their
    configuration files. It corresponds to the [--base-dir] option. *)
val base_dir : t -> string

(** Get [Account.key list] of all extra bootstraps.

    Additional bootstrap accounts are created when you use the
    [additional_bootstrap_account_count] or
    [additional_revealed_bootstrap_account_count] arguments of
    [init_with_protocol]. They do not include the default accounts that are
    always created. *)
val additional_bootstraps : t -> Account.key list

(** Call [Daemon.Make.wait_for] on a [Node] or [Proxy_server] endpoint.

    Fail if the endpoint is a [Foreign_endpoint]. *)
val endpoint_wait_for :
  ?where:string -> endpoint -> string -> (JSON.t -> 'a option) -> 'a Lwt.t

(** Create a client.

    The standard output and standard error output of the node will
    be logged with prefix [name] and color [color].

    Default [base_dir] is a temporary directory
    which is always the same for each [name].

    The endpoint argument is used to know which port the client should connect to.
    This endpoint can be overridden for each command, as a client is not actually tied
    to an endpoint. Most commands require an endpoint to be specified (either with [create]
    or with the command itself). *)
val create :
  ?runner:Runner.t ->
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?endpoint:endpoint ->
  ?media_type:media_type ->
  ?dal_node:Dal_node.t ->
  ?remote_signer:Uri.t ->
  unit ->
  t

(** Create a client like [create] but do not assume [Client] as the mode. *)
val create_with_mode :
  ?runner:Runner.t ->
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?dal_node:Dal_node.t ->
  ?remote_signer:Uri.t ->
  mode ->
  t

(** Get a client's mode. Used with [set_mode] to temporarily change
    a client's mode *)
val get_mode : t -> mode

(** Change the client's mode. This function is required for example because
    we wanna keep a client's wallet. This is impossible if we created
    a new client from scratch. *)
val set_mode : mode -> t -> unit

(** Get a client's Dal node. *)
val get_dal_node : t -> Dal_node.t option

(** [with_dal_node t dal_node] returns the client [t] with its
    Dal node updated to [dal_node]. *)
val with_dal_node : ?dal_node:Dal_node.t -> t -> t

(** Write the [--sources] file used by the light mode. *)
val write_sources_file :
  min_agreement:float -> uris:string list -> t -> unit Lwt.t

(** {2 RPC calls} *)

(** Paths for RPCs.

    For instance, [["chains"; "main"; "blocks"; "head"]]
    denotes [/chains/main/blocks/head]. *)
type path = string list

(** [string_of_path ["seg1"; "seg2"]] is ["/seg1/seg2"] *)
val string_of_path : path -> string

(** Query strings for RPCs.

    For instance, [["key1", "value1"; "key2", "value2"]]
    denotes [?key1=value1&key2=value2]. *)
type query_string = (string * string) list

(** HTTP methods for RPCs. *)
type meth = RPC_core.verb = GET | PUT | POST | PATCH | DELETE

(** Data type for RPCs. *)
type data = RPC_core.data

(** A lowercase string of the method. *)
val string_of_meth : meth -> string

(** [rpc_path_query_to_string ["key1", "value1"; "key2", "value2")] ["seg1"; "seg2"]]
    returns [/seg1/seg2?key1=value1&key2=value2] where seg1, seg2, key1, key2,
    value1, and value2 have been appropriately encoded *)
val rpc_path_query_to_string : ?query_string:query_string -> path -> string

(** Use the client to call an RPC.

    Run [rpc meth path?query_string with data].
    Fail the test if the RPC call failed.

    The [protocol_hash] argument allows to run the RPC command for a specific
    protocol hash.

    See the documentation of {!Process.spawn} for information about
    [log_*], [hooks] and [env] arguments.

    In particular, [env] can be used to pass [TEZOS_LOG], e.g.
    [("TEZOS_LOG", "proxy_rpc->debug")] to enable logging.

    The [data] argument allows to add data to the RPC call either with JSON
    value or with a filename containing a JSON value.
*)
val rpc :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?better_errors:bool ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?data:data ->
  ?query_string:query_string ->
  ?protocol_hash:string ->
  meth ->
  path ->
  t ->
  JSON.t Lwt.t

(** Same as [rpc], but do not wait for the process to exit. *)
val spawn_rpc :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?better_errors:bool ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?data:data ->
  ?query_string:query_string ->
  ?protocol_hash:string ->
  meth ->
  path ->
  t ->
  Process.t

module Spawn : sig
  (* FIXME: This module is temporary and is here to make the new
     interface cohabits with the old one. *)
  val rpc :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?data:data ->
    ?query_string:query_string ->
    ?protocol_hash:string ->
    meth ->
    path ->
    t ->
    JSON.t Runnable.process
end

(** Run [octez-client rpc list <url>]. *)
val rpc_list :
  ?endpoint:endpoint -> ?hooks:Process.hooks -> ?url:string -> t -> string Lwt.t

(** Same as [rpc_list], but do not wait for the process to exit. *)
val spawn_rpc_list :
  ?endpoint:endpoint -> ?hooks:Process.hooks -> ?url:string -> t -> Process.t

(** Run [octez-client rpc schema]. *)
val rpc_schema :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?better_errors:bool ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?protocol_hash:string ->
  meth ->
  path ->
  t ->
  JSON.t Lwt.t

(** Same as [rpc_schema], but do not wait for the process to exit. *)
val spawn_rpc_schema :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?better_errors:bool ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?protocol_hash:string ->
  meth ->
  path ->
  t ->
  Process.t

(** Run [octez-client rpc /chains/<chain>/blocks/<block>/header/shell]. *)
val shell_header :
  ?endpoint:endpoint -> ?chain:string -> ?block:string -> t -> string Lwt.t

(** Same as [shell_header], but do not wait for the process to exit. *)
val spawn_shell_header :
  ?endpoint:endpoint -> ?chain:string -> ?block:string -> t -> Process.t

(** Run [shell_header] and retrieves the level. *)
val level :
  ?endpoint:endpoint -> ?chain:string -> ?block:string -> t -> int Lwt.t

(** {2 Admin Client Commands} *)

module Admin : sig
  (** Run octez-admin-client commands. *)

  (** Ask a node to trust the address and port of another node. *)
  val trust_address : ?endpoint:endpoint -> peer:Node.t -> t -> unit Lwt.t

  (** Ask a node to untrust the address and port of another node. *)
  val untrust_address : ?endpoint:endpoint -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [trust_address], but do not wait for the process to exit. *)
  val spawn_trust_address : ?endpoint:endpoint -> peer:Node.t -> t -> Process.t

  (** Same as [untrust_address], but do not wait for the process to exit. *)
  val spawn_untrust_address :
    ?endpoint:endpoint -> peer:Node.t -> t -> Process.t

  (** Connect a node to another peer. *)
  val connect_address : ?endpoint:endpoint -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [connect_address], but do not wait for the process to exit. *)
  val spawn_connect_address :
    ?endpoint:endpoint -> peer:Node.t -> t -> Process.t

  (** Connect a P2P node to another peer. *)
  val connect_p2p_node_address :
    ?endpoint:endpoint -> peer:P2p_node.t -> t -> unit Lwt.t

  (** Same as [connect_p2p_node_address], but do not wait for the process to exit. *)
  val spawn_connect_p2p_node_address :
    ?endpoint:endpoint -> peer:P2p_node.t -> t -> Process.t

  (** Kick a peer.

      [peer] is the identity of the peer to kick.
      You can get it with [Node.wait_for_identity] for instance. *)
  val kick_peer : ?endpoint:endpoint -> peer:string -> t -> unit Lwt.t

  (** Same as [kick_peer], but do not wait for the process to exit. *)
  val spawn_kick_peer : ?endpoint:endpoint -> peer:string -> t -> Process.t

  (** Ban a peer.

      [peer] is the identity of the peer to ban.
      You can get it with [Node.wait_for_identity] for instance. *)
  val ban_peer : ?endpoint:endpoint -> peer:string -> t -> unit Lwt.t

  (** Same as [ban_peer], but do not wait for the process to exit. *)
  val spawn_ban_peer : ?endpoint:endpoint -> peer:string -> t -> Process.t

  (** Run [octez-admin-client p2p stat]. *)
  val p2p_stat : ?endpoint:endpoint -> t -> string Lwt.t

  (** Same as [p2p_stat], but do not wait for the process to exit. *)
  val spawn_p2p_stat : ?endpoint:endpoint -> t -> Process.t

  (** Run [octez-admin-client inject protocol <protocol_path>].

      Returns the hash of the injected protocol. *)
  val inject_protocol :
    ?endpoint:endpoint -> protocol_path:string -> t -> string Lwt.t

  (** Same as [inject_protocol], but do not wait for the process to exit. *)
  val spawn_inject_protocol :
    ?endpoint:endpoint -> protocol_path:string -> t -> Process.t

  (** Run [octez-admin-client list protocols] and return the list of protocol hashes. *)
  val list_protocols : ?endpoint:endpoint -> t -> string list Lwt.t

  (** Same as [list_protocols], but do not wait for the process to exit. *)
  val spawn_list_protocols : ?endpoint:endpoint -> t -> Process.t

  (** Run [octez-admin-client protocol environment] on a protocol hash.

      Return its environment version as a string such as ["V1"]. *)
  val protocol_environment : ?endpoint:endpoint -> t -> string -> string Lwt.t

  (** Same as [protocol_environment], but do not wait for the process to exit. *)
  val spawn_protocol_environment :
    ?endpoint:endpoint -> t -> string -> Process.t
end

(** {2 Regular Client Commands} *)

(** Run [octez-client --version]. *)
val version : t -> unit Lwt.t

(** Same as [version], but do not wait for the process to exit. *)
val spawn_version : t -> Process.t

(** Run [octez-client import secret keys from mnemonic]. *)
val import_keys_from_mnemonic :
  ?endpoint:endpoint ->
  ?force:bool ->
  ?passphrase:string ->
  ?encryption_password:string ->
  t ->
  alias:string ->
  mnemonic:string list ->
  unit Lwt.t

(** Same as [import_keys_from_mnemonic], but do not wait for the
    process to exit. This function opens and returns an [output_channel]
    and returns it along with the process.  *)
val spawn_import_keys_from_mnemonic :
  ?endpoint:endpoint ->
  ?force:bool ->
  ?encrypt:bool ->
  t ->
  alias:string ->
  Process.t * Lwt_io.output_channel

(** Run [octez-client import secret key] for an encrypted key. *)
val import_encrypted_secret_key :
  ?hooks:Process_hooks.t ->
  ?force:bool ->
  ?endpoint:endpoint ->
  t ->
  Account.secret_key ->
  alias:string ->
  password:string ->
  unit Lwt.t

(** Same as [import_encrypted_secret_key], but do not wait for the process to exit. *)
val spawn_import_encrypted_secret_key :
  ?hooks:Process_hooks.t ->
  ?force:bool ->
  ?endpoint:endpoint ->
  t ->
  Account.secret_key ->
  alias:string ->
  Process.t * Lwt_io.output_channel

(** Run [octez-client import public key]. *)
val import_public_key :
  ?force:bool ->
  ?endpoint:endpoint ->
  t ->
  public_key:string ->
  alias:string ->
  unit Lwt.t

(** Run [octez-client import secret key]. *)
val import_secret_key :
  ?force:bool ->
  ?endpoint:endpoint ->
  t ->
  Account.secret_key ->
  alias:string ->
  unit Lwt.t

(** Run [octez-client import secret key] for remote signer. *)
val import_signer_key :
  ?endpoint:endpoint ->
  ?force:bool ->
  ?signer:Uri.t ->
  public_key_hash:string ->
  alias:string ->
  t ->
  unit Lwt.t

(** Same as [import_secret_key] for signer, but do not wait for the
    process to exit. *)
val spawn_import_signer_key :
  ?endpoint:endpoint ->
  ?force:bool ->
  ?signer:Uri.t ->
  public_key_hash:string ->
  alias:string ->
  t ->
  Process.t

(** Same as [import_secret_key], but do not wait for the process to exit. *)
val spawn_import_secret_key :
  ?force:bool ->
  ?endpoint:endpoint ->
  t ->
  Account.secret_key ->
  alias:string ->
  Process.t

(** Run [octez-client forget all keys --force] *)
val forget_all_keys : ?endpoint:endpoint -> t -> unit Lwt.t

(** Run [octez-client activate protocol].

    If [timestamp] is not specified explicitly, it is set to [Ago
    timestamp_delay], where [timestamp_delay] is 365 days, which
    allows to bake plenty of blocks before their timestamp reach the
    present (at which point one would have to wait between each block
    so that peers do not reject them for being in the future).

    You must either provide [protocol] (in which case [parameter_file] defaults
    to [Protocol.parameter_file protocol]), or provide both [protocol_hash]
    and [parameter_file].

    If you want to wait until the node switches its head to the block
    activating the given protocol, consider using
    {!activate_protocol_and_wait} below.
*)
val activate_protocol :
  ?endpoint:endpoint ->
  ?block:string ->
  ?protocol:Protocol.t ->
  ?protocol_hash:string ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:timestamp ->
  ?parameter_file:string ->
  t ->
  unit Lwt.t

(** Same as {!activate_protocol}, but wait until level increases by 1.

    Waiting ensures that the activation block has been produced.
    This makes your test more deterministic.

    Uses the node provided via argument [node] if any. Otherwise, it
    searches for a node in the client's mode, and fails if no node is
    found. *)
val activate_protocol_and_wait :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?protocol_hash:string ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:timestamp ->
  ?parameter_file:string ->
  ?node:Node.t ->
  t ->
  unit Lwt.t

(** Same as [activate_protocol], but do not wait for the process to exit. *)
val spawn_activate_protocol :
  ?endpoint:endpoint ->
  ?block:string ->
  ?protocol:Protocol.t ->
  ?protocol_hash:string ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:timestamp ->
  ?parameter_file:string ->
  t ->
  Process.t

(** [empty_mempool_file ?filename ()] creates a file containing the
   encoding of an empty mempool. This file can be given to [bake_for]
   command with the [mempool] parameter to ensure that the block baked
   will contain no operations. *)
val empty_mempool_file : ?filename:string -> unit -> string

(** Run [octez-client bake for].

    Default [key] is {!Constant.bootstrap1.alias}.

    If you want to wait until the node switches its head to the baked block,
    consider using {!bake_for_and_wait} below. *)
val bake_for :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?keys:string list ->
  ?minimal_fees:int ->
  ?minimal_nanotez_per_gas_unit:int ->
  ?minimal_nanotez_per_byte:int ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?ignore_node_mempool:bool ->
  ?count:int ->
  ?force:bool ->
  ?context_path:string ->
  ?dal_node_endpoint:string ->
  ?ai_vote:ai_vote ->
  ?state_recorder:bool ->
  ?expect_failure:bool ->
  t ->
  unit Lwt.t

(** Same as {!bake_for}, but wait for your node to reach the expected level.

    Waiting ensures that the baked block has been well processed by
    the node. This makes your test more deterministic.

    Uses the node provided via argument [node] if any. Otherwise, it
    searches for a node in the client's mode, and fails if no node is
    found.

    @param level_before If provided, check that the node is at this
    level before baking. *)
val bake_for_and_wait :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?keys:string list ->
  ?minimal_fees:int ->
  ?minimal_nanotez_per_gas_unit:int ->
  ?minimal_nanotez_per_byte:int ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?ignore_node_mempool:bool ->
  ?count:int ->
  ?force:bool ->
  ?context_path:string ->
  ?level_before:int ->
  ?node:Node.t ->
  ?dal_node_endpoint:string ->
  ?ai_vote:ai_vote ->
  t ->
  unit Lwt.t

(** Same as {!bake_for_and_wait}, but return the new level. *)
val bake_for_and_wait_level :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?keys:string list ->
  ?minimal_fees:int ->
  ?minimal_nanotez_per_gas_unit:int ->
  ?minimal_nanotez_per_byte:int ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?ignore_node_mempool:bool ->
  ?count:int ->
  ?force:bool ->
  ?context_path:string ->
  ?level_before:int ->
  ?node:Node.t ->
  ?dal_node_endpoint:string ->
  ?ai_vote:ai_vote ->
  ?state_recorder:bool ->
  t ->
  int Lwt.t

(** Same as [bake_for], but do not wait for the process to exit. *)
val spawn_bake_for :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?keys:string list ->
  ?minimal_fees:int ->
  ?minimal_nanotez_per_gas_unit:int ->
  ?minimal_nanotez_per_byte:int ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?ignore_node_mempool:bool ->
  ?count:int ->
  ?force:bool ->
  ?context_path:string ->
  ?dal_node_endpoint:string ->
  ?ai_vote:ai_vote ->
  ?state_recorder:bool ->
  t ->
  Process.t

(** Bake until the node is at [target_level], using
    {!bake_for_and_wait}.

    Fail if the node is already at [target_level] or higher.

    @param keys See {!bake_for}.

    @param node See {!bake_for_and_wait}. *)
val bake_until_level :
  target_level:int -> ?keys:string list -> ?node:Node.t -> t -> unit Lwt.t

(** Bake until the node is at [target_cycle], using {!bake_for_and_wait}. This
  function calls an RPC to know the exact "blocks_per_cycle" value to compute
  the number of blocks to bake. As this occurs only once, the ending cycle might
  be different from the target cycle if the "blocks_per_cycle" value changes
  during the execution, for instance because of a protocol migration.

  Fail if the node is already at [target_cycle] or higher.

  @param keys See {!bake_for}.

  @param node See {!bake_for_and_wait}. *)
val bake_until_cycle :
  target_cycle:int -> ?keys:string list -> ?node:Node.t -> t -> unit Lwt.t

(** Similar to {!bake_until_cycle} but stops at the last block of the queried
  cycle. *)
val bake_until_cycle_end :
  target_cycle:int -> ?keys:string list -> ?node:Node.t -> t -> unit Lwt.t

(** Run [octez-client attest for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val attest_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** Same as [attest_for], but do not wait for the process to exit. *)
val spawn_attest_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(** Run [octez-client preattest for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val preattest_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** Same as [preattest_for], but do not wait for the process to exit. *)
val spawn_preattest_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(** Run [octez-client propose for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val spawn_propose_for :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2336
   refactor this *)

(** [propose_for] *)
val propose_for :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** [propose_for_and_wait] is the analogous of {!bake_for_and_wait} for
    {!propose_for}. *)
val propose_for_and_wait :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** [propose_for_and_wait] is the analogous of {!bake_for_and_wait} for
    {!propose_for}. *)
val repropose_for_and_wait :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  ?force_round:int ->
  ?force_reproposal:bool ->
  t ->
  unit Lwt.t

(** Run [octez-client show address <alias> --show-secret] and parse
    the output into an [Account.key].
    E.g. for [~alias:"bootstrap1"] the command yields:
{v
      Hash: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
      Public Key: edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav
      Secret Key: unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh
v}
    which becomes:
{[
     { alias = "bootstrap1";
       public_key_hash = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
       public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
       secret_key =
         Unencrypted "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh"; }
]} *)
val show_address : alias:string -> t -> Account.key Lwt.t

(** Same as [show_address], but do not wait for the process to exit
    (which also implies that there is no output key to parse). *)
val spawn_show_address : alias:string -> t -> Process.t

(** Run [octez-client list known addresses] and parse the output into
    a association list from aliases to public key hashes. *)
val list_known_addresses : t -> (string * string) list Lwt.t

(** Same as [list_known_addresses] but do not wait for the process to
    exit. *)
val spawn_list_known_addresses : t -> Process.t

(** Run [octez-client list known remote keys] and parse the output into a list
    of public key hashes. *)
val list_known_remote_keys : t -> Uri.t -> string list Lwt.t

(** Same as [list_known_remote_keys] but do not wait for the process to
    exit. *)
val spawn_list_known_remote_keys : t -> Uri.t -> Process.t

(** Run [octez-client --wait wait set consensus key for account to key] *)
val spawn_set_consensus_key :
  ?wait:string -> t -> account:string -> key:string -> Process.t

type key_encryption =
  | Encrypted of string
  | Forced_encrypted of string
  | Forced_unencrypted

(** Run [octez-client gen keys] and return the key alias.

    The default value for [alias] is a fresh alias of the form [tezt_<n>]. *)
val gen_keys :
  ?force:bool ->
  ?alias:string ->
  ?sig_alg:string ->
  ?key_encryption:key_encryption ->
  t ->
  string Lwt.t

(** A helper to run [octez-client gen keys] followed by
    [octez-client show address] to get the generated key. *)
val gen_and_show_keys :
  ?alias:string -> ?sig_alg:string -> t -> Account.key Lwt.t

(** Run [octez-client add address <alias> <src>]. *)
val add_address : ?force:bool -> t -> alias:string -> src:string -> unit Lwt.t

(** Same as [add_address] but do not wait for the process to
    exit. *)
val spawn_add_address :
  ?force:bool -> t -> alias:string -> src:string -> Process.t

(** Run [octez-client activate accoung <alias> with <activation_key>]. *)
val activate_account :
  ?wait:string -> t -> alias:string -> activation_key:string -> unit Lwt.t

(** Same as [activate_account] but do not wait for the process to exit. *)
val spawn_activate_account :
  ?wait:string -> t -> alias:string -> activation_key:string -> Process.t

(** Run [octez-client transfer amount from giver to receiver]. *)
val transfer :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  ?log_requests:bool ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?fee_cap:Tez.t ->
  ?gas_limit:int ->
  ?safety_guard:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?entrypoint:string ->
  ?arg:string ->
  ?simulation:bool ->
  ?force:bool ->
  ?expect_failure:bool ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  unit Lwt.t

(** Same as [transfer], but do not wait for the process to exit. *)
val spawn_transfer :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  ?log_requests:bool ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?fee_cap:Tez.t ->
  ?gas_limit:int ->
  ?safety_guard:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?entrypoint:string ->
  ?arg:string ->
  ?simulation:bool ->
  ?force:bool ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  Process.t

(** Run [octez-client call <destination> from <source>]. *)
val call :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?safety_guard:int ->
  ?entrypoint:string ->
  ?arg:string ->
  destination:string ->
  source:string ->
  t ->
  unit Lwt.t

(** Same as [call], but do not wait for the process to exit. *)
val spawn_call :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?safety_guard:int ->
  ?entrypoint:string ->
  ?arg:string ->
  destination:string ->
  source:string ->
  t ->
  Process.t

(** Run [octez-client multiple transfers from giver using json_batch]. *)
val multiple_transfers :
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee_cap:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?simulation:bool ->
  ?force:bool ->
  giver:string ->
  json_batch:string ->
  t ->
  unit Runnable.process

(** Run octez-client register key <delegate> as delegate. *)
val register_delegate :
  ?endpoint:endpoint -> ?wait:string -> delegate:string -> t -> string Lwt.t

(** Run octez-client get delegate for <src>. Returns [Some address] if delegate
    is set or [None] otherwise. *)
val get_delegate : ?endpoint:endpoint -> src:string -> t -> string option Lwt.t

(** Run [octez-client set delegate for <src> to <delegate>]. *)
val set_delegate :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?fee:Tez.t ->
  ?fee_cap:Tez.t ->
  ?force_low_fee:bool ->
  ?expect_failure:bool ->
  ?simulation:bool ->
  ?amount:Tez.t ->
  src:string ->
  delegate:string ->
  t ->
  unit Lwt.t

(** Same as [set_delegate], but do not wait for the process to exit. *)
val spawn_set_delegate :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?fee:Tez.t ->
  ?fee_cap:Tez.t ->
  ?force_low_fee:bool ->
  ?simulation:bool ->
  ?amount:Tez.t ->
  src:string ->
  delegate:string ->
  t ->
  Process.t

(** Run [octez-client call <destination> from <src>] *)
val call_contract :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?burn_cap:Tez.t ->
  src:string ->
  destination:string ->
  ?entrypoint:string ->
  ?arg:string ->
  t ->
  unit Lwt.t

(** Same as [call_contract], but do not wait for the process to exit. *)
val spawn_call_contract :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?burn_cap:Tez.t ->
  src:string ->
  destination:string ->
  ?entrypoint:string ->
  ?arg:string ->
  t ->
  Process.t

(** Run [octez-client reveal key for <src>]. *)
val reveal :
  ?endpoint:endpoint ->
  ?log_requests:bool ->
  ?wait:string ->
  ?fee:Tez.t ->
  ?fee_cap:Tez.t ->
  ?force_low_fee:bool ->
  src:string ->
  t ->
  unit Runnable.process

(** Run [octez-client withdraw delegate from <src>]. *)
val withdraw_delegate :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?expect_failure:bool ->
  src:string ->
  t ->
  unit Lwt.t

(** Same as [withdraw_delegate], but do not wait for the process to exit. *)
val spawn_withdraw_delegate :
  ?endpoint:endpoint -> ?wait:string -> src:string -> t -> Process.t

(** Run [octez-client get balance for]. *)
val get_balance_for : ?endpoint:endpoint -> account:string -> t -> Tez.t Lwt.t

(** Run [octez-client get full balance for]. *)
val get_full_balance_for :
  ?endpoint:endpoint -> account:string -> t -> Tez.t Lwt.t

(** Same as [get_balance_for], but do not wait for the process to exit. *)
val spawn_get_balance_for :
  ?endpoint:endpoint -> account:string -> t -> Process.t

(** Run [octez-client get ticket balance for contract with ticketer and type and content ]. *)
val ticket_balance :
  ?hooks:Process.hooks ->
  contract:string ->
  ticketer:string ->
  content_type:string ->
  content:string ->
  t ->
  string Lwt.t

(** Same as [ticket_balance], but do not wait for the process to exit. *)
val spawn_ticket_balance :
  ?hooks:Process.hooks ->
  contract:string ->
  ticketer:string ->
  content_type:string ->
  content:string ->
  t ->
  Process.t

(** Run [octez-client get all ticket balances for contract]. *)
val all_ticket_balances :
  ?hooks:Process.hooks -> contract:string -> t -> string Runnable.process

(** Run [octez-client create mockup]. *)
val create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?parameter_file:string ->
  ?bootstrap_accounts_file:string ->
  protocol:Protocol.t ->
  t ->
  unit Lwt.t

(** Same as [create_mockup], but do not wait for the process to exit. *)
val spawn_create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?parameter_file:string ->
  ?bootstrap_accounts_file:string ->
  protocol:Protocol.t ->
  t ->
  Process.t

(** Run [octez-client submit proposals for].

    If both [proto_hash] and [proto_hashes] are specified,
    the list of protocols which are proposed is [proto_hash :: proto_hashes].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val submit_proposals :
  ?key:string ->
  ?wait:string ->
  ?proto_hash:string ->
  ?proto_hashes:string list ->
  ?force:bool ->
  ?expect_failure:bool ->
  t ->
  unit Lwt.t

(** Same as [submit_proposals], but do not wait for the process to exit. *)
val spawn_submit_proposals :
  ?key:string ->
  ?wait:string ->
  ?proto_hash:string ->
  ?proto_hashes:string list ->
  ?force:bool ->
  t ->
  Process.t

type ballot = Nay | Pass | Yay

(** Run [octez-client submit ballot for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val submit_ballot :
  ?key:string -> ?wait:string -> proto_hash:string -> ballot -> t -> unit Lwt.t

(** Same as [submit_ballot], but do not wait for the process to exit. *)
val spawn_submit_ballot :
  ?key:string -> ?wait:string -> proto_hash:string -> ballot -> t -> Process.t

(** Run [octez-client set deposits limit for <src> to <limit>]. *)
val set_deposits_limit :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  src:string ->
  limit:string ->
  t ->
  string Lwt.t

(* Same as [set_deposits_limit], but do not wait for the process to exit. *)
val spawn_set_deposits_limit :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  src:string ->
  limit:string ->
  t ->
  Process.t

(** Run [octez-client unset deposits limit for <src>]; do not wait for
    the process to exit. *)
val spawn_unset_deposits_limit :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  src:string ->
  t ->
  Process.t

(** Run [octez-client increase the paid storage of <contract> by <amount> bytes from <payer>]. *)
val increase_paid_storage :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  contract:string ->
  amount:int ->
  payer:string ->
  t ->
  string Lwt.t

(** Run [octez-client get contract used storage space for <contract>]. *)
val used_storage_space :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  contract:string ->
  t ->
  string Lwt.t

(** Run [octez-client get contract paid storage space for <contract>]. *)
val paid_storage_space :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  contract:string ->
  t ->
  string Lwt.t

(** Run [octez-client set consensus key for <src> to <pk>] *)
val update_consensus_key :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?consensus_key_pop:string ->
  ?expect_failure:bool ->
  src:string ->
  pk:string ->
  t ->
  unit Lwt.t

(** [update_fresh_consensus_key delegate client] runs the following commands:
    [octez-client gen keys] to generate a new key,
    [octez-client show address] to retrieve the newly generated key <fresh>,
    and [octez-client update consensus key for <delegate> to <fresh>].
    Returns the newly generated key <fresh>. *)
val update_fresh_consensus_key :
  ?alias:string ->
  ?algo:string ->
  ?hooks:Process_hooks.t ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?expect_failure:bool ->
  Account.key ->
  t ->
  Account.key Lwt.t

(** Run [octez-client set companion key for <src> to <pk>] *)
val update_companion_key :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?companion_key_pop:string ->
  ?expect_failure:bool ->
  src:string ->
  pk:string ->
  t ->
  unit Lwt.t

(** [update_fresh_companion_key delegate client] runs the following commands:
    [octez-client gen keys] to generate a new key,
    [octez-client show address] to retrieve the newly generated key <fresh>,
    and [octez-client update companion key for <delegate> to <fresh>].
    Returns the newly generated key <fresh>. *)
val update_fresh_companion_key :
  ?alias:string ->
  ?algo:string ->
  ?hooks:Process_hooks.t ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?expect_failure:bool ->
  Account.key ->
  t ->
  Account.key Lwt.t

(** Run [octez-client drain delegate with consensus key <src>] *)
val drain_delegate :
  ?hooks:Process.hooks ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?expect_failure:bool ->
  delegate:string ->
  consensus_key:string ->
  ?destination:string ->
  t ->
  unit Lwt.t

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2336
   [amount] should be named [transferring] *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/2336
   [src] should be named [from] and probably have type [Account.t] *)

(** Run [octez-client originate contract alias transferring amount from src
    running prg]. Returns the originated contract hash.

    Avoid this function when using [originate_contract_at] is possible. *)
val originate_contract :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  ?gas_limit:int ->
  ?dry_run:bool ->
  ?force:bool ->
  alias:string ->
  amount:Tez.t ->
  src:string ->
  prg:string ->
  t ->
  string Lwt.t

(** Same as [originate_contract], but do not wait for the process to exit. *)
val spawn_originate_contract :
  ?env:string String_map.t ->
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  ?gas_limit:int ->
  ?dry_run:bool ->
  ?force:bool ->
  alias:string ->
  amount:Tez.t ->
  src:string ->
  prg:string ->
  t ->
  Process.t

(** Originates the contract with name [n] for a given protocol [p].

    This function passes [n] and [p] to [Contract.find]. See the documentation
    there for more info.

    Returns a pair [(alias, res)] where [alias] is a value which can be used to
    identify the originated contract, and [res] is the originated contract hash.
    By default, [alias] is the last component of the scripts name [n], but this
    can be overridden through the [alias] parameter.
*)
val originate_contract_at :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  ?gas_limit:int ->
  ?dry_run:bool ->
  ?force:bool ->
  ?prefix:string ->
  ?alias:string ->
  amount:Tez.t ->
  src:string ->
  t ->
  string list ->
  Protocol.t ->
  (string * string) Lwt.t

(** Same as [originate_contract_at], but do not wait for the process to exit. *)
val spawn_originate_contract_at :
  ?hooks:Process.hooks ->
  ?log_output:bool ->
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  ?gas_limit:int ->
  ?dry_run:bool ->
  ?force:bool ->
  ?prefix:string ->
  ?alias:string ->
  amount:Tez.t ->
  src:string ->
  t ->
  string list ->
  Protocol.t ->
  string * Process.t

(** Run [octez-client remember contract <alias> <address>]. *)
val remember_contract :
  ?force:bool -> alias:string -> address:string -> t -> unit Lwt.t

(** Same as [remember_contract], but do not wait for the process to exit. *)
val spawn_remember_contract :
  ?force:bool -> alias:string -> address:string -> t -> Process.t

(** Run [octez-client remember script <alias> <src>]. *)
val remember_script : alias:string -> src:string -> t -> unit Lwt.t

(** Same as [remember_script], but do not wait for the process to exit. *)
val spawn_remember_script : alias:string -> src:string -> t -> Process.t

(** The information that the user has to provide for every smart contract
    they want to call during the stress test. *)
type stresstest_contract_parameters = {
  probability : float;  (** The probability of calling this smart contract *)
  invocation_fee : Tez.t;
      (** Fee to use for invocations during the stress test *)
  invocation_gas_limit : int;
      (** Gas limit to use for invocations during the stress test *)
}

(** Run [octez-client stresstest transfer using <sources>].

    [sources] is a string containing all the [source_aliases],
    [source_pkhs], and [source_accounts] in JSON format as expected by
    the [stresstest] command. Each optional argument [source_aliases],
    [source_pkhs], and [source_accounts] defaults to an empty
    list. However, if all three are empty, then the [sources] given to
    the command are [Constant.bootstrap_keys] i.e. [bootstrap1], ...,
    [bootstrap5].

    The parameter [--seed <seed>] is always provided (because without
    it, the [stresstest] command would use a fixed seed). If the
    corresponding optional argument is not provided to the function,
    then a new random seed is generated.

    Optional parameters:
    - [env] is the environment passed when calling the stresstest command
    - [seed] is the seed used for the random number generator
    - [fee] is the custom fee to pay instead of the default one
    - [gas_limit] is the custom gas limit
    - [--transfers <transfers>]
    - [--tps <tps>]
    - [--fresh_probabilty <probability>], probability from 0.0 to 1.0 that
      new bootstrap accounts will be created during the stress test
    - [--smart-contract-parameters] is the map of parameters for
      calling smart contracts during the smart test

    [endpoint]: cf {!create} *)
val stresstest :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?source_aliases:string list ->
  ?source_pkhs:string list ->
  ?source_accounts:Account.key list ->
  ?seed:int ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?transfers:int ->
  ?tps:int ->
  ?fresh_probability:float ->
  ?smart_contract_parameters:(string * stresstest_contract_parameters) list ->
  t ->
  unit Lwt.t

(** Same as {!stresstest}, but use a file path instead of lists of sources
    and do not wait for the process to exit. *)
val spawn_stresstest_with_filename :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?seed:int ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?transfers:int ->
  ?tps:int ->
  ?fresh_probability:float ->
  ?smart_contract_parameters:(string * stresstest_contract_parameters) list ->
  ?strategy:int ->
  ?level_limit:int ->
  t ->
  string ->
  Process.t

(** Same as {!stresstest}, but do not wait for the process to exit. *)
val spawn_stresstest :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?source_aliases:string list ->
  ?source_pkhs:string list ->
  ?source_accounts:Account.key list ->
  ?seed:int ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?transfers:int ->
  ?tps:int ->
  ?fresh_probability:float ->
  ?smart_contract_parameters:(string * stresstest_contract_parameters) list ->
  t ->
  Process.t Lwt.t

(** Run [octez-client stresstest gen keys <nb_keys>].

    [nb_keys] contains the number of new keys to be generated.

    Optional parameters:
    - alias_prefix: allows to use a dedicated alias prefix for
      generated keys (default: bootstrap<key_index>),
    - sig_algo: uses custom signature algorithm

    [endpoint]: cf {!create}*)
val stresstest_gen_keys :
  ?endpoint:endpoint ->
  ?alias_prefix:string ->
  ?sig_algo:string ->
  int ->
  t ->
  Account.key list Lwt.t

(** Costs of every kind of transaction used in the stress test. *)
type stresstest_gas_estimation = {
  regular : int;  (** Cost of a regular transaction. *)
  smart_contracts : (string * int) list;  (** Cost of smart contract calls. *)
}

(** Call the [stresstest estimate gas] command. *)
val stresstest_estimate_gas :
  ?endpoint:endpoint -> t -> stresstest_gas_estimation Lwt.t

(** Originate all smart contracts for use in the stress test. *)
val stresstest_originate_smart_contracts :
  ?endpoint:endpoint -> Account.key -> t -> unit Lwt.t

(** Run [octez-client stresstest fund accounts from <source_key_pkh>].

    [source_key_pkh] is the address from which the funds will be withdraw.

    Optional parameters:
    - batch_size: maximum number of operations that can be put into a
      single batch,
    - batches_per_block: maximum number of batches that can be put
      into a single block,
    - initial_amount: number of token, in μtz, that will be funded on
      each of the accounts to fund.

     [endpoint]: cf {!create} *)
val stresstest_fund_accounts_from_source :
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  source_key_pkh:string ->
  ?burn_cap:int ->
  ?fee_cap:int ->
  ?default_gas_limit:int ->
  ?batch_size:int ->
  ?batches_per_block:int ->
  ?initial_amount:Tez.t ->
  t ->
  unit Lwt.t

(** The result of a [octez-client run script .. on storage .. and input ..] call. *)
type run_script_result = {storage : string; big_map_diff : string list}

(** Run [octez-client run script .. on storage .. and input ..].

    Returns the new storage as a string.

    Fails if the new storage cannot be extracted from the output.

    Avoid this function when using [run_script_at] is possible. *)
val run_script :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  ?balance:Tez.t ->
  ?self_address:string ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?trace_stack:bool ->
  ?level:int ->
  ?now:string ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  prg:string ->
  storage:string ->
  input:string ->
  t ->
  run_script_result Lwt.t

(** Same as [run_script] but do not wait for the process to exit. *)
val spawn_run_script :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  ?balance:Tez.t ->
  ?self_address:string ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?trace_stack:bool ->
  ?level:int ->
  ?now:string ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  prg:string ->
  storage:string ->
  input:string ->
  t ->
  Process.t

(** Run [octez-client run script .. on storage .. and input ..] on the script
    name [n] for a given protocol [p].

    This function passes [n] and [p] to [Contract.find]. See the documentation
    there for more info.

    Returns the new storage as a string.

    Fails if the new storage cannot be extracted from the output. *)
val run_script_at :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?balance:Tez.t ->
  ?self_address:string ->
  ?source:string ->
  ?payer:string ->
  ?prefix:string ->
  ?now:string ->
  ?trace_stack:bool ->
  ?level:int ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  storage:string ->
  input:string ->
  t ->
  string list ->
  Protocol.t ->
  run_script_result Lwt.t

(** Same as [run_script_at] but do not wait for the process to exit. *)
val spawn_run_script_at :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?balance:Tez.t ->
  ?self_address:string ->
  ?source:string ->
  ?payer:string ->
  ?prefix:string ->
  ?now:string ->
  ?trace_stack:bool ->
  ?level:int ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  storage:string ->
  input:string ->
  t ->
  string list ->
  Protocol.t ->
  Process.t

(** Run [octez-client run michelson code .. on stack ..]. *)
val run_code :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  ?amount:Tez.t ->
  ?balance:Tez.t ->
  ?source:string ->
  ?payer:string ->
  ?self_address:string ->
  ?gas:int ->
  ?mode:normalize_mode ->
  ?level:int ->
  ?now:string ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  src:string ->
  stack:string ->
  t ->
  string Lwt.t

(** Same as [run_code] but do not wait for the process to exit. *)
val spawn_run_code :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  ?amount:Tez.t ->
  ?balance:Tez.t ->
  ?source:string ->
  ?payer:string ->
  ?self_address:string ->
  ?gas:int ->
  ?mode:normalize_mode ->
  ?level:int ->
  ?now:string ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  src:string ->
  stack:string ->
  t ->
  Process.t

(** Run [octez-client register global constant value from src].
    Returns the address hash of the new constant. *)
val register_global_constant :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  src:string ->
  value:string ->
  t ->
  string Lwt.t

(** Same as [register_global_constant] but do not wait for the process to exit. *)
val spawn_register_global_constant :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  value:string ->
  src:string ->
  t ->
  Process.t

(** Represents the result of a [octez-client hash data .. of type ...] call.

    Given the output:
{v
    $ ./octez-client hash data Unit of type unit
    Raw packed data: 0x05030b
    Script-expression-ID-Hash: expruaDPoTWXcTR6fiQPy4KZSW72U6Swc1rVmMiP...
    Raw Script-expression-ID-Hash: 0x8b456a4530fb6d0fea9a0dcd0e9d6ff6b3...
    Ledger Blake2b hash: ANf4YSkDc71Uy14uWg3wL8u49LUAHdVVnVHbSwNKyEJo
    Raw Sha256 hash: 0x5f6d1c5a35306dc8be3a54058774f9cb8f1bc71a6a743a25...
    Raw Sha512 hash: 0xe89c39b714a041046cf421532526b466a8ad29a89a4f0279...
    Gas remaining: 1039999.624 units remaining
v}

    we obtain a [hash_data_result] record with each field filled from
    the corresponding line above (omitting the last [Gas remaining
    line]). *)
type hash_data_result = {
  packed : string;
  script_expr_hash : string;
  raw_script_expr_hash : string;
  ledger_blake2b_hash : string;
  raw_sha256_hash : string;
  raw_sha512_hash : string;
}

(** Run [octez-client hash data .. of type ...] *)
val hash_data :
  ?hooks:Process.hooks ->
  data:string ->
  typ:string ->
  t ->
  hash_data_result Lwt.t

(** Same as [hash_data], but do not wait for the process to exit. *)
val spawn_hash_data :
  ?hooks:Process.hooks -> data:string -> typ:string -> t -> Process.t

(** Arguments to the [hash_script]'s and [hash_data]'s [?for_script] *)
type hash_script_format = TSV | CSV

(** Run [octez-client hash script ..]*)
val hash_script :
  ?hooks:Process_hooks.t ->
  ?for_script:hash_script_format ->
  script:string ->
  t ->
  string Lwt.t

(** Same as [hash_script], but do not wait for the process to exit. *)
val spawn_hash_script :
  ?hooks:Process_hooks.t ->
  ?for_script:hash_script_format ->
  script:string ->
  t ->
  Process.t

(** Run [octez-client get contract script hash for ..]*)
val get_contract_hash :
  ?hooks:Process_hooks.t -> contract:string -> t -> string Lwt.t

(** Same as [get_contract_hash], but do not wait for the process to exit. *)
val spawn_get_contract_hash :
  ?hooks:Process_hooks.t -> contract:string -> t -> Process.t

(** Run [octez-client hash script ..] with a list of scripts to hash *)
val hash_scripts :
  ?hooks:Process_hooks.t ->
  ?display_names:bool ->
  ?for_script:hash_script_format ->
  string list ->
  t ->
  string list Lwt.t

(** Same as [hash_scripts], but do not wait for the process to exit. *)
val spawn_hash_scripts :
  ?hooks:Process_hooks.t ->
  ?display_names:bool ->
  ?for_script:hash_script_format ->
  string list ->
  t ->
  Process.t

(** Run [octez-client normalize data .. of type ...]*)
val normalize_data :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  ?legacy:bool ->
  data:string ->
  typ:string ->
  t ->
  string Lwt.t

(** Same as [normalize_data], but do not wait for the process to exit. *)
val spawn_normalize_data :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  ?legacy:bool ->
  data:string ->
  typ:string ->
  t ->
  Process.t

(** Run [octez-client normalize stack ...]*)
val normalize_stack :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  ?legacy:bool ->
  stack:string ->
  t ->
  string Lwt.t

(** Same as [normalize_stack], but do not wait for the process to exit. *)
val spawn_normalize_stack :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  ?legacy:bool ->
  stack:string ->
  t ->
  Process.t

(** Run [octez-client normalize script ..]*)
val normalize_script :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  script:string ->
  t ->
  string Lwt.t

(** Same as [normalize_script], but do not wait for the process to exit. *)
val spawn_normalize_script :
  ?hooks:Process_hooks.t ->
  ?mode:normalize_mode ->
  script:string ->
  t ->
  Process.t

(** Run [octez-client normalize type ..]*)
val normalize_type : ?hooks:Process_hooks.t -> typ:string -> t -> string Lwt.t

(** Same as [normalize_type], but do not wait for the process to exit. *)
val spawn_normalize_type :
  ?hooks:Process_hooks.t -> typ:string -> t -> Process.t

(** Run [octez-client typecheck data ..]*)
val typecheck_data :
  data:string -> typ:string -> ?gas:int -> ?legacy:bool -> t -> unit Lwt.t

(** Same as [typecheck_data], but do not wait for the process to exit. *)
val spawn_typecheck_data :
  data:string -> typ:string -> ?gas:int -> ?legacy:bool -> t -> Process.t

(** Run [octez-client typecheck script ..]*)
val typecheck_script :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  scripts:string list ->
  ?no_base_dir_warnings:bool ->
  ?details:bool ->
  ?emacs:bool ->
  ?no_print_source:bool ->
  ?gas:int ->
  ?legacy:bool ->
  ?display_names:bool ->
  t ->
  unit Lwt.t

(** Same as [typecheck_script], but do not wait for the process to exit. *)
val spawn_typecheck_script :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  scripts:string list ->
  ?no_base_dir_warnings:bool ->
  ?details:bool ->
  ?emacs:bool ->
  ?no_print_source:bool ->
  ?gas:int ->
  ?legacy:bool ->
  ?display_names:bool ->
  t ->
  Process.t

(** Run [octez-client run unit tests from ..]*)
val run_tzt_unit_tests :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  tests:string list ->
  ?no_base_dir_warnings:bool ->
  t ->
  unit Lwt.t

(** Same as [run_tzt_unit_tests], but do not wait for the process to exit. *)
val spawn_run_tzt_unit_tests :
  ?hooks:Process.hooks ->
  ?protocol_hash:string ->
  tests:string list ->
  ?no_base_dir_warnings:bool ->
  t ->
  Process.t

(** Same as [run_tzip4_view] but does not wait for the process to exit. *)
val spawn_run_tzip4_view :
  ?hooks:Process.hooks ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?unparsing_mode:normalize_mode ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  entrypoint:string ->
  contract:string ->
  ?input:string ->
  ?unlimited_gas:bool ->
  t ->
  Process.t

(** Run [octez-client run tzip4 view .. on contract .. with input .. ]

    Returns the value returned by a view as a string.

    Fails if the view or the contract does not exist. If [input] is [None],
    it runs [octez-client run tzip4 view .. on contract ..]. *)
val run_tzip4_view :
  ?hooks:Process.hooks ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?unparsing_mode:normalize_mode ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  entrypoint:string ->
  contract:string ->
  ?input:string ->
  ?unlimited_gas:bool ->
  t ->
  string Lwt.t

(** Same as [run_view] but does not wait for the process to exit. *)
val spawn_run_view :
  ?hooks:Process.hooks ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?unparsing_mode:normalize_mode ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  view:string ->
  contract:string ->
  ?input:string ->
  ?unlimited_gas:bool ->
  t ->
  Process.t

(** Run [octez-client run view .. on contract .. with input ..].

    Returns the value returned by a view as a string.

    Fails if the view or the contract does not exist. If [input] is [None], it
    runs [octez-client run view .. on contract ..]. *)
val run_view :
  ?hooks:Process.hooks ->
  ?source:string ->
  ?payer:string ->
  ?gas:int ->
  ?unparsing_mode:normalize_mode ->
  ?other_contracts:string ->
  ?extra_big_maps:string ->
  view:string ->
  contract:string ->
  ?input:string ->
  ?unlimited_gas:bool ->
  t ->
  string Lwt.t

(** Run [octez-client list mode protocols].

    Note: the [list protocols] command (without mode) is an admin command
    (see {!Admin.list_protocols}). *)
val list_protocols : [< `Light | `Mockup | `Proxy] -> t -> string list Lwt.t

(** Same as [list_protocols], but do not wait for the process to exit
    and do not process stdout. *)
val spawn_list_protocols : [< `Light | `Mockup | `Proxy] -> t -> Process.t

(** Run [octez-client list understood protocols]. *)
val list_understood_protocols : ?config_file:string -> t -> string list Lwt.t

(** Same as [list_understood_protocols], but do not wait for the process to exit
    and do not process stdout. *)
val spawn_list_understood_protocols : ?config_file:string -> t -> Process.t

(** Run [octez-client migrate mockup to]. *)
val migrate_mockup : next_protocol:Protocol.t -> t -> unit Lwt.t

(** Same as [migrate_mockup], but do not wait for the process to exit. *)
val spawn_migrate_mockup : next_protocol:Protocol.t -> t -> Process.t

(** Run [octez-client sign block <hexdata> for <delegate>]. *)
val sign_block : t -> string -> delegate:string -> string Lwt.t

(** Same as [sign_block], but do not wait for the process to exit. *)
val spawn_sign_block : t -> string -> delegate:string -> Process.t

(** Run [octez-client sign bytes <bytes> for <signer>]. *)
val sign_bytes : signer:string -> data:string -> t -> string Lwt.t

(** Same as [sign_bytes], but do not wait for the process to exit. *)
val spawn_sign_bytes : signer:string -> data:string -> t -> Process.t

(** Run [octez-client sign message <message> for <src>]. *)
val sign_message : ?branch:string -> t -> string -> src:string -> string Lwt.t

(** Same as [sign_message], but do not wait for the process to exit. *)
val spawn_sign_message :
  ?branch:string -> t -> string -> src:string -> Process.t

(** Run [octez-client check that message <message> was signed by <src> to produce <signature>]. *)
val check_message :
  ?branch:string -> t -> src:string -> signature:string -> string -> unit Lwt.t

(** Same as [check_message], but do not wait for the process to exit. *)
val spawn_check_message :
  ?branch:string -> t -> src:string -> signature:string -> string -> Process.t

val transfer_tickets :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?hooks:Process.hooks ->
  ?expect_failure:bool ->
  qty:int64 ->
  src:string ->
  destination:string ->
  entrypoint:string ->
  contents:string ->
  ty:string ->
  ticketer:string ->
  t ->
  unit Runnable.process

(** Run [octez-client show voting period] and return the period name. *)
val show_voting_period : ?endpoint:endpoint -> t -> string Lwt.t

(** Same as [show_voting_period], but do not wait for the process to exit. *)
val spawn_show_voting_period : ?endpoint:endpoint -> t -> Process.t

module Sc_rollup : sig
  (** Run [octez-client remember smart rollup <alias> <address>]. *)
  val remember_smart_rollup :
    ?hooks:Process.hooks ->
    ?force:bool ->
    t ->
    alias:string ->
    address:string ->
    unit Runnable.process

  (** Run [octez-client list known smart rollups]. *)
  val list_known_smart_rollups :
    ?hooks:Process.hooks -> t -> (string * string) list Runnable.process

  (** Run [octez-client forget all smart rollups]. *)
  val forget_all_smart_rollups :
    ?hooks:Process.hooks -> ?force:bool -> t -> unit Runnable.process

  (** Run [octez-client show known smart rollup <alias>]. *)
  val show_known_smart_rollup :
    ?hooks:Process.hooks -> t -> alias:string -> string Runnable.process

  (** Run [octez-client originate sc rollup <alias> from <src> of kind <kind> booting with <boot_sector>]. *)
  val originate :
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?force:bool ->
    ?burn_cap:Tez.t ->
    ?whitelist:string list ->
    alias:string ->
    src:string ->
    kind:string ->
    parameters_ty:string ->
    boot_sector:string ->
    t ->
    string Lwt.t

  (** Same as [originate], but do not wait for the process to exit. *)
  val spawn_originate :
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?force:bool ->
    ?burn_cap:Tez.t ->
    ?whitelist:string list ->
    alias:string ->
    src:string ->
    kind:string ->
    parameters_ty:string ->
    boot_sector:string ->
    t ->
    Process.t

  (** Run [octez-client send rollup message <msg> from <src>]. *)
  val send_message :
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?burn_cap:Tez.t ->
    ?fee:Tez.t ->
    ?fee_cap:Tez.t ->
    msg:string ->
    src:string ->
    t ->
    unit Lwt.t

  (** Same as [send_message], but do not wait for the process to exit. *)
  val spawn_send_message :
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?burn_cap:Tez.t ->
    ?fee:Tez.t ->
    ?fee_cap:Tez.t ->
    msg:string ->
    src:string ->
    t ->
    Process.t

  (** Run [octez-client publish commitment from <src> for sc rollup <sc_rollup>
      with compressed state <compressed_state> at inbox level <inbox_level>
      and predecessor <predecessor> and number of ticks <number_of_ticks>]. *)
  val publish_commitment :
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?burn_cap:Tez.t ->
    src:string ->
    sc_rollup:string ->
    compressed_state:string ->
    inbox_level:int ->
    predecessor:string ->
    number_of_ticks:int ->
    t ->
    unit Runnable.process

  (** Run [octez-client cement commitment <hash> from <src> for sc rollup <rollup>]. *)
  val cement_commitment :
    Protocol.t ->
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?burn_cap:Tez.t ->
    hash:string ->
    src:string ->
    dst:string ->
    t ->
    unit Runnable.process

  (** Run [octez-client timeout dispute on sc rollup <dst> with
     <staker1> against <staker2> from <src>]. *)
  val timeout :
    ?expect_failure:bool ->
    ?hooks:Process.hooks ->
    ?wait:string ->
    ?burn_cap:Tez.t ->
    staker1:string ->
    staker2:string ->
    src:string ->
    dst:string ->
    t ->
    unit Runnable.process

  (** Run [octez-client submit sc rollup recover bond of <staker> for <sc_rollup> from <src>]. *)
  val submit_recover_bond :
    ?wait:string ->
    ?burn_cap:Tez.t ->
    ?storage_limit:int ->
    ?fee:Tez.t ->
    ?hooks:Process.hooks ->
    rollup:string ->
    src:string ->
    staker:string ->
    t ->
    unit Runnable.process

  (** Run [octez-client execute outbox message of sc rollup <rollup> from <src>
      for commitment hash <hash> and output proof <proof>]. *)
  val execute_outbox_message :
    ?wait:string ->
    ?burn_cap:Tez.t ->
    ?storage_limit:int ->
    ?fee:Tez.t ->
    ?hooks:Process.hooks ->
    rollup:string ->
    src:string ->
    commitment_hash:string ->
    proof:string ->
    t ->
    unit Runnable.process
end

(** {2 Commands for managing FA1.2-compatible smart contracts} *)

(** Run [octez-client check contract <contract> implements fa1.2]. *)
val check_contract_implements_fa1_2 : contract:string -> t -> unit Lwt.t

(** Same as [check_contract_implements_fa1_2], but do not wait for the process to exit. *)
val spawn_check_contract_implements_fa1_2 : contract:string -> t -> Process.t

(** Run [octez-client from fa1.2 contract <contract> get balance for <from>]. *)
val from_fa1_2_contract_get_balance :
  contract:string -> from:string -> t -> int Lwt.t

(** Same as [from_fa1_2_contract_get_balance], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_balance :
  contract:string -> from:string -> t -> Process.t

(** Run [octez-client from fa1.2 contract <contract> get allowance on <owner> as <operator>]. *)
val from_fa1_2_contract_get_allowance :
  contract:string -> owner:string -> operator:string -> t -> int Lwt.t

(** Same as [from_fa1_2_contract_get_allowance], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_allowance :
  contract:string -> owner:string -> operator:string -> t -> Process.t

(** Run [octez-client from fa1.2 contract <contract> get total supply]. *)
val from_fa1_2_contract_get_total_supply : contract:string -> t -> int Lwt.t

(** Same as [from_fa1_2_contract_get_total_supply], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_total_supply :
  contract:string -> t -> Process.t

(** Run [octez-client from fa1.2 contract <contract> get balance for <from> callback on <callback>]. *)
val from_fa1_2_contract_get_balance_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  callback:string ->
  t ->
  unit Lwt.t

(** Same as [from_fa1_2_contract_get_balance_callback], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_balance_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  callback:string ->
  t ->
  Process.t

(** Run [octez-client from fa1.2 contract <contract> get allowance on <from> as <to> callback on <callback>]. *)
val from_fa1_2_contract_get_allowance_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  to_:string ->
  callback:string ->
  t ->
  unit Lwt.t

(** Same as [from_fa1_2_contract_get_allowance_callback], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_allowance_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  to_:string ->
  callback:string ->
  t ->
  Process.t

(** Run [octez-client from fa1.2 contract <contract> get total supply as <from> callback on <callback>]. *)
val from_fa1_2_contract_get_total_supply_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  callback:string ->
  t ->
  unit Lwt.t

(** Same as [from_fa1_2_contract_get_total_supply_callback], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_get_total_supply_callback :
  ?burn_cap:Tez.t ->
  contract:string ->
  from:string ->
  callback:string ->
  t ->
  Process.t

(** Run [octez-client from fa1.2 contract <contract> transfer <amount> from <from> to <to>]. *)
val from_fa1_2_contract_transfer :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  contract:string ->
  amount:int ->
  from:string ->
  to_:string ->
  ?as_:string ->
  t ->
  unit Lwt.t

(** Same as [from_fa1_2_contract_transfer], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_transfer :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  contract:string ->
  amount:int ->
  from:string ->
  to_:string ->
  ?as_:string ->
  t ->
  Process.t

(** Run [octez-client from fa1.2 contract <contract> as <as> approve <amount> from <from>]. *)
val from_fa1_2_contract_approve :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  contract:string ->
  as_:string ->
  amount:int ->
  from:string ->
  t ->
  unit Lwt.t

(** Same as [from_fa1_2_contract_approve], but do not wait for the process to exit. *)
val spawn_from_fa1_2_contract_approve :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  contract:string ->
  as_:string ->
  amount:int ->
  from:string ->
  t ->
  Process.t

(** Run [octez-client multiple fa1.2 transfers from <src> using <transfers.json>]. *)
val multiple_fa1_2_transfers :
  ?burn_cap:Tez.t ->
  src:string ->
  transfers_json:string ->
  ?as_:string ->
  t ->
  unit Lwt.t

(** Same as [multiple_fa1_2_transfers], but do not wait for the process to exit. *)
val spawn_multiple_fa1_2_transfers :
  ?burn_cap:Tez.t ->
  src:string ->
  transfers_json:string ->
  ?as_:string ->
  t ->
  Process.t

module Zk_rollup : sig
  (** Run
      [octez-client originate epoxy <alias> from <src>
        public_parameters file:<public_parameters_file>
        init_state file:<init_state_file>
        circuits_info file:<circuits_info_file>
        nb_ops <nb_ops>
        -G <gas_cap> --burn_cap <burn_cap> --S <storage_limit>]
      and return the ZKRU address. *)
  val originate :
    ?expect_failure:bool ->
    t ->
    src:string ->
    alias:string ->
    public_parameters_file:string ->
    init_state_file:string ->
    circuits_info_file:string ->
    nb_ops:int ->
    gas_cap:int ->
    burn_cap:int ->
    storage_limit:int ->
    string option Lwt.t

  (** Run
      [octez-client epoxy publish from <src>
        rollup <zk_rollup>
        ops file:<ops_file>
        -G <gas_cap> --burn_cap <burn_cap>]. *)
  val publish :
    ?expect_failure:bool ->
    t ->
    src:string ->
    zk_rollup:string ->
    ops_file:string ->
    gas_cap:int ->
    burn_cap:int ->
    unit Lwt.t

  (** Run
      [octez-client epoxy update from <src>
        rollup <zk_rollup>
        update file:<update_files>
        -G <gas_cap> --burn_cap <burn_cap>]. *)
  val update :
    ?expect_failure:bool ->
    t ->
    src:string ->
    zk_rollup:string ->
    update_file:string ->
    gas_cap:int ->
    burn_cap:int ->
    unit Lwt.t
end

(** {2 Commands for working with Sapling transactions} *)

(** Run [octez-client sapling gen key <name>].

    Returns mnemonic of the generated key. *)
val sapling_gen_key :
  name:string -> ?force:bool -> ?unencrypted:bool -> t -> string list Lwt.t

(** Same as [sapling_gen_key], but do not wait for the process to exit. *)
val spawn_sapling_gen_key :
  name:string -> ?force:bool -> ?unencrypted:bool -> t -> Process.t

(** Run [octez-client sapling use key <sapling-key> for contract <contract>]. *)
val sapling_use_key :
  sapling_key:string -> contract:string -> ?memo_size:int -> t -> unit Lwt.t

(** Same as [sapling_use_key], but do not wait for the process to exit. *)
val spawn_sapling_use_key :
  sapling_key:string -> contract:string -> ?memo_size:int -> t -> Process.t

(** Run [octez-client sapling import key <new>]. *)
val sapling_import_key :
  new_:string ->
  ?force:bool ->
  ?unencrypted:bool ->
  ?mnemonic:string list ->
  t ->
  unit Lwt.t

(** Same as [sapling_import_key], but do not wait for the process to exit. *)
val spawn_sapling_import_key :
  new_:string ->
  ?force:bool ->
  ?unencrypted:bool ->
  ?mnemonic:string list ->
  t ->
  Process.t

(** Run [octez-client sapling derive key <new> from <name> at index <child-index>]. *)
val sapling_derive_key :
  new_:string ->
  name:string ->
  child_index:int ->
  ?force:bool ->
  ?for_contract:string ->
  ?unencrypted:bool ->
  ?memo_size:int ->
  t ->
  string Lwt.t

(** Same as [sapling_derive_key], but do not wait for the process to exit. *)
val spawn_sapling_derive_key :
  new_:string ->
  name:string ->
  child_index:int ->
  ?force:bool ->
  ?for_contract:string ->
  ?unencrypted:bool ->
  ?memo_size:int ->
  t ->
  Process.t

(** Run [octez-client sapling gen address <name>].

    Returns the generated address and its index. *)
val sapling_gen_address :
  name:string -> ?address_index:int -> t -> (string * int) Lwt.t

(** Same as [sapling_gen_address], but do not wait for the process to exit. *)
val spawn_sapling_gen_address :
  name:string -> ?address_index:int -> t -> Process.t

(** Run [octez-client sapling export key <name> in <file>]. *)
val sapling_export_key : name:string -> file:string -> t -> unit Lwt.t

(** Same as [sapling_export_key], but do not wait for the process to exit. *)
val spawn_sapling_export_key : name:string -> file:string -> t -> Process.t

(** Run [octez-client sapling get balance for <sapling-key> in contract <contract>]. *)
val sapling_get_balance :
  sapling_key:string -> contract:string -> ?verbose:bool -> t -> Tez.t Lwt.t

(** Same as [sapling_get_balance], but do not wait for the process to exit. *)
val spawn_sapling_get_balance :
  sapling_key:string -> contract:string -> ?verbose:bool -> t -> Process.t

(** Run [octez-client sapling list keys]. *)
val sapling_list_keys : t -> string list Lwt.t

(** Same as [sapling_list_keys], but do not wait for the process to exit. *)
val spawn_sapling_list_keys : t -> Process.t

(** Run [octez-client sapling shield <qty> from <src-tz> to <dst-sap> using <sapling contract>].

    Returns [(balance_diff, fees)] where [balance_diff] is [sapling_contract]'s diff in balance and
    [fees] is the amount of fees paid. *)
val sapling_shield :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_tz:string ->
  dst_sap:string ->
  sapling_contract:string ->
  ?message:string ->
  t ->
  (Tez.t * Tez.t) Lwt.t

(** Same as [sapling_shield], but do not wait for the process to exit. *)
val spawn_sapling_shield :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_tz:string ->
  dst_sap:string ->
  sapling_contract:string ->
  ?message:string ->
  t ->
  Process.t

(** Run [octez-client sapling unshield <qty> from <src-sap> to <dst-tz> using <sapling_contract>].

    Returns [(balance_diff, fees)] where [balance_diff] is [sapling_contract]'s diff in balance and
    [fees] is the amount of fees paid.
 *)
val sapling_unshield :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_sap:string ->
  dst_tz:string ->
  sapling_contract:string ->
  t ->
  (Tez.t * Tez.t) Lwt.t

(** Same as [sapling_unshield], but do not wait for the process to exit. *)
val spawn_sapling_unshield :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_sap:string ->
  dst_tz:string ->
  sapling_contract:string ->
  t ->
  Process.t

(** Run [octez-client sapling forge transaction <qty> from <src-sap> to <dst-sap> using <sapling contract>]. *)
val sapling_forge_transaction :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_sap:string ->
  dst_sap:string ->
  sapling_contract:string ->
  ?file:string ->
  ?json:bool ->
  t ->
  unit Lwt.t

(** Same as [sapling_forge_transaction], but do not wait for the process to exit. *)
val spawn_sapling_forge_transaction :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  qty:Tez.t ->
  src_sap:string ->
  dst_sap:string ->
  sapling_contract:string ->
  ?file:string ->
  ?json:bool ->
  t ->
  Process.t

(** Run [octez-client sapling submit <file> from <alias-tz> using <sapling contract>]. *)
val sapling_submit :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  file:string ->
  alias_tz:string ->
  sapling_contract:string ->
  ?json:bool ->
  t ->
  unit Lwt.t

(** Same as [sapling_submit], but do not wait for the process to exit. *)
val spawn_sapling_submit :
  ?wait:string ->
  ?burn_cap:Tez.t ->
  file:string ->
  alias_tz:string ->
  sapling_contract:string ->
  ?json:bool ->
  t ->
  Process.t

(** {2 High-Level Functions} *)

(** Create a client with mode [Client] and import all secret keys
    listed in {!Constant.all_secret_keys}. *)
val init :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?endpoint:endpoint ->
  ?keys:Account.key list ->
  ?media_type:media_type ->
  ?remote_signer:Uri.t ->
  unit ->
  t Lwt.t

(** Set up a client and node(s).

    - Create a client with mode [Client], [Light], or [Proxy].
    - Import all secret [?keys] (by default, {!Constant.all_secret_keys}).

    In addition to the client, returns the first created node
    (if [`Light] is passed, a second node has been created, but it is
    not exposed). *)
val init_with_node :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?node_name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?media_type:media_type ->
  ?nodes_args:Node.argument list ->
  ?keys:Account.key list ->
  ?rpc_external:bool ->
  ?dal_node:Dal_node.t ->
  ?remote_signer:Uri.t ->
  [`Client | `Light | `Proxy] ->
  unit ->
  (Node.t * t) Lwt.t

(** Set up a client and node(s) and activate a protocol.

    - Create a client with mode [Client], [Light], or [Proxy]
    - Import all secret keys listed in {!Constant.all_secret_keys}
    - Create [additional_bootstrap_account_count] unrevealed accounts and
      [additional_revealed_bootstrap_account_count] revealed accounts. These
      accounts are created with [default_accounts_balance].
    - Activate the given protocol with [additional_account_count]
      additional bootstrap accounts whose aliases are given by
     [Account.bootstrap].

    In addition to the client, returns the first created node
    (if [`Light] is passed, a second node has been created, but it is
    not exposed). *)
val init_with_protocol :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?node_name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?media_type:media_type ->
  ?nodes_args:Node.argument list ->
  ?additional_bootstrap_account_count:int ->
  ?additional_revealed_bootstrap_account_count:int ->
  ?default_accounts_balance:int ->
  ?parameter_file:string ->
  ?timestamp:timestamp ->
  ?keys:Account.key list ->
  ?rpc_external:bool ->
  ?dal_node:Dal_node.t ->
  ?remote_signer:Uri.t ->
  [`Client | `Light | `Proxy] ->
  protocol:Protocol.t ->
  unit ->
  (Node.t * t) Lwt.t

(** Create a client with mode [Mockup] and run [create mockup].

    Contrary to [init], this does not import any secret key, because
   [octez-client create mockup] already initializes the mockup with bootstrap
   keys.
*)
val init_mockup :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?sync_mode:mockup_sync_mode ->
  ?parameter_file:string ->
  ?constants:Protocol.constants ->
  protocol:Protocol.t ->
  unit ->
  t Lwt.t

(** Create a client with mode [Light]. In addition to the client, created
    nodes are returned, as they are created by this call; and
    the light mode needs tight interaction with the nodes. The [nodes_args]
    argument allows to configure the created nodes, but note
    that arguments [Node.Connections] and [Node.Synchronisation_threshold]
    are ignored. *)
val init_light :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?min_agreement:float ->
  ?event_level:Daemon.Level.default_level ->
  ?event_sections_levels:(string * Daemon.Level.level) list ->
  ?nodes_args:Node.argument list ->
  ?dal_node:Dal_node.t ->
  ?remote_signer:Uri.t ->
  unit ->
  (t * Node.t * Node.t) Lwt.t

(** Spawn a low-level client command.

   Prefer using higher-level functions defined in this module, or adding a new
   one, to deferring to [spawn_command].

   It can be used, for example, for low-level one-shot customization of client
   commands.  *)
val spawn_command :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?admin:bool ->
  ?protocol_hash:string ->
  ?config_file:string ->
  ?no_base_dir_warnings:bool ->
  ?block:string ->
  t ->
  string list ->
  Process.t

(** Spawn a low-level client command as in [spawn_command] but with a channel to send data to the process [stdin]. *)
val spawn_command_with_stdin :
  ?log_command:bool ->
  ?log_status_on_exit:bool ->
  ?log_output:bool ->
  ?env:string String_map.t ->
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?admin:bool ->
  ?protocol_hash:string ->
  t ->
  string list ->
  Process.t * Lwt_io.output_channel

(** Register public key for given account with given client. *)
val spawn_register_key :
  ?hooks:Process.hooks ->
  ?consensus:string ->
  ?consensus_pop:string ->
  ?companion:string ->
  ?companion_pop:string ->
  ?amount:Tez.t ->
  string ->
  t ->
  Process.t

(** Register public key for given account with given client. *)
val register_key :
  ?hooks:Process.hooks ->
  ?expect_failure:bool ->
  ?consensus:string ->
  ?consensus_pop:string ->
  ?companion:string ->
  ?companion_pop:string ->
  ?amount:Tez.t ->
  string ->
  t ->
  unit Lwt.t

(** Get contract storage for a contract. Returns a Micheline expression
    representing the storage as a string. *)
val contract_storage :
  ?hooks:Process.hooks ->
  ?unparsing_mode:normalize_mode ->
  ?endpoint:endpoint ->
  string ->
  t ->
  string Lwt.t

(** Get contract code for a contract. Returns a Micheline expression
    representing the code as a string. *)
val contract_code :
  ?unparsing_mode:normalize_mode -> string -> t -> string Lwt.t

(** Get contract entrypoint type for a contract. *)
val contract_entrypoint_type :
  entrypoint:string -> contract:string -> t -> string Lwt.t

(** Same as [contract_entrypoint_type], but do not wait for the process to exit. *)
val spawn_contract_entrypoint_type :
  entrypoint:string -> contract:string -> t -> Process.t

(** Show a conversion format as used for the [convert*] function
    family *)
val conversion_format_to_string :
  [< `Binary | `Json | `Michelson | `OCaml] -> string

(** Use octez-client to convert a script between given forms. *)
val convert_script :
  script:string ->
  src_format:[`Michelson | `Json | `Binary] ->
  dst_format:[`Michelson | `Json | `Binary | `OCaml] ->
  ?typecheck:string ->
  t ->
  string Lwt.t

(** Use octez-client to convert a script between given forms. *)
val convert_data :
  data:string ->
  src_format:[`Michelson | `Json | `Binary] ->
  dst_format:[`Michelson | `Json | `Binary | `OCaml] ->
  ?typecheck:string ->
  t ->
  string Lwt.t

(** Convert the given smart contract from Michelson to JSON string. *)
val convert_script_to_json :
  ?endpoint:endpoint -> script:string -> t -> JSON.u Lwt.t

(** Convert the given Michelson constant to JSON string. *)
val convert_data_to_json :
  ?endpoint:endpoint -> data:string -> ?typecheck:string -> t -> JSON.u Lwt.t

(** Run [octez-client bootstrapped]. *)
val bootstrapped : t -> unit Lwt.t

(** Run [octez-client config show]. *)
val config_show :
  ?config_file:string -> ?protocol:Protocol.t -> t -> string Lwt.t

(** Same as [config_show], but do not wait for the process to exit. *)
val spawn_config_show :
  ?config_file:string -> ?protocol:Protocol.t -> t -> Process.t

(** Run [octez-client config show]. *)
val config_init :
  ?config_file:string ->
  ?protocol:Protocol.t ->
  ?bootstrap_accounts:string ->
  ?protocol_constants:string ->
  ?output:string ->
  t ->
  unit Lwt.t

(** Same as [config_init], but do not wait for the process to exit. *)
val spawn_config_init :
  ?config_file:string ->
  ?protocol:Protocol.t ->
  ?bootstrap_accounts:string ->
  ?protocol_constants:string ->
  ?output:string ->
  t ->
  Process.t

(** Run [octez-client compute chain id from block hash]. *)
val compute_chain_id_from_block_hash :
  ?endpoint:endpoint -> t -> string -> string Lwt.t

(** Same as [compute_chain_id_from_block_hash], but do not wait for the process to exit. *)
val spawn_compute_chain_id_from_block_hash :
  ?endpoint:endpoint -> t -> string -> Process.t

(** Run [octez-client compute chain id from seed]. *)
val compute_chain_id_from_seed :
  ?endpoint:endpoint -> t -> string -> string Lwt.t

(** Same as [compute_chain_id_from_seed], but do not wait for the process to exit. *)
val spawn_compute_chain_id_from_seed :
  ?endpoint:endpoint -> t -> string -> Process.t

(** {2 Commands for managing a multisig smart contract} *)

(** Run [octez-client show supported multisig hashes]. *)
val show_supported_multisig_hashes : t -> unit Lwt.t

(** Same as [show_supported_multisig_hashes], but do not wait for the
    process to exit. *)
val spawn_show_supported_multisig_hashes : t -> Process.t

(** Run [octez-client show multisig script]. *)
val show_multisig_script : t -> unit Lwt.t

(** Same as [show_multisig_script], but do not wait for the process to
    exit. *)
val spawn_show_multisig_script : t -> Process.t

(** Run [octez-client deploy multisig <new_multisig> transferring
    <qty> from <src> with threshold <threshold> on public keys
    <key>]. *)
val deploy_multisig :
  new_multisig:string ->
  qty:Tez.t ->
  src:string ->
  threshold:int ->
  keys:string list ->
  ?delegate:string ->
  ?force:bool ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [deploy_multisig], but do not wait for the process to exit. *)
val spawn_deploy_multisig :
  new_multisig:string ->
  qty:Tez.t ->
  src:string ->
  threshold:int ->
  keys:string list ->
  ?delegate:string ->
  ?force:bool ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client sign multisig transaction on <multisig>
    transferring <qty> to <dst> using secret key <key>]. *)
val sign_multisig_transaction_transfer :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  key:string ->
  ?arg:string ->
  ?entrypoint:string ->
  t ->
  string Lwt.t

(** Same as [sign_multisig_transaction_transfer],
    but do not wait for the process to exit. *)
val spawn_sign_multisig_transaction_transfer :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  key:string ->
  ?arg:string ->
  ?entrypoint:string ->
  t ->
  Process.t

(** Run [octez-client sign multisig transaction on <multisig> running
    lambda <lambda> using secret key <key>]. *)
val sign_multisig_transaction_run_lambda :
  multisig:string -> lambda:string -> key:string -> t -> string Lwt.t

(** Same as [sign_multisig_transaction_run_lambda],
    but do not wait for the process to exit. *)
val spawn_sign_multisig_transaction_run_lambda :
  multisig:string -> lambda:string -> key:string -> t -> Process.t

(** Run [octez-client sign multisig transaction on <multisig> setting
    delegate to <dlgt> using secret key <key>]. *)
val sign_multisig_transaction_set_delegate :
  multisig:string -> dlgt:string -> key:string -> t -> string Lwt.t

(** Same as [sign_multisig_transaction_set_delegate],
    but do not wait for the process to exit. *)
val spawn_sign_multisig_transaction_set_delegate :
  multisig:string -> dlgt:string -> key:string -> t -> Process.t

(** Run [octez-client sign multisig transaction on <multisig>
    withdrawing delegate using secret key <key>]. *)
val sign_multisig_transaction_withdraw_delegate :
  multisig:string -> key:string -> t -> string Lwt.t

(** Same as [sign_multisig_transaction_withdraw_delegate],
    but do not wait for the process to exit. *)
val spawn_sign_multisig_transaction_withdraw_delegate :
  multisig:string -> key:string -> t -> Process.t

(** Run [octez-client sign multisig transaction on <multisig> using
    secret key <key> setting threshold to <threshold> and public keys
    to <key>]. *)
val sign_multisig_transaction_set_threshold_and_public_keys :
  multisig:string ->
  signing_key:string ->
  threshold:int ->
  public_keys:string list ->
  t ->
  string Lwt.t

(** Same as [sign_multisig_transaction_set_threshold_and_public_keys],
    but do not wait for the process to exit. *)
val spawn_sign_multisig_transaction_set_threshold_and_public_keys :
  multisig:string ->
  signing_key:string ->
  threshold:int ->
  public_keys:string list ->
  t ->
  Process.t

(** Run [octez-client from multisig contract <multisig> transfer <qty>
    to <dst> on behalf of <src> with signatures <signature>]. *)
val from_multisig_transfer :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  src:string ->
  signatures:string list ->
  ?arg:string ->
  ?burn_cap:Tez.t ->
  ?entrypoint:string ->
  t ->
  unit Lwt.t

(** Same as [from_multisig_transfer],
    but do not wait for the process to exit. *)
val spawn_from_multisig_transfer :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  src:string ->
  signatures:string list ->
  ?arg:string ->
  ?burn_cap:Tez.t ->
  ?entrypoint:string ->
  t ->
  Process.t

(** Run [octez-client from multisig contract <multisig> run lambda
    <lambda> on behalf of <src> with signatures <signature>]. *)
val from_multisig_run_lambda :
  multisig:string ->
  lambda:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [from_multisig_run_lambda],
    but do not wait for the process to exit. *)
val spawn_from_multisig_run_lambda :
  multisig:string ->
  lambda:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client set delegate of multisig contract <multisig> to
    <dlgt> on behalf of <src> with signatures <signature>]. *)
val set_delegate_of_multisig :
  multisig:string ->
  dlgt:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [set_delegate_of_multisig],
    but do not wait for the process to exit. *)
val spawn_set_delegate_of_multisig :
  multisig:string ->
  dlgt:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client withdraw delegate of multisig contract
    <multisig> on behalf of <src> with signatures <signature>]. *)
val withdraw_delegate_of_multisig :
  multisig:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [withdraw_delegate_of_multisig],
    but do not wait for the process to exit. *)
val spawn_withdraw_delegate_of_multisig :
  multisig:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client set threshold of multisig contract <multisig> to
    <threshold> and public keys to <public_keys> on behalf of <src> with
    signatures <signature>]. *)
val set_threshold_of_multisig :
  multisig:string ->
  threshold:int ->
  public_keys:string list ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [set_threshold_of_multisig],
    but do not wait for the process to exit. *)
val spawn_set_threshold_of_multisig :
  multisig:string ->
  threshold:int ->
  public_keys:string list ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client run transaction <bytes> on multisig contract
    <multisig> on behalf of <src> with signatures <signature>]. *)
val run_transaction_on_multisig :
  bytes:string ->
  multisig:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  unit Lwt.t

(** Same as [run_transaction_on_multisig],
    but do not wait for the process to exit. *)
val spawn_run_transaction_on_multisig :
  bytes:string ->
  multisig:string ->
  src:string ->
  signatures:string list ->
  ?burn_cap:Tez.t ->
  t ->
  Process.t

(** Run [octez-client prepare multisig transaction on <multisig>
    transferring <qty> to <dst>]. *)
val prepare_multisig_transaction :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  ?arg:string ->
  ?entrypoint:string ->
  ?bytes_only:bool ->
  t ->
  string Lwt.t

(** Same as [prepare_multisig_transaction], but do
    not wait for the process to exit. *)
val spawn_prepare_multisig_transaction :
  multisig:string ->
  qty:Tez.t ->
  dst:string ->
  ?arg:string ->
  ?entrypoint:string ->
  ?bytes_only:bool ->
  t ->
  Process.t

(** Run [octez-client prepare multisig transaction on <multisig>
    running lambda <lambda>]. *)
val prepare_multisig_transaction_run_lambda :
  multisig:string -> lambda:string -> ?bytes_only:bool -> t -> string Lwt.t

(** Same as [prepare_multisig_transaction_run_lambda], but do
    not wait for the process to exit. *)
val spawn_prepare_multisig_transaction_run_lambda :
  multisig:string -> lambda:string -> ?bytes_only:bool -> t -> Process.t

(** Run [octez-client prepare multisig transaction on <multisig>
    setting delegate to <dlgt>]. *)
val prepare_multisig_transaction_set_delegate :
  multisig:string -> dlgt:string -> ?bytes_only:bool -> t -> string Lwt.t

(** Same as [prepare_multisig_transaction_set_delegate], but
    do not wait for the process to exit. *)
val spawn_prepare_multisig_transaction_set_delegate :
  multisig:string -> dlgt:string -> ?bytes_only:bool -> t -> Process.t

(** Run [octez-client prepare multisig transaction on <multisig>
    withdrawing delegate]. *)
val prepare_multisig_transaction_withdraw_delegate :
  multisig:string -> ?bytes_only:bool -> t -> string Lwt.t

(** Same as [prepare_multisig_transaction_withdraw_delegate],
    but do not wait for the process to exit. *)
val spawn_prepare_multisig_transaction_withdraw_delegate :
  multisig:string -> ?bytes_only:bool -> t -> Process.t

(** Run [octez-client prepare multisig transaction on <multisig>
    setting threshold to <threshold> and public keys to <public_keys>]. *)
val prepare_multisig_transaction_set_threshold_and_public_keys :
  multisig:string ->
  threshold:int ->
  public_keys:string list ->
  ?bytes_only:bool ->
  t ->
  string Lwt.t

(** Same as
    [prepare_multisig_transaction_set_threshold_and_public_keys],
    but do not wait for the process to exit. *)
val spawn_prepare_multisig_transaction_set_threshold_and_public_keys :
  multisig:string ->
  threshold:int ->
  public_keys:string list ->
  ?bytes_only:bool ->
  t ->
  Process.t

(** Run [octez-client expand macros in <script>]. *)
val expand_macros :
  ?endpoint:endpoint ->
  ?hooks:Process_hooks.t ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  t ->
  string ->
  string Lwt.t

(** Same as [expand_macros], but do not wait for the process to exit. *)
val spawn_expand_macros :
  ?endpoint:endpoint ->
  ?hooks:Process_hooks.t ->
  ?protocol_hash:string ->
  ?no_base_dir_warnings:bool ->
  t ->
  string ->
  Process.t

(** Run [octez-client get timestamp]. *)
val get_timestamp :
  ?endpoint:endpoint -> ?block:string -> ?seconds:bool -> t -> string Lwt.t

(** Same as [get_timestamp], but do not wait for the process to exit. *)
val spawn_get_timestamp :
  ?endpoint:endpoint -> ?block:string -> ?seconds:bool -> t -> Process.t

(** Run [octez-client publish dal commitment <commitment> from <src> for slot
    <slot_index> with proof <proof>]. *)
val publish_dal_commitment :
  ?hooks:Process_hooks.t ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  src:string ->
  commitment:string ->
  slot_index:int ->
  proof:string ->
  t ->
  unit Runnable.process

(** Return the information stored in the given endpoint as a foreign
    endpoint. *)
val as_foreign_endpoint : endpoint -> Endpoint.t

(** Run [octez-client get receipt for <operation> --check-previous <blocks>]. *)
val get_receipt_for :
  operation:string -> ?check_previous:int -> t -> string Lwt.t

(** Run [octez-client stake <amount> for <staker>]. *)
val stake :
  ?wait:string ->
  ?hooks:Process_hooks.t ->
  Tez.t ->
  staker:string ->
  t ->
  unit Lwt.t

(** Same as [stake], but do not wait for the process to exit. *)
val spawn_stake :
  ?wait:string ->
  ?hooks:Process_hooks.t ->
  Tez.t ->
  staker:string ->
  t ->
  Process.t

(** Run [octez-client unstake <amount> for <staker>]. *)
val unstake : ?wait:string -> Tez.t -> staker:string -> t -> unit Lwt.t

(** Same as [unstake], but do not wait for the process to exit. *)
val spawn_unstake : ?wait:string -> Tez.t -> staker:string -> t -> Process.t

(** Run [octez-client finalize_unstake for <staker>]. *)
val finalize_unstake : ?wait:string -> staker:string -> t -> unit Lwt.t

(** Same as [finalize_unstake], but do not wait for the process to exit. *)
val spawn_finalize_unstake : ?wait:string -> staker:string -> t -> Process.t

(** Run [octez-client set delegate parameters for <delegate> --limit-of-staking-over-baking <limit> --edge-of-baking-over-staking <edge>]. *)
val set_delegate_parameters :
  ?wait:string ->
  delegate:string ->
  limit:string ->
  edge:string ->
  t ->
  unit Lwt.t

(** Same as [set_delegate_parameters], but do not wait for the process to exit. *)
val spawn_set_delegate_parameters :
  ?wait:string ->
  delegate:string ->
  limit:string ->
  edge:string ->
  t ->
  Process.t

(** Run [octez-client update delegate parameters for <delegate> --limit-of-staking-over-baking <limit> --edge-of-baking-over-staking <edge>].
Either edge or limit can be omitted, the corresponding argument will not be passed. *)
val update_delegate_parameters :
  ?wait:string ->
  delegate:string ->
  ?limit:string ->
  ?edge:string ->
  t ->
  unit Lwt.t

(** Same as [update_delegate_parameters], but do not wait for the process to exit. *)
val spawn_update_delegate_parameters :
  ?wait:string ->
  delegate:string ->
  ?limit:string ->
  ?edge:string ->
  t ->
  Process.t

module RPC : sig
  (** Perform RPC calls using [octez-client]. *)

  (** RPC calls performed this way are slower and should only be used to test
      the [rpc] command of the client. *)

  (** Call an RPC using [octez-client rpc].

      The response body is parsed as JSON, then decoded using the decode function
      of the RPC description.

      The following arguments:
      - [log_command];
      - [log_status_on_exit];
      - [log_output];
      - [better_errors];
      - [endpoint];
      - [hooks];
      - [env];
      - [protocol_hash];
      are passed to [Client.rpc]. *)
  val call :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    t ->
    'result RPC_core.t ->
    'result Lwt.t

  (** Call an RPC using the client's endpoint, if available. Otherwise, fallback
      to {!call}, that is, fallback to [octez-client rpc]. Note that using
      directly the endpoint is much (likely 2 orders of magnitude) faster than
      using [octez-client]. *)
  val call_via_endpoint : t -> 'result RPC_core.t -> 'result Lwt.t

  (** Call an RPC, but do not parse the client output. *)
  val call_raw :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    t ->
    'result RPC_core.t ->
    string Lwt.t

  (** Call an RPC, but do not decode the client output, only parse it. *)
  val call_json :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    t ->
    'result RPC_core.t ->
    JSON.t Lwt.t

  (** Get the schema of an RPC as JSON. *)
  val schema :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    t ->
    'result RPC_core.t ->
    JSON.t Lwt.t

  (** Same as [call_raw], but do not wait for the process to exit.

      Because this function is mostly used to test error cases, the response body
      is not decoded. *)
  val spawn :
    ?log_command:bool ->
    ?log_status_on_exit:bool ->
    ?log_output:bool ->
    ?better_errors:bool ->
    ?endpoint:endpoint ->
    ?hooks:Process.hooks ->
    ?env:string String_map.t ->
    ?protocol_hash:string ->
    t ->
    'result RPC_core.t ->
    JSON.t Runnable.process
end

(** Run [octez-client aggregate bls signatures <signatures>]. *)
val aggregate_bls_signatures :
  pk:string -> msg:string -> t -> string list -> string Lwt.t

(** Run [octez-client create bls proof for <signer>]. *)
val create_bls_proof : ?override_pk:string -> signer:string -> t -> string Lwt.t

(** Run [octez-client check bls proof <proof> for <pk>]. *)
val check_bls_proof :
  ?override_pk:string -> pk:string -> proof:string -> t -> unit Lwt.t

(** Run [octez-client aggregate bls public keys <pks_with_proofs>].
    Returns [(aggregated_public_key, aggregated_public_key_hash)]. *)
val aggregate_bls_public_keys :
  t -> (string * string) list -> (string * string) Lwt.t

(** Run [octez-client aggregate bls proofs <pk_with_proofs>]. *)
val aggregate_bls_proofs : pk:string -> t -> string list -> string Lwt.t

(** Run [octez-client share bls secret key <sk> between <n> shares
    with threshold <m>]. *)
val share_bls_secret_key :
  sk:string ->
  n:int ->
  m:int ->
  t ->
  (string * string * string * (int * string) list) Lwt.t

(** Run [octez-client threshold bls signatures <id_signatures>]. *)
val threshold_bls_signatures :
  pk:string -> msg:string -> t -> (int * string) list -> string Lwt.t

(* Environment variable names for experimental client/baker features *)
val signing_delay_env_var : string

val fixed_seed_env_var : string

(** Run [octez-client clst deposit <amount> for <source>]. *)
val clst_deposit :
  ?wait:string -> ?burn_cap:Tez.t -> Tez.t -> src:string -> t -> unit Lwt.t

(** Same as [clst_deposit], but do not wait for the process to exit. *)
val spawn_clst_deposit :
  ?wait:string -> ?burn_cap:Tez.t -> Tez.t -> src:string -> t -> Process.t

(** Run [octez-client clst redeem <amount> for <source>]. *)
val clst_redeem :
  ?wait:string -> ?burn_cap:Tez.t -> Tez.t -> src:string -> t -> unit Lwt.t

(** Same as [clst_redeem], but do not wait for the process to exit. *)
val spawn_clst_redeem :
  ?wait:string -> ?burn_cap:Tez.t -> Tez.t -> src:string -> t -> Process.t
