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

(** Run Tezos client commands. *)

(** Values that can be passed to the client's [--endpoint] argument *)
type endpoint =
  | Node of Node.t  (** A full-fledged node *)
  | Proxy_server of Proxy_server.t  (** A proxy server *)

(** Values that can be passed to the client's [--media-type] argument *)
type media_type = Json | Binary | Any

(** [rpc_port endpoint] returns the port on which to reach [endpoint]
    when doing RPC calls. *)
val rpc_port : endpoint -> int

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
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?endpoint:endpoint ->
  ?media_type:media_type ->
  unit ->
  t

(** Create a client like [create] but do not assume [Client] as the mode. *)
val create_with_mode :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  mode ->
  t

(** Get a client's mode. Used with [set_mode] to temporarily change
    a client's mode *)
val get_mode : t -> mode

(** Change the client's mode. This function is required for example because
    we wanna keep a client's wallet. This is impossible if we created
    a new client from scratch. *)
val set_mode : mode -> t -> unit

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
type meth = GET | PUT | POST | PATCH | DELETE

(** A lowercase string of the method. *)
val string_of_meth : meth -> string

(** [rpc_path_query_to_string ["key1", "value1"; "key2", "value2")] ["seg1"; "seg2"]]
    returns [/seg1/seg2?key1=value1&key2=value2] where seg1, seg2, key1, key2,
    value1, and value2 have been appropriately encoded *)
val rpc_path_query_to_string : ?query_string:query_string -> path -> string

(** Use the client to call an RPC.

    Run [rpc meth path?query_string with data].
    Fail the test if the RPC call failed.

    [env] can be used to customize environment variables, e.g.
    [("TEZOS_LOG", Protocol.daemon_name protocol ^ ".proxy_rpc->debug")] to enable
    logging. *)
val rpc :
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?data:JSON.u ->
  ?query_string:query_string ->
  meth ->
  path ->
  t ->
  JSON.t Lwt.t

(** Same as [rpc], but do not wait for the process to exit. *)
val spawn_rpc :
  ?endpoint:endpoint ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?data:JSON.u ->
  ?query_string:query_string ->
  meth ->
  path ->
  t ->
  Process.t

(** Run [tezos-client rpc list]. *)
val rpc_list : ?endpoint:endpoint -> t -> string Lwt.t

(** Same as [rpc_list], but do not wait for the process to exit. *)
val spawn_rpc_list : ?endpoint:endpoint -> t -> Process.t

(** Run [tezos-client rpc /chains/<chain>/blocks/<block>/header/shell]. *)
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
  (** Run tezos-admin-client commands. *)

  (** Ask a node to trust the address and port of another node. *)
  val trust_address : ?endpoint:endpoint -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [trust_address], but do not wait for the process to exit. *)
  val spawn_trust_address : ?endpoint:endpoint -> peer:Node.t -> t -> Process.t

  (** Connect a node to another peer. *)
  val connect_address : ?endpoint:endpoint -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [connect_address], but do not wait for the process to exit. *)
  val spawn_connect_address :
    ?endpoint:endpoint -> peer:Node.t -> t -> Process.t

  (** Kick a peer.

      [peer] is the identity of the peer to kick.
      You can get it with [Node.wait_for_identity] for instance. *)
  val kick_peer : ?endpoint:endpoint -> peer:string -> t -> unit Lwt.t

  (** Same as [kick_peer], but do not wait for the process to exit. *)
  val spawn_kick_peer : ?endpoint:endpoint -> peer:string -> t -> Process.t
end

(** {2 Regular Client Commands} *)

(** Run [tezos-client --version]. *)
val version : t -> unit Lwt.t

(** Same as [version], but do not wait for the process to exit. *)
val spawn_version : t -> Process.t

(** Run [tezos-client import secret key]. *)
val import_secret_key : ?endpoint:endpoint -> t -> Account.key -> unit Lwt.t

(** Same as [import_secret_key], but do not wait for the process to exit. *)
val spawn_import_secret_key :
  ?endpoint:endpoint -> t -> Account.key -> Process.t

(** Run [tezos-client activate protocol].

    If [timestamp] is not specified explicitely, it is set to [now -. timestamp_delay].
    Default value for [timestamp_delay] is 365 days, which allows to bake plenty of blocks
    before their timestamp reach the present (at which point one would have to wait
    between each block so that peers do not reject them for being in the future). *)
val activate_protocol :
  ?endpoint:endpoint ->
  protocol:Protocol.t ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:string ->
  ?timestamp_delay:float ->
  ?parameter_file:string ->
  t ->
  unit Lwt.t

(** Same as [activate_protocol], but do not wait for the process to exit. *)
val spawn_activate_protocol :
  ?endpoint:endpoint ->
  protocol:Protocol.t ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:string ->
  ?timestamp_delay:float ->
  ?parameter_file:string ->
  t ->
  Process.t

(** [empty_mempool_file ?filename ()] creates a file containing the
   encoding of an empty mempool. This file can be given to [bake_for]
   command with the [mempool] parameter to ensure that the block baked
   will contain no operations. *)
val empty_mempool_file : ?filename:string -> unit -> string Lwt.t

(** Run [tezos-client bake for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val bake_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?monitor_node_mempool:bool ->
  ?force:bool ->
  ?context_path:string ->
  t ->
  unit Lwt.t

(** Same as [bake_for], but do not wait for the process to exit. *)
val spawn_bake_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?monitor_node_mempool:bool ->
  ?force:bool ->
  ?context_path:string ->
  t ->
  Process.t

(** Run [tezos-client bake for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val tenderbake_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?keys:string list ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?monitor_node_mempool:bool ->
  ?force:bool ->
  ?context_path:string ->
  t ->
  unit Lwt.t

(** Run [tezos-client endorse for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val endorse_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** Same as [endorse_for], but do not wait for the process to exit. *)
val spawn_endorse_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(** Run [tezos-client preendorse for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val preendorse_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** Same as [preendorse_for], but do not wait for the process to exit. *)
val spawn_preendorse_for :
  ?endpoint:endpoint ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(** Run [tezos-client propose for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val spawn_propose_for :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  Process.t

(* TODO refactor this *)

(** [propose_for] *)
val propose_for :
  ?endpoint:endpoint ->
  ?minimal_timestamp:bool ->
  ?protocol:Protocol.t ->
  ?key:string list ->
  ?force:bool ->
  t ->
  unit Lwt.t

(** Run [tezos-client show address]. *)
val show_address : ?show_secret:bool -> alias:string -> t -> Account.key Lwt.t

(** Same as [show_address], but do not wait for the process to exit. *)
val spawn_show_address : ?show_secret:bool -> alias:string -> t -> Process.t

(** Run [tezos-client gen keys]. *)
val gen_keys : alias:string -> t -> unit Lwt.t

(** A helper to run [tezos-client gen keys] followed by
    [tezos-client show address] to get the generated key. *)
val gen_and_show_keys : alias:string -> t -> Account.key Lwt.t

(** Same as [gen_and_show_keys] but returns a [Constant.key] instead of an
    [Account.key]. *)
val gen_and_show_secret_keys : alias:string -> t -> Account.key Lwt.t

(** Run [tezos-client transfer amount from giver to receiver]. *)
val transfer :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?arg:string ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  unit Lwt.t

(** Same as [transfer], but do not wait for the process to exit. *)
val spawn_transfer :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?arg:string ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  Process.t

(** Run [tezos-client multiple transfers from giver using json_batch]. *)
val multiple_transfers :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?arg:string ->
  giver:string ->
  json_batch:string ->
  t ->
  unit Lwt.t

(** Same as [multiple_transfers], but do not wait for the process to exit. *)
val spawn_multiple_transfers :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  ?counter:int ->
  ?arg:string ->
  giver:string ->
  json_batch:string ->
  t ->
  Process.t

(** Run [tezos-client set delegate for <src> to <delegate>]. *)
val set_delegate :
  ?endpoint:endpoint ->
  ?wait:string ->
  src:string ->
  delegate:string ->
  t ->
  unit Lwt.t

(** Same as [set_delegate], but do not wait for the process to exit. *)
val spawn_set_delegate :
  ?endpoint:endpoint ->
  ?wait:string ->
  src:string ->
  delegate:string ->
  t ->
  Process.t

(** Run [tezos-client withdraw delegate from <src>]. *)
val withdraw_delegate :
  ?endpoint:endpoint -> ?wait:string -> src:string -> t -> unit Lwt.t

(** Same as [withdraw_delegate], but do not wait for the process to exit. *)
val spawn_withdraw_delegate :
  ?endpoint:endpoint -> ?wait:string -> src:string -> t -> Process.t

(** Run [tezos-client get balance for]. *)
val get_balance_for : ?endpoint:endpoint -> account:string -> t -> float Lwt.t

(** Same as [get_balance_for], but do not wait for the process to exit. *)
val spawn_get_balance_for :
  ?endpoint:endpoint -> account:string -> t -> Process.t

(** Run [tezos-client create mockup]. *)
val create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?parameter_file:string ->
  protocol:Protocol.t ->
  t ->
  unit Lwt.t

(** Same as [create_mockup], but do not wait for the process to exit. *)
val spawn_create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?parameter_file:string ->
  protocol:Protocol.t ->
  t ->
  Process.t

(** Run [tezos-client submit proposals for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val submit_proposals :
  ?key:string -> ?wait:string -> proto_hash:string -> t -> unit Lwt.t

(** Same as [submit_proposals], but do not wait for the process to exit. *)
val spawn_submit_proposals :
  ?key:string -> ?wait:string -> proto_hash:string -> t -> Process.t

type ballot = Nay | Pass | Yay

(** Run [tezos-client submit ballot for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val submit_ballot :
  ?key:string -> ?wait:string -> proto_hash:string -> ballot -> t -> unit Lwt.t

(** Same as [submit_ballot], but do not wait for the process to exit. *)
val spawn_submit_ballot :
  ?key:string -> ?wait:string -> proto_hash:string -> ballot -> t -> Process.t

(** Run [tezos-client originate contract alias transferring amount from src
    running prg]. Returns the originated contract hash *)
val originate_contract :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  alias:string ->
  amount:Tez.t ->
  src:string ->
  prg:string ->
  t ->
  string Lwt.t

(** Same as [originate_contract], but do not wait for the process to exit. *)
val spawn_originate_contract :
  ?endpoint:endpoint ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  alias:string ->
  amount:Tez.t ->
  src:string ->
  prg:string ->
  t ->
  Process.t

(** Returns the name of a file containing the accounts corresponding
    to [bootstrap1], ..., [bootstrap5], in JSON format as expected by
    the [stresstest] command. *)
val write_bootstrap_stresstest_sources_file : t -> string Lwt.t

(** [stresstest ?endpoint ?transfers ?tps client] calls
    [tezos-client stresstest transfer using <sources> --transfers <transfers> --tps <tps>],
    where [sources] is the result of {!write_bootstrap_stresstest_sources_file}.

    Default values:
    - [endpoint]: cf {!create}
    - [transfers] and [tps]: do not provide the argument to the command
*)
val stresstest :
  ?endpoint:endpoint -> ?transfers:int -> ?tps:int -> t -> unit Lwt.t

(** Same as {!stresstest}, but does not wait for the process to exit,
    and takes an additional argument [sources] to pass on to te command.

    Note that the [sources] argument cannot easily be made optional or
    removed: indeed, we would need [Lwt] to compute the value used in
    {!stresstest}. *)
val spawn_stresstest :
  ?endpoint:endpoint ->
  ?transfers:int ->
  ?tps:int ->
  sources:string ->
  t ->
  Process.t

(** Run [tezos-client run script .. on storage .. and input ..].

    Returns the new storage as a string.

    Fails if the new storage cannot be extracted from the output. *)
val run_script :
  src:string -> storage:string -> input:string -> t -> string Lwt.t

(** Same as [run_script] but do not wait for the process to exit. *)
val spawn_run_script :
  src:string -> storage:string -> input:string -> t -> Process.t

(** Run [tezos-client register global constant value from src].
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

(** Run [tezos-client hash data .. of type ...]

    Given that the output of [tezos-client] is:

    [Raw packed data: 0x050303
     Script-expression-ID-Hash: exprvDnoPjyKeR9FSnvwYg5a1v6mDyB6TmnATwWySSP6VmJxrzQb9E
     Raw Script-expression-ID-Hash: 0xe0978ddc9329cbd84d25fd15a161a7d2e7e555da91e2a335ece8c8bc11ade245
     Ledger Blake2b hash: G7iMrYNckCFRujDLFgtj3cDMCnfeSQ2cmhnDtkh9REc4
     Raw Sha256 hash: 0x35ef99f7718e7d1f065bae635780f41c0cd201e9ffb3390ba6ef428c2815fa66
     Raw Sha512 hash: 0x2c9ca967bf47f6cc76861693379b7397f65e6a1b6e633df28cf02be0b0d18319ae783b4c199fd61115e000a15a5ba8a292a3b1468c2cfe2b3e3a9fa08d419698
     Gas remaining: 1039991.350 units remaining]

    this function returns the list:

    [("Raw packed data"; "0x050303")
     ("Script-expression-ID-Hash"; "exprvDnoPjyKeR9FSnvwYg5a1v6mDyB6TmnATwWySSP6VmJxrzQb9E")
     ("Raw Script-expression-ID-Hash"; "0xe0978ddc9329cbd84d25fd15a161a7d2e7e555da91e2a335ece8c8bc11ade245")
     ("Ledger Blake2b hash"; "G7iMrYNckCFRujDLFgtj3cDMCnfeSQ2cmhnDtkh9REc4")
     ("Raw Sha256 hash"; "0x35ef99f7718e7d1f065bae635780f41c0cd201e9ffb3390ba6ef428c2815fa66")
     ("Raw Sha512 hash"; "0x2c9ca967bf47f6cc76861693379b7397f65e6a1b6e633df28cf02be0b0d18319ae783b4c199fd61115e000a15a5ba8a292a3b1468c2cfe2b3e3a9fa08d419698")
     ("Gas remaining"; "1039991.350 units remaining")]

    If some lines cannot be parsed, warnings are emitted in output and
    the corresponding lines are omitted from the result. *)
val hash_data :
  ?expect_failure:bool ->
  ?hooks:Process.hooks ->
  data:string ->
  typ:string ->
  t ->
  (string * string) list Lwt.t

(** Same as [hash_data], but do not wait for the process to exit. *)
val spawn_hash_data :
  ?hooks:Process.hooks -> data:string -> typ:string -> t -> Process.t

(** Run [tezos-client normalize data .. of type ...]*)
val normalize_data :
  ?mode:normalize_mode ->
  ?legacy:bool ->
  data:string ->
  typ:string ->
  t ->
  string Lwt.t

(** Same as [normalize_data], but do not wait for the process to exit. *)
val spawn_normalize_data :
  ?mode:normalize_mode ->
  ?legacy:bool ->
  data:string ->
  typ:string ->
  t ->
  Process.t

(** Run [tezos-client normalize script ..]*)
val normalize_script :
  ?mode:normalize_mode -> script:string -> t -> string Lwt.t

(** Same as [normalize_script], but do not wait for the process to exit. *)
val spawn_normalize_script :
  ?mode:normalize_mode -> script:string -> t -> Process.t

(** Run [tezos-client typecheck script ..]*)
val typecheck_script :
  script:string ->
  ?details:bool ->
  ?emacs:bool ->
  ?no_print_source:bool ->
  ?gas:int ->
  ?legacy:bool ->
  t ->
  string Lwt.t

(** Same as [typecheck_script], but do not wait for the process to exit. *)
val spawn_typecheck_script :
  script:string ->
  ?details:bool ->
  ?emacs:bool ->
  ?no_print_source:bool ->
  ?gas:int ->
  ?legacy:bool ->
  t ->
  Process.t

(** Run [tezos-client list mode protocols]. *)
val list_protocols : [< `Light | `Mockup | `Proxy] -> t -> string list Lwt.t

(** Same as [list_protocols], but do not wait for the process to exit
    and do not process stdout. *)
val spawn_list_protocols : [< `Light | `Mockup | `Proxy] -> t -> Process.t

(** Run [tezos-client migrate mockup to]. *)
val migrate_mockup : next_protocol:Protocol.t -> t -> unit Lwt.t

(** Same as [migrate_mockup], but do not wait for the process to exit. *)
val spawn_migrate_mockup : next_protocol:Protocol.t -> t -> Process.t

(** Run [tezos-client sign block <hexdata> for <delegate>]. *)
val sign_block : t -> string -> delegate:string -> string Lwt.t

(** Same as [sign_block], but do not wait for the process to exit. *)
val spawn_sign_block : t -> string -> delegate:string -> Process.t

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
  ?media_type:media_type ->
  unit ->
  t Lwt.t

(** Set up a client and node(s) and activate a protocol.

    - Create a client with mode [Client], [Light], or [Proxy]
    - Import all secret keys listed in {!Constant.all_secret_keys}
    - Create [additional_account_count] accounts with
      [default_accounts_balance]
    - Activate the given protocol with [additional_account_count]
      additional bootstrap accounts

    In addition to the client, returns the first created node
    (if [`Light] is passed, a second node has been created, but it is
    not exposed). *)
val init_with_protocol :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?event_level:string ->
  ?nodes_args:Node.argument list ->
  ?additional_bootstrap_account_count:int ->
  ?default_accounts_balance:int ->
  ?parameter_file:string ->
  [`Client | `Light | `Proxy] ->
  protocol:Protocol.t ->
  unit ->
  (Node.t * t) Lwt.t

(** Create a client with mode [Mockup] and run [create mockup].

    Contrary to [init], this does not import any secret key, because
   [tezos-client create mockup] already initializes the mockup with bootstrap
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
  ?event_level:string ->
  ?nodes_args:Node.argument list ->
  unit ->
  (t * Node.t * Node.t) Lwt.t
