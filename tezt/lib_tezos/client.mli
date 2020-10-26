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

(** Mode of the client *)
type mode =
  | Client of Node.t option
  | Mockup
  | Light of float * Node.t list
  | Proxy of Node.t

(** The synchronization mode of the client.

    - [Asynchronous] mode is when transfer doesn't bake the block.
    - [Synchronous] is the default mode (no flag passed to [create mockup]). *)
type mockup_sync_mode = Asynchronous | Synchronous

(** The mode argument of the client's 'normalize data' command *)
type normalize_mode = Readable | Optimized | Optimized_legacy

(** Tezos client states. *)
type t

(** Get the base directory of a client.

    The base directory is the location where clients store their
    configuration files. It corresponds to the [--base-dir] option. *)
val base_dir : t -> string

(** Create a client.

    The standard output and standard error output of the node will
    be logged with prefix [name] and color [color].

    Default [base_dir] is a temporary directory
    which is always the same for each [name].

    The node argument is used to know which port the client should connect to.
    This node can be overridden for each command, as a client is not actually tied
    to a node. Most commands require a node to be specified (either with [create]
    or with the command itself). *)
val create :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?node:Node.t ->
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

(** {2 RPC calls} *)

(** Paths for RPCs.

    For instance, [["chains"; "main"; "blocks"; "head"]]
    denotes [/chains/main/blocks/head]. *)
type path = string list

(** Query strings for RPCs.

    For instance, [["key1", "value1"; "key2", "value2"]]
    denotes [?key1=value1&key2=value2]. *)
type query_string = (string * string) list

(** HTTP methods for RPCs. *)
type meth = GET | PUT | POST | PATCH

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
  ?node:Node.t ->
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
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?env:string String_map.t ->
  ?data:JSON.u ->
  ?query_string:query_string ->
  meth ->
  path ->
  t ->
  Process.t

(** Run [tezos-client rpc list]. *)
val rpc_list : ?node:Node.t -> t -> string Lwt.t

(** Same as [rpc_list], but do not wait for the process to exit. *)
val spawn_rpc_list : ?node:Node.t -> t -> Process.t

(** Run [tezos-client rpc /chains/<chain>/blocks/<block>/header/shell]. *)
val shell_header :
  ?node:Node.t -> ?chain:string -> ?block:string -> t -> string Lwt.t

(** Same as [shell_header], but do not wait for the process to exit. *)
val spawn_shell_header :
  ?node:Node.t -> ?chain:string -> ?block:string -> t -> Process.t

(** {2 Admin Client Commands} *)

module Admin : sig
  (** Run tezos-admin-client commands. *)

  (** Ask a node to trust the address and port of another node. *)
  val trust_address : ?node:Node.t -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [trust_address], but do not wait for the process to exit. *)
  val spawn_trust_address : ?node:Node.t -> peer:Node.t -> t -> Process.t

  (** Connect a node to another peer. *)
  val connect_address : ?node:Node.t -> peer:Node.t -> t -> unit Lwt.t

  (** Same as [connect_address], but do not wait for the process to exit. *)
  val spawn_connect_address : ?node:Node.t -> peer:Node.t -> t -> Process.t

  (** Kick a peer.

      [peer] is the identity of the peer to kick.
      You can get it with [Node.wait_for_identity] for instance. *)
  val kick_peer : ?node:Node.t -> peer:string -> t -> unit Lwt.t

  (** Same as [kick_peer], but do not wait for the process to exit. *)
  val spawn_kick_peer : ?node:Node.t -> peer:string -> t -> Process.t
end

(** {2 Regular Client Commands} *)

(** Run [tezos-client import secret key]. *)
val import_secret_key : ?node:Node.t -> t -> Constant.key -> unit Lwt.t

(** Same as [import_secret_key], but do not wait for the process to exit. *)
val spawn_import_secret_key : ?node:Node.t -> t -> Constant.key -> Process.t

(** Run [tezos-client activate protocol].

    If [timestamp] is not specified explicitely, it is set to [now -. timestamp_delay].
    Default value for [timestamp_delay] is 365 days, which allows to bake plenty of blocks
    before their timestamp reach the present (at which point one would have to wait
    between each block so that peers do not reject them for being in the future). *)
val activate_protocol :
  ?node:Node.t ->
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
  ?node:Node.t ->
  protocol:Protocol.t ->
  ?fitness:int ->
  ?key:string ->
  ?timestamp:string ->
  ?timestamp_delay:float ->
  ?parameter_file:string ->
  t ->
  Process.t

(** Run [tezos-client bake for].

    Default [key] is {!Constant.bootstrap1.alias}. *)
val bake_for :
  ?node:Node.t ->
  ?protocol:Protocol.t ->
  ?key:string ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?force:bool ->
  ?context_path:string ->
  t ->
  unit Lwt.t

(** Same as [bake_for], but do not wait for the process to exit. *)
val spawn_bake_for :
  ?node:Node.t ->
  ?protocol:Protocol.t ->
  ?key:string ->
  ?minimal_timestamp:bool ->
  ?mempool:string ->
  ?force:bool ->
  ?context_path:string ->
  t ->
  Process.t

(** Run [tezos-client show address]. *)
val show_address : ?show_secret:bool -> alias:string -> t -> Account.key Lwt.t

(** Same as [show_address], but do not wait for the process to exit. *)
val spawn_show_address : ?show_secret:bool -> alias:string -> t -> Process.t

(** A helper to run [tezos-client gen keys] followed by
    [tezos-client show address] to get the generated key. *)
val gen_and_show_keys : alias:string -> t -> Account.key Lwt.t

(** Run [tezos-client endorse for].

    Default [key] is {!Constant.bootstrap2.alias}. *)
val endorse_for : ?node:Node.t -> ?key:string -> t -> unit Lwt.t

(** Same as [endorse_for], but do not wait for the process to exit. *)
val spawn_endorse_for : ?node:Node.t -> ?key:string -> t -> Process.t

(** Run [tezos-client transfer amount from giver to receiver]. *)
val transfer :
  ?node:Node.t ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  unit Lwt.t

(** Same as [transfer], but do not wait for the process to exit. *)
val spawn_transfer :
  ?node:Node.t ->
  ?wait:string ->
  ?burn_cap:Tez.t ->
  ?fee:Tez.t ->
  ?gas_limit:int ->
  ?storage_limit:int ->
  amount:Tez.t ->
  giver:string ->
  receiver:string ->
  t ->
  Process.t

(** Run [tezos-client set delegate for <src> to <delegate>]. *)
val set_delegate :
  ?node:Node.t ->
  ?wait:string ->
  src:string ->
  delegate:string ->
  t ->
  unit Lwt.t

(** Same as [set_delegate], but do not wait for the process to exit. *)
val spawn_set_delegate :
  ?node:Node.t ->
  ?wait:string ->
  src:string ->
  delegate:string ->
  t ->
  Process.t

(** Run [tezos-client withdraw delegate from <src>]. *)
val withdraw_delegate :
  ?node:Node.t -> ?wait:string -> src:string -> t -> unit Lwt.t

(** Same as [withdraw_delegate], but do not wait for the process to exit. *)
val spawn_withdraw_delegate :
  ?node:Node.t -> ?wait:string -> src:string -> t -> Process.t

(** Run [tezos-client get balance for]. *)
val get_balance_for : ?node:Node.t -> account:string -> t -> float Lwt.t

(** Same as [get_balance_for], but do not wait for the process to exit. *)
val spawn_get_balance_for : ?node:Node.t -> account:string -> t -> Process.t

(** Run [tezos-client create mockup]. *)
val create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?constants:Protocol.constants ->
  protocol:Protocol.t ->
  t ->
  unit Lwt.t

(** Same as [create_mockup], but do not wait for the process to exit. *)
val spawn_create_mockup :
  ?sync_mode:mockup_sync_mode ->
  ?constants:Protocol.constants ->
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
  ?node:Node.t ->
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
  ?node:Node.t ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:Tez.t ->
  alias:string ->
  amount:Tez.t ->
  src:string ->
  prg:string ->
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

(** Run [tezos-client list mockup protocols]. *)
val list_mockup_protocols : t -> string list Lwt.t

(** Same as [list_mockup_protocols], but do not wait for the process to exit
    and do not process stdout. *)
val spawn_list_mockup_protocols : t -> Process.t

(** Run [tezos-client migrate mockup to]. *)
val migrate_mockup : next_protocol:Protocol.t -> t -> unit Lwt.t

(** Same as [migrate_mockup], but do not wait for the process to exit. *)
val spawn_migrate_mockup : next_protocol:Protocol.t -> t -> Process.t

(** {2 High-Level Functions} *)

(** Create a client with mode [Client] and import all secret keys
    listed in {!Constant.all_secret_keys}. *)
val init :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?node:Node.t ->
  unit ->
  t Lwt.t

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
  ?constants:Protocol.constants ->
  protocol:Protocol.t ->
  unit ->
  t Lwt.t

(** Create a client with mode [Light]. In addition to the client, the list
    of nodes is returned, as it was potentially created by this call; and
    the light mode needs tight interaction with the nodes. *)
val init_light :
  ?path:string ->
  ?admin_path:string ->
  ?name:string ->
  ?color:Log.Color.t ->
  ?base_dir:string ->
  ?nodes:Node.t list ->
  ?min_agreement:float ->
  unit ->
  (t * Node.t list) Lwt.t
