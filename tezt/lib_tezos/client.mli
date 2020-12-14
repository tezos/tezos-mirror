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
type mode = Client of Node.t option | Mockup | Proxy of Node.t

(** The synchronization mode of the client.

    - [Asynchronous] mode is when transfer doesn't bake the block.
    - [Synchronous] is the default mode (no flag passed to [create mockup]). *)
type mockup_sync_mode = Asynchronous | Synchronous

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

(** Use the client to call an RPC.

    Run [tezos-client rpc meth path?query_string with data].
    Fail the test if the RPC call failed. *)
val rpc :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
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
  ?node:Node.t -> ?key:string -> ?minimal_timestamp:bool -> t -> unit Lwt.t

(** Same as [bake_for], but do not wait for the process to exit. *)
val spawn_bake_for :
  ?node:Node.t -> ?key:string -> ?minimal_timestamp:bool -> t -> Process.t

(** Run [tezos-client show address]. *)
val show_address : ?show_secret:bool -> alias:string -> t -> Account.key Lwt.t

(** Same as [show_address], but do not wait for the process to exit. *)
val spawn_show_address : ?show_secret:bool -> alias:string -> t -> Process.t

(** A helper to run [tezos-client gen keys] followed by
    [tezos-client show address] to get the generated key. *)
val gen_and_show_keys : alias:string -> t -> Account.key Lwt.t

(** Run [tezos-client transfer amount from giver to receiver]. *)
val transfer :
  ?node:Node.t ->
  ?wait:string ->
  ?args:string list ->
  amount:int ->
  giver:string ->
  receiver:string ->
  t ->
  unit Lwt.t

(** Same as [transfer], but do not wait for the process to exit. *)
val spawn_transfer :
  ?node:Node.t ->
  ?wait:string ->
  ?args:string list ->
  amount:int ->
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
  ?sync_mode:mockup_sync_mode -> protocol:Protocol.t -> t -> unit Lwt.t

(** Same as [create_mockup], but do not wait for the process to exit. *)
val spawn_create_mockup :
  ?sync_mode:mockup_sync_mode -> protocol:Protocol.t -> t -> Process.t

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
  ?burn_cap:int ->
  alias:string ->
  amount:int ->
  src:string ->
  prg:string ->
  t ->
  string Lwt.t

(** Same as [originate_contract], but do not wait for the process to exit. *)
val spawn_originate_contract :
  ?node:Node.t ->
  ?wait:string ->
  ?init:string ->
  ?burn_cap:int ->
  alias:string ->
  amount:int ->
  src:string ->
  prg:string ->
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
  protocol:Protocol.t ->
  unit ->
  t Lwt.t
