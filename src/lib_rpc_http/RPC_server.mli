(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Typed RPC services: server implementation. *)

type cors = {allowed_headers : string list; allowed_origins : string list}

module RPC_logging : Resto_cohttp_server.Server.LOGGING

include module type of
    Resto_cohttp_server.Server.Make (Tezos_rpc.Encoding) (RPC_logging)

(** [launch ?host server ?conn_closed ?callback
    ?max_active_connections listening_protocol] starts the given resto
    [server] initiating the listening loop using the

    @param [callback] overwrites (if given) the default handler of
    each resto http query will be treated by.

    @param [conn_closed] is an optional function that is called when
    a connection is closed.

    @param [max_active_connections] limits the number of active
    connections. When this limit is reached, the server will not
    process new requests until existing ones are
    completed. Defaults to 100. *)
val launch :
  ?host:string ->
  server ->
  ?conn_closed:(Cohttp_lwt_unix.Server.conn -> unit) ->
  ?callback:callback ->
  ?max_active_connections:int ->
  Conduit_lwt_unix.server ->
  unit Lwt.t

module Acl : sig
  include module type of Resto_acl.Acl

  (** A policy for the whole RPC server is a set of access control lists for the
      different addresses that the RPC server listens to. It is represented as
      an association list mapping listening addresses (in string literal forms)
      to deny/access access policies. *)
  type policy

  (** Default ACL policy in case none is defined in configuration. It only
      exposes such endpoints that are necessary for the node to allow clients to
      make use of their Tez. It applies to all listening addresses except for
      [localhost] (see {!allow_all} for this address). *)
  val secure : t

  (** An allow-all policy, which is the default for the [localhost] listening address. *)
  val allow_all : t

  (** Selects default ACL based on listening address. Selects [allow_all] for
      loopback addresses and [secure] for everything else. *)
  val default : P2p_addr.t -> t

  (** Add an ACL for given address into the policy. Overrides previously existing
      policy for that address if any. *)
  val put_policy : P2p_point.Id.addr_port_id * t -> policy -> policy

  (** Empty ACL policy allows access to all endpoints. Currently it's the same
      as [default] below, but that will likely change in the future, therefore
      it's better to use [default] rather than this value. It's mainly intended
      for testing. *)
  val empty_policy : policy

  (** [policy_type p] returns the "type" of [p], either
      ["Secure"] or ["AllowAll"] or ["Custom"]. *)
  val policy_type : t -> string

  val policy_encoding : policy Data_encoding.t

  (** Returns the JSON representation of the policy. *)
  val policy_to_string : policy -> string

  (** [find_policy policy address] looks for the [address] within the [policy]
      and returns corresponding access control list.

      An ACL is considered matching if its corresponding IP part matches the IP
      part of the [address] and either:
      - its corresponding port also matches [address]'s port OR
      - its corresponding address does not mention any port at all.

      The first ACL whose corresponding address matches these criteria is
      returned. *)
  val find_policy : policy -> string * int option -> t option

  (** Returns string representation of a given matcher. Useful for testing. *)
  val matcher_to_string : matcher -> string

  (** Returns the ACL type, either `Whitelist or `Blacklist. *)
  val acl_type : t -> [`Whitelist | `Blacklist]

  (** Replace domain-name addresses in the policy with the IP addresses
      they resolve to.

      [resolve_domain_names p] returns a policy equivalent to [p] but with all
      domain-name addresses resolved to IPs. This is useful to make it easier
      to match them with listening addresses given to the server. *)

  val resolve_domain_names : policy -> policy Lwt.t

  module Internal_for_test : sig
    type endpoint = P2p_point.Id.addr_port_id

    val resolve_domain_names :
      (endpoint -> (Ipaddr.V6.t * int option) list Lwt.t) ->
      policy ->
      policy Lwt.t
  end
end
