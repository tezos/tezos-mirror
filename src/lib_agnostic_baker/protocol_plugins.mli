(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Protocol registration logic} *)

type proto_plugin = (module Protocol_plugin_sig.S)

(** Register a protocol plugin for a specific protocol to be used by the
    agnostic baker. *)
val register : proto_plugin -> unit

(** Returns the list of registered protocols. *)
val registered_protocols : unit -> Protocol_hash.t list

(** Returns the last registered protocol.

    NOTE: This is the last protocol with which the agnostic baker is linked
    against, and this is decided only by the order in [manifest/main.ml]. *)
val last_registered : unit -> Protocol_hash.t

(** {2 Using the correct protocol plugin} *)

(** Return the protocol plugin for a given protocol (or an error if not
    supported). *)
val proto_plugin_for_protocol : Protocol_hash.t -> proto_plugin tzresult
