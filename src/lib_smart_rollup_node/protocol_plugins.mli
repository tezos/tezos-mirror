(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** {2 Protocol registration logic} *)

type proto_plugin = (module Protocol_plugin_sig.S)

(** Register a protocol plugin for a specific protocol to be used by the
    rollup node. *)
val register : proto_plugin -> unit

(** Returns the list of registered protocols. *)
val registered_protocols : unit -> Protocol_hash.t list

(** Returns the last registered protocol.

    NOTE: This is the last protocol with which the rollup node is linked
    against, and this is decided only by the order in [manifest/main.ml]. *)
val last_registered : unit -> Protocol_hash.t

(** {2 Using the correct protocol plugin} *)

(** Return the protocol plugin for a given protocol (or an error if not
    supported). *)
val proto_plugin_for_protocol : Protocol_hash.t -> proto_plugin tzresult

(** Return the protocol plugin for a given level (or an error if not
    supported). *)
val proto_plugin_for_level :
  _ Node_context.t -> int32 -> proto_plugin tzresult Lwt.t

(** Return the protocol plugin for a given level (or an error if not
    supported). *)
val proto_plugin_for_level_with_store :
  _ Store.t -> int32 -> proto_plugin tzresult Lwt.t

(** Return the protocol plugin for a given block (or an error if not
    supported). *)
val proto_plugin_for_block :
  _ Node_context.t -> Block_hash.t -> proto_plugin tzresult Lwt.t

(** Returns the plugin corresponding to the last protocol seen by the rollup
    node. *)
val last_proto_plugin : _ Node_context.t -> proto_plugin tzresult Lwt.t

(** {2 Safe protocol specific constants}

    These functions provide a way to retrieve constants in a safe manner,
    depending on the context.
*)

(** Retrieve constants for a given level (values are cached). *)
val get_constants_of_level :
  _ Node_context.t ->
  int32 ->
  Rollup_constants.protocol_constants tzresult Lwt.t

(** Retrieve constants for a given block hash (values are cached). *)
val get_constants_of_block_hash :
  _ Node_context.t ->
  Block_hash.t ->
  Rollup_constants.protocol_constants tzresult Lwt.t
