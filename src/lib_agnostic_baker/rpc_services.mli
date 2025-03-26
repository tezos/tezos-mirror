(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [request_uri ~node_addr ~uri] issues an HTTP [GET] request to the [~uri] and
    returns the response and its body. In case the connection is refused,
    it fails with a connection error on [~node_addr]. *)
val request_uri :
  node_addr:string ->
  uri:string ->
  (Cohttp_lwt_unix.Response.t * Cohttp_lwt.Body.t) tzresult Lwt.t

(** [get_level ~node_addr] retrieves the current block level from the node at
    [~node_addr]. *)
val get_level : node_addr:string -> (int, error trace) result Lwt.t

(** [get_block_hash ~node_addr] retrieves the hash of the current block from the node
    at [~node_addr]. *)
val get_block_hash :
  node_addr:string -> (Block_hash.t, error trace) result Lwt.t

(** [get_next_protocol_hash ~node_addr] retrieves the protocol hash from the [next_protocol]
    field in the metadata of the current block, as seen by the node at [~node_addr]. *)
val get_next_protocol_hash : node_addr:string -> Protocol_hash.t tzresult Lwt.t

(** [get_current_period ~node_addr] retrieves the current voting period information
    from the node at [~node_addr]. It returns a pair [(kind, remaining)] where:
    - [kind] is a string representing the voting period kind, and
    - [remaining] is the number of blocks remaining until the end of the voting period. *)

val get_current_period : node_addr:string -> (string * int) tzresult Lwt.t
