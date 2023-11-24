(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [get t witness_hash] retrieves the messages for the merkelized
    payloads hash [witness_hash] stored by the rollup node. *)
val get :
  Node_context.rw ->
  Merkelized_payload_hashes_hash.t ->
  string list tzresult Lwt.t

(** Same as {!get} but returns [None] if the payloads hash is not known. *)
val find :
  Node_context.rw ->
  Merkelized_payload_hashes_hash.t ->
  string list option tzresult Lwt.t
