(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type cryptobox_with_precomputation

type t

type error += No_cryptobox | Cannot_register_shard_layout of {msg : string}

(** [init_~cctxt ~header ~config ~current_head_proto_parameters
    ~first_seen_level profile plugins] returns the new proto_cryptoboxes state
    in which the cryptoboxes for each protocol known by the associated L1 node
    are initialized. *)
val init :
  cctxt:Rpc_context.t ->
  header:Block_header.t ->
  config:Configuration_file.t ->
  first_seen_level:int32 ->
  Profile_manager.t ->
  Proto_plugins.t ->
  t tzresult Lwt.t

(** [add param ~level t] adds a new proto_cryptobox which becomes registered at
    [level] and is expected to be used starting at this level. *)
val add : Types.proto_parameters -> level:int32 -> t -> t tzresult Lwt.t

(** [get_for_level ~level t] returns the cryptobox that is associated to the
    given [level]. *)
val get_for_level :
  level:int32 ->
  t ->
  (Cryptobox.t * Cryptobox.shards_proofs_precomputation option) tzresult

(** [get_latest t] returns the cryptobox registered for the highest block, that
    is expected to be used by the latest block. *)
val get_latest :
  t -> (Cryptobox.t * Cryptobox.shards_proofs_precomputation option) tzresult
