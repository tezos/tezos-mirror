(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type cryptobox_with_precomputation

type t

type error += No_cryptobox

(** [init_cryptoboxes config param profile ~level] returns the new
    proto_cryptoboxes state in which the cryptobox for the given ~level has been
    initialized. *)
val init :
  Configuration_file.t ->
  Types.proto_parameters ->
  Profile_manager.t ->
  level:int32 ->
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
