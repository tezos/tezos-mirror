(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val make : data_dir:string -> t

(** [store s ~kind blueprint number] saves the [blueprint] of height [number]
    in the store [s].

    The [kind] is used to differentiate between blueprints to publish and
    to execute. See {!Sequencer_blueprint} for further details.
    *)
val store :
  t ->
  kind:[< `Execute | `Publish] ->
  Blueprint_types.payload ->
  Ethereum_types.quantity ->
  unit tzresult Lwt.t

(** [find s ~kind number] tries to fetch the blueprint of height [number] in
    the store [s], and returns [None] if it does not exist.

    The [kind] is used to differentiate between blueprints to publish and
    to execute. See {!Sequencer_blueprint} for further details.
*)
val find :
  t ->
  kind:[< `Execute | `Publish] ->
  Ethereum_types.quantity ->
  Blueprint_types.payload option Lwt.t
