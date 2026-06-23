(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(** Smart rollup node plugin for the WASM 2.0.0 PVM. *)

(** Durable part of the storage of this PVM. *)
module type Durable_state = sig
  type state

  (** [value_length state key] returns the length of data stored
        for the [key] in the durable storage of the PVM state [state], if any. *)
  val value_length : state -> string -> int64 option Lwt.t

  (** [lookup state key] returns the data stored
        for the [key] in the durable storage of the PVM state [state], if any. *)
  val lookup : state -> string -> bytes option Lwt.t

  (** [list state key] returns subtrees
        for the [key] in the durable storage of the PVM state [state].
        Empty list in case if path doesn't exist. *)
  val list : state -> string -> string list Lwt.t

  module Tree_encoding_runner :
    Tezos_tree_encoding.Runner.S with type tree = state
end

(** Durable storage access for the on-disk (single Irmin) PVM state. *)
module Durable_state : Durable_state with type state = Irmin_context.tree

include Pvm_sig.S
