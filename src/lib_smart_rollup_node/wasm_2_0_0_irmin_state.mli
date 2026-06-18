(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(** Irmin-backed state and proof format of the WASM 2.0.0 PVM: the
    tree-encoding state representation paired with the Irmin Merkle proof
    format, packaged as the [STATE_PROOF] the WASM PVM machine functor
    consumes.

    The PVM state is an {!Irmin_context.tree} and the proof context is the
    write-permitted {!Irmin_context.rw_index} (proof production commits to the
    index). Durable storage is decoded through the [Encoding_runner] carried by
    {!Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF}. *)

include
  Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF
    with type state = Irmin_context.tree
     and type context = Irmin_context.rw_index

module Wrapped_tree :
  Tezos_tree_encoding.TREE with type tree = Irmin_context.tree

(** Durable-storage reads over an Irmin PVM [state] tree.

    Each call locates the durable-storage subtree of [state] and serves the
    durable key parsed from its [string] argument. Locating the subtree is
    lazy — values are materialised on demand — so reading several keys with
    separate calls does not re-read the whole storage. *)
module Storage : sig
  (** [find_value state key] is the raw bytes stored at [key], if any. *)
  val find_value : state -> string -> bytes option Lwt.t

  (** [value_length state key] is the byte length of the value at [key], if
      any, without materialising it as [bytes]. *)
  val value_length : state -> string -> int64 option Lwt.t

  (** [list state key] is the immediate subkeys under [key]. *)
  val list : state -> string -> string list Lwt.t
end
