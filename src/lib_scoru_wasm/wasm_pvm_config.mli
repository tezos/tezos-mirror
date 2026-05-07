(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Internal WASM PVM configuration.

    The protocol environment exposes a stringly-typed config so that
    adding a new feature does not require an environment-signature
    bump. This module is the typed counterpart used inside
    [lib_scoru_wasm]: the env implementation calls {!of_signals} to
    convert the opaque list into a [t], which is then threaded into
    the PVM machine via the [Wasm_vm.Make_vm] functor. *)

(** Features the protocol can enable via canonical rollup signals. *)
type feature =
  | Nds_host_functions
      (** Expose the new-durable-storage host functions to kernels. The
          canonical signal name is [nds_host_functions]. *)

module Feature_map : Map.S with type key = feature

type t = {features : int32 Feature_map.t}

(** Configuration with no features enabled. *)
val empty : t

(** [is_enabled t f ~current_level] is [true] iff [f] is present in
    [t.features] and [current_level] is strictly greater than its
    activation level. A feature is active for levels beyond the one
    at which the signal was recorded, not for the recording level
    itself. *)
val is_enabled : t -> feature -> current_level:int32 -> bool

(** [activation_level t f] is the L1 level at which [f] was activated,
    or [None] if [f] is not enabled in [t]. *)
val activation_level : t -> feature -> int32 option

(** Structural equality on configurations. *)
val equal : t -> t -> bool

(** [feature_of_signal_name s] returns the feature associated with the
    canonical-rollup signal name [s], or [None] if no feature matches.

    Unknown signal names are deliberately not errors: this is what
    keeps the env-signature stable across feature additions. *)
val feature_of_signal_name : string -> feature option

(** [signal_name_of_feature f] is the canonical string identifier used
    in outbox signals to enable [f]. Inverse of {!feature_of_signal_name}
    on known features. *)
val signal_name_of_feature : feature -> string

(** [of_signals signals] builds a typed config from the env-sig
    [(signal_name, activation_level)] list. Entries whose name has no
    matching feature are dropped silently. *)
val of_signals : (string * int32) list -> t

(** [nds_host_functions_enabled t ~current_level] is a shortcut for
    [is_enabled t Nds_host_functions ~current_level]. It's the
    single question the WASM linker asks to decide whether to expose
    the NDS host functions to the kernel at link time. *)
val nds_host_functions_enabled : t -> current_level:int32 -> bool
