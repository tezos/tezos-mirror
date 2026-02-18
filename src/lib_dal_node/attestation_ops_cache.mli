(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Cache for attestation data fetched from L1.

    This cache stores attestation data per block level to avoid redundant
    RPC calls to [Plugin.get_attestations] for the same level.

    The abstract protocol-plugin-dependent types ([tb_slot],
    [attestation_operation], [dal_attestations]) are projected into concrete
    types at insertion time: [tb_slot] is converted to [int], the
    [dal_attestations] value is captured inside a closure, and
    [attestation_operation] (only needed by the accuser) is dropped. *)

type t

(** A function that checks whether a given slot is attested by a baker.

    The abstract [dal_attestations] value from the protocol plugin is captured
    inside the closure, so consumers do not need access to the plugin to
    evaluate it. *)
type is_attested_fn =
  number_of_slots:int -> number_of_lags:int -> lag_index:int -> int -> bool

(** A cached attestation entry: the Tenderbake slot as [int], and an optional
    attestation-checking closure (present iff the baker included a DAL
    payload). *)
type entry = int * is_attested_fn option

(** The cached attestation data for a level. *)
type cached_ops = entry list

(** [create ~max_size] creates an empty cache. If the cache size exceeds
    [max_size], entries for old levels are removed in FIFO order. *)
val create : max_size:int -> t

(** [find t ~level] returns the cached attestation data at [level], or [None]
    if not cached. *)
val find : t -> level:int32 -> cached_ops option

(** [add t ~level ~attestation_ops] caches [attestation_ops] for [level].
    If an entry for [level] already exists, it is replaced. *)
val add : t -> level:int32 -> attestation_ops:cached_ops -> unit
