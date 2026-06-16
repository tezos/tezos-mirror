(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type constants = {target : Z.t; alpha : float}

(** Returns the gas constants depending on the storage version set by the
    kernel. *)
val gas_constants : storage_version:int -> constants

(** [price_from_backlog ~version ~minimum backlog] mimics the computation
    performed by the kernel in order to compute the base fee per gas for the
    next block.

    See the [gas_price] Rust module in [kernel_latest/kernel].

    [version] is used to decide what is the unit of backlog. If it is recent
    enough, the backlog is interpreted as a gas quantity. Otherwise, it
    fallbacks to the initial behavior of the kernel where the backlog was
    counted in estimated ticks. *)
val price_from_backlog : version:int -> minimum:Z.t -> Z.t -> Z.t
