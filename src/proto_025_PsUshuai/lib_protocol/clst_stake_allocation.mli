(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** CLST stake allocation at cycle boundaries.

    Computes per-delegate allocation amounts from the [CLST_deposits],
    up to a per-delegate cap derived from staking parameters. Allocations
    are pure accounting (no tez movement): the amounts are tracked in the
    [stez_frozen] field of each delegate's staking balance, used by the
    baking-rights computation but the tez remains in the deposits so that
    stakers can always redeem.

    Allocations are tracked in direct tez (no pseudotokens), eliminating
    rounding issues. *)

(** [rebalance_at_cycle_end ctxt ~new_cycle] recomputes CLST stake allocations
    for all registered delegates.  Called at cycle end.

    1. Clear all previous allocations.

    2. Greedily allocate from [CLST_deposits] all registered delegates in
    public-key-hash order. *)
val rebalance_at_cycle_end :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

module For_RPC : sig
  (** [allocated_rights_of_delegate ctxt delegate] returns the tez amount
    allocated to [delegate] from the sTez deposits for baking rights (i.e. the
    [stez_frozen] field of the delegate's staking balance).
    Returns [Tez_repr.zero] if no sTez stake is allocated to the delegate. *)
  val allocated_rights_of_delegate :
    Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

  (** [total_allocated_rights ctxt] returns the total tez allocated across all
    registered delegates from the sTez deposits for baking rights. *)
  val total_allocated_rights : Raw_context.t -> Tez_repr.t tzresult Lwt.t
end
