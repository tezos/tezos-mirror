(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type t = {
  hash : Block_hash.t;
  header : Block_header.t;
  operations : Operation.packed list;
  context : Tezos_protocol_environment.Context.t;  (** Resulting context *)
}

type block = t

val rpc_ctxt : t Environment.RPC_context.simple

(** Policies to select the next baker:
    - [By_priority p] selects the baker at priority [p]
    - [By_account pkh] selects the first slot for baker [pkh]
    - [Excluding pkhs] selects the first baker that doesn't belong to [pkhs]
*)
type baker_policy =
  | By_priority of int
  | By_account of public_key_hash
  | Excluding of public_key_hash list

(** Returns (account, priority, timestamp) of the next baker given
    a policy, defaults to By_priority 0. *)
val get_next_baker :
  ?policy:baker_policy ->
  t ->
  (public_key_hash * int * Time.Protocol.t) tzresult Lwt.t

val get_endorsing_power : block -> int tzresult Lwt.t

module Forge : sig
  val contents :
    ?proof_of_work_nonce:Bytes.t ->
    ?priority:int ->
    ?seed_nonce_hash:Nonce_hash.t ->
    unit ->
    Block_header.contents

  type header

  (** Forges a correct header following the policy.
      The header can then be modified and applied with [apply]. *)
  val forge_header :
    ?policy:baker_policy ->
    ?timestamp:Timestamp.time ->
    ?operations:Operation.packed list ->
    t ->
    header tzresult Lwt.t

  (** Sets uniquely seed_nonce_hash of a header *)
  val set_seed_nonce_hash : Nonce_hash.t option -> header -> header

  (** Sets the baker that will sign the header to an arbitrary pkh *)
  val set_baker : public_key_hash -> header -> header

  (** Signs the header with the key of the baker configured in the header.
      The header can no longer be modified, only applied. *)
  val sign_header : header -> Block_header.block_header tzresult Lwt.t
end

val check_constants_consistency : Constants.parametric -> unit tzresult Lwt.t

(** [genesis <opts> accounts] : generates an initial block with the
    given constants [<opts>] and initializes [accounts] with their
    associated amounts.
*)
val genesis :
  ?with_commitments:bool ->
  ?endorsers_per_block:int ->
  ?initial_endorsers:int ->
  ?min_proposal_quorum:int32 ->
  ?time_between_blocks:Period.t list ->
  ?minimal_block_delay:Period.t ->
  ?delay_per_missing_endorsement:Period.t ->
  (Account.t * Tez.tez) list ->
  block tzresult Lwt.t

val genesis_with_parameters : Parameters.t -> block tzresult Lwt.t

(** Applies a signed header and its operations to a block and
    obtains a new block *)
val apply :
  Block_header.block_header ->
  ?operations:Operation.packed list ->
  t ->
  t tzresult Lwt.t

(**
   [bake b] returns a block [b'] which has as predecessor block [b].
   Optional parameter [policy] allows to pick the next baker in several ways.
   This function bundles together [forge_header], [sign_header] and [apply].
   These functions should be used instead of bake to craft unusual blocks for
   testing together with setters for properties of the headers.
   For examples see seed.ml or double_baking.ml
*)
val bake :
  ?policy:baker_policy ->
  ?timestamp:Timestamp.time ->
  ?operation:Operation.packed ->
  ?operations:Operation.packed list ->
  t ->
  t tzresult Lwt.t

(** Bakes [n] blocks. *)
val bake_n : ?policy:baker_policy -> int -> t -> block tzresult Lwt.t

val current_cycle : t -> Cycle.t tzresult Lwt.t

(** Given a block [b] at level [l] bakes enough blocks to complete a cycle,
    that is [blocks_per_cycle - (l % blocks_per_cycle)]. *)
val bake_until_cycle_end : ?policy:baker_policy -> t -> t tzresult Lwt.t

(** Bakes enough blocks to end [n] cycles. *)
val bake_until_n_cycle_end :
  ?policy:baker_policy -> int -> t -> t tzresult Lwt.t

(** Bakes enough blocks to reach the cycle. *)
val bake_until_cycle : ?policy:baker_policy -> Cycle.t -> t -> t tzresult Lwt.t
