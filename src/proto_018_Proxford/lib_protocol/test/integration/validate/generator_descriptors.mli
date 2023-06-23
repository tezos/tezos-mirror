(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Validate_helpers

(** {2 Generation state} *)

(** The state to generate valid double pre- and endorsement evidence
   contains a temporary state for making the slashable evidence, and
   the lists of slashables operations, one for each kind:
   preendorsement and endorsement. *)
type dbl_endorsement_state = {
  temporary : (Block.t * Block.t) option;
  slashable_preend :
    (Kind.preendorsement operation * Kind.preendorsement operation) list;
  slashable_end : (Kind.endorsement operation * Kind.endorsement operation) list;
}

(** Generic generation state collecting
    information to generate any kind of operation.

    For example, {!Manager.infos} for manager
    or voters (Contract.t list) for voting operations...

   When adding a new operation kind, [state] might be extended if a
   new kind of information is required for this new kind valid
   operations generation. *)
type state = {
  block : Block.t;
  pred : Block.t option;
  bootstraps : public_key_hash list;
  delegates : (public_key_hash * public_key_hash option) list;
  voters : Contract.t list;
  seed_nonce_to_reveal : (Raw_level.t * Nonce_hash.t) list;
  commitments : secret_account list;
  protocol_hashes : Protocol_hash.t list;
  slashable_bakes : (block_header * block_header) list;
  vdf : bool;
  dbl_endorsement : dbl_endorsement_state;
  manager : Manager.infos;
}

(** The initialization of a [state] requires the [voters] contracts --
    the contracts allowed to vote -- and the [bootstraps] contracts. *)
val init_state :
  Block.t -> voters:Contract.t list -> bootstraps:Contract.t list -> state

(** {2 Descriptor for valid operations generation} *)

(** Each prelude action either takes place on a specific cycle or
    from a specific cycle to the end a the context setting. *)
type cycle_index = On of int | From of int

(** Descriptors are specific to operation kinds, [op_kind]. A
    descriptor provides the information and functions used in the
    context setup to generate valid operations of its kind and a
    generator for such operations.

   - [parameters] enables setting constants in the initial context.

   - [required_cycle] is the number of cycles in the context setup
   before generating valid operations of this kind.

   - [required_block] the number of blocks in the last cycle.

   - [prelude] is a set of actions that either gather information in
   the setup [state] or perform operations in the setup blocks or both
   that have to be performed according to a [cycle_index].

   - [opt_prelude] is an optional prelude.

   - [candidates_generator] generates operations of the descriptor
   [op_kind] according to the information in [state] that are valid
   upon [state.block]. *)
type descriptor = {
  parameters : Parameters.t -> Parameters.t;
  required_cycle : Parameters.t -> int;
  required_block : Parameters.t -> int;
  prelude :
    cycle_index * (state -> (packed_operation list * state) tzresult Lwt.t);
  opt_prelude :
    (cycle_index * (state -> (packed_operation list * state) tzresult Lwt.t))
    option;
  candidates_generator : state -> packed_operation list tzresult Lwt.t;
}

(** {2 Operation kinds} *)

(** When adding a new operation:
  - a new op_kind [k] should extend the [op_kind] type,
  - a [descriptor] defined,
  - [descriptor_of] must associate this new descriptor to [k],
  - [k] must be added to [all_kinds],
  - If the validity of [k] operations is not exclusive with the
  validity of other [op_kind], [k] must be added to
  [non_exclusive_kinds]. Otherwise, see, for example, how voting
  operation op_kinds are handled in {! test_covalidity.tests}. *)
type op_kind =
  | KEndorsement
  | KPreendorsement
  | KDalattestation
  | KBallotExp
  | KBallotProm
  | KProposals
  | KNonce
  | KVdf
  | KActivate
  | KDbl_consensus
  | KDbl_baking
  | KDrain
  | KManager

val pp_op_kind : Format.formatter -> op_kind -> unit

(** This sanity function returns the [op_kind] associated to
    an [packed_operation].*)
val op_kind_of_packed_operation : packed_operation -> op_kind

(** Associate to each [op_kind] a [descriptor]. Some descriptors are
    parametrized by the number of bootstraps and the maximum size of a
    batch.*)
val descriptor_of :
  nb_bootstrap:int -> max_batch_size:int -> op_kind -> descriptor

(** Given a list of [op_kind] returns the list of corresponding
    descriptors as provided by [descriptor_of] for each [op_kind].
    Some descriptors are parametrized by the number of bootstraps and
    the maximum size of a batch.*)
val descriptors_of :
  nb_bootstrap:int -> max_batch_size:int -> op_kind list -> descriptor list

(** List of all [op_kind] that are non exclusive (i.e. no voting
operation kind or nonce revelation kind) *)
val non_exclusive_kinds : op_kind trace

(** List of all [op_kind] used for sanity check. *)
val all_kinds : op_kind list
