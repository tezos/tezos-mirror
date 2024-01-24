(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Bare_context :
  Tezos_tree_encoding.Encodings_util.Bare_tezos_context_sig
    with type index = Irmin_context.rw_index
     and type t = Irmin_context.rw
     and type tree = Irmin_context.tree

type evm_state = Irmin_context.PVMState.value

type t = {
  data_dir : string;  (** Data dir of the EVM node. *)
  context : Irmin_context.rw;  (** Irmin read and write context. *)
  kernel : string;  (** Path to the kernel to execute. *)
  preimages : string;  (** Path to the preimages directory. *)
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  mutable next_blueprint_number : Ethereum_types.quantity;
      (** Number for the next bluerpint to be produced. *)
}

(** [init ~data_dir ~kernel ~preimages ~smart_rollup_address] creates
    a context where it initializes the {!type-index}, and use a
    checkpoint mechanism to load the latest {!type-store} if any.

    Also returns a boolean denoting whether the context was initialized or not.
*)
val init :
  data_dir:string ->
  kernel:string ->
  preimages:string ->
  smart_rollup_address:string ->
  (t * bool) tzresult Lwt.t

(** [commit ctxt evm_state] updates the [evm_state] in [ctxt], commits
    to disk the changes, and update the checkpoint. *)
val commit : t -> evm_state -> t tzresult Lwt.t

(** [sync ctxt] synchronizes the [ctxt] based on on-disk information, loads the
    latest checkpoint. *)
val sync : t -> t tzresult Lwt.t

val evm_state : t -> evm_state Lwt.t
