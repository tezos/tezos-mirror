(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Context : Tezos_tree_encoding.Encodings_util.Bare_tezos_context_sig

type index

type store

type evm_state = Context.tree

type t = {
  data_dir : string;  (** Data dir of the EVM node. *)
  index : index;  (** Irmin index. *)
  store : store;  (** Irmin store. *)
  evm_state : evm_state;  (** EVM local state of the sequencer. *)
  kernel : string;  (** Path to the kernel to execute. *)
  preimages : string;  (** Path to the preimages directory. *)
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
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
