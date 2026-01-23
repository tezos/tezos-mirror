(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
(** Events related to the publication of signals. *)

val section : string list

(** [publisher_is_ready ()] advertises that the worker is ready to
    accept requests. *)
val publisher_is_ready : unit -> unit Lwt.t

(** [publisher_shutdown ()] advertises that the worker has been
    shutdown and will not accept requests anymore. *)
val publisher_shutdown : unit -> unit Lwt.t

(** [untracking ~injector_op_hash] advertizes that the DAL slot
    publication associated with the injection id [injector_op_hash] is
    not longer tracked by the signal publisher. *)
val untracking :
  injector_op_hash:Tezos_crypto.Hashed.Injector_operations_hash.t -> unit Lwt.t

(** [commited_or_included_injection_id ~injector_op_hash
    ~publish_level] advertizes that injection id [injector_op_hash]
    has been reported commited or included, and finalized, by the
    rollup node and published at L1 level [publish_level]. *)
val commited_or_included_injection_id :
  injector_op_hash:Tezos_crypto.Hashed.Injector_operations_hash.t ->
  published_level:int32 ->
  unit Lwt.t

(** [signal_signed ~signals ~smart_rollup_address] advertizes that
    some [signals] have been signed for the smart rollup address
    [smart_rollup_address]. Here signals are lists of pairs, where
    each pair consists of a DAL slot index and the level at which its
    injection was published. *)
val signal_signed :
  signals:(int * int32) list ->
  smart_rollup_address:Tezos_crypto.Hashed.Smart_rollup_address.t ->
  unit Lwt.t

(** [report_ready_and_statuses ~operations ~ready_opertions]
    advertizes the number of operations ready to be published among
    the currrently injected DAL operations. *)
val report_ready_operations :
  operations:int -> ready_operations:int -> unit Lwt.t
