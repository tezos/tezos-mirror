(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** The [encoded_value_size] required by {!Key_value_store.layout} is known when
    the daemon loads a protocol, after the store is initialized. We use the
    closures like [set_share_size] to perform delayed protocol-specific
    parameter passing.

    Note that this mechanism is not sufficient to make the key-value store
    robust to dynamic changes in [encoded_value_size]. For instance, there could
    be concurrent writes for protocol P-1 and protocol P, if they define
    distinct [encoded_value_size] this will make it so that [P-1] uses the
    [encoded_value_size] of [P].

    A potential solution would have a function [Cryptobox.share_encoding : t ->
    share encoding] with the property that the produced encodings are of
    [`Fixed] class.  The {!Key_value_store.t} type could be parameterized by an
    extra type parameter corresponding to some dynamic state (corresponding to
    the cryptobox in our use case), passed explicitly to the [write] and [read]
    functions.

    Correcting this is left to future work.

    TODO: https://gitlab.com/tezos/tezos/-/issues/6034 *)

(** [set_share_size size] sets the size of shard shares. It fails if the size
    was already set to a different value. This function must be called once,
    before the [layout] function of the KVS instance is used. *)
val set_share_size : int -> unit

(** Get the share size. It fails if {!set_share_size} has not been called
    before. *)
val share_size : unit -> int
