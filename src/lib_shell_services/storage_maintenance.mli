(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Storage maintenance configuration.

    Storage maintenance aims to configure the internals of the storage
    maintenance procedure that aims to be run on regular basis.

 *)

(** The type [delay] specifies whether or not a storage maintenance
    should be delayed or not.
    Setting it to [Disabled] will trigger the storage maintenance as
    soon as possible, that is, at the very beginning of a new cycle
    dawn.
    [Custom n] will trigger the storage maintenance n blocks after a
     new cycle dawn.
    [Auto] will trigger the storage maintenance after a delay that is
    determined automatically. For now, we enforce:
     - an exclusion period: the early beginning of the cycle is not
       subject to storage maintenance -- it avoids triggering the
       maintenance shortly after a protocol activation or when the
       payouts are injected into the network
     - a trigger limit: the storage maintenance cannot be triggered
       after the second half of a cycle -- it avoids overlapping with
       the next cycle to come and increase too much the storage size
    For a given blocks_per_cycle, we have:
     - an exclusion period of 1/20th blocks_per_cycles
     - a trigger limit of 1/2 blocks_per_cycles
*)
type delay = Disabled | Custom of Int32.t | Auto

val delay_encoding : delay Data_encoding.t

val pp_delay : Format.formatter -> delay -> unit

(** [default_auto_delay ~blocks_per_cycle] returns a random number
    between [blocks_per_cycle / 20] and [blocks_per_cycle / 2], in
    line with the [delay]'s description. *)
val default_auto_delay : blocks_per_cycle:Int32.t -> Int32.t
