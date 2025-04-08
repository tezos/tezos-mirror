(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type level = {
  level : int32;
      (** The level of the block relative to genesis. This
      is also the Shell's notion of level. *)
  cycle : int32;
      (** The current cycle's number. Note that cycles are a protocol-specific
      notion. As a result, the cycle number starts at 0 with the first block of
      the first version of protocol alpha. *)
  cycle_position : int32;
      (** The current level of the block relative to the first block of the current
      cycle. *)
}
