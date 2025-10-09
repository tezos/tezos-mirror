(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Types declaration for native contracts. *)

type z_n = Script_int.z Script_int.num

module Accumulator_types = struct
  type arg = z_n

  type storage = z_n * z_n
end

type ('arg, 'storage) kind =
  | Accumulator_kind : (Accumulator_types.arg, Accumulator_types.storage) kind
