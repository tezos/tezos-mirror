(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Error code extracted from the WASM PVM library. *)

type t = private int

val store_key_too_large : t

val store_invalid_key : t

val store_not_a_value : t

val store_invalid_access : t

val store_value_size_exceeded : t

val memory_invalid_access : t

val input_output_too_large : t

val generic_invalid_access : t

val store_readonly_value : t

val store_not_a_node : t

val full_outbox : t

val store_invalid_subkey_index : t

val store_value_already_exists : t
