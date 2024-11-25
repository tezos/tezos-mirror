(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = int

let store_key_too_large = -1

let store_invalid_key = -2

let store_not_a_value = -3

let store_invalid_access = -4

let store_value_size_exceeded = -5

let memory_invalid_access = -6

let input_output_too_large = -7

let generic_invalid_access = -8

let store_readonly_value = -9

let store_not_a_node = -10

let full_outbox = -11

let store_invalid_subkey_index = -12

let store_value_already_exists = -13
