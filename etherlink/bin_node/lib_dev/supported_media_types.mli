(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** List all the media types supported by the EVM node (including
    application/json with variants mentioning the UTF-8 charset). *)
val all : Media_type.t list
