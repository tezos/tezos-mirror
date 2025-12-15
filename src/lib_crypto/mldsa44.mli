(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos - ML-DSA-44 cryptography *)

include S.SIGNATURE with type watermark = Bytes.t

include S.RAW_DATA with type t := t
