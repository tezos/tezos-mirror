(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open TzCore

(** Extension of the open type [error] with the errors that could be raised by
    the DAL node. *)
type error +=
  | Download_status of {status : Cohttp.Code.status_code}
  | Mismatched_SHA of {expected_sha : Hex.t; computed_sha : Hex.t}
