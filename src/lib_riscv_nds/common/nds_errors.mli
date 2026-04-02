(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Common error types shared by both the in-memory and on-disk backends.

    Each variant is registered once with the error monad so that callers
    can match on a single constructor regardless of the backend. *)

type invalid_argument_error =
  | Key_not_found
  | Key_too_long
  | Offset_too_large
  | Database_index_out_of_bounds
  | Registry_resize_too_large

type error += Invalid_argument of invalid_argument_error

type error += Proof_deserialisation_error of string

type verification_error = Not_found

type error += Verification_error of verification_error

type verification_argument_error =
  | Verification_invalid_argument of invalid_argument_error
  | Verification of verification_error

type error += Verification_argument_error of verification_argument_error
