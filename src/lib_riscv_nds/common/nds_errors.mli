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
  | Io_request_too_large
  | Offset_too_large
  | Value_size_too_large
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

(** Raised by Verify-mode backends when a host-function operation
    diverges from what the proof recorded.  Consumers (notably the
    WASM PVM's [verify_proof]) catch this at the boundary of the
    kernel-step replay via [Lwt.catch], turning a mid-stream
    proof-mismatch into a clean rejection.

    Lives in the common layer so both memory and disk backends raise
    a single, uniformly-catchable exception. *)
exception Verification_failed of verification_error
