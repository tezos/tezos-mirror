(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Synthesis of a Michelson [script] for native contracts.

    Native contracts have no on-chain Michelson code, so the contract [script]
    RPC used to return nothing for them. This module synthesizes, from a native
    contract's typed interface, a Michelson script that exposes the contract's
    real entrypoints and the real input/output types of its views, with
    non-functional placeholder bodies. This lets standard contract tooling
    discover and validate a native contract's interface as if it were an
    ordinary contract. *)

open Environment.Error_monad
open Protocol
open Protocol.Alpha_context

(** Raised when a native contract's typed interface cannot be derived, so no
    script can be synthesized. The underlying cause is kept in the error
    trace. *)
type error += Native_script_synthesis_failed of {kind : Script_native_repr.t}

(** [of_native ctxt kind ~storage] synthesizes a Michelson [michelson_with_storage]
    for the native contract [kind].

    The synthesized code exposes:
    - the contract's real parameter type, carrying the real entrypoint
      annotations;
    - the contract's real declared storage type (coherent with [storage]);
    - the contract's real views, each with its real input and output types.

    The contract code and every view body are non-functional [FAILWITH] stubs:
    the result is well-typed (it type-checks) but reproduces none of the native
    contract's behavior. [storage] is returned unchanged as the script storage.

    @return {!Native_script_synthesis_failed} (with the underlying cause traced)
    if the typed interface cannot be derived or unparsed. *)
val of_native :
  context ->
  Script_native_repr.t ->
  storage:Script.lazy_expr ->
  Script.michelson_with_storage tzresult
