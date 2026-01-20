(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Run-time errors of the {!Script_interpreter}.

    These errors are defined separately so that they may be used in
    {!Script_native}, which is itself a dependency for
    {!Script_interpreter}. *)

open Alpha_context
open Script_typed_ir

type error +=
  | Reject of Script.location * Script.expr * execution_trace option
  | Overflow of Script.location * execution_trace option
  | Runtime_contract_error of Contract_hash.t
  | Bad_contract_parameter of Contract.t (* `Permanent *)
  | Cannot_serialize_failure
  | Cannot_serialize_storage
  | Michelson_too_many_recursive_calls
