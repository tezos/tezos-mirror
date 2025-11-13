(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

module Accumulator_contract = struct
  open Script_native_types.Accumulator_types

  let execute (ctxt, _) (value : arg) (acc, count) =
    Lwt.return_ok
      ((Script_list.empty, Script_int.(add value acc, add one count)), ctxt)
end

let execute (type arg storage) (ctxt, step_constants)
    (kind : (arg, storage) kind) (arg : arg) (storage : storage) :
    ((operation Script_list.t, storage) pair * context, error trace) result
    Lwt.t =
  match kind with
  | Accumulator_kind ->
      Accumulator_contract.execute (ctxt, step_constants) arg storage
