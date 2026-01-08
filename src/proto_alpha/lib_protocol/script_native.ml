(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_native_types
open Script_typed_ir

module CLST_contract = struct
  open Script_native_types.CLST_types

  let execute (ctxt, _) (_value : arg) (storage : storage) =
    Lwt_result_syntax.return ((Script_list.empty, storage), ctxt)
end

let execute (type arg storage) (ctxt, step_constants)
    (kind : (arg, storage) kind) (arg : arg) (storage : storage) :
    ((operation Script_list.t, storage) pair * context, error trace) result
    Lwt.t =
  match kind with
  | CLST_kind -> CLST_contract.execute (ctxt, step_constants) arg storage
