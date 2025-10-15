(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let find ctxt address =
  Storage.Contract.Address_registry.Registry.find ctxt address

type add_result = {ctxt : Raw_context.t; index : Z.t; existed : bool}

let add_if_missing ctxt address =
  let open Lwt_result_syntax in
  let* ctxt, index_opt = find ctxt address in
  match index_opt with
  | None ->
      let* index = Storage.Contract.Address_registry.Next.get ctxt in
      let* ctxt =
        Storage.Contract.Address_registry.Next.update ctxt (Z.succ index)
      in
      let* ctxt, _, _ =
        Storage.Contract.Address_registry.Registry.add ctxt address index
      in
      return {ctxt; index; existed = false}
  | Some index -> return {ctxt; index; existed = true}

let init ctxt =
  let open Lwt_result_syntax in
  let* ctxt = Storage.Contract.Address_registry.Next.init ctxt Z.zero in
  let* {ctxt; index = _; existed = _} =
    add_if_missing ctxt (Destination_repr.Contract Contract_repr.zero)
  in
  return ctxt

module Internal_for_tests = struct
  let set_counter ctxt index =
    Storage.Contract.Address_registry.Next.update ctxt index

  let get_counter ctxt = Storage.Contract.Address_registry.Next.get ctxt
end
