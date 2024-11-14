(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

module Dal_proto_services = Dal_services
open Protocol

module Skip_list_handlers = struct
  let cell (rpc_context, cell_hash) () () =
    let open Lwt_result_syntax in
    let hash =
      Dal_proto_types.Skip_list_hash.of_proto
        Alpha_context.Dal.Slots_history.Pointer_hash.encoding
        cell_hash
    in
    let* cell = Skip_list_cells_store.find rpc_context hash in
    return
    @@ Dal_proto_types.Skip_list_cell.to_proto
         Alpha_context.Dal.Slots_history.encoding
         cell
end

let add_service registerer subst service handler directory =
  registerer directory (subst service) handler

let register_commitments_history ctxt directory =
  directory
  |> add_service
       Tezos_rpc.Directory.register
       Tezos_rpc.Service.subst1
       Dal_proto_services.Commitments_history.hash_content
       Skip_list_handlers.cell
  |> Tezos_rpc.Directory.map (fun _prefix -> Lwt.return ctxt)

let directory (rpc_ctxt : Skip_list_cells_store.t) =
  register_commitments_history rpc_ctxt Tezos_rpc.Directory.empty
