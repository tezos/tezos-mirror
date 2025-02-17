(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Agnostic_baker_plugin = struct
  let hash = Registerer.Registered.hash

  let name = Protocol.name

  let register_commands () =
    Client_commands.register Protocol.hash @@ fun _network ->
    List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
    @@ Baking_commands.baker_commands ()

  let select_commands _ _ =
    let open Lwt_result_syntax in
    return
      (List.map
         (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
         (Baking_commands.baker_commands ()))
end

let () =
  Protocol_plugin.register_agnostic_baker_plugin (module Agnostic_baker_plugin)
