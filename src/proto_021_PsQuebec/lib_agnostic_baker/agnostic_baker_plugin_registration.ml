(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Agnostic_baker_plugin = struct
  let hash = Registerer.Registered.hash

  let name = Protocol.name

  let map_commands () =
    List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
    @@ Baking_commands.baker_commands ()

  let register_commands () =
    Client_commands.register Protocol.hash @@ fun _network -> map_commands ()

  let select_commands _ _ = Lwt_result_syntax.return @@ map_commands ()
end

let () =
  Protocol_plugin.register_agnostic_baker_plugin (module Agnostic_baker_plugin)
