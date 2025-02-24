(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Agnostic_baker_plugin = struct
  let hash = Registerer.Registered.hash

  let map_commands () =
    List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
    @@ Baking_commands.baker_commands ()
end

let () =
  Protocol_plugin.register_agnostic_baker_plugin (module Agnostic_baker_plugin)
