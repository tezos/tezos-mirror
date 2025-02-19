(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Agnostic_baker_plugin = struct
  let hash = Registerer.Registered.hash
end

let () =
  Protocol_plugin.register_agnostic_baker_plugin (module Agnostic_baker_plugin)
