(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let run ~cctxt ~preserved_levels ~keep_alive =
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  Client_daemon.Accuser.run
    cctxt
    ~chain:cctxt#chain
    ~preserved_levels
    ~keep_alive
