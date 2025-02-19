(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Protocol

let constants_key = [Constants_repr.version; "constants"]

let get_constants ctxt =
  let open Lwt_syntax in
  let* bytes_opt = Tezos_protocol_environment.Context.find ctxt constants_key in
  match bytes_opt with
  | Some bytes ->
      return
      @@ Data_encoding.Binary.of_bytes_opt
           Constants_parametric_repr.encoding
           bytes
  | None -> return_none

let get_blocks_per_cycle (ctxt : Tezos_protocol_environment.Context.t) =
  let open Lwt_option_syntax in
  let* {blocks_per_cycle; _} = get_constants ctxt in
  Lwt.return_some blocks_per_cycle
