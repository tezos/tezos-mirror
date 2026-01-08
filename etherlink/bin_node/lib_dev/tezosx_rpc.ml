(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let add_rpc_directory (module Backend : Services_backend_sig.S) ~l2_chain_id dir
    = function
  | Tezosx.Tezos ->
      Tezos_rpc.Directory.merge
        (Tezlink_directory.register_tezlink_services
           ~l2_chain_id
           (module Backend.Tezos)
           ~add_operation:(fun _ -> failwith "TODO: can't inject operation"))
        dir
