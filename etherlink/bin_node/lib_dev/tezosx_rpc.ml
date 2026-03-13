(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let add_rpc_directory ro_ctxt ~l2_chain_id ~add_operation dir = function
  | Tezosx.Tezos ->
      Tezos_rpc.Directory.merge
        (Tezlink_directory.register_tezlink_services
           ~l2_chain_id
           (Tezos_backend.make ro_ctxt)
           ~add_operation
           ~get_da_fee_per_byte_nanotez:Prevalidator.get_da_fee_per_byte_nanotez)
        dir
