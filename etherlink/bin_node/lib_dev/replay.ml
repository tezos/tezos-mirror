(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let main ~data_dir ~preimages ~preimages_endpoint ~smart_rollup_address number =
  let open Lwt_result_syntax in
  let* _init =
    Evm_context.start
      ~data_dir
      ~preimages
      ~preimages_endpoint
      ~fail_on_missing_blueprint:false
      ~smart_rollup_address
      ()
  in
  let* apply_result = Evm_context.replay number in
  match apply_result with
  | Apply_success (_, _, hash) ->
      Format.printf
        "Replaying blueprint %a led to block %a\n%!"
        Ethereum_types.pp_quantity
        number
        Ethereum_types.pp_block_hash
        hash ;
      return_unit
  | Apply_failure ->
      failwith "Could not replay blueprint %a" Ethereum_types.pp_quantity number
