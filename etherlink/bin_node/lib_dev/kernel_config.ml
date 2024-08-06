(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let make_instr ?(path_prefix = "/evm/") ?(convert = Fun.id) arg_opt =
  arg_opt
  |> Option.map (fun (key, value) ->
         Installer_config.make ~key:(path_prefix ^ key) ~value:(convert value))
  |> Option.to_list

let padded_32_le_int_bytes z =
  String.of_bytes @@ Ethereum_types.encode_u256_le (Qty z)

let encode_hexa hexa =
  let hexa = Ethereum_types.hex_of_string hexa in
  Ethereum_types.hex_to_bytes hexa

let parse_z_to_padded_32_le_int_bytes s =
  let z = Z.of_string s in
  padded_32_le_int_bytes z

let make ~mainnet_compat ~boostrap_balance ?bootstrap_accounts ?kernel_root_hash
    ?chain_id ?sequencer ?delayed_bridge ?ticketer ?admin ?sequencer_governance
    ?kernel_governance ?kernel_security_governance ?minimum_base_fee_per_gas
    ?da_fee_per_byte ?delayed_inbox_timeout ?delayed_inbox_min_levels
    ?sequencer_pool_address ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?remove_whitelist ?enable_fa_bridge
    ?enable_dal ?dal_slots ?set_account_code ~output () =
  let bootstrap_accounts =
    match bootstrap_accounts with
    | None -> []
    | Some bootstrap_accounts ->
        let balance = padded_32_le_int_bytes boostrap_balance in
        List.map
          (fun address ->
            make_instr
              ~path_prefix:"/evm/world_state/eth_accounts/"
              (Some (address ^ "/balance", balance)))
          bootstrap_accounts
        |> List.flatten
  in
  let set_account_code =
    match set_account_code with
    | None -> []
    | Some set_account_codes ->
        List.map
          (fun (address, code) ->
            make_instr
              ~convert:encode_hexa
              ~path_prefix:"/evm/world_state/eth_accounts/"
              (Some (address ^ "/code", code)))
          set_account_codes
        |> List.flatten
  in
  let le_int64_bytes i =
    let b = Bytes.make 8 '\000' in
    Bytes.set_int64_le b 0 (Int64.of_string i) ;
    String.of_bytes b
  in
  (* Convert a comma-separated list of decimal values in the [0; 255]
     range into a sequence of bytes (of type string). *)
  let decimal_list_to_bytes l =
    l |> String.split ',' |> List.to_seq
    |> Seq.map (fun s -> Char.chr (int_of_string s))
    |> String.of_seq
  in
  let instrs =
    (if mainnet_compat then make_instr ~path_prefix:"/evm/" ticketer
     else
       (* For compatibility reason for Mainnet and Ghostnet *)
       make_instr ~path_prefix:"/evm/world_state/" ticketer)
    @ make_instr
        ~convert:(fun s -> Hex.to_bytes_exn (`Hex s) |> Bytes.to_string)
        kernel_root_hash
    @ make_instr ~convert:parse_z_to_padded_32_le_int_bytes chain_id
    @ make_instr sequencer @ make_instr delayed_bridge @ make_instr admin
    @ make_instr sequencer_governance
    @ make_instr kernel_governance
    @ make_instr kernel_security_governance
    @ make_instr
        ~path_prefix:"/evm/world_state/fees/"
        ~convert:parse_z_to_padded_32_le_int_bytes
        minimum_base_fee_per_gas
    @ make_instr
        ~path_prefix:"/evm/world_state/fees/"
        ~convert:parse_z_to_padded_32_le_int_bytes
        da_fee_per_byte
    @ make_instr ~convert:le_int64_bytes delayed_inbox_timeout
    @ make_instr ~convert:le_int64_bytes delayed_inbox_min_levels
    @ make_instr
        ~convert:(fun addr ->
          let addr = Misc.normalize_addr addr in
          Hex.to_bytes_exn (`Hex addr) |> String.of_bytes)
        sequencer_pool_address
    @ make_instr ~convert:le_int64_bytes maximum_allowed_ticks
    @ make_instr ~convert:le_int64_bytes maximum_gas_per_transaction
    @ make_instr ~convert:le_int64_bytes max_blueprint_lookahead_in_seconds
    @ bootstrap_accounts @ set_account_code
    @ make_instr remove_whitelist
    @ make_instr ~path_prefix:"/evm/feature_flags/" enable_fa_bridge
    @ make_instr ~path_prefix:"/evm/feature_flags/" enable_dal
    @ make_instr ~convert:decimal_list_to_bytes dal_slots
  in
  Installer_config.to_file instrs ~output
