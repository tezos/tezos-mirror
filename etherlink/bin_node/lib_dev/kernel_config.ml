(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type evm_version = Shanghai | Cancun

let make_path = function [] -> "" | l -> "/" ^ String.concat "/" l ^ "/"

let make_instr ?(path_prefix = ["evm"]) ?(convert = Fun.id) arg_opt =
  let path_prefix = make_path path_prefix in
  arg_opt
  |> Option.map (fun (key, value) ->
         Installer_config.make ~key:(path_prefix ^ key) ~value:(convert value))
  |> Option.to_list

let make_l2_config_instr ?(convert = Fun.id) ~l2_chain_id config =
  make_instr
    ~path_prefix:["evm"; "chain_configurations"; l2_chain_id]
    ~convert
    config

let padded_32_le_int_bytes z =
  String.of_bytes @@ Ethereum_types.encode_u256_le (Qty z)

let encode_hexa hexa =
  let hexa = Ethereum_types.hex_of_string hexa in
  Ethereum_types.hex_to_bytes hexa

let parse_z_to_padded_32_le_int_bytes s =
  let z = Z.of_string s in
  padded_32_le_int_bytes z

let le_int64_bytes i =
  let b = Bytes.make 8 '\000' in
  Bytes.set_int64_le b 0 (Int64.of_string i) ;
  String.of_bytes b

module ChainSet = Set.Make (struct
  type t = L2_types.chain_id

  let compare = L2_types.Chain_id.compare
end)

let all_duplicate_chain_ids ~l2_chain_ids =
  let rec aux_same answer seen = function
    | [] -> answer
    | chain_id :: tl when ChainSet.mem chain_id seen ->
        aux_same (ChainSet.add chain_id answer) seen tl
    | chain_id :: tl -> aux_same answer (ChainSet.add chain_id seen) tl
  in
  aux_same ChainSet.empty ChainSet.empty l2_chain_ids

(* This code comes from etherlink/bin_node/config/configuration.ml *)
let warn =
  Format.kasprintf @@ fun s ->
  let reset = Pretty_printing.add_ansi_marking Format.err_formatter in
  Format.eprintf "@{<fg_yellow>[Warning] %s@}@." s ;
  reset ()

let check_for_duplicate ~l2_chain_ids =
  let duplicate_chain_ids = all_duplicate_chain_ids ~l2_chain_ids in
  if not (ChainSet.is_empty duplicate_chain_ids) then
    warn
      "The following chain ids have been provided more than once: %a. Chain \
       IDs must be unique."
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         (fun ppf chain_id ->
           Format.fprintf ppf "%s" (L2_types.Chain_id.to_string chain_id)))
      (ChainSet.elements duplicate_chain_ids)

(* When splitting the path given in argument,
   their should be empty string at the start/end of
   the list *)
let clean_path path =
  List.fold_left
    (fun acc l -> if l = "" then acc else l :: acc)
    []
    (List.rev path)

let make_l2 ~eth_bootstrap_balance ~tez_bootstrap_balance
    ?eth_bootstrap_accounts ?tez_bootstrap_accounts ?tez_bootstrap_contracts
    ?minimum_base_fee_per_gas ?da_fee_per_byte ?sequencer_pool_address
    ?maximum_gas_per_transaction ?set_account_code ?world_state_path
    ~l2_chain_id ~l2_chain_family ~output () =
  let world_state_prefix =
    match world_state_path with
    | None -> ["evm"; "world_state"; l2_chain_id]
    | Some (_, value) -> clean_path (String.split_on_char '/' value)
  in
  let eth_bootstrap_accounts =
    match eth_bootstrap_accounts with
    | None -> []
    | Some eth_bootstrap_accounts ->
        let open Ethereum_types in
        let balance = padded_32_le_int_bytes eth_bootstrap_balance in
        List.map
          (fun (Address (Hex address)) ->
            make_instr
              ~path_prefix:(world_state_prefix @ ["eth_accounts"; address])
              (Some ("balance", balance)))
          eth_bootstrap_accounts
        |> List.flatten
  in
  let tez_bootstrap_accounts =
    match tez_bootstrap_accounts with
    | None -> []
    | Some tez_bootstrap_accounts ->
        List.map
          (fun manager ->
            let make_account_field key value converter =
              let path_prefix =
                manager |> Signature.V1.Public_key.hash
                |> Tezos_types.Contract.of_implicit
                |> Tezlink_durable_storage.Path.account
                |> String.split_on_char '/' |> clean_path
              in
              make_instr ~path_prefix (Some (key, converter value))
            in
            make_account_field
              "balance"
              tez_bootstrap_balance
              (Data_encoding.Binary.to_string_exn Tezos_types.Tez.encoding)
            @ make_account_field
                "manager"
                (Tezos_types.Manager.Public_key manager)
                (Data_encoding.Binary.to_string_exn
                   Tezos_types.Manager.encoding)
            @ make_account_field
                "counter"
                Z.zero
                (Data_encoding.Binary.to_string_exn Data_encoding.n))
          tez_bootstrap_accounts
        |> List.flatten
  in
  let tez_bootstrap_contracts =
    match tez_bootstrap_contracts with
    | None -> []
    | Some contracts ->
        contracts
        |> List.map (fun (address, script, storage) ->
               let path_prefix =
                 address |> Tezlink_durable_storage.Path.account
                 |> String.split_on_char '/' |> clean_path
               in
               let make_account_field key value converter =
                 make_instr ~path_prefix (Some (key, converter value))
               in

               make_account_field "data/code" script encode_hexa
               @ make_account_field "data/storage" storage encode_hexa)
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
              ~path_prefix:(world_state_prefix @ ["eth_accounts"; address])
              (Some ("code", code)))
          set_account_codes
        |> List.flatten
  in
  let config_instrs =
    (* These configuration parameter will not be stored in the world_state of an l2 chain but are parameter for an l2 chain *)
    (* To do so we put them into another path /evm/config/<l2_chain_id> *)
    make_l2_config_instr
      ~l2_chain_id
      ~convert:parse_z_to_padded_32_le_int_bytes
      minimum_base_fee_per_gas
    @ make_l2_config_instr
        ~l2_chain_id
        ~convert:parse_z_to_padded_32_le_int_bytes
        da_fee_per_byte
    @ make_l2_config_instr
        ~l2_chain_id
        ~convert:le_int64_bytes
        maximum_gas_per_transaction
    @ make_l2_config_instr
        ~l2_chain_id
        (Some
           ("chain_family", l2_chain_family |> L2_types.Chain_family.to_string))
    @ make_l2_config_instr ~l2_chain_id world_state_path
  in
  let world_state_instrs =
    make_instr
      ~convert:(fun addr ->
        match Misc.normalize_hex addr with
        | Ok (`Hex hex) -> hex
        | Error _ -> raise (Invalid_argument "sequencer_pool_address"))
      ~path_prefix:world_state_prefix
      sequencer_pool_address
    @ eth_bootstrap_accounts @ tez_bootstrap_accounts @ tez_bootstrap_contracts
    @ set_account_code
  in
  Installer_config.to_file (config_instrs @ world_state_instrs) ~output

let make ~mainnet_compat ~eth_bootstrap_balance ?l2_chain_ids
    ?eth_bootstrap_accounts ?kernel_root_hash ?chain_id ?sequencer
    ?delayed_bridge ?ticketer ?admin ?sequencer_governance ?kernel_governance
    ?kernel_security_governance ?minimum_base_fee_per_gas ?da_fee_per_byte
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?sequencer_pool_address
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?remove_whitelist ?enable_fa_bridge
    ?enable_revm ?enable_dal ?dal_slots ?enable_fast_withdrawal
    ?enable_fast_fa_withdrawal ?enable_multichain ?set_account_code
    ?max_delayed_inbox_blueprint_length ?evm_version ~output () =
  let eth_bootstrap_accounts =
    let open Ethereum_types in
    match eth_bootstrap_accounts with
    | None -> []
    | Some eth_bootstrap_accounts ->
        let balance = padded_32_le_int_bytes eth_bootstrap_balance in
        List.map
          (fun (Address (Hex address)) ->
            make_instr
              ~path_prefix:["evm"; "world_state"; "eth_accounts"; address]
              (Some ("balance", balance)))
          eth_bootstrap_accounts
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
              ~path_prefix:["evm"; "world_state"; "eth_accounts"; address]
              (Some ("code", code)))
          set_account_codes
        |> List.flatten
  in
  (* Convert a comma-separated list of decimal values in the [0; 255]
     range into a sequence of bytes (of type string). *)
  let decimal_list_to_bytes l =
    l |> String.split ',' |> List.to_seq
    |> Seq.map (fun s -> Char.chr (int_of_string s))
    |> String.of_seq
  in
  let chain_ids_instr =
    match l2_chain_ids with
    | None -> []
    | Some l2_chain_ids ->
        let open Rlp in
        check_for_duplicate ~l2_chain_ids ;
        let chain_ids =
          List.map
            (fun chain_id ->
              let chain_id = L2_types.Chain_id.to_string chain_id in
              Value (Bytes.of_string chain_id))
            l2_chain_ids
        in
        let encoded_chain_ids =
          Rlp.encode (List chain_ids) |> Bytes.to_string
        in
        make_instr (Some ("chain_ids", encoded_chain_ids))
  in
  let evm_version =
    Option.map
      (fun evm_version ->
        ( "evm_version",
          padded_32_le_int_bytes
          @@ match evm_version with Shanghai -> Z.zero | Cancun -> Z.one ))
      evm_version
  in
  let instrs =
    (if mainnet_compat then make_instr ticketer
     else
       (* For compatibility reason for Mainnet and Ghostnet *)
       make_instr ~path_prefix:["evm"; "world_state"] ticketer)
    @ make_instr
        ~convert:(fun s -> Hex.to_bytes_exn (`Hex s) |> Bytes.to_string)
        kernel_root_hash
    @ make_instr ~convert:parse_z_to_padded_32_le_int_bytes chain_id
    @ make_instr sequencer @ make_instr delayed_bridge @ make_instr admin
    @ make_instr sequencer_governance
    @ make_instr kernel_governance
    @ make_instr kernel_security_governance
    @ make_instr evm_version
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "fees"]
        ~convert:parse_z_to_padded_32_le_int_bytes
        minimum_base_fee_per_gas
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "fees"]
        ~convert:parse_z_to_padded_32_le_int_bytes
        da_fee_per_byte
    @ make_instr ~convert:le_int64_bytes delayed_inbox_timeout
    @ make_instr ~convert:le_int64_bytes delayed_inbox_min_levels
    @ make_instr
        ~convert:(fun addr ->
          match Misc.normalize_hex addr with
          | Ok hex -> Hex.to_bytes_exn hex |> String.of_bytes
          | Error _ -> raise (Invalid_argument "sequencer_pool_address"))
        sequencer_pool_address
    @ make_instr ~convert:le_int64_bytes maximum_allowed_ticks
    @ make_instr ~convert:le_int64_bytes maximum_gas_per_transaction
    @ make_instr ~convert:le_int64_bytes max_blueprint_lookahead_in_seconds
    @ eth_bootstrap_accounts @ set_account_code
    @ make_instr remove_whitelist
    @ make_instr ~path_prefix:["evm"; "feature_flags"] enable_fa_bridge
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "feature_flags"]
        enable_revm
    @ make_instr ~path_prefix:["evm"; "feature_flags"] enable_dal
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "feature_flags"]
        enable_fast_withdrawal
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "feature_flags"]
        enable_fast_fa_withdrawal
    @ make_instr ~convert:decimal_list_to_bytes dal_slots
    @ make_instr ~path_prefix:["evm"; "feature_flags"] enable_multichain
    @ make_instr
        ~convert:(fun s -> Ethereum_types.u16_to_bytes (int_of_string s))
        max_delayed_inbox_blueprint_length
    @ chain_ids_instr
  in
  Installer_config.to_file instrs ~output
