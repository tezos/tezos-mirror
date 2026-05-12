(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type evm_version = Shanghai | Cancun | Prague | Osaka

let make_path = function [] -> "" | l -> "/" ^ String.concat "/" l ^ "/"

let make_instr ?(path_prefix = ["evm"]) ?(convert = Fun.id) arg_opt =
  let path_prefix = make_path path_prefix in
  arg_opt
  |> Option.map (fun (key, value) ->
         Installer_config.make ~key:(path_prefix ^ key) ~value:(convert value))
  |> Option.to_list

let make_l2_config_instr ?(convert = Fun.id) ?(root = "evm") ~l2_chain_id config
    =
  make_instr
    ~path_prefix:[root; "chain_configurations"; l2_chain_id]
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

let make_l2 ?(kernel_compat = Constants.Latest) ~eth_bootstrap_balance
    ~tez_bootstrap_balance ?eth_bootstrap_accounts ?tez_bootstrap_accounts
    ?tez_bootstrap_contracts ?minimum_base_fee_per_gas
    ?michelson_to_evm_gas_multiplier ?da_fee_per_byte ?sequencer_pool_address
    ?maximum_gas_per_transaction ?set_account_code ?world_state_path
    ~l2_chain_id ~l2_chain_family ~output () =
  (* Phase 2 of the durable storage reorganization (storage version V53)
     moves /evm/chain_configurations/<id>/* to /base/chain_configurations/<id>/*.
     Match the same gate used by [make] for governance/config keys. *)
  let chain_configurations_root =
    if Constants.(kernel_is_newer ~than:FarfadetR2 kernel_compat) then "base"
    else "evm"
  in
  let make_l2_config_instr ?convert ~l2_chain_id config =
    make_l2_config_instr
      ?convert
      ~root:chain_configurations_root
      ~l2_chain_id
      config
  in
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
                manager |> Signature.V2.Public_key.hash
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
               let encode_len (s : string) =
                 s |> String.length |> Z.of_int
                 |> Data_encoding.Binary.to_string_exn Data_encoding.n
               in

               let encode_hexa_with_len x =
                 let encoded = encode_hexa x in
                 let encoded_len = encode_len encoded in
                 (encoded, encoded_len)
               in

               let path_prefix =
                 address |> Tezlink_durable_storage.Path.account
                 |> String.split_on_char '/' |> clean_path
               in

               let encoded_balance =
                 Data_encoding.Binary.to_string_exn
                   Tezos_types.Tez.encoding
                   tez_bootstrap_balance
               in

               let encoded_script, encoded_script_len =
                 encode_hexa_with_len script
               in
               let encoded_storage, encoded_storage_len =
                 encode_hexa_with_len storage
               in

               let instr key value =
                 make_instr ~path_prefix (Some (key, value))
               in

               [
                 ("balance", encoded_balance);
                 ("data/code", encoded_script);
                 ("len/code", encoded_script_len);
                 ("data/storage", encoded_storage);
                 ("len/storage", encoded_storage_len);
               ]
               |> List.concat_map (fun (k, v) -> instr k v))
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
        ~convert:le_int64_bytes
        michelson_to_evm_gas_multiplier
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

let make_tezos_bootstrap_instr tez_bootstrap_balance
    (tez_bootstrap_accounts : Signature.V2.public_key list) =
  List.map
    (fun manager ->
      let tezos_account_info =
        Tezosx.Tezos_runtime.
          {
            balance = tez_bootstrap_balance;
            nonce = 0L;
            public_key = Some manager;
          }
      in
      let address = Signature.V2.Public_key.hash manager in
      let (Address (Hex alias)) =
        Tezosx.Ethereum_runtime.generate_alias
          (Signature.V2.Public_key_hash.to_b58check address)
      in
      let payload =
        Tezosx.Tezos_runtime.encode_account_info tezos_account_info
      in
      let rlp_address =
        Bytes.to_string (Tezosx.Foreign_address.encode (`Tezos address))
      in
      make_instr
        ~path_prefix:
          [
            "tez";
            "tez_accounts";
            "tezosx";
            Signature.V2.Public_key_hash.to_b58check address;
          ]
        (Some ("info", Bytes.to_string payload))
      @ make_instr
          ~path_prefix:["tez"; "tez_accounts"; "tezosx"; "native"; "ethereum"]
          (Some ("0x" ^ alias, rlp_address)))
    tez_bootstrap_accounts
  |> List.flatten

let make_tezos_bootstrap_contracts_instr tez_bootstrap_balance contracts =
  contracts
  |> List.map (fun (address, script, storage) ->
         let encode_len (s : string) =
           s |> String.length |> Z.of_int
           |> Data_encoding.Binary.to_string_exn Data_encoding.n
         in

         let encode_hexa_with_len x =
           let encoded = encode_hexa x in
           let encoded_len = encode_len encoded in
           (encoded, encoded_len)
         in

         let path_prefix =
           Tezlink_durable_storage.Path.account address
           |> String.split_on_char '/' |> clean_path
         in

         let encoded_balance =
           Data_encoding.Binary.to_string_exn
             Tezos_types.Tez.encoding
             tez_bootstrap_balance
         in

         let encoded_script, encoded_script_len = encode_hexa_with_len script in
         let encoded_storage, encoded_storage_len =
           encode_hexa_with_len storage
         in

         let instr key value = make_instr ~path_prefix (Some (key, value)) in

         [
           ("balance", encoded_balance);
           ("data/code", encoded_script);
           ("len/code", encoded_script_len);
           ("data/storage", encoded_storage);
           ("len/storage", encoded_storage_len);
         ]
         |> List.concat_map (fun (k, v) -> instr k v))
  |> List.flatten

let make ?(kernel_compat = Constants.Latest) ~eth_bootstrap_balance
    ?l2_chain_ids ?eth_bootstrap_accounts ?kernel_root_hash ?chain_id
    ?michelson_runtime_chain_id ?sequencer ?delayed_bridge ?ticketer ?admin
    ?sequencer_governance ?kernel_governance ?kernel_security_governance
    ?minimum_base_fee_per_gas ?michelson_to_evm_gas_multiplier ?da_fee_per_byte
    ?delayed_inbox_timeout ?delayed_inbox_min_levels ?sequencer_pool_address
    ?maximum_allowed_ticks ?maximum_gas_per_transaction
    ?max_blueprint_lookahead_in_seconds ?remove_whitelist ?enable_fa_bridge
    ?enable_revm ?enable_dal ?dal_slots ?dal_publishers_whitelist
    ?disable_legacy_dal_signals ?enable_fast_withdrawal
    ?enable_fast_fa_withdrawal ?enable_multichain ?enable_michelson_gas_refund
    ?set_account_code ?max_delayed_inbox_blueprint_length ?evm_version
    ?(with_runtimes = []) ?tez_bootstrap_accounts ~tez_bootstrap_balance
    ?tez_bootstrap_contracts ~output () =
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
  let tez_bootstrap_accounts =
    match tez_bootstrap_accounts with
    | None -> []
    | Some tez_bootstrap_accounts ->
        make_tezos_bootstrap_instr tez_bootstrap_balance tez_bootstrap_accounts
  in
  let tez_bootstrap_contracts =
    match tez_bootstrap_contracts with
    | None -> []
    | Some tez_bootstrap_contracts ->
        make_tezos_bootstrap_contracts_instr
          tez_bootstrap_balance
          tez_bootstrap_contracts
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
          @@
          match evm_version with
          | Shanghai -> Z.zero
          | Cancun -> Z.one
          | Prague -> Z.of_int 2
          | Osaka -> Z.of_int 3 ))
      evm_version
  in
  (* Phase 2 of the durable storage reorganization (storage version V53)
     moves governance / sequencing / config installer-set paths from
     /evm/ to /base/. Phase 3 (storage version V54) moves all feature
     flags to /base/feature_flags/. Both ship on the same dev kernel, so
     a single gate against [FarfadetR2] selects the new paths. *)
  let newer_than_farfadet_r2 =
    Constants.(kernel_is_newer ~than:FarfadetR2 kernel_compat)
  in
  let governance_in_base = newer_than_farfadet_r2 in
  let feature_flags_in_base = newer_than_farfadet_r2 in
  let base_or_evm_prefix = if governance_in_base then ["base"] else ["evm"] in
  let make_governance_instr ?convert arg =
    make_instr ?convert ~path_prefix:base_or_evm_prefix arg
  in
  (* Path prefix for feature flags according to the target kernel's version. *)
  let feature_flag_prefix_base = ["base"; "feature_flags"] in
  let feature_flag_prefix_evm = ["evm"; "feature_flags"] in
  let feature_flag_prefix_world_state =
    ["evm"; "world_state"; "feature_flags"]
  in
  let feature_flag_prefix_tezlink = ["tezlink"; "feature_flags"] in
  let make_feature_flag_instr ?convert ~legacy_prefix arg =
    let path_prefix =
      if feature_flags_in_base then feature_flag_prefix_base else legacy_prefix
    in
    make_instr ?convert ~path_prefix arg
  in
  (* V57 moved /evm/michelson_runtime/{,target_}sunrise_level under
     /tez/world_state/. V57 ships in the [Latest] kernel — i.e. on any
     kernel_compat strictly newer than [FarfadetR2]. *)
  let michelson_runtime_paths_in_world_state = newer_than_farfadet_r2 in
  let with_runtimes =
    List.concat_map
      (fun (runtime, target_sunrise_level) ->
        let flag_path =
          let prefix =
            if feature_flags_in_base then feature_flag_prefix_base
            else feature_flag_prefix_evm
          in
          match runtime with
          | Tezosx.Tezos ->
              String.concat "/" (("" :: prefix) @ ["enable_tezos_runtime"])
        in
        let target_sunrise_level_path = function
          | Tezosx.Tezos ->
              let storage_version =
                if michelson_runtime_paths_in_world_state then 57 else 56
              in
              Durable_storage_path.michelson_runtime_target_sunrise_level
                ~storage_version
        in
        Installer_config.make ~key:flag_path ~value:""
        ::
        (match target_sunrise_level with
        | None -> []
        | Some level ->
            [
              Installer_config.make
                ~key:(target_sunrise_level_path runtime)
                ~value:(Tezosx.encode_target_sunrise_level level);
            ]))
      with_runtimes
  in
  let instrs =
    (if Constants.(kernel_is_newer ~than:Mainnet_beta kernel_compat) then
       make_instr ~path_prefix:["evm"; "world_state"] ticketer
     else make_instr ticketer)
    @ (if Constants.(kernel_is_newer ~than:FarfadetR2 kernel_compat) then
         make_instr ~path_prefix:["evm"; "world_state"] sequencer
       else make_instr sequencer)
    @ make_governance_instr
        ~convert:(fun s -> Hex.to_bytes_exn (`Hex s) |> Bytes.to_string)
        kernel_root_hash
    @ make_instr ~convert:parse_z_to_padded_32_le_int_bytes chain_id
    @ make_instr
        ~path_prefix:["tez"; "world_state"]
        (Option.map
           (fun chain_id ->
             chain_id
             |> Data_encoding.Binary.to_bytes_exn Chain_id.encoding
             |> Bytes.to_string
             |> fun s -> ("chain_id", s))
           michelson_runtime_chain_id)
    @ make_governance_instr delayed_bridge
    @ make_governance_instr admin
    @ make_instr sequencer_governance
    @ make_governance_instr kernel_governance
    @ make_governance_instr kernel_security_governance
    @ make_instr evm_version
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "fees"]
        ~convert:parse_z_to_padded_32_le_int_bytes
        minimum_base_fee_per_gas
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "fees"]
        ~convert:le_int64_bytes
        michelson_to_evm_gas_multiplier
    @ make_instr
        ~path_prefix:["evm"; "world_state"; "fees"]
        ~convert:parse_z_to_padded_32_le_int_bytes
        da_fee_per_byte
    @ make_governance_instr ~convert:le_int64_bytes delayed_inbox_timeout
    @ make_governance_instr ~convert:le_int64_bytes delayed_inbox_min_levels
    @ make_instr
        ~convert:(fun addr ->
          match Misc.normalize_hex addr with
          | Ok hex -> Hex.to_bytes_exn hex |> String.of_bytes
          | Error _ -> raise (Invalid_argument "sequencer_pool_address"))
        sequencer_pool_address
    @ make_governance_instr ~convert:le_int64_bytes maximum_allowed_ticks
    @ make_instr ~convert:le_int64_bytes maximum_gas_per_transaction
    @ make_governance_instr
        ~convert:le_int64_bytes
        max_blueprint_lookahead_in_seconds
    @ eth_bootstrap_accounts @ tez_bootstrap_accounts @ tez_bootstrap_contracts
    @ set_account_code
    @ make_governance_instr remove_whitelist
    @ make_feature_flag_instr
        ~legacy_prefix:feature_flag_prefix_evm
        enable_fa_bridge
    (* enable_revm is only consumed by pre-V54 kernels — V54+ kernels
       unconditionally enable revm and ignore the flag. *)
    @ (if feature_flags_in_base then []
       else make_instr ~path_prefix:feature_flag_prefix_world_state enable_revm)
    @ make_feature_flag_instr ~legacy_prefix:feature_flag_prefix_evm enable_dal
    @ make_feature_flag_instr
        ~legacy_prefix:feature_flag_prefix_evm
        disable_legacy_dal_signals
    (* enable_fast_withdrawal and enable_fast_fa_withdrawal are only
       consumed by pre-Farfadet kernels — Farfadet+ kernels run the
       corresponding code paths unconditionally and ignore these flags. *)
    @ (if feature_flags_in_base then []
       else
         make_instr
           ~path_prefix:feature_flag_prefix_world_state
           enable_fast_withdrawal)
    @ (if feature_flags_in_base then []
       else
         make_instr
           ~path_prefix:feature_flag_prefix_world_state
           enable_fast_fa_withdrawal)
    @ make_governance_instr ~convert:decimal_list_to_bytes dal_slots
    @ make_instr
        ~convert:(fun s ->
          let open Evm_node_lib_dev_encoding.Rlp in
          let pkh_list =
            if String.trim s = "" then [] else String.split_on_char ',' s
          in
          let encoded_list =
            List.map
              (fun pkh_str ->
                let pkh_str = String.trim pkh_str in
                (* Decode base58check to get the PublicKeyHash *)
                let pkh =
                  Tezos_crypto.Signature.Public_key_hash.of_b58check_exn pkh_str
                in
                (* Encode to binary using Data_encoding *)
                let binary =
                  Data_encoding.Binary.to_bytes_exn
                    Tezos_crypto.Signature.Public_key_hash.encoding
                    pkh
                in
                (* Store as binary bytes in RLP *)
                Value binary)
              pkh_list
          in
          (* RLP-encode the list *)
          let rlp_item = List encoded_list in
          Bytes.to_string (encode rlp_item))
        ~path_prefix:base_or_evm_prefix
        dal_publishers_whitelist
    @ make_feature_flag_instr
        ~legacy_prefix:feature_flag_prefix_evm
        enable_multichain
    @ make_feature_flag_instr
        ~legacy_prefix:feature_flag_prefix_tezlink
        enable_michelson_gas_refund
    @ make_governance_instr
        ~convert:(fun s -> Ethereum_types.u16_to_bytes (int_of_string s))
        max_delayed_inbox_blueprint_length
    @ chain_ids_instr @ with_runtimes
  in
  Installer_config.to_file instrs ~output
