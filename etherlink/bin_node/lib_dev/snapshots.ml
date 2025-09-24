(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Invalid_snapshot_file of string
  | Invalid_snapshot_provider of string
  | Data_dir_populated of string
  | History_mode_mismatch of
      Configuration.history_mode * Configuration.history_mode
  | Incorrect_rollup of Address.t * Address.t
  | Outdated_snapshot of Z.t * Z.t

let () =
  register_error_kind
    `Permanent
    ~id:"evm_invalid_snapshot_file"
    ~title:"Invalid snapshot file path"
    ~description:"The snapshot file path is invalid"
    ~pp:(fun ppf name ->
      Format.fprintf ppf "%s is not a valid snapshot file name." name)
    Data_encoding.(obj1 (req "snapshot_file" string))
    (function Invalid_snapshot_file name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_file name) ;
  register_error_kind
    `Permanent
    ~id:"evm_invalid_snapshot_provider"
    ~title:"Invalid snapshot provider"
    ~description:"The snapshot provider is invalid."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "%s is not a valid snapshot provider name" name)
    Data_encoding.(obj1 (req "snapshot_provider" string))
    (function Invalid_snapshot_provider name -> Some name | _ -> None)
    (fun name -> Invalid_snapshot_provider name) ;
  register_error_kind
    `Permanent
    ~id:"evm_data_dir_populated"
    ~title:"Data dir already populated"
    ~description:"Raise an error when the data dir is already populated"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "The EVM node data directory %s is already populated."
        path)
    Data_encoding.(obj1 (req "data_dir_already_populated" string))
    (function Data_dir_populated path -> Some path | _ -> None)
    (fun path -> Data_dir_populated path) ;
  register_error_kind
    `Permanent
    ~id:"evm_history_mode_mismatch"
    ~title:"History mode mismatch"
    ~description:"Configuration history mode does not match snapshot's."
    ~pp:(fun ppf (config_mode, snapshot_mode) ->
      Format.fprintf
        ppf
        "History mode values are: Configuration = %s; Snapshot = %s. Consider \
         running with `--history-mode`."
        (Configuration.string_of_history_mode_info config_mode)
        (Configuration.string_of_history_mode_info snapshot_mode))
    Data_encoding.(
      obj2
        (req "config_history_mode" Configuration.history_mode_encoding)
        (req "snapshot_history_mode" Configuration.history_mode_encoding))
    (function History_mode_mismatch (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> History_mode_mismatch (a1, a2)) ;
  register_error_kind
    `Permanent
    ~id:"evm_incorrect_rollup"
    ~title:"Snapshot for incorrect rollup"
    ~description:"Snapshot for incorrect rollup."
    ~pp:(fun ppf (snap_addr, exp_addr) ->
      Format.fprintf
        ppf
        "The existing EVM node is for the rollup %a whereas the snapshot is \
         for %a."
        Address.pp
        exp_addr
        Address.pp
        snap_addr)
    Data_encoding.(
      obj2
        (req "evm_node_rollup" Address.encoding)
        (req "snapshot_rollup" Address.encoding))
    (function Incorrect_rollup (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> Incorrect_rollup (a1, a2)) ;
  register_error_kind
    `Permanent
    ~id:"evm_outdated_snapshot"
    ~title:"Outdated snapshot"
    ~description:"Snapshot is outdated with respect to existing data."
    ~pp:(fun ppf (snap_level, exp_level) ->
      Format.fprintf
        ppf
        "The snapshot is outdated (for level %a) while the existing EVM node \
         is already at %a"
        Z.pp_print
        snap_level
        Z.pp_print
        exp_level)
    Data_encoding.(obj2 (req "evm_node_level" z) (req "snapshot_level" z))
    (function Outdated_snapshot (a1, a2) -> Some (a1, a2) | _ -> None)
    (fun (a1, a2) -> Outdated_snapshot (a1, a2))

type metadata =
  | V1 of {
      rollup_address : Address.t;
      current_level : Ethereum_types.quantity;
      history_mode : Configuration.history_mode;
      first_level : Ethereum_types.quantity;
    }

let quantity_hum_encoding =
  let open Data_encoding in
  union
    [
      case
        Json_only
        ~title:"int"
        int31
        (fun (Ethereum_types.Qty z) ->
          if Z.fits_int z then Some (Z.to_int z) else None)
        (fun i -> Ethereum_types.quantity_of_z (Z.of_int i));
      case
        Json_only
        ~title:"z"
        z
        (fun (Ethereum_types.Qty z) -> Some z)
        (fun z -> Ethereum_types.quantity_of_z z);
    ]

let metadata_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"evm_node.snapshot_metadata.v1"
        (Tag 1)
        (obj5
           (req "version" (constant "evm_node.snapshot_metadata.v1"))
           (req "rollup_address" Address.encoding)
           (req "current_level" quantity_hum_encoding)
           (req "history_mode" Configuration.history_mode_encoding)
           (req "first_level" quantity_hum_encoding))
        (function
          | V1 {rollup_address; current_level; history_mode; first_level} ->
              Some ((), rollup_address, current_level, history_mode, first_level))
        (fun ((), rollup_address, current_level, history_mode, first_level) ->
          V1 {rollup_address; current_level; history_mode; first_level});
    ]

let interpolate_snapshot_file current_level rollup_address history_mode filename
    =
  let rollup_address_short =
    ('r', `Available (Address.to_short_b58check rollup_address))
  in
  let rollup_address_long =
    ('R', `Available (Address.to_b58check rollup_address))
  in
  let current_level =
    ( 'l',
      `Available (Format.asprintf "%a" Ethereum_types.pp_quantity current_level)
    )
  in
  let history_mode =
    ( 'h',
      `Available
        (Configuration.string_of_history_mode_info history_mode
        |> String.lowercase_ascii) )
  in
  record_trace (Invalid_snapshot_file filename)
  @@ Misc.interpolate
       filename
       [rollup_address_short; rollup_address_long; current_level; history_mode]

let interpolate_snapshot_provider ?rollup_address ?network history_mode provider
    =
  let inferred_rollup_address = Option.map Constants.rollup_address network in
  let reason = "try specifying the network with --network" in
  let rollup_address_short, rollup_address_long =
    match (rollup_address, inferred_rollup_address) with
    | Some rollup_address, _ | None, Some rollup_address ->
        ( ( 'r',
            `Available
              (Tezos_crypto.Hashed.Smart_rollup_address.to_short_b58check
                 rollup_address) ),
          ( 'R',
            `Available
              (Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
                 rollup_address) ) )
    | None, None -> (('r', `Disabled reason), ('R', `Disabled reason))
  in
  let history_mode =
    ( 'h',
      `Available
        (Configuration.string_of_history_mode_info history_mode
        |> String.lowercase_ascii) )
  in
  let network =
    match
      (network, Option.bind rollup_address Constants.network_of_address)
    with
    | Some n, _ | None, Some n ->
        ( 'n',
          `Available (Format.asprintf "%a" Configuration.pp_supported_network n)
        )
    | None, None -> ('n', `Disabled reason)
  in

  record_trace (Invalid_snapshot_provider provider)
  @@ Misc.interpolate
       provider
       [rollup_address_short; rollup_address_long; history_mode; network]
