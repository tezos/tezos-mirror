(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Lost_node_connection
  | Cannot_connect_to_node of string
  | Cannot_decode_node_data of string
  | Missing_current_agent of string
  | Agent_process_error of string
  | Node_version_incompatible of {
      node_version : Tezos_version_parser.t;
      node_commit_info : Tezos_version.Octez_node_version.commit_info option;
      baker_version : Tezos_version_parser.t;
      baker_commit_info : Tezos_version.Octez_node_version.commit_info option;
    }
  | Node_version_malformatted of string
  | Block_vote_file_not_found of string
  | Block_vote_file_invalid of string
  | Block_vote_file_wrong_content of string
  | Block_vote_file_missing_liquidity_baking_toggle_vote of string
  | Missing_vote_on_startup

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.lost_node_connection"
    ~title:"Lost node connection"
    ~description:"Connection with node lost."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection with node was lost")
    Data_encoding.(unit)
    (function Lost_node_connection -> Some () | _ -> None)
    (fun () -> Lost_node_connection) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.cannot_connect_to_node"
    ~title:"Cannot connect to node"
    ~description:"Cannot connect to node."
    ~pp:(fun ppf uri ->
      Format.fprintf
        ppf
        "Cannot connect to node. Connection refused (ECONNREFUSED): %s"
        uri)
    Data_encoding.(obj1 (req "uri" string))
    (function Cannot_connect_to_node uri -> Some uri | _ -> None)
    (fun uri -> Cannot_connect_to_node uri) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.missing_current_agent"
    ~title:"Missing current agent"
    ~description:"The current agent process is missing."
    ~pp:(fun ppf agent -> Format.fprintf ppf "Missing current %s" agent)
    Data_encoding.(obj1 (req "agent" string))
    (function Missing_current_agent agent -> Some agent | _ -> None)
    (fun agent -> Missing_current_agent agent) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.agent_process_error"
    ~title:"Underlying agent process error"
    ~description:"There is an error in the underlying agent process."
    ~pp:(fun ppf agent ->
      Format.fprintf ppf "Error in the underlying %s process" agent)
    Data_encoding.(obj1 (req "agent" string))
    (function Agent_process_error agent -> Some agent | _ -> None)
    (fun agent -> Agent_process_error agent) ;
  register_error_kind
    `Temporary
    ~id:"agnostic_agent.node_version_incompatible"
    ~title:"Node version is incompatible"
    ~description:"The node version is incompatible with this baker"
    ~pp:(fun
        fmt
        ((node_version, node_commit_info, baker_version, baker_commit_info) :
          Tezos_version_parser.t
          * Tezos_version.Octez_node_version.commit_info option
          * Tezos_version_parser.t
          * Tezos_version.Octez_node_version.commit_info option)
      ->
      Format.fprintf
        fmt
        "@[Node version is %a (%a) but it is expected to be more recent than \
         %a (%a).@ This check can be bypassed at your own risk with the flag \
         --node-version-check-bypass or the argument --node-version-allowed \
         %a%a .@]"
        Tezos_version.Version.pp
        node_version
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "none")
           Tezos_version.Octez_node_version.commit_info_pp_short)
        node_commit_info
        Tezos_version.Version.pp
        baker_version
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.pp_print_string ppf "none")
           Tezos_version.Octez_node_version.commit_info_pp_short)
        baker_commit_info
        Tezos_version.Version.pp_arg
        node_version
        (Format.pp_print_option
           ~none:(fun _ppf () -> ())
           (fun ppf ->
             Format.fprintf
               ppf
               ":%a"
               Tezos_version.Octez_node_version.commit_info_pp_short))
        node_commit_info)
    Data_encoding.(
      obj4
        (req "node_version" Tezos_version.Octez_node_version.version_encoding)
        (opt
           "node_commit_info"
           Tezos_version.Octez_node_version.commit_info_encoding)
        (req "baker_version" Tezos_version.Octez_node_version.version_encoding)
        (opt
           "baker_commit_info"
           Tezos_version.Octez_node_version.commit_info_encoding))
    (function
      | Node_version_incompatible
          {node_version; node_commit_info; baker_version; baker_commit_info} ->
          Some (node_version, node_commit_info, baker_version, baker_commit_info)
      | _ -> None)
    (fun (node_version, node_commit_info, baker_version, baker_commit_info) ->
      Node_version_incompatible
        {node_version; node_commit_info; baker_version; baker_commit_info}) ;
  register_error_kind
    `Temporary
    ~id:"agnostic_agent.node_version_malformatted"
    ~title:"Node version in command argument is malformatted"
    ~description:"The node version provided in command argument is malformatted"
    ~pp:(fun fmt version ->
      Format.fprintf
        fmt
        "The node version provided in command argument: '%s' is a malformatted \
         version"
        version)
    Data_encoding.(obj1 (req "provided_version" string))
    (function Node_version_malformatted v -> Some v | _ -> None)
    (fun v -> Node_version_malformatted v) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_not_found"
    ~title:
      "The provided block vote file path does not point to an existing file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to an existing file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to an \
         existing file.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_not_found file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_not_found file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_invalid"
    ~title:
      "The provided block vote file path does not point to a valid JSON file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to a valid JSON file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to a valid \
         JSON file. The file exists but its content is not valid JSON.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function Block_vote_file_invalid file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_invalid file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_wrong_content"
    ~title:"The content of the provided block vote file is unexpected."
    ~description:
      "The block vote file is valid JSON but its content is not the expected \
       one."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file \"%s\" is a valid JSON file but its \
         content is unexpected. Expecting a JSON file containing \
         '{\"liquidity_baking_toggle_vote\": value1, \
         \"adaptive_issuance_vote\": value2}' or '{\"adaptive_issuance_vote\": \
         value1, \"liquidity_baking_toggle_vote\": value2}', where value1 is \
         one of \"on\", \"off\", or \"pass\" and value2 is one of \"on\", \
         \"off\", or \"pass\", or '{\"liquidity_baking_toggle_vote\": value}' \
         where value is one of \"on\", \"off\", or \"pass\".@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_wrong_content file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_wrong_content file_path) ;
  register_error_kind
    `Permanent
    ~id:
      "Per_block_vote_file.block_vote_file_missing_liquidity_baking_toggle_vote"
    ~title:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found"
    ~description:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[In the provided block vote file \"%s\", the \
         \"liquidity_baking_toggle_vote\" field is missing. Expecting a JSON \
         file containing '{\"liquidity_baking_toggle_vote\": value1, \
         \"adaptive_issuance_vote\": value2}' or '{\"adaptive_issuance_vote\": \
         value1, \"liquidity_baking_toggle_vote\": value2}', where value1 is \
         one of \"on\", \"off\", or \"pass\" and value2 is one of \"on\", \
         \"off\", or \"pass\", or '{\"liquidity_baking_toggle_vote\": value}' \
         where value is one of \"on\", \"off\", or \"pass\".@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_missing_liquidity_baking_toggle_vote file_path ->
          Some file_path
      | _ -> None)
    (fun file_path ->
      Block_vote_file_missing_liquidity_baking_toggle_vote file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.missing_vote_on_startup"
    ~title:"Missing vote on startup"
    ~description:
      "No CLI flag, file path, or votes file in default location provided on \
       startup"
    ~pp:(fun fmt () ->
      Format.fprintf
        fmt
        "Missing liquidity baking toggle vote, please use either the \
         --liquidity-baking-toggle-vote option, or the --votefile option or a \
         votes file in the default location: per_block_votes.json in the \
         current working directory or in the baker directory.")
    Data_encoding.empty
    (function Missing_vote_on_startup -> Some () | _ -> None)
    (fun () -> Missing_vote_on_startup)
