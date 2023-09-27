(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Safety checker tool
   ------------------------
   Invocation:
     ./_build/default/devtools/testnet_experiment_tools/safety_checker.exe check \
     --data-dir <data_dir> \
     --protocol <protocol> \
     --min-unsafe-round <min_unsafe_round> \
     --max-maybe-unsafe-ratio <max_maybe_unsafe_ratio> \
     --min-maybe-unsafe-round <min_maybe_unsafe_round>
   Requirements:
     <data-dir>               - directory where the node context is stored
     <protocol>               - the protocol used for the network
     <min-unsafe-round>       - all blocks are required to have round less than it
     <max-maybe-unsafe-ratio> - no more than this percentage of blocks should have
                                at least [min-maybe-unsafe-round] round
     <min-maybe-unsafe-round> - limit the number of blocks with at least this round
   Description:
     This file contains the tool for checking that the network
     obtained from the "reduce block time" experiment is "safe".
*)

open Filename.Infix
open Tezos_clic

type error +=
  | Invalid_positive_int_parameter of string
  | Invalid_protocol_parameter of string

let () =
  register_error_kind
    `Permanent
    ~id:"safety_checker.invalid_positive_int_parameter"
    ~title:"Argument is not a positive integer"
    ~description:"Argument must be a positive integer"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Expected a valid positive integer, provided %s instead"
        reveal_data_path)
    Data_encoding.(obj1 (req "arg" string))
    (function Invalid_positive_int_parameter s -> Some s | _ -> None)
    (fun s -> Invalid_positive_int_parameter s) ;
  register_error_kind
    `Permanent
    ~id:"safety_checker.invalid_protocol_parameter"
    ~title:"Argument is not a valid protocol name"
    ~description:"Argument must be either \"oxford\", \"nairobi\", or \"alpha\""
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Expected one of these protocol names: \"oxford\", \"nairobi\", \
         \"alpha\". %s was provided instead"
        reveal_data_path)
    Data_encoding.(obj1 (req "arg" string))
    (function Invalid_protocol_parameter s -> Some s | _ -> None)
    (fun s -> Invalid_protocol_parameter s)

let data_dir_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Octez data directory path"
    ~short:'D'
    ~long:"data-dir"
    ~placeholder:"data-dir-path"
    ~default:Config_file.default_data_dir
    (parameter @@ fun _ data_dir -> return data_dir)

let positive_int_parameter =
  parameter (fun _cctxt p ->
      let open Lwt_result_syntax in
      let* i =
        try return (int_of_string p)
        with _ -> tzfail @@ Invalid_positive_int_parameter p
      in
      if i <= 0 then tzfail @@ Invalid_positive_int_parameter p else return i)

let protocol_hash_parameter =
  parameter (fun _cctxt p ->
      let open Lwt_result_syntax in
      match String.lowercase_ascii p with
      | "nairobi" ->
          return
          @@ Protocol_hash.of_b58check_exn
               "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
      | "oxford" ->
          return
          @@ Protocol_hash.of_b58check_exn
               "ProxfordZNRgFcnNcXRSN4rtHAMFpu4w7FNjyx49pjQVU6Ww4ef"
      | "alpha" ->
          return
          @@ Protocol_hash.of_b58check_exn
               "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
      | _ -> tzfail @@ Invalid_protocol_parameter p)

let protocol_hash_arg =
  arg
    ~doc:
      "Protocol for the network. Once a block from a different protocol is \
       encountered, the safety-checker ends. Options available are: oxford, \
       nairobi, alpha. If this argument is not provided, the protocol will not \
       be checked."
    ~long:"protocol"
    ~placeholder:"protocol"
    protocol_hash_parameter

let min_unsafe_round_arg =
  default_arg
    ~doc:"Min unsafe round value where we declare the network unsafe."
    ~long:"min-unsafe-round"
    ~placeholder:"min-unsafe-round"
    ~default:"2"
    positive_int_parameter

let max_maybe_unsafe_ratio_arg =
  default_arg
    ~doc:"Max percentage of \"maybe unsafe\" blocks that we allow."
    ~long:"max-maybe-unsafe-ratio"
    ~placeholder:"max-maybe-unsafe-ratio"
    ~default:"1"
    positive_int_parameter

let min_maybe_unsafe_round_arg =
  default_arg
    ~doc:
      "If we find more than [max_maybe_unsafe_ratio]% of blocks with at least \
       this round value, the network is unsafe."
    ~long:"min-maybe-unsafe-round"
    ~placeholder:"min-maybe-unsafe-round"
    ~default:"1"
    positive_int_parameter

let min_block_level_arg =
  arg
    ~doc:
      "The minimum level for blocks to be considered in the safety check. When \
       a block with a lower level is found, the safety checker stops."
    ~long:"min-block-level"
    ~placeholder:"min-block-level"
    positive_int_parameter

let run_safety_check_experiment chain_store current_head protocol_hash_opt
    min_unsafe_round max_maybe_unsafe_ratio min_maybe_unsafe_round
    min_block_level_opt =
  let open Lwt_result_syntax in
  let unsafe_blocks = ref 0 in
  let is_network_safe = ref true in
  let current_head_level = Int32.to_int @@ Store.Block.level current_head in

  (* [check_safety] block maybe_unsafe_blocks - check whether [block] has safe
     round and aggregates in [maybe_unsafe_blocks] the "maybe safe" blocks *)
  let rec check_safety block maybe_unsafe_blocks =
    let*! current_protocol_hash =
      Store.Block.protocol_hash_exn chain_store block
    in
    let level = Int32.to_int @@ Store.Block.level block in

    let protocol_should_check_block =
      match protocol_hash_opt with
      | None -> true
      | Some protocol_hash ->
          Protocol_hash.equal current_protocol_hash protocol_hash
    in
    let level_should_check_block =
      match min_block_level_opt with
      | None -> true
      | Some min_block_level -> level >= min_block_level
    in
    let should_check_block =
      protocol_should_check_block && level_should_check_block
    in

    if should_check_block then (
      let fitness = Store.Block.fitness block in
      (* Fitness = [consensus_number, block_level, opt(locked_round), pred_round, round] *)
      let round_idx = 4 in
      let round_int =
        match List.nth fitness round_idx with
        | Some round ->
            let (`Hex round) = Hex.of_bytes round in
            int_of_string round
        | None ->
            raise
              (Invalid_argument
                 ("No valid round was found for block at level: "
                ^ string_of_int level))
      in

      if round_int >= min_unsafe_round then (
        unsafe_blocks := !unsafe_blocks + 1 ;
        is_network_safe := false ;
        Format.printf
          "Block %s at level %d has round %d, greater or equal to \
           min_unsafe_round = %d.@."
          (Block_hash.to_b58check (Store.Block.hash block))
          level
          round_int
          min_unsafe_round) ;

      let maybe_unsafe_blocks =
        if round_int >= min_maybe_unsafe_round then maybe_unsafe_blocks + 1
        else maybe_unsafe_blocks
      in

      let*! predecessor_block =
        Store.Block.read_predecessor chain_store block
      in
      match predecessor_block with
      | Ok predecessor_block ->
          check_safety predecessor_block maybe_unsafe_blocks
      | Error _ ->
          Format.printf "No predecessor for block at level: %d@." level ;
          return (maybe_unsafe_blocks, level))
    else (
      if not protocol_should_check_block then
        Format.printf
          "Found different protocol hash: %a@."
          Protocol_hash.pp
          current_protocol_hash ;

      if not level_should_check_block then
        Format.printf "Found block level: %d@." level ;
      Format.printf "So we stopped.@." ;

      return (maybe_unsafe_blocks, level))
  in

  Format.printf "Start safety checking:@." ;
  let* maybe_unsafe_blocks, last_block_level = check_safety current_head 0 in
  let total_number_of_blocks = current_head_level - last_block_level + 1 in
  Format.printf "Total number of blocks checked: %d.@." total_number_of_blocks ;

  (* Check that there are no more than [max_maybe_unsafe_ratio]% of blocks where
     consensus was reached at round = [min_maybe_unsafe_round] *)
  if maybe_unsafe_blocks * 100 > total_number_of_blocks * max_maybe_unsafe_ratio
  then (
    is_network_safe := false ;
    Format.printf
      "More than %d%% blocks have round = %d.@."
      max_maybe_unsafe_ratio
      min_maybe_unsafe_round ;
    Format.printf
      "Number of blocks with round %d : %d.@."
      min_maybe_unsafe_round
      maybe_unsafe_blocks ;
    Format.printf
      "Percentage of blocks with round %d : %f%% @."
      min_maybe_unsafe_round
      (Int.to_float maybe_unsafe_blocks
      *. 100.
      /. Int.to_float total_number_of_blocks)) ;

  if !is_network_safe then
    Format.printf "Experiment finished: Network safety result = TRUE.@."
  else (
    Format.printf
      "Number of blocks with round at least %d : %d.@."
      min_unsafe_round
      !unsafe_blocks ;
    Format.printf
      "Percentage of blocks with round at least %d : %f%% @."
      min_unsafe_round
      (Int.to_float !unsafe_blocks
      *. 100.
      /. Int.to_float total_number_of_blocks) ;
    Format.printf "Experiment finished: Network safety result = FALSE.@.") ;

  return_unit

let commands =
  let open Lwt_result_syntax in
  [
    command
      ~group:
        {
          name = "devtools";
          title = "Command for checking the safety of the network";
        }
      ~desc:"Safety checking of the network."
      (args6
         data_dir_arg
         protocol_hash_arg
         min_unsafe_round_arg
         max_maybe_unsafe_ratio_arg
         min_maybe_unsafe_round_arg
         min_block_level_arg)
      (fixed ["check"])
      (fun
        ( data_dir,
          protocol_hash_opt,
          min_unsafe_round,
          max_maybe_unsafe_ratio,
          min_maybe_unsafe_round,
          min_block_level_opt )
        _cctxt
      ->
        let* () =
          if Sys.file_exists data_dir && Sys.is_directory data_dir then
            return_unit
          else failwith "%s does not exists or is not a directory" data_dir
        in
        let* _, config =
          Shared_arg.resolve_data_dir_and_config_file ~data_dir ()
        in
        let* store =
          Store.init
            ~readonly:true
            ~store_dir:(data_dir // "store")
            ~context_dir:(data_dir // "context")
            ~allow_testchains:false
            config.blockchain_network.genesis
        in
        let chain_store = Store.main_chain_store store in
        let*! current_head = Store.Chain.current_head chain_store in
        run_safety_check_experiment
          chain_store
          current_head
          protocol_hash_opt
          min_unsafe_round
          max_maybe_unsafe_ratio
          min_maybe_unsafe_round
          min_block_level_opt);
  ]

module Custom_client_config : Client_main_run.M = struct
  type t = unit

  let default_base_dir = "/tmp"

  let global_options () = args1 @@ constant ()

  let parse_config_args ctx argv =
    let open Lwt_result_syntax in
    let* (), remaining =
      Tezos_clic.parse_global_options (global_options ()) ctx argv
    in
    let open Client_config in
    return (default_parsed_config_args, remaining)

  let default_chain = `Main

  let default_block = `Head 0

  let default_daily_logs_path = None

  let default_media_type = Tezos_rpc_http.Media_type.Command_line.Binary

  let other_registrations = None

  let clic_commands ~base_dir:_ ~config_commands:_ ~builtin_commands:_
      ~other_commands:_ ~require_auth:_ =
    commands

  let logger = None
end

let () =
  let open Lwt_result_syntax in
  let select_commands _ctx _ = return commands in
  Client_main_run.run (module Custom_client_config) ~select_commands
