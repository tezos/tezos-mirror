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

type block_kind = Safe | Maybe_unsafe | Unsafe

type block_info = {
  block_hash : Block_hash.t;
  level : int;
  round : int;
  number_of_manager_ops : int;
  kind : block_kind;
}

type stats = {min : int; max : int; median : float; avg : float}

let pp_stats fmt {min; max; median; avg} =
  Format.fprintf
    fmt
    "@[<v 0>Minimum: %d,@ Maximum: %d,@ Median: %f,@ Average: %f@]"
    min
    max
    median
    avg

module Block_kind_map = Map.Make (struct
  type t = block_kind

  let compare k1 k2 =
    let score k = match k with Safe -> 0 | Maybe_unsafe -> 1 | Unsafe -> 2 in
    Int.compare (score k1) (score k2)
end)

let pp_block_kind fmt kind =
  match kind with
  | Safe -> Format.fprintf fmt "safe"
  | Maybe_unsafe -> Format.fprintf fmt "potentially unsafe"
  | Unsafe -> Format.fprintf fmt "unsafe"

let pp_block_info fmt {block_hash; level; round; number_of_manager_ops; kind} =
  let custom_break =
    Format.pp_print_custom_break ~fits:(",", 1, "") ~breaks:("", 0, "")
  in
  Format.fprintf
    fmt
    "@[<hov 2>Block: %a%tLevel: %d%tRound: %d%tManager operations: \
     %d%tClassified as: %a@]"
    Block_hash.pp
    block_hash
    custom_break
    level
    custom_break
    round
    custom_break
    number_of_manager_ops
    custom_break
    pp_block_kind
    kind

type error +=
  | Invalid_positive_int_parameter of string
  | Invalid_protocol_parameter of string
  | Cannot_extract_manager_operations of Block_hash.t * int
  | Cannot_retrieve_round_of_block of Block_hash.t * int

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
    (fun s -> Invalid_protocol_parameter s) ;
  register_error_kind
    `Permanent
    ~id:"safety_checker.cannot_extract_manager_operations"
    ~title:"Cannot extract manager operations from block"
    ~description:"Cannot extract manager operations from block"
    ~pp:(fun ppf (block_hash, level) ->
      Format.fprintf
        ppf
        "Cannot extract manager operations from block %a at level %d"
        Block_hash.pp
        block_hash
        level)
    Data_encoding.(
      obj2 (req "block_hash" Block_hash.encoding) (req "level" int31))
    (function
      | Cannot_extract_manager_operations (block_hash, level) ->
          Some (block_hash, level)
      | _ -> None)
    (fun (block_hash, level) ->
      Cannot_extract_manager_operations (block_hash, level)) ;
  register_error_kind
    `Permanent
    ~id:"safety_checker.no_block_stored_for_level"
    ~title:"No block stored for given level"
    ~description:"No block stored for given level"
    ~pp:(fun ppf (block_hash, level) ->
      Format.fprintf
        ppf
        "No block stored for block %a level %d"
        Block_hash.pp
        block_hash
        level)
    Data_encoding.(
      obj2 (req "block_hash" Block_hash.encoding) (req "level" int31))
    (function
      | Cannot_retrieve_round_of_block (block_hash, level) ->
          Some (block_hash, level)
      | _ -> None)
    (fun (block_hash, level) ->
      Cannot_retrieve_round_of_block (block_hash, level))

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

let get_stats ~extract_metric blocks_info =
  let blocks_metrics =
    blocks_info |> List.map extract_metric |> List.sort Int.compare
  in
  let number_of_blocks = List.length blocks_metrics in
  let median =
    if number_of_blocks = 0 then None
    else if number_of_blocks mod 2 = 1 then
      List.nth blocks_metrics (number_of_blocks / 2) |> Option.map Int.to_float
    else
      let lower_median = List.nth blocks_metrics (number_of_blocks / 2) in
      let upper_median = List.nth blocks_metrics ((number_of_blocks / 2) + 1) in
      Option.bind lower_median (fun lm ->
          Option.map
            (fun um -> (Int.to_float lm +. Int.to_float um) /. 2.)
            upper_median)
  in
  let stats =
    List.fold_left
      (fun stats block_manager_ops ->
        match stats with
        | None -> Some (block_manager_ops, block_manager_ops, block_manager_ops)
        | Some (min_manager_ops, max_manager_ops, total_manager_ops) ->
            Some
              ( Int.min min_manager_ops block_manager_ops,
                Int.max max_manager_ops block_manager_ops,
                total_manager_ops + block_manager_ops ))
      None
      blocks_metrics
  in
  Option.map
    (fun (min, max, total) ->
      {
        min;
        max;
        median = Option.value median ~default:0.;
        avg = Int.to_float total /. (Int.to_float @@ number_of_blocks);
      })
    stats

let display_stats metric_name ~extract_metric ~safe_blocks ~maybe_unsafe_blocks
    ~unsafe_blocks ~min_maybe_unsafe_round ~min_unsafe_round =
  let safe_blocks_manager_ops_stats =
    get_stats
      ~extract_metric:(fun {number_of_manager_ops; _} -> number_of_manager_ops)
      safe_blocks
  in
  let maybe_unsafe_blocks_stats =
    get_stats ~extract_metric maybe_unsafe_blocks
  in
  let unsafe_blocks_stats = get_stats ~extract_metric unsafe_blocks in
  let all_blocks_manager_ops_stats =
    get_stats
      ~extract_metric:(fun {number_of_manager_ops; _} -> number_of_manager_ops)
      (safe_blocks @ unsafe_blocks @ maybe_unsafe_blocks)
  in
  Format.printf "@[<v 2>%s:@," metric_name ;
  Option.iter
    (fun stats -> Format.printf "@[<v 2>Safe blocks:@,%a.@]@," pp_stats stats)
    safe_blocks_manager_ops_stats ;
  Option.iter
    (fun stats ->
      Format.printf
        "@[<v 2>Maybe unsafe blocks (round >= %d):@,%a.@]@,"
        min_maybe_unsafe_round
        pp_stats
        stats)
    maybe_unsafe_blocks_stats ;
  Option.iter
    (fun stats ->
      Format.printf
        "@[<v 2>Unsafe blocks (round >= %d):@,%a.@]@,"
        min_unsafe_round
        pp_stats
        stats)
    unsafe_blocks_stats ;
  Option.iter
    (fun stats -> Format.printf "@[<v 2>All blocks:@,%a.@]@,@." pp_stats stats)
    all_blocks_manager_ops_stats

let classify_block ~min_unsafe_round ~min_maybe_unsafe_round block =
  let open Lwt_result_syntax in
  let level = Int32.to_int @@ Store.Block.level block in
  (* Operations in a block are divided in 4 categories, ordered from highest to
     lowest priority: Consensus, Voting, Anonymous and Manager operations. *)
  let manager_ops_idx = 3 in
  let* manager_ops =
    match List.nth (Store.Block.operations block) manager_ops_idx with
    | None ->
        tzfail
        @@ Cannot_extract_manager_operations (Store.Block.hash block, level)
    | Some n -> return n
  in
  let number_of_manager_ops = List.length manager_ops in
  let fitness = Store.Block.fitness block in
  (* Fitness = [consensus_number, block_level, opt(locked_round), pred_round, round] *)
  let round_idx = 4 in
  let* round =
    match List.nth fitness round_idx with
    | Some round ->
        let (`Hex round) = Hex.of_bytes round in
        return @@ int_of_string round
    | None ->
        tzfail @@ Cannot_retrieve_round_of_block (Store.Block.hash block, level)
  in
  let block_kind =
    if round >= min_unsafe_round then Unsafe
    else if round >= min_maybe_unsafe_round then Maybe_unsafe
    else Safe
  in
  return
    {
      block_hash = Store.Block.hash block;
      round;
      level;
      number_of_manager_ops;
      kind = block_kind;
    }

let run_safety_check_experiment chain_store current_head protocol_hash_opt
    min_unsafe_round max_maybe_unsafe_ratio min_maybe_unsafe_round
    min_block_level_opt =
  let open Lwt_result_syntax in
  (* [check_safety block block_info_map] classifies blocks in the canonical
      chain in one of three different categories, according to their round:
      {ul
        {li [Safe] if the block round is strictly less than
            [min_maybe_unsafe_round], }
        {li [Maybe_unsafe] if the block round is greater or equal than
            [min_maybe_unsafe_round], but strictly less than
             [min_unsafe_round], }
        {li [Unsafe] if the block round is greater of equal than
            [min_unsafe_round].}
      }
      The classification of blocks also include other information, such as
      the round and level of the block, and the number of manager operations.
  *)
  let rec check_safety block block_info_map =
    let*! current_protocol_hash =
      Store.Block.protocol_hash_exn chain_store block
    in
    let* block_info =
      classify_block ~min_unsafe_round ~min_maybe_unsafe_round block
    in
    let protocol_should_check_block =
      match protocol_hash_opt with
      | None -> true
      | Some protocol_hash ->
          Protocol_hash.equal current_protocol_hash protocol_hash
    in
    let level_should_check_block =
      match min_block_level_opt with
      | None -> true
      | Some min_block_level -> block_info.level >= min_block_level
    in
    let should_check_block =
      protocol_should_check_block && level_should_check_block
    in

    if should_check_block then (
      let block_info_map =
        Block_kind_map.update
          block_info.kind
          (function
            | None -> Some [block_info]
            | Some blocks_info -> Some (block_info :: blocks_info))
          block_info_map
      in

      let*! predecessor_block =
        Store.Block.read_predecessor chain_store block
      in
      match predecessor_block with
      | Ok predecessor_block -> check_safety predecessor_block block_info_map
      | Error _ ->
          Format.printf
            "Stopping block classification. No predecessor for block at level: \
             %d@,\
             @."
            block_info.level ;
          return block_info_map)
    else (
      if not protocol_should_check_block then
        Format.printf
          "Finished classifying blocks for protocol: %a@,@."
          Protocol_hash.pp
          current_protocol_hash ;

      if not level_should_check_block then
        Format.printf
          "Finished classifying blocks at level: %d@,@."
          block_info.level ;

      return block_info_map)
  in

  let* blocks_info = check_safety current_head Block_kind_map.empty in
  let total_number_of_blocks =
    blocks_info |> Block_kind_map.to_seq |> List.of_seq |> List.map snd
    |> List.flatten |> List.length
  in
  let safe_blocks =
    Block_kind_map.find Safe blocks_info |> Option.value ~default:[]
  in
  let maybe_unsafe_blocks =
    Block_kind_map.find Maybe_unsafe blocks_info |> Option.value ~default:[]
  in
  let unsafe_blocks =
    Block_kind_map.find Unsafe blocks_info |> Option.value ~default:[]
  in

  Format.printf
    "@[<v 2>The following blocks have been found to be potentially unsafe \
     (round >= %d):@,"
    min_maybe_unsafe_round ;
  List.iter
    (fun block_info -> Format.printf "%a.@," pp_block_info block_info)
    maybe_unsafe_blocks ;
  Format.printf "@." ;
  Format.printf
    "@[<v 2>The following blocks have been found to be unsafe (round >= %d):@,"
    min_unsafe_round ;
  List.iter
    (fun block_info -> Format.printf "%a.@," pp_block_info block_info)
    unsafe_blocks ;
  Format.printf "@." ;
  Format.printf
    "@[<h 2>Total number of blocks checked:@ %d.@]@,@."
    total_number_of_blocks ;

  let safe_blocks_number = List.length safe_blocks in
  let safe_blocks_ratio =
    Int.to_float safe_blocks_number
    *. 100.
    /. Int.to_float total_number_of_blocks
  in
  let unsafe_blocks_number = List.length unsafe_blocks in
  let unsafe_blocks_ratio =
    Int.to_float unsafe_blocks_number
    *. 100.
    /. Int.to_float total_number_of_blocks
  in
  let either_unsafe_or_maybe_unsafe_blocks_number =
    unsafe_blocks_number + List.length maybe_unsafe_blocks
  in
  let either_unsafe_or_maybe_unsafe_blocks_ratio =
    Int.to_float either_unsafe_or_maybe_unsafe_blocks_number
    *. 100.
    /. Int.to_float total_number_of_blocks
  in
  Format.printf "@[<v 2>Round statistics:@," ;
  Format.printf
    "@[<h 1>Blocks classified as safe (round < %d):@ %d (%f%%).@]@,"
    min_maybe_unsafe_round
    safe_blocks_number
    safe_blocks_ratio ;
  Format.printf
    "@[<h 1>Blocks classified as unsafe (round >= %d):@ %d (%f%%).@]@,"
    min_unsafe_round
    unsafe_blocks_number
    unsafe_blocks_ratio ;
  Format.printf
    "@[<h 1>Blocks classified as either unsafe or potentially unsafe (round >= \
     %d):@ %d (%f%%).@]@,\
     @."
    min_maybe_unsafe_round
    either_unsafe_or_maybe_unsafe_blocks_number
    either_unsafe_or_maybe_unsafe_blocks_ratio ;

  (* Check that there are no more than [max_maybe_unsafe_ratio]% of blocks where
     consensus was reached at round = [min_maybe_unsafe_round] *)
  let display_stats =
    display_stats
      ~min_unsafe_round
      ~min_maybe_unsafe_round
      ~safe_blocks
      ~unsafe_blocks
      ~maybe_unsafe_blocks
  in
  display_stats
    "Manager operations"
    ~extract_metric:(fun {number_of_manager_ops; _} -> number_of_manager_ops) ;

  let is_network_safe =
    unsafe_blocks_number = 0
    && either_unsafe_or_maybe_unsafe_blocks_number * 100
       <= total_number_of_blocks * max_maybe_unsafe_ratio
  in
  Format.printf
    "Experiment finished: Network safety result = %b.@."
    is_network_safe ;
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
