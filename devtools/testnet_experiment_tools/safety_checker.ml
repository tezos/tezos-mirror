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

let data_dir_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Octez data directory path"
    ~short:'D'
    ~long:"data-dir"
    ~placeholder:"data-dir-path"
    ~default:Config_file.default_data_dir
    ( parameter @@ fun _ data_dir ->
      if Sys.file_exists data_dir && Sys.is_directory data_dir then
        return data_dir
      else failwith "%s does not exists or is not a directory" data_dir )

let get_protocol_hash protocol_name =
  match String.lowercase_ascii protocol_name with
  | "nairobi" -> "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
  | "oxford" -> "ProxfordZNRgFcnNcXRSN4rtHAMFpu4w7FNjyx49pjQVU6Ww4ef"
  | "alpha" -> "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  | _ ->
      raise
        (Invalid_argument "Please specify a valid PROTOCOL name (e.g. Nairobi).")

let protocol_hash_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Protocol for the network"
    ~long:"protocol"
    ~placeholder:"protocol"
    ~default:"Alpha"
    ( parameter @@ fun _ protocol_name ->
      return @@ get_protocol_hash protocol_name )

let min_unsafe_round_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Min unsafe round value where we declare the network unsafe"
    ~long:"min-unsafe-round"
    ~placeholder:"min-unsafe-round"
    ~default:"2"
    (parameter @@ fun _ round_str -> return @@ int_of_string round_str)

let max_maybe_unsafe_ratio_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Max percentage of \"maybe unsafe\" blocks that we allow"
    ~long:"max-maybe-unsafe-ratio"
    ~placeholder:"max-maybe-unsafe-ratio"
    ~default:"1"
    (parameter @@ fun _ ratio_str -> return @@ int_of_string ratio_str)

let min_maybe_unsafe_round_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:
      "If we find more than [max_maybe_unsafe_ratio]%% of blocks with at least \
       this round value, the network is unsafe"
    ~long:"min-maybe-unsafe-round"
    ~placeholder:"min-maybe-unsafe-round"
    ~default:"1"
    (parameter @@ fun _ round_str -> return @@ int_of_string round_str)

let run_safety_check_experiment chain_store current_head protocol_hash
    min_unsafe_round max_maybe_unsafe_ratio min_maybe_unsafe_round =
  let open Lwt_result_syntax in
  let current_head_level = Int32.to_int @@ Store.Block.level current_head in

  (* [check_safety] block maybe_unsafe_blocks - check whether [block] has safe
     round and aggregates in [maybe_unsafe_blocks] the "maybe safe" blocks *)
  let rec check_safety block maybe_unsafe_blocks =
    let*! current_protocol_hash =
      Store.Block.protocol_hash_exn chain_store block
    in
    let level = Int32.to_int @@ Store.Block.level block in

    if
      String.equal
        (Protocol_hash.to_b58check current_protocol_hash)
        protocol_hash
    then (
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
                ^ string_of_int level ^ "\n"))
      in

      if round_int >= min_unsafe_round then (
        Format.printf
          "Experiment found that block at level %d has round %d, so we stop.\n"
          level
          round_int ;
        return (maybe_unsafe_blocks, level))
      else
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
            Format.printf "No predecessor for block at level: %d\n" level ;
            Lwt_exit.exit_and_raise 1)
    else (
      Format.printf
        "Found protocol hash: %s\n"
        (Protocol_hash.to_b58check current_protocol_hash) ;
      Format.printf "So we stopped.\n" ;
      return (maybe_unsafe_blocks, level))
  in
  Format.printf "Start safety checking:\n" ;
  let* maybe_unsafe_blocks, last_block_level = check_safety current_head 0 in
  let total_number_of_blocks = current_head_level - last_block_level + 1 in

  (* Check that there are no more than [max_maybe_unsafe_ratio]% of blocks where
     consensus was reached at round = [min_maybe_unsafe_round] *)
  if maybe_unsafe_blocks * 100 > total_number_of_blocks * max_maybe_unsafe_ratio
  then
    Format.printf
      "Experiment found that more than %d%% blocks have round = %d.\n"
      max_maybe_unsafe_ratio
      min_maybe_unsafe_round
  else Format.printf "Experiment finished: Network is safe!\n" ;
  Format.printf "Done!\n" ;
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
      (args5
         data_dir_arg
         protocol_hash_arg
         min_unsafe_round_arg
         max_maybe_unsafe_ratio_arg
         min_maybe_unsafe_round_arg)
      (fixed ["check"])
      (fun
        ( data_dir,
          protocol_hash,
          min_unsafe_round,
          max_maybe_unsafe_ratio,
          min_maybe_unsafe_round )
        _cctxt
      ->
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
          protocol_hash
          min_unsafe_round
          max_maybe_unsafe_ratio
          min_maybe_unsafe_round);
  ]

let run () =
  let argv = Sys.argv |> Array.to_list |> List.tl |> Option.value ~default:[] in
  Tezos_clic.dispatch commands () argv

let () =
  match Lwt_main.run (run ()) with
  | Ok () -> ()
  | Error trace -> Format.printf "ERROR: %a%!" Error_monad.pp_print_trace trace
