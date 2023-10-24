(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(* Extract profiling result for provided block hash(es)
   ------------------------
   Invocation:
     ./_build/default/devtools/testnet_experiment_tools/extract_data.exe extract \
     --profiling-dir <profiling-dir> --blocks <blocks> --output-dir <output-dir>
   Requirements:
     <profiling-dir>  - directory where the profiling reports are stored
     <blocks>         - a list of block hashes to be searched, separated by a space
     <output-dir>     - Output directory to store results
   Description:
     This file contains the script to extract all the results
     of profiling reports related to the given block hash.
     It produces one file per profiling report, for example,
     for the blocks BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw
     and BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY
     the following reports will be generated:
        - chain_validator_profiling_BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw.txt
        - chain_validator_profiling_BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY.txt
        - p2p_reader_profiling_BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw.txt
        - p2p_reader_profiling_BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY.txt
        - requester_profiling_BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw.txt
        - requester_profiling_BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY.txt
        - rpc_server_profiling_BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw.txt
        - rpc_server_profiling_BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY.txt
     all these files are generated inside the specified <output-dir> folder.
*)

open Tezos_clic
open Lwt_result_syntax

type error +=
  | No_profiling_reports_directory
  | No_output_directory
  | No_block_hashes
  | Invalid_block_hashes of string
  | Cannot_get_profiling_file_name of string

let () =
  register_error_kind
    `Permanent
    ~id:"extract_data.no_profiling_reports_directory"
    ~title:"Reports directory is not provided"
    ~description:"Argument must be a valid path to existing directory."
    ~pp:(fun ppf _profiling_dir ->
      Format.fprintf
        ppf
        "Expected a valid path to profiling directory, nothing provided.")
    Data_encoding.empty
    (function No_profiling_reports_directory -> Some () | _ -> None)
    (fun () -> No_profiling_reports_directory) ;
  register_error_kind
    `Permanent
    ~id:"extract_data.no_block_hashes"
    ~title:"Block hashes list is not provided"
    ~description:"Argument must be a list of block hashes separated by space."
    ~pp:(fun ppf _ ->
      Format.fprintf ppf "Expected list of block hashes, nothing provided.")
    Data_encoding.empty
    (function No_block_hashes -> Some () | _ -> None)
    (fun () -> No_block_hashes) ;
  register_error_kind
    `Permanent
    ~id:"extract_data.no_output_directory"
    ~title:"Output directory must be provided"
    ~description:"Argument must be a valid path to existing directory."
    ~pp:(fun ppf _ ->
      Format.fprintf
        ppf
        "Expected a valid path for profiling output, nothing provided.")
    Data_encoding.empty
    (function No_output_directory -> Some () | _ -> None)
    (fun () -> No_output_directory) ;
  register_error_kind
    `Permanent
    ~id:"extract_data.invalid_block_hashes"
    ~title:"Invalid list of block hashes."
    ~description:"Argument must be a list of block hashes separated by space."
    ~pp:(fun ppf blocks ->
      Format.fprintf
        ppf
        "Expected a list of block hashes separated by space, %s was provided \
         instead"
        blocks)
    Data_encoding.(obj1 (req "arg" string))
    (function Invalid_block_hashes s -> Some s | _ -> None)
    (fun s -> Invalid_block_hashes s) ;
  register_error_kind
    `Permanent
    ~id:"extract_data.cannot_get_profiling_file_name"
    ~title:"Cannot find type of profiling report."
    ~description:"Cannot find type of profiling report."
    ~pp:(fun ppf input_file ->
      Format.fprintf ppf "Cannot get profiling file name of %s" input_file)
    Data_encoding.(obj1 (req "arg" string))
    (function Cannot_get_profiling_file_name s -> Some s | _ -> None)
    (fun s -> Cannot_get_profiling_file_name s)

let output_directory_parameter =
  parameter @@ fun _ctxt data_dir ->
  if Sys.file_exists data_dir && Sys.is_directory data_dir then return data_dir
  else tzfail No_output_directory

let output_directory_arg =
  arg
    ~doc:"Output directory to store results."
    ~short:'O'
    ~long:"output-dir"
    ~placeholder:"output-dir-path"
    output_directory_parameter

let profiling_reports_directory_parameter =
  parameter @@ fun _ctxt data_dir ->
  if Sys.file_exists data_dir && Sys.is_directory data_dir then return data_dir
  else tzfail No_profiling_reports_directory

let profiling_reports_directory_arg =
  arg
    ~doc:"Profiling reports directory"
    ~short:'D'
    ~long:"profiling-dir"
    ~placeholder:"profiling-dir-path"
    profiling_reports_directory_parameter

let searched_blocks_parameter =
  parameter @@ fun _ p ->
  let searched_blocks = String.split_on_char ' ' p in
  match searched_blocks with
  | [] -> tzfail @@ Invalid_block_hashes p
  | searched_blocks -> return searched_blocks

let searched_blocks_arg =
  arg
    ~doc:
      "Blocks list from which ones we want the profiling results. Argument \
       must be block hashes list separetaed by white space and surrounded by \
       \", example:  \"BKtvd3iZm1P4JDcr25XpQLE7nHbyTBYD3dfU9c62hwUueHVSZMw and \
       BLGTW18zuGn7yc1SMp9DHTAVfSUYnmmWgyybEFHksGVJ6eMYNgY\""
    ~short:'B'
    ~long:"blocks"
    ~placeholder:"blocks"
    searched_blocks_parameter

let split_lines_starting_with_b input_str =
  let regexp = Str.regexp "\nB" in
  let lines = Str.split regexp input_str in
  (* Split gets rid of the searched characters, let's re-add the 'B'. *)
  let lines = List.map (fun line -> "B" ^ line) lines in
  lines

let mkdir dirname =
  try Unix.mkdir dirname 0o775 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let create_files_from_lines input_file searched_block output_directory =
  (* Get only file name. *)
  let output_file_prefix = String.split_on_char '/' input_file in
  (* Get baker name. *)
  let baker_name =
    List.hd
    @@ List.filter
         (fun s -> String.starts_with ~prefix:"baker-" s)
         output_file_prefix
  in
  (* Remove .txt. *)
  let output_file_prefix =
    String.split_on_char '.' (List.last "" output_file_prefix)
  in
  (* Get only the part before file extension. *)
  let output_file_prefix = List.hd output_file_prefix in
  match (output_file_prefix, baker_name) with
  | Some output_file_prefix, Some baker_name ->
      let in_channel = open_in input_file in
      let input_string =
        really_input_string in_channel (in_channel_length in_channel)
      in
      close_in in_channel ;
      let lines = split_lines_starting_with_b input_string in
      let extract_block_name = function [] -> "" | hd :: _ -> hd in
      return
      @@ List.iter
           (fun line ->
             (* The searched block name is always the first line. *)
             let first_line =
               extract_block_name (String.split_on_char '\n' line)
             in
             (* Luckily, its length is fixed! *)
             let block_name = String.sub first_line 0 51 in
             if
               String.starts_with ~prefix:"B" first_line
               && String.equal block_name searched_block
             then (
               let file_name =
                 Printf.sprintf "%s_%s.txt" baker_name output_file_prefix
               in
               let () = mkdir @@ output_directory ^ "/" ^ block_name in
               let out_channel =
                 open_out (output_directory ^ "/" ^ block_name ^ "/" ^ file_name)
               in
               output_string out_channel line ;
               close_out out_channel))
           lines
  | _ -> tzfail @@ Cannot_get_profiling_file_name input_file

(* Map the whole directory to find corresponding filenames. *)
let rec find_files_with_suffix dir suffix =
  let dir_contents = Sys.readdir dir in
  let matching_files = ref [] in
  Array.iter
    (fun entry ->
      let entry_path = Filename.concat dir entry in
      if Sys.is_directory entry_path then
        matching_files :=
          !matching_files @ find_files_with_suffix entry_path suffix
      else if Filename.check_suffix entry suffix then
        matching_files := entry_path :: !matching_files)
    dir_contents ;
  !matching_files

(* Find all files with [_profiling.txt] suffix in provided directory. *)
let find_and_process_profiling_file dir search_block output_directory =
  let profiling_files = find_files_with_suffix dir "_profiling.txt" in
  List.iter
    (fun profiling_file ->
      Printf.printf "Found profiling file: %s\n%!" profiling_file ;
      let _ =
        create_files_from_lines profiling_file search_block output_directory
      in
      ())
    profiling_files

let commands =
  [
    command
      ~group:
        {
          name = "devtools";
          title =
            "Command for extracting profiling reports of specified block hash";
        }
      ~desc:"Extract block profiling info."
      (args3
         profiling_reports_directory_arg
         searched_blocks_arg
         output_directory_arg)
      (fixed ["extract"])
      (fun (profiling_reports_directory, search_blocks, output_directory) _cctxt
      ->
        match
          (profiling_reports_directory, search_blocks, output_directory)
        with
        | ( Some profiling_reports_directory,
            Some search_blocks,
            Some output_directory ) ->
            return
            @@ List.iter
                 (fun search_block ->
                   find_and_process_profiling_file
                     profiling_reports_directory
                     search_block
                     output_directory)
                 search_blocks
        | None, _, _ -> tzfail No_profiling_reports_directory
        | _, None, _ -> tzfail No_block_hashes
        | _, _, None -> tzfail No_output_directory);
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
  let select_commands _ _ = return commands in
  Client_main_run.run (module Custom_client_config) ~select_commands
