(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_clic

let profiling_reports_directory_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Profiling reports directory"
    ~short:'D'
    ~long:"profiling-reports-directory"
    ~placeholder:"profiling-reports-directory-path"
    ~default:"./baker"
    ( parameter @@ fun _ data_dir ->
      if Sys.file_exists data_dir && Sys.is_directory data_dir then
        return data_dir
      else failwith "%s does not exists or is not a directory" data_dir )

let searched_block_arg =
  let open Lwt_result_syntax in
  arg
    ~doc:"Block from which one we want the profiling results"
    ~short:'B'
    ~long:"searched-block"
    ~placeholder:"searched-block-path"
    (parameter @@ fun _ searched_block -> return searched_block)

let split_lines_starting_with_b input_str =
  let regexp = Str.regexp "\nB" in
  let lines = Str.split regexp input_str in
  (* Split get rif of the searched characters, let's re-add the B. *)
  let lines = List.map (fun line -> "B" ^ line) lines in
  lines

let create_files_from_lines input_file searched_block =
  (* Get only file name. *)
  let output_file_prefix = String.split_on_char '/' input_file in
  (* Remove .txt. *)
  let output_file_prefix =
    String.split_on_char '.' (List.last "" output_file_prefix)
  in
  (* Get only the part before file extension. *)
  let output_file_prefix = List.hd output_file_prefix in
  match output_file_prefix with
  | None ->
      Stdlib.failwith @@ "Cannot get profiling file name of: " ^ input_file
  | Some output_file_prefix ->
      let in_channel = open_in input_file in
      let input_string =
        really_input_string in_channel (in_channel_length in_channel)
      in
      close_in in_channel ;
      let lines = split_lines_starting_with_b input_string in
      let extract_block_name = function [] -> "" | hd :: _ -> hd in
      List.iter
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
              Printf.sprintf "%s_%s.txt" output_file_prefix block_name
            in
            let out_channel = open_out file_name in
            output_string out_channel line ;
            close_out out_channel))
        lines

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
let find_and_process_profiling_file dir search_block =
  let profiling_files = find_files_with_suffix dir "_profiling.txt" in
  List.iter
    (fun profiling_file ->
      Printf.printf "Found profiling file: %s\n" profiling_file ;
      create_files_from_lines profiling_file search_block)
    profiling_files

let commands () =
  let open Lwt_result_syntax in
  [
    command
      ~group:
        {
          name = "devtools";
          title = "Command for extracting reports of specified block hash";
        }
      ~desc:"Extract block profiling info."
      (args2 profiling_reports_directory_arg searched_block_arg)
      (fixed ["extract"; "profiling"; "info"; "for"; "block"; "hash"])
      (fun (profiling_reports_directory, search_block) _cctxt ->
        match search_block with
        | Some search_block ->
            return
            @@ find_and_process_profiling_file
                 profiling_reports_directory
                 search_block
        | None -> failwith "No block hash specified, it is mandatory.");
  ]

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
