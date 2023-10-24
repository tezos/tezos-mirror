(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Teztale querying tool
   ------------------------
   Invocation:
   1. ./_build/default/devtools/testnet_experiment_tools/get_teztale_data.exe \
      canonical_chain_query \
      --db-path <db-path> \
      [--print ]
   2. ./_build/default/devtools/testnet_experiment_tools/get_teztale_data.exe \
      reorganised_blocks_query \
      --db-path <db-path> \
      [--print]
   3. ./_build/default/devtools/testnet_experiment_tools/get_teztale_data.exe \
      baker_nodes_query \
      --db-path <db-path> \
      --experiment-dir <experiment-dir> \
      --delegates-path <path-to-delegates>
      [--print]
   Requirements:
     <db-path> - path to the teztale database
     <experiment-dir> - path to experiment data folder, which contains the bakers
     <path-to-delegates> - path to baker addresses, starting from baker directory
     [<print>] - if this flag is set, we print the result of the query
   Description:
     This file contains the tool for querying the teztale database.
     The queries that it provides are:
     - canonical_chain_query : get table (id, block_id, predecessor) of block ids in the
     increasing order as they appear in the canonical chain from the db;
     - reorganised_blocks_query : get table (id, block_id, level, round) of block ids
     which are not part of the canonical chain, together with more detailed info.
     - baker_nodes_query : get table (id, pod_name) of baker nodes which took part
     in the experiment, additionally get table (id, baker_id, delegate_id) which
     links these bakers to their respective delegates
*)

open Filename.Infix
open Tezos_clic
open Teztale_sql_queries
module Query_map = Map.Make (Int)
module Delegates_map = Map.Make (String)
module Query_set = Set.Make (String)

let group = {name = "devtools"; title = "Command for querying the teztale db"}

(* Errors. *)

type error +=
  | Db_path of string
  | Caqti_db_connection of string
  | Canonical_chain_head of string
  | Canonical_chain_query of string
  | Reorganised_blocks_query of string
  | Baker_nodes_query of string
  | Delegates_of_baker_query of string
  | Experiment_dir of string

let () =
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.db_path"
    ~title:"Teztale database path provided was invalid"
    ~description:"Teztale database path must be valid"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "%s is not a valid path to a teztale db" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Db_path s -> Some s | _ -> None)
    (fun s -> Db_path s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.caqti_db_connection"
    ~title:"Connection to teztale db failed"
    ~description:"Connection to teztale db must be achieved"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Expected to connect to teztale db: %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Caqti_db_connection s -> Some s | _ -> None)
    (fun s -> Caqti_db_connection s) ;
  register_error_kind
    `Permanent
    ~id:"get_tezale_data.canonical_chain_head"
    ~title:"Failed to obtain the head of the canonical chain"
    ~description:"Canonical chain head is required"
    ~pp:(fun ppf s -> Format.fprintf ppf "Expected canonical chain head: %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Canonical_chain_head s -> Some s | _ -> None)
    (fun s -> Canonical_chain_head s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.canonical_chain_query"
    ~title:"Failed to create canonical_chain table"
    ~description:"canonical_chain table must be created"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "canonical_chain table could not be created : %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Canonical_chain_query s -> Some s | _ -> None)
    (fun s -> Canonical_chain_query s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.reorganised_blocks_query"
    ~title:"Failed to create reorganised_blocks table"
    ~description:"reorganised_blocks table must be created"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "reorganised_blocks table could not be created : %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Reorganised_blocks_query s -> Some s | _ -> None)
    (fun s -> Reorganised_blocks_query s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.baker_nodes_query"
    ~title:"Failed to create baker_nodes table"
    ~description:"baker_nodes table must be created"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "baker_nodes table could not be created : %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Baker_nodes_query s -> Some s | _ -> None)
    (fun s -> Baker_nodes_query s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.delegates_of_baker_query"
    ~title:"Failed to create delegates_of_baker_query table"
    ~description:"delegates_of_baker_query table must be created"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "delegates_of_baker_query table could not be created : %s"
        s)
    Data_encoding.(obj1 (req "arg" string))
    (function Delegates_of_baker_query s -> Some s | _ -> None)
    (fun s -> Delegates_of_baker_query s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.experiment_dir"
    ~title:"Experiment directory path provided was invalid"
    ~description:"Experiment directory path must be valid"
    ~pp:(fun ppf s -> Format.fprintf ppf "%s is not a valid path" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Experiment_dir s -> Some s | _ -> None)
    (fun s -> Experiment_dir s)

(* Aggregators & helpers. *)

let add_head_id block_id acc =
  acc := Some block_id ;
  acc

let add_canonical_chain_row (block_id, predecessor) acc =
  Query_map.add block_id predecessor acc

let add_reorganised_blocks_row (block_id, block_hash, level, round) acc =
  Query_map.add block_id (Block_hash.to_b58check block_hash, level, round) acc

let add_delegates_row (id, address) acc = Query_map.add id address acc

(** [find_key_by_value]: searches the first key in [map] with 
    corresponding value [v] *)
let find_key_by_value map v =
  Query_map.filter (fun _key value -> value = v) map
  |> Query_map.to_seq |> List.of_seq
  |> fun lst -> List.hd lst |> Option.map fst

(* Query helper functions. *)

(** [get_head_id] obtains the unique block id that is the predecessor of the
    predecessor of the block with highest level from the teztale db; the 
    uniqueness property is due to the tenderbake block finality 2 *)
let get_head_id db_pool =
  let open Lwt_result_syntax in
  let*! head_id_ref =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold get_canonical_chain_head_id_query add_head_id () (ref None))
      db_pool
  in
  match head_id_ref with
  | Error e -> tzfail (Canonical_chain_head (Caqti_error.show e))
  | Ok head_id_ref -> (
      match !head_id_ref with
      | None ->
          tzfail (Canonical_chain_head "Cannot retrieve canonical chain head")
      | Some head_id -> return head_id)

(** [create_table]: creates a table in the teztale db according to [query] *)
let create_table ~db_pool ~query ~query_error =
  let open Lwt_result_syntax in
  let*! query_result =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec query ())
      db_pool
  in
  match query_result with
  | Error e -> tzfail (query_error (Caqti_error.show e))
  | Ok () -> return_unit

(** [get_entries]: obtains the entries that will populate a new table in
    the teztale db according to [query]
    - [db_pool]: the teztale db we connected to
    - [add_to_map]: method to aggregate the entries for each row from 
    the result of [query] into a Query_map object initialised as empty
    - [arg]: additional input argument for [query]
    - [query_error]: depending on the command, we have a different error *)
let get_entries ~db_pool ~query ~add_to_map ~arg ~query_error =
  let open Lwt_result_syntax in
  let*! query_result =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold query add_to_map arg Query_map.empty)
      db_pool
  in
  match query_result with
  | Error e -> tzfail (query_error (Caqti_error.show e))
  | Ok map -> return map

(** [insert_entry]: adds a row in the corresponding teztale db, according
    to [query] with values from [entry] 
    - [entry]: tuple of values matching the schema of the table from [query] *)
let insert_entry ~db_pool ~query ~entry ~query_error =
  let open Lwt_result_syntax in
  let*! query_result =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec query entry)
      db_pool
  in
  match query_result with
  | Error e -> tzfail (query_error (Caqti_error.show e))
  | Ok () -> return_unit

(* Printing. *)

(** [print_canonical_block]: print the current (canonical) block 
    - [map]: maps a block id to the id of its predecessor
    - [current_predecessor]: optional value of the predecessor of the 
    current block that we are printing (None for the first block) *)
let rec print_canonical_block ~map ~current_predecessor =
  let current_block_opt = find_key_by_value map current_predecessor in
  match (current_block_opt, current_predecessor) with
  | None, None -> Format.printf "No canonical chain found!@."
  | None, _ -> () (* stop printing *)
  | Some current_block, current_predecessor ->
      if current_predecessor = None then Format.printf "%d@," current_block
      else Format.printf " --> %d@," current_block ;
      print_canonical_block ~map ~current_predecessor:current_block_opt

let print_reorganised_blocks =
  Query_map.iter (fun block_id (block_hash, level, round) ->
      Format.printf
        "Reorganised block with ID: %d with hash: %s at level: %d with round: \
         %d@."
        block_id
        block_hash
        level
        round)

let print_baker_nodes map =
  Delegates_map.iter
    (fun baker delegate_addresses ->
      Format.printf "Baker: %s@." baker ;
      List.iter
        (fun delegate_address ->
          Format.printf "%s @,"
          @@ Tezos_crypto.Signature.Public_key_hash.to_b58check delegate_address)
        delegate_addresses ;
      Format.printf "@.")
    map

(* Commands. *)

(** [connect_db]: establishes connection with the (sqlite) teztale db
    - [db_path]: path to teztale db *)
let connect_db db_path =
  let open Lwt_result_syntax in
  let db_uri = Uri.of_string ("sqlite3:" ^ db_path) in
  match Caqti_lwt.connect_pool db_uri with
  | Error e -> tzfail (Caqti_db_connection (Caqti_error.show e))
  | Ok db_pool -> return db_pool

let canonical_chain_command db_path print_result =
  let open Lwt_result_syntax in
  let* db_pool = connect_db db_path in
  (* 1. Obtain the head id of the canonical chain *)
  let* head_id = get_head_id db_pool in
  (* 2. Create canonical_chain table in teztale db *)
  let* () =
    create_table
      ~db_pool
      ~query:create_canonical_chain_table_query
      ~query_error:(fun e -> Canonical_chain_query e)
  in
  (* 3. Retrieve the entries which form the canonical chain *)
  let* map =
    get_entries
      ~db_pool
      ~query:get_canonical_chain_entries_query
      ~add_to_map:add_canonical_chain_row
      ~arg:head_id
      ~query_error:(fun e -> Canonical_chain_query e)
  in
  (* 4. Optional printing of the result *)
  if print_result then print_canonical_block ~map ~current_predecessor:None ;
  (* 5. Populate the canonical_chain table *)
  Query_map.iter_es
    (fun block_id predecessor ->
      insert_entry
        ~db_pool
        ~query:insert_canonical_chain_entry_query
        ~entry:(block_id, Option.value ~default:(-1) predecessor)
        ~query_error:(fun e -> Canonical_chain_query e))
    map

let reorganised_blocks_command db_path print_result =
  let open Lwt_result_syntax in
  let* db_pool = connect_db db_path in
  (* 1. Obtain the head id of the canonical chain *)
  let* head_id = get_head_id db_pool in
  (* 2. Create reorganised_blocks table in teztale db *)
  let* () =
    create_table
      ~db_pool
      ~query:create_reorganised_blocks_table_query
      ~query_error:(fun e -> Reorganised_blocks_query e)
  in
  (* 3. Retrieve the entries which form the reorganised blocks *)
  let* map =
    get_entries
      ~db_pool
      ~query:get_reorganised_blocks_entries_query
      ~add_to_map:add_reorganised_blocks_row
      ~arg:head_id
      ~query_error:(fun e -> Reorganised_blocks_query e)
  in
  (* 4. Optional printing of the result *)
  if print_result then print_reorganised_blocks map ;
  (* 5. Populate the reorganised_blocks table *)
  Query_map.iter_es
    (fun block_id (block_hash, level, round) ->
      insert_entry
        ~db_pool
        ~query:insert_reorganised_blocks_entry_query
        ~entry:(block_id, block_hash, level, round)
        ~query_error:(fun e -> Reorganised_blocks_query e))
    map

let baker_nodes_command db_path exp_dir path_to_delegates print_result =
  let open Lwt_result_syntax in
  let* db_pool = connect_db db_path in
  (* 1. Create baker_nodes table in teztale db *)
  let* () =
    create_table
      ~db_pool
      ~query:create_baker_nodes_table_query
      ~query_error:(fun e -> Baker_nodes_query e)
  in
  (* 2. Retrieve the entries which form the baker nodes; the form of a
     baker directory is "baker-[idx]-[str]" where [idx] is an integer
     and [str] can be anything *)
  let regex = Str.regexp "baker-\\([0-9]+\\)-.*" in
  let dir_contents =
    Query_set.of_list @@ Array.to_list @@ Sys.readdir exp_dir
  in
  let bakers_set =
    Query_set.filter
      (fun entry ->
        Sys.is_directory (exp_dir // entry) && Str.string_match regex entry 0)
      dir_contents
  in
  (* 3. Populate the baker_nodes table *)
  let* () =
    Query_set.iter_es
      (fun pod_name ->
        insert_entry
          ~db_pool
          ~query:insert_baker_nodes_entry_query
          ~entry:pod_name
          ~query_error:(fun e -> Baker_nodes_query e))
      bakers_set
  in
  (* 4. Create delegates_of_baker table *)
  let* () =
    create_table
      ~db_pool
      ~query:create_delegates_of_baker_table_query
      ~query_error:(fun e -> Delegates_of_baker_query e)
  in
  (* [get_delegate_addresses] receives as input a baker directory and
     returns all the addresses found in the file following the
     path_to_delegates *)
  let get_delegate_addresses baker_dir path_to_delegates =
    let delegate_addresses_file = baker_dir // path_to_delegates in
    let* () =
      if not (Sys.file_exists delegate_addresses_file) then
        tzfail (Experiment_dir delegate_addresses_file)
      else return_unit
    in
    let in_channel = open_in delegate_addresses_file in
    let baker_addresses = In_channel.input_all in_channel in
    close_in in_channel ;
    return
    @@ List.map
         Tezos_crypto.Signature.Public_key_hash.of_b58check_exn
         Str.(split (regexp "\n") baker_addresses)
  in
  (* 5. Construct the map between baker nodes and delegate addresses *)
  let* baker_delegates_map =
    Query_set.fold_es
      (fun baker acc ->
        let* delegate_addresses =
          get_delegate_addresses (exp_dir // baker) path_to_delegates
        in
        return @@ Delegates_map.add baker delegate_addresses acc)
      bakers_set
      Delegates_map.empty
  in
  (* 6. Optional printing of the result *)
  if print_result then print_baker_nodes baker_delegates_map ;
  (* 7. Get delegates map *)
  let* delegates_map =
    get_entries
      ~db_pool
      ~query:get_delegate_address_query
      ~add_to_map:add_delegates_row
      ~arg:()
      ~query_error:(fun e -> Delegates_of_baker_query e)
  in
  (* 8. Populate the delegates_of_baker table *)
  Delegates_map.iter_es
    (fun baker delegate_addresses ->
      List.iter_es
        (fun delegate_address ->
          let delegate_id =
            Option.value
              (find_key_by_value delegates_map delegate_address)
              ~default:(-1)
          in
          insert_entry
            ~db_pool
            ~query:insert_delegates_of_baker_entry_query
            ~entry:(baker, delegate_id)
            ~query_error:(fun e -> Delegates_of_baker_query e))
        delegate_addresses)
    baker_delegates_map

(* Arguments. *)

let db_arg =
  let open Lwt_result_syntax in
  arg
    ~doc:"Teztale db path"
    ~long:"db-path"
    ~placeholder:"db-path"
    ( parameter @@ fun _ctxt db_path ->
      if Sys.file_exists db_path then return db_path
      else tzfail (Db_path db_path) )

let experiment_dir_arg =
  let open Lwt_result_syntax in
  arg
    ~doc:"Experiment data folder path"
    ~long:"experiment-dir"
    ~placeholder:"experiment-dir"
    ( parameter @@ fun _ctxt experiment_dir ->
      if Sys.file_exists experiment_dir && Sys.is_directory experiment_dir then
        return experiment_dir
      else tzfail (Experiment_dir experiment_dir) )

let print_arg =
  Tezos_clic.switch
    ~short:'p'
    ~long:"print"
    ~doc:"If print flag is set, the result of the query will be printed."
    ()

let path_to_delegates_arg =
  let open Lwt_result_syntax in
  default_arg
    ~doc:"Path from the baker directories to the delegate addresses"
    ~long:"delegates-path"
    ~placeholder:"delegates-path"
    ~default:"baker/baker_addresses"
    (parameter @@ fun _ctxt delegates_path -> return delegates_path)

let commands =
  let open Lwt_result_syntax in
  [
    command
      ~group
      ~desc:"Canonical chain query."
      (args2 db_arg print_arg)
      (fixed ["canonical_chain_query"])
      (fun (db_path, print_result) _cctxt ->
        match db_path with
        | Some db_path -> canonical_chain_command db_path print_result
        | None -> tzfail (Db_path ""));
    command
      ~group
      ~desc:"Reorganised blocks query."
      (args2 db_arg print_arg)
      (fixed ["reorganised_blocks_query"])
      (fun (db_path, print_result) _cctxt ->
        match db_path with
        | Some db_path -> reorganised_blocks_command db_path print_result
        | None -> tzfail (Db_path ""));
    command
      ~group
      ~desc:"Baker nodes query."
      (args4 db_arg experiment_dir_arg path_to_delegates_arg print_arg)
      (fixed ["baker_nodes_query"])
      (fun (db_path, exp_dir, path_to_delegates, print_result) _cctxt ->
        match (db_path, exp_dir) with
        | Some db_path, Some exp_dir ->
            baker_nodes_command db_path exp_dir path_to_delegates print_result
        | None, _ -> tzfail (Db_path "")
        | _, None -> tzfail (Experiment_dir ""));
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
