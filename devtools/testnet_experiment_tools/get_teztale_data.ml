(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Teztale querying tool
   ------------------------
   Invocation:
     ./_build/default/devtools/testnet_experiment_tools/get_teztale_data.exe \
     canonical_chain_query \
     --db-path <db-path>
     [ --print ]
   Requirements:
     <db-path> - path to the teztale database
     [<print>] - if this flag is set, we print the result of the query
   Description:
     This file contains the tool for querying the teztale database.
     The queries that it provides are:
     - get_canonical_chain : get table (id, predecessor) of block ids in the
     increasing order as they appear in the canonical chain from the db.
*)

open Tezos_clic

(* Data Structures. *)

module Canonical_chain_map = Map.Make (Int)

(* Errors. *)

type error +=
  | Db_path of string
  | Caqti_database_connection of string
  | Canonical_chain_query of string
  | Canonical_chain_head of string

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
    ~id:"get_teztale_data.caqti_database_connection"
    ~title:"Connection to teztale db failed"
    ~description:"Connection to teztale db must be achieved"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Expected to connect to teztale db: %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Caqti_database_connection s -> Some s | _ -> None)
    (fun s -> Caqti_database_connection s) ;
  register_error_kind
    `Permanent
    ~id:"get_teztale_data.canonical_chain_query"
    ~title:"Failed to create canonical_chain table"
    ~description:"canonical_chain table must be created"
    ~pp:(fun ppf s -> Format.fprintf ppf "Expected canonical_chain table: %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Canonical_chain_query s -> Some s | _ -> None)
    (fun s -> Canonical_chain_query s) ;
  register_error_kind
    `Permanent
    ~id:"get_tezale_data.canonical_chain_head"
    ~title:"Failed to obtain the head of the canonical chain"
    ~description:"Canonical chain head is required"
    ~pp:(fun ppf s -> Format.fprintf ppf "Expected canonical chain head: %s" s)
    Data_encoding.(obj1 (req "arg" string))
    (function Canonical_chain_head s -> Some s | _ -> None)
    (fun s -> Canonical_chain_head s)

(* Aggregators. *)

let add_canonical_chain_row (id, predecessor) acc =
  Canonical_chain_map.add id predecessor acc

let add_head_id id acc =
  acc := Some id ;
  acc

(* Queries *)

let get_canonical_chain db_pool head_id =
  let open Lwt_result_syntax in
  let query =
    {| WITH canonical_chain AS (
          SELECT id, predecessor
          FROM blocks
          WHERE id = ?

          UNION ALL

          SELECT b.id, b.predecessor
          FROM canonical_chain c
          JOIN blocks b ON c.predecessor = b.id
       )

       SELECT id, predecessor
       FROM canonical_chain
       ORDER BY id ASC |}
  in
  let canonical_chain_request =
    Caqti_request.Infix.(Caqti_type.int ->* Caqti_type.(tup2 int (option int)))
      query
  in
  let*! map =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold
          canonical_chain_request
          add_canonical_chain_row
          head_id
          Canonical_chain_map.empty)
      db_pool
  in
  match map with
  | Error e -> tzfail (Canonical_chain_query (Caqti_error.show e))
  | Ok map -> return map

let create_canonical_chain_table db_pool =
  let open Lwt_result_syntax in
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->. unit))
      {| CREATE TABLE IF NOT EXISTS canonical_chain(
         id INTEGER PRIMARY KEY,
         block_id INTEGER ,
         predecessor INTEGER,
         FOREIGN KEY (block_id) REFERENCES blocks(id),
         FOREIGN KEY (predecessor) REFERENCES blocks(predecessor)) |}
  in
  let*! result =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec query ())
      db_pool
  in
  match result with
  | Error e -> tzfail (Canonical_chain_query (Caqti_error.show e))
  | Ok () -> return_unit

let insert_canonical_chain_entry db_pool counter id predecessor =
  let open Lwt_result_syntax in
  let query =
    Caqti_request.Infix.(Caqti_type.(tup3 string string string ->. unit))
      {| INSERT INTO canonical_chain(id, block_id, predecessor) 
         VALUES ($1, $2, $3) ON CONFLICT DO NOTHING |}
  in
  let*! result =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.exec query (string_of_int counter, id, predecessor))
      db_pool
  in

  match result with
  | Error e -> tzfail (Canonical_chain_query (Caqti_error.show e))
  | Ok () -> return_unit

let get_head_id db_pool =
  let open Lwt_result_syntax in
  let query =
    {| SELECT predecessor
       FROM blocks
       WHERE id = (
         SELECT predecessor
         FROM blocks
         WHERE level = (
           SELECT MAX(level)
           FROM blocks
         )
         LIMIT 1
       ) |}
  in
  let get_head_id_request =
    Caqti_request.Infix.(Caqti_type.unit ->* Caqti_type.int) query
  in
  let*! head_id_ref =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold get_head_id_request add_head_id () (ref None))
      db_pool
  in
  match head_id_ref with
  | Error e -> tzfail (Canonical_chain_head (Caqti_error.show e))
  | Ok head_id_ref -> return !head_id_ref

(* Printing. *)

let print_canonical_chain map =
  let find_key_by_value map x =
    Canonical_chain_map.filter (fun _ value -> value = x) map
    |> Canonical_chain_map.to_seq |> List.of_seq
    |> fun lst -> List.hd lst |> Option.map fst
  in

  let rec process_block current_block =
    match find_key_by_value map (Some current_block) with
    | None -> ()
    | Some next_block ->
        Format.printf " --> %s@," (string_of_int next_block) ;
        process_block next_block
  in

  match find_key_by_value map None with
  | None -> Format.printf "No canonical chain found"
  | Some first_block ->
      Format.printf "%s" (string_of_int first_block) ;
      process_block first_block

(* Commands. *)

let canonical_chain_command db_path print_result =
  let open Lwt_result_syntax in
  let db_uri = Uri.of_string ("sqlite3:" ^ db_path) in
  match Caqti_lwt.connect_pool db_uri with
  | Error e -> tzfail (Caqti_database_connection (Caqti_error.show e))
  | Ok db_pool ->
      (* 1. Create canonical_table in teztale db *)
      let* () = create_canonical_chain_table db_pool in

      (* 2. Obtain the head id of the canonical chain *)
      let* head_id = get_head_id db_pool in

      (* 3. Retrieve the entries which form the canonical chain *)
      let* map =
        get_canonical_chain db_pool (Option.value ~default:0 head_id)
      in

      (* 4. Populate the canonical_chain table *)
      let* () =
        let counter = ref 0 in
        if print_result then print_canonical_chain map ;
        Canonical_chain_map.iter_es
          (fun id predecessor ->
            let id_str = string_of_int id in
            let predecessor_str =
              Option.value ~default:"N/A" (Option.map string_of_int predecessor)
            in
            counter := !counter + 1 ;
            insert_canonical_chain_entry db_pool !counter id_str predecessor_str)
          map
      in
      return_unit

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

let print_arg =
  Tezos_clic.switch
    ~short:'p'
    ~long:"print"
    ~doc:"If print flag is set, the result of the query will be printed."
    ()

let commands =
  let open Lwt_result_syntax in
  [
    command
      ~group:
        {
          name = "devtools";
          title = "Command for querying the teztale db for canonical chain";
        }
      ~desc:"Canonical chain query."
      (args2 db_arg print_arg)
      (fixed ["canonical_chain_query"])
      (fun (db_path, print_result) _cctxt ->
        match db_path with
        | Some db_path -> canonical_chain_command db_path print_result
        | None -> tzfail (Db_path ""));
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
