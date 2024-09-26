(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Caqti_request.Infix
open Caqti_type.Std

module Events = struct
  include Internal_event.Simple

  let section = ["smart_rollup_node"; "store"]

  let init_store =
    declare_0
      ~section
      ~name:"smart_rollup_node_store_init"
      ~msg:"Store is being initialized for the first time"
      ~level:Notice
      ()

  let applied_migration =
    declare_1
      ~section
      ~name:"smart_rollup_node_store_applied_migration"
      ~msg:"Applied migration {name} to the store"
      ~level:Notice
      ("name", Data_encoding.string)

  let migrations_from_the_future =
    declare_2
      ~section
      ~name:"smart_rollup_node_migrations_from_the_future"
      ~msg:
        "Store has {applied} migrations applied but the rollup node is only \
         aware of {known}"
      ~level:Error
      ("applied", Data_encoding.int31)
      ("known", Data_encoding.int31)
end

let with_connection store conn =
  match conn with
  | Some conn -> Sqlite.with_connection conn
  | None ->
      fun k -> Sqlite.use store @@ fun conn -> Sqlite.with_connection conn k

let with_transaction store k =
  Sqlite.use store @@ fun conn -> Sqlite.with_transaction conn k

module Types = struct
  let level = int32

  let tzcustom ~encode ~decode t =
    custom
      ~encode:(fun v -> Ok (encode v))
      ~decode:(fun s ->
        decode s
        |> Result.map_error (fun err -> Format.asprintf "%a" pp_print_trace err))
      t

  let commitment_hash =
    tzcustom
      ~encode:Commitment.Hash.to_b58check
      ~decode:Commitment.Hash.of_b58check
      string

  let state_hash =
    tzcustom
      ~encode:State_hash.to_b58check
      ~decode:State_hash.of_b58check
      string

  let commitment =
    product (fun compressed_state inbox_level predecessor number_of_ticks ->
        Commitment.{compressed_state; inbox_level; predecessor; number_of_ticks})
    @@ proj state_hash (fun c -> c.Commitment.compressed_state)
    @@ proj level (fun c -> c.Commitment.inbox_level)
    @@ proj commitment_hash (fun c -> c.Commitment.predecessor)
    @@ proj int64 (fun c -> c.Commitment.number_of_ticks)
    @@ proj_end
end

let table_exists_req =
  (string ->! bool)
  @@ {sql|
    SELECT EXISTS (
      SELECT name FROM sqlite_master
      WHERE type='table'
        AND name=?
    )|sql}

module Migrations = struct
  module Q = struct
    let create_table =
      (unit ->. unit)
      @@ {sql|
      CREATE TABLE migrations (
        id SERIAL PRIMARY KEY,
        name TEXT
      )|sql}

    let current_migration =
      (unit ->? int) @@ {|SELECT id FROM migrations ORDER BY id DESC LIMIT 1|}

    let register_migration =
      (t2 int string ->. unit)
      @@ {sql|
      INSERT INTO migrations (id, name) VALUES (?, ?)
      |sql}

    let version = 0

    let all : Rollup_node_sqlite_migrations.migration list =
      Rollup_node_sqlite_migrations.migrations version
  end

  let create_table store =
    Sqlite.with_connection store @@ fun conn ->
    Sqlite.Db.exec conn Q.create_table ()

  let table_exists store =
    Sqlite.with_connection store @@ fun conn ->
    Sqlite.Db.find conn table_exists_req "migrations"

  let missing_migrations store =
    let open Lwt_result_syntax in
    let all_migrations = List.mapi (fun i m -> (i, m)) Q.all in
    let* current =
      Sqlite.with_connection store @@ fun conn ->
      Sqlite.Db.find_opt conn Q.current_migration ()
    in
    match current with
    | Some current ->
        let applied = current + 1 in
        let known = List.length all_migrations in
        if applied <= known then return (List.drop_n applied all_migrations)
        else
          let*! () =
            Events.(emit migrations_from_the_future) (applied, known)
          in
          failwith
            "Cannot use a store modified by a more up-to-date version of the \
             EVM node"
    | None -> return all_migrations

  let apply_migration store id (module M : Rollup_node_sqlite_migrations.S) =
    let open Lwt_result_syntax in
    Sqlite.with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Sqlite.Db.exec conn up ()) M.apply in
    Sqlite.Db.exec conn Q.register_migration (id, M.name)
end

let sqlite_file_name = "store.sqlite"

type 'a t = Sqlite.t

type rw = Store_sigs.rw t

type ro = Store_sigs.ro t

let init (type m) (mode : m Store_sigs.mode) ~data_dir : m t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let path = Filename.concat data_dir sqlite_file_name in
  let*! exists = Lwt_unix.file_exists path in
  let migration conn =
    Sqlite.assert_in_transaction conn ;
    let* () =
      if not exists then
        let* () = Migrations.create_table conn in
        let*! () = Events.(emit init_store) () in
        return_unit
      else
        let* table_exists = Migrations.table_exists conn in
        let* () =
          when_ (not table_exists) (fun () ->
              failwith "A store already exists, but its content is incorrect.")
        in
        return_unit
    in
    let* migrations = Migrations.missing_migrations conn in
    let*? () =
      match (mode, migrations) with
      | Read_only, _ :: _ ->
          error_with
            "The store has %d missing migrations but was opened in read-only \
             mode."
            (List.length migrations)
      | _, _ -> Ok ()
    in
    let* () =
      List.iter_es
        (fun (i, ((module M : Rollup_node_sqlite_migrations.S) as mig)) ->
          let* () = Migrations.apply_migration conn i mig in
          let*! () = Events.(emit applied_migration) M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  let perm =
    match mode with Read_only -> `Read_only | Read_write -> `Read_write
  in
  Sqlite.init ~path ~perm migration

let close store = Sqlite.close store

let readonly (store : Store_sigs.rw t) : Store_sigs.ro t = store

module Commitments = struct
  module Q = struct
    open Types

    let insert =
      (t2 commitment_hash commitment ->. unit)
      @@ {sql|
      REPLACE INTO commitments
      (hash, compressed_state, inbox_level, predecessor, number_of_ticks)
      VALUES (?, ?, ?, ?, ?)
      |sql}

    let select =
      (commitment_hash ->? commitment)
      @@ {sql|
      SELECT compressed_state, inbox_level, predecessor, number_of_ticks
      FROM commitments
      WHERE hash = ?
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM commitments WHERE inbox_level < ?
      |sql}

    let lcc =
      (unit ->? commitment)
      @@ {sql|
      SELECT compressed_state, inbox_level, predecessor, number_of_ticks
      FROM commitments
      INNER JOIN rollup_node_state
      ON name = "lcc" AND value = hash
      |sql}

    let lpc =
      (unit ->? commitment)
      @@ {sql|
      SELECT compressed_state, inbox_level, predecessor, number_of_ticks
      FROM commitments
      INNER JOIN rollup_node_state
      ON name = "lpc" AND value = hash
      |sql}
  end

  let store ?conn store commitment =
    let open Lwt_result_syntax in
    with_connection store conn @@ fun conn ->
    let hash = Commitment.hash commitment in
    let+ () = Sqlite.Db.exec conn Q.insert (hash, commitment) in
    hash

  let find ?conn store commitment_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select commitment_hash

  let find_lcc ?conn store commitment_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.lcc commitment_hash

  let find_lpc ?conn store =
    with_connection store conn @@ fun conn -> Sqlite.Db.find_opt conn Q.lpc ()

  let delete_before ?conn store ~level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level
end
