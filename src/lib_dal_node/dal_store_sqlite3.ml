(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix
include Sqlite

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let table_exists =
    (string ->! bool)
    @@ {|
    SELECT EXISTS (
      SELECT name FROM sqlite_master
      WHERE type='table'
        AND name=?
    )|}

  module Schemas = struct
    let get_all =
      (unit ->* string)
      @@ {|SELECT sql FROM sqlite_schema WHERE name
           NOT LIKE 'sqlite_%' AND name != 'migrations'|}
  end

  module Migrations = struct
    let create_table =
      (unit ->. unit)
      @@ {|
      CREATE TABLE migrations (
        id SERIAL PRIMARY KEY,
        name TEXT
      )|}

    let current_migration =
      (unit ->? int) @@ {|SELECT id FROM migrations ORDER BY id DESC LIMIT 1|}

    let register_migration =
      (t2 int string ->. unit)
      @@ {|
      INSERT INTO migrations (id, name) VALUES (?, ?)
      |}

    let version = 1

    let all : Dal_node_migrations.migration list =
      Dal_node_migrations.migrations version
  end
end

module Schemas = struct
  let get_all store =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Schemas.get_all ()
end

module Migrations = struct
  let create_table store =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Migrations.create_table ()

  let table_exists store =
    with_connection store @@ fun conn ->
    Db.find conn Q.table_exists "migrations"

  let missing_migrations store =
    let open Lwt_result_syntax in
    let all_migrations = List.mapi (fun i m -> (i, m)) Q.Migrations.all in
    let* current =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Migrations.current_migration ()
    in
    match current with
    | Some current ->
        let applied = current + 1 in
        let known = List.length all_migrations in
        if applied <= known then return (List.drop_n applied all_migrations)
        else
          let*! () =
            Dal_store_sqlite3_events.migrations_from_the_future ~applied ~known
          in
          failwith
            "Cannot use a store modified by a more up-to-date version of the \
             DAL node"
    | None -> return all_migrations

  let apply_migration store id (module M : Dal_node_migrations.S) =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Db.exec conn up ()) M.up in
    Db.exec conn Q.Migrations.register_migration (id, M.name)
end

let sqlite_file_name = "store.sqlite"

let init ~data_dir ~perm () =
  let open Lwt_result_syntax in
  let path = data_dir // sqlite_file_name in
  let*! exists = Lwt_unix.file_exists path in
  let migration conn =
    Sqlite.assert_in_transaction conn ;
    let* () =
      if not exists then
        let* () = Migrations.create_table conn in
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
      match (perm, migrations) with
      | `Read_only, _ :: _ ->
          error_with
            "The store has %d missing migrations but was opened in read-only \
             mode."
            (List.length migrations)
      | _, _ -> Ok ()
    in
    let* () =
      List.iter_es
        (fun (i, ((module M : Dal_node_migrations.S) as mig)) ->
          let start_time = Unix.gettimeofday () in
          let* () = Migrations.apply_migration conn i mig in
          let end_time = Unix.gettimeofday () in
          let duration = end_time -. start_time in
          let*! () =
            Dal_store_sqlite3_events.applied_migration ~name:M.name ~duration
          in
          return_unit)
        migrations
    in
    return_unit
  in
  Sqlite.init ~path ~perm migration
