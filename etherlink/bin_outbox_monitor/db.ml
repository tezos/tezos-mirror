(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Caqti_request.Infix
open Caqti_type.Std

(** Current version for migrations. *)
let version = 0

module Events = struct
  include Internal_event.Simple

  let section = ["outbox_monitor"; "db"]

  let create_db =
    declare_0
      ~section
      ~name:"create_db"
      ~msg:"Database is being created"
      ~level:Info
      ()

  let applied_migration =
    declare_1
      ~section
      ~name:"applied_migration"
      ~msg:"Applied migration \"{name}\" to the database"
      ~level:Notice
      ("name", Data_encoding.string)

  let migrations_from_the_future =
    declare_2
      ~section
      ~name:"migrations_from_the_future"
      ~msg:"Database has {applied} migrations applied but only aware of {known}"
      ~level:Error
      ("applied", Data_encoding.int31)
      ("known", Data_encoding.int31)
end

let _with_connection store conn =
  match conn with
  | Some conn -> Sqlite.with_connection conn
  | None ->
      fun k -> Sqlite.use store @@ fun conn -> Sqlite.with_connection conn k

let _with_transaction store k =
  Sqlite.use store @@ fun conn -> Sqlite.with_transaction conn k

module Migrations = struct
  module Q = struct
    let table_exists =
      (string ->! bool)
      @@ {sql|
    SELECT EXISTS (
      SELECT name FROM sqlite_master
      WHERE type='table'
        AND name=?
    )|sql}

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

    let all : Sqlite_migrations.migration list =
      Sqlite_migrations.migrations version
  end

  let create_table store =
    Sqlite.with_connection store @@ fun conn ->
    Sqlite.Db.exec conn Q.create_table ()

  let table_exists store =
    Sqlite.with_connection store @@ fun conn ->
    Sqlite.Db.find conn Q.table_exists "migrations"

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
            "Cannot use a database at migration %d, the outbox monitor only \
             supports up to %d"
            applied
            known
    | None -> return all_migrations

  let apply_migration store id (module M : Sqlite_migrations.S) =
    let open Lwt_result_syntax in
    Sqlite.with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Sqlite.Db.exec conn up ()) M.apply in
    Sqlite.Db.exec conn Q.register_migration (id, M.name)
end

type t = Sqlite.t

let sqlite_file_name = "outbox-monitor.sqlite"

let init ~data_dir perm : t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! () = Tezos_stdlib_unix.Lwt_utils_unix.create_dir data_dir in
  let path = Filename.concat data_dir sqlite_file_name in
  let*! exists = Lwt_unix.file_exists path in
  let migration conn =
    Sqlite.assert_in_transaction conn ;
    let* () =
      if not exists then
        let* () = Migrations.create_table conn in
        let*! () = Events.(emit create_db) () in
        return_unit
      else
        let* table_exists = Migrations.table_exists conn in
        let* () =
          when_ (not table_exists) (fun () ->
              failwith
                "A database already exists, but its content is incorrect.")
        in
        return_unit
    in
    let* migrations = Migrations.missing_migrations conn in
    let* () =
      List.iter_es
        (fun (i, ((module M : Sqlite_migrations.S) as mig)) ->
          let* () = Migrations.apply_migration conn i mig in
          let*! () = Events.(emit applied_migration) M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  Sqlite.init ~path ~perm migration
