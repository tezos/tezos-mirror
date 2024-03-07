(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix

type t = {db_uri : Uri.t}

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  open Ethereum_types

  let level =
    custom
      ~encode:(fun (Qty x) -> Ok Z.(to_int x))
      ~decode:(fun x -> Ok (Qty Z.(of_int x)))
      int

  let payload =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Blueprint_types.payload_encoding
             payload))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid blueprint payload"
        @@ Data_encoding.Binary.of_string_opt
             Blueprint_types.payload_encoding
             bytes)
      string

  let context_hash =
    custom
      ~encode:(fun hash -> Ok (Context_hash.to_b58check hash))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid b58check encoded hash"
        @@ Context_hash.of_b58check_opt bytes)
      string

  let root_hash =
    let open Ethereum_types in
    custom
      ~encode:(fun (Hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Hash (Hex hash)))
      string

  let timestamp =
    custom
      ~encode:(fun t -> Ok (Time.Protocol.to_seconds t))
      ~decode:(fun i ->
        if i >= 0L then Ok (Time.Protocol.of_seconds i)
        else Error "invalid negative timestamp")
      int64

  let table_exists =
    (string ->! bool)
    @@ {|
    SELECT EXISTS (
      SELECT name FROM sqlite_master
      WHERE type='table'
        AND name=?
    )|}

  module type MIGRATION = sig
    val name : string

    val up : (unit, unit, [`Zero]) Caqti_request.t list
  end

  type migration = (module MIGRATION)

  let migration_step = ( @@ ) (unit ->. unit)

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

    module V0 = struct
      let name = "initial_set_of_tables"

      let up =
        [
          migration_step
            {|
        CREATE TABLE executable_blueprints (
          id SERIAL PRIMARY KEY,
          payload BLOB NOT NULL
        )|};
          migration_step
            {|
        CREATE TABLE publishable_blueprints (
          id SERIAL PRIMARY KEY,
          payload BLOB NOT NULL
        )|};
          migration_step
            {|
        CREATE TABLE context_hashes (
          id SERIAL PRIMARY KEY,
          context_hash VARCHAR(52) NOT NULL
        );|};
        ]
    end

    module V1 = struct
      let name = "create_upgrade_events_table"

      let up =
        [
          migration_step
            {|
        CREATE TABLE kernel_upgrades (
          applied_before INT NOT NULL,
          root_hash TEXT NOT NULL,
          activation_timestamp INT NOT NULL,
          applied INT
        )
        |};
        ]
    end

    let all : migration list = [(module V0); (module V1)]
  end

  module Executable_blueprints = struct
    let insert =
      (t2 level payload ->. unit)
      @@ {eos|INSERT INTO executable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (level ->? payload)
      @@ {eos|SELECT (payload) FROM executable_blueprints WHERE id = ?|eos}
  end

  module Publishable_blueprints = struct
    let insert =
      (t2 level payload ->. unit)
      @@ {eos|INSERT INTO publishable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (level ->? payload)
      @@ {eos|SELECT (payload) FROM publishable_blueprints WHERE id = ?|eos}
  end

  module Context_hashes = struct
    let insert =
      (t2 level context_hash ->. unit)
      @@ {eos|REPLACE INTO context_hashes (id, context_hash) VALUES (?, ?)|eos}

    let select =
      (level ->? context_hash)
      @@ {eos|SELECT (context_hash) FROM context_hashes WHERE id = ?|eos}

    let get_latest =
      (unit ->? t2 level context_hash)
      @@ {eos|SELECT id, context_hash FROM context_hashes ORDER BY id DESC LIMIT 1|eos}
  end

  module Kernel_upgrades = struct
    let insert =
      (t3 level root_hash timestamp ->. unit)
      @@ {|INSERT INTO kernel_upgrades (applied_before, root_hash, activation_timestamp) VALUES (?, ?, ?)|}
  end
end

let with_caqti_error p =
  let open Lwt_syntax in
  let* p in
  match p with
  | Ok p -> return_ok p
  | Error err -> failwith "Caqti: %a" Caqti_error.pp err

let with_connection {db_uri} k =
  with_caqti_error @@ Caqti_lwt_unix.with_connection db_uri k

module Migrations = struct
  let create_table store =
    with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Migrations.create_table ()

  let table_exists store =
    with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find Q.table_exists "migrations"

  let missing_migrations store =
    let open Lwt_result_syntax in
    let all_migrations = List.mapi (fun i m -> (i, m)) Q.Migrations.all in
    let* current =
      with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.find_opt Q.Migrations.current_migration ()
    in
    match current with
    | Some current ->
        let applied = current + 1 in
        let known = List.length all_migrations in
        if applied <= known then return (List.drop_n applied all_migrations)
        else
          let*! () =
            Store_events.migrations_from_the_future ~applied ~known:0
          in
          failwith
            "Cannot use a store modified by a more up-to-date version of the \
             EVM node"
    | None -> return all_migrations

  let apply_migration store id (module M : Q.MIGRATION) =
    let open Lwt_result_syntax in
    with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.with_transaction @@ fun () ->
    let* () = List.iter_es (fun up -> Db.exec up ()) M.up in
    Db.exec Q.Migrations.register_migration (id, M.name)

  let assume_old_store store =
    let open Lwt_result_syntax in
    with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    let* () = Db.exec Q.Migrations.create_table () in
    Db.exec Q.Migrations.register_migration (0, Q.Migrations.V0.name)

  let check_V0 store =
    let open Lwt_result_syntax in
    with_connection store @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    let* publishable = Db.find Q.table_exists "publishable_blueprints" in
    let* executable = Db.find Q.table_exists "executable_blueprints" in
    let* context_hashes = Db.find Q.table_exists "context_hashes" in
    return (publishable && executable && context_hashes)
end

let init ~data_dir =
  let open Lwt_result_syntax in
  let path = data_dir // "store.sqlite" in
  let*! exists = Lwt_unix.file_exists path in
  let uri = Uri.of_string Format.(sprintf "sqlite3:%s" path) in
  let store = {db_uri = uri} in
  let* () =
    if not exists then
      let* () = Migrations.create_table store in
      let*! () = Store_events.init_store () in
      return_unit
    else
      let* table_exists = Migrations.table_exists store in
      let* () =
        when_ (not table_exists) (fun () ->
            (* The database already exists, but the migrations table does not.
               This probably means it was created before the introduction of
               the migration system. We check that the three initial tables are
               there and if so, we assume the first migration is applied moving
               forward, with a warning. *)
            let* old_db = Migrations.check_V0 store in
            if old_db then
              let* () = Migrations.assume_old_store store in
              let*! () = Store_events.assume_old_store () in
              return_unit
            else
              failwith "A store already exists, but its content is incorrect.")
      in
      return_unit
  in
  let* migrations = Migrations.missing_migrations store in
  let* () =
    List.iter_es
      (fun (i, ((module M : Q.MIGRATION) as mig)) ->
        let* () = Migrations.apply_migration store i mig in
        let*! () = Store_events.applied_migration M.name in
        return_unit)
      migrations
  in
  return store

module Executable_blueprints = struct
  let store {db_uri} number blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Executable_blueprints.insert (number, blueprint)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Executable_blueprints.select number
end

module Publishable_blueprints = struct
  let store {db_uri} number blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Publishable_blueprints.insert (number, blueprint)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Publishable_blueprints.select number
end

module Context_hashes = struct
  let store {db_uri} number hash =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Context_hashes.insert (number, hash)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Context_hashes.select number

  let find_latest {db_uri} =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Context_hashes.get_latest ()
end

module Kernel_upgrades = struct
  let store {db_uri} next_blueprint_number (event : Ethereum_types.Upgrade.t) =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec
      Q.Kernel_upgrades.insert
      (next_blueprint_number, event.hash, event.timestamp)
end
