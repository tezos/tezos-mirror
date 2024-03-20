(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix

type error += Caqti_error of string

module Db = struct
  let caqti (p : ('a, Caqti_error.t) result Lwt.t) : 'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*! p in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let start (module Db : Caqti_lwt.CONNECTION) = caqti @@ Db.start ()

  let collect_list (module Db : Caqti_lwt.CONNECTION) req arg =
    caqti @@ Db.collect_list req arg

  let commit (module Db : Caqti_lwt.CONNECTION) = caqti @@ Db.commit ()

  let rollback (module Db : Caqti_lwt.CONNECTION) = caqti @@ Db.rollback ()

  let exec (module Db : Caqti_lwt.CONNECTION) req arg = caqti @@ Db.exec req arg

  let find (module Db : Caqti_lwt.CONNECTION) req arg = caqti @@ Db.find req arg

  let find_opt (module Db : Caqti_lwt.CONNECTION) req arg =
    caqti @@ Db.find_opt req arg
end

type t = {
  db_uri : Uri.t;
  with_transaction : (module Caqti_lwt.CONNECTION) option;
}

let assert_in_transaction store = assert (store.with_transaction <> None)

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  open Ethereum_types

  let l1_level = int32

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

  let upgrade =
    custom
      ~encode:(fun Ethereum_types.Upgrade.{hash; timestamp} ->
        Ok (hash, timestamp))
      ~decode:(fun (hash, timestamp) ->
        Ok Ethereum_types.Upgrade.{hash; timestamp})
      (t2 root_hash timestamp)

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

    module V2 = struct
      let name = "add_lastest_tezos_level"

      let up =
        [
          migration_step
            {|
        CREATE TABLE l1_latest_level (
          lock CHAR(1) PRIMARY KEY NOT NULL DEFAULT 'l',
          level INT,
          CONSTRAINT CK_T1_Locked CHECK (lock='l')
        );|};
          (* The CONSTRAINT allows no more than 1 row in this
             table, making sure we only have 1 tezos level at
             most *)
        ]
    end

    module V3 = struct
      let name = "add_timestamp_to_blueprints"

      let up =
        [
          (* We need to drop the content of [Executable_blueprint] in order to
             add a column that should not be left empty. *)
          migration_step
            {|
            DELETE FROM executable_blueprints
        |};
          migration_step
            {|
            ALTER TABLE executable_blueprints
            ADD COLUMN timestamp DATETIME NOT NULL
        |};
        ]
    end

    module V4 = struct
      let name = "upgrade_events_table_improvement"

      let up =
        [
          (* We need to drop the previous contents because the contents has not
             been kept up to date *)
          migration_step {|
        DELETE FROM kernel_upgrades
        |};
          (* We use this opportunity to do some renaming. *)
          migration_step
            {|
        ALTER TABLE kernel_upgrades
        RENAME COLUMN applied_before TO injected_before
        |};
          migration_step
            {|
        ALTER TABLE kernel_upgrades
        RENAME COLUMN applied TO applied_before
        |};
          (* We add a constraint that we can only have one kernel upgrade not
             applied at a given time *)
          migration_step
            {|
        CREATE UNIQUE INDEX unapplied_upgrade
        ON kernel_upgrades (applied_before)
        WHERE applied_before IS NULL
    |};
        ]
    end

    let all : migration list =
      [(module V0); (module V1); (module V2); (module V3); (module V4)]
  end

  module Executable_blueprints = struct
    let insert =
      (t3 level timestamp payload ->. unit)
      @@ {eos|INSERT INTO executable_blueprints (id, timestamp, payload) VALUES (?, ?, ?)|eos}

    let select =
      (level ->? t2 payload timestamp)
      @@ {eos|SELECT payload, timestamp FROM executable_blueprints WHERE id = ?|eos}
  end

  module Publishable_blueprints = struct
    let insert =
      (t2 level payload ->. unit)
      @@ {eos|INSERT INTO publishable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (level ->? payload)
      @@ {eos|SELECT payload FROM publishable_blueprints WHERE id = ?|eos}

    let select_range =
      (t2 level level ->* t2 level payload)
      @@ {|SELECT id, payload FROM publishable_blueprints
           WHERE ? <= id AND id <= ?
           ORDER BY id ASC|}
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
      @@ {|REPLACE INTO kernel_upgrades (injected_before, root_hash, activation_timestamp) VALUES (?, ?, ?)|}

    let get_latest_unapplied =
      (unit ->? upgrade)
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE applied_before IS NULL
           ORDER BY applied_before DESC
           LIMIT 1
    |}

    let record_apply =
      (level ->. unit)
      @@ {|
      UPDATE kernel_upgrades SET applied_before = ? WHERE applied_before = NULL
    |}
  end

  module L1_latest_level = struct
    let insert =
      (l1_level ->. unit)
      @@ {eos|REPLACE INTO l1_latest_level (level) VALUES (?)|eos}

    let get =
      (unit ->! l1_level) @@ {eos|SELECT (level) FROM l1_latest_level|eos}
  end
end

let with_connection store k =
  let open Lwt_result_syntax in
  match store.with_transaction with
  | Some conn -> k conn
  | None ->
      Caqti_lwt_unix.System.Switch.run @@ fun sw ->
      let* conn = Db.caqti (Caqti_lwt_unix.connect ~sw store.db_uri) in
      k conn

let with_transaction store k =
  let open Lwt_result_syntax in
  match store.with_transaction with
  | None -> (
      with_connection store @@ fun conn ->
      let* () = Db.start conn in
      let*! res =
        Lwt.catch
          (fun () -> k {store with with_transaction = Some conn})
          (fun exn -> fail_with_exn exn)
      in

      match res with
      | Ok x ->
          let* () = Db.commit conn in
          return x
      | Error err ->
          let* () = Db.rollback conn in
          fail err)
  | Some _ ->
      failwith "Internal error: attempting to perform a nested transaction"

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
            Evm_store_events.migrations_from_the_future ~applied ~known:0
          in
          failwith
            "Cannot use a store modified by a more up-to-date version of the \
             EVM node"
    | None -> return all_migrations

  let apply_migration store id (module M : Q.MIGRATION) =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Db.exec conn up ()) M.up in
    Db.exec conn Q.Migrations.register_migration (id, M.name)

  let assume_old_store store =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = Db.exec conn Q.Migrations.create_table () in
    Db.exec conn Q.Migrations.register_migration (0, Q.Migrations.V0.name)

  let check_V0 store =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* publishable = Db.find conn Q.table_exists "publishable_blueprints" in
    let* executable = Db.find conn Q.table_exists "executable_blueprints" in
    let* context_hashes = Db.find conn Q.table_exists "context_hashes" in
    return (publishable && executable && context_hashes)
end

let init ~data_dir =
  let open Lwt_result_syntax in
  let path = data_dir // "store.sqlite" in
  let*! exists = Lwt_unix.file_exists path in
  let uri = Uri.of_string Format.(sprintf "sqlite3:%s" path) in
  let store = {db_uri = uri; with_transaction = None} in
  let* () =
    with_transaction store @@ fun store ->
    let* () =
      if not exists then
        let* () = Migrations.create_table store in
        let*! () = Evm_store_events.init_store () in
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
                let*! () = Evm_store_events.assume_old_store () in
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
          let*! () = Evm_store_events.applied_migration M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  return store

module Executable_blueprints = struct
  let store store (blueprint : Blueprint_types.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Executable_blueprints.insert
      (blueprint.number, blueprint.timestamp, blueprint.payload)

  let find store number =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let+ opt = Db.find_opt conn Q.Executable_blueprints.select number in
    match opt with
    | Some (payload, timestamp) ->
        Some Blueprint_types.{payload; timestamp; number}
    | None -> None
end

module Publishable_blueprints = struct
  let store store number blueprint =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Publishable_blueprints.insert (number, blueprint)

  let find store number =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Publishable_blueprints.select number

  let find_range store ~from ~to_ =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Publishable_blueprints.select_range (from, to_)
end

module Context_hashes = struct
  let store store number hash =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Context_hashes.insert (number, hash)

  let find store number =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Context_hashes.select number

  let find_latest store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Context_hashes.get_latest ()
end

module Kernel_upgrades = struct
  let store store next_blueprint_number (event : Ethereum_types.Upgrade.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Kernel_upgrades.insert
      (next_blueprint_number, event.hash, event.timestamp)

  let find_latest_pending store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Kernel_upgrades.get_latest_unapplied ()

  let record_apply store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Kernel_upgrades.record_apply level
end

module L1_latest_known_level = struct
  let store store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_latest_level.insert level

  let find store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_latest_level.get ()
end

(* Error registration *)
let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_caqti_error"
    ~title:"Error raised by Caqti"
    ~description:"Caqti raised an error while processing a SQL statement"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Caqti raised an error while processing a SQL statement: %s"
        msg)
    Data_encoding.(obj1 (req "caqti_error" string))
    (function Caqti_error err -> Some err | _ -> None)
    (fun err -> Caqti_error err)
