(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix
open Sqlite
open Caqti_type.Std

let sqlite_file_name = "store.sqlite"

type conn = Sqlite.conn

(*
  To introduce a new migration

    - Create a .sql file led by the next migration number [N = version + 1]
      (with leading 0s) followed by the name of the migration (e.g.
      [005_create_blueprints_table.sql])
    - Run [src/lib_dal_node/scripts/check_dal_store_migrations.sh promote]
    - Regenerate the schemas, using [[
          dune exec tezt/tests/main.exe -- --file dal.ml store \
            schemas regression --reset-regressions
      ]]
    - Increment [Migrations.version]

  You can review the result at
  [tezt/tests/expected/dal.ml/DAL Node- debug print store schemas.out].
*)
module Migrations = Sqlite.Migration.Make (struct
  let table_name = "migrations"

  let version = 1

  let all_migrations =
    Sqlite.Migration.from_ocaml_crunch
      Dal_node_migrations.Migrations.file_list
      Dal_node_migrations.Migrations.read
end)

module Types = struct
  let from_encoding ~name encoding =
    custom
      ~encode:(fun v ->
        Data_encoding.Binary.to_string encoding v
        |> Result.map_error
             (Format.asprintf
                "Fail to encode %s for database: %a"
                name
                Data_encoding.Binary.pp_write_error))
      ~decode:(fun s ->
        Data_encoding.Binary.of_string encoding s
        |> Result.map_error
             (Format.asprintf
                "Fail to decode %s from database: %a"
                name
                Data_encoding.Binary.pp_read_error))
      octets

  open Tezos_dal_node_services.Types

  let published_level : level Caqti_type.t = int32

  let attestation_lag : int Caqti_type.t = int16

  let dal_slot_index : slot_index Caqti_type.t = int16

  let skip_list_hash =
    from_encoding ~name:"skip_list_hash" Dal_proto_types.Skip_list_hash.encoding

  let skip_list_cell =
    from_encoding ~name:"skip_list_cell" Dal_proto_types.Skip_list_cell.encoding
end

let with_connection store conn =
  match conn with
  | Some conn -> Sqlite.with_connection conn
  | None ->
      fun k -> Sqlite.use store @@ fun conn -> Sqlite.with_connection conn k

module Skip_list_cells = struct
  type t = Sqlite.t

  open Types

  module Q = struct
    open Sqlite.Request
    open Dal_proto_types
    open Tezos_dal_node_services.Types

    let find_opt : (Skip_list_hash.t, Skip_list_cell.t, [`Zero | `One]) t =
      (skip_list_hash ->? skip_list_cell)
      @@ {sql|
      SELECT cell
      FROM skip_list_cells
      WHERE hash = $1
      |sql}

    (* Returns the skip list cell together with its attestation_lag *)
    let find_by_slot_id_opt :
        (level * slot_index, Skip_list_cell.t * int, [`One | `Zero]) t =
      let open Caqti_type.Std in
      (t2 published_level dal_slot_index ->? t2 skip_list_cell attestation_lag)
      @@ {sql|
       SELECT c.cell, s.attestation_lag
       FROM skip_list_cells AS c
       JOIN skip_list_slots AS s
         ON s.skip_list_cell_hash = c.hash
       WHERE s.published_level = $1 AND s.slot_index = $2
       |sql}

    let find_by_level :
        ( level,
          Skip_list_cell.t * Skip_list_hash.t * int * int,
          [`Zero | `One | `Many] )
        t =
      let open Caqti_type.Std in
      published_level
      ->* t4 skip_list_cell skip_list_hash dal_slot_index attestation_lag
      @@ {sql|
        SELECT c.cell, c.hash, s.slot_index, s.attestation_lag
        FROM skip_list_cells AS c
        JOIN skip_list_slots AS s
        ON s.skip_list_cell_hash = c.hash
        WHERE s.published_level = $1
        ORDER BY s.slot_index DESC
      |sql}

    let insert_skip_list_slot :
        (level * slot_index * int * Skip_list_hash.t, unit, [`Zero]) t =
      (t4 published_level dal_slot_index attestation_lag skip_list_hash ->. unit)
      @@ {sql|
      INSERT INTO skip_list_slots
      (published_level, slot_index, attestation_lag, skip_list_cell_hash)
      VALUES ($1, $2, $3, $4)
      ON CONFLICT(published_level, slot_index)
      DO UPDATE SET
        attestation_lag = excluded.attestation_lag,
        skip_list_cell_hash = excluded.skip_list_cell_hash
      |sql}

    let insert_skip_list_cell :
        (Skip_list_hash.t * Skip_list_cell.t, unit, [`Zero]) t =
      (t2 skip_list_hash skip_list_cell ->. unit)
      @@ {sql|
      INSERT INTO skip_list_cells
      (hash, cell)
      VALUES ($1, $2)
      ON CONFLICT(hash) DO UPDATE SET cell = $2
      |sql}

    let delete_skip_list_cell : (level, unit, [`Zero]) t =
      (published_level ->. unit)
      @@ {sql|
      DELETE FROM skip_list_cells
      WHERE hash IN (
        SELECT skip_list_cell_hash
        FROM skip_list_slots
        WHERE published_level = $1)
      |sql}

    let delete_skip_list_slot : (level, unit, [`Zero]) t =
      (published_level ->. unit)
      @@ {sql|
      DELETE FROM skip_list_slots
      WHERE published_level = $1
      |sql}
  end

  let find_opt ?conn store skip_list_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.find_opt skip_list_hash

  let find_by_slot_id_opt ?conn store slot_id =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt
      conn
      Q.find_by_slot_id_opt
      ( slot_id.Tezos_dal_node_services.Types.Slot_id.slot_level,
        slot_id.slot_index )

  let find_by_level ?conn store ~published_level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.find_by_level published_level

  let remove ?conn store ~published_level =
    let open Lwt_result_syntax in
    with_connection store conn @@ fun conn ->
    let* () = Sqlite.Db.exec conn Q.delete_skip_list_cell published_level in
    let* () = Sqlite.Db.exec conn Q.delete_skip_list_slot published_level in
    return_unit

  let insert ?conn store ~attested_level items extract =
    let open Lwt_result_syntax in
    with_connection store conn @@ fun conn ->
    List.iter_es
      (fun item ->
        let cell_hash, cell, slot_index, attestation_lag = extract item in
        let published_level =
          Int32.(sub attested_level (of_int attestation_lag))
        in
        let* () =
          Sqlite.Db.exec
            conn
            Q.insert_skip_list_slot
            (published_level, slot_index, attestation_lag, cell_hash)
        in
        Sqlite.Db.exec conn Q.insert_skip_list_cell (cell_hash, cell))
      items

  let init ~data_dir ~perm () =
    let path = data_dir // sqlite_file_name in
    let read_only = match perm with Sqlite.Read_only _ -> true | _ -> false in
    let migration conn =
      Sqlite.assert_in_transaction conn ;
      Sqlite.with_connection conn @@ fun db_conn ->
      Migrations.apply
        db_conn
        ~read_only
        ~on_future:Dal_store_sqlite3_events.migrations_from_the_future
        ~on_applied:Dal_store_sqlite3_events.applied_migration
        ()
    in
    Sqlite.init ~path ~perm migration

  let close store = Sqlite.close store

  let use t f = use t f

  let schemas t = use t Schemas.get_all

  module Internal_for_tests = struct
    module Q = struct
      open Sqlite.Request
      open Dal_proto_types

      let skip_list_hash_exists : (Skip_list_hash.t, bool, [`One]) t =
        (skip_list_hash ->! bool)
        @@ {sql|
    SELECT EXISTS (
      SELECT cell
      FROM skip_list_cells
      WHERE hash = $1
    )|sql}
    end

    let skip_list_hash_exists ?conn store skip_list_hash =
      with_connection store conn @@ fun conn ->
      Sqlite.Db.find conn Q.skip_list_hash_exists skip_list_hash
  end
end
