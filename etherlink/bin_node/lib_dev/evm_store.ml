(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix

type error += Caqti_error of string

type pool =
  ( Caqti_lwt.connection,
    [Caqti_error.connect | `K_error of tztrace] )
  Caqti_lwt_unix.Pool.t

type sqlite_journal_mode = Wal | Other

module Db = struct
  let caqti (p : ('a, Caqti_error.t) result Lwt.t) : 'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*! p in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let caqti' (p : ('a, Caqti_error.t) result) : 'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let use_pool (pool : pool) (k : Caqti_lwt.connection -> 'a tzresult Lwt.t) =
    let open Lwt_result_syntax in
    let*! res =
      Caqti_lwt_unix.Pool.use
        (fun conn ->
          let*! res = k conn in
          match res with
          | Ok res -> return res
          | Error err -> fail (`K_error err))
        pool
    in
    match res with
    | Ok err -> return err
    | Error (`K_error err) -> fail err
    | Error (#Caqti_error.connect as err) ->
        fail [Caqti_error (Caqti_error.show err)]

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

type t = Pool : {db_pool : pool} -> t

type conn =
  | Raw_connection of (module Caqti_lwt.CONNECTION)
  | Ongoing_transaction of (module Caqti_lwt.CONNECTION)

let assert_in_transaction conn =
  match conn with
  | Raw_connection _ -> assert false
  | Ongoing_transaction _ -> ()

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
      ~encode:(fun hash ->
        Ok
          (hash |> Irmin_context.context_hash_of_hash
         |> Smart_rollup_context_hash.to_context_hash
         |> Context_hash.to_b58check))
      ~decode:(fun bytes ->
        let open Result_syntax in
        let+ hash =
          Option.to_result ~none:"Not a valid b58check encoded hash"
          @@ Context_hash.of_b58check_opt bytes
        in
        hash |> Smart_rollup_context_hash.of_context_hash
        |> Irmin_context.hash_of_context_hash)
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

  let delayed_transaction =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Ethereum_types.Delayed_transaction.encoding
             payload))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid blueprint payload"
        @@ Data_encoding.Binary.of_string_opt
             Ethereum_types.Delayed_transaction.encoding
             bytes)
      string

  let smart_rollup_address =
    custom
      ~encode:(fun smart_rollup_address ->
        Ok
          (Tezos_crypto.Hashed.Smart_rollup_address.to_b58check
             smart_rollup_address))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid smart rollup address"
        @@ Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_opt bytes)
      string

  let journal_mode =
    custom
      ~encode:(function Wal -> Ok "wal" | Other -> Ok "delete")
      ~decode:(function "wal" -> Ok Wal | _ -> Ok Other)
      string

  let table_exists =
    (string ->! bool)
    @@ {|
    SELECT EXISTS (
      SELECT name FROM sqlite_master
      WHERE type='table'
        AND name=?
    )|}

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

    (*
      To introduce a new migration

        - Create a .sql file led by the next migration number [N = version + 1]
          (with leading 0s) followed by the name of the migration (e.g.
          [005_create_blueprints_table.sql])
        - Run [etherlink/scripts/check_evm_store_migrations.sh promote]
        - Increment [version]
    *)
    let version = 7

    let all : Evm_node_migrations.migration list =
      Evm_node_migrations.migrations version
  end

  module Journal_mode = struct
    let get = (unit ->! journal_mode) @@ {|PRAGMA journal_mode|}

    (* It does not appear possible to write a request {|PRAGMA journal_mode=?|}
       accepted by caqti, sadly. *)

    let set_wal = (unit ->! journal_mode) @@ {|PRAGMA journal_mode=wal|}
  end

  module Blueprints = struct
    let insert =
      (t3 level timestamp payload ->. unit)
      @@ {eos|INSERT INTO blueprints (id, timestamp, payload) VALUES (?, ?, ?)|eos}

    let select =
      (level ->? t2 payload timestamp)
      @@ {eos|SELECT payload, timestamp FROM blueprints WHERE id = ?|eos}

    let select_range =
      (t2 level level ->* t2 level payload)
      @@ {|SELECT id, payload FROM blueprints
           WHERE ? <= id AND id <= ?
           ORDER BY id ASC|}

    let clear_after =
      (level ->. unit) @@ {|DELETE FROM blueprints WHERE id > ?|}
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

    let get_earliest =
      (unit ->? t2 level context_hash)
      @@ {|SELECT id, context_hash FROM context_hashes
           WHERE id >= 0 ORDER BY id ASC LIMIT 1|}

    let clear_after =
      (level ->. unit) @@ {|DELETE FROM context_hashes WHERE id > ?|}
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

    let find_applied_before =
      (level ->? upgrade)
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE applied_before = ?|}

    let record_apply =
      (level ->. unit)
      @@ {|
      UPDATE kernel_upgrades SET applied_before = ? WHERE applied_before IS NULL
    |}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM kernel_upgrades WHERE injected_before > ?|}
  end

  module Delayed_transactions = struct
    let insert =
      (t3 level root_hash delayed_transaction ->. unit)
      @@ {|INSERT INTO delayed_transactions (injected_before, hash, payload) VALUES (?, ?, ?)|}

    let select_at_level =
      (level ->* delayed_transaction)
      @@ {|SELECT payload FROM delayed_transactions WHERE ? = injected_before|}

    let select_at_hash =
      (root_hash ->? delayed_transaction)
      @@ {|SELECT payload FROM delayed_transactions WHERE ? = hash|}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM delayed_transactions WHERE injected_before > ?|}
  end

  module L1_latest_level = struct
    let insert =
      (t2 level l1_level ->. unit)
      @@ {|INSERT INTO l1_latest_level_with_l2_level (l2_level, l1_level) VALUES (?, ?)|}

    let get =
      (unit ->! t2 level l1_level)
      @@ {|SELECT l2_level, l1_level  FROM l1_latest_level_with_l2_level ORDER BY l2_level DESC LIMIT 1|}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM l1_latest_level_with_l2_level WHERE l2_level > ?|}
  end

  module Metadata = struct
    let insert =
      (smart_rollup_address ->. unit)
      @@ {|INSERT INTO metadata (smart_rollup_address) VALUES (?)|}

    let get =
      (unit ->! smart_rollup_address)
      @@ {|SELECT smart_rollup_address from metadata|}
  end
end

let with_connection conn k =
  match conn with
  | Ongoing_transaction conn -> k conn
  | Raw_connection conn -> k conn

let with_transaction conn k =
  let open Lwt_result_syntax in
  match conn with
  | Raw_connection conn -> (
      let* () = Db.start conn in
      let*! res =
        Lwt.catch
          (fun () -> k (Ongoing_transaction conn))
          (fun exn -> fail_with_exn exn)
      in

      match res with
      | Ok x ->
          let* () = Db.commit conn in
          return x
      | Error err ->
          let* () = Db.rollback conn in
          fail err)
  | Ongoing_transaction _ ->
      failwith "Internal error: attempting to perform a nested transaction"

module Journal_mode = struct
  let set_wal_journal_mode store =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* current_mode = Db.find conn Q.Journal_mode.get () in
    when_ (current_mode <> Wal) @@ fun () ->
    let* _wal = Db.find conn Q.Journal_mode.set_wal () in
    return_unit
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
            Evm_store_events.migrations_from_the_future ~applied ~known
          in
          failwith
            "Cannot use a store modified by a more up-to-date version of the \
             EVM node"
    | None -> return all_migrations

  let apply_migration store id (module M : Evm_node_migrations.S) =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Db.exec conn up ()) M.up in
    Db.exec conn Q.Migrations.register_migration (id, M.name)
end

let use (Pool {db_pool}) k =
  Db.use_pool db_pool @@ fun conn -> k (Raw_connection conn)

let init ~data_dir ~perm () =
  let open Lwt_result_syntax in
  let path = data_dir // "store.sqlite" in
  let*! exists = Lwt_unix.file_exists path in
  let write_perm =
    match perm with `Read_only -> false | `Read_write -> true
  in
  let uri =
    Uri.of_string Format.(sprintf "sqlite3:%s?write=%b" path write_perm)
  in
  let* db_pool = Db.caqti' @@ Caqti_lwt_unix.connect_pool uri in
  let store = Pool {db_pool} in
  use store @@ fun conn ->
  let* () = Journal_mode.set_wal_journal_mode conn in
  let* () =
    with_transaction conn @@ fun conn ->
    let* () =
      if not exists then
        let* () = Migrations.create_table conn in
        let*! () = Evm_store_events.init_store () in
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
    let* () =
      List.iter_es
        (fun (i, ((module M : Evm_node_migrations.S) as mig)) ->
          let* () = Migrations.apply_migration conn i mig in
          let*! () = Evm_store_events.applied_migration M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  return store

module Blueprints = struct
  let store store (blueprint : Blueprint_types.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Blueprints.insert
      (blueprint.number, blueprint.timestamp, blueprint.payload)

  let find store number =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let+ opt = Db.find_opt conn Q.Blueprints.select number in
    match opt with
    | Some (payload, timestamp) ->
        Some Blueprint_types.{payload; timestamp; number}
    | None -> None

  let find_range store ~from ~to_ =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Blueprints.select_range (from, to_)

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Blueprints.clear_after l2_level
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

  let find_earliest store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Context_hashes.get_earliest ()

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Context_hashes.clear_after l2_level
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

  let find_applied_before store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Kernel_upgrades.find_applied_before level

  let record_apply store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Kernel_upgrades.record_apply level

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Kernel_upgrades.clear_after l2_level
end

module Delayed_transactions = struct
  let store store next_blueprint_number
      (delayed_transaction : Ethereum_types.Delayed_transaction.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Delayed_transactions.insert
      (next_blueprint_number, delayed_transaction.hash, delayed_transaction)

  let at_level store blueprint_number =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Delayed_transactions.select_at_level blueprint_number

  let at_hash store hash =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Delayed_transactions.select_at_hash hash

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Delayed_transactions.clear_after l2_level
end

module L1_latest_known_level = struct
  let store store l1_level l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_latest_level.insert (l1_level, l2_level)

  let find store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_latest_level.get ()

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_latest_level.clear_after l2_level
end

module Metadata = struct
  let store store smart_rollup_address =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Metadata.insert smart_rollup_address

  let get store =
    with_connection store @@ fun conn -> Db.find conn Q.Metadata.get ()

  let find store =
    with_connection store @@ fun conn -> Db.find_opt conn Q.Metadata.get ()
end

let reset store ~l2_level =
  let open Lwt_result_syntax in
  let* () = Blueprints.clear_after store l2_level in
  let* () = Context_hashes.clear_after store l2_level in
  let* () = L1_latest_known_level.clear_after store l2_level in
  let* () = Kernel_upgrades.clear_after store l2_level in
  let* () = Delayed_transactions.clear_after store l2_level in
  return_unit

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
