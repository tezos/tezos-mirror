(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix
include Sqlite

type levels = {
  l1_level : int32;
  current_number : Ethereum_types.quantity;
  finalized : Ethereum_types.quantity;
}

type pending_kernel_upgrade = {
  kernel_upgrade : Evm_events.Upgrade.t;
  injected_before : Ethereum_types.quantity;
}

type metadata = {
  smart_rollup_address : Address.t;
  history_mode : Configuration.history_mode;
}

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

  let quantity =
    custom
      ~encode:(fun (Qty x) -> Ok Z.(to_bits x))
      ~decode:(fun x -> Ok (Qty Z.(of_bits x)))
      string

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

  let address =
    let open Ethereum_types in
    custom
      ~encode:(fun (Address (Hex address)) ->
        Result.of_option ~error:"not a valid address"
        @@ Hex.to_string (`Hex address))
      ~decode:(fun address ->
        let (`Hex address) = Hex.of_string address in
        Ok (Address (Hex address)))
      string

  let block_hash =
    let open Ethereum_types in
    custom
      ~encode:(fun (Block_hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Block_hash (Hex hash)))
      string

  let block =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Ethereum_types.(block_encoding legacy_transaction_object_encoding)
             payload))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid block payload"
        @@ Data_encoding.Binary.of_string_opt
             Ethereum_types.(block_encoding legacy_transaction_object_encoding)
             bytes)
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
      ~encode:(fun Evm_events.Upgrade.{hash; timestamp} -> Ok (hash, timestamp))
      ~decode:(fun (hash, timestamp) -> Ok Evm_events.Upgrade.{hash; timestamp})
      (t2 root_hash timestamp)

  let delayed_transaction =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Evm_events.Delayed_transaction.encoding
             payload))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid blueprint payload"
        @@ Data_encoding.Binary.of_string_opt
             Evm_events.Delayed_transaction.encoding
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

  let history_mode =
    custom
      ~encode:(fun mode -> Ok (Configuration.string_of_history_mode_debug mode))
      ~decode:(fun str ->
        Option.to_result
          ~none:(Format.sprintf "Cannot decode %S" str)
          Configuration.(history_mode_of_string_opt str))
      string

  let levels =
    custom
      ~encode:(fun {current_number; l1_level; finalized} ->
        Ok (current_number, l1_level, finalized))
      ~decode:(fun (current_number, l1_level, finalized) ->
        Ok {current_number; l1_level; finalized})
      (t3 level l1_level level)

  let pending_kernel_upgrade =
    product (fun injected_before hash timestamp ->
        {injected_before; kernel_upgrade = Evm_events.Upgrade.{hash; timestamp}})
    @@ proj level (fun k -> k.injected_before)
    @@ proj root_hash (fun k -> k.kernel_upgrade.hash)
    @@ proj timestamp (fun k -> k.kernel_upgrade.timestamp)
    @@ proj_end

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

    (*
      To introduce a new migration

        - Create a .sql file led by the next migration number [N = version + 1]
          (with leading 0s) followed by the name of the migration (e.g.
          [005_create_blueprints_table.sql])
        - Run [etherlink/scripts/check_evm_store_migrations.sh promote]
        - Increment [version]
        - Regenerate the schemas, using [[
              dune exec -- etherlink/tezt/tests/main.exe --file evm_sequencer.ml \
                evm store schemas regression --reset-regressions
          ]]

      You can review the result at
      [etherlink/tezt/tests/expected/evm_sequencer.ml/EVM Node- debug print store schemas.out].
    *)
    let version = 19

    let all : Evm_node_migrations.migration list =
      Evm_node_migrations.migrations version
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

    let clear_before =
      (level ->. unit) @@ {|DELETE FROM blueprints WHERE id < ?|}
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

    let clear_before =
      (level ->. unit) @@ {|DELETE FROM context_hashes WHERE id < ?|}
  end

  module Kernel_upgrades = struct
    let insert =
      (t3 level root_hash timestamp ->. unit)
      @@ {|REPLACE INTO kernel_upgrades (injected_before, root_hash, activation_timestamp) VALUES (?, ?, ?)|}

    let activation_levels =
      (unit ->* level)
      @@ {|SELECT applied_before
           FROM kernel_upgrades
           WHERE applied_before IS NOT NULL
           ORDER BY applied_before DESC
    |}

    let get_latest_unapplied =
      (unit ->? pending_kernel_upgrade)
      @@ {|SELECT injected_before, root_hash, activation_timestamp
           FROM kernel_upgrades WHERE applied_before IS NULL
           ORDER BY injected_before DESC
           LIMIT 1
    |}

    let find_injected_before =
      (level ->? upgrade)
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE injected_before = ?|}

    let find_latest_injected_after =
      (level ->? upgrade)
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE injected_before > ?
           ORDER BY injected_before DESC
           LIMIT 1|}

    let record_apply =
      (level ->. unit)
      @@ {|
      UPDATE kernel_upgrades SET applied_before = ? WHERE applied_before IS NULL
    |}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM kernel_upgrades WHERE injected_before > ?|}

    let nullify_after =
      (level ->. unit)
      @@ {|UPDATE kernel_upgrades SET applied_before = NULL WHERE applied_before > ?|}

    let clear_before =
      (level ->. unit)
      @@ {|DELETE FROM kernel_upgrades WHERE injected_before < ?|}
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

    let clear_before =
      (level ->. unit)
      @@ {|DELETE FROM delayed_transactions WHERE injected_before < ?|}
  end

  module L1_l2_levels_relationships = struct
    let insert =
      (t3 level l1_level level ->. unit)
      @@ {|INSERT INTO l1_l2_levels_relationships (latest_l2_level, l1_level, finalized_l2_level) VALUES (?, ?, ?)|}

    let get =
      (unit ->! levels)
      @@ {|SELECT latest_l2_level, l1_level, finalized_l2_level FROM l1_l2_levels_relationships ORDER BY latest_l2_level DESC LIMIT 1|}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM l1_l2_levels_relationships WHERE latest_l2_level > ?|}

    let clear_before =
      (level ->. unit)
      @@ {|DELETE FROM l1_l2_levels_relationships WHERE latest_l2_level < ?|}
  end

  module Metadata = struct
    let insert_smart_rollup_address =
      (smart_rollup_address ->. unit)
      @@ {|
INSERT INTO metadata (key, value) VALUES ('smart_rollup_address', ?)
ON CONFLICT(key)
DO UPDATE SET value = excluded.value
|}

    let get_smart_rollup_address =
      (unit ->! smart_rollup_address)
      @@ {|SELECT value from metadata WHERE key = 'smart_rollup_address'|}

    let insert_history_mode =
      (history_mode ->. unit)
      @@ {|
INSERT INTO metadata (key, value) VALUES ('history_mode', ?)
ON CONFLICT(key)
DO UPDATE SET value = excluded.value
|}

    let get_history_mode =
      (unit ->! history_mode)
      @@ {|SELECT value from metadata WHERE key = 'history_mode'|}
  end

  module Transactions = struct
    let receipt_fields =
      custom
        ~encode:(fun payload ->
          Ok
            (Data_encoding.Binary.to_string_exn
               Transaction_info.receipt_fields_encoding
               payload))
        ~decode:(fun bytes ->
          Option.to_result ~none:"Not a valid receipt fields payload"
          @@ Data_encoding.Binary.of_string_opt
               Transaction_info.receipt_fields_encoding
               bytes)
        string

    let object_fields =
      custom
        ~encode:(fun payload ->
          Ok
            (Data_encoding.Binary.to_string_exn
               Transaction_info.object_fields_encoding
               payload))
        ~decode:(fun bytes ->
          Option.to_result ~none:"Not a valid object fields payload"
          @@ Data_encoding.Binary.of_string_opt
               Transaction_info.object_fields_encoding
               bytes)
        string

    let insert =
      t8
        block_hash
        level
        quantity
        root_hash
        address
        (option address)
        receipt_fields
        object_fields
      ->. unit
      @@ {eos|INSERT INTO transactions (block_hash, block_number, index_, hash, from_, to_, receipt_fields, object_fields) VALUES (?, ?, ?, ?, ?, ?, ?, ?)|eos}

    let select_receipt =
      root_hash
      ->? t7
            block_hash
            level
            quantity
            root_hash
            address
            (option address)
            receipt_fields
      @@ {eos|SELECT block_hash, block_number, index_, hash, from_, to_, receipt_fields FROM transactions WHERE hash = ?|eos}

    let select_receipts_from_block_number =
      level
      ->* t6
            block_hash
            quantity
            root_hash
            address
            (option address)
            receipt_fields
      @@ {eos|SELECT block_hash, index_, hash, from_, to_, receipt_fields FROM transactions WHERE block_number = ?|eos}

    let select_object =
      root_hash
      ->? t7
            block_hash
            level
            quantity
            root_hash
            address
            (option address)
            object_fields
      @@ {eos|SELECT block_hash, block_number, index_, hash, from_, to_, object_fields FROM transactions WHERE hash = ?|eos}

    let select_objects_from_block_number =
      (level ->* t5 quantity root_hash address (option address) object_fields)
      @@ {eos|SELECT index_, hash, from_, to_, object_fields FROM transactions WHERE block_number = ?|eos}

    let clear_after =
      (level ->. unit) @@ {|DELETE FROM transactions WHERE block_number > ?|}

    let clear_before =
      (level ->. unit) @@ {|DELETE FROM transactions WHERE block_number < ?|}
  end

  module Block_storage_mode = struct
    let legacy =
      (unit ->! Caqti_type.Std.bool)
      @@ {|SELECT legacy FROM block_storage_mode|}

    let force_legacy =
      (unit ->. unit) @@ {|UPDATE block_storage_mode SET legacy = 1|}
  end

  module Blocks = struct
    let insert =
      (t3 level block_hash block ->. unit)
      @@ {eos|INSERT INTO blocks (level, hash, block) VALUES (?, ?, ?)|eos}

    let select_with_level =
      (level ->? block) @@ {eos|SELECT block FROM blocks WHERE level = ?|eos}

    let select_with_hash =
      (block_hash ->? block)
      @@ {eos|SELECT block FROM blocks WHERE hash = ?|eos}

    let select_hash_of_number =
      (level ->? block_hash)
      @@ {eos|SELECT hash FROM blocks WHERE level = ?|eos}

    let select_number_of_hash =
      (block_hash ->? level)
      @@ {eos|SELECT level FROM blocks WHERE hash = ?|eos}

    let clear_after = (level ->. unit) @@ {|DELETE FROM blocks WHERE level > ?|}

    let clear_before =
      (level ->. unit) @@ {|DELETE FROM blocks WHERE level < ?|}
  end

  let context_hash_of_block_hash =
    (block_hash ->? context_hash)
    @@ {eos|SELECT c.context_hash from Context_hashes c JOIN Blocks b on c.id = b.level WHERE hash = ?|eos}

  module Irmin_chunks = struct
    let insert =
      (t2 level timestamp ->. unit)
      @@ {|INSERT INTO irmin_chunks (level, timestamp) VALUES (?, ?)|}

    let nth =
      (int ->? t2 level timestamp)
      @@ {|SELECT level, timestamp from irmin_chunks ORDER BY level DESC LIMIT 1 OFFSET ?|}

    let latest =
      (unit ->? t2 level timestamp)
      @@ {|SELECT level, timestamp from irmin_chunks ORDER BY level DESC LIMIT 1|}

    let clear = (unit ->. unit) @@ {|DELETE FROM irmin_chunks|}

    let clear_after =
      (level ->. unit) @@ {|DELETE FROM irmin_chunks WHERE level > ?|}

    let clear_before_included =
      (level ->. unit) @@ {|DELETE FROM irmin_chunks WHERE level <= ?|}
  end

  module Pending_confirmations = struct
    let insert =
      (t2 level block_hash ->. unit)
      @@ {|INSERT INTO pending_confirmations (level, hash) VALUES (?, ?)|}

    let select_with_level =
      (level ->? block_hash)
      @@ {|SELECT hash FROM pending_confirmations WHERE level = ?|}

    let delete_with_level =
      (level ->. unit) @@ {|DELETE FROM pending_confirmations WHERE level = ?|}

    let clear = (unit ->. unit) @@ {|DELETE FROM pending_confirmations|}

    let count =
      (unit ->! level) @@ {|SELECT COUNT(*) FROM pending_confirmations|}
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
        (fun (i, ((module M : Evm_node_migrations.S) as mig)) ->
          let* () = Migrations.apply_migration conn i mig in
          let*! () = Evm_store_events.applied_migration M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  Sqlite.init ~path ~perm migration

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

  let get_latest store =
    let open Lwt_result_syntax in
    let* candidate = find_latest store in
    match candidate with
    | Some c -> return c
    | None -> failwith "Could not fetch latest context hash from store"

  let find_earliest store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Context_hashes.get_earliest ()

  let get_earliest store =
    let open Lwt_result_syntax in
    let* candidate = find_earliest store in
    match candidate with
    | Some c -> return c
    | None -> failwith "Could not fetch earliest context hash from store"

  let find_finalized store =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* l1_l2_levels = Db.find_opt conn Q.L1_l2_levels_relationships.get () in
    match l1_l2_levels with
    | None -> return_none
    | Some {current_number = Qty current_number; finalized = Qty finalized; _}
      ->
        let min = Ethereum_types.Qty (Z.min current_number finalized) in
        let+ hash = Db.find_opt conn Q.Context_hashes.select min in
        Option.map (fun hash -> (min, hash)) hash

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Context_hashes.clear_after l2_level

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Context_hashes.clear_before l2_level
end

module Kernel_upgrades = struct
  let store store next_blueprint_number (event : Evm_events.Upgrade.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Kernel_upgrades.insert
      (next_blueprint_number, event.hash, event.timestamp)

  let find_latest_pending store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Kernel_upgrades.get_latest_unapplied ()

  let find_injected_before store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Kernel_upgrades.find_injected_before level

  let find_latest_injected_after store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Kernel_upgrades.find_latest_injected_after level

  let record_apply store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Kernel_upgrades.record_apply level

  let clear_after store l2_level =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = Db.exec conn Q.Kernel_upgrades.clear_after l2_level in
    Db.exec conn Q.Kernel_upgrades.nullify_after l2_level

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Kernel_upgrades.clear_before l2_level

  let activation_levels store =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Kernel_upgrades.activation_levels ()
end

module Delayed_transactions = struct
  let store store next_blueprint_number
      (delayed_transaction : Evm_events.Delayed_transaction.t) =
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

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Delayed_transactions.clear_before l2_level
end

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

  let find_with_events conn level =
    let open Lwt_result_syntax in
    let* blueprint = find conn level in
    let* kernel_upgrade = Kernel_upgrades.find_injected_before conn level in
    match blueprint with
    | None -> return None
    | Some blueprint ->
        let* delayed_transactions = Delayed_transactions.at_level conn level in
        return_some
          Blueprint_types.{delayed_transactions; kernel_upgrade; blueprint}

  let get_with_events conn level =
    let open Lwt_result_syntax in
    let* blueprint_candidate = find_with_events conn level in
    match blueprint_candidate with
    | Some blueprint -> return blueprint
    | None ->
        failwith
          "Could not read blueprint %a from store"
          Ethereum_types.pp_quantity
          level

  let find_range store ~from ~to_ =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Blueprints.select_range (from, to_)

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Blueprints.clear_after l2_level

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Blueprints.clear_before l2_level
end

module L1_l2_levels_relationships = struct
  type t = levels = {
    l1_level : int32;
    current_number : Ethereum_types.quantity;
    finalized : Ethereum_types.quantity;
  }

  let store store ~l1_level ~latest_l2_level ~finalized_l2_level =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.L1_l2_levels_relationships.insert
      (latest_l2_level, l1_level, finalized_l2_level)

  let find store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_l2_levels_relationships.get ()

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_l2_levels_relationships.clear_after l2_level

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_l2_levels_relationships.clear_before l2_level
end

module Metadata = struct
  let store store {smart_rollup_address; history_mode} =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () =
      Db.exec conn Q.Metadata.insert_smart_rollup_address smart_rollup_address
    in
    Db.exec conn Q.Metadata.insert_history_mode history_mode

  let get store =
    with_connection store @@ fun conn ->
    let open Lwt_result_syntax in
    let* smart_rollup_address =
      Db.find conn Q.Metadata.get_smart_rollup_address ()
    in
    let* history_mode = Db.find conn Q.Metadata.get_history_mode () in
    return {smart_rollup_address; history_mode}

  let find store =
    with_connection store @@ fun conn ->
    let open Lwt_result_syntax in
    let* smart_rollup_address =
      Db.find_opt conn Q.Metadata.get_smart_rollup_address ()
    in
    let* history_mode = Db.find_opt conn Q.Metadata.get_history_mode () in
    match (smart_rollup_address, history_mode) with
    | Some smart_rollup_address, Some history_mode ->
        return_some {smart_rollup_address; history_mode}
    | _ -> return_none

  let store_history_mode store history_mode =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Metadata.insert_history_mode history_mode

  let find_history_mode store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Metadata.get_history_mode ()

  let get_history_mode store =
    with_connection store @@ fun conn ->
    Db.find conn Q.Metadata.get_history_mode ()
end

module Transactions = struct
  let store store
      ({
         block_hash;
         block_number;
         index;
         from;
         hash;
         to_;
         receipt_fields;
         object_fields;
       } :
        Transaction_info.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Transactions.insert
      ( block_hash,
        block_number,
        index,
        hash,
        from,
        to_,
        receipt_fields,
        object_fields )

  let find_receipt store hash =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let+ receipt = Db.find_opt conn Q.Transactions.select_receipt hash in
    Option.map
      (fun ( block_hash,
             block_number,
             index,
             hash,
             from,
             to_,
             Transaction_info.
               {
                 cumulative_gas_used;
                 effective_gas_price;
                 gas_used;
                 logs;
                 logs_bloom;
                 type_;
                 status;
                 contract_address;
               } ) ->
        Transaction_receipt.
          {
            transactionHash = hash;
            transactionIndex = index;
            blockHash = block_hash;
            blockNumber = block_number;
            from;
            to_;
            cumulativeGasUsed = cumulative_gas_used;
            effectiveGasPrice = effective_gas_price;
            gasUsed = gas_used;
            logs;
            logsBloom = logs_bloom;
            type_;
            status;
            contractAddress = contract_address;
          })
      receipt

  let receipts_of_block_number store level =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let+ rows =
      Db.collect_list
        conn
        Q.Transactions.select_receipts_from_block_number
        level
    in
    List.map
      (fun ( block_hash,
             index,
             hash,
             from,
             to_,
             Transaction_info.
               {
                 cumulative_gas_used;
                 effective_gas_price;
                 gas_used;
                 logs;
                 logs_bloom;
                 type_;
                 status;
                 contract_address;
               } ) ->
        Transaction_receipt.
          {
            transactionHash = hash;
            transactionIndex = index;
            blockHash = block_hash;
            blockNumber = level;
            from;
            to_;
            cumulativeGasUsed = cumulative_gas_used;
            effectiveGasPrice = effective_gas_price;
            gasUsed = gas_used;
            logs;
            logsBloom = logs_bloom;
            type_;
            status;
            contractAddress = contract_address;
          })
      rows

  let find_object store hash =
    let open Lwt_result_syntax in
    let* object_ =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Transactions.select_object hash
    in
    let legacy_object =
      Option.map
        (fun ( block_hash,
               block_number,
               index,
               hash,
               from,
               to_,
               Transaction_info.{gas; gas_price; input; nonce; value; v; r; s}
             ) ->
          Ethereum_types.
            {
              blockHash = Some block_hash;
              blockNumber = Some block_number;
              from;
              gas;
              gasPrice = gas_price;
              hash;
              input;
              nonce;
              to_;
              transactionIndex = Some index;
              value;
              v;
              r;
              s;
            })
        object_
    in
    match legacy_object with
    | Some ({blockNumber = Some number; _} as obj) -> (
        let* blueprint = Blueprints.find store number in
        match blueprint with
        | Some blueprint ->
            let*? obj = Transaction_object.reconstruct blueprint.payload obj in
            return_some obj
        | None ->
            return_some (Transaction_object.from_store_transaction_object obj))
    | Some _ ->
        (* We should not end up in this branch, per the function used in above
           [Option.map]. We do not store transactions which have not been
           included in a block already. *)
        assert false
    | None -> return_none

  let clear_after store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Transactions.clear_after level

  let clear_before store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Transactions.clear_before level
end

module Irmin_chunks = struct
  let insert conn level timestamp =
    with_connection conn @@ fun conn ->
    Db.exec conn Q.Irmin_chunks.insert (level, timestamp)

  let nth conn n =
    with_connection conn @@ fun conn -> Db.find_opt conn Q.Irmin_chunks.nth n

  let latest conn =
    with_connection conn @@ fun conn ->
    Db.find_opt conn Q.Irmin_chunks.latest ()

  let clear store =
    with_connection store @@ fun conn -> Db.exec conn Q.Irmin_chunks.clear ()

  let clear_after store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Irmin_chunks.clear_after level

  let clear_before_included store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Irmin_chunks.clear_before_included level
end

module Blocks = struct
  let store store
      (block : Ethereum_types.legacy_transaction_object Ethereum_types.block) =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Blocks.insert (block.number, block.hash, block)

  let block_with_objects store block =
    let open Lwt_result_syntax in
    let* rows =
      with_connection store @@ fun conn ->
      Db.collect_list
        conn
        Q.Transactions.select_objects_from_block_number
        block.Ethereum_types.number
    in
    let objects_ =
      List.map
        (fun ( index,
               hash,
               from,
               to_,
               Transaction_info.{gas; gas_price; input; nonce; value; v; r; s}
             ) ->
          Ethereum_types.
            {
              blockHash = Some block.hash;
              blockNumber = Some block.number;
              from;
              gas;
              gasPrice = gas_price;
              hash;
              input;
              nonce;
              to_;
              transactionIndex = Some index;
              value;
              v;
              r;
              s;
            })
        rows
    in
    let block = {block with transactions = TxFull objects_} in
    let* blueprint = Blueprints.find store block.number in
    let*? res =
      match blueprint with
      | Some blueprint ->
          Transaction_object.reconstruct_block blueprint.payload block
      | None -> Ok (Transaction_object.block_from_legacy block)
    in
    return res

  let find_with_level ~full_transaction_object store level =
    let open Lwt_result_syntax in
    let* block_opt =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Blocks.select_with_level level
    in
    if full_transaction_object then
      Option.map_es (block_with_objects store) block_opt
    else return (Option.map Transaction_object.block_from_legacy block_opt)

  let find_with_hash ~full_transaction_object store hash =
    let open Lwt_result_syntax in
    let* block_opt =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Blocks.select_with_hash hash
    in
    if full_transaction_object then
      Option.map_es (block_with_objects store) block_opt
    else return (Option.map Transaction_object.block_from_legacy block_opt)

  let find_hash_of_number store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Blocks.select_hash_of_number level

  let find_number_of_hash store hash =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Blocks.select_number_of_hash hash

  let clear_after store level =
    with_connection store @@ fun conn -> Db.exec conn Q.Blocks.clear_after level

  let clear_before store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Blocks.clear_before level
end

module Block_storage_mode = struct
  let legacy store =
    with_connection store @@ fun conn ->
    Db.find conn Q.Block_storage_mode.legacy ()

  let force_legacy store =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Block_storage_mode.force_legacy ()
end

module Pending_confirmations = struct
  let insert store level hash =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Pending_confirmations.insert (level, hash)

  let find_with_level store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Pending_confirmations.select_with_level level

  let delete_with_level store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Pending_confirmations.delete_with_level level

  let clear store =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Pending_confirmations.clear ()

  let is_empty store =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* (Qty count) = Db.find conn Q.Pending_confirmations.count () in
    return Z.(equal zero count)
end

let context_hash_of_block_hash store hash =
  with_connection store @@ fun conn ->
  Db.find_opt conn Q.context_hash_of_block_hash hash

let reset_after store ~l2_level =
  let open Lwt_result_syntax in
  let* () = Blueprints.clear_after store l2_level in
  let* () = Context_hashes.clear_after store l2_level in
  let* () = L1_l2_levels_relationships.clear_after store l2_level in
  let* () = Kernel_upgrades.clear_after store l2_level in
  let* () = Delayed_transactions.clear_after store l2_level in
  let* () = Blocks.clear_after store l2_level in
  let* () = Transactions.clear_after store l2_level in
  (* Blocks in [Pending_confirmations] are always after the current head. *)
  let* () = Pending_confirmations.clear store in
  (* If splits were produced on the resetted branch, they will be garbage
     collected by the GC anyway later. *)
  let* () = Irmin_chunks.clear_after store l2_level in
  return_unit

let reset_before store ~l2_level ~history_mode =
  let open Lwt_result_syntax in
  let* () = Context_hashes.clear_before store l2_level in
  let* () = L1_l2_levels_relationships.clear_before store l2_level in
  let* () =
    match history_mode with
    | Configuration.Rolling _ ->
        let* () = Blueprints.clear_before store l2_level in
        let* () = Blocks.clear_before store l2_level in
        let* () = Transactions.clear_before store l2_level in
        let* () = Kernel_upgrades.clear_before store l2_level in
        let* () = Delayed_transactions.clear_before store l2_level in
        return_unit
    | _ -> return_unit
  in

  (* {!reset_before} is called when garbage collector is trigerred.
     Garbage collector is trigerred when the maximum number of splits is
     reached, [l2_level] was the pointer to the first split to remove.

     If it wasn't included, the garbage collector would keep the maximum
     number of splits plus an additional one. *)
  let* () = Irmin_chunks.clear_before_included store l2_level in
  return_unit
