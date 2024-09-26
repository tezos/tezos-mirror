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
                "Fail to decode %s in database: %a"
                name
                Data_encoding.Binary.pp_read_error))
      octets

  let tzcustom ~encode ~decode t =
    custom
      ~encode:(fun v -> Ok (encode v))
      ~decode:(fun s ->
        decode s
        |> Result.map_error (fun err -> Format.asprintf "%a" pp_print_trace err))
      t

  let block_hash =
    tzcustom
      ~encode:Block_hash.to_b58check
      ~decode:Block_hash.of_b58check
      string

  let inbox_hash =
    tzcustom
      ~encode:Inbox_hash.to_b58check
      ~decode:Inbox_hash.of_b58check
      string

  let payload_hashes_hash =
    tzcustom
      ~encode:Merkelized_payload_hashes_hash.to_b58check
      ~decode:Merkelized_payload_hashes_hash.of_b58check
      string

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

  let protocol_hash =
    tzcustom
      ~encode:Protocol_hash.to_b58check
      ~decode:Protocol_hash.of_b58check
      string

  let dal_commitment =
    tzcustom
      ~encode:Dal.Commitment.to_b58check
      ~decode:Dal.Commitment.of_b58check
      string

  let dal_slot_index : Dal.Slot_index.t Caqti_type.t = int16

  let dal_slot_status =
    custom
      ~encode:(function `Confirmed -> Ok true | `Unconfirmed -> Ok false)
      ~decode:(function true -> Ok `Confirmed | false -> Ok `Unconfirmed)
      bool

  let z =
    custom
      ~encode:(fun i -> Ok (Z.to_string i))
      ~decode:(fun s ->
        try Ok (Z.of_string s) with _ -> Error "Invalid Z.t value in database")
      string

  let bitset = tzcustom ~encode:Bitset.to_z ~decode:Bitset.from_z z

  let history_proof =
    from_encoding ~name:"Inbox.history_proof" Inbox.history_proof_encoding

  let messages_list =
    from_encoding
      ~name:"message list"
      Data_encoding.(list @@ dynamic_size (Variable.string' Hex))

  let commitment =
    product (fun compressed_state inbox_level predecessor number_of_ticks ->
        Commitment.{compressed_state; inbox_level; predecessor; number_of_ticks})
    @@ proj state_hash (fun c -> c.Commitment.compressed_state)
    @@ proj level (fun c -> c.Commitment.inbox_level)
    @@ proj commitment_hash (fun c -> c.Commitment.predecessor)
    @@ proj int64 (fun c -> c.Commitment.number_of_ticks)
    @@ proj_end

  let inbox =
    product (fun level m -> Inbox.{level; old_levels_messages = m})
    @@ proj level (fun i -> i.Inbox.level)
    @@ proj history_proof (fun i -> i.Inbox.old_levels_messages)
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

module Commitments_published_at_levels = struct
  type publication_levels = {
    first_published_at_level : int32;
    published_at_level : int32 option;
  }

  module Q = struct
    open Types

    let publication_levels =
      product (fun first_published_at_level published_at_level ->
          {first_published_at_level; published_at_level})
      @@ proj level (fun p -> p.first_published_at_level)
      @@ proj (option level) (fun p -> p.published_at_level)
      @@ proj_end

    let register =
      (t2 commitment_hash publication_levels ->. unit)
      @@ {sql|
      REPLACE INTO commitments_published_at_levels
      (commitment_hash, first_published_at_level, published_at_level)
      VALUES (?, ?, ?)
      |sql}

    let select =
      (commitment_hash ->? publication_levels)
      @@ {sql|
      SELECT first_published_at_level, published_at_level
      FROM commitments_published_at_levels
      WHERE commitment_hash = ?
      |sql}

    let first_published =
      (commitment_hash ->? level)
      @@ {sql|
      SELECT first_published_at_level
      FROM commitments_published_at_levels
      WHERE commitment_hash = ?
        AND first_published_at_level IS NOT NULL
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM commitments_published_at_levels
      WHERE first_published_at_level < ?
      |sql}
  end

  let register ?conn store commitment levels =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.register (commitment, levels)

  let get ?conn store commitment =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select commitment

  let get_first_published_level ?conn store commitment =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.first_published commitment

  let delete_before ?conn store ~level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level
end

module Inboxes = struct
  module Q = struct
    open Types

    let insert =
      (t2 inbox_hash inbox ->. unit)
      @@ {sql|
      REPLACE INTO inboxes
      (hash, inbox_level, history_proof)
      VALUES (?, ?, ?)
      |sql}

    let select =
      (inbox_hash ->? inbox)
      @@ {sql|
      SELECT inbox_level, history_proof
      FROM inboxes
      WHERE hash = ?
      |sql}

    let select_by_block_hash =
      (block_hash ->? inbox)
      @@ {sql|
      SELECT i.inbox_level, i.history_proof
      FROM inboxes as i
      INNER JOIN l2_blocks as b
      ON b.inbox_hash = i.hash AND b.block_hash = ?
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM inboxes WHERE inbox_level < ?
      |sql}
  end

  let store ?conn store inbox =
    let open Lwt_result_syntax in
    with_connection store conn @@ fun conn ->
    let hash = Inbox.hash inbox in
    let+ () = Sqlite.Db.exec conn Q.insert (hash, inbox) in
    hash

  let find ?conn store inbox_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select inbox_hash

  let find_by_block_hash ?conn store inbox_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select_by_block_hash inbox_hash

  let delete_before ?conn store ~level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level
end

module Messages = struct
  module Q = struct
    open Types

    let insert =
      (t3 payload_hashes_hash level messages_list ->. unit)
      @@ {sql|
      REPLACE INTO messages
      (payload_hashes_hash, inbox_level, message_list)
      VALUES (?, ?, ?)
      |sql}

    let select =
      (payload_hashes_hash ->? t2 level messages_list)
      @@ {sql|
      SELECT inbox_level, message_list
      FROM messages
      WHERE payload_hashes_hash = ?
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM messages WHERE inbox_level < ?
      |sql}
  end

  let store ?conn store ~level payload_hashes_hash messages =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.insert (payload_hashes_hash, level, messages)

  let find ?conn store hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select hash

  let delete_before ?conn store ~level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level
end

module Outbox_messages = struct
  type messages_per_level = {messages : Bitset.t; executed_messages : Bitset.t}

  module Q = struct
    open Types

    let messages_per_level =
      product (fun messages executed_messages -> {messages; executed_messages})
      @@ proj bitset (fun m -> m.messages)
      @@ proj bitset (fun m -> m.executed_messages)
      @@ proj_end

    let register =
      (t2 level bitset ->. unit)
      @@ {sql|
      INSERT INTO outbox_messages
      (outbox_level, messages, executed_messages)
      VALUES ($1, $2, 0)
      ON CONFLICT DO UPDATE SET messages = $2
      |sql}

    let range =
      (t2 level level ->* t2 level messages_per_level)
      @@ {sql|
      SELECT outbox_level, messages, executed_messages
      FROM outbox_messages
      WHERE outbox_level BETWEEN ? AND ?
      ORDER BY outbox_level DESC
      |sql}

    let select_executed =
      (level ->? bitset)
      @@ {sql|
      SELECT executed_messages
      FROM outbox_messages
      WHERE outbox_level = ?
      |sql}

    let update_executed =
      (t2 level bitset ->. unit)
      @@ {sql|
      UPDATE outbox_messages
      SET executed_messages = $2
      WHERE outbox_level = $1
      |sql}
  end

  let pending ?conn store ~min_level ~max_level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.fold
      conn
      Q.range
      (fun (outbox_level, {messages; executed_messages}) acc ->
        let pending_at_level = Bitset.diff messages executed_messages in
        let l = Bitset.to_list pending_at_level in
        if List.is_empty l then acc else (outbox_level, l) :: acc)
      (min_level, max_level)
      []

  let register_outbox_messages ?conn store ~outbox_level ~indexes =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.register (outbox_level, indexes)

  let set_outbox_message_executed ?conn store ~outbox_level ~index =
    let open Lwt_result_syntax in
    with_connection store conn @@ fun conn ->
    let* executed_messages =
      Sqlite.Db.find_opt conn Q.select_executed outbox_level
    in
    match executed_messages with
    | None ->
        (* Not tracking *)
        return_unit
    | Some executed_messages ->
        let*? executed_messages = Bitset.add executed_messages index in
        Sqlite.Db.exec conn Q.update_executed (outbox_level, executed_messages)
end

module Protocols = struct
  type level = First_known of int32 | Activation_level of int32

  type proto_info = {
    level : level;
    proto_level : int;
    protocol : Protocol_hash.t;
  }

  module Q = struct
    open Types

    let proto_info =
      product (fun protocol proto_level first_level first_is_activation ->
          let level =
            if first_is_activation then Activation_level first_level
            else First_known first_level
          in
          {level; proto_level; protocol})
      @@ proj protocol_hash (fun p -> p.protocol)
      @@ proj int (fun p -> p.proto_level)
      @@ proj level (fun {level = First_known l | Activation_level l; _} -> l)
      @@ proj bool (function
             | {level = First_known _; _} -> false
             | {level = Activation_level _; _} -> true)
      @@ proj_end

    let insert =
      (proto_info ->. unit)
      @@ {sql|
      REPLACE INTO protocols
      (hash, proto_level, first_level, first_is_activation)
      VALUES (?, ?, ?, ?)
      |sql}

    let select =
      (protocol_hash ->? proto_info)
      @@ {sql|
      SELECT hash, proto_level, first_level, first_is_activation
      FROM protocols
      WHERE hash = ?
      |sql}

    let proto_of_level =
      (level ->? proto_info)
      @@ {sql|
      SELECT hash, proto_level, first_level, first_is_activation
      FROM protocols
      WHERE ($1 >= first_level AND first_is_activation = false)
         OR ($1 > first_level AND first_is_activation = true)
      ORDER BY first_level DESC LIMIT 1
      |sql}

    let last =
      (unit ->? proto_info)
      @@ {sql|
      SELECT hash, proto_level, first_level, first_is_activation
      FROM protocols
      ORDER BY first_level DESC LIMIT 1
      |sql}
  end

  let store ?conn store proto =
    with_connection store conn @@ fun conn -> Sqlite.Db.exec conn Q.insert proto

  let find ?conn store inbox_hash =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select inbox_hash

  let proto_of_level ?conn store level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.proto_of_level level

  let last ?conn store =
    with_connection store conn @@ fun conn -> Sqlite.Db.find_opt conn Q.last ()
end

module Dal_slots_headers = struct
  module Q = struct
    open Types

    let slot_header =
      let open Dal.Slot_header in
      product (fun index published_level commitment ->
          {id = {published_level; index}; commitment})
      @@ proj dal_slot_index (fun h -> h.id.index)
      @@ proj level (fun h -> h.id.published_level)
      @@ proj dal_commitment (fun h -> h.commitment)
      @@ proj_end

    let insert =
      (t2 block_hash slot_header ->. unit)
      @@ {sql|
      REPLACE INTO dal_slots_headers
      (block_hash, slot_index, published_level, slot_commitment)
      VALUES (?, ?, ?, ?)
      |sql}

    let find_slot_header =
      (t2 block_hash dal_slot_index ->? slot_header)
      @@ {sql|
      SELECT slot_index, published_level, slot_commitment
      FROM dal_slots_headers
      WHERE block_hash = ? AND slot_index = ?
      |sql}

    let select_slot_headers =
      (block_hash ->* slot_header)
      @@ {sql|
      SELECT slot_index, published_level, slot_commitment
      FROM dal_slots_headers
      WHERE block_hash = ?
      ORDER BY slot_index DESC
      |sql}

    let select_slot_indexes =
      (block_hash ->* dal_slot_index)
      @@ {sql|
      SELECT slot_index
      FROM dal_slots_headers
      WHERE block_hash = ?
      ORDER BY slot_index DESC
      |sql}
  end

  let store ?conn store block slot_header =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.insert (block, slot_header)

  let find_slot_header ?conn store block ~slot_index =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.find_slot_header (block, slot_index)

  let list_slot_headers ?conn store block =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.select_slot_headers block

  let list_slot_indexes ?conn store block =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.select_slot_indexes block
end

module Dal_slots_statuses = struct
  module Q = struct
    open Types

    let insert =
      (t3 block_hash dal_slot_index dal_slot_status ->. unit)
      @@ {sql|
      REPLACE INTO dal_slots_statuses
      (block_hash, slot_index, attested)
      VALUES (?, ?, ?)
      |sql}

    let find_slot_status =
      (t2 block_hash dal_slot_index ->? dal_slot_status)
      @@ {sql|
      SELECT attested
      FROM dal_slots_statuses
      WHERE block_hash = ? AND slot_index = ?
      |sql}

    let select_slot_statuses =
      (block_hash ->* t2 dal_slot_index dal_slot_status)
      @@ {sql|
      SELECT slot_index, attested
      FROM dal_slots_statuses
      WHERE block_hash = ?
      ORDER BY slot_index DESC
      |sql}
  end

  let store ?conn store block slot_index slot_status =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.insert (block, slot_index, slot_status)

  let find_slot_status ?conn store block ~slot_index =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.find_slot_status (block, slot_index)

  let list_slot_statuses ?conn store block =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.select_slot_statuses block
end

module L2_levels = struct
  module Q = struct
    open Types

    let insert =
      (t2 level block_hash ->. unit)
      @@ {sql|
      REPLACE INTO l2_levels
      (level, block_hash)
      VALUES (?, ?)
      |sql}

    let select =
      (level ->? block_hash)
      @@ {sql|
      SELECT block_hash
      FROM l2_levels
      WHERE level = ?
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM l2_levels WHERE level < ?
      |sql}
  end

  let store ?conn store level block =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.insert (level, block)

  let find ?conn store level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.select level

  let delete_before ?conn store ~level =
    with_connection store conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level
end
