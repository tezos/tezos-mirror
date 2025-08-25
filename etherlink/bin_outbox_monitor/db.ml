(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Sqlite.Request
open Caqti_type.Std

(** Current version for migrations. *)
let version = 0

module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

let quantity_hum_encoding =
  Data_encoding.conv
    (fun (Ethereum_types.Qty z) -> z)
    Ethereum_types.quantity_of_z
    Data_encoding.z

type withdrawal_kind =
  | Xtz
  | FA of Ethereum_types.Address.t
  | Fast_xtz
  | Fast_FA of Ethereum_types.Address.t

type withdrawal = {
  kind : withdrawal_kind;
  amount : Ethereum_types.quantity;
  sender : Ethereum_types.Address.t;
  receiver : Contract.t;
  withdrawal_id : Ethereum_types.quantity;
}

type withdrawal_log = {
  transactionHash : Ethereum_types.hash;
  transactionIndex : Ethereum_types.quantity;
  logIndex : Ethereum_types.quantity;
  blockHash : Ethereum_types.block_hash;
  blockNumber : Ethereum_types.quantity;
  removed : bool;
  withdrawal : withdrawal;
}

type l2_levels_range = {
  start_l2 : Ethereum_types.quantity;
  end_l2 : Ethereum_types.quantity;
}

type outbox_index = {
  outbox_level : int32;
  message_index : int;
  transaction_index : int;
}

type execution_short_info = {
  operation_hash : Operation_hash.t;
  l1_block : int32;
  timestamp : Time.Protocol.t;
}

let withdrawal_kind_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"XTZ"
        (Tag 0)
        (obj1 (req "kind" (constant "XTZ")))
        (function Xtz -> Some () | _ -> None)
        (fun () -> Xtz);
      case
        ~title:"FA"
        (Tag 1)
        (obj2
           (req "kind" (constant "FA"))
           (req "ticket_owner" Ethereum_types.address_encoding))
        (function FA tow -> Some ((), tow) | _ -> None)
        (fun ((), tow) -> FA tow);
      case
        ~title:"FAST_XTZ"
        (Tag 2)
        (obj1 (req "kind" (constant "FAST_XTZ")))
        (function Fast_xtz -> Some () | _ -> None)
        (fun () -> Fast_xtz);
      case
        ~title:"FAST_FA"
        (Tag 3)
        (obj2
           (req "kind" (constant "FAST_FA"))
           (req "ticket_owner" Ethereum_types.address_encoding))
        (function Fast_FA tow -> Some ((), tow) | _ -> None)
        (fun ((), tow) -> Fast_FA tow);
    ]

let pp_withdrawal_kind fmt k =
  match k with
  | Xtz -> Format.pp_print_string fmt "XTZ"
  | FA ticket_owner ->
      Format.fprintf
        fmt
        "FA(%s)"
        (Ethereum_types.Address.to_string ticket_owner)
  | Fast_xtz -> Format.pp_print_string fmt "Fast XTZ"
  | Fast_FA ticket_owner ->
      Format.fprintf
        fmt
        "Fast FA(%s)"
        (Ethereum_types.Address.to_string ticket_owner)

let withdrawal_encoding =
  let open Data_encoding in
  conv
    (fun {kind; amount; sender; receiver; withdrawal_id} ->
      (kind, (amount, sender, receiver, withdrawal_id)))
    (fun (kind, (amount, sender, receiver, withdrawal_id)) ->
      {kind; amount; sender; receiver; withdrawal_id})
  @@ merge_objs
       withdrawal_kind_encoding
       (obj4
          (req "amount" quantity_hum_encoding)
          (req "sender" Ethereum_types.address_encoding)
          (req "receiver" Contract.encoding)
          (req "withdrawal_id" quantity_hum_encoding))

let withdrawal_log_encoding =
  let open Data_encoding in
  let open Ethereum_types in
  conv
    (fun {
           transactionHash;
           transactionIndex;
           logIndex;
           blockHash;
           blockNumber;
           removed;
           withdrawal;
         }
       ->
      ( transactionHash,
        transactionIndex,
        logIndex,
        blockHash,
        blockNumber,
        removed,
        withdrawal ))
    (fun ( transactionHash,
           transactionIndex,
           logIndex,
           blockHash,
           blockNumber,
           removed,
           withdrawal )
       ->
      {
        transactionHash;
        transactionIndex;
        logIndex;
        blockHash;
        blockNumber;
        removed;
        withdrawal;
      })
  @@ obj7
       (req "transactionHash" hash_encoding)
       (req "transactionIndex" quantity_encoding)
       (req "logIndex" quantity_encoding)
       (req "blockHash" block_hash_encoding)
       (req "blockNumber" quantity_encoding)
       (req "removed" bool)
       (req "withdrawal" withdrawal_encoding)

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

let with_connection db conn =
  match conn with
  | Some conn -> Sqlite.with_connection conn
  | None -> fun k -> Sqlite.use db @@ fun conn -> Sqlite.with_connection conn k

let _with_transaction db k =
  Sqlite.use db @@ fun conn -> Sqlite.with_transaction conn k

module Types = struct
  let level =
    custom
      ~encode:(fun (Ethereum_types.Qty x) -> Ok Z.(to_int x))
      ~decode:(fun x -> Ok (Qty Z.(of_int x)))
      int

  let amount =
    custom
      ~encode:(fun (Ethereum_types.Qty x) -> Ok Z.(to_string x))
      ~decode:(fun x -> Ok (Qty Z.(of_string x)))
      string

  let address =
    custom
      ~encode:(fun (Ethereum_types.Address (Hex address)) ->
        Result.of_option ~error:"not a valid address"
        @@ Hex.to_string (`Hex address))
      ~decode:(fun address ->
        let (`Hex address) = Hex.of_string address in
        Ok (Address (Hex address)))
      octets

  let hash =
    custom
      ~encode:(fun (Ethereum_types.Hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Hash (Hex hash)))
      octets

  let block_hash =
    custom
      ~encode:(fun (Ethereum_types.Block_hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Block_hash (Hex hash)))
      octets

  let withdrawal_kind =
    custom
      ~encode:(function
        | Xtz -> Ok (0, None)
        | FA ticket_owner -> Ok (1, Some ticket_owner)
        | Fast_xtz -> Ok (2, None)
        | Fast_FA ticket_owner -> Ok (3, Some ticket_owner))
      ~decode:(function
        | 0, None -> Ok Xtz
        | 1, Some ticket_owner -> Ok (FA ticket_owner)
        | 2, None -> Ok Fast_xtz
        | 3, Some ticket_owner -> Ok (Fast_FA ticket_owner)
        | _ -> Error "not a valid withdrawal kind")
      (t2 int (option address))

  let contract =
    custom
      ~encode:(fun c -> Ok (Contract.to_b58check c))
      ~decode:(fun s ->
        Contract.of_b58check s
        |> Result.map_error (fun _e -> "invalid contract in db"))
      string

  let operation_hash =
    custom
      ~encode:(fun c -> Ok (Operation_hash.to_b58check c))
      ~decode:(fun s ->
        Operation_hash.of_b58check s
        |> Result.map_error (fun _e -> "invalid operation hash in db"))
      string

  let proto_timestamp =
    custom
      ~encode:(fun t -> Ok (Time.Protocol.to_seconds t))
      ~decode:(fun s -> Ok (Time.Protocol.of_seconds s))
      int64

  let withdrawal =
    product (fun kind amount sender receiver withdrawal_id ->
        {kind; amount; sender; receiver; withdrawal_id})
    @@ proj withdrawal_kind (fun w -> w.kind)
    @@ proj amount (fun w -> w.amount)
    @@ proj address (fun w -> w.sender)
    @@ proj contract (fun w -> w.receiver)
    @@ proj level (fun w -> w.withdrawal_id)
    @@ proj_end

  let withdrawal_log =
    product
      (fun
        transactionHash
        transactionIndex
        logIndex
        blockHash
        blockNumber
        removed
        withdrawal
      ->
        {
          transactionHash;
          transactionIndex;
          logIndex;
          blockHash;
          blockNumber;
          removed;
          withdrawal;
        })
    @@ proj hash (fun l -> l.transactionHash)
    @@ proj level (fun l -> l.transactionIndex)
    @@ proj level (fun l -> l.logIndex)
    @@ proj block_hash (fun l -> l.blockHash)
    @@ proj level (fun l -> l.blockNumber)
    @@ proj bool (fun l -> l.removed)
    @@ proj withdrawal (fun l -> l.withdrawal)
    @@ proj_end

  let l2_levels_range =
    product (fun start_l2 end_l2 -> {start_l2; end_l2})
    @@ proj level (fun l -> l.start_l2)
    @@ proj level (fun l -> l.end_l2)
    @@ proj_end

  let micheline : string Tezos_micheline.Micheline.canonical Caqti_type.t =
    custom
      ~encode:(fun expr ->
        let open Tezos_micheline.Micheline_printer in
        Format.asprintf "%a" print_expr_unwrapped (printable Fun.id expr)
        |> Result.ok)
      ~decode:(fun str ->
        let open Tezos_micheline.Micheline_parser in
        let open Result_syntax in
        let node =
          let* tokens = tokenize str |> no_parsing_error in
          parse_expression ~check:false tokens |> no_parsing_error
        in
        let+ node =
          node |> Result.map_error (Format.asprintf "%a" pp_print_trace)
        in
        Tezos_micheline.Micheline.strip_locations node)
      string

  let outbox_transaction =
    let open Octez_smart_rollup.Outbox_message in
    product (fun destination entrypoint parameters ->
        {destination; entrypoint; parameters; parameters_ty = None})
    @@ proj string (fun o -> o.destination)
    @@ proj string (fun o -> o.entrypoint)
    @@ proj micheline (fun o -> o.parameters)
    @@ proj_end

  let outbox_index =
    product (fun outbox_level message_index transaction_index ->
        {outbox_level; message_index; transaction_index})
    @@ proj int32 (fun o -> o.outbox_level)
    @@ proj int (fun o -> o.message_index)
    @@ proj int (fun o -> o.transaction_index)
    @@ proj_end

  let execution_short_info =
    product (fun operation_hash l1_block timestamp ->
        {operation_hash; l1_block; timestamp})
    @@ proj operation_hash (fun e -> e.operation_hash)
    @@ proj int32 (fun e -> e.l1_block)
    @@ proj proto_timestamp (fun e -> e.timestamp)
    @@ proj_end
end

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

module Withdrawals = struct
  module Q = struct
    open Types

    let insert =
      (withdrawal_log ->. unit)
      @@ {sql|
      INSERT INTO withdrawals
      (transactionHash,
       transactionIndex, logIndex, blockHash, blockNumber,
       removed, kind, ticket_owner, amount, sender, receiver, withdrawal_id)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
      ON CONFLICT (transactionHash, transactionIndex, logIndex) DO UPDATE SET
        blockHash = $4,
        blockNumber = $5,
        removed = $6,
        kind = $7,
        ticket_owner = $8,
        amount = $9,
        sender = $10,
        receiver = $11,
        withdrawal_id = $12
      |sql}

    let find_by_blockNumbers =
      (t2 level level ->* withdrawal_log)
      @@ {sql|
      SELECT
       transactionHash,
       transactionIndex, logIndex, blockHash, blockNumber,
       removed, kind, ticket_owner, amount, sender, receiver,
       CAST(withdrawal_id as integer)
      FROM withdrawals
      WHERE blockNumber BETWEEN ? AND ?
      ORDER BY transactionIndex DESC, logIndex DESC
      |sql}

    let set_outbox_index =
      (t4 outbox_index hash level level ->. unit)
      @@ {sql|
      UPDATE withdrawals
      SET
        outbox_level = ?,
        outbox_message_index = ?,
        outbox_transaction_index = ?
      WHERE transactionHash = ?
      AND transactionIndex = ?
      AND logIndex = ?
      |sql}

    let get_overdue =
      (int ->* t2 withdrawal_log int32)
      @@ {sql|
      SELECT
        w.transactionHash,
        w.transactionIndex, w.logIndex, w.blockHash, w.blockNumber,
        w.removed, w.kind, w.ticket_owner, w.amount, w.sender, w.receiver,
        CAST(w.withdrawal_id as integer),
        l.l1
      FROM withdrawals w
      JOIN levels l ON
        w.blockNumber > l.start_l2 AND
        w.blockNumber <= l.end_l2
      LEFT OUTER JOIN outbox_messages o ON
        w.outbox_level = o.outbox_level AND
        w.outbox_message_index = o.message_index AND
        w.outbox_transaction_index = o.transaction_index
      LEFT OUTER JOIN executions e ON
        e.outbox_level = w.outbox_level AND
        e.message_index = w.outbox_message_index
      JOIN pointers lcc ON
        lcc.name = "lcc"
      WHERE
        l.l1 <= lcc.value - ? AND
        e.outbox_level IS NULL
      |sql}
  end

  let store ?conn db log =
    with_connection db conn @@ fun conn -> Sqlite.Db.exec conn Q.insert log

  let list_by_block_numbers ?conn db ~min_level ~max_level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.find_by_blockNumbers (min_level, max_level)

  let set_outbox_index ?conn db ~transactionHash ~transactionIndex ~logIndex
      outbox_index =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec
      conn
      Q.set_outbox_index
      (outbox_index, transactionHash, transactionIndex, logIndex)

  let get_overdue ?conn db ~challenge_window =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.get_overdue challenge_window
end

module Levels = struct
  module Q = struct
    open Types

    let set =
      (t2 int32 l2_levels_range ->. unit)
      @@ {sql|REPLACE INTO levels (l1, start_l2, end_l2) VALUES (?, ?, ?)|sql}

    let get_l2_range =
      (int32 ->? l2_levels_range)
      @@ {sql|SELECT start_l2, end_l2 FROM levels WHERE l1 = ?|sql}

    let get_l1 =
      (level ->? int32)
      @@ {sql|SELECT l1 FROM levels WHERE $1 > start_l2 AND $1 <= end_l2|sql}

    let last =
      (unit ->? t2 int32 l2_levels_range)
      @@ {sql|SELECT l1, start_l2, end_l2 FROM levels ORDER BY l1 DESC LIMIT 1|sql}

    let levels_to_match =
      (unit ->* t2 int32 l2_levels_range)
      @@ {sql|
      SELECT l1, start_l2, end_l2 FROM levels
      JOIN pointers px ON l1 > px.value
      JOIN pointers py ON end_l2 <= py.value
      WHERE px.name = "last_matched_l1_level"
      AND py.name = "l2_head"
      ORDER BY l1 DESC
      |sql}
  end

  let store ?conn db ~l1_level ~start_l2_level ~end_l2_level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec
      conn
      Q.set
      (l1_level, {start_l2 = start_l2_level; end_l2 = end_l2_level})

  let get_l2_range ?conn db ~l1_level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.get_l2_range l1_level

  let get_l1 ?conn db ~l2_level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.find_opt conn Q.get_l1 l2_level

  let last ?conn db =
    with_connection db conn @@ fun conn -> Sqlite.Db.find_opt conn Q.last ()

  let levels_to_match ?conn db =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.levels_to_match ()
end

module Pointers = struct
  module type S = sig
    type value

    val set : ?conn:Sqlite.conn -> t -> value -> unit tzresult Lwt.t

    val find : ?conn:Sqlite.conn -> t -> value option tzresult Lwt.t

    val get : ?conn:Sqlite.conn -> t -> value tzresult Lwt.t
  end

  module Make (N : sig
    val name : string

    type value

    val value : value Caqti_type.t
  end) : S with type value := N.value = struct
    module Q = struct
      let set =
        (N.value ->. unit)
        @@ Format.sprintf
             {sql|REPLACE INTO pointers (name, value) VALUES (%S, ?)|sql}
             N.name

      let find =
        (unit ->? N.value)
        @@ Format.sprintf
             {sql|SELECT value from pointers WHERE name = %S|sql}
             N.name

      let get =
        (unit ->! N.value)
        @@ Format.sprintf
             {sql|SELECT value from pointers WHERE name = %S|sql}
             N.name
    end

    let set ?conn db value =
      with_connection db conn @@ fun conn -> Sqlite.Db.exec conn Q.set value

    let find ?conn db =
      with_connection db conn @@ fun conn -> Sqlite.Db.find_opt conn Q.find ()

    let get ?conn db =
      with_connection db conn @@ fun conn -> Sqlite.Db.find conn Q.get ()
  end

  module Finalized_L1_head = Make (struct
    let name = "finalized_l1_head"

    type value = Ethereum_types.quantity

    let value = Types.level
  end)

  module L2_head = Make (struct
    let name = "l2_head"

    type value = Ethereum_types.quantity

    let value = Types.level
  end)

  module LCC = Make (struct
    let name = "lcc"

    type value = int32

    let value = Caqti_type.int32
  end)

  module Last_matched_L1_level = Make (struct
    let name = "last_matched_l1_level"

    type value = int32

    let value = Caqti_type.int32
  end)
end

module Outbox_messages = struct
  module Q = struct
    open Types

    let insert =
      (t2 outbox_index outbox_transaction ->. unit)
      @@ {sql|
      INSERT INTO outbox_messages
      (outbox_level, message_index, transaction_index,
       destination, entrypoint, parameters)
      VALUES ($1, $2, $3, $4, $5, $6)
      ON CONFLICT (outbox_level, message_index, transaction_index) DO UPDATE SET
        destination = $4,
        entrypoint = $5,
        parameters = $6
      |sql}

    let indexes_by_outbox_level =
      (int32 ->* outbox_index)
      @@ {sql|
      SELECT outbox_level, message_index, transaction_index
      FROM outbox_messages
      WHERE outbox_level = ?
      ORDER BY message_index DESC, transaction_index DESC
      |sql}
  end

  let store ?conn db ~outbox_level ~message_index ~transaction_index tx =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec
      conn
      Q.insert
      ({outbox_level; message_index; transaction_index}, tx)

  let indexes_by_outbox_level ?conn db ~outbox_level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.indexes_by_outbox_level outbox_level
end

module Executions = struct
  module Q = struct
    open Types

    let insert =
      (t5 int32 int operation_hash int32 proto_timestamp ->. unit)
      @@ {sql|
      REPLACE INTO executions
      (outbox_level, message_index, operation_hash,
       l1_block, timestamp)
      VALUES (?, ?, ?, ?, ?)
      |sql}

    let withdrawals_of_level =
      (int32 ->* t3 withdrawal_log outbox_index execution_short_info)
      @@ {sql|
      SELECT
        w.transactionHash,
        w.transactionIndex, w.logIndex, w.blockHash, w.blockNumber,
        w.removed, w.kind, w.ticket_owner, w.amount, w.sender, w.receiver,
        CAST(w.withdrawal_id as integer),
        o.outbox_level, o.message_index, o.transaction_index,
        e.operation_hash, e.l1_block, e.timestamp
      FROM withdrawals w
      JOIN outbox_messages o ON
        w.outbox_level = o.outbox_level AND
        w.outbox_message_index = o.message_index AND
        w.outbox_transaction_index = o.transaction_index
      JOIN executions e ON
        e.outbox_level = w.outbox_level AND
        e.message_index = w.outbox_message_index
      WHERE
        w.outbox_level IS NOT NULL AND
        w.removed = false AND
        e.l1_block = ?
      |sql}
  end

  let store ?conn db ~outbox_level ~message_index ~operation_hash ~l1_block
      ~timestamp =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec
      conn
      Q.insert
      (outbox_level, message_index, operation_hash, l1_block, timestamp)

  let withdrawals_executed_in_l1_block ?conn db ~l1_block =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.withdrawals_of_level l1_block
end
