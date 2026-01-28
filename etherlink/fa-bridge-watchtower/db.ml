(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025-2026 Functori, <contact@functori.com>                  *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Sqlite.Request
open Caqti_type.Std

(** Current version for migrations. *)
let version = 2

module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

let quantity_hum_encoding =
  Data_encoding.conv
    (fun (Ethereum_types.Qty z) -> z)
    Ethereum_types.quantity_of_z
    Data_encoding.z

type token_info = {
  proxy : Ethereum_types.Address.t;
  ticket_hash : Ethereum_types.hash;
}

type token = FA of token_info | XTZ

type deposit = {
  nonce : Ethereum_types.quantity;
  token : token;
  receiver : Ethereum_types.Address.t;
  amount : Ethereum_types.quantity;
}

type log_info = {
  transactionHash : Ethereum_types.hash;
  transactionIndex : Ethereum_types.quantity;
  logIndex : Ethereum_types.quantity;
  blockHash : Ethereum_types.block_hash;
  blockNumber : Ethereum_types.quantity;
  removed : bool;
}

type execution_info = {
  transactionHash : Ethereum_types.hash;
  transactionIndex : Ethereum_types.quantity;
  blockHash : Ethereum_types.block_hash;
  blockNumber : Ethereum_types.quantity;
}

type deposit_log = {
  deposit : deposit;
  log_info : log_info;
  claimed : execution_info option;
}

module Events = struct
  include Internal_event.Simple

  let section = ["fa_bridge_watchtower"; "db"]

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

let with_transaction db k =
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

  let address_option_blob =
    custom
      ~encode:(function
        | None -> Ok ""
        | Some (Ethereum_types.Address (Hex address)) ->
            Result.of_option ~error:"not a valid address"
            @@ Hex.to_string (`Hex address))
      ~decode:(function
        | "" -> Ok None
        | address ->
            let (`Hex address) = Hex.of_string address in
            Ok (Some (Address (Hex address))))
      octets

  let hash =
    custom
      ~encode:(fun (Ethereum_types.Hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Hash (Hex hash)))
      octets

  let hash_option_blob =
    custom
      ~encode:(function
        | None -> Ok ""
        | Some (Ethereum_types.Hash (Hex hash)) ->
            Result.of_option ~error:"not a valid hash"
            @@ Hex.to_string (`Hex hash))
      ~decode:(function
        | "" -> Ok None
        | hash ->
            let (`Hex hash) = Hex.of_string hash in
            Ok (Some (Hash (Hex hash))))
      octets

  let block_hash =
    custom
      ~encode:(fun (Ethereum_types.Block_hash (Hex hash)) ->
        Result.of_option ~error:"not a valid hash" @@ Hex.to_string (`Hex hash))
      ~decode:(fun hash ->
        let (`Hex hash) = Hex.of_string hash in
        Ok (Block_hash (Hex hash)))
      octets

  let native_deposit =
    custom ~encode:(fun b -> Ok b) ~decode:(fun b -> Ok b) bool

  let deposit =
    product (fun nonce proxy ticket_hash receiver amount native_deposit ->
        if native_deposit then {nonce; token = XTZ; receiver; amount}
        else
          let proxy =
            match proxy with
            | Some proxy -> proxy
            | None -> Stdlib.failwith "FA deposit is missing the proxy"
          in
          let ticket_hash =
            match ticket_hash with
            | Some ticket_hash -> ticket_hash
            | None -> Stdlib.failwith "FA deposit is missing the ticket_hash"
          in
          {nonce; token = FA {proxy; ticket_hash}; receiver; amount})
    @@ proj level (fun d -> d.nonce)
    @@ proj address_option_blob (fun d ->
           match d.token with XTZ -> None | FA {proxy; _} -> Some proxy)
    @@ proj hash_option_blob (fun d ->
           match d.token with
           | XTZ -> None
           | FA {ticket_hash; _} -> Some ticket_hash)
    @@ proj address (fun d -> d.receiver)
    @@ proj amount (fun d -> d.amount)
    @@ proj native_deposit (fun d ->
           match d.token with XTZ -> true | FA _ -> false)
    @@ proj_end

  let log_info =
    product
      (fun
        transactionHash
        transactionIndex
        logIndex
        blockHash
        blockNumber
        removed
      ->
        {
          transactionHash;
          transactionIndex;
          logIndex;
          blockHash;
          blockNumber;
          removed;
        })
    @@ proj hash (fun (l : log_info) -> l.transactionHash)
    @@ proj level (fun (l : log_info) -> l.transactionIndex)
    @@ proj level (fun (l : log_info) -> l.logIndex)
    @@ proj block_hash (fun (l : log_info) -> l.blockHash)
    @@ proj level (fun (l : log_info) -> l.blockNumber)
    @@ proj bool (fun (l : log_info) -> l.removed)
    @@ proj_end

  let execution_info =
    product (fun transactionHash transactionIndex blockHash blockNumber ->
        {transactionHash; transactionIndex; blockHash; blockNumber})
    @@ proj hash (fun (l : execution_info) -> l.transactionHash)
    @@ proj level (fun (l : execution_info) -> l.transactionIndex)
    @@ proj block_hash (fun (l : execution_info) -> l.blockHash)
    @@ proj level (fun (l : execution_info) -> l.blockNumber)
    @@ proj_end

  let deposit_log =
    product (fun deposit log_info claimed -> {deposit; log_info; claimed})
    @@ proj deposit (fun d -> d.deposit)
    @@ proj log_info (fun d -> d.log_info)
    @@ proj (option execution_info) (fun d -> d.claimed)
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

    let all : Sql_migrations.migration list = Sql_migrations.migrations version
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
            "Cannot use a database at migration %d, the fa bridge watchtower \
             only supports up to %d"
            applied
            known
    | None -> return all_migrations

  let apply_migration store id (module M : Sql_migrations.S) =
    let open Lwt_result_syntax in
    Sqlite.with_connection store @@ fun conn ->
    let* () = List.iter_es (fun up -> Sqlite.Db.exec conn up ()) M.apply in
    Sqlite.Db.exec conn Q.register_migration (id, M.name)
end

type t = Sqlite.t

let sqlite_file_name = "fa-bridge-watchtower.sqlite"

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
        (fun (i, ((module M : Sql_migrations.S) as mig)) ->
          let* () = Migrations.apply_migration conn i mig in
          let*! () = Events.(emit applied_migration) M.name in
          return_unit)
        migrations
    in
    return_unit
  in
  Sqlite.init ~path ~perm migration

module Deposits = struct
  module Q = struct
    open Types

    let insert =
      (t2 deposit log_info ->. unit)
      @@ {sql|
      REPLACE INTO deposits
      (nonce, proxy, ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
      |sql}

    let unclaimed_ignore_whitelist =
      (unit ->* deposit)
      @@ {sql|
      SELECT
       nonce, proxy, ticket_hash, receiver, amount, native_deposit
      FROM deposits
      where exec_transactionHash IS NULL
      ORDER BY nonce DESC
      |sql}

    let unclaimed =
      (unit ->* deposit)
      @@ {sql|
      SELECT
       d.nonce, d.proxy, d.ticket_hash, d.receiver, d.amount, d.native_deposit
      FROM deposits d
      JOIN whitelist w
      ON (
       (d.proxy = w.proxy or w.proxy IS NULL)
       AND
       (d.ticket_hash = w.ticket_hash or w.ticket_hash IS NULL))
      where d.exec_transactionHash IS NULL
      ORDER BY nonce DESC
      |sql}

    let unclaimed_full_ignore_whitelist =
      (unit ->* t2 deposit log_info)
      @@ {sql|
      SELECT
       nonce, proxy, ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed
      FROM deposits
      where exec_transactionHash IS NULL
      ORDER BY nonce DESC
      |sql}

    let unclaimed_full =
      (unit ->* t2 deposit log_info)
      @@ {sql|
      SELECT
       nonce, d.proxy, d.ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed
      FROM deposits d
      JOIN whitelist w
      ON (
       (d.proxy = w.proxy or w.proxy IS NULL)
       AND
       (d.ticket_hash = w.ticket_hash or w.ticket_hash IS NULL))
      where exec_transactionHash IS NULL
      ORDER BY nonce DESC
      |sql}

    let list_ignore_whitelist =
      (t2 int int ->* deposit_log)
      @@ {sql|
      SELECT
       nonce, proxy, ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed,
       exec_transactionHash, exec_transactionIndex, exec_blockHash,
       exec_blockNumber
      FROM deposits
      ORDER BY nonce DESC LIMIT ? OFFSET ?
      |sql}

    let list =
      (t2 int int ->* deposit_log)
      @@ {sql|
      SELECT
       nonce, d.proxy, d.ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed,
       exec_transactionHash, exec_transactionIndex, exec_blockHash,
       exec_blockNumber
      FROM deposits d
      JOIN whitelist w
      ON (
       (d.proxy = w.proxy or w.proxy IS NULL)
       AND
       (d.ticket_hash = w.ticket_hash or w.ticket_hash IS NULL))
      ORDER BY nonce DESC LIMIT ? OFFSET ?
      |sql}

    let list_by_receiver_ignore_whitelist =
      (t3 address int int ->* deposit_log)
      @@ {sql|
      SELECT
       nonce, proxy, ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed,
       exec_transactionHash, exec_transactionIndex, exec_blockHash,
       exec_blockNumber
      FROM deposits
      WHERE receiver = ?
      ORDER BY nonce DESC LIMIT ? OFFSET ?
      |sql}

    let list_by_receiver =
      (t3 address int int ->* deposit_log)
      @@ {sql|
      SELECT
       nonce, d.proxy, d.ticket_hash, receiver, amount, native_deposit,
       log_transactionHash, log_transactionIndex, log_logIndex, log_blockHash,
       log_blockNumber, log_removed,
       exec_transactionHash, exec_transactionIndex, exec_blockHash,
       exec_blockNumber
      FROM deposits d
      JOIN whitelist w
      ON (
       (d.proxy = w.proxy or w.proxy IS NULL)
       AND
       (d.ticket_hash = w.ticket_hash or w.ticket_hash IS NULL))
      WHERE receiver = ?
      ORDER BY nonce DESC LIMIT ? OFFSET ?
      |sql}

    let set_claimed =
      (t2 execution_info level ->. unit)
      @@ {sql|
      UPDATE deposits
      SET
        exec_transactionHash = ?,
        exec_transactionIndex = ?,
        exec_blockHash = ?,
        exec_blockNumber = ?
      WHERE
        nonce = ?
      |sql}

    let delete_before =
      (level ->. unit)
      @@ {sql|
      DELETE FROM deposits
      WHERE log_blockNumber < ?
      |sql}

    let delete_after =
      (level ->. unit)
      @@ {sql|
      DELETE FROM deposits
      WHERE log_blockNumber > ?
      |sql}
  end

  let store ?conn db deposit log_info =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec conn Q.insert (deposit, log_info)

  let get_unclaimed ?conn db =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.unclaimed ()

  let get_unclaimed_full ?conn db =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.unclaimed_full ()

  let list ?conn db ~limit ~offset =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.list (limit, offset)

  let list_by_receiver ?conn db addr ~limit ~offset =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.list_by_receiver (addr, limit, offset)

  let get_unclaimed_ignore_whitelist ?conn db =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.unclaimed_ignore_whitelist ()

  let get_unclaimed_full_ignore_whitelist ?conn db =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.rev_collect_list conn Q.unclaimed_full_ignore_whitelist ()

  let list_ignore_whitelist ?conn db ~limit ~offset =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list conn Q.list_ignore_whitelist (limit, offset)

  let list_by_receiver_ignore_whitelist ?conn db addr ~limit ~offset =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.collect_list
      conn
      Q.list_by_receiver_ignore_whitelist
      (addr, limit, offset)

  let set_claimed ?conn db nonce execution_info =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec conn Q.set_claimed (execution_info, nonce)

  let delete_before ?conn db level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_before level

  let delete_after ?conn db level =
    with_connection db conn @@ fun conn ->
    Sqlite.Db.exec conn Q.delete_after level
end

module Pointers = struct
  module L2_head = struct
    module Q = struct
      open Types

      let set =
        (level ->. unit)
        @@ {sql|REPLACE INTO pointers (name, value) VALUES ("l2_head", ?)|sql}

      let find =
        (unit ->? level)
        @@ {sql|SELECT value from pointers WHERE name = "l2_head"|sql}

      let get =
        (unit ->! level)
        @@ {sql|SELECT value from pointers WHERE name = "l2_head"|sql}
    end

    let set ?conn db value =
      with_connection db conn @@ fun conn -> Sqlite.Db.exec conn Q.set value

    let find ?conn db =
      with_connection db conn @@ fun conn -> Sqlite.Db.find_opt conn Q.find ()

    let get ?conn db =
      with_connection db conn @@ fun conn -> Sqlite.Db.find conn Q.get ()
  end
end

module Whitelist = struct
  module Q = struct
    open Types

    let insert =
      (t2 (option address) (option hash) ->. unit)
      @@ {sql|
      INSERT INTO whitelist
      (proxy, ticket_hash)
      VALUES (?, ?)
      |sql}

    let clear = (unit ->. unit) @@ {sql| DELETE FROM whitelist |sql}
  end

  let insert ?conn db pth =
    with_connection db conn @@ fun conn -> Sqlite.Db.exec conn Q.insert pth

  let clear ?conn db =
    with_connection db conn @@ fun conn -> Sqlite.Db.exec conn Q.clear ()

  let register db (whitelist : Config.whitelist_item list option) =
    let open Lwt_result_syntax in
    with_transaction db @@ fun conn ->
    let* () = clear ~conn db in
    match whitelist with
    | None ->
        (* Matches every deposit log *)
        insert ~conn db (None, None)
    | Some whitelist ->
        List.iter_es
          (fun {Config.proxy; ticket_hashes} ->
            match ticket_hashes with
            | None -> insert ~conn db (proxy, None)
            | Some ticket_hashes ->
                List.iter_es
                  (fun ticket_hash -> insert ~conn db (proxy, Some ticket_hash))
                  ticket_hashes)
          whitelist
end
