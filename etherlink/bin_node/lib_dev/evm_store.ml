(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix
include Sqlite

module Legacy_encodings = struct
  open Ethereum_types

  let block_encoding =
    let open Data_encoding in
    conv
      (fun {
             number;
             hash;
             parent;
             nonce;
             sha3Uncles;
             logsBloom;
             transactionRoot;
             stateRoot;
             receiptRoot;
             miner;
             difficulty;
             totalDifficulty;
             extraData;
             size;
             gasLimit;
             gasUsed;
             timestamp;
             transactions;
             uncles;
             baseFeePerGas;
             prevRandao;
             withdrawals = _;
             withdrawalsRoot = _;
             blobGasUsed = _;
             excessBlobGas = _;
             parentBeaconBlockRoot = _;
           }
         ->
        ( ( ( number,
              hash,
              parent,
              nonce,
              sha3Uncles,
              logsBloom,
              transactionRoot,
              stateRoot,
              receiptRoot,
              miner ),
            ( difficulty,
              totalDifficulty,
              extraData,
              size,
              gasLimit,
              gasUsed,
              timestamp,
              transactions,
              uncles,
              baseFeePerGas ) ),
          prevRandao ))
      (fun ( ( ( number,
                 hash,
                 parent,
                 nonce,
                 sha3Uncles,
                 logsBloom,
                 transactionRoot,
                 stateRoot,
                 receiptRoot,
                 miner ),
               ( difficulty,
                 totalDifficulty,
                 extraData,
                 size,
                 gasLimit,
                 gasUsed,
                 timestamp,
                 transactions,
                 uncles,
                 baseFeePerGas ) ),
             prevRandao )
         ->
        {
          number;
          hash;
          parent;
          nonce;
          sha3Uncles;
          logsBloom;
          transactionRoot;
          stateRoot;
          receiptRoot;
          miner;
          difficulty;
          totalDifficulty;
          extraData;
          size;
          gasLimit;
          gasUsed;
          baseFeePerGas;
          timestamp;
          transactions;
          uncles;
          prevRandao;
          withdrawals = None;
          withdrawalsRoot = None;
          blobGasUsed = None;
          excessBlobGas = None;
          parentBeaconBlockRoot = None;
        })
      (merge_objs
         (merge_objs
            (obj10
               (req "number" quantity_encoding)
               (req "hash" block_hash_encoding)
               (req "parentHash" block_hash_encoding)
               (req "nonce" hex_encoding)
               (req "sha3Uncles" hash_encoding)
               (req "logsBloom" hex_encoding)
               (req "transactionsRoot" hash_encoding)
               (req "stateRoot" hash_encoding)
               (req "receiptsRoot" hash_encoding)
               (req "miner" hex_encoding))
            (obj10
               (req "difficulty" quantity_encoding)
               (req "totalDifficulty" quantity_encoding)
               (req "extraData" hex_encoding)
               (req "size" quantity_encoding)
               (req "gasLimit" quantity_encoding)
               (req "gasUsed" quantity_encoding)
               (req "timestamp" quantity_encoding)
               (req
                  "transactions"
                  (block_transactions_encoding
                     legacy_transaction_object_encoding))
               (req "uncles" (list hash_encoding))
               (opt "baseFeePerGas" quantity_encoding)))
         (obj1
            (* [mixHash] has been replaced by [prevRandao] internally in the
               Paris EVM version, but every public RPC endpoints we have been
               testing keep using [mixHash] in their JSON encoding (probably for
               backward compatibility). *)
            (opt "mixHash" block_hash_encoding)))
end

type levels = {l1_level : int32; current_number : Ethereum_types.quantity}

type finalized_levels = {
  start_l2_level : Ethereum_types.quantity;
  end_l2_level : Ethereum_types.quantity;
}

type pending_kernel_upgrade = {
  kernel_upgrade : Evm_events.Upgrade.t;
  injected_before : Ethereum_types.quantity;
}

type pending_sequencer_upgrade = {
  sequencer_upgrade : Evm_events.Sequencer_upgrade.t;
  injected_before : Ethereum_types.quantity;
}

type metadata = {
  smart_rollup_address : Address.t;
  history_mode : Configuration.history_mode;
}

module Compact_transactions_receipt = struct
  open Ethereum_types

  let strip_log_context (log : transaction_log) =
    {
      log with
      blockNumber = None;
      transactionHash = None;
      transactionIndex = None;
      blockHash = None;
    }

  let fill_log_context ~hash ~block_hash ~block_number ~index
      (log : transaction_log) =
    {
      log with
      transactionHash = Some hash;
      blockHash = Some block_hash;
      blockNumber = Some block_number;
      transactionIndex = Some index;
    }

  (* Compact encoding for receipt fields: strips per-log context fields
   (blockNumber, transactionHash, transactionIndex, blockHash) that are
   derivable from the outer transaction row. They are stored as [None] and
   reconstructed at read time.

   The decoder is identical to [receipt_fields_encoding], so both legacy rows
   (fields stored as [Some ...]) and compact rows (fields stored as [None])
   decode transparently. *)
  let encoding : Transaction_info.receipt_fields Data_encoding.t =
    Data_encoding.conv
      (fun rf ->
        Transaction_info.{rf with logs = List.map strip_log_context rf.logs})
      Fun.id
      Transaction_info.receipt_fields_encoding

  (* Legacy encoding for receipt fields: keeps per-log context fields and
     uses the legacy hex bloom encoding. Used when
     [experimental_features.compact_receipt_encoding = false] so that rows
     remain readable by older node releases that predate the compact format. *)
  let legacy_encoding : Transaction_info.receipt_fields Data_encoding.t =
    Transaction_info.legacy_receipt_fields_encoding
end

module Q = struct
  open Sqlite.Request
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

  (* WARNING: uses [octets] — SELECT queries must use
     [CAST(payload AS BLOB)] to support old TEXT rows. *)
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
      octets

  let context_hash =
    custom
      ~encode:(fun hash ->
        Ok
          (hash |> Smart_rollup_context_hash.to_context_hash
         |> Context_hash.to_b58check))
      ~decode:(fun bytes ->
        let open Result_syntax in
        let+ hash =
          Option.to_result ~none:"Not a valid b58check encoded hash"
          @@ Context_hash.of_b58check_opt bytes
        in
        hash |> Smart_rollup_context_hash.of_context_hash)
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

  let public_key =
    custom
      ~encode:(fun public_key ->
        Ok (Signature.Public_key.to_b58check public_key))
      ~decode:(fun b58 ->
        let public_key = Signature.Public_key.of_b58check_exn b58 in
        Ok public_key)
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

  (* WARNING: uses [octets] — SELECT queries must use
     [CAST(block AS BLOB)] to support old TEXT rows. *)
  let block =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Ethereum_types.(block_encoding legacy_transaction_object_encoding)
             payload))
      ~decode:(fun bytes ->
        Result.map_error
          (Format.asprintf
             "Not a valid block payload: %a"
             Data_encoding.Binary.pp_read_error)
          (* The block encoding in Ethereum_types was modified in a patch
             without taking into account backward compatibility.

             As a consequence, it is possible for a block to be serialized
             with the previous encoding. We fallback to this legacy encoding
             just in case. *)
          (Result.bind_error
             (Data_encoding.Binary.of_string
                Ethereum_types.(
                  block_encoding legacy_transaction_object_encoding)
                bytes)
             (fun _ ->
               Data_encoding.Binary.of_string
                 Legacy_encodings.block_encoding
                 bytes)))
      octets

  (* WARNING: uses [octets] — SELECT queries must use
     [CAST(tez_block AS BLOB)] to support old TEXT rows. *)
  let tezos_block =
    custom
      ~encode:L2_types.Tezos_block.encode_block_for_store
      ~decode:L2_types.Tezos_block.decode_block_for_store
      octets

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

  let sequencer_upgrade =
    custom
      ~encode:(fun
          Evm_events.Sequencer_upgrade.{sequencer; pool_address; timestamp} ->
        Ok (sequencer, pool_address, timestamp))
      ~decode:(fun (sequencer, pool_address, timestamp) ->
        Ok Evm_events.Sequencer_upgrade.{sequencer; pool_address; timestamp})
      (t3 public_key address timestamp)

  (* WARNING: uses [octets] — SELECT queries must use
     [CAST(payload AS BLOB)] to support old TEXT rows. *)
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
      octets

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
      ~encode:(fun {current_number; l1_level} -> Ok (current_number, l1_level))
      ~decode:(fun (current_number, l1_level) -> Ok {current_number; l1_level})
      (t2 level l1_level)

  let finalized_levels =
    custom
      ~encode:(fun {start_l2_level; end_l2_level} ->
        Ok (start_l2_level, end_l2_level))
      ~decode:(fun (start_l2_level, end_l2_level) ->
        Ok {start_l2_level; end_l2_level})
      (t2 level level)

  let pending_kernel_upgrade =
    product (fun injected_before hash timestamp ->
        {injected_before; kernel_upgrade = Evm_events.Upgrade.{hash; timestamp}})
    @@ proj level (fun (k : pending_kernel_upgrade) -> k.injected_before)
    @@ proj root_hash (fun k -> k.kernel_upgrade.hash)
    @@ proj timestamp (fun k -> k.kernel_upgrade.timestamp)
    @@ proj_end

  let pending_sequencer_upgrade =
    product (fun injected_before sequencer pool_address timestamp ->
        {
          injected_before;
          sequencer_upgrade =
            Evm_events.Sequencer_upgrade.{sequencer; pool_address; timestamp};
        })
    @@ proj level (fun (k : pending_sequencer_upgrade) -> k.injected_before)
    @@ proj public_key (fun k -> k.sequencer_upgrade.sequencer)
    @@ proj address (fun k -> k.sequencer_upgrade.pool_address)
    @@ proj timestamp (fun k -> k.sequencer_upgrade.timestamp)
    @@ proj_end

  module Blueprints = struct
    let table = "blueprints" (* For opentelemetry *)

    let insert =
      (t3 level timestamp payload ->. unit) ~name:__FUNCTION__ ~table
      @@ {eos|INSERT INTO blueprints (id, timestamp, payload) VALUES (?, ?, ?)|eos}

    (* See {Note cast} *)
    let select =
      (level ->? t2 payload timestamp) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT CAST(payload AS BLOB), timestamp FROM blueprints WHERE id = ?|eos}

    (* See {Note cast} *)
    let select_range =
      (t2 level level ->* t2 level payload) ~name:__FUNCTION__ ~table
      @@ {|SELECT id, CAST(payload AS BLOB) FROM blueprints
           WHERE ? <= id AND id <= ?
           ORDER BY id ASC|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM blueprints WHERE id > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM blueprints WHERE id < ?|}
  end

  module Context_hashes = struct
    let table = "context_hash" (* For opentelemetry *)

    let insert =
      (t2 level context_hash ->. unit) ~name:__FUNCTION__ ~table
      @@ {eos|REPLACE INTO context_hashes (id, context_hash) VALUES (?, ?)|eos}

    let select =
      (level ->? context_hash) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT (context_hash) FROM context_hashes WHERE id = ?|eos}

    let get_latest =
      (unit ->? t2 level context_hash) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT id, context_hash FROM context_hashes ORDER BY id DESC LIMIT 1|eos}

    let get_earliest =
      (unit ->? t2 level context_hash) ~name:__FUNCTION__ ~table
      @@ {|SELECT id, context_hash FROM context_hashes
           WHERE id >= 0 ORDER BY id ASC LIMIT 1|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM context_hashes WHERE id > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM context_hashes WHERE id < ?|}
  end

  module Kernel_upgrades = struct
    let table = "kernel_updates" (* For opentelemetry *)

    let insert =
      (t3 level root_hash timestamp ->. unit) ~name:__FUNCTION__ ~table
      @@ {|REPLACE INTO kernel_upgrades (injected_before, root_hash, activation_timestamp) VALUES (?, ?, ?)|}

    let activation_levels =
      (unit ->* level) ~name:__FUNCTION__ ~table
      @@ {|SELECT applied_before
           FROM kernel_upgrades
           WHERE applied_before IS NOT NULL
           ORDER BY applied_before DESC
    |}

    let get_latest_unapplied =
      (unit ->? pending_kernel_upgrade) ~name:__FUNCTION__ ~table
      @@ {|SELECT injected_before, root_hash, activation_timestamp
           FROM kernel_upgrades WHERE applied_before IS NULL
           ORDER BY injected_before DESC
           LIMIT 1
    |}

    let find_injected_before =
      (level ->? upgrade) ~name:__FUNCTION__ ~table
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE injected_before = ?|}

    let find_latest_injected_after =
      (level ->? upgrade) ~name:__FUNCTION__ ~table
      @@ {|SELECT root_hash, activation_timestamp
           FROM kernel_upgrades WHERE injected_before > ?
           ORDER BY injected_before DESC
           LIMIT 1|}

    let record_apply =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|
      UPDATE kernel_upgrades SET applied_before = ? WHERE applied_before IS NULL
    |}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM kernel_upgrades WHERE injected_before > ?|}

    let nullify_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|UPDATE kernel_upgrades SET applied_before = NULL WHERE applied_before > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM kernel_upgrades WHERE injected_before < ?|}
  end

  module Sequencer_upgrades = struct
    let insert =
      (t4 level public_key address timestamp ->. unit)
      @@ {|REPLACE INTO sequencer_upgrades (injected_before, sequencer, pool_address, activation_timestamp) VALUES (?, ?, ?, ?)|}

    let activation_levels =
      (unit ->* level)
      @@ {|SELECT applied_before
           FROM sequencer_upgrades
           WHERE applied_before IS NOT NULL
           ORDER BY applied_before DESC
    |}

    let get_latest_unapplied =
      (unit ->? pending_sequencer_upgrade)
      @@ {|SELECT injected_before, sequencer, pool_address, activation_timestamp
           FROM sequencer_upgrades WHERE applied_before IS NULL
           ORDER BY injected_before DESC
           LIMIT 1
    |}

    let find_injected_before =
      (level ->? sequencer_upgrade)
      @@ {|SELECT sequencer, pool_address, activation_timestamp
           FROM sequencer_upgrades WHERE injected_before = ?|}

    let find_latest_injected_after =
      (level ->? sequencer_upgrade)
      @@ {|SELECT sequencer, pool_address, activation_timestamp
           FROM sequencer_upgrades WHERE injected_before > ?
           ORDER BY injected_before DESC
           LIMIT 1|}

    let record_apply =
      (level ->. unit)
      @@ {|
      UPDATE sequencer_upgrades SET applied_before = ? WHERE applied_before IS NULL
    |}

    let clear_after =
      (level ->. unit)
      @@ {|DELETE FROM sequencer_upgrades WHERE injected_before > ?|}

    let nullify_after =
      (level ->. unit)
      @@ {|UPDATE sequencer_upgrades SET applied_before = NULL WHERE applied_before > ?|}

    let clear_before =
      (level ->. unit)
      @@ {|DELETE FROM sequencer_upgrades WHERE injected_before < ?|}
  end

  module Delayed_transactions = struct
    let table = "delayed_transactions" (* For opentelemetry *)

    let insert =
      (t3 level root_hash delayed_transaction ->. unit)
        ~name:__FUNCTION__
        ~table
      @@ {|INSERT INTO delayed_transactions (injected_before, hash, payload) VALUES (?, ?, ?)|}

    (* See {Note cast} *)
    let select_at_level =
      (level ->* delayed_transaction) ~name:__FUNCTION__ ~table
      @@ {|SELECT CAST(payload AS BLOB) FROM delayed_transactions WHERE ? = injected_before|}

    (* See {Note cast} *)
    let select_at_hash =
      (root_hash ->? delayed_transaction) ~name:__FUNCTION__ ~table
      @@ {|SELECT CAST(payload AS BLOB) FROM delayed_transactions WHERE ? = hash|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM delayed_transactions WHERE injected_before > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM delayed_transactions WHERE injected_before < ?|}
  end

  module L1_l2_levels_relationships = struct
    let table = "l1_l2_levels_relationships" (* For opentelemetry *)

    let insert =
      (t2 level l1_level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|INSERT INTO l1_l2_levels_relationships (latest_l2_level, l1_level) VALUES (?, ?)|}

    let get =
      (unit ->! levels) ~name:__FUNCTION__ ~table
      @@ {|SELECT latest_l2_level, l1_level FROM l1_l2_levels_relationships ORDER BY latest_l2_level DESC LIMIT 1|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM l1_l2_levels_relationships WHERE latest_l2_level > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM l1_l2_levels_relationships WHERE latest_l2_level < ?|}
  end

  module L1_l2_finalized_levels = struct
    let table = "l1_l2_finalized_levels" (* For opentelemetry *)

    let insert =
      (t2 l1_level finalized_levels ->. unit) ~name:__FUNCTION__ ~table
      @@ {|REPLACE INTO l1_l2_finalized_levels
           (l1_level, start_l2_level, end_l2_level)
           VALUES (?, ?, ?)|}

    let get =
      (l1_level ->? finalized_levels) ~name:__FUNCTION__ ~table
      @@ {|SELECT start_l2_level, end_l2_level
           FROM l1_l2_finalized_levels
           WHERE l1_level = ?|}

    let last_l2_level =
      (unit ->? level) ~name:__FUNCTION__ ~table
      @@ {|SELECT MAX(end_l2_level) FROM l1_l2_finalized_levels|}

    let last =
      (unit ->? t2 l1_level finalized_levels) ~name:__FUNCTION__ ~table
      @@ {|SELECT l1_level, start_l2_level, end_l2_level
           FROM l1_l2_finalized_levels
           ORDER BY l1_level DESC LIMIT 1|}

    let find_l1_level =
      (level ->? l1_level) ~name:__FUNCTION__ ~table
      @@ {|SELECT l1_level
           FROM l1_l2_finalized_levels
           WHERE $1 > start_l2_level
             AND $1 <= end_l2_level
           ORDER BY l1_level DESC LIMIT 1|}

    let list_by_l2_levels =
      (t2 level level ->* t2 l1_level finalized_levels)
        ~name:__FUNCTION__
        ~table
      @@ {|SELECT l1_level, start_l2_level, end_l2_level
           FROM l1_l2_finalized_levels
           WHERE start_l2_level >= ?
           AND end_l2_level <= ?
           ORDER BY l1_level ASC|}

    let list_by_l1_levels =
      (t2 l1_level l1_level ->* t2 l1_level finalized_levels)
        ~name:__FUNCTION__
        ~table
      @@ {|SELECT l1_level, start_l2_level, end_l2_level
           FROM l1_l2_finalized_levels
           WHERE l1_level BETWEEN ? AND ?
           ORDER BY l1_level ASC|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM l1_l2_finalized_levels
           WHERE start_l2_level < ?|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM l1_l2_finalized_levels
           WHERE end_l2_level > ?|}
  end

  module Metadata = struct
    let table = "metadata" (* For opentelemetry *)

    let insert_smart_rollup_address =
      (smart_rollup_address ->. unit) ~name:__FUNCTION__ ~table
      @@ {|
INSERT INTO metadata (key, value) VALUES ('smart_rollup_address', ?)
ON CONFLICT(key)
DO UPDATE SET value = excluded.value
|}

    let get_smart_rollup_address =
      (unit ->! smart_rollup_address) ~name:__FUNCTION__ ~table
      @@ {|SELECT value from metadata WHERE key = 'smart_rollup_address'|}

    let insert_history_mode =
      (history_mode ->. unit) ~name:__FUNCTION__ ~table
      @@ {|
INSERT INTO metadata (key, value) VALUES ('history_mode', ?)
ON CONFLICT(key)
DO UPDATE SET value = excluded.value
|}

    let get_history_mode =
      (unit ->! history_mode) ~name:__FUNCTION__ ~table
      @@ {|SELECT value from metadata WHERE key = 'history_mode'|}
  end

  module Transactions = struct
    let table = "transactions" (* For opentelemetry *)

    (* WARNING: the SQLite extension in [sqlite_receipt_bloom/] depends on the
       database storing receipts with [Transaction_info.receipt_fields_encoding]
       to work. If it is ever changed, the SQLite extension will also need to be
       adapted. *)
    (* WARNING: uses [octets] — SELECT queries must use
       [CAST(receipt_fields AS BLOB)] to support old TEXT rows. *)
    (* The decoder is shared by both column types — it transparently accepts
       compact and legacy rows — so SELECT queries can use either; only the
       encoder used at INSERT time differs. *)
    let make_receipt_fields write_encoding =
      custom
        ~encode:(fun payload ->
          Ok (Data_encoding.Binary.to_string_exn write_encoding payload))
        ~decode:(fun bytes ->
          Option.to_result ~none:"Not a valid receipt fields payload"
          @@ Data_encoding.Binary.of_string_opt
               Transaction_info.receipt_fields_encoding
               bytes)
        octets

    let receipt_fields =
      make_receipt_fields Compact_transactions_receipt.encoding

    (* Same column type as [receipt_fields] but encodes using the legacy
       (non-compact) format, so the resulting rows remain decodable by node
       releases that predate the compact encoding. *)
    let receipt_fields_legacy =
      make_receipt_fields Compact_transactions_receipt.legacy_encoding

    (* WARNING: uses [octets] — SELECT queries must use
       [CAST(object_fields AS BLOB)] to support old TEXT rows. *)
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
        octets

    (* Built once per receipt-fields column type so Caqti's
       prepared-statement cache keeps each request's identity stable across
       calls. Dispatch happens at the call site. *)
    let insert_with receipt_fields =
      (t8
         block_hash
         level
         quantity
         root_hash
         address
         (option address)
         receipt_fields
         object_fields
      ->. unit)
        ~name:__FUNCTION__
        ~table
        ~attrs:(fun (_, _, _, hash, _, _, _, _) ->
          [Telemetry.Attributes.Transaction.hash hash])
      @@ {eos|INSERT INTO transactions (block_hash, block_number, index_, hash, from_, to_, receipt_fields, object_fields) VALUES (?, ?, ?, ?, ?, ?, ?, ?)|eos}

    let insert_compact = insert_with receipt_fields

    let insert_legacy = insert_with receipt_fields_legacy

    let select_receipt =
      (root_hash
      ->? t7
            block_hash
            level
            quantity
            root_hash
            address
            (option address)
            receipt_fields)
        ~name:__FUNCTION__
        ~table
      (* See {Note cast} *)
      @@ {eos|SELECT block_hash, block_number, index_, hash, from_, to_, CAST(receipt_fields AS BLOB) FROM transactions WHERE hash = ?|eos}

    let select_receipts_from_block_number =
      (level
      ->* t6
            block_hash
            quantity
            root_hash
            address
            (option address)
            receipt_fields)
        ~name:__FUNCTION__
        ~table
      (* See {Note cast} *)
      @@ {eos|SELECT block_hash, index_, hash, from_, to_, CAST(receipt_fields AS BLOB) FROM transactions WHERE block_number = ? ORDER BY index_ DESC|eos}

    let select_receipts_from_block_range =
      (t3 level level (option octets)
      ->* t6
            block_hash
            quantity
            root_hash
            address
            (option address)
            receipt_fields)
        ~name:__FUNCTION__
        ~table
      (* See {Note cast} *)
      @@ {eos|SELECT block_hash, index_, hash, from_, to_, CAST(receipt_fields AS BLOB) FROM transactions
              WHERE $1 <= block_number AND block_number < $2
              AND ($3 IS NULL OR
                   receipt_contains_bloom_filter(receipt_fields, $3))
              ORDER BY block_number DESC, index_ DESC|eos}

    let select_object =
      (root_hash
      ->? t7
            block_hash
            level
            quantity
            root_hash
            address
            (option address)
            object_fields)
        ~name:__FUNCTION__
        ~table
      (* See {Note cast} *)
      @@ {eos|SELECT block_hash, block_number, index_, hash, from_, to_, CAST(object_fields AS BLOB) FROM transactions WHERE hash = ?|eos}

    let select_objects_from_block_number =
      (level ->* t5 quantity root_hash address (option address) object_fields)
        ~name:__FUNCTION__
        ~table
      (* See {Note cast} *)
      @@ {eos|SELECT index_, hash, from_, to_, CAST(object_fields AS BLOB) FROM transactions WHERE block_number = ?|eos}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM transactions WHERE block_number > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM transactions WHERE block_number < ?|}
  end

  module Block_storage_mode = struct
    let table = "block_storage_mode" (* For opentelemetry *)

    let legacy =
      (unit ->! Caqti_type.Std.bool) ~name:__FUNCTION__ ~table
      @@ {|SELECT legacy FROM block_storage_mode|}

    let force_legacy =
      (unit ->. unit) ~name:__FUNCTION__ ~table
      @@ {|UPDATE block_storage_mode SET legacy = 1|}
  end

  module Blocks = struct
    let table = "blocks" (* For opentelemetry *)

    let insert =
      (t5 level block_hash block (option tezos_block) (option block_hash)
      ->. unit)
        ~name:__FUNCTION__
        ~table
      @@ {eos|INSERT INTO blocks (level, hash, block, tez_block, tez_hash) VALUES (?, ?, ?, ?, ?)|eos}

    let tez_insert =
      (t3 level block_hash tezos_block ->. unit) ~name:__FUNCTION__ ~table
      @@ {eos|INSERT INTO blocks (level, hash, block) VALUES (?, ?, ?)|eos}

    (* See {Note cast} *)
    let select_with_level =
      (level ->? block) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT CAST(block AS BLOB) FROM blocks WHERE level = ?|eos}

    (* See {Note cast} *)
    let tez_select_with_level =
      (level ->? tezos_block) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT CAST(block AS BLOB) FROM blocks WHERE level = ?|eos}

    (* See {Note cast} *)
    let tezosx_select_tez_block_with_level =
      (level ->? tezos_block) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT CAST(tez_block AS BLOB) FROM blocks WHERE level = ? AND tez_block IS NOT NULL|eos}

    (* See {Note cast} *)
    let select_with_hash =
      (block_hash ->? block) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT CAST(block AS BLOB) FROM blocks WHERE hash = ?|eos}

    let select_hash_of_number =
      (level ->? block_hash) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT hash FROM blocks WHERE level = ?|eos}

    let select_tez_hash_of_number =
      (level ->? block_hash) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT tez_hash FROM blocks WHERE level = ? AND tez_hash IS NOT NULL|eos}

    let select_number_of_hash =
      (block_hash ->? level) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT level FROM blocks WHERE hash = ?|eos}

    let select_number_of_tez_hash =
      (block_hash ->? level) ~name:__FUNCTION__ ~table
      @@ {eos|SELECT level FROM blocks WHERE tez_hash = ?|eos}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM blocks WHERE level > ?|}

    let clear_before =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM blocks WHERE level < ?|}
  end

  let context_hash_of_block_hash =
    (block_hash ->? context_hash) ~name:__FUNCTION__ ~table:"context_hashes"
    @@ {eos|SELECT c.context_hash from Context_hashes c JOIN Blocks b on c.id = b.level WHERE hash = ?|eos}

  module Irmin_chunks = struct
    let table = "irmin_chunks" (* For opentelemetry *)

    let insert =
      (t2 level timestamp ->. unit) ~name:__FUNCTION__ ~table
      @@ {|INSERT INTO irmin_chunks (level, timestamp) VALUES (?, ?)|}

    let nth =
      (int ->? t2 level timestamp) ~name:__FUNCTION__ ~table
      @@ {|SELECT level, timestamp from irmin_chunks ORDER BY level DESC LIMIT 1 OFFSET ?|}

    let latest =
      (unit ->? t2 level timestamp) ~name:__FUNCTION__ ~table
      @@ {|SELECT level, timestamp from irmin_chunks ORDER BY level DESC LIMIT 1|}

    let clear =
      (unit ->. unit) ~name:__FUNCTION__ ~table @@ {|DELETE FROM irmin_chunks|}

    let clear_after =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM irmin_chunks WHERE level > ?|}

    let clear_before_included =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM irmin_chunks WHERE level <= ?|}
  end

  module Pending_confirmations = struct
    let table = "pending_confirmations" (* For opentelemetry *)

    let insert =
      (t2 level block_hash ->. unit) ~name:__FUNCTION__ ~table
      @@ {|INSERT INTO pending_confirmations (level, hash) VALUES (?, ?)|}

    let select_with_level =
      (level ->? block_hash) ~name:__FUNCTION__ ~table
      @@ {|SELECT hash FROM pending_confirmations WHERE level = ?|}

    let delete_with_level =
      (level ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM pending_confirmations WHERE level = ?|}

    let clear =
      (unit ->. unit) ~name:__FUNCTION__ ~table
      @@ {|DELETE FROM pending_confirmations|}

    let count =
      (unit ->! level) ~name:__FUNCTION__ ~table
      @@ {|SELECT COUNT(*) FROM pending_confirmations|}
  end
end

(*
  To introduce a new migration

    - Create a .sql file led by the next migration number [N = version + 1]
      (with leading 0s) followed by the name of the migration (e.g.
      [005_create_blueprints_table.sql]), or
    - Create a .ml file of with name [mN_migration_name.ml] (e.g.
      [m005_create_blueprints_table.ml])
    - Run [etherlink/scripts/check_evm_store_migrations.sh promote]
    - Increment [Evm_migration.version] or [Tezlink_migration.version] below
      depending on the context
    - Regenerate the schemas, using [[
          dune exec -- etherlink/tezt/tests/main.exe --file evm_sequencer.ml \
            evm store schemas regression --reset-regressions
      ]]

  You can review the result at
  [etherlink/tezt/tests/expected/evm_sequencer.ml/EVM Node- debug print store schemas.out].
*)
module Evm_migration = Sqlite.Migration.Make (struct
  let table_name = "migrations"

  let version = 24

  let all_migrations = Evm_node_migrations.all
end)

module Tezlink_migration = Sqlite.Migration.Make (struct
  let table_name = "tezlink_migrations"

  let version = 0

  let all_migrations = Tezlink_node_migrations.all
end)

let sqlite_file_name = "store.sqlite"

let init (type f) ?max_conn_reuse_count
    ~(chain_family : f L2_types.chain_family) ~data_dir ~perm () =
  let open Lwt_result_syntax in
  let path = data_dir // sqlite_file_name in
  let read_only = match perm with Read_only _ -> true | Read_write -> false in
  let migration conn =
    Sqlite.assert_in_transaction conn ;
    with_connection conn @@ fun db_conn ->
    let* () =
      Evm_migration.apply
        db_conn
        ~read_only
        ~on_init:Evm_store_events.init_store
        ~on_future:Evm_store_events.migrations_from_the_future
        ~on_applied:Evm_store_events.applied_migration
        ()
    in
    let* () =
      match chain_family with
      | L2_types.Michelson ->
          Tezlink_migration.apply
            db_conn
            ~read_only
            ~on_future:Evm_store_events.migrations_from_the_future
            ~on_applied:Evm_store_events.applied_migration
            ()
      | _ -> return_unit
    in
    let* legacy_block_storage_mode =
      Db.find db_conn Q.Block_storage_mode.legacy ()
    in
    let* () =
      when_ legacy_block_storage_mode @@ fun () ->
      failwith
        "The EVM node is in legacy block storage mode which is no longer \
         supported."
    in
    return_unit
  in
  Sqlite.init
    ?max_conn_reuse_count
    ~register:Sqlite_receipt_bloom.register
    ~path
    ~perm
    migration

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
    let* levels = Db.find_opt conn Q.L1_l2_levels_relationships.get () in
    let* finalized =
      Db.find_opt conn Q.L1_l2_finalized_levels.last_l2_level ()
    in
    match (levels, finalized) with
    | None, _ | _, None -> return_none
    | Some {current_number = Qty current_number; _}, Some (Qty finalized) ->
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

module Sequencer_upgrades = struct
  let store store next_blueprint_number (event : Evm_events.Sequencer_upgrade.t)
      =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Sequencer_upgrades.insert
      ( next_blueprint_number,
        event.sequencer,
        event.pool_address,
        event.timestamp )

  let find_latest_pending store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Sequencer_upgrades.get_latest_unapplied ()

  let find_injected_before store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Sequencer_upgrades.find_injected_before level

  let find_latest_injected_after store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Sequencer_upgrades.find_latest_injected_after level

  let record_apply store level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Sequencer_upgrades.record_apply level

  let clear_after store l2_level =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let* () = Db.exec conn Q.Sequencer_upgrades.clear_after l2_level in
    Db.exec conn Q.Sequencer_upgrades.nullify_after l2_level

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.Sequencer_upgrades.clear_before l2_level

  let activation_levels store =
    with_connection store @@ fun conn ->
    Db.collect_list conn Q.Sequencer_upgrades.activation_levels ()
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

  let find_with_events_legacy conn level =
    let open Lwt_result_syntax in
    let* blueprint = find conn level in
    match blueprint with
    | None -> return None
    | Some blueprint ->
        let* kernel_upgrade = Kernel_upgrades.find_injected_before conn level in
        let* delayed_transactions = Delayed_transactions.at_level conn level in
        return_some
          Blueprint_types.Legacy.
            {delayed_transactions; kernel_upgrade; blueprint}

  let find_with_events conn level =
    let open Lwt_result_syntax in
    let* blueprint = find conn level in
    match blueprint with
    | None -> return None
    | Some blueprint ->
        let* kernel_upgrade = Kernel_upgrades.find_injected_before conn level in
        let* sequencer_upgrade =
          Sequencer_upgrades.find_injected_before conn level
        in
        let* delayed_transactions = Delayed_transactions.at_level conn level in
        return_some
          Blueprint_types.
            {delayed_transactions; sequencer_upgrade; kernel_upgrade; blueprint}

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
  type t = levels = {l1_level : int32; current_number : Ethereum_types.quantity}

  let store store ~l1_level ~latest_l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_l2_levels_relationships.insert (latest_l2_level, l1_level)

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

module L1_l2_finalized_levels = struct
  type t = finalized_levels = {
    start_l2_level : Ethereum_types.quantity;
    end_l2_level : Ethereum_types.quantity;
  }

  let store store ~l1_level ~start_l2_level ~end_l2_level =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.L1_l2_finalized_levels.insert
      (l1_level, {start_l2_level; end_l2_level})

  let find store ~l1_level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_l2_finalized_levels.get l1_level

  let last store =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_l2_finalized_levels.last ()

  let find_l1_level store ~l2_level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.L1_l2_finalized_levels.find_l1_level l2_level

  let list_by_l2_levels store ~start_l2_level ~end_l2_level =
    with_connection store @@ fun conn ->
    Db.collect_list
      conn
      Q.L1_l2_finalized_levels.list_by_l2_levels
      (start_l2_level, end_l2_level)

  let list_by_l1_levels store ~start_l1_level ~end_l1_level =
    with_connection store @@ fun conn ->
    Db.collect_list
      conn
      Q.L1_l2_finalized_levels.list_by_l1_levels
      (start_l1_level, end_l1_level)

  let max_blocks = 10_000

  let make_l1_bounds x y =
    let max = Int32.of_int (max_blocks - 1) in
    let rec aux acc x y =
      let prev_x = Int32.sub y max in
      if prev_x <= x then (x, y) :: acc
      else aux ((prev_x, y) :: acc) x (Int32.pred prev_x)
    in
    aux [] x y

  let make_l2_bounds x y =
    let max = Z.of_int (max_blocks - 1) in
    let rec aux acc x y =
      let prev_x = Z.sub y max in
      if Z.Compare.(prev_x <= x) then (x, y) :: acc
      else aux ((prev_x, y) :: acc) x (Z.pred prev_x)
    in
    aux [] x y

  (* Paginated version of list_by_l2_levels *)
  let list_by_l2_levels store
      ~start_l2_level:(Ethereum_types.Qty start_l2_level)
      ~end_l2_level:(Ethereum_types.Qty end_l2_level) =
    let levels = make_l2_bounds start_l2_level end_l2_level in
    List.concat_map_es
      (fun (x, y) ->
        list_by_l2_levels store ~start_l2_level:(Qty x) ~end_l2_level:(Qty y))
      levels

  (* Paginated version of list_by_l1_levels *)
  let list_by_l1_levels store ~start_l1_level ~end_l1_level =
    let levels = make_l1_bounds start_l1_level end_l1_level in
    List.concat_map_es
      (fun (start_l1_level, end_l1_level) ->
        list_by_l1_levels store ~start_l1_level ~end_l1_level)
      levels

  let clear_before store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_l2_finalized_levels.clear_before l2_level

  let clear_after store l2_level =
    with_connection store @@ fun conn ->
    Db.exec conn Q.L1_l2_finalized_levels.clear_after l2_level
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
  let store ~compact_receipt_encoding store
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
      (if compact_receipt_encoding then Q.Transactions.insert_compact
       else Q.Transactions.insert_legacy)
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
               } )
         ->
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
            logs =
              List.map
                (Compact_transactions_receipt.fill_log_context
                   ~hash
                   ~block_hash
                   ~block_number
                   ~index)
                logs;
            logsBloom = logs_bloom;
            type_;
            status;
            contractAddress = contract_address;
          })
      receipt

  let receipts_of_block_number store level =
    with_connection store @@ fun conn ->
    Db.fold
      conn
      Q.Transactions.select_receipts_from_block_number
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
               } )
           acc
         ->
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
            logs =
              List.map
                (Compact_transactions_receipt.fill_log_context
                   ~hash
                   ~block_hash
                   ~block_number:level
                   ~index)
                logs;
            logsBloom = logs_bloom;
            type_;
            status;
            contractAddress = contract_address;
          }
        :: acc)
      level
      []

  let receipts_of_block_range ?mask store (Ethereum_types.Qty level) len =
    let open Lwt_result_syntax in
    with_connection store @@ fun conn ->
    let+ res =
      Db.fold
        conn
        Q.Transactions.select_receipts_from_block_range
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
                 } )
             acc
           ->
          Transaction_receipt.
            {
              transactionHash = hash;
              transactionIndex = index;
              blockHash = block_hash;
              blockNumber = Qty level;
              from;
              to_;
              cumulativeGasUsed = cumulative_gas_used;
              effectiveGasPrice = effective_gas_price;
              gasUsed = gas_used;
              logs =
                List.map
                  (Compact_transactions_receipt.fill_log_context
                     ~hash
                     ~block_hash
                     ~block_number:(Qty level)
                     ~index)
                  logs;
              logsBloom = logs_bloom;
              type_;
              status;
              contractAddress = contract_address;
            }
          :: acc)
        ( Qty level,
          Qty Z.(level + of_int len),
          Option.map Bytes.unsafe_to_string mask )
        []
    in
    res

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
             )
           ->
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
  let store ?tez_block store
      (block : Ethereum_types.legacy_transaction_object Ethereum_types.block) =
    with_connection store @@ fun conn ->
    let tez_hash =
      Option.map (fun (block : L2_types.Tezos_block.t) -> block.hash) tez_block
    in
    Db.exec
      conn
      Q.Blocks.insert
      (block.number, block.hash, block, tez_block, tez_hash)

  let tez_store store (block : L2_types.Tezos_block.t) =
    with_connection store @@ fun conn ->
    Db.exec
      conn
      Q.Blocks.tez_insert
      (Qty (Z.of_int32 block.level), block.hash, block)

  let tezosx_find_tez_block_with_level store level =
    let open Lwt_result_syntax in
    let* block_opt =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Blocks.tezosx_select_tez_block_with_level level
    in
    return block_opt

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
             )
           ->
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

  let get_with_level ~full_transaction_object store (Ethereum_types.Qty level) =
    let open Lwt_result_syntax in
    let* block_opt =
      find_with_level ~full_transaction_object store (Qty level)
    in
    match block_opt with
    | Some block -> return block
    | None -> failwith "Could not find block %a" Z.pp_print level

  let tez_find_with_level store level =
    let open Lwt_result_syntax in
    let* block_opt =
      with_connection store @@ fun conn ->
      Db.find_opt conn Q.Blocks.tez_select_with_level level
    in
    return block_opt

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

  let find_tez_hash_of_number store level =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Blocks.select_tez_hash_of_number level

  let find_number_of_hash store hash =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Blocks.select_number_of_hash hash

  let find_number_of_tez_hash store hash =
    with_connection store @@ fun conn ->
    Db.find_opt conn Q.Blocks.select_number_of_tez_hash hash

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
  let* () = L1_l2_finalized_levels.clear_after store l2_level in
  let* () = Kernel_upgrades.clear_after store l2_level in
  let* () = Sequencer_upgrades.clear_after store l2_level in
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
  let* () = L1_l2_finalized_levels.clear_before store l2_level in
  let* () =
    match history_mode with
    | Configuration.Rolling _ ->
        let* () = Blueprints.clear_before store l2_level in
        let* () = Blocks.clear_before store l2_level in
        let* () = Transactions.clear_before store l2_level in
        let* () = Kernel_upgrades.clear_before store l2_level in
        let* () = Sequencer_upgrades.clear_before store l2_level in
        let* () = Delayed_transactions.clear_before store l2_level in
        return_unit
    | Configuration.Seed _ ->
        (* Blueprints are kept forever: they are the only data served to
           consumers in this mode. Blocks and transactions are pruned.
           Delayed_transactions, Kernel_upgrades and Sequencer_upgrades are
           kept because [find_with_events] reads them to reconstruct
           blueprint events served at the /evm/v2/blueprint endpoint. *)
        let* () = Blocks.clear_before store l2_level in
        let* () = Transactions.clear_before store l2_level in
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

(* {Note cast}

   Backward compatibility cast to avoid a costly migration, as some
   columns have been stored as TEXT instead of BLOB. *)
