(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Low-level durable storage primitives *)

let inspect_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* value = Pvm.Kernel.find_key_in_durable evm_state key in
  Option.map_s Tezos_lazy_containers.Chunked_byte_vector.to_bytes value

let modify_durable ?edit_readonly ~key ~value evm_state =
  Pvm.Kernel.set_durable_value ?edit_readonly evm_state key value

let delete_durable ~kind evm_state path =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn path in
  let* pvm_state = Pvm.Kernel.decode evm_state in
  let open Tezos_scoru_wasm.Wasm_pvm_state.Internal_state in
  let* durable =
    Tezos_scoru_wasm.Durable.delete ~kind (durable_of pvm_state.storage) key
  in
  Pvm.Kernel.encode
    {pvm_state with storage = update_durable pvm_state.storage durable}
    evm_state

let exists_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  let+ v = Tezos_scoru_wasm.Durable.find_value durable key in
  Option.is_some v

let exists_dir_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.exists durable key

let subkeys_durable evm_state key =
  let open Lwt_syntax in
  let key = Tezos_scoru_wasm.Durable.key_of_string_exn key in
  let* durable = Pvm.Kernel.wrap_as_durable_storage evm_state in
  let durable = Tezos_scoru_wasm.Durable.of_storage_exn durable in
  Tezos_scoru_wasm.Durable.list durable key

(** Decoded form of the [/evm/world_state/accounts/<addr>/info] record:
    balance, nonce, and code hash for an EVM account. *)
module EVM_account_info = struct
  type t = {
    balance : Ethereum_types.quantity;
    nonce : Ethereum_types.quantity;
    code_hash : Ethereum_types.hash;
  }

  let encode {balance; nonce; code_hash} =
    let open Rlp in
    Rlp.encode
      (List
         [
           Value (Ethereum_types.encode_u256_le balance);
           Value (Ethereum_types.encode_u64_le nonce);
           Value (Ethereum_types.encode_hash code_hash);
         ])

  let decode_opt b =
    match Rlp.decode b with
    | Ok (List [Value balance; Value nonce; Value code_hash]) ->
        let balance = Ethereum_types.decode_number_le balance in
        let nonce = Ethereum_types.decode_number_le nonce in
        let code_hash = Ethereum_types.decode_hash code_hash in
        Some {balance; nonce; code_hash}
    | _ -> None

  let decode b =
    match decode_opt b with
    | Some x -> Ok x
    | None ->
        Result_syntax.tzfail
          (Rlp.Rlp_decoding_error
             (Format.asprintf
                "Invalid account info format: %s"
                Hex.(show @@ of_bytes b)))
end

(* Typed path GADT — see [.mli] for the capability semantics. *)

(** Phantom capability markers for [path]: [rw] is read+write+delete, [ro] is
    read-only. *)
type rw = [`Read | `Write | `Delete]

type ro = [`Read]

type read_delete = [`Read | `Delete]

type ('a, 'cap) path =
  | Raw_path : string -> (bytes, rw) path
  | Chain_id : (L2_types.chain_id, ro) path
  | Michelson_runtime_chain_id : (L2_types.chain_id, ro) path
  | Kernel_version : (string, ro) path
  | Kernel_root_hash : (Ethereum_types.hex, ro) path
  | Multichain_flag : (unit, ro) path
  | Sequencer_key : (Signature.Public_key.t, ro) path
  | Chain_config_family :
      L2_types.chain_id
      -> (L2_types.ex_chain_family, ro) path
  | Tezosx_feature_flag : Tezosx.runtime -> (unit, ro) path
  | Michelson_runtime_sunrise_level : (Ethereum_types.quantity, ro) path
  | Current_block_number :
      _ L2_types.chain_family
      -> (Ethereum_types.quantity, ro) path
  | Current_block_hash :
      _ L2_types.chain_family
      -> (Ethereum_types.block_hash, ro) path
  | Evm_node_flag : (unit, rw) path
  | Blueprint_chunk : {
      blueprint_number : Z.t;
      chunk_index : int;
    }
      -> (bytes, rw) path
  | Blueprint_nb_chunks : Z.t -> (int, rw) path
  | Blueprint_generation : Z.t -> (Ethereum_types.quantity, rw) path
  | Single_tx_input : (Rlp.item, rw) path
  | Assemble_block_input : (Rlp.item, rw) path
  | Current_block :
      _ L2_types.chain_family
      -> (Ethereum_types.legacy_transaction_object L2_types.block, ro) path
  | Block_by_hash :
      _ L2_types.chain_family * Ethereum_types.block_hash
      -> ( Ethereum_types.legacy_transaction_object L2_types.block,
           read_delete )
         path
  | Block_index :
      _ L2_types.chain_family * Durable_storage_path.Block.number
      -> (unit, [`Delete]) path
  | Tezosx_tezos_current_block :
      (Ethereum_types.legacy_transaction_object L2_types.block, ro) path
  | Current_receipts : (Transaction_receipt.t, ro) path
  | Backlog : (int64, ro) path
  | Minimum_base_fee_per_gas : (Z.t, ro) path
  | Da_fee_per_byte : (Ethereum_types.quantity, ro) path
  | Maximum_gas_per_transaction : (Ethereum_types.quantity, ro) path
  | Michelson_to_evm_gas_multiplier : (int64, ro) path
  | Sequencer_pool_address : (Ethereum_types.address, ro) path
  | Evm_legacy_account_balance :
      Ethereum_types.address
      -> (Ethereum_types.quantity, rw) path
  | Evm_legacy_account_nonce :
      Ethereum_types.address
      -> (Ethereum_types.quantity, rw) path
  | Evm_legacy_account_code :
      Ethereum_types.address
      -> (Ethereum_types.hex, rw) path
  | Evm_legacy_account_code_hash :
      Ethereum_types.address
      -> (Ethereum_types.hash, ro) path
  | Evm_legacy_block_by_hash :
      Ethereum_types.block_hash
      -> ( Ethereum_types.legacy_transaction_object Ethereum_types.block,
           ro )
         path
  | Evm_legacy_current_block :
      (Ethereum_types.legacy_transaction_object Ethereum_types.block, ro) path
  | Evm_code_by_hash : Ethereum_types.hash -> (Ethereum_types.hex, rw) path
  | Evm_account_storage :
      Durable_storage_path.Accounts.fixed_address
      * Durable_storage_path.Accounts.fixed_index
      -> (Ethereum_types.hex, rw) path
  | Evm_account_info : Ethereum_types.address -> (EVM_account_info.t, rw) path
  | Tezos_account_info :
      Tezosx.Tezos_runtime.address
      -> (Tezosx.Tezos_runtime.account_info, ro) path
  | Evm_block_hash_by_number :
      Durable_storage_path.Block.number
      -> (Ethereum_types.block_hash, ro) path
  | Evm_transaction_receipt_by_hash :
      Ethereum_types.hash * Ethereum_types.block_hash
      -> (Transaction_receipt.t, ro) path
  | Evm_transaction_object_by_hash :
      Ethereum_types.hash * Ethereum_types.block_hash option
      -> (Ethereum_types.legacy_transaction_object, ro) path
  | Evm_current_block_receipts :
      Ethereum_types.block_hash
      -> (Transaction_receipt.t list, ro) path
  | Evm_current_block_transactions_objects :
      Ethereum_types.block_hash
      -> (Ethereum_types.legacy_transaction_object list, ro) path
  | Tezos_contract_storage :
      Tezos_types.Contract.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_contract_code :
      Tezos_types.Contract.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_value :
      Tezlink_imports.Imported_context.Big_map.Id.t
      * Tezlink_imports.Imported_protocol.Script_expr_hash.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_key_type :
      Tezlink_imports.Imported_context.Big_map.Id.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_value_type :
      Tezlink_imports.Imported_context.Big_map.Id.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezlink_balance : Tezos_types.Contract.t -> (Tezos_types.Tez.t, ro) path
  | Tezlink_manager : Tezos_types.Contract.t -> (Tezos_types.Manager.t, ro) path
  | Tezlink_counter : Tezos_types.Contract.t -> (Z.t, ro) path
  | Tezlink_block_by_hash :
      Ethereum_types.block_hash
      -> (L2_types.Tezos_block.t, ro) path
  | Tezlink_block_hash_by_number :
      Durable_storage_path.Block.number
      -> (Ethereum_types.block_hash, ro) path
  | Blueprint_current_generation : (Ethereum_types.quantity, ro) path
  | Kernel_boot_wasm : (bytes, rw) path
  | Kernel_verbosity : (string, rw) path

type 'a ro_resolved = {path : string; decode : bytes -> 'a tzresult}

type 'a rw_resolved = {
  path : string;
  decode : bytes -> 'a tzresult;
  encode : 'a -> string;
}

(** Read-capable resolution variants. [Delete_only] is intentionally absent
    here — it lives directly in {!resolved}, so [path_and_decode] can
    pattern-match exhaustively without an [assert false] on an impossible
    case. *)
type ('a, 'cap) readable_resolved =
  | Read_write : 'a rw_resolved -> ('a, rw) readable_resolved
  | Read_only : 'a ro_resolved -> ('a, ro) readable_resolved
  | Read_delete : 'a ro_resolved -> ('a, read_delete) readable_resolved

(** A resolved path: either read-capable (and optionally writable / deletable),
    or delete-only. *)
type ('a, 'cap) resolved =
  | Readable : ('a, 'cap) readable_resolved -> ('a, 'cap) resolved
  | Delete_only : {path : string} -> (unit, [`Delete]) resolved

let path_of : type a cap. (a, cap) resolved -> string = function
  | Readable (Read_write {path; _}) -> path
  | Readable (Read_only {path; _}) -> path
  | Readable (Read_delete {path; _}) -> path
  | Delete_only {path} -> path

type ('a, 'cap) resolution =
  | Static of ('a, 'cap) resolved
  | Versioned of (storage_version:int -> ('a, 'cap) resolved)

let infallible_decode decode bytes = Ok (decode bytes)

(** Decode [bytes] using a data-encoding and surface decoding failures as a
    [tzresult]. Use this instead of [infallible_decode (Binary.of_bytes_exn _)]:
    the latter only wraps the result in [Ok] without catching the exception
    that [of_bytes_exn] raises on malformed payloads, so the exception escapes
    [read_opt]'s [let*?] into the surrounding Lwt task. *)
let safe_binary_decode enc bytes =
  match Data_encoding.Binary.of_bytes enc bytes with
  | Ok v -> Ok v
  | Error e ->
      Result_syntax.tzfail (Tezos_base.Data_encoding_wrapper.Decoding_error e)

(** Empty-body presence flags: the value on disk is an (empty) marker; we only
    care about existence. *)
let unit_flag_codec ~path : unit rw_resolved =
  {path; decode = (fun _bytes -> Ok ()); encode = (fun () -> "")}

(** Read-only sibling of {!unit_flag_codec} for presence flags the EVM node
    only checks but never writes. *)
let unit_flag_ro_codec ~path : unit ro_resolved =
  {path; decode = (fun _bytes -> Ok ())}

(** Little-endian [int64]-backed kernel-owned scalars stored via
    [Data_encoding.Little_endian.int64]. Read-only — the EVM node never
    writes these. *)
let int64_le_ro_codec ~path : int64 ro_resolved =
  {
    path;
    decode =
      infallible_decode Data_encoding.(Binary.of_bytes_exn Little_endian.int64);
  }

(** Little-endian [Z.t]-backed quantities stored via [Z.{to,of}_bits].
    Read-only — the EVM node never writes these. *)
let qty_le_ro_codec ~path : Ethereum_types.quantity ro_resolved =
  {
    path;
    decode =
      infallible_decode (fun bytes ->
          Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits));
  }

(** RLP-encoded values stored as bytes. *)
let rlp_codec ~path : Rlp.item rw_resolved =
  {
    path;
    decode = Rlp.decode;
    encode = (fun item -> Bytes.to_string (Rlp.encode item));
  }

(** Kernel-owned typed block stored under [path]; read-only, decoded via
    [L2_types.block_from_bytes] for the given [chain_family]. *)
let block_ro_codec (type f) ~path ~(chain_family : f L2_types.chain_family) :
    Ethereum_types.legacy_transaction_object L2_types.block ro_resolved =
  {
    path;
    decode = (fun bytes -> Ok (L2_types.block_from_bytes ~chain_family bytes));
  }

(** Smart constructors for [resolve] arms — wrap the [ro_resolved] /
    [rw_resolved] records into a [resolution], hiding the
    [Readable]/[Read_write]/[Read_only]/[Read_delete] layers that every
    read-side arm would otherwise have to spell out. *)
let static_rw : type a. a rw_resolved -> (a, rw) resolution =
 fun r -> Static (Readable (Read_write r))

let static_ro : type a. a ro_resolved -> (a, ro) resolution =
 fun r -> Static (Readable (Read_only r))

let static_read_delete : type a. a ro_resolved -> (a, read_delete) resolution =
 fun r -> Static (Readable (Read_delete r))

let versioned_rw : type a.
    (storage_version:int -> a rw_resolved) -> (a, rw) resolution =
 fun f ->
  Versioned (fun ~storage_version -> Readable (Read_write (f ~storage_version)))

let versioned_ro : type a.
    (storage_version:int -> a ro_resolved) -> (a, ro) resolution =
 fun f ->
  Versioned (fun ~storage_version -> Readable (Read_only (f ~storage_version)))

let resolve : type a cap. (a, cap) path -> (a, cap) resolution = function
  | Raw_path key ->
      static_rw
        {
          path = key;
          decode = infallible_decode Fun.id;
          encode = Bytes.to_string;
        }
  | Chain_id ->
      versioned_ro (fun ~storage_version ->
          {
            path = Durable_storage_path.chain_id ~storage_version;
            decode = infallible_decode L2_types.Chain_id.decode_le;
          })
  | Michelson_runtime_chain_id ->
      static_ro
        {
          path = Durable_storage_path.michelson_runtime_chain_id;
          decode = infallible_decode L2_types.Chain_id.decode_be;
        }
  | Kernel_version ->
      versioned_ro (fun ~storage_version ->
          {
            path = Durable_storage_path.kernel_version ~storage_version;
            decode = infallible_decode Bytes.to_string;
          })
  | Kernel_root_hash ->
      versioned_ro (fun ~storage_version ->
          {
            path = Durable_storage_path.kernel_root_hash ~storage_version;
            decode =
              (fun bytes ->
                let (`Hex s) = Hex.of_bytes bytes in
                Ok (Ethereum_types.Hex s));
          })
  | Multichain_flag ->
      static_ro
        (unit_flag_ro_codec ~path:Durable_storage_path.Feature_flags.multichain)
  | Sequencer_key ->
      versioned_ro (fun ~storage_version ->
          {
            path = Durable_storage_path.sequencer_key ~storage_version;
            decode =
              (fun bytes ->
                Signature.Public_key.of_b58check (Bytes.to_string bytes));
          })
  | Chain_config_family cid ->
      versioned_ro (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Chain_configuration.chain_family
                ~storage_version
                cid;
            decode = L2_types.Chain_family.of_bytes;
          })
  | Tezosx_feature_flag runtime ->
      static_ro (unit_flag_ro_codec ~path:(Tezosx.feature_flag runtime))
  | Michelson_runtime_sunrise_level ->
      versioned_ro (fun ~storage_version ->
          qty_le_ro_codec
            ~path:
              (Durable_storage_path.michelson_runtime_sunrise_level
                 ~storage_version))
  | Current_block_number chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_ro
        (qty_le_ro_codec
           ~path:(Durable_storage_path.Block.current_number ~root))
  | Current_block_hash chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_ro
        {
          path = Durable_storage_path.Block.current_hash ~root;
          decode = infallible_decode Ethereum_types.decode_block_hash;
        }
  | Evm_node_flag ->
      versioned_rw (fun ~storage_version ->
          unit_flag_codec
            ~path:(Durable_storage_path.evm_node_flag ~storage_version))
  | Blueprint_chunk {blueprint_number; chunk_index} ->
      versioned_rw (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Blueprint.chunk
                ~storage_version
                ~blueprint_number
                ~chunk_index;
            decode = infallible_decode Fun.id;
            encode = Bytes.to_string;
          })
  | Blueprint_nb_chunks blueprint_number ->
      versioned_rw (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Blueprint.nb_chunks
                ~storage_version
                ~blueprint_number;
            decode =
              infallible_decode (fun bytes ->
                  Helpers.decode_z_le bytes |> Z.to_int);
            encode = (fun n -> Z.to_bits (Z.of_int n));
          })
  | Blueprint_generation blueprint_number ->
      versioned_rw (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Blueprint.generation
                ~storage_version
                ~blueprint_number;
            decode = infallible_decode Ethereum_types.decode_number_le;
            encode =
              (fun qty -> Bytes.to_string (Ethereum_types.encode_u256_le qty));
          })
  | Single_tx_input ->
      versioned_rw (fun ~storage_version ->
          rlp_codec
            ~path:(Durable_storage_path.Single_tx.input_tx ~storage_version))
  | Assemble_block_input ->
      versioned_rw (fun ~storage_version ->
          rlp_codec
            ~path:(Durable_storage_path.Assemble_block.input ~storage_version))
  | Current_block chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_ro
        (block_ro_codec
           ~path:(Durable_storage_path.Block.current_block ~root)
           ~chain_family)
  | Block_by_hash (chain_family, block_hash) ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_read_delete
        {
          path = Durable_storage_path.Block.by_hash ~root block_hash;
          decode =
            (fun bytes -> Ok (L2_types.block_from_bytes ~chain_family bytes));
        }
  | Block_index (chain_family, block_number) ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      Versioned
        (fun ~storage_version ->
          Delete_only
            {
              path =
                Durable_storage_path.Indexes.block_by_number
                  ~storage_version
                  ~root
                  block_number;
            })
  | Tezosx_tezos_current_block ->
      static_ro
        (block_ro_codec
           ~path:
             (Durable_storage_path.Block.current_block
                ~root:Durable_storage_path.tezosx_tezos_blocks_root)
           ~chain_family:Michelson)
  | Evm_legacy_block_by_hash block_hash ->
      static_ro
        {
          path =
            Durable_storage_path.Block.by_hash
              ~root:Durable_storage_path.etherlink_root
              block_hash;
          decode = infallible_decode Ethereum_types.block_from_rlp;
        }
  | Evm_legacy_current_block ->
      static_ro
        {
          path =
            Durable_storage_path.Block.current_block
              ~root:Durable_storage_path.etherlink_root;
          decode = infallible_decode Ethereum_types.block_from_rlp;
        }
  | Current_receipts ->
      static_ro
        {
          path =
            Durable_storage_path.Block.current_receipts
              ~root:Durable_storage_path.etherlink_safe_root;
          decode =
            infallible_decode
              (Transaction_receipt.decode_last_from_list
                 Ethereum_types.(Block_hash (Hex (String.make 64 '0'))));
        }
  | Backlog -> static_ro (int64_le_ro_codec ~path:Durable_storage_path.backlog)
  | Minimum_base_fee_per_gas ->
      static_ro
        {
          path = Durable_storage_path.minimum_base_fee_per_gas;
          decode = infallible_decode Helpers.decode_z_le;
        }
  | Da_fee_per_byte ->
      static_ro (qty_le_ro_codec ~path:Durable_storage_path.da_fee_per_byte)
  | Maximum_gas_per_transaction ->
      versioned_ro (fun ~storage_version ->
          qty_le_ro_codec
            ~path:
              (Durable_storage_path.maximum_gas_per_transaction
                 ~storage_version))
  | Michelson_to_evm_gas_multiplier ->
      static_ro
        (int64_le_ro_codec
           ~path:Durable_storage_path.michelson_to_evm_gas_multiplier)
  | Sequencer_pool_address ->
      versioned_ro (fun ~storage_version ->
          {
            path = Durable_storage_path.sequencer_pool_address ~storage_version;
            decode = infallible_decode Ethereum_types.decode_address;
          })
  | Evm_legacy_account_balance address ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.Accounts.balance ~storage_version address;
            decode = infallible_decode Ethereum_types.decode_number_le;
            encode =
              (fun q -> Bytes.to_string (Ethereum_types.encode_u256_le q));
          })
  | Evm_legacy_account_nonce address ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.Accounts.nonce ~storage_version address;
            decode = infallible_decode Ethereum_types.decode_number_le;
            encode = (fun q -> Bytes.to_string (Ethereum_types.encode_u64_le q));
          })
  | Evm_legacy_account_code address ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.Accounts.code ~storage_version address;
            decode =
              infallible_decode (fun bytes ->
                  Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hex_of_string);
            encode = Ethereum_types.hex_to_bytes;
          })
  | Evm_legacy_account_code_hash address ->
      versioned_ro (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Accounts.code_hash ~storage_version address;
            decode =
              infallible_decode (fun bytes ->
                  Hex.of_bytes bytes |> Hex.show
                  |> Ethereum_types.hash_of_string);
          })
  | Evm_code_by_hash hash ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.Code.code ~storage_version hash;
            decode =
              infallible_decode (fun bytes ->
                  Hex.of_bytes bytes |> Hex.show |> Ethereum_types.hex_of_string);
            encode = Ethereum_types.hex_to_bytes;
          })
  | Evm_account_storage (addr, idx) ->
      static_rw
        {
          path = Durable_storage_path.Accounts.storage addr idx;
          decode =
            infallible_decode (fun bytes ->
                Bytes.to_string bytes |> Hex.of_string |> Hex.show
                |> Ethereum_types.hex_of_string);
          encode = Ethereum_types.hex_to_bytes;
        }
  | Evm_account_info address ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.Accounts.info ~storage_version address;
            decode = EVM_account_info.decode;
            encode =
              (fun info -> Bytes.to_string (EVM_account_info.encode info));
          })
  | Tezos_account_info pkh ->
      static_ro
        {
          path = Tezosx.Durable_storage_path.Accounts.Tezos.info pkh;
          decode = Tezosx.Tezos_runtime.decode_account_info;
        }
  | Evm_block_hash_by_number number ->
      versioned_ro (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Indexes.block_by_number
                ~storage_version
                ~root:Durable_storage_path.etherlink_root
                number;
            decode = infallible_decode Ethereum_types.decode_block_hash;
          })
  | Evm_transaction_receipt_by_hash (tx_hash, block_hash) ->
      static_ro
        {
          path = Durable_storage_path.Transaction_receipt.receipt tx_hash;
          decode =
            infallible_decode (Transaction_receipt.of_rlp_bytes block_hash);
        }
  | Evm_transaction_object_by_hash (tx_hash, block_hash) ->
      static_ro
        {
          path = Durable_storage_path.Transaction_object.object_ tx_hash;
          decode =
            infallible_decode
              (Ethereum_types.legacy_transaction_object_from_rlp block_hash);
        }
  | Evm_current_block_receipts block_hash ->
      static_ro
        {
          path =
            Durable_storage_path.Block.current_receipts
              ~root:Durable_storage_path.etherlink_root;
          decode =
            infallible_decode (fun bytes ->
                match Rlp.decode bytes with
                | Ok (Rlp.List receipts_rlp) ->
                    List.map
                      (fun rlp ->
                        Transaction_receipt.of_rlp_item block_hash rlp)
                      receipts_rlp
                | _ ->
                    raise
                      (Invalid_argument "Transaction receipts should be a list"));
        }
  | Evm_current_block_transactions_objects block_hash ->
      static_ro
        {
          path =
            Durable_storage_path.Block.current_transactions_objects
              ~root:Durable_storage_path.etherlink_root;
          decode =
            infallible_decode (fun bytes ->
                match Rlp.decode bytes with
                | Ok (Rlp.List objects_rlp) ->
                    List.map
                      (fun rlp ->
                        Ethereum_types.legacy_transaction_object_from_rlp_item
                          (Some block_hash)
                          rlp)
                      objects_rlp
                | _ ->
                    raise
                      (Invalid_argument "Transaction objects should be a list"));
        }
  | Tezos_contract_storage contract ->
      static_ro
        {
          path = Durable_storage_path.michelson_contract_storage contract;
          decode =
            safe_binary_decode
              Tezlink_imports.Imported_context.Script.expr_encoding;
        }
  | Tezos_contract_code contract ->
      static_ro
        {
          path = Durable_storage_path.michelson_contract_code contract;
          decode =
            safe_binary_decode
              Tezlink_imports.Imported_context.Script.expr_encoding;
        }
  | Tezos_big_map_value (id, key_hash) ->
      let raw_hash =
        Tezlink_imports.Imported_protocol.Script_expr_hash.to_bytes key_hash
      in
      let (`Hex key_hex) = Hex.of_bytes raw_hash in
      static_ro
        {
          path =
            Durable_storage_path.tezos_big_map_root ^ "/"
            ^ Z.to_string
                (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
            ^ "/" ^ key_hex;
          decode =
            safe_binary_decode
              Tezlink_imports.Imported_context.Script.expr_encoding;
        }
  | Tezos_big_map_key_type id ->
      static_ro
        {
          path =
            Durable_storage_path.tezos_big_map_root ^ "/"
            ^ Z.to_string
                (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
            ^ "/key_type";
          decode =
            safe_binary_decode
              Tezlink_imports.Imported_context.Script.expr_encoding;
        }
  | Tezos_big_map_value_type id ->
      static_ro
        {
          path =
            Durable_storage_path.tezos_big_map_root ^ "/"
            ^ Z.to_string
                (Tezlink_imports.Imported_context.Big_map.Id.unparse_to_z id)
            ^ "/value_type";
          decode =
            safe_binary_decode
              Tezlink_imports.Imported_context.Script.expr_encoding;
        }
  | Tezlink_balance contract ->
      static_ro
        {
          path = Durable_storage_path.michelson_contract_balance contract;
          decode =
            infallible_decode
              (Data_encoding.Binary.of_bytes_exn Tezos_types.Tez.encoding);
        }
  | Tezlink_manager contract ->
      static_ro
        {
          path = Durable_storage_path.michelson_contract_manager contract;
          decode =
            infallible_decode
              (Data_encoding.Binary.of_bytes_exn Tezos_types.Manager.encoding);
        }
  | Tezlink_counter contract ->
      static_ro
        {
          path = Durable_storage_path.michelson_contract_counter contract;
          decode =
            infallible_decode
              (Data_encoding.Binary.of_bytes_exn Data_encoding.n);
        }
  | Tezlink_block_by_hash block_hash ->
      static_ro
        {
          path =
            Durable_storage_path.Block.by_hash
              ~root:Durable_storage_path.tezosx_tezos_blocks_root
              block_hash;
          decode = infallible_decode L2_types.Tezos_block.block_from_kernel;
        }
  | Tezlink_block_hash_by_number number ->
      versioned_ro (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Indexes.block_by_number
                ~storage_version
                ~root:Durable_storage_path.tezosx_tezos_blocks_root
                number;
            decode = infallible_decode Ethereum_types.decode_block_hash;
          })
  | Blueprint_current_generation ->
      versioned_ro (fun ~storage_version ->
          {
            path =
              Durable_storage_path.Blueprint.current_generation ~storage_version;
            decode = infallible_decode Ethereum_types.decode_number_le;
          })
  | Kernel_boot_wasm ->
      static_rw
        {
          path = Durable_storage_path.kernel_boot_wasm;
          decode = infallible_decode Fun.id;
          encode = Bytes.to_string;
        }
  | Kernel_verbosity ->
      versioned_rw (fun ~storage_version ->
          {
            path = Durable_storage_path.kernel_verbosity ~storage_version;
            decode = infallible_decode Bytes.to_string;
            encode = Fun.id;
          })

let storage_version state =
  let open Lwt_result_syntax in
  let decode bytes = Helpers.decode_z_le bytes |> Z.to_int in
  let*! bytes =
    inspect_durable state Durable_storage_path.storage_version_base
  in
  match bytes with
  | Some bytes -> return (decode bytes)
  | None -> (
      let*! bytes =
        inspect_durable state Durable_storage_path.storage_version_legacy
      in
      match bytes with Some bytes -> return (decode bytes) | None -> return 0)

let resolve_with_state : type a cap.
    ?sv:int ->
    (a, cap) path ->
    Pvm.State.t ->
    (int option * (a, cap) resolved) tzresult Lwt.t =
 fun ?sv p state ->
  let open Lwt_result_syntax in
  match resolve p with
  | Static r -> return (sv, r)
  | Versioned f ->
      let* sv =
        match sv with Some sv -> return sv | None -> storage_version state
      in
      return (Some sv, f ~storage_version:sv)

let path_and_decode : type a cap.
    (a, cap) readable_resolved -> string * (bytes -> a tzresult) = function
  | Read_write {path; decode; _} -> (path, decode)
  | Read_only {path; decode} -> (path, decode)
  | Read_delete {path; decode} -> (path, decode)

let read_path_and_decode : type a.
    (a, [> `Read]) resolved -> string * (bytes -> a tzresult) = function
  | Readable rr -> path_and_decode rr

let read (type a) (p : (a, [> `Read]) path) (state : Pvm.State.t) :
    a tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* _sv, r = resolve_with_state p state in
  let path, decode = read_path_and_decode r in
  let*! bytes_opt = inspect_durable state path in
  match bytes_opt with
  | Some bytes ->
      let*? v = decode bytes in
      return v
  | None -> failwith "No value found under %s" path

let read_opt (type a) (p : (a, [> `Read]) path) (state : Pvm.State.t) :
    a option tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* _sv, r = resolve_with_state p state in
  let path, decode = read_path_and_decode r in
  let*! bytes_opt = inspect_durable state path in
  match bytes_opt with
  | Some bytes ->
      let*? v = decode bytes in
      return_some v
  | None -> return_none

let read_or_default (type a) ~(default : a) (p : (a, [> `Read]) path)
    (state : Pvm.State.t) : a tzresult Lwt.t =
  let open Lwt_result_syntax in
  let+ v_opt = read_opt p state in
  Option.value v_opt ~default

let write : type a.
    (a, [> `Write]) path -> a -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t =
 fun p value state ->
  let open Lwt_result_syntax in
  let* _sv, Readable (Read_write {path; encode; _}) =
    resolve_with_state p state
  in
  let*! state = modify_durable ~key:path ~value:(encode value) state in
  return state

let delete (type a) (p : (a, [> `Delete]) path) (state : Pvm.State.t) :
    Pvm.State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* _sv, r = resolve_with_state p state in
  let*! state =
    delete_durable ~kind:Tezos_scoru_wasm.Durable.Value state (path_of r)
  in
  return state

let exists : ('a, 'cap) path -> Pvm.State.t -> bool tzresult Lwt.t =
 fun p state ->
  let open Lwt_result_syntax in
  let* _sv, r = resolve_with_state p state in
  let*! b = exists_durable state (path_of r) in
  return b

let write_all : type a.
    ((a, [> `Write]) path * a) list -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t
    =
 fun pairs state ->
  let open Lwt_result_syntax in
  let* _sv, state =
    List.fold_left_es
      (fun (sv, state) (p, value) ->
        let* sv, Readable (Read_write {path; encode; _}) =
          resolve_with_state ?sv p state
        in
        let*! state = modify_durable ~key:path ~value:(encode value) state in
        return (sv, state))
      (None, state)
      pairs
  in
  return state

let list_runtimes state =
  let open Lwt_result_syntax in
  let check_runtime r =
    let* enabled = read_opt (Tezosx_feature_flag r) state in
    if Option.is_some enabled then return @@ Some r else return None
  in
  List.filter_map_ep check_runtime Tezosx.known_runtimes

exception Invalid_block_structure of string

type dir =
  | Raw_dir of string
  | Delayed_inbox
  | Delayed_transactions
  | Evm_events
  | Transaction_receipts
  | Transaction_objects
  | Michelson_runtime_contracts_index
  | Michelson_runtime_ledger
  | Evm_account_storage_dir of Durable_storage_path.Accounts.fixed_address

type resolved_dir =
  | Static_dir of string
  | Versioned_dir of (storage_version:int -> string)

let resolve_dir : dir -> resolved_dir = function
  | Raw_dir str -> Static_dir str
  | Delayed_inbox -> Versioned_dir Durable_storage_path.delayed_inbox
  | Delayed_transactions ->
      Versioned_dir Durable_storage_path.Delayed_transaction.hashes
  | Evm_events -> Versioned_dir Durable_storage_path.Evm_events.events
  | Transaction_receipts ->
      Static_dir Durable_storage_path.Transaction_receipt.receipts
  | Transaction_objects ->
      Static_dir Durable_storage_path.Transaction_object.objects
  | Michelson_runtime_contracts_index ->
      Static_dir Durable_storage_path.michelson_contracts_index
  | Michelson_runtime_ledger ->
      Static_dir Durable_storage_path.michelson_ledger_root
  | Evm_account_storage_dir addr ->
      Static_dir (Durable_storage_path.Accounts.storage_dir addr)

let resolve_dir_with_state (p : dir) (state : Pvm.State.t) :
    string tzresult Lwt.t =
  let open Lwt_result_syntax in
  match resolve_dir p with
  | Static_dir r -> return r
  | Versioned_dir f ->
      let* sv = storage_version state in
      return (f ~storage_version:sv)

let delete_dir (p : dir) (state : Pvm.State.t) : Pvm.State.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_dir_with_state p state in
  let*! state =
    delete_durable ~kind:Tezos_scoru_wasm.Durable.Directory state r
  in
  return state

let exists_dir (p : dir) (state : Pvm.State.t) : bool tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_dir_with_state p state in
  let*! b = exists_dir_durable state r in
  return b

let subkeys (p : dir) (state : Pvm.State.t) : string trace tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* r = resolve_dir_with_state p state in
  let*! keys = subkeys_durable state r in
  return keys
