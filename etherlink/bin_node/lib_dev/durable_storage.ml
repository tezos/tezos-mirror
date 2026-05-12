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

(* Typed path GADT — see [.mli] for the capability semantics. *)

(** Phantom capability markers for [path]: [rw] is read+write+delete, [ro] is
    read-only. *)
type rw = [`Read | `Write | `Delete]

type ro = [`Read]

type ('a, 'cap) path =
  | Raw_path : string -> (bytes, rw) path
  | Chain_id : (L2_types.chain_id, rw) path
  | Michelson_runtime_chain_id : (L2_types.chain_id, rw) path
  | Kernel_version : (string, rw) path
  | Kernel_root_hash : (Ethereum_types.hex, rw) path
  | Multichain_flag : (unit, rw) path
  | Sequencer_key : (Signature.Public_key.t, rw) path
  | Chain_config_family :
      L2_types.chain_id
      -> (L2_types.ex_chain_family, rw) path
  | Tezosx_feature_flag : Tezosx.runtime -> (unit, rw) path
  | Michelson_runtime_sunrise_level : (Ethereum_types.quantity, rw) path
  | Current_block_number :
      _ L2_types.chain_family
      -> (Ethereum_types.quantity, rw) path
  | Current_block_hash :
      _ L2_types.chain_family
      -> (Ethereum_types.block_hash, rw) path
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
           [`Read | `Delete] )
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

(** Read-capable resolution variants. [Delete_only] is intentionally absent
    here — it lives directly in {!resolved}, so [path_and_decode] can
    pattern-match exhaustively without an [assert false] on an impossible
    case. *)
type ('a, 'cap) read_resolved =
  | Read_write : {
      path : string;
      decode : bytes -> 'a tzresult;
      encode : 'a -> string;
    }
      -> ('a, rw) read_resolved
  | Read_only : {
      path : string;
      decode : bytes -> 'a tzresult;
    }
      -> ('a, ro) read_resolved
  | Read_delete : {
      path : string;
      decode : bytes -> 'a tzresult;
    }
      -> ('a, [`Read | `Delete]) read_resolved

(** A resolved path: either read-capable (and optionally writable / deletable),
    or delete-only. *)
type ('a, 'cap) resolved =
  | Readable : ('a, 'cap) read_resolved -> ('a, 'cap) resolved
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

(** Empty-body presence flags: the value on disk is an (empty) marker; we only
    care about existence. *)
let unit_flag_codec ~path : (unit, rw) read_resolved =
  Read_write {path; decode = (fun _bytes -> Ok ()); encode = (fun () -> "")}

(** Little-endian [int64]-backed kernel-owned scalars stored via
    [Data_encoding.Little_endian.int64]. Read-only — the EVM node never
    writes these. *)
let int64_le_ro_codec ~path : (int64, ro) read_resolved =
  Read_only
    {
      path;
      decode =
        infallible_decode
          Data_encoding.(Binary.of_bytes_exn Little_endian.int64);
    }

(** Little-endian [Z.t]-backed quantities stored via [Z.{to,of}_bits]. *)
let qty_le_codec ~path : (Ethereum_types.quantity, rw) read_resolved =
  Read_write
    {
      path;
      decode =
        infallible_decode (fun bytes ->
            Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits));
      encode = (fun (Ethereum_types.Qty z) -> Z.to_bits z);
    }

(** Read-only sibling of {!qty_le_codec} for kernel-owned quantity scalars
    that the EVM node never writes. *)
let qty_le_ro_codec ~path : (Ethereum_types.quantity, ro) read_resolved =
  Read_only
    {
      path;
      decode =
        infallible_decode (fun bytes ->
            Ethereum_types.Qty (Bytes.to_string bytes |> Z.of_bits));
    }

(** RLP-encoded values stored as bytes. *)
let rlp_codec ~path : (Rlp.item, rw) read_resolved =
  Read_write
    {
      path;
      decode = Rlp.decode;
      encode = (fun item -> Bytes.to_string (Rlp.encode item));
    }

(** Kernel-owned typed block stored under [path]; read-only, decoded via
    [L2_types.block_from_bytes] for the given [chain_family]. *)
let block_ro_codec (type f) ~path ~(chain_family : f L2_types.chain_family) :
    (Ethereum_types.legacy_transaction_object L2_types.block, ro) read_resolved
    =
  Read_only
    {
      path;
      decode = (fun bytes -> Ok (L2_types.block_from_bytes ~chain_family bytes));
    }

(** Smart constructors for [resolve] arms — wrap a [read_resolved]
    into a [resolution], hiding the [Readable] layer that every read-side
    arm would otherwise have to spell out. *)
let static_read : type a cap. (a, cap) read_resolved -> (a, cap) resolution =
 fun r -> Static (Readable r)

let versioned_read : type a cap.
    (storage_version:int -> (a, cap) read_resolved) -> (a, cap) resolution =
 fun f -> Versioned (fun ~storage_version -> Readable (f ~storage_version))

let resolve : type a cap. (a, cap) path -> (a, cap) resolution = function
  | Raw_path key ->
      static_read
        (Read_write
           {
             path = key;
             decode = infallible_decode Fun.id;
             encode = Bytes.to_string;
           })
  | Chain_id ->
      static_read
        (Read_write
           {
             path = Durable_storage_path.chain_id;
             decode = infallible_decode L2_types.Chain_id.decode_le;
             encode = L2_types.Chain_id.encode_le;
           })
  | Michelson_runtime_chain_id ->
      static_read
        (Read_write
           {
             path = Durable_storage_path.michelson_runtime_chain_id;
             decode = infallible_decode L2_types.Chain_id.decode_be;
             encode = L2_types.Chain_id.encode_be;
           })
  | Kernel_version ->
      versioned_read (fun ~storage_version ->
          Read_write
            {
              path = Durable_storage_path.kernel_version ~storage_version;
              decode = infallible_decode Bytes.to_string;
              encode = Fun.id;
            })
  | Kernel_root_hash ->
      versioned_read (fun ~storage_version ->
          Read_write
            {
              path = Durable_storage_path.kernel_root_hash ~storage_version;
              decode =
                (fun bytes ->
                  let (`Hex s) = Hex.of_bytes bytes in
                  Ok (Ethereum_types.Hex s));
              encode = Ethereum_types.hex_to_string;
            })
  | Multichain_flag ->
      static_read
        (unit_flag_codec ~path:Durable_storage_path.Feature_flags.multichain)
  | Sequencer_key ->
      versioned_read (fun ~storage_version ->
          Read_write
            {
              path = Durable_storage_path.sequencer_key ~storage_version;
              decode =
                (fun bytes ->
                  Signature.Public_key.of_b58check (Bytes.to_string bytes));
              encode = (fun pk -> Signature.Public_key.to_b58check pk);
            })
  | Chain_config_family cid ->
      versioned_read (fun ~storage_version ->
          Read_write
            {
              path =
                Durable_storage_path.Chain_configuration.chain_family
                  ~storage_version
                  cid;
              decode = L2_types.Chain_family.of_bytes;
              encode =
                (fun (L2_types.Ex_chain_family cf) ->
                  L2_types.Chain_family.to_string cf);
            })
  | Tezosx_feature_flag runtime ->
      static_read (unit_flag_codec ~path:(Tezosx.feature_flag runtime))
  | Michelson_runtime_sunrise_level ->
      versioned_read (fun ~storage_version ->
          qty_le_codec
            ~path:
              (Durable_storage_path.michelson_runtime_sunrise_level
                 ~storage_version))
  | Current_block_number chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_read
        (qty_le_codec ~path:(Durable_storage_path.Block.current_number ~root))
  | Current_block_hash chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_read
        (Read_write
           {
             path = Durable_storage_path.Block.current_hash ~root;
             decode = infallible_decode Ethereum_types.decode_block_hash;
             encode =
               (fun h -> Bytes.to_string (Ethereum_types.encode_block_hash h));
           })
  | Evm_node_flag ->
      versioned_read (fun ~storage_version ->
          unit_flag_codec
            ~path:(Durable_storage_path.evm_node_flag ~storage_version))
  | Blueprint_chunk {blueprint_number; chunk_index} ->
      versioned_read (fun ~storage_version ->
          Read_write
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
      versioned_read (fun ~storage_version ->
          Read_write
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
      versioned_read (fun ~storage_version ->
          Read_write
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
      versioned_read (fun ~storage_version ->
          rlp_codec
            ~path:(Durable_storage_path.Single_tx.input_tx ~storage_version))
  | Assemble_block_input ->
      versioned_read (fun ~storage_version ->
          rlp_codec
            ~path:(Durable_storage_path.Assemble_block.input ~storage_version))
  | Current_block chain_family ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_read
        (block_ro_codec
           ~path:(Durable_storage_path.Block.current_block ~root)
           ~chain_family)
  | Block_by_hash (chain_family, block_hash) ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      static_read
        (Read_delete
           {
             path = Durable_storage_path.Block.by_hash ~root block_hash;
             decode =
               (fun bytes -> Ok (L2_types.block_from_bytes ~chain_family bytes));
           })
  | Block_index (chain_family, block_number) ->
      let root = Durable_storage_path.root_of_chain_family chain_family in
      Static
        (Delete_only
           {
             path =
               Durable_storage_path.Indexes.block_by_number ~root block_number;
           })
  | Tezosx_tezos_current_block ->
      static_read
        (block_ro_codec
           ~path:
             (Durable_storage_path.Block.current_block
                ~root:Durable_storage_path.tezosx_tezos_blocks_root)
           ~chain_family:Michelson)
  | Current_receipts ->
      static_read
        (Read_only
           {
             path =
               Durable_storage_path.Block.current_receipts
                 ~root:Durable_storage_path.etherlink_safe_root;
             decode =
               infallible_decode
                 (Transaction_receipt.decode_last_from_list
                    Ethereum_types.(Block_hash (Hex (String.make 64 '0'))));
           })
  | Backlog ->
      static_read (int64_le_ro_codec ~path:Durable_storage_path.backlog)
  | Minimum_base_fee_per_gas ->
      static_read
        (Read_only
           {
             path = Durable_storage_path.minimum_base_fee_per_gas;
             decode = infallible_decode Helpers.decode_z_le;
           })
  | Da_fee_per_byte ->
      static_read (qty_le_ro_codec ~path:Durable_storage_path.da_fee_per_byte)
  | Maximum_gas_per_transaction ->
      static_read
        (qty_le_ro_codec ~path:Durable_storage_path.maximum_gas_per_transaction)
  | Michelson_to_evm_gas_multiplier ->
      static_read
        (int64_le_ro_codec
           ~path:Durable_storage_path.michelson_to_evm_gas_multiplier)
  | Sequencer_pool_address ->
      static_read
        (Read_only
           {
             path = Durable_storage_path.sequencer_pool_address;
             decode = infallible_decode Ethereum_types.decode_address;
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
    (a, cap) read_resolved -> string * (bytes -> a tzresult) = function
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

let inspect_durable_and_decode_opt state path decode =
  let open Lwt_result_syntax in
  let* bytes = read_opt (Raw_path path) state in
  match bytes with
  | Some bytes -> return_some (decode bytes)
  | None -> return_none

let inspect_durable_and_decode_default ~default state path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt state path decode in
  match res_opt with Some res -> return res | None -> return default

let inspect_durable_and_decode state path decode =
  let open Lwt_result_syntax in
  let* res_opt = inspect_durable_and_decode_opt state path decode in
  match res_opt with
  | Some res -> return res
  | None -> failwith "No value found under %s" path

let l2_minimum_base_fee_per_gas state chain_id =
  let open Lwt_result_syntax in
  let* sv = storage_version state in
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.minimum_base_fee_per_gas
       ~storage_version:sv
       chain_id)
    Helpers.decode_z_le

let l2_da_fee_per_byte state chain_id =
  let open Lwt_result_syntax in
  let* sv = storage_version state in
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.da_fee_per_byte
       ~storage_version:sv
       chain_id)
    Helpers.decode_z_le

let l2_maximum_gas_per_transaction state chain_id =
  let open Lwt_result_syntax in
  let* sv = storage_version state in
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.maximum_gas_per_transaction
       ~storage_version:sv
       chain_id)
    Helpers.decode_z_le

let world_state state chain_id =
  let open Lwt_result_syntax in
  let* sv = storage_version state in
  inspect_durable_and_decode
    state
    (Durable_storage_path.Chain_configuration.world_state
       ~storage_version:sv
       chain_id)
    Bytes.to_string

type dir =
  | Raw_dir of string
  | Delayed_inbox
  | Delayed_transactions
  | Evm_events
  | Transaction_receipts
  | Transaction_objects
  | Michelson_runtime_contracts_index
  | Michelson_runtime_ledger

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
