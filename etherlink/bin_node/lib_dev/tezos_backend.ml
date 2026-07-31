(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Micheline = Tezos_micheline.Micheline

type error +=
  | Entrypoints_decode_error of string
  | Run_script_view_decode_error of string
  | Run_script_view_failed of string
  | Run_script_view_host_error of string
  | Run_script_view_unsupported_source of string
  | Kernel_entrypoint_unavailable of {
      entrypoint : string;
      storage_version : int;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezosx.entrypoints_decode_error"
    ~title:"Entrypoints result decode error"
    ~description:
      "Failed to decode the entrypoints result returned by the kernel."
    ~pp:(fun ppf msg -> Format.fprintf ppf "Entrypoints decode error: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Entrypoints_decode_error msg -> Some msg | _ -> None)
    (fun msg -> Entrypoints_decode_error msg) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezosx.run_script_view_decode_error"
    ~title:"run_script_view result decode error"
    ~description:
      "Failed to decode the run_script_view result returned by the kernel."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "run_script_view decode error: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Run_script_view_decode_error msg -> Some msg | _ -> None)
    (fun msg -> Run_script_view_decode_error msg) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezosx.run_script_view_failed"
    ~title:"Michelson view simulation failed"
    ~description:"The simulation of a Michelson view failed."
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Michelson view simulation failed: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Run_script_view_failed msg -> Some msg | _ -> None)
    (fun msg -> Run_script_view_failed msg) ;
  register_error_kind
    (* [`Temporary]: a storage/host failure is infrastructure, not a
       deterministic script rejection, so a client retrying on transient
       errors must be able to tell the two apart. *)
    `Temporary
    ~id:"evm_node.dev.tezosx.run_script_view_host_error"
    ~title:"Michelson view simulation could not read the state"
    ~description:
      "A durable-storage failure prevented the simulation of a Michelson view."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Michelson view simulation could not read the state: %s"
        msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Run_script_view_host_error msg -> Some msg | _ -> None)
    (fun msg -> Run_script_view_host_error msg) ;
  register_error_kind
    (* [`Permanent]: the request itself is unanswerable, retrying cannot
       help. Distinct from [Run_script_view_failed] so a client can tell
       "your request is malformed" from "the view rejected". *)
    `Permanent
    ~id:"evm_node.dev.tezosx.run_script_view_unsupported_source"
    ~title:"run_script_view was given an implicit source"
    ~description:
      "The source of a view call on an enshrined contract must be an \
       originated contract: only an originated contract can execute VIEW."
    ~pp:(fun ppf view ->
      Format.fprintf
        ppf
        "view %s was called on an enshrined contract with an implicit \
         `source`; only an originated contract can execute VIEW"
        view)
    Data_encoding.(obj1 (req "view" string))
    (function
      | Run_script_view_unsupported_source view -> Some view | _ -> None)
    (fun view -> Run_script_view_unsupported_source view) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.tezosx.kernel_entrypoint_unavailable"
    ~title:"Kernel entrypoint unavailable"
    ~description:
      "The kernel running at the requested block does not expose the \
       entrypoint this RPC is served through."
    ~pp:(fun ppf (entrypoint, storage_version) ->
      Format.fprintf
        ppf
        "The kernel at storage version %d does not expose the %s entrypoint"
        storage_version
        entrypoint)
    Data_encoding.(obj2 (req "entrypoint" string) (req "storage_version" int31))
    (function
      | Kernel_entrypoint_unavailable {entrypoint; storage_version} ->
          Some (entrypoint, storage_version)
      | _ -> None)
    (fun (entrypoint, storage_version) ->
      Kernel_entrypoint_unavailable {entrypoint; storage_version})

(* Decode the entrypoints + synthetic-views result written by the
   kernel's tezosx_michelson_entrypoints entrypoint.

   Two RLP shapes are accepted, both wrapped by the canonical-option
   [Some] wrapper from [append_option_canonical] on the kernel side
   (empty list is the canonical [None]):

   - Current shape (kernels from !21936 onward):
       List []                  → None (contract not found)
       List [[entries, views]]  → Some ([], entries, views)
     where
     * [entries] is an RLP list of [name_bytes, micheline_type_bytes]
       pairs;
     * [views] is an RLP list of [name_bytes, parameter_type_bytes,
       return_type_bytes] triples (empty when the contract has no
       synthetic views).

   - Legacy shape (kernels predating !21936):
       List [entries]  → Some ([], entries, [])
     i.e. only the entries list, no views element. Required so the
     EVM node can be upgraded ahead of the rollup kernel upgrade
     that introduces the new shape — without this branch, every
     [/script] call against an enshrined contract on an old kernel
     would surface as a 500.

   The two shapes are distinguished by the structure of the first
   child of the inner list: in the new shape it is itself a list
   (the [entries_list]); in the legacy shape it is the first
   entrypoint pair, whose first element is a [Value name_bytes]. *)
let decode_entrypoints_result bytes =
  let open Lwt_result_syntax in
  let decode_type_bytes type_bytes =
    match
      Data_encoding.Binary.of_bytes_opt
        Tezlink_imports.Imported_context.Script.expr_encoding
        type_bytes
    with
    | None ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Failed to decode Micheline type bytes")
    | Some type_expr -> Result_syntax.return type_expr
  in
  let decode_entry = function
    | Rlp.List [Value name_bytes; Value type_bytes] ->
        let open Result_syntax in
        let name = Bytes.to_string name_bytes in
        let* type_expr = decode_type_bytes type_bytes in
        return (name, type_expr)
    | _ ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Invalid RLP entry format")
  in
  let decode_view = function
    | Rlp.List
        [Value name_bytes; Value param_type_bytes; Value return_type_bytes] ->
        let open Result_syntax in
        let name = Bytes.to_string name_bytes in
        let* param_expr = decode_type_bytes param_type_bytes in
        let* return_expr = decode_type_bytes return_type_bytes in
        return (name, param_expr, return_expr)
    | _ ->
        Result_syntax.tzfail
          (Entrypoints_decode_error "Invalid RLP view triple format")
  in
  let*? rlp = Rlp.decode bytes in
  match rlp with
  | Rlp.List [] -> return_none
  | Rlp.List [Rlp.List inner] -> (
      match inner with
      | [(Rlp.List (Rlp.List _ :: _) as entries_rlp); (Rlp.List _ as views_rlp)]
      | [(Rlp.List [] as entries_rlp); (Rlp.List _ as views_rlp)] ->
          (* New shape: the first child of [inner] is itself a list
             (the entries list, whose children are pairs), so we
             know the second child is the views list. *)
          let*? entries = Rlp.decode_list decode_entry entries_rlp in
          let*? views = Rlp.decode_list decode_view views_rlp in
          (* The kernel does not encode unreachable entrypoints;
             always return an empty list for the unreachable field. *)
          return_some ([], entries, views)
      | _ ->
          (* Legacy shape (`[entries]`): [inner] is the entries list
             itself. No views are reported, matching the pre-!21936
             behaviour. *)
          let*? entries = Rlp.decode_list decode_entry (Rlp.List inner) in
          return_some ([], entries, []))
  | _ ->
      tzfail
        (Entrypoints_decode_error "Invalid RLP structure for entrypoints result")

(* Kernel IPC for `tezosx_run_code`: [input_encoding] mirrors the kernel's
   [RunCodeInput] derives and [run_code_result_encoding] its
   [RunCodeResult] union ([evm_node_entrypoint.rs]), whose unit tests pin
   the wire bytes. *)
module Run_code = struct
  (* [now] and [level] are unbounded on the RPC but the runtime carries
     them as an i64 and a u32; reject out-of-range values instead of
     wrapping. A negative [now] would reach the EVM side as a u64. *)
  let bounded name fits convert z =
    let open Result_syntax in
    if fits z then return (convert z)
    else
      tzfail
        (Run_script_view_failed
           (Format.asprintf "`%s` is out of range: %a" name Z.pp_print z))

  let bounded_int64 name =
    bounded name (fun z -> Z.fits_int64 z && Z.geq z Z.zero) Z.to_int64

  (* The wire carries the raw u32 bits, so [Z.to_int32_unsigned] is the
     right conversion even though the field reads as [int32]. *)
  let bounded_level =
    bounded
      "level"
      (fun z -> Z.fits_int32_unsigned z && Z.geq z Z.zero)
      Z.to_int32_unsigned

  (* Runs on caller-controlled data (the viewer script embeds the caller's
     [input] verbatim); return the failure rather than raising. *)
  let expr_bytes expr =
    match
      Data_encoding.Binary.to_bytes
        Tezlink_imports.Imported_context.Script.expr_encoding
        expr
    with
    | Ok bytes -> Result_syntax.return bytes
    | Error err ->
        Result_syntax.tzfail
          (Run_script_view_decode_error
             (Format.asprintf
                "Cannot encode a Micheline expression: %a"
                Data_encoding.Binary.pp_write_error
                err))

  let input_encoding =
    Data_encoding.(
      merge_objs
        (obj10
           (req "script" bytes)
           (req "storage" bytes)
           (req "input" bytes)
           (req "entrypoint" string)
           (req "chain_id" Chain_id.encoding)
           (req "self" Tezos_types.Contract.encoding)
           (req "amount" int64)
           (opt "sender" Tezos_types.Contract.encoding)
           (opt "payer" Signature.V3.Public_key_hash.encoding)
           (opt "balance" int64))
        (obj3 (opt "gas" int64) (opt "now" int64) (opt "level" int32)))

  let encode_input ~script ~storage ~input ~entrypoint ~chain_id ~self ~amount
      ~sender ~payer ~balance ~gas ~now ~level =
    let open Result_syntax in
    let* gas = Option.map_e (bounded_int64 "gas") gas in
    let* now = Option.map_e (bounded_int64 "now") now in
    let* level = Option.map_e bounded_level level in
    let* script = expr_bytes script in
    let* storage = expr_bytes storage in
    let* input = expr_bytes input in
    match
      Data_encoding.Binary.to_bytes
        input_encoding
        ( ( script,
            storage,
            input,
            entrypoint,
            chain_id,
            self,
            amount,
            sender,
            payer,
            balance ),
          (gas, now, level) )
    with
    | Ok bytes -> return bytes
    | Error err ->
        tzfail
          (Run_script_view_decode_error
             (Format.asprintf
                "Cannot encode the run_code input: %a"
                Data_encoding.Binary.pp_write_error
                err))

  type run_code_result =
    | Success of bytes
    | Execution_error of string
    | Host_error of string

  (* An unknown tag fails decoding outright: a future kernel-side variant
     must not be misread as a permanent script rejection. *)
  let run_code_result_encoding =
    Data_encoding.(
      union
        [
          case
            (Tag 0)
            ~title:"success"
            (obj1 (req "storage" bytes))
            (function Success storage -> Some storage | _ -> None)
            (fun storage -> Success storage);
          case
            (Tag 1)
            ~title:"execution_error"
            string
            (function Execution_error msg -> Some msg | _ -> None)
            (fun msg -> Execution_error msg);
          case
            (Tag 2)
            ~title:"host_error"
            string
            (function Host_error msg -> Some msg | _ -> None)
            (fun msg -> Host_error msg);
        ])

  let decode_result bytes =
    let open Result_syntax in
    let* result =
      match Data_encoding.Binary.of_bytes run_code_result_encoding bytes with
      | Ok result -> return result
      | Error err ->
          tzfail
            (Run_script_view_decode_error
               (Format.asprintf
                  "Invalid run_code result: %a"
                  Data_encoding.Binary.pp_read_error
                  err))
    in
    match result with
    | Execution_error msg -> tzfail (Run_script_view_failed msg)
    | Host_error msg -> tzfail (Run_script_view_host_error msg)
    | Success storage -> (
        match
          Data_encoding.Binary.of_bytes_opt
            Tezlink_imports.Imported_context.Script.expr_encoding
            storage
        with
        | Some expr -> return expr
        | None ->
            tzfail
              (Run_script_view_decode_error
                 "Failed to decode the resulting storage as Micheline"))

  (* A mocked protocol context for the plugin functions borrowed from L1.
     **Only functions that never read the context's contents may run
     against it** — it is a genesis context, so anything consulting real
     state would answer plausibly and wrongly, not fail. Today's users
     ([script_view_type], [parse_ty], [normalize_data]) only charge gas,
     unlimited here. Forced once: building it is expensive and context
     values are functional. *)
  let dummy_context = lazy (Tezlink_mock.init_dummy_context ())

  let normalize ~unparsing_mode ~output_type value =
    let open Lwt_result_syntax in
    match unparsing_mode with
    | Tezlink_imports.Imported_protocol.Script_ir_unparser.Optimized_legacy ->
        (* The kernel's native output form: normalizing to it is the
           identity for every constructor, so skip the context entirely. *)
        return value
    | Readable | Optimized ->
        let* ctxt = Lazy.force dummy_context in
        let*? Tezlink_imports.Imported_protocol.Script_typed_ir.Ex_ty ty, ctxt =
          Tezlink_imports.Imported_env.wrap_tzresult
          @@ Tezlink_imports.Imported_protocol.Script_ir_translator.parse_ty
               ctxt
               ~legacy:true
               ~allow_lazy_storage:true
               ~allow_operation:false
               ~allow_contract:true
               ~allow_ticket:true
               (Micheline.root output_type)
        in
        let normalized =
          Tezlink_imports.Imported_protocol_plugin.RPC.Scripts.Normalize_data
          .normalize_data
            ~unparsing_mode
            ty
            ctxt
            (Micheline.root value)
        in
        return (Micheline.strip_locations normalized)
end

let make (ctxt : Evm_ro_context.t) =
  (module struct
    type block_param =
      [ `Head of int32
      | `Level of int32
      | `Hash of Ethereum_types.block_hash * int32 ]

    (* Same as [Evm_ro_context.execute_entrypoint], but refuses upfront when
       the kernel that produced [state] is too old to export [entrypoint]:
       the call would otherwise leave the IPC result path empty and be
       reported as an opaque failure. *)
    let execute_kernel_entrypoint state ~available ~input_path ~input
        ~output_path ~entrypoint =
      let open Lwt_result_syntax in
      let* storage_version = Durable_storage.storage_version state in
      if not (available ~storage_version) then
        tzfail (Kernel_entrypoint_unavailable {entrypoint; storage_version})
      else
        Evm_ro_context.execute_entrypoint
          ctxt
          state
          ~input_path
          ~input
          ~output_path
          ~entrypoint

    let shell_block_param_to_block_number =
      let open Lwt_result_syntax in
      let compute_offset (Ethereum_types.Qty block_number) offset =
        let result = Int32.sub (Z.to_int32 block_number) offset in
        return (max 0l result)
      in
      function
      | `Head offset ->
          let* current_block_number =
            Evm_ro_context.block_param_to_block_number
              ctxt
              ~chain_family:L2_types.Michelson
              ~hash_column:`Michelson
              (Ethereum_types.Block_parameter.Block_parameter Latest)
          in
          compute_offset current_block_number offset
      | `Hash (hash, offset) ->
          let* current_block_number =
            Evm_ro_context.block_param_to_block_number
              ctxt
              ~chain_family:L2_types.Michelson
              ~hash_column:`Michelson
              (Ethereum_types.Block_parameter.Block_hash
                 {hash; require_canonical = false})
          in
          compute_offset current_block_number offset
      | `Level l -> return l

    let shell_block_param_to_eth_block_param =
      let open Lwt_result_syntax in
      function
      | `Head 0l ->
          return
          @@ Ethereum_types.Block_parameter.Block_parameter
               Ethereum_types.Block_parameter.Latest
      | block ->
          let* num = shell_block_param_to_block_number block in
          return
          @@ Ethereum_types.Block_parameter.Block_parameter
               (Number (Ethereum_types.quantity_of_z (Z.of_int32 num)))

    let on_implicit_account (c : Tezos_types.Contract.t) k =
      match c with
      | Implicit pkh -> k pkh
      | Originated _ -> failwith "Only implicit account are supported"

    let constants chain (_block : block_param) =
      let open Lwt_result_syntax in
      let `Main = chain in
      return
        (Tezlink_constants.all_constants
           ?hard_gas_limit_per_block:ctxt.michelson_hard_gas_limit_per_block
           ())

    let current_level chain block ~offset =
      let open Lwt_result_syntax in
      let `Main = chain in

      let* offset =
        (* Tezos l1 requires non-negative offset #7845 *)
        if offset >= 0l then return offset
        else failwith "The specified level offset should be positive."
      in

      let* block_number = shell_block_param_to_block_number block in

      let* constants = constants chain block in
      let level = Int32.add block_number offset in
      return
        Tezos_types.
          {
            level;
            cycle =
              Int32.(div (sub level 1l) constants.parametric.blocks_per_cycle);
            cycle_position =
              Int32.(rem (sub level 1l) constants.parametric.blocks_per_cycle);
          }

    let balance _chain block contract =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Tezlink_durable_storage.balance state contract

    let subkeys ~block p =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Evm_ro_context.subkeys state p

    let list_contracts chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* contracts_keys = subkeys ~block Michelson_runtime_contracts_index in
      let contracts =
        List.filter_map
          (fun k ->
            let open Option_syntax in
            let* bytes = Hex.to_string (`Hex k) in
            Data_encoding.Binary.of_string_opt
              Tezos_types.Contract.encoding
              bytes)
          contracts_keys
      in
      let* accounts_keys = subkeys ~block Michelson_runtime_ledger in
      let accounts =
        List.filter_map
          (fun k -> Result.to_option @@ Tezos_types.Contract.of_b58check k)
          accounts_keys
      in
      return (accounts @ contracts)

    let get_storage chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Durable_storage.read_opt (Tezos_contract_storage c) state

    let get_code chain block c =
      let open Lwt_result_syntax in
      (* TODO: #7986
         Support unparsing_mode argument. *)
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      (* Resolves a code-less alias to the shared implementation, so [/script]
         and [/entrypoints] match the kernel. A genuinely code-less,
         non-alias account (e.g. an enshrined contract) stays [None] and is
         handled by the kernel-entrypoints fall-through in [get_script]. *)
      Durable_storage.read_contract_code c state

    (* The stub script of a code-less originated contract (an enshrined
       TezosX contract): unit storage and FAILWITH code, carrying the
       entrypoints and synthetic views returned by the kernel. [None] if
       the kernel does not know the contract either. *)
    let enshrined_script c state =
      let open Lwt_result_syntax in
      let addr_bytes =
        Data_encoding.Binary.to_bytes_exn Tezos_types.Contract.encoding c
      in
      let* bytes =
        execute_kernel_entrypoint
          state
          ~available:Storage_version.tezosx_michelson_entrypoints
          ~input_path:Durable_storage_path.Tezosx_entrypoints.input
          ~input:addr_bytes
          ~output_path:Durable_storage_path.Tezosx_entrypoints.result
          ~entrypoint:"tezosx_michelson_entrypoints"
      in
      let* result = decode_entrypoints_result bytes in
      match result with
      | None -> return_none
      | Some (_, entries, views) ->
          return_some (Tezlink_mock.script_of_metadata ~views entries)

    let get_script chain block c =
      let open Lwt_result_syntax in
      let* code = get_code chain block c in
      match code with
      | Some code -> (
          let* storage = get_storage chain block c in
          match storage with
          | Some storage ->
              return_some
                Tezlink_imports.Imported_context.Script.
                  {code = lazy_expr code; storage = lazy_expr storage}
          | None -> return_none)
      | None -> (
          match c with
          | Implicit _ -> return_none
          | Originated _ ->
              let* eth_block = shell_block_param_to_eth_block_param block in
              let* state = Evm_ro_context.get_state ctxt ~block:eth_block () in
              enshrined_script c state)

    let manager_key _chain block contract =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      on_implicit_account contract @@ fun pkh ->
      let* info_opt = Durable_storage.read_opt (Tezos_account_info pkh) state in
      match info_opt with
      | Some info -> return info.public_key
      | None ->
          (* An implicit account absent from durable storage is treated
             as unrevealed, matching L1's `manager_key` RPC which returns
             null for any unrevealed implicit account. *)
          return_none

    let counter _chain block (contract : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      match contract with
      | Originated _ -> return_none
      | Implicit pkh -> (
          let* info_opt =
            Durable_storage.read_opt (Tezos_account_info pkh) state
          in
          match info_opt with
          | Some info -> return_some (Z.of_int64 info.nonce)
          | None -> return_none)

    let used_storage_space _chain block (contract : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      match contract with
      | Implicit _ -> return_none
      | Originated _ ->
          let* block = shell_block_param_to_eth_block_param block in
          let* state = Evm_ro_context.get_state ctxt ~block () in
          let* used =
            Durable_storage.michelson_contract_used_storage_space contract state
          in
          return_some used

    let paid_storage_space _chain block (contract : Tezos_types.Contract.t) =
      let open Lwt_result_syntax in
      match contract with
      | Implicit _ -> return_none
      | Originated _ ->
          let* block = shell_block_param_to_eth_block_param block in
          let* state = Evm_ro_context.get_state ctxt ~block () in
          let* paid =
            Durable_storage.michelson_contract_paid_storage_space contract state
          in
          return_some paid

    let address_registry_index _chain block destination =
      let open Lwt_result_syntax in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Tezlink_durable_storage.address_registry_index state destination

    let big_map_get chain block id key_hash =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      Durable_storage.read_opt (Tezos_big_map_value (id, key_hash)) state

    let big_map_key_type state id =
      Durable_storage.read_opt (Tezos_big_map_key_type id) state

    let big_map_value_type state id =
      Durable_storage.read_opt (Tezos_big_map_value_type id) state

    let big_map_raw_info chain block id =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* block = shell_block_param_to_eth_block_param block in
      let* state = Evm_ro_context.get_state ctxt ~block () in
      let* key_type = big_map_key_type state id in
      let* value_type = big_map_value_type state id in
      let* total_bytes =
        Durable_storage.read_opt (Tezos_big_map_total_bytes id) state
      in
      match (key_type, value_type) with
      | Some kt, Some vt ->
          (* [total_bytes] defaults to zero when the counter path is absent,
             which only happens for a big-map predating the kernel-side counter
             and not written since (the kernel migrates it lazily on write; this
             read-only RPC cannot trigger that migration).
             contents: intentionally empty, consistent with L1 raw context
             behavior (L1 never returns big_map contents in this RPC). *)
          return_some (kt, vt, Option.value ~default:Z.zero total_bytes, [])
      | _ -> return_none

    let block _chain block =
      let open Lwt_result_syntax in
      let* block_number = shell_block_param_to_block_number block in
      Evm_ro_context.tezosx_nth_block ctxt (Z.of_int32 block_number)

    let monitor_heads chain query =
      (* TODO: #7831
         take chain into account
         For the moment this implementation only supports the main chain, once
         the rpc support of tezlink is more stable, we can add support for other chains *)
      ignore (chain, query) ;

      let blueprint_stream, stopper = Broadcast.create_blueprint_stream () in

      let retry_delays_ms = [0.; 50.; 100.; 500.] in

      (* Convert blueprint notifications into full blocks, giving the store a
         short grace period if the block is not yet written.
         Note that this delay does not correspond to the time between blueprint production
         and block application, but rather from the time the Database says the data has been
         written in the storage and the moment it actually becomes available to be read. *)
      let rec fetch_block level =
        let open Lwt_syntax in
        function
        | [] ->
            (* After all retries failed, emit warning event. *)
            let* () = Events.missing_block @@ Z.to_int32 level in
            return_none
        | delay_ms :: rest -> (
            let* () = Lwt_unix.sleep (delay_ms /. 1000.) in
            let* block_result = Evm_ro_context.tezosx_nth_block ctxt level in
            match block_result with
            | Ok block -> return_some block
            | Error _ -> fetch_block level rest)
      in

      let block_stream =
        Lwt_stream.filter_map_s
          (fun (bp_with_events : Blueprint_types.Legacy.with_events) ->
            (* Extract the level from the blueprint. *)
            let (Ethereum_types.Qty level) = bp_with_events.blueprint.number in
            fetch_block level retry_delays_ms)
          blueprint_stream
      in
      (block_stream, stopper)

    (* TODO: #7963 Support Observer Mode
       Here the catchup mechanism to fetch blueprints is not taken into account as
       the observer mode is not supported yet *)
    let bootstrapped () =
      let open Lwt_result_syntax in
      let* (Qty current_block_number) =
        Evm_ro_context.block_param_to_block_number
          ctxt
          ~chain_family:L2_types.Michelson
          ~hash_column:`Michelson
          (Ethereum_types.Block_parameter.Block_parameter Latest)
      in
      let* (block : L2_types.Tezos_block.t) =
        Evm_ro_context.tezosx_nth_block ctxt current_block_number
      in
      return (block.hash, block.timestamp)

    let block_hash chain block =
      let open Lwt_result_syntax in
      let `Main = chain in
      let* number = shell_block_param_to_block_number block in
      Evm_ro_context.tezosx_nth_block_hash ctxt (Z.of_int32 number)

    let simulate_operation ~chain_id ~simulator_mode op hash block =
      let open Lwt_result_syntax in
      let* () =
        match block with
        | `Head 0l -> return_unit
        | _ ->
            failwith "operation simulation is only possible on the head block"
      in
      let block = Ethereum_types.Block_parameter.(Block_parameter Latest) in
      Simulator.TezosX.simulate_operation
        ctxt
        ~simulator_mode
        ~chain_id
        op
        hash
        block

    let get_entrypoints chain block contract ~normalize_types =
      let open Lwt_result_syntax in
      (* For originated contracts (code stored in durable storage), use the
         OCaml-side entrypoints extraction which follows L1 behavior exactly
         (correct handling of the implicit `default` entrypoint).
         For enshrined contracts (no code in durable storage, e.g. TezosX
         Gateway), fall through to the kernel via tezosx_michelson_entrypoints. *)
      let* code = get_code chain block contract in
      match code with
      | Some code -> Tezlink_mock.list_entrypoints code normalize_types
      | None -> (
          let* eth_block = shell_block_param_to_eth_block_param block in
          let addr_bytes =
            Bytes.to_string
              (Data_encoding.Binary.to_bytes_exn
                 Tezos_types.Contract.encoding
                 contract)
          in
          let* state = Evm_ro_context.get_state ctxt ~block:eth_block () in
          let* bytes =
            execute_kernel_entrypoint
              state
              ~available:Storage_version.tezosx_michelson_entrypoints
              ~input_path:Durable_storage_path.Tezosx_entrypoints.input
              ~input:(Bytes.of_string addr_bytes)
              ~output_path:Durable_storage_path.Tezosx_entrypoints.result
              ~entrypoint:"tezosx_michelson_entrypoints"
          in
          let* result = decode_entrypoints_result bytes in
          match result with
          | None -> return_none
          | Some (unreachable, entries, _views) ->
              if normalize_types then
                let* normalized =
                  Tezlink_mock.normalize_entrypoint_type_exprs entries
                in
                return_some (unreachable, normalized)
              else return_some (unreachable, entries))

    (* Mirrors L1's `run_script_view` handler: resolve the view's declared
       types, synthesise the very same viewer script L1 builds
       (`View_helpers.make_michelson_viewer_script`), run it, and read the
       value back out of its storage. The only difference is where the
       script runs — the kernel's `run_code` entrypoint rather than an
       in-process `Script_interpreter.execute`. *)
    let run_script_view chain block ~contract ~view ~input ~chain_id
        ~unlimited_gas ~gas ~payer ~sender ~now ~level ~unparsing_mode =
      let open Lwt_result_syntax in
      let `Main = chain in
      let module Imported = Tezlink_imports.Imported_protocol in
      let module View_helpers =
        Tezlink_imports.Imported_protocol_plugin.View_helpers
      in
      let proto e =
        Result_syntax.tzfail (Tezlink_imports.Imported_env.wrap_tzerror e)
      in
      let*? contract_hash =
        match contract with
        | Tezlink_imports.Imported_context.Contract.Originated hash ->
            Result_syntax.return hash
        | Implicit _ ->
            (* Unreachable through the RPC: the service types the
               contract with [Contract.originated_encoding]. *)
            Result_syntax.tzfail
              (Run_script_view_failed
                 "A view can only be called on an originated contract")
      in
      let* eth_block = shell_block_param_to_eth_block_param block in
      (* One checkout serves the code read and the kernel entrypoints
         below: the state is functional, [execute_entrypoint] never
         commits, and the tree is thrown away with the request. *)
      let* state = Evm_ro_context.get_state ctxt ~block:eth_block () in
      let* on_chain_code = Durable_storage.read_contract_code contract state in
      let* code =
        match on_chain_code with
        | Some code -> return code
        | None -> (
            (* No on-chain code: the enshrined stub carries the synthetic
               views. *)
            let* script = enshrined_script contract state in
            match script with
            | None ->
                Lwt.return (proto View_helpers.Viewed_contract_has_no_script)
            | Some {code; _} -> (
                match Data_encoding.force_decode code with
                | Some code -> return code
                | None ->
                    tzfail
                      (Run_script_view_decode_error
                         "Cannot decode the script code")))
      in
      (* Under unlimited gas: the requested budget only applies to the run
         itself. Gas-only, so the mocked context is sound here. *)
      let* proto_ctxt = Lazy.force Run_code.dummy_context in
      let* input_ty, output_ty =
        let*! res =
          View_helpers.script_view_type proto_ctxt contract_hash code view
        in
        Lwt.return (Tezlink_imports.Imported_env.wrap_tzresult res)
      in
      let viewer =
        View_helpers.make_michelson_viewer_script
          contract
          view
          input
          input_ty
          output_ty
      in
      let*? viewer_code, viewer_storage =
        match
          ( Data_encoding.force_decode viewer.code,
            Data_encoding.force_decode viewer.storage )
        with
        | Some code, Some storage -> Result_syntax.return (code, storage)
        | _ ->
            Result_syntax.tzfail
              (Run_script_view_decode_error "Cannot decode the viewer script")
      in
      (* L1 caps `unlimited_gas` at the largest representable milligas; the
         runtime's own per-operation cap is lower and the kernel clamps to
         it, so [None] already expresses "as much as allowed". *)
      let gas = if unlimited_gas then None else gas in
      (* [sender] is inert for an ordinary view but is what the enshrined
         synthetic views report as the caller — see
         [Tezlink_backend_sig.S.run_script_view]. [on_chain_code] is
         [None] exactly for an enshrined target: an unknown address
         already failed above. *)
      let*? self =
        match (sender, on_chain_code) with
        | None, _ | _, Some _ -> Result_syntax.return contract
        | ( Some (Tezlink_imports.Imported_context.Contract.Originated _ as s),
            None ) ->
            Result_syntax.return s
        | Some (Implicit _), None ->
            (* Only an originated contract can execute `VIEW` on chain;
               refuse rather than quietly report the enshrined contract
               as its own caller. *)
            Result_syntax.tzfail (Run_script_view_unsupported_source view)
      in
      let unit_parameter =
        Micheline.strip_locations
          (Micheline.Prim (0, Imported.Michelson_v1_primitives.D_Unit, [], []))
      in
      let*? input_bytes =
        Run_code.encode_input
          ~script:viewer_code
          ~storage:viewer_storage
          ~input:unit_parameter
          ~entrypoint:"default"
          ~chain_id
          ~self
          ~amount:0L
          ~sender
          ~payer
          ~balance:None
          ~gas
          ~now
          ~level
      in
      let* bytes =
        execute_kernel_entrypoint
          state
          ~available:Storage_version.tezosx_run_code
          ~input_path:Durable_storage_path.Tezosx_run_code.input
          ~input:input_bytes
          ~output_path:Durable_storage_path.Tezosx_run_code.result
          ~entrypoint:"tezosx_run_code"
      in
      let*? storage = Run_code.decode_result bytes in
      let*? value =
        Tezlink_imports.Imported_env.wrap_tzresult
          (View_helpers.extract_value_from_storage storage)
      in
      Run_code.normalize
        ~unparsing_mode
        ~output_type:(Micheline.strip_locations output_ty)
        (Micheline.strip_locations value)
  end : Tezlink_backend_sig.S)
