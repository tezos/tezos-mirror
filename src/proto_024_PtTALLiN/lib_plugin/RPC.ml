(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Environment
open Alpha_context

let rpc_services =
  Alpha_services.register () ;
  Services_registration.get_rpc_services ()

type version = Version_1

let string_of_version = function Version_1 -> "1"

let version_of_string = function
  | "0" -> Error "Version \"0\" is not supported anymore"
  | "1" -> Ok Version_1
  | _ -> Error "Cannot parse version (supported version \"1\")"

let version_arg =
  let open RPC_arg in
  make
    ~descr:"Supported RPC version is version '1'"
    ~name:"version"
    ~destruct:version_of_string
    ~construct:string_of_version
    ()

(** The assumed number of blocks between operation-creation time and
    the actual time when the operation is included in a block. *)
let default_operation_inclusion_latency = 3

let path = RPC_path.(open_root / "helpers")

let elab_conf =
  Script_ir_translator_config.make
    ~keep_extra_types_for_interpreter_logging:true

module Registration = Services_registration_plugin

let unparsing_mode_encoding =
  let open Script_ir_unparser in
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        (Tag 0)
        ~title:"Readable"
        (constant "Readable")
        (function Readable -> Some () | Optimized | Optimized_legacy -> None)
        (fun () -> Readable);
      case
        (Tag 1)
        ~title:"Optimized"
        (constant "Optimized")
        (function Optimized -> Some () | Readable | Optimized_legacy -> None)
        (fun () -> Optimized);
      case
        (Tag 2)
        ~title:"Optimized_legacy"
        (constant "Optimized_legacy")
        (function Optimized_legacy -> Some () | Readable | Optimized -> None)
        (fun () -> Optimized_legacy);
    ]

module Scripts = struct
  module S = struct
    open Data_encoding

    let path = RPC_path.(path / "scripts")

    type other_contract_description = {
      address : Contract_hash.t;
      ty : Script.expr;
    }

    let other_contracts_encoding =
      list
        (conv
           (fun {address; ty} -> (address, ty))
           (fun (address, ty) -> {address; ty})
           (obj2
              (req "address" Contract_hash.encoding)
              (req "type" Script.expr_encoding)))

    type extra_big_map_description = {
      id : Big_map.Id.t;
      kty : Script.expr;
      vty : Script.expr;
      items : Script.expr;
    }

    let extra_big_maps_encoding =
      list
        (conv
           (fun {id; kty; vty; items} -> (id, kty, vty, items))
           (fun (id, kty, vty, items) -> {id; kty; vty; items})
           (obj4
              (req "id" Big_map.Id.encoding)
              (req "key_type" Script.expr_encoding)
              (req "val_type" Script.expr_encoding)
              (req "map_literal" Script.expr_encoding)))

    let run_code_input_encoding =
      merge_objs
        (obj10
           (req "script" Script.expr_encoding)
           (req "storage" Script.expr_encoding)
           (req "input" Script.expr_encoding)
           (req "amount" Tez.encoding)
           (opt "balance" Tez.encoding)
           (req "chain_id" Chain_id.encoding)
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/710
              Rename the "source" field into "sender" *)
           (opt "source" Contract.encoding)
           (opt "payer" Contract.implicit_encoding)
           (opt "self" Contract.originated_encoding)
           (dft "entrypoint" Entrypoint.simple_encoding Entrypoint.default))
        (obj6
           (opt "unparsing_mode" unparsing_mode_encoding)
           (opt "gas" Gas.Arith.z_integral_encoding)
           (opt "now" Script_timestamp.encoding)
           (opt "level" Script_int.n_encoding)
           (opt "other_contracts" other_contracts_encoding)
           (opt "extra_big_maps" extra_big_maps_encoding))

    let run_code_output_encoding =
      conv
        (fun (storage, operations, lazy_storage_diff) ->
          (storage, operations, lazy_storage_diff))
        (fun (storage, operations, lazy_storage_diff) ->
          (storage, operations, lazy_storage_diff))
        (obj3
           (req "storage" Script.expr_encoding)
           (req
              "operations"
              (list Apply_internal_results.internal_operation_encoding))
           (opt "lazy_storage_diff" Lazy_storage.encoding))

    let trace_code_input_encoding = run_code_input_encoding

    let trace_encoding : Script_typed_ir.execution_trace encoding =
      def "scripted.trace" @@ list
      @@ obj3
           (req "location" Script.location_encoding)
           (req "gas" Gas.Arith.z_fp_encoding)
           (req "stack" (list Script.expr_encoding))

    let trace_code_output_encoding =
      conv
        (fun (storage, operations, trace, lazy_storage_diff) ->
          (storage, operations, trace, lazy_storage_diff))
        (fun (storage, operations, trace, lazy_storage_diff) ->
          (storage, operations, trace, lazy_storage_diff))
        (obj4
           (req "storage" Script.expr_encoding)
           (req
              "operations"
              (list Apply_internal_results.internal_operation_encoding))
           (req "trace" trace_encoding)
           (opt "lazy_storage_diff" Lazy_storage.encoding))

    let stack_encoding =
      list
        (obj2
           (req "type" Script.expr_encoding)
           (req "val" Script.expr_encoding))

    let run_instr_input_encoding =
      merge_objs
        (obj10
           (req "input" stack_encoding)
           (req "code" Script.expr_encoding)
           (req "chain_id" Chain_id.encoding)
           (opt "gas" Gas.Arith.z_integral_encoding)
           (opt "now" Script_timestamp.encoding)
           (opt "level" Script_int.n_encoding)
           (opt "sender" Contract.encoding)
           (opt "source" Contract.implicit_encoding)
           (opt "self" Contract.originated_encoding)
           (opt "parameter" Script.expr_encoding))
        (obj6
           (req "amount" Tez.encoding)
           (opt "balance" Tez.encoding)
           (opt "other_contracts" other_contracts_encoding)
           (opt "big_maps" extra_big_maps_encoding)
           (opt "unparsing_mode" unparsing_mode_encoding)
           (dft "legacy" bool false))

    let run_instr_output_encoding =
      obj2 (req "output" stack_encoding) (req "gas" Gas.encoding)

    let run_tzip4_view_encoding =
      let open Data_encoding in
      merge_objs
        (obj10
           (req "contract" Contract.originated_encoding)
           (req "entrypoint" Entrypoint.simple_encoding)
           (req "input" Script.expr_encoding)
           (req "chain_id" Chain_id.encoding)
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/710
              Rename the "source" field into "sender" *)
           (opt "source" Contract.encoding)
           (opt "payer" Contract.implicit_encoding)
           (opt "gas" Gas.Arith.z_integral_encoding)
           (req "unparsing_mode" unparsing_mode_encoding)
           (opt "now" Script_timestamp.encoding)
           (opt "level" Script_int.n_encoding))
        (obj2
           (opt "other_contracts" other_contracts_encoding)
           (opt "extra_big_maps" extra_big_maps_encoding))

    let run_script_view_encoding =
      let open Data_encoding in
      merge_objs
        (obj10
           (req "contract" Contract.originated_encoding)
           (req "view" (string Plain))
           (req "input" Script.expr_encoding)
           (dft "unlimited_gas" bool false)
           (req "chain_id" Chain_id.encoding)
           (* TODO: https://gitlab.com/tezos/tezos/-/issues/710
              Rename the "source" field into "sender" *)
           (opt "source" Contract.encoding)
           (opt "payer" Contract.implicit_encoding)
           (opt "gas" Gas.Arith.z_integral_encoding)
           (req "unparsing_mode" unparsing_mode_encoding)
           (opt "now" Script_timestamp.encoding))
        (obj3
           (opt "level" Script_int.n_encoding)
           (opt "other_contracts" other_contracts_encoding)
           (opt "extra_big_maps" extra_big_maps_encoding))

    let normalize_stack_input_encoding =
      obj5
        (req "input" stack_encoding)
        (req "unparsing_mode" unparsing_mode_encoding)
        (dft "legacy" bool false)
        (opt "other_contracts" other_contracts_encoding)
        (opt "extra_big_maps" extra_big_maps_encoding)

    let normalize_stack_output_encoding = obj1 (req "output" stack_encoding)

    let run_code =
      RPC_service.post_service
        ~description:"Run a Michelson script in the current context"
        ~query:RPC_query.empty
        ~input:run_code_input_encoding
        ~output:run_code_output_encoding
        RPC_path.(path / "run_code")

    let trace_code =
      RPC_service.post_service
        ~description:
          "Run a Michelson script in the current context, keeping a trace"
        ~query:RPC_query.empty
        ~input:trace_code_input_encoding
        ~output:trace_code_output_encoding
        RPC_path.(path / "trace_code")

    let run_tzip4_view =
      RPC_service.post_service
        ~description:
          "Simulate a call to a view following the TZIP-4 standard. See \
           https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints."
        ~input:run_tzip4_view_encoding
        ~output:(obj1 (req "data" Script.expr_encoding))
        ~query:RPC_query.empty
        (* This path should be deprecated in the future *)
        RPC_path.(path / "run_view")

    let run_script_view =
      RPC_service.post_service
        ~description:"Simulate a call to a michelson view"
        ~input:run_script_view_encoding
        ~output:(obj1 (req "data" Script.expr_encoding))
        ~query:RPC_query.empty
        RPC_path.(path / "run_script_view")

    let run_instr =
      RPC_service.post_service
        ~description:"Run a single Michelson instruction"
        ~query:RPC_query.empty
        ~input:run_instr_input_encoding
        ~output:run_instr_output_encoding
        RPC_path.(path / "run_instruction")

    let typecheck_code =
      RPC_service.post_service
        ~description:"Typecheck a piece of code in the current context"
        ~query:RPC_query.empty
        ~input:
          (obj4
             (req "program" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (dft "legacy" bool false)
             (dft "show_types" bool true))
        ~output:
          (obj2
             (req "type_map" Script_tc_errors_registration.type_map_enc)
             (req "gas" Gas.encoding))
        RPC_path.(path / "typecheck_code")

    let script_size =
      RPC_service.post_service
        ~description:"Compute the size of a script in the current context"
        ~query:RPC_query.empty
        ~input:
          (obj4
             (req "program" Script.expr_encoding)
             (req "storage" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (dft "legacy" bool false))
        ~output:(obj1 (req "script_size" int31))
        RPC_path.(path / "script_size")

    let typecheck_data =
      RPC_service.post_service
        ~description:
          "Check that some data expression is well formed and of a given type \
           in the current context"
        ~query:RPC_query.empty
        ~input:
          (obj4
             (req "data" Script.expr_encoding)
             (req "type" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (dft "legacy" bool false))
        ~output:(obj1 (req "gas" Gas.encoding))
        RPC_path.(path / "typecheck_data")

    let pack_data =
      RPC_service.post_service
        ~description:
          "Computes the serialized version of some data expression using the \
           same algorithm as script instruction PACK"
        ~input:
          (obj3
             (req "data" Script.expr_encoding)
             (req "type" Script.expr_encoding)
             (opt "gas" Gas.Arith.z_integral_encoding))
        ~output:(obj2 (req "packed" (bytes Hex)) (req "gas" Gas.encoding))
        ~query:RPC_query.empty
        RPC_path.(path / "pack_data")

    let normalize_data =
      RPC_service.post_service
        ~description:
          "Normalizes some data expression using the requested unparsing mode"
        ~input:
          (obj6
             (req "data" Script.expr_encoding)
             (req "type" Script.expr_encoding)
             (req "unparsing_mode" unparsing_mode_encoding)
             (dft "legacy" bool false)
             (opt "other_contracts" other_contracts_encoding)
             (opt "extra_big_maps" extra_big_maps_encoding))
        ~output:(obj1 (req "normalized" Script.expr_encoding))
        ~query:RPC_query.empty
        RPC_path.(path / "normalize_data")

    let normalize_stack =
      RPC_service.post_service
        ~description:
          "Normalize a Michelson stack using the requested unparsing mode"
        ~query:RPC_query.empty
        ~input:normalize_stack_input_encoding
        ~output:normalize_stack_output_encoding
        RPC_path.(path / "normalize_stack")

    let normalize_script =
      RPC_service.post_service
        ~description:
          "Normalizes a Michelson script using the requested unparsing mode"
        ~input:
          (obj2
             (req "script" Script.expr_encoding)
             (req "unparsing_mode" unparsing_mode_encoding))
        ~output:(obj1 (req "normalized" Script.expr_encoding))
        ~query:RPC_query.empty
        RPC_path.(path / "normalize_script")

    let normalize_type =
      RPC_service.post_service
        ~description:
          "Normalizes some Michelson type by expanding `pair a b c` as `pair a \
           (pair b c)"
        ~input:(obj1 (req "type" Script.expr_encoding))
        ~output:(obj1 (req "normalized" Script.expr_encoding))
        ~query:RPC_query.empty
        RPC_path.(path / "normalize_type")

    let run_operation_query =
      let open RPC_query in
      query (fun version ->
          object
            method version = version
          end)
      |+ opt_field "version" version_arg (fun t -> t#version)
      |> seal

    let run_operation_output_encoding =
      Apply_results.operation_data_and_metadata_encoding

    let run_operation =
      RPC_service.post_service
        ~description:
          "Run an operation with the context of the given block and without \
           signature checks. Return the operation application result, \
           including the consumed gas. This RPC does not support consensus \
           operations."
        ~query:run_operation_query
        ~input:
          (obj2
             (req "operation" Operation.encoding)
             (req "chain_id" Chain_id.encoding))
        ~output:run_operation_output_encoding
        RPC_path.(path / "run_operation")

    let simulate_query =
      let open RPC_query in
      query (fun version successor_level ->
          object
            method version = version

            method successor_level = successor_level
          end)
      |+ opt_field "version" version_arg (fun t -> t#version)
      |+ flag
           ~descr:
             "If true, the simulation is done on the successor level of the \
              current context."
           "successor_level"
           (fun t -> t#successor_level)
      |> seal

    let simulate_operation =
      RPC_service.post_service
        ~description:
          "Simulate running an operation at some future moment (based on the \
           number of blocks given in the `latency` argument), and return the \
           operation application result. The result is the same as \
           run_operation except for the consumed gas, which depends on the \
           contents of the cache at that future moment. This RPC estimates \
           future gas consumption by trying to predict the state of the cache \
           using some heuristics."
        ~query:simulate_query
        ~input:
          (obj4
             (opt "blocks_before_activation" int32)
             (req "operation" Operation.encoding)
             (req "chain_id" Chain_id.encoding)
             (dft "latency" int16 default_operation_inclusion_latency))
        ~output:run_operation_output_encoding
        RPC_path.(path / "simulate_operation")

    let entrypoint_type =
      RPC_service.post_service
        ~description:"Return the type of the given entrypoint"
        ~query:RPC_query.empty
        ~input:
          (obj2
             (req "script" Script.expr_encoding)
             (dft "entrypoint" Entrypoint.simple_encoding Entrypoint.default))
        ~output:(obj1 (req "entrypoint_type" Script.expr_encoding))
        RPC_path.(path / "entrypoint")

    let list_entrypoints =
      RPC_service.post_service
        ~description:"Return the list of entrypoints of the given script"
        ~query:RPC_query.empty
        ~input:(obj1 (req "script" Script.expr_encoding))
        ~output:
          (obj2
             (dft
                "unreachable"
                (Data_encoding.list
                   (obj1
                      (req
                         "path"
                         (Data_encoding.list
                            Michelson_v1_primitives.prim_encoding))))
                [])
             (req "entrypoints" (assoc Script.expr_encoding)))
        RPC_path.(path / "entrypoints")
  end

  module type UNPARSING_MODE = sig
    val unparsing_mode : Script_ir_unparser.unparsing_mode
  end

  module Traced_interpreter (Unparsing_mode : UNPARSING_MODE) = struct
    type log_element =
      | Log :
          context
          * Script.location
          * ('a * 's)
          * ('a, 's) Script_typed_ir.stack_ty
          -> log_element

    let unparse_stack ctxt (stack, stack_ty) =
      let open Lwt_result_syntax in
      (* We drop the gas limit as this function is only used for debugging/errors. *)
      let ctxt = Gas.set_unlimited ctxt in
      let rec unparse_stack : type a s.
          (a, s) Script_typed_ir.stack_ty * (a * s) ->
          Script.expr list Environment.Error_monad.tzresult Lwt.t = function
        | Bot_t, (EmptyCell, EmptyCell) -> return_nil
        | Item_t (ty, rest_ty), (v, rest) ->
            let* data, _ctxt =
              Script_ir_translator.unparse_data
                ctxt
                Unparsing_mode.unparsing_mode
                ty
                v
            in
            let+ rest = unparse_stack (rest_ty, rest) in
            data :: rest
      in
      unparse_stack (stack_ty, stack)

    let trace_logger ctxt : Script_typed_ir.logger =
      let open Lwt_result_syntax in
      Script_interpreter_logging.make
        (module struct
          let log : log_element list ref = ref []

          let log_interp _ ctxt loc sty stack =
            log := Log (ctxt, loc, stack, sty) :: !log

          let log_entry _ _ctxt _loc _sty _stack = ()

          let log_exit _ ctxt loc sty stack =
            log := Log (ctxt, loc, stack, sty) :: !log

          let log_control _ = ()

          let get_log () =
            let+ _ctxt, res =
              List.fold_left_es
                (fun (old_ctxt, l) (Log (ctxt, loc, stack, stack_ty)) ->
                  let consumed_gas = Gas.consumed ~since:old_ctxt ~until:ctxt in
                  let+ stack =
                    Environment.Error_monad.trace
                      Plugin_errors.Cannot_serialize_log
                      (unparse_stack ctxt (stack, stack_ty))
                  in
                  (ctxt, (loc, consumed_gas, stack) :: l))
                (ctxt, [])
                (List.rev !log)
            in
            Some (List.rev res)
        end)

    let execute ctxt step_constants ~script ~entrypoint ~parameter =
      let open Lwt_result_syntax in
      let logger = trace_logger ctxt in
      let* res =
        Script_interpreter.execute
          ~logger
          ~cached_script:None
          ctxt
          Unparsing_mode.unparsing_mode
          step_constants
          ~script
          ~entrypoint
          ~parameter
          ~internal:true
      in
      let+ trace = logger.get_log () in
      let trace = Option.value ~default:[] trace in
      (res, trace)
  end

  let typecheck_data :
      legacy:bool ->
      context ->
      Script.expr * Script.expr ->
      context Environment.Error_monad.tzresult Lwt.t =
    let open Lwt_result_syntax in
    fun ~legacy ctxt (data, exp_ty) ->
      let*? Ex_ty exp_ty, ctxt =
        Environment.Error_monad.record_trace
          (Script_tc_errors.Ill_formed_type (None, exp_ty, 0))
          (Script_ir_translator.parse_passable_ty
             ctxt
             ~legacy
             (Micheline.root exp_ty))
      in
      let+ _, ctxt =
        Environment.Error_monad.trace_eval
          (fun () ->
            let exp_ty = Script_ir_unparser.serialize_ty_for_error exp_ty in
            Script_tc_errors.Ill_typed_data (None, data, exp_ty))
          (let allow_forged_tickets = true in
           let allow_forged_lazy_storage_id =
             true
             (* Safe since we ignore the value afterwards. *)
           in
           Script_ir_translator.parse_data
             ctxt
             ~elab_conf:(elab_conf ~legacy ())
             ~allow_forged_tickets
             ~allow_forged_lazy_storage_id
             exp_ty
             (Micheline.root data))
      in
      ctxt

  module Unparse_types = struct
    (* Same as the unparsing functions for types in Script_ir_translator but
       does not consume gas and never folds (pair a (pair b c)) *)

    open Micheline
    open Michelson_v1_primitives
    open Script_typed_ir

    let unparse_memo_size ~loc memo_size =
      let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
      Int (loc, z)

    let rec unparse_ty : type a ac loc.
        loc:loc -> (a, ac) ty -> (loc, Script.prim) Micheline.node =
     fun ~loc ty ->
      let return (name, args, annot) = Prim (loc, name, args, annot) in
      match ty with
      | Unit_t -> return (T_unit, [], [])
      | Int_t -> return (T_int, [], [])
      | Nat_t -> return (T_nat, [], [])
      | Signature_t -> return (T_signature, [], [])
      | String_t -> return (T_string, [], [])
      | Bytes_t -> return (T_bytes, [], [])
      | Mutez_t -> return (T_mutez, [], [])
      | Bool_t -> return (T_bool, [], [])
      | Key_hash_t -> return (T_key_hash, [], [])
      | Key_t -> return (T_key, [], [])
      | Timestamp_t -> return (T_timestamp, [], [])
      | Address_t -> return (T_address, [], [])
      | Operation_t -> return (T_operation, [], [])
      | Chain_id_t -> return (T_chain_id, [], [])
      | Never_t -> return (T_never, [], [])
      | Bls12_381_g1_t -> return (T_bls12_381_g1, [], [])
      | Bls12_381_g2_t -> return (T_bls12_381_g2, [], [])
      | Bls12_381_fr_t -> return (T_bls12_381_fr, [], [])
      | Contract_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_contract, [t], [])
      | Pair_t (utl, utr, _meta, _) ->
          let annot = [] in
          let tl = unparse_ty ~loc utl in
          let tr = unparse_ty ~loc utr in
          return (T_pair, [tl; tr], annot)
      | Or_t (utl, utr, _meta, _) ->
          let annot = [] in
          let tl = unparse_ty ~loc utl in
          let tr = unparse_ty ~loc utr in
          return (T_or, [tl; tr], annot)
      | Lambda_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_lambda, [ta; tr], [])
      | Option_t (ut, _meta, _) ->
          let annot = [] in
          let ut = unparse_ty ~loc ut in
          return (T_option, [ut], annot)
      | List_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_list, [t], [])
      | Ticket_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_ticket, [t], [])
      | Set_t (ut, _meta) ->
          let t = unparse_ty ~loc ut in
          return (T_set, [t], [])
      | Map_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_map, [ta; tr], [])
      | Big_map_t (uta, utr, _meta) ->
          let ta = unparse_ty ~loc uta in
          let tr = unparse_ty ~loc utr in
          return (T_big_map, [ta; tr], [])
      | Sapling_transaction_t memo_size ->
          return (T_sapling_transaction, [unparse_memo_size ~loc memo_size], [])
      | Sapling_transaction_deprecated_t memo_size ->
          return
            ( T_sapling_transaction_deprecated,
              [unparse_memo_size ~loc memo_size],
              [] )
      | Sapling_state_t memo_size ->
          return (T_sapling_state, [unparse_memo_size ~loc memo_size], [])
      | Chest_t -> return (T_chest, [], [])
      | Chest_key_t -> return (T_chest_key, [], [])
  end

  module Normalize_stack = struct
    type ex_stack =
      | Ex_stack : ('a, 's) Script_typed_ir.stack_ty * 'a * 's -> ex_stack

    let rec parse_stack :
        context ->
        legacy:bool ->
        (Script.node * Script.node) list ->
        (ex_stack * context) Environment.Error_monad.tzresult Lwt.t =
      let open Lwt_result_syntax in
      fun ctxt ~legacy l ->
        match l with
        | [] -> return (Ex_stack (Bot_t, EmptyCell, EmptyCell), ctxt)
        | (ty_node, data_node) :: l ->
            let*? Ex_ty ty, ctxt =
              Script_ir_translator.parse_ty
                ctxt
                ~legacy
                ~allow_lazy_storage:true
                ~allow_operation:true
                ~allow_contract:true
                ~allow_ticket:true
                ty_node
            in
            let elab_conf = elab_conf ~legacy () in
            let* x, ctxt =
              Script_ir_translator.parse_data
                ctxt
                ~elab_conf
                ~allow_forged_tickets:true
                ~allow_forged_lazy_storage_id:true
                ty
                data_node
            in
            let+ Ex_stack (sty, y, st), ctxt = parse_stack ctxt ~legacy l in
            (Ex_stack (Item_t (ty, sty), x, (y, st)), ctxt)

    let rec unparse_stack : type a s.
        context ->
        Script_ir_unparser.unparsing_mode ->
        (a, s) Script_typed_ir.stack_ty ->
        a ->
        s ->
        ((Script.expr * Script.expr) list * context)
        Environment.Error_monad.tzresult
        Lwt.t =
      let open Lwt_result_syntax in
      let loc = Micheline.dummy_location in
      fun ctxt unparsing_mode sty x st ->
        match (sty, x, st) with
        | Bot_t, EmptyCell, EmptyCell -> return ([], ctxt)
        | Item_t (ty, sty), x, (y, st) ->
            let*? ty_node, ctxt = Script_ir_unparser.unparse_ty ~loc ctxt ty in
            let* data_node, ctxt =
              Script_ir_translator.unparse_data ctxt unparsing_mode ty x
            in
            let+ l, ctxt = unparse_stack ctxt unparsing_mode sty y st in
            ((Micheline.strip_locations ty_node, data_node) :: l, ctxt)
  end

  let rec pp_instr_name : type a b c d.
      Format.formatter -> (a, b, c, d) Script_typed_ir.kinstr -> unit =
    let open Script_typed_ir in
    let open Format in
    fun fmt -> function
      | IDrop _ -> pp_print_string fmt "DROP"
      | IDup _ -> pp_print_string fmt "DUP"
      | ISwap _ -> pp_print_string fmt "SWAP"
      | IPush _ -> pp_print_string fmt "PUSH"
      | IUnit _ -> pp_print_string fmt "UNIT"
      | ICons_pair _ -> pp_print_string fmt "PAIR"
      | ICar _ -> pp_print_string fmt "CAR"
      | ICdr _ -> pp_print_string fmt "CDR"
      | IUnpair _ -> pp_print_string fmt "UNPAIR"
      | ICons_some _ -> pp_print_string fmt "SOME"
      | ICons_none _ -> pp_print_string fmt "NONE"
      | IIf_none _ -> pp_print_string fmt "IF_NONE"
      | IOpt_map _ -> pp_print_string fmt "MAP"
      | ICons_left _ -> pp_print_string fmt "LEFT"
      | ICons_right _ -> pp_print_string fmt "RIGHT"
      | IIf_left _ -> pp_print_string fmt "IF_LEFT"
      | ICons_list _ -> pp_print_string fmt "CONS"
      | INil _ -> pp_print_string fmt "NIL"
      | IIf_cons _ -> pp_print_string fmt "IF_CONS"
      | IList_map _ -> pp_print_string fmt "MAP"
      | IList_iter _ -> pp_print_string fmt "ITER"
      | IList_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_set _ -> pp_print_string fmt "EMPTY_SET"
      | ISet_iter _ -> pp_print_string fmt "ITER"
      | ISet_mem _ -> pp_print_string fmt "MEM"
      | ISet_update _ -> pp_print_string fmt "UPDATE"
      | ISet_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_map _ -> pp_print_string fmt "EMPTY_MAP"
      | IMap_map _ -> pp_print_string fmt "MAP"
      | IMap_iter _ -> pp_print_string fmt "ITER"
      | IMap_mem _ -> pp_print_string fmt "MEM"
      | IMap_get _ -> pp_print_string fmt "GET"
      | IMap_update _ -> pp_print_string fmt "UPDATE"
      | IMap_get_and_update _ -> pp_print_string fmt "GET_AND_UPDATE"
      | IMap_size _ -> pp_print_string fmt "SIZE"
      | IEmpty_big_map _ -> pp_print_string fmt "EMPTY_BIG_MAP"
      | IBig_map_mem _ -> pp_print_string fmt "MEM"
      | IBig_map_get _ -> pp_print_string fmt "GET"
      | IBig_map_update _ -> pp_print_string fmt "UPDATE"
      | IBig_map_get_and_update _ -> pp_print_string fmt "GET_AND_UPDATE"
      | IConcat_string _ -> pp_print_string fmt "CONCAT"
      | IConcat_string_pair _ -> pp_print_string fmt "CONCAT"
      | ISlice_string _ -> pp_print_string fmt "SLICE"
      | IString_size _ -> pp_print_string fmt "SIZE"
      | IConcat_bytes _ -> pp_print_string fmt "CONCAT"
      | IConcat_bytes_pair _ -> pp_print_string fmt "CONCAT"
      | ISlice_bytes _ -> pp_print_string fmt "SLICE"
      | IBytes_size _ -> pp_print_string fmt "SIZE"
      | IBytes_nat _ -> pp_print_string fmt "BYTES"
      | INat_bytes _ -> pp_print_string fmt "NAT"
      | IBytes_int _ -> pp_print_string fmt "BYTES"
      | IInt_bytes _ -> pp_print_string fmt "INT"
      | IAdd_seconds_to_timestamp _ -> pp_print_string fmt "ADD"
      | IAdd_timestamp_to_seconds _ -> pp_print_string fmt "ADD"
      | ISub_timestamp_seconds _ -> pp_print_string fmt "SUB"
      | IDiff_timestamps _ -> pp_print_string fmt "DIFF"
      | IAdd_tez _ -> pp_print_string fmt "ADD"
      | ISub_tez _ -> pp_print_string fmt "SUB_MUTEZ"
      | ISub_tez_legacy _ -> pp_print_string fmt "SUB"
      | IMul_teznat _ | IMul_nattez _ -> pp_print_string fmt "MUL"
      | IEdiv_teznat _ -> pp_print_string fmt "EDIV"
      | IEdiv_tez _ -> pp_print_string fmt "EDIV"
      | IOr _ -> pp_print_string fmt "OR"
      | IAnd _ -> pp_print_string fmt "AND"
      | IXor _ -> pp_print_string fmt "XOR"
      | INot _ -> pp_print_string fmt "NOT"
      | IIs_nat _ -> pp_print_string fmt "ISNAT"
      | INeg _ -> pp_print_string fmt "NEG"
      | IAbs_int _ -> pp_print_string fmt "ABS"
      | IInt_nat _ -> pp_print_string fmt "INT"
      | IAdd_int _ | IAdd_nat _ -> pp_print_string fmt "ADD"
      | ISub_int _ -> pp_print_string fmt "SUB"
      | IMul_int _ | IMul_nat _ -> pp_print_string fmt "MUL"
      | IEdiv_int _ | IEdiv_nat _ -> pp_print_string fmt "EDIV"
      | ILsl_nat _ -> pp_print_string fmt "LSL"
      | ILsl_bytes _ -> pp_print_string fmt "LSL"
      | ILsr_nat _ -> pp_print_string fmt "LSR"
      | ILsr_bytes _ -> pp_print_string fmt "LSR"
      | IOr_nat _ -> pp_print_string fmt "OR"
      | IOr_bytes _ -> pp_print_string fmt "OR"
      | IAnd_nat _ -> pp_print_string fmt "AND"
      | IAnd_int_nat _ -> pp_print_string fmt "AND"
      | IAnd_bytes _ -> pp_print_string fmt "AND"
      | IXor_nat _ -> pp_print_string fmt "XOR"
      | IXor_bytes _ -> pp_print_string fmt "XOR"
      | INot_int _ -> pp_print_string fmt "NOT"
      | INot_bytes _ -> pp_print_string fmt "NOT"
      | IIf _ -> pp_print_string fmt "IF"
      | ILoop _ -> pp_print_string fmt "LOOP"
      | ILoop_left _ -> pp_print_string fmt "LOOP_LEFT"
      | IDip _ -> pp_print_string fmt "DIP"
      | IExec _ -> pp_print_string fmt "EXEC"
      | IApply _ -> pp_print_string fmt "APPLY"
      | ILambda (_, Lam _, _) -> pp_print_string fmt "LAMBDA"
      | ILambda (_, LamRec _, _) -> pp_print_string fmt "LAMBDA_REC"
      | IFailwith _ -> pp_print_string fmt "FAILWITH"
      | ICompare _ -> pp_print_string fmt "COMPARE"
      | IEq _ -> pp_print_string fmt "EQ"
      | INeq _ -> pp_print_string fmt "NEQ"
      | ILt _ -> pp_print_string fmt "LT"
      | IGt _ -> pp_print_string fmt "GT"
      | ILe _ -> pp_print_string fmt "LE"
      | IGe _ -> pp_print_string fmt "GE"
      | IAddress _ -> pp_print_string fmt "ADDRESS"
      | IContract _ -> pp_print_string fmt "CONTACT"
      | IView _ -> pp_print_string fmt "VIEW"
      | ITransfer_tokens _ -> pp_print_string fmt "TRANSFER_TOKENS"
      | IImplicit_account _ -> pp_print_string fmt "IMPLICIT_ACCOUNT"
      | IIs_implicit_account _ -> pp_print_string fmt "IS_IMPLICIT_ACCOUNT"
      | IIndex_address _ -> pp_print_string fmt "INDEX_ADDRESS"
      | IGet_address_index _ -> pp_print_string fmt "GET_ADDRESS_INDEX"
      | ICreate_contract _ -> pp_print_string fmt "CREATE_CONTRACT"
      | ISet_delegate _ -> pp_print_string fmt "SET_DELEGATE"
      | INow _ -> pp_print_string fmt "NOW"
      | IMin_block_time _ -> pp_print_string fmt "MIN_BLOCK_TIME"
      | IBalance _ -> pp_print_string fmt "BALANCE"
      | ILevel _ -> pp_print_string fmt "LEVEL"
      | ICheck_signature _ -> pp_print_string fmt "CHECK_SIGNATURE"
      | IHash_key _ -> pp_print_string fmt "HASH_KEY"
      | IPack _ -> pp_print_string fmt "PACK"
      | IBlake2b _ -> pp_print_string fmt "BLAKE2B"
      | ISha3 _ -> pp_print_string fmt "SHA3"
      | ISha256 _ -> pp_print_string fmt "SHA256"
      | ISha512 _ -> pp_print_string fmt "SHA512"
      | IUnpack _ -> pp_print_string fmt "UNPACK"
      | ISource _ -> pp_print_string fmt "SOURCE"
      | ISender _ -> pp_print_string fmt "SENDER"
      | ISelf _ -> pp_print_string fmt "SELF"
      | ISelf_address _ -> pp_print_string fmt "SELF_ADDRESS"
      | IAmount _ -> pp_print_string fmt "AMOUNT"
      | ISapling_empty_state _ -> pp_print_string fmt "SAPLING_EMPTY_STATE"
      | ISapling_verify_update _ | ISapling_verify_update_deprecated _ ->
          pp_print_string fmt "SAPLING_VERIFY_UPDATE"
      | IDig _ -> pp_print_string fmt "DIG"
      | IDug _ -> pp_print_string fmt "DUG"
      | IDipn _ -> pp_print_string fmt "DIP"
      | IDropn _ -> pp_print_string fmt "DROP"
      | IChainId _ -> pp_print_string fmt "CHAIN_ID"
      | INever _ -> pp_print_string fmt "NEVER"
      | IVoting_power _ -> pp_print_string fmt "VOTING_POWER"
      | ITotal_voting_power _ -> pp_print_string fmt "TOTAL_VOTING_POWER"
      | IKeccak _ -> pp_print_string fmt "KECCAK"
      | IAdd_bls12_381_g1 _ | IAdd_bls12_381_g2 _ | IAdd_bls12_381_fr _ ->
          pp_print_string fmt "ADD"
      | IMul_bls12_381_g1 _ | IMul_bls12_381_g2 _ | IMul_bls12_381_fr _
      | IMul_bls12_381_z_fr _ | IMul_bls12_381_fr_z _ ->
          pp_print_string fmt "MUL"
      | IInt_bls12_381_fr _ -> pp_print_string fmt "INT"
      | INeg_bls12_381_g1 _ | INeg_bls12_381_g2 _ | INeg_bls12_381_fr _ ->
          pp_print_string fmt "NEG"
      | IPairing_check_bls12_381 _ -> pp_print_string fmt "PAIRING_CHECK"
      | IComb _ -> pp_print_string fmt "PAIR"
      | IUncomb _ -> pp_print_string fmt "UNPAIR"
      | IComb_get _ -> pp_print_string fmt "GET"
      | IComb_set _ -> pp_print_string fmt "UPDATE"
      | IDup_n _ -> pp_print_string fmt "DUP"
      | ITicket _ -> pp_print_string fmt "TICKET"
      | ITicket_deprecated _ -> pp_print_string fmt "TICKET_DEPRECATED"
      | IRead_ticket _ -> pp_print_string fmt "READ_TICKET"
      | ISplit_ticket _ -> pp_print_string fmt "SPLIT_TICKET"
      | IJoin_tickets _ -> pp_print_string fmt "JOIN_TICKETS"
      | IOpen_chest _ -> pp_print_string fmt "OPEN_CHEST"
      | IEmit _ -> pp_print_string fmt "EMIT"
      | IHalt _ -> pp_print_string fmt "[halt]"
      | ILog (_, _, _, _, instr) ->
          Format.fprintf fmt "log/%a" pp_instr_name instr

  type Environment.Error_monad.error +=
    | Run_operation_does_not_support_consensus_operations

  let () =
    let description =
      "The run_operation RPC does not support consensus operations."
    in
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"run_operation_does_not_support_consensus_operations"
      ~title:"Run operation does not support consensus operations"
      ~description
      ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
      Data_encoding.empty
      (function
        | Run_operation_does_not_support_consensus_operations -> Some ()
        | _ -> None)
      (fun () -> Run_operation_does_not_support_consensus_operations)

  (** Validate and apply the operation but skip signature checks; do
      not support consensus operations.

      Return the unchanged operation protocol data, and the operation
      receipt ie. metadata containing balance updates, consumed gas,
      application success or failure, etc. *)
  let run_operation_service rpc_ctxt _params (packed_operation, chain_id) =
    let open Lwt_result_syntax in
    let {Services_registration.context; block_header; _} = rpc_ctxt in
    let*? () =
      match packed_operation.protocol_data with
      | Operation_data {contents = Single (Preattestation _); _}
      | Operation_data {contents = Single (Attestation _); _} ->
          Environment.Error_monad.Result_syntax.tzfail
            Run_operation_does_not_support_consensus_operations
      | _ -> Result_syntax.return_unit
    in
    let oph = Operation.hash_packed packed_operation in
    let validity_state = Validate.begin_no_predecessor_info context chain_id in
    let* _validate_operation_state =
      Validate.validate_operation
        ~check_signature:false
        validity_state
        oph
        packed_operation
    in
    let application_mode =
      Apply.Partial_construction {predecessor_fitness = block_header.fitness}
    in
    let application_state =
      Apply.
        {
          ctxt = context;
          chain_id;
          mode = application_mode;
          op_count = 0;
          migration_balance_updates = [];
          liquidity_baking_toggle_ema =
            Per_block_votes.Liquidity_baking_toggle_EMA.zero;
          implicit_operations_results = [];
        }
    in
    let* _ctxt, op_metadata =
      Apply.apply_operation application_state oph packed_operation
    in
    return (packed_operation.protocol_data, op_metadata)

  (*

       The execution of an operation depends on the state of the
       cache. In particular, gas consumption is usually impacted by
       cache hits and misses.

       Unfortunately, the state of the cache is different between the
       context at operation-creation time and the context when is
       included in a block.

       Therefore, the simulation tries to predict the state of the
       cache in a [time_in_blocks] assumed to be close to the inclusion
       time of the operation.

    *)
  let simulate_operation_service rpc_ctxt params
      (blocks_before_activation, op, chain_id, time_in_blocks) =
    let open Lwt_result_syntax in
    let {Services_registration.context; _} = rpc_ctxt in
    let* context =
      Cache.Admin.future_cache_expectation
        context
        ~time_in_blocks
        ?blocks_before_activation
    in
    run_operation_service
      {rpc_ctxt with context}
      (object
         method version = params#version
      end)
      (op, chain_id)

  let default_from_context ctxt get =
    let open Lwt_result_syntax in
    function None -> get ctxt | Some x -> return x

  (* A convenience type for return values of [ensure_contracts_exist] below. *)
  type run_code_config = {
    balance : Tez.t;
    self : Contract_hash.t;
    payer : Signature.public_key_hash;
    sender : Contract.t;
  }

  (* 4_000_000 ꜩ *)
  let default_balance = Tez.of_mutez_exn 4_000_000_000_000L

  let pack_data_impl ?(allow_forged_lazy_storage_id = true) ctxt ~gas ~data ~ty
      =
    let open Lwt_result_syntax in
    let open Script_ir_translator in
    let ctxt =
      match gas with
      | None -> Gas.set_unlimited ctxt
      | Some gas -> Gas.set_limit ctxt gas
    in
    let*? Ex_ty typ, ctxt =
      parse_packable_ty ctxt ~legacy:true (Micheline.root ty)
    in
    let* data, ctxt =
      parse_data
        ctxt
        ~elab_conf:(elab_conf ~legacy:true ())
        ~allow_forged_tickets:true
        ~allow_forged_lazy_storage_id
        typ
        (Micheline.root data)
    in
    let+ bytes, ctxt = Script_ir_translator.pack_data ctxt typ data in
    (bytes, Gas.level ctxt)

  let register () =
    let open Lwt_result_syntax in
    let originate_dummy_contract ctxt script balance =
      let ctxt = Origination_nonce.init ctxt Operation_hash.zero in
      let*? ctxt, dummy_contract_hash =
        Contract.fresh_contract_from_current_nonce ctxt
      in
      let dummy_contract = Contract.Originated dummy_contract_hash in
      let* ctxt =
        Contract.raw_originate
          ctxt
          ~prepaid_bootstrap_storage:false
          dummy_contract_hash
          ~script:(script, None)
      in
      let+ ctxt, _ =
        Token.transfer
          ~origin:Simulation
          ctxt
          `Minted
          (`Contract dummy_contract)
          balance
      in
      (ctxt, dummy_contract_hash)
    in
    let originate_dummy_contracts ctxt =
      List.fold_left_es
        (fun ctxt {S.address; ty} ->
          Contract.raw_originate
            ctxt
            ~prepaid_bootstrap_storage:false
            address
            (* We reuse the default script from View_helpers because
               the purpose is the same; we only care about having a
               script declaring the correct parameter type but we will
               never actually run the script. *)
            ~script:(View_helpers.make_tzip4_viewer_script ty, None))
        ctxt
    in
    let initialize_big_maps ctxt big_maps =
      let* ctxt, (big_map_diff : Lazy_storage.diffs) =
        List.fold_left_es
          (fun (ctxt, big_map_diff_tl) {S.id; kty; vty; items} ->
            let open Script_ir_translator in
            let items = Micheline.root items in
            let init =
              Lazy_storage.(Alloc Big_map.{key_type = kty; value_type = vty})
            in
            let*? Ex_comparable_ty key_comparable_type, ctxt =
              parse_comparable_ty ctxt (Micheline.root kty)
            in
            let*? Ex_ty value_type, ctxt =
              parse_big_map_value_ty ctxt ~legacy:false (Micheline.root vty)
            in
            (* Typecheck the update seq to check that the values are well-typed and the keys are sorted *)
            let*? map_ty =
              Script_typed_ir.map_t (-1) key_comparable_type value_type
            in
            let* _, ctxt =
              parse_data
                ctxt
                ~elab_conf:(Script_ir_translator_config.make ~legacy:false ())
                ~allow_forged_tickets:true
                ~allow_forged_lazy_storage_id:true
                map_ty
                items
            in
            let items =
              match items with
              | Micheline.Seq (_, items) -> items
              | _ -> assert false
            in
            (* Build a big_map_diff *)
            let+ ctxt, updates =
              List.fold_left_es
                (fun (ctxt, acc) key_value ->
                  let open Micheline in
                  let key, value =
                    match key_value with
                    | Prim (_, Michelson_v1_primitives.D_Elt, [key; value], _)
                      ->
                        (key, value)
                    | _ -> assert false
                  in
                  let* k, ctxt =
                    parse_comparable_data ctxt key_comparable_type key
                  in
                  let+ key_hash, ctxt = hash_data ctxt key_comparable_type k in
                  let key = Micheline.strip_locations key in
                  let value = Some (Micheline.strip_locations value) in
                  (ctxt, Big_map.{key; key_hash; value} :: acc))
                (ctxt, [])
                items
            in
            ( ctxt,
              Lazy_storage.(
                make Big_map id (Update {init; updates = List.rev updates}))
              :: big_map_diff_tl ))
          (ctxt, [])
          big_maps
      in
      let+ ctxt, _size_change = Lazy_storage.apply ctxt big_map_diff in
      ctxt
    in
    let sender_and_payer ~sender_opt ~payer_opt ~default_sender =
      match (sender_opt, payer_opt) with
      | None, None ->
          (Contract.Originated default_sender, Signature.Public_key_hash.zero)
      | Some c, None -> (c, Signature.Public_key_hash.zero)
      | None, Some c -> (Contract.Implicit c, c)
      | Some sender, Some payer -> (sender, payer)
    in
    let compute_step_constants ctxt ~balance ~amount ~chain_id ~sender_opt
        ~payer_opt ~self ~now_opt ~level_opt =
      let sender, payer =
        sender_and_payer ~sender_opt ~payer_opt ~default_sender:self
      in
      let now =
        match now_opt with None -> Script_timestamp.now ctxt | Some t -> t
      in
      let level =
        match level_opt with
        | None ->
            (Level.current ctxt).level |> Raw_level.to_int32
            |> Script_int.of_int32 |> Script_int.abs
        | Some z -> z
      in
      let open Script_interpreter in
      let sender = Destination.Contract sender in
      (ctxt, {sender; payer; self; amount; balance; chain_id; now; level})
    in
    let configure_gas_and_step_constants ctxt ~script ~gas_opt ~balance ~amount
        ~chain_id ~sender_opt ~payer_opt ~self_opt ~now_opt ~level_opt =
      let gas =
        match gas_opt with
        | Some gas -> gas
        | None -> Constants.hard_gas_limit_per_operation ctxt
      in
      let ctxt = Gas.set_limit ctxt gas in
      let+ ctxt, self, balance =
        match self_opt with
        | None ->
            let balance = Option.value ~default:default_balance balance in
            let+ ctxt, addr = originate_dummy_contract ctxt script balance in
            (ctxt, addr, balance)
        | Some addr ->
            let+ bal =
              default_from_context
                ctxt
                (fun c -> Contract.get_balance c @@ Contract.Originated addr)
                balance
            in
            (ctxt, addr, bal)
      in
      compute_step_constants
        ctxt
        ~balance
        ~amount
        ~chain_id
        ~sender_opt
        ~payer_opt
        ~self
        ~now_opt
        ~level_opt
    in
    let script_entrypoint_type ctxt expr entrypoint =
      let ctxt = Gas.set_unlimited ctxt in
      let legacy = false in
      let open Script_ir_translator in
      let* {arg_type; _}, ctxt = parse_toplevel ctxt expr in
      let*? Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _ =
        parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
      in
      let*? r, _ctxt =
        Gas_monad.run ctxt
        @@ Script_ir_translator.find_entrypoint
             ~error_details:(Informative ())
             arg_type
             entrypoints
             entrypoint
      in
      let*? (Ex_ty_cstr {original_type_expr; _}) = r in
      return @@ Micheline.strip_locations original_type_expr
    in
    let script_view_type ctxt contract expr view =
      let ctxt = Gas.set_unlimited ctxt in
      let open Script_ir_translator in
      let* {views; _}, _ = parse_toplevel ctxt expr in
      let*? view_name = Script_string.of_string view in
      match Script_map.get view_name views with
      | None ->
          Environment.Error_monad.tzfail
            (View_helpers.View_not_found (contract, view))
      | Some Script_typed_ir.{input_ty; output_ty; _} ->
          return (input_ty, output_ty)
    in
    Registration.register0
      ~chunked:true
      S.run_code
      (fun
        ctxt
        ()
        ( ( code,
            storage,
            parameter,
            amount,
            balance,
            chain_id,
            sender_opt,
            payer_opt,
            self_opt,
            entrypoint ),
          ( unparsing_mode,
            gas_opt,
            now_opt,
            level_opt,
            other_contracts,
            extra_big_maps ) )
      ->
        let unparsing_mode = Option.value ~default:Readable unparsing_mode in
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let storage = Script.lazy_expr storage in
        let code = Script.lazy_expr code in
        let* ctxt, step_constants =
          configure_gas_and_step_constants
            ctxt
            ~script:{storage; code}
            ~gas_opt
            ~balance
            ~amount
            ~chain_id
            ~sender_opt
            ~payer_opt
            ~self_opt
            ~now_opt
            ~level_opt
        in
        let+ ( {
                 script = _;
                 code_size = _;
                 Script_interpreter.storage;
                 operations;
                 lazy_storage_diff;
                 ticket_diffs = _;
                 ticket_receipt = _;
                 address_registry_diff = _;
               },
               _ ) =
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~cached_script:None
            ~script:{storage; code}
            ~entrypoint
            ~parameter
            ~internal:true
        in
        ( storage,
          Apply_internal_results.packed_internal_operations operations,
          lazy_storage_diff )) ;
    Registration.register0
      ~chunked:true
      S.trace_code
      (fun
        ctxt
        ()
        ( ( code,
            storage,
            parameter,
            amount,
            balance,
            chain_id,
            sender_opt,
            payer_opt,
            self_opt,
            entrypoint ),
          ( unparsing_mode,
            gas_opt,
            now_opt,
            level_opt,
            other_contracts,
            extra_big_maps ) )
      ->
        let unparsing_mode = Option.value ~default:Readable unparsing_mode in
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let storage = Script.lazy_expr storage in
        let code = Script.lazy_expr code in
        let* ctxt, step_constants =
          configure_gas_and_step_constants
            ctxt
            ~script:{storage; code}
            ~gas_opt
            ~balance
            ~amount
            ~chain_id
            ~sender_opt
            ~payer_opt
            ~self_opt
            ~now_opt
            ~level_opt
        in
        let module Unparsing_mode = struct
          let unparsing_mode = unparsing_mode
        end in
        let module Interp = Traced_interpreter (Unparsing_mode) in
        let+ ( ( {
                   script = _;
                   code_size = _;
                   Script_interpreter.storage;
                   operations;
                   lazy_storage_diff;
                   ticket_diffs = _;
                   ticket_receipt = _;
                   address_registry_diff = _;
                 },
                 _ctxt ),
               trace ) =
          Interp.execute
            ctxt
            step_constants
            ~script:{storage; code}
            ~entrypoint
            ~parameter
        in
        ( storage,
          Apply_internal_results.packed_internal_operations operations,
          trace,
          lazy_storage_diff )) ;
    Registration.register0
      ~chunked:true
      S.run_tzip4_view
      (fun
        ctxt
        ()
        ( ( contract_hash,
            entrypoint,
            input,
            chain_id,
            sender_opt,
            payer_opt,
            gas,
            unparsing_mode,
            now_opt,
            level_opt ),
          (other_contracts, extra_big_maps) )
      ->
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let* ctxt, script_opt = Contract.get_script ctxt contract_hash in
        let*? script =
          Option.fold
            ~some:Result_syntax.return
            ~none:
              (Environment.Error_monad.Result_syntax.tzfail
                 View_helpers.Viewed_contract_has_no_script)
            script_opt
        in
        let*? decoded_script = Script_repr.(force_decode script.code) in
        let* view_ty = script_entrypoint_type ctxt decoded_script entrypoint in
        let*? ty = View_helpers.extract_view_output_type entrypoint view_ty in
        let contract = Contract.Originated contract_hash in
        let* balance = Contract.get_balance ctxt contract in
        let* ctxt, viewer_contract =
          Error_monad.trace View_helpers.View_callback_origination_failed
          @@ originate_dummy_contract
               ctxt
               (View_helpers.make_tzip4_viewer_script ty)
               Tez.zero
        in
        let ctxt, step_constants =
          compute_step_constants
            ctxt
            ~balance
            ~amount:Tez.zero
            ~chain_id
            ~sender_opt
            ~payer_opt
            ~self:contract_hash
            ~now_opt
            ~level_opt
        in
        let gas =
          Option.value
            ~default:(Constants.hard_gas_limit_per_operation ctxt)
            gas
        in
        let ctxt = Gas.set_limit ctxt gas in
        let parameter =
          View_helpers.make_view_parameter
            (Micheline.root input)
            (Contract.Originated viewer_contract)
        in
        let* ( {
                 Script_interpreter.operations;
                 script = _;
                 code_size = _;
                 storage = _;
                 lazy_storage_diff = _;
                 ticket_diffs = _;
                 ticket_receipt = _;
                 address_registry_diff = _;
               },
               _ctxt ) =
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~script
            ~cached_script:None
            ~entrypoint
            ~parameter
            ~internal:true
        in
        Lwt.return
          (View_helpers.extract_parameter_from_operations
             entrypoint
             operations
             viewer_contract)) ;
    Registration.register0
      ~chunked:true
      S.run_script_view
      (fun
        ctxt
        ()
        ( ( contract_hash,
            view,
            input,
            unlimited_gas,
            chain_id,
            sender_opt,
            payer_opt,
            gas,
            unparsing_mode,
            now_opt ),
          (level_opt, other_contracts, extra_big_maps) )
      ->
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let* ctxt, script_opt = Contract.get_script ctxt contract_hash in
        let*? script =
          Option.fold
            ~some:Result_syntax.return
            ~none:(Error_monad.error View_helpers.Viewed_contract_has_no_script)
            script_opt
        in
        let*? decoded_script = Script_repr.(force_decode script.code) in
        let contract = Contract.Originated contract_hash in
        let* input_ty, output_ty =
          script_view_type ctxt contract_hash decoded_script view
        in
        let* balance = Contract.get_balance ctxt contract in
        let ctxt, step_constants =
          compute_step_constants
            ctxt
            ~balance
            ~amount:Tez.zero
            ~chain_id
            ~sender_opt
            ~payer_opt
            ~self:contract_hash
            ~now_opt
            ~level_opt
        in
        (* Using [Gas.set_unlimited] won't work, since the interpreter doesn't
           use this mode (see !4034#note_774734253) and still consumes gas.
           Our best shot to emulate this is to use the maximum amount of
           milligas possible which is represented by [2^62 - 1] according to
           [Saturation_repr.saturated], which is [max_int]. *)
        let max_gas = Gas.fp_of_milligas_int max_int in
        let gas =
          Option.value
            ~default:(Constants.hard_gas_limit_per_operation ctxt)
            gas
        in
        let ctxt =
          if unlimited_gas then Gas.set_limit ctxt max_gas
          else Gas.set_limit ctxt gas
        in
        let viewer_script =
          View_helpers.make_michelson_viewer_script
            contract
            view
            input
            input_ty
            output_ty
        in
        let parameter =
          Micheline.(strip_locations (Prim (0, Script.D_Unit, [], [])))
        in
        let* ( {
                 Script_interpreter.operations = _;
                 script = _;
                 code_size = _;
                 storage;
                 lazy_storage_diff = _;
                 ticket_diffs = _;
                 ticket_receipt = _;
                 address_registry_diff = _;
               },
               _ctxt ) =
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~script:viewer_script
            ~cached_script:None
            ~entrypoint:Entrypoint.default
            ~parameter
            ~internal:true
        in
        let*? value = View_helpers.extract_value_from_storage storage in
        return (Micheline.strip_locations value)) ;
    Registration.register0
      ~chunked:false
      S.typecheck_code
      (fun ctxt () (expr, maybe_gas, legacy, show_types) ->
        let ctxt =
          match maybe_gas with
          | None -> Gas.set_unlimited ctxt
          | Some gas -> Gas.set_limit ctxt gas
        in
        let+ res, ctxt =
          Script_ir_translator.typecheck_code ~legacy ~show_types ctxt expr
        in
        (res, Gas.level ctxt)) ;
    Registration.register0
      ~chunked:false
      S.script_size
      (fun ctxt () (expr, storage, maybe_gas, legacy) ->
        let ctxt =
          match maybe_gas with
          | None -> Gas.set_unlimited ctxt
          | Some gas -> Gas.set_limit ctxt gas
        in
        let elab_conf = elab_conf ~legacy () in
        let code = Script.lazy_expr expr in
        let* ( Ex_code
                 (Code
                    {
                      code;
                      arg_type;
                      storage_type;
                      views;
                      entrypoints;
                      code_size;
                    }),
               ctxt ) =
          Script_ir_translator.parse_code ~elab_conf ctxt ~code
        in
        let* storage, _ =
          Script_ir_translator.parse_data
            ~elab_conf
            ~allow_forged_tickets:true
            ~allow_forged_lazy_storage_id:true
            ctxt
            storage_type
            (Micheline.root storage)
        in
        let script =
          Script_ir_translator.Ex_script
            (Script
               {
                 code;
                 arg_type;
                 storage_type;
                 views;
                 entrypoints;
                 code_size;
                 storage;
               })
        in
        let size, cost = Script_ir_translator.script_size script in
        let*? _ctxt = Gas.consume ctxt cost in
        return size) ;

    Registration.register0
      ~chunked:false
      S.typecheck_data
      (fun ctxt () (data, ty, maybe_gas, legacy) ->
        let ctxt =
          match maybe_gas with
          | None -> Gas.set_unlimited ctxt
          | Some gas -> Gas.set_limit ctxt gas
        in
        let+ ctxt = typecheck_data ~legacy ctxt (data, ty) in
        Gas.level ctxt) ;
    Registration.register0
      ~chunked:true
      S.pack_data
      (fun ctxt () (data, ty, gas) -> pack_data_impl ctxt ~gas ~data ~ty) ;
    Registration.register0
      ~chunked:true
      S.normalize_data
      (fun
        ctxt
        ()
        (expr, typ, unparsing_mode, legacy, other_contracts, extra_big_maps)
      ->
        let open Script_ir_translator in
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let ctxt = Gas.set_unlimited ctxt in
        let*? Ex_ty typ, ctxt =
          Script_ir_translator.parse_any_ty ctxt ~legacy (Micheline.root typ)
        in
        let* data, ctxt =
          parse_data
            ctxt
            ~elab_conf:(elab_conf ~legacy ())
            ~allow_forged_tickets:true
            ~allow_forged_lazy_storage_id:true
            typ
            (Micheline.root expr)
        in
        let+ normalized, _ctxt =
          Script_ir_translator.unparse_data ctxt unparsing_mode typ data
        in
        normalized) ;
    Registration.register0
      ~chunked:true
      S.normalize_stack
      (fun
        ctxt
        ()
        (stack, unparsing_mode, legacy, other_contracts, extra_big_maps)
      ->
        let ctxt = Gas.set_unlimited ctxt in
        let nodes =
          List.map (fun (a, b) -> (Micheline.root a, Micheline.root b)) stack
        in
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let* Normalize_stack.Ex_stack (st_ty, x, st), ctxt =
          Normalize_stack.parse_stack ctxt ~legacy nodes
        in
        let+ normalized, _ctxt =
          Normalize_stack.unparse_stack ctxt unparsing_mode st_ty x st
        in
        normalized) ;
    Registration.register0
      ~chunked:true
      S.normalize_script
      (fun ctxt () (script, unparsing_mode) ->
        let ctxt = Gas.set_unlimited ctxt in
        let+ normalized, _ctxt =
          Script_ir_translator.unparse_code
            ctxt
            unparsing_mode
            (Micheline.root script)
        in
        normalized) ;
    Registration.register0 ~chunked:true S.normalize_type (fun ctxt () typ ->
        let open Script_typed_ir in
        let ctxt = Gas.set_unlimited ctxt in
        (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
        let*? Ex_ty typ, _ctxt =
          Script_ir_translator.parse_ty
            ctxt
            ~legacy:true
            ~allow_lazy_storage:true
            ~allow_operation:true
            ~allow_contract:true
            ~allow_ticket:true
            (Micheline.root typ)
        in
        let normalized = Unparse_types.unparse_ty ~loc:() typ in
        return @@ Micheline.strip_locations normalized) ;
    Registration.register0
      ~chunked:true
      S.run_instr
      (fun
        ctxt
        ()
        ( ( input,
            code,
            chain_id,
            gas_opt,
            now_opt,
            level_opt,
            sender_opt,
            source_opt,
            self_opt,
            parameter_opt ),
          ( amount,
            balance,
            other_contracts,
            extra_big_maps,
            unparsing_mode,
            legacy ) )
      ->
        let unparsing_mode = Option.value ~default:Readable unparsing_mode in
        let other_contracts = Option.value ~default:[] other_contracts in
        let* ctxt = originate_dummy_contracts ctxt other_contracts in
        let extra_big_maps = Option.value ~default:[] extra_big_maps in
        let* ctxt = initialize_big_maps ctxt extra_big_maps in
        let parameter =
          Option.value
            ~default:
              (Micheline.strip_locations
                 (Prim (0, Michelson_v1_primitives.T_unit, [], [])))
            parameter_opt
        in
        let*? Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, ctxt =
          Script_ir_translator.parse_parameter_ty_and_entrypoints
            ctxt
            ~legacy
            (Micheline.root parameter)
        in
        let* ctxt, step_constants =
          configure_gas_and_step_constants
            ctxt
            ~script:(View_helpers.make_tzip4_viewer_script parameter)
            ~gas_opt
            ~balance
            ~amount
            ~chain_id
            ~sender_opt
            ~payer_opt:source_opt
            ~self_opt
            ~now_opt
            ~level_opt
        in
        let input_nodes =
          List.map (fun (a, b) -> (Micheline.root a, Micheline.root b)) input
        in
        let* Normalize_stack.Ex_stack (st_ty, x, st), ctxt =
          Normalize_stack.parse_stack ctxt ~legacy input_nodes
        in
        let* j, ctxt =
          Script_ir_translator.parse_instr
            ~elab_conf:(Script_ir_translator_config.make ~legacy ())
            (Script_tc_context.toplevel
               ~storage_type:Script_typed_ir.unit_t
               ~param_type:arg_type
               ~entrypoints)
            ctxt
            (Micheline.root code)
            st_ty
        in
        match j with
        | Failed {descr} -> (
            let impossible_stack_ty =
              Script_typed_ir.(Item_t (never_t, Bot_t))
            in
            let descr = descr impossible_stack_ty in
            let descr = Script_ir_translator.close_descr descr in
            let* absurd =
              Script_interpreter.Internals.step_descr
                None
                ctxt
                step_constants
                descr
                x
                st
            in
            match absurd with _ -> .)
        | Typed descr ->
            let descr = Script_ir_translator.close_descr descr in
            let* y, output_st, _ctxt =
              Script_interpreter.Internals.step_descr
                None
                ctxt
                step_constants
                descr
                x
                st
            in
            let+ output, ctxt =
              Normalize_stack.unparse_stack
                ctxt
                unparsing_mode
                descr.kaft
                y
                output_st
            in
            (output, Gas.level ctxt)) ;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/3364

       Should [run_operation] be registered at successor level? *)
    Registration.register0_fullctxt
      ~chunked:true
      S.run_operation
      run_operation_service ;
    Registration.register0_fullctxt_successor_level
      ~chunked:true
      S.simulate_operation
      simulate_operation_service ;
    Registration.register0
      ~chunked:true
      S.entrypoint_type
      (fun ctxt () (expr, entrypoint) ->
        script_entrypoint_type ctxt expr entrypoint) ;
    Registration.register0 ~chunked:true S.list_entrypoints (fun ctxt () expr ->
        let ctxt = Gas.set_unlimited ctxt in
        let legacy = false in
        let open Script_ir_translator in
        let* {arg_type; _}, ctxt = parse_toplevel ctxt expr in
        let*? Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, _ =
          parse_parameter_ty_and_entrypoints ctxt ~legacy arg_type
        in
        return
        @@
        let unreachable_entrypoint, map =
          Script_ir_translator.list_entrypoints_uncarbonated
            arg_type
            entrypoints
        in
        ( unreachable_entrypoint,
          Entrypoint.Map.fold
            (fun entry (_ex_ty, original_type_expr) acc ->
              ( Entrypoint.to_string entry,
                Micheline.strip_locations original_type_expr )
              :: acc)
            map
            [] ))

  let run_code ~unparsing_mode ~gas ~entrypoint ~balance ~other_contracts
      ~extra_big_maps ~script ~storage ~input ~amount ~chain_id ~sender ~payer
      ~self ~now ~level ctxt block =
    RPC_context.make_call0
      S.run_code
      ctxt
      block
      ()
      ( ( script,
          storage,
          input,
          amount,
          balance,
          chain_id,
          sender,
          payer,
          self,
          entrypoint ),
        (unparsing_mode, gas, now, level, other_contracts, extra_big_maps) )

  let trace_code ~unparsing_mode ~gas ~entrypoint ~balance ~other_contracts
      ~extra_big_maps ~script ~storage ~input ~amount ~chain_id ~sender ~payer
      ~self ~now ~level ctxt block =
    RPC_context.make_call0
      S.trace_code
      ctxt
      block
      ()
      ( ( script,
          storage,
          input,
          amount,
          balance,
          chain_id,
          sender,
          payer,
          self,
          entrypoint ),
        (unparsing_mode, gas, now, level, other_contracts, extra_big_maps) )

  let run_tzip4_view ~gas ~other_contracts ~extra_big_maps ~contract ~entrypoint
      ~input ~chain_id ~now ~level ~sender ~payer ~unparsing_mode ctxt block =
    RPC_context.make_call0
      S.run_tzip4_view
      ctxt
      block
      ()
      ( ( contract,
          entrypoint,
          input,
          chain_id,
          sender,
          payer,
          gas,
          unparsing_mode,
          now,
          level ),
        (other_contracts, extra_big_maps) )

  (** [run_script_view] is an helper function to call the corresponding
        RPC. *)
  let run_script_view ~gas ~other_contracts ~extra_big_maps ~contract ~view
      ~input ~unlimited_gas ~chain_id ~now ~level ~sender ~payer ~unparsing_mode
      ctxt block =
    RPC_context.make_call0
      S.run_script_view
      ctxt
      block
      ()
      ( ( contract,
          view,
          input,
          unlimited_gas,
          chain_id,
          sender,
          payer,
          gas,
          unparsing_mode,
          now ),
        (level, other_contracts, extra_big_maps) )

  let run_instr ~gas ~legacy ~input ~code ~chain_id ~now ~level ~unparsing_mode
      ~source ~sender ~self ~parameter ~amount ~balance ~other_contracts
      ~extra_big_maps ctxt block =
    RPC_context.make_call0
      S.run_instr
      ctxt
      block
      ()
      ( (input, code, chain_id, gas, now, level, sender, source, self, parameter),
        ( amount,
          balance,
          other_contracts,
          extra_big_maps,
          unparsing_mode,
          legacy ) )

  let typecheck_code ~gas ~legacy ~script ~show_types ctxt block =
    RPC_context.make_call0
      S.typecheck_code
      ctxt
      block
      ()
      (script, gas, legacy, show_types)

  let script_size ~gas ~legacy ~script ~storage ctxt block =
    RPC_context.make_call0
      S.script_size
      ctxt
      block
      ()
      (script, storage, gas, legacy)

  let typecheck_data ~gas ~legacy ~data ~ty ctxt block =
    RPC_context.make_call0 S.typecheck_data ctxt block () (data, ty, gas, legacy)

  let pack_data ~gas ~data ~ty ctxt block =
    RPC_context.make_call0 S.pack_data ctxt block () (data, ty, gas)

  let normalize_data ~legacy ~other_contracts ~extra_big_maps ~data ~ty
      ~unparsing_mode ctxt block =
    RPC_context.make_call0
      S.normalize_data
      ctxt
      block
      ()
      (data, ty, unparsing_mode, legacy, other_contracts, extra_big_maps)

  let normalize_stack ~legacy ~other_contracts ~extra_big_maps ~stack
      ~unparsing_mode ctxt block =
    RPC_context.make_call0
      S.normalize_stack
      ctxt
      block
      ()
      (stack, unparsing_mode, legacy, other_contracts, extra_big_maps)

  let normalize_script ~script ~unparsing_mode ctxt block =
    RPC_context.make_call0
      S.normalize_script
      ctxt
      block
      ()
      (script, unparsing_mode)

  let normalize_type ~ty ctxt block =
    RPC_context.make_call0 S.normalize_type ctxt block () ty

  let run_operation ~op ~chain_id ?version ctxt block =
    let open Lwt_result_syntax in
    let* run_operation =
      RPC_context.make_call0
        S.run_operation
        ctxt
        block
        (object
           method version = version
        end)
        (op, chain_id)
    in
    return run_operation

  let simulate_operation ~op ~chain_id ~latency ?version
      ?(successor_level = false) ?blocks_before_activation ctxt block =
    let open Lwt_result_syntax in
    let* simulate_operation =
      RPC_context.make_call0
        S.simulate_operation
        ctxt
        block
        (object
           method version = version

           method successor_level = successor_level
        end)
        (blocks_before_activation, op, chain_id, latency)
    in
    return simulate_operation

  let entrypoint_type ~script ~entrypoint ctxt block =
    RPC_context.make_call0 S.entrypoint_type ctxt block () (script, entrypoint)

  let list_entrypoints ctxt block ~script =
    RPC_context.make_call0 S.list_entrypoints ctxt block () script
end

module Contract = struct
  let ticket_balances_encoding =
    let open Data_encoding in
    list
      (merge_objs Ticket_token.unparsed_token_encoding (obj1 (req "amount" n)))

  module S = struct
    let path =
      (RPC_path.(open_root / "context" / "contracts")
        : RPC_context.t RPC_path.context)

    let get_storage_normalized =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Access the data of the contract and normalize it using the \
           requested unparsing mode."
        ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
        ~query:RPC_query.empty
        ~output:(option Script.expr_encoding)
        RPC_path.(path /: Contract.rpc_arg / "storage" / "normalized")

    let get_script_normalized =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Access the script of the contract and normalize it using the \
           requested unparsing mode."
        ~input:
          (obj2
             (req "unparsing_mode" unparsing_mode_encoding)
             (dft "normalize_types" bool false))
        ~query:RPC_query.empty
        ~output:(option Script.encoding)
        RPC_path.(path /: Contract.rpc_arg / "script" / "normalized")

    let get_used_storage_space =
      let open Data_encoding in
      RPC_service.get_service
        ~description:"Access the used storage space of the contract."
        ~query:RPC_query.empty
        ~output:(option z)
        RPC_path.(path /: Contract.rpc_arg / "storage" / "used_space")

    let get_paid_storage_space =
      let open Data_encoding in
      RPC_service.get_service
        ~description:"Access the paid storage space of the contract."
        ~query:RPC_query.empty
        ~output:(option z)
        RPC_path.(path /: Contract.rpc_arg / "storage" / "paid_space")

    let ticket_balance =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Access the contract's balance of ticket with specified ticketer, \
           content type, and content."
        ~query:RPC_query.empty
        ~input:Ticket_token.unparsed_token_encoding
        ~output:n
        RPC_path.(path /: Contract.rpc_arg / "ticket_balance")

    let all_ticket_balances =
      RPC_service.get_service
        ~description:
          "Access the complete list of tickets owned by the given contract by \
           scanning the contract's storage."
        ~query:RPC_query.empty
        ~output:ticket_balances_encoding
        RPC_path.(path /: Contract.rpc_arg / "all_ticket_balances")
  end

  let get_contract contract f =
    let open Lwt_result_syntax in
    match contract with
    | Contract.Implicit _ -> return_none
    | Contract.Originated contract -> f contract

  let register () =
    let open Lwt_result_syntax in
    (* Patched RPC: get_storage *)
    Registration.register1
      ~chunked:true
      S.get_storage_normalized
      (fun ctxt contract () unparsing_mode ->
        get_contract contract @@ fun contract ->
        let* ctxt, script = Contract.get_script ctxt contract in
        match script with
        | None -> return_none
        | Some script ->
            let ctxt = Gas.set_unlimited ctxt in
            let open Script_ir_translator in
            let* Ex_script (Script {storage; storage_type; _}), ctxt =
              parse_script
                ctxt
                ~elab_conf:(elab_conf ~legacy:true ())
                ~allow_forged_tickets_in_storage:true
                ~allow_forged_lazy_storage_id_in_storage:true
                script
            in
            let+ storage, _ctxt =
              unparse_data ctxt unparsing_mode storage_type storage
            in
            Some storage) ;
    (* Patched RPC: get_script *)
    Registration.register1
      ~chunked:true
      S.get_script_normalized
      (fun ctxt contract () (unparsing_mode, normalize_types) ->
        get_contract contract @@ fun contract ->
        let* ctxt, script = Contract.get_script ctxt contract in
        match script with
        | None -> return_none
        | Some script ->
            let ctxt = Gas.set_unlimited ctxt in
            let+ script, _ctxt =
              Script_ir_translator.parse_and_unparse_script_unaccounted
                ctxt
                ~legacy:true
                ~allow_forged_tickets_in_storage:true
                ~allow_forged_lazy_storage_id_in_storage:true
                unparsing_mode
                ~normalize_types
                script
            in
            Some script) ;
    Registration.register1
      ~chunked:false
      S.get_used_storage_space
      (fun ctxt contract () () ->
        get_contract contract @@ fun _ ->
        let+ x = Contract.used_storage_space ctxt contract in
        Some x) ;
    Registration.register1
      ~chunked:false
      S.get_paid_storage_space
      (fun ctxt contract () () ->
        get_contract contract @@ fun _ ->
        let+ x = Contract.paid_storage_space ctxt contract in
        Some x) ;
    Registration.register1
      ~chunked:false
      S.ticket_balance
      (fun ctxt contract () Ticket_token.{ticketer; contents_type; contents} ->
        let* ticket_hash, ctxt =
          Ticket_balance_key.make
            ctxt
            ~owner:(Contract contract)
            ~ticketer
            ~contents_type:(Micheline.root contents_type)
            ~contents:(Micheline.root contents)
        in
        let+ amount, _ctxt = Ticket_balance.get_balance ctxt ticket_hash in
        Option.value amount ~default:Z.zero) ;
    Registration.opt_register1
      ~chunked:false
      S.all_ticket_balances
      (fun ctxt contract () () ->
        get_contract contract @@ fun contract ->
        let* ctxt, script = Contract.get_script ctxt contract in
        match script with
        | None -> return_none
        | Some script ->
            let* Ex_script (Script {storage; storage_type; _}), ctxt =
              Script_ir_translator.parse_script
                ctxt
                ~elab_conf:(elab_conf ~legacy:true ())
                ~allow_forged_tickets_in_storage:true
                ~allow_forged_lazy_storage_id_in_storage:true
                script
            in
            let*? has_tickets, ctxt =
              Ticket_scanner.type_has_tickets ctxt storage_type
            in
            let* ticket_token_map, ctxt =
              Ticket_accounting.ticket_balances_of_value
                ctxt
                ~include_lazy:true
                has_tickets
                storage
            in
            let+ ticket_balances, _ctxt =
              Ticket_token_map.fold_es
                ctxt
                (fun ctxt acc ex_token amount ->
                  let+ unparsed_token, ctxt =
                    Ticket_token_unparser.unparse ctxt ex_token
                  in
                  ((unparsed_token, amount) :: acc, ctxt))
                []
                ticket_token_map
            in
            Some ticket_balances)

  let get_storage_normalized ctxt block ~contract ~unparsing_mode =
    RPC_context.make_call1
      S.get_storage_normalized
      ctxt
      block
      (Contract.Originated contract)
      ()
      unparsing_mode

  let get_script_normalized ctxt block ~contract ~unparsing_mode
      ~normalize_types =
    RPC_context.make_call1
      S.get_script_normalized
      ctxt
      block
      (Contract.Originated contract)
      ()
      (unparsing_mode, normalize_types)

  let get_used_storage_space ctxt block ~contract =
    RPC_context.make_call1
      S.get_used_storage_space
      ctxt
      block
      (Contract.Originated contract)
      ()
      ()

  let get_paid_storage_space ctxt block ~contract =
    RPC_context.make_call1
      S.get_paid_storage_space
      ctxt
      block
      (Contract.Originated contract)
      ()
      ()

  let get_ticket_balance ctxt block contract key =
    RPC_context.make_call1 S.ticket_balance ctxt block contract () key

  let get_all_ticket_balances ctxt block contract =
    RPC_context.make_call1
      S.all_ticket_balances
      ctxt
      block
      (Contract.Originated contract)
      ()
      ()
end

module Big_map = struct
  module S = struct
    let path =
      (RPC_path.(open_root / "context" / "big_maps")
        : RPC_context.t RPC_path.context)

    let big_map_get_normalized =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Access the value associated with a key in a big map, normalize the \
           output using the requested unparsing mode."
        ~query:RPC_query.empty
        ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
        ~output:Script.expr_encoding
        RPC_path.(
          path /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg / "normalized")
  end

  let register () =
    let open Lwt_result_syntax in
    Registration.register2
      ~chunked:true
      S.big_map_get_normalized
      (fun ctxt id key () unparsing_mode ->
        let open Script_ir_translator in
        let ctxt = Gas.set_unlimited ctxt in
        let* ctxt, types = Big_map.exists ctxt id in
        match types with
        | None -> raise Not_found
        | Some (_, value_type) -> (
            let*? Ex_ty value_type, ctxt =
              parse_big_map_value_ty
                ctxt
                ~legacy:true
                (Micheline.root value_type)
            in
            let* _ctxt, value = Big_map.get_opt ctxt id key in
            match value with
            | None -> raise Not_found
            | Some value ->
                let* value, ctxt =
                  parse_data
                    ctxt
                    ~elab_conf:(elab_conf ~legacy:true ())
                    ~allow_forged_tickets:true
                    ~allow_forged_lazy_storage_id:true
                    value_type
                    (Micheline.root value)
                in
                let+ value, _ctxt =
                  unparse_data ctxt unparsing_mode value_type value
                in
                value))

  let big_map_get_normalized ctxt block id key ~unparsing_mode =
    RPC_context.make_call2
      S.big_map_get_normalized
      ctxt
      block
      id
      key
      ()
      unparsing_mode
end

module Protocol = struct
  module S = struct
    let path : RPC_context.t RPC_path.context =
      RPC_path.(open_root / "context" / "protocol")

    let first_level =
      let output = Raw_level.encoding in
      RPC_service.get_service
        ~description:
          "Returns the level at which the current protocol was activated."
        ~query:RPC_query.empty
        ~output
        RPC_path.(path / "first_level")
  end

  let register_first_level () =
    Registration.register0 ~chunked:false S.first_level (fun context () () ->
        Alpha_context.First_level_of_protocol.get context)

  let register () = register_first_level ()

  let first_level ctxt block =
    RPC_context.make_call0 S.first_level ctxt block () ()
end

module Sc_rollup = struct
  open Data_encoding

  module S = struct
    let prefix : RPC_context.t RPC_path.context =
      RPC_path.(open_root / "context" / "smart_rollups")

    let path_sc_rollup : (RPC_context.t, RPC_context.t * Sc_rollup.t) RPC_path.t
        =
      RPC_path.(prefix / "smart_rollup" /: Sc_rollup.Address.rpc_arg)

    let path_sc_rollups : RPC_context.t RPC_path.context =
      RPC_path.(prefix / "all")

    let kind =
      RPC_service.get_service
        ~description:"Kind of smart rollup"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Kind.encoding
        RPC_path.(path_sc_rollup / "kind")

    let genesis_info =
      RPC_service.get_service
        ~description:
          "Genesis information (level and commitment hash) for a smart rollup"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Commitment.genesis_info_encoding
        RPC_path.(path_sc_rollup / "genesis_info")

    let last_cemented_commitment_hash_with_level =
      RPC_service.get_service
        ~description:
          "Level and hash of the last cemented commitment for a smart rollup"
        ~query:RPC_query.empty
        ~output:
          (obj2
             (req "hash" Sc_rollup.Commitment.Hash.encoding)
             (req "level" Raw_level.encoding))
        RPC_path.(path_sc_rollup / "last_cemented_commitment_hash_with_level")

    let staked_on_commitment =
      RPC_service.get_service
        ~description:
          "The newest commitment on which the operator has staked on for a \
           smart rollup. Note that is can return a commitment that is before \
           the last cemented one."
        ~query:RPC_query.empty
        ~output:
          (option
             (merge_objs
                (obj1 (req "hash" Sc_rollup.Commitment.Hash.encoding))
                Sc_rollup.Commitment.encoding))
        RPC_path.(
          path_sc_rollup / "staker" /: Sc_rollup.Staker.rpc_arg
          / "staked_on_commitment")

    let commitment =
      RPC_service.get_service
        ~description:"Commitment for a smart rollup from its hash"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Commitment.encoding
        RPC_path.(
          path_sc_rollup / "commitment" /: Sc_rollup.Commitment.Hash.rpc_arg)

    let dal_slot_subscriptions =
      RPC_service.get_service
        ~description:
          "List of slot indices to which a rollup is subscribed to at a given \
           level"
        ~query:RPC_query.empty
        ~output:(Data_encoding.list Dal.Slot_index.encoding)
        RPC_path.(
          path_sc_rollup / "dal_slot_subscriptions" /: Raw_level.rpc_arg)

    let ongoing_refutation_games =
      let output =
        Sc_rollup.(
          Data_encoding.(
            list
              (obj3
                 (req "game" Game.encoding)
                 (req "alice" Staker.encoding)
                 (req "bob" Staker.encoding))))
      in
      RPC_service.get_service
        ~description:"Ongoing refutation games for a given staker"
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "staker" /: Sc_rollup.Staker.rpc_arg / "games")

    let commitments =
      let output =
        Data_encoding.(option (list Sc_rollup.Commitment.Hash.encoding))
      in
      RPC_service.get_service
        ~description:
          "List of commitments associated to a rollup for a given inbox level"
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "inbox_level" /: Raw_level.rpc_arg / "commitments")

    let stakers_ids =
      let output = Data_encoding.list Sc_rollup.Staker.Index.encoding in
      RPC_service.get_service
        ~description:"List of stakers indexes staking on a given commitment"
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "commitment" /: Sc_rollup.Commitment.Hash.rpc_arg
          / "stakers_indexes")

    let staker_id =
      let output = Sc_rollup.Staker.Index.encoding in
      RPC_service.get_service
        ~description:
          "Staker index associated to a public key hash for a given rollup"
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "staker" /: Sc_rollup.Staker.rpc_arg / "index")

    let stakers =
      let output = Data_encoding.list Sc_rollup.Staker.encoding in
      RPC_service.get_service
        ~description:"List of active stakers' public key hashes of a rollup"
        ~query:RPC_query.empty
        ~output
        RPC_path.(path_sc_rollup / "stakers")

    let conflicts =
      let output =
        Sc_rollup.(Data_encoding.list Refutation_storage.conflict_encoding)
      in
      RPC_service.get_service
        ~description:"List of stakers in conflict with the given staker"
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "staker" /: Sc_rollup.Staker.rpc_arg / "conflicts")

    let timeout =
      let output = Data_encoding.option Sc_rollup.Game.timeout_encoding in
      RPC_service.get_service
        ~description:"Returns the timeout of players."
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "staker1" /: Sc_rollup.Staker.rpc_arg_staker1
          / "staker2" /: Sc_rollup.Staker.rpc_arg_staker2 / "timeout")

    let timeout_reached =
      let output = Data_encoding.option Sc_rollup.Game.game_result_encoding in
      RPC_service.get_service
        ~description:
          "Returns whether the timeout creates a result for the game."
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "staker1" /: Sc_rollup.Staker.rpc_arg_staker1
          / "staker2" /: Sc_rollup.Staker.rpc_arg_staker2 / "timeout_reached")

    let can_be_cemented =
      let output = Data_encoding.bool in
      RPC_service.get_service
        ~description:
          "Returns true if and only if the provided commitment can be cemented."
        ~query:RPC_query.empty
        ~output
        RPC_path.(
          path_sc_rollup / "commitment" /: Sc_rollup.Commitment.Hash.rpc_arg
          / "can_be_cemented")

    let root =
      RPC_service.get_service
        ~description:"List of all originated smart rollups"
        ~query:RPC_query.empty
        ~output:(Data_encoding.list Sc_rollup.Address.encoding)
        path_sc_rollups

    let inbox =
      RPC_service.get_service
        ~description:"Inbox for the smart rollups"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Inbox.encoding
        RPC_path.(path_sc_rollups / "inbox")

    let ticket_balance =
      let open Data_encoding in
      RPC_service.post_service
        ~description:
          "Access the smart rollup's balance of ticket with specified \
           ticketer, content type, and content."
        ~query:RPC_query.empty
        ~input:Ticket_token.unparsed_token_encoding
        ~output:n
        RPC_path.(path_sc_rollup / "ticket_balance")

    let whitelist =
      RPC_service.get_service
        ~description:
          "Whitelist for private smart rollups. If the output is None then the \
           rollup is public."
        ~query:RPC_query.empty
        ~output:Data_encoding.(option Sc_rollup.Whitelist.encoding)
        RPC_path.(path_sc_rollup / "whitelist")

    let last_whitelist_update =
      RPC_service.get_service
        ~description:
          "Last whitelist update for private smart rollups. If the output is \
           None then the rollup is public."
        ~query:RPC_query.empty
        ~output:
          Data_encoding.(
            option Sc_rollup.Whitelist.last_whitelist_update_encoding)
        RPC_path.(path_sc_rollup / "last_whitelist_update")

    let consumed_outputs =
      RPC_service.get_service
        ~description:"Return the known consumed outputs of a smart rollup."
        ~query:RPC_query.empty
        ~output:Data_encoding.(list int31)
        RPC_path.(path_sc_rollup / "consumed_outputs" /: Raw_level.rpc_arg)
  end

  let kind ctxt block sc_rollup_address =
    RPC_context.make_call1 S.kind ctxt block sc_rollup_address ()

  let register_inbox () =
    let open Lwt_result_syntax in
    Registration.register0 ~chunked:true S.inbox (fun ctxt () () ->
        let+ inbox, _ctxt = Sc_rollup.Inbox.get_inbox ctxt in
        inbox)

  let register_whitelist () =
    Registration.register1 ~chunked:true S.whitelist (fun ctxt address () () ->
        Sc_rollup.Whitelist.find_whitelist_uncarbonated ctxt address)

  let register_last_whitelist_update () =
    let open Lwt_result_syntax in
    Registration.register1
      ~chunked:true
      S.last_whitelist_update
      (fun ctxt address () () ->
        let* ctxt, is_private = Sc_rollup.Whitelist.is_private ctxt address in
        if is_private then
          let* _ctxt, last_whitelist_update =
            Sc_rollup.Whitelist.get_last_whitelist_update ctxt address
          in
          return_some last_whitelist_update
        else return_none)

  let register_consumed_outputs () =
    let open Lwt_result_syntax in
    Registration.register2 ~chunked:true S.consumed_outputs
    @@ fun ctxt address outbox_level () () ->
    let* _ctxt, consumed_outputs =
      Sc_rollup.Outbox.consumed_outputs ~level:outbox_level ctxt address
    in
    return consumed_outputs

  let register_kind () =
    let open Lwt_result_syntax in
    Registration.opt_register1 ~chunked:true S.kind @@ fun ctxt address () () ->
    let+ _ctxt, kind = Alpha_context.Sc_rollup.kind ctxt address in
    Some kind

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2688 *)
  let register_genesis_info () =
    let open Lwt_result_syntax in
    Registration.register1 ~chunked:true S.genesis_info
    @@ fun ctxt address () () ->
    let+ _ctxt, genesis_info =
      Alpha_context.Sc_rollup.genesis_info ctxt address
    in
    genesis_info

  let register_last_cemented_commitment_hash_with_level () =
    let open Lwt_result_syntax in
    Registration.register1
      ~chunked:false
      S.last_cemented_commitment_hash_with_level
    @@ fun ctxt address () () ->
    let+ last_cemented_commitment, level, _ctxt =
      Alpha_context.Sc_rollup.Commitment
      .last_cemented_commitment_hash_with_level
        ctxt
        address
    in
    (last_cemented_commitment, level)

  let register_staked_on_commitment () =
    let open Lwt_result_syntax in
    Registration.register2 ~chunked:false S.staked_on_commitment
    @@ fun ctxt address staker () () ->
    let* ctxt, commitment_hash =
      Alpha_context.Sc_rollup.Stake_storage.find_staker ctxt address staker
    in
    match commitment_hash with
    | None -> return_none
    | Some commitment_hash ->
        let+ commitment, _ctxt =
          Alpha_context.Sc_rollup.Commitment.get_commitment
            ctxt
            address
            commitment_hash
        in
        Some (commitment_hash, commitment)

  let register_commitment () =
    let open Lwt_result_syntax in
    Registration.register2 ~chunked:false S.commitment
    @@ fun ctxt address commitment_hash () () ->
    let+ commitment, _ =
      Alpha_context.Sc_rollup.Commitment.get_commitment
        ctxt
        address
        commitment_hash
    in
    commitment

  let register_root () =
    Registration.register0 ~chunked:true S.root (fun context () () ->
        Sc_rollup.list_unaccounted context)

  let register_ongoing_refutation_games () =
    let open Lwt_result_syntax in
    Registration.register2
      ~chunked:false
      S.ongoing_refutation_games
      (fun context rollup staker () () ->
        let open Sc_rollup.Game.Index in
        let open Sc_rollup.Refutation_storage in
        let+ game, _ = get_ongoing_games_for_staker context rollup staker in
        List.map (fun (game, index) -> (game, index.alice, index.bob)) game)

  let register_commitments () =
    Registration.register2
      ~chunked:false
      S.commitments
      (fun context rollup inbox_level () () ->
        Sc_rollup.Stake_storage.commitments_uncarbonated
          context
          ~rollup
          ~inbox_level)

  let register_stakers_ids () =
    Registration.register2
      ~chunked:false
      S.stakers_ids
      (fun context rollup commitment () () ->
        Sc_rollup.Stake_storage.stakers_ids_uncarbonated
          context
          ~rollup
          ~commitment)

  let register_staker_id () =
    Registration.register2
      ~chunked:false
      S.staker_id
      (fun context rollup pkh () () ->
        Sc_rollup.Stake_storage.staker_id_uncarbonated context ~rollup ~pkh)

  let register_stakers () =
    let open Lwt_result_syntax in
    Registration.register1 ~chunked:false S.stakers (fun context rollup () () ->
        let*! stakers_pkhs =
          Sc_rollup.Stake_storage.stakers_pkhs_uncarbonated context ~rollup
        in
        return stakers_pkhs)

  let register_conflicts () =
    Registration.register2
      ~chunked:false
      S.conflicts
      (fun context rollup staker () () ->
        Sc_rollup.Refutation_storage.conflicting_stakers_uncarbonated
          context
          rollup
          staker)

  let register_timeout () =
    let open Lwt_result_syntax in
    Registration.register3
      ~chunked:false
      S.timeout
      (fun context rollup staker1 staker2 () () ->
        let index = Sc_rollup.Game.Index.make staker1 staker2 in
        let*! res =
          Sc_rollup.Refutation_storage.get_timeout context rollup index
        in
        match res with
        | Ok (timeout, _context) -> return_some timeout
        | Error _ -> return_none)

  let register_timeout_reached () =
    let open Lwt_result_syntax in
    Registration.register3
      ~chunked:false
      S.timeout_reached
      (fun context rollup staker1 staker2 () () ->
        let index = Sc_rollup.Game.Index.make staker1 staker2 in
        let*! res = Sc_rollup.Refutation_storage.timeout context rollup index in
        match res with
        | Ok (game_result, _context) -> return_some game_result
        | Error _ -> return_none)

  let register_can_be_cemented () =
    let open Lwt_result_syntax in
    Registration.register2
      ~chunked:false
      S.can_be_cemented
      (fun context rollup commitment_hash () () ->
        let*! res = Sc_rollup.Stake_storage.cement_commitment context rollup in
        match res with
        | Ok (_context, _cemented_commitment, cemented_commitment_hash)
          when Sc_rollup.Commitment.Hash.equal
                 commitment_hash
                 cemented_commitment_hash ->
            return_true
        | Ok _ | Error _ -> return_false)

  let register_ticket_balance () =
    Registration.register1
      ~chunked:false
      S.ticket_balance
      (fun ctxt sc_rollup () Ticket_token.{ticketer; contents_type; contents} ->
        let open Lwt_result_syntax in
        let* ticket_hash, ctxt =
          Ticket_balance_key.make
            ctxt
            ~owner:(Sc_rollup sc_rollup)
            ~ticketer
            ~contents_type:(Micheline.root contents_type)
            ~contents:(Micheline.root contents)
        in
        let* amount, _ctxt = Ticket_balance.get_balance ctxt ticket_hash in
        return @@ Option.value amount ~default:Z.zero)

  let register () =
    register_kind () ;
    register_inbox () ;
    register_whitelist () ;
    register_last_whitelist_update () ;
    register_consumed_outputs () ;
    register_genesis_info () ;
    register_last_cemented_commitment_hash_with_level () ;
    register_staked_on_commitment () ;
    register_commitment () ;
    register_root () ;
    register_ongoing_refutation_games () ;
    register_commitments () ;
    register_stakers_ids () ;
    register_staker_id () ;
    register_stakers () ;
    register_conflicts () ;
    register_timeout () ;
    register_timeout_reached () ;
    register_can_be_cemented () ;
    register_ticket_balance ()

  let list ctxt block = RPC_context.make_call0 S.root ctxt block () ()

  let inbox ctxt block = RPC_context.make_call0 S.inbox ctxt block () ()

  let whitelist ctxt block sc_rollup_address =
    RPC_context.make_call1 S.whitelist ctxt block sc_rollup_address () ()

  let last_whitelist_update ctxt block sc_rollup_address =
    RPC_context.make_call1
      S.last_whitelist_update
      ctxt
      block
      sc_rollup_address
      ()
      ()

  let genesis_info ctxt block sc_rollup_address =
    RPC_context.make_call1 S.genesis_info ctxt block sc_rollup_address () ()

  let last_cemented_commitment_hash_with_level ctxt block sc_rollup_address =
    RPC_context.make_call1
      S.last_cemented_commitment_hash_with_level
      ctxt
      block
      sc_rollup_address
      ()
      ()

  let staked_on_commitment ctxt block sc_rollup_address staker =
    RPC_context.make_call2
      S.staked_on_commitment
      ctxt
      block
      sc_rollup_address
      staker
      ()
      ()

  let commitment ctxt block sc_rollup_address commitment_hash =
    RPC_context.make_call2
      S.commitment
      ctxt
      block
      sc_rollup_address
      commitment_hash
      ()
      ()

  let ongoing_refutation_games ctxt block sc_rollup_address staker =
    RPC_context.make_call2
      S.ongoing_refutation_games
      ctxt
      block
      sc_rollup_address
      staker
      ()
      ()

  let commitments ctxt rollup inbox_level =
    RPC_context.make_call2 S.commitments ctxt rollup inbox_level

  let stakers_ids ctxt rollup commitment =
    RPC_context.make_call2 S.stakers_ids ctxt rollup commitment

  let staker_id ctxt rollup pkh =
    RPC_context.make_call2 S.staker_id ctxt rollup pkh

  let stakers ctxt rollup = RPC_context.make_call1 S.stakers ctxt rollup

  let conflicts ctxt block sc_rollup_address staker =
    RPC_context.make_call2 S.conflicts ctxt block sc_rollup_address staker () ()

  let timeout_reached ctxt block sc_rollup_address staker1 staker2 =
    RPC_context.make_call3
      S.timeout_reached
      ctxt
      block
      sc_rollup_address
      staker1
      staker2
      ()
      ()

  let timeout ctxt block sc_rollup_address staker1 staker2 =
    RPC_context.make_call3
      S.timeout
      ctxt
      block
      sc_rollup_address
      staker1
      staker2
      ()
      ()

  let can_be_cemented ctxt block sc_rollup_address commitment_hash =
    RPC_context.make_call2
      S.can_be_cemented
      ctxt
      block
      sc_rollup_address
      commitment_hash
      ()
      ()

  let get_ticket_balance ctxt block sc_rollup key =
    RPC_context.make_call1 S.ticket_balance ctxt block sc_rollup () key
end

type Environment.Error_monad.error +=
  | Published_slot_headers_not_initialized of Raw_level.t

let () =
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"published_slot_headers_not_initialized"
    ~title:"The published slot headers bucket not initialized in the context"
    ~description:
      "The published slot headers bucket is not initialized in the context"
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "The published slot headers bucket is not initialized in the context \
         at level %a"
        Raw_level.pp
        level)
    Data_encoding.(obj1 (req "level" Raw_level.encoding))
    (function
      | Published_slot_headers_not_initialized level -> Some level | _ -> None)
    (fun level -> Published_slot_headers_not_initialized level)

module Dal = struct
  let path : RPC_context.t RPC_path.context =
    RPC_path.(open_root / "context" / "dal")

  module S = struct
    let dal_commitments_history =
      let output = Data_encoding.option Dal.Slots_history.encoding in
      let query = RPC_query.(seal @@ query ()) in
      RPC_service.get_service
        ~description:
          "Returns the (currently last) DAL skip list cell if DAL is enabled, \
           or [None] otherwise."
        ~output
        ~query
        RPC_path.(path / "commitments_history")

    let level_query =
      RPC_query.(
        query (fun level -> level)
        |+ opt_field "level" Raw_level.rpc_arg (fun t -> t)
        |> seal)

    type shards_query = {
      level : Raw_level.t option;
      delegates : Signature.Public_key_hash.t list;
    }

    let shards_query =
      let open RPC_query in
      query (fun level delegates -> {level; delegates})
      |+ opt_field "level" Raw_level.rpc_arg (fun t -> t.level)
      |+ multi_field "delegates" Signature.Public_key_hash.rpc_arg (fun t ->
             t.delegates)
      |> seal

    type shards_assignment = {
      delegate : Signature.Public_key_hash.t;
      indexes : int list;
    }

    let shards_assignment_encoding =
      let open Data_encoding in
      conv
        (fun {delegate; indexes} -> (delegate, indexes))
        (fun (delegate, indexes) -> {delegate; indexes})
        (obj2
           (req "delegate" Signature.Public_key_hash.encoding)
           (req "indexes" (list int16)))

    type shards_output = shards_assignment list

    let shards =
      RPC_service.get_service
        ~description:
          "Get the shards assignment for a given level (the default is the \
           current level) and given delegates (the default is all delegates)"
        ~query:shards_query
        ~output:(Data_encoding.list shards_assignment_encoding)
        RPC_path.(path / "shards")

    let published_slot_headers =
      let output = Data_encoding.(list Dal.Slot.Header.encoding) in
      RPC_service.get_service
        ~description:"Get the published slots headers for the given level"
        ~query:level_query
        ~output
        RPC_path.(path / "published_slot_headers")

    let skip_list_cells_of_level =
      let output =
        Data_encoding.(
          list
            (tup2
               Dal.Slots_history.Pointer_hash.encoding
               Dal.Slots_history.encoding))
      in
      RPC_service.get_service
        ~description:
          "Returns the cells of the DAL skip list constructed during this \
           targeted block and stored in the context. The cells ordering in the \
           list is not specified (not relevant). The list is expected to be \
           empty if the entry is not initialized in the context (yet), or to \
           have a size that coincides with the number of DAL slots otherwise."
        ~query:RPC_query.empty
        ~output
        RPC_path.(path / "skip_list_cells_of_level")
  end

  let register_dal_commitments_history () =
    let open Lwt_result_syntax in
    Registration.register0
      ~chunked:false
      S.dal_commitments_history
      (fun ctxt () () ->
        if (Constants.parametric ctxt).dal.feature_enable then
          let+ result = Dal.Slots_storage.get_slot_headers_history ctxt in
          Option.some result
        else return_none)

  let dal_commitments_history ctxt block =
    RPC_context.make_call0 S.dal_commitments_history ctxt block () ()

  let dal_shards ctxt block ?level ?(delegates = []) () =
    RPC_context.make_call0 S.shards ctxt block {level; delegates} ()

  let register_shards () =
    Registration.register0 ~chunked:true S.shards @@ fun ctxt q () ->
    let open Lwt_result_syntax in
    let*? level_opt =
      Option.map_e (Level.from_raw_with_offset ctxt ~offset:0l) q.level
    in
    let level = Option.value level_opt ~default:(Level.current ctxt) in
    let* _ctxt, map = Dal_services.shards ctxt ~level in
    let query_delegates = Signature.Public_key_hash.Set.of_list q.delegates in
    let all_delegates =
      Signature.Public_key_hash.Set.is_empty query_delegates
    in
    Signature.Public_key_hash.Map.fold
      (fun delegate indexes acc ->
        if
          all_delegates
          || Signature.Public_key_hash.Set.mem delegate query_delegates
        then ({delegate; indexes} : S.shards_assignment) :: acc
        else acc)
      map
      []
    |> return

  let dal_published_slot_headers ctxt block ?level () =
    RPC_context.make_call0 S.published_slot_headers ctxt block level ()

  let skip_list_cells_of_level ctxt block () =
    RPC_context.make_call0 S.skip_list_cells_of_level ctxt block () ()

  let register_published_slot_headers () =
    let open Lwt_result_syntax in
    Registration.register0 ~chunked:true S.published_slot_headers
    @@ fun ctxt level () ->
    let level = Option.value level ~default:(Level.current ctxt).level in
    let* result = Dal.Slot.find_slot_headers ctxt level in
    match result with
    | Some l -> return @@ List.map fst l
    | None ->
        Environment.Error_monad.tzfail
        @@ Published_slot_headers_not_initialized level

  let register_skip_list_cells_of_level () =
    let open Lwt_result_syntax in
    Registration.register0 ~chunked:true S.skip_list_cells_of_level
    @@ fun ctxt () () ->
    let* result = Dal.Slots_storage.find_level_histories ctxt in
    match result with Some l -> return l | None -> return []

  let register () =
    register_dal_commitments_history () ;
    register_shards () ;
    register_published_slot_headers () ;
    register_skip_list_cells_of_level ()
end

module Forge = struct
  module S = struct
    open Data_encoding

    let path = RPC_path.(path / "forge")

    let operations =
      RPC_service.post_service
        ~description:"Forge an operation"
        ~query:RPC_query.empty
        ~input:Operation.unsigned_encoding
        ~output:(bytes Hex)
        RPC_path.(path / "operations")

    let signed_operations =
      RPC_service.post_service
        ~description:"Forge a signed operation"
        ~query:RPC_query.empty
        ~input:Operation.encoding
        ~output:(bytes Hex)
        RPC_path.(path / "signed_operations")

    let empty_proof_of_work_nonce =
      Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

    let protocol_data =
      RPC_service.post_service
        ~description:"Forge the protocol-specific part of a block header"
        ~query:RPC_query.empty
        ~input:
          (obj5
             (req "payload_hash" Block_payload_hash.encoding)
             (req "payload_round" Round.encoding)
             (opt "nonce_hash" Nonce_hash.encoding)
             (dft
                "proof_of_work_nonce"
                (Fixed.bytes
                   Hex
                   Alpha_context.Constants.proof_of_work_nonce_size)
                empty_proof_of_work_nonce)
             Per_block_votes.(
               dft
                 "per_block_votes"
                 per_block_votes_encoding
                 {liquidity_baking_vote = Per_block_vote_pass}))
        ~output:(obj1 (req "protocol_data" (bytes Hex)))
        RPC_path.(path / "protocol_data")

    let consensus_operations =
      RPC_service.post_service
        ~description:"Forge a consensus operation in BLS mode"
        ~query:RPC_query.empty
        ~input:Operation.unsigned_encoding
        ~output:(obj2 (req "sign" (bytes Hex)) (req "inject" (bytes Hex)))
        RPC_path.(path / "bls_consensus_operations")
  end

  let register () =
    let open Lwt_result_syntax in
    Registration.register0_noctxt
      ~chunked:true
      S.operations
      (fun () operation ->
        return
          (Data_encoding.Binary.to_bytes_exn
             Operation.unsigned_encoding
             operation)) ;
    Registration.register0_noctxt
      ~chunked:true
      S.signed_operations
      (fun () signed_operation ->
        return
          (Data_encoding.Binary.to_bytes_exn
             Operation.encoding
             signed_operation)) ;
    Registration.register0_noctxt
      ~chunked:true
      S.consensus_operations
      (fun () operation ->
        return
          Data_encoding.Binary.
            ( to_bytes_exn Operation.bls_mode_unsigned_encoding operation,
              to_bytes_exn Operation.unsigned_encoding operation )) ;
    Registration.register0_noctxt
      ~chunked:true
      S.protocol_data
      (fun
        ()
        ( payload_hash,
          payload_round,
          seed_nonce_hash,
          proof_of_work_nonce,
          per_block_votes )
      ->
        return
          (Data_encoding.Binary.to_bytes_exn
             Block_header.contents_encoding
             {
               payload_hash;
               payload_round;
               seed_nonce_hash;
               proof_of_work_nonce;
               per_block_votes;
             }))

  module Manager = struct
    let operations ctxt block ~branch ~source ?sourcePubKey ?proof ~counter ~fee
        ~gas_limit ~storage_limit operations =
      let open Lwt_result_syntax in
      let*! result = Contract_services.manager_key ctxt block source in
      match result with
      | Error _ as e -> Lwt.return e
      | Ok revealed ->
          let ops =
            List.map
              (fun (Manager operation) ->
                Contents
                  (Manager_operation
                     {source; counter; operation; fee; gas_limit; storage_limit}))
              operations
          in
          let ops =
            match (sourcePubKey, revealed) with
            | None, _ | _, Some _ -> ops
            | Some pk, None ->
                let operation = Reveal {public_key = pk; proof} in
                Contents
                  (Manager_operation
                     {source; counter; operation; fee; gas_limit; storage_limit})
                :: ops
          in
          let*? ops = Environment.wrap_tzresult @@ Operation.of_list ops in
          RPC_context.make_call0 S.operations ctxt block () ({branch}, ops)

    let reveal ctxt block ~branch ~source ~sourcePubKey ?proof ~counter ~fee ()
        =
      operations
        ctxt
        block
        ~branch
        ~source
        ~sourcePubKey
        ?proof
        ~counter
        ~fee
        ~gas_limit:Gas.Arith.zero
        ~storage_limit:Z.zero
        []

    let transaction ctxt block ~branch ~source ?sourcePubKey ?proof ~counter
        ~amount ~destination ?(entrypoint = Entrypoint.default) ?parameters
        ~gas_limit ~storage_limit ~fee () =
      let parameters =
        Option.fold
          ~some:Script.lazy_expr
          ~none:Script.unit_parameter
          parameters
      in
      operations
        ctxt
        block
        ~branch
        ~source
        ?sourcePubKey
        ?proof
        ~counter
        ~fee
        ~gas_limit
        ~storage_limit
        [Manager (Transaction {amount; parameters; destination; entrypoint})]

    let origination ctxt block ~branch ~source ?sourcePubKey ?proof ~counter
        ~balance ?delegatePubKey ~script ~gas_limit ~storage_limit ~fee () =
      operations
        ctxt
        block
        ~branch
        ~source
        ?sourcePubKey
        ?proof
        ~counter
        ~fee
        ~gas_limit
        ~storage_limit
        [
          Manager
            (Origination {delegate = delegatePubKey; script; credit = balance});
        ]

    let delegation ctxt block ~branch ~source ?sourcePubKey ?proof ~counter ~fee
        delegate =
      operations
        ctxt
        block
        ~branch
        ~source
        ?sourcePubKey
        ?proof
        ~counter
        ~fee
        ~gas_limit:Gas.Arith.zero
        ~storage_limit:Z.zero
        [Manager (Delegation delegate)]
  end

  let operation ctxt block ~branch operation =
    RPC_context.make_call0
      S.operations
      ctxt
      block
      ()
      ({branch}, Contents_list (Single operation))

  let attestation ctxt b ~branch ~consensus_content ?dal_content () =
    operation ctxt b ~branch (Attestation {consensus_content; dal_content})

  let proposals ctxt b ~branch ~source ~period ~proposals () =
    operation ctxt b ~branch (Proposals {source; period; proposals})

  let ballot ctxt b ~branch ~source ~period ~proposal ~ballot () =
    operation ctxt b ~branch (Ballot {source; period; proposal; ballot})

  let failing_noop ctxt b ~branch ~message () =
    operation ctxt b ~branch (Failing_noop message)

  let seed_nonce_revelation ctxt block ~branch ~level ~nonce () =
    operation ctxt block ~branch (Seed_nonce_revelation {level; nonce})

  let vdf_revelation ctxt block ~branch ~solution () =
    operation ctxt block ~branch (Vdf_revelation {solution})

  let double_baking_evidence ctxt block ~branch ~bh1 ~bh2 () =
    operation ctxt block ~branch (Double_baking_evidence {bh1; bh2})

  let double_consensus_operation_evidence ctxt block ~branch ~slot ~op1 ~op2 ()
      =
    operation
      ctxt
      block
      ~branch
      (Double_consensus_operation_evidence {slot; op1; op2})

  let dal_entrapment_evidence ctxt block ~branch ~attestation ~consensus_slot
      ~slot_index ~shard_with_proof =
    operation
      ctxt
      block
      ~branch
      (Dal_entrapment_evidence
         {attestation; consensus_slot; slot_index; shard_with_proof})

  let empty_proof_of_work_nonce =
    Bytes.make Constants_repr.proof_of_work_nonce_size '\000'
end

module Parse = struct
  module S = struct
    open Data_encoding

    let path = RPC_path.(path / "parse")

    let operations_query =
      let open RPC_query in
      query (fun version ->
          object
            method version = version
          end)
      |+ opt_field "version" version_arg (fun t -> t#version)
      |> seal

    let parse_operations_encoding = list (dynamic_size Operation.encoding)

    let operations =
      RPC_service.post_service
        ~description:"Parse operations"
        ~query:operations_query
        ~input:
          (obj2
             (req "operations" (list (dynamic_size Operation.raw_encoding)))
             (opt "check_signature" bool))
        ~output:parse_operations_encoding
        RPC_path.(path / "operations")

    let block =
      RPC_service.post_service
        ~description:"Parse a block"
        ~query:RPC_query.empty
        ~input:Block_header.raw_encoding
        ~output:Block_header.protocol_data_encoding
        RPC_path.(path / "block")
  end

  let parse_protocol_data protocol_data =
    match
      Data_encoding.Binary.of_bytes_opt
        Block_header.protocol_data_encoding
        protocol_data
    with
    | None -> Stdlib.failwith "Cant_parse_protocol_data"
    | Some protocol_data -> protocol_data

  let parse_operation (op : Operation.raw) =
    let open Result_syntax in
    match
      Data_encoding.Binary.of_bytes_opt
        Operation.protocol_data_encoding
        op.proto
    with
    | Some protocol_data -> return {shell = op.shell; protocol_data}
    | None -> Environment.Error_monad.error Plugin_errors.Cannot_parse_operation

  let register () =
    let open Lwt_result_syntax in
    Registration.register0
      ~chunked:true
      S.operations
      (fun _ctxt _params (operations, check) ->
        let* ops =
          List.map_es
            (fun raw ->
              let*? op = parse_operation raw in
              let () =
                match check with
                | Some true -> ()
                (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5702
                   The signature of the parsed operation should be properly
                   checked *)
                (* I.check_signature ctxt *)
                (* op.protocol_data.signature op.shell op.protocol_data.contents *)
                | Some false | None -> ()
              in
              return op)
            operations
        in
        return ops) ;
    Registration.register0_noctxt ~chunked:false S.block (fun () raw_block ->
        return @@ parse_protocol_data raw_block.protocol_data)

  let operations ctxt block ?version ?check operations =
    let open Lwt_result_syntax in
    let*! v =
      RPC_context.make_call0
        S.operations
        ctxt
        block
        (object
           method version = version
        end)
        (operations, check)
    in
    match v with
    | Error e -> tzfail e
    | Ok parse_operation -> return parse_operation

  let block ctxt block shell protocol_data =
    RPC_context.make_call0
      S.block
      ctxt
      block
      ()
      ({shell; protocol_data} : Block_header.raw)
end

(* Compute the estimated starting time of a [round] at a future
   [level], given the head's level [current_level], timestamp
   [current_timestamp], and round [current_round]. Assumes blocks at
   intermediate levels are produced at round 0. *)
let estimated_time round_durations ~current_level ~current_round
    ~current_timestamp ~level ~round =
  let open Result_syntax in
  if Level.(level <= current_level) then return_none
  else
    let* round_start_at_next_level =
      Round.timestamp_of_round
        round_durations
        ~round
        ~predecessor_timestamp:current_timestamp
        ~predecessor_round:current_round
    in
    let step = Round.round_duration round_durations Round.zero in
    let diff = Level.diff level current_level in
    let* delay = Period.mult (Int32.pred diff) step in
    let* timestamp = Timestamp.(round_start_at_next_level +? delay) in
    return_some timestamp

let requested_levels ~default_level ctxt cycles levels =
  match (levels, cycles) with
  | [], [] -> [default_level]
  | levels, cycles ->
      (* explicitly fail when requested levels or cycle are in the past...
         or too far in the future...
         TODO: https://gitlab.com/tezos/tezos/-/issues/2335
               this old comment (from version Alpha) conflicts with
               the specification of the RPCs that use this code.
      *)
      List.sort_uniq
        Level.compare
        (List.rev_append
           (List.rev_map (Level.from_raw ctxt) levels)
           (List.concat_map (Level.levels_in_cycle ctxt) cycles))

module Baking_rights = struct
  type t = {
    level : Raw_level.t;
    delegate : public_key_hash;
    consensus_key : public_key_hash;
    round : Round.t;
    timestamp : Timestamp.t option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; delegate; consensus_key; round; timestamp} ->
        (level, delegate, round, timestamp, consensus_key))
      (fun (level, delegate, round, timestamp, consensus_key) ->
        {level; delegate; consensus_key; round; timestamp})
      (obj5
         (req "level" Raw_level.encoding)
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "round" Round.encoding)
         (opt "estimated_time" Timestamp.encoding)
         (req "consensus_key" Signature.Public_key_hash.encoding))

  let default_max_round = 64

  module S = struct
    open Data_encoding

    let path = RPC_path.(open_root / "helpers" / "baking_rights")

    type baking_rights_query = {
      levels : Raw_level.t list;
      cycle : Cycle.t option;
      delegates : Signature.Public_key_hash.t list;
      consensus_keys : Signature.Public_key_hash.t list;
      max_round : int option;
      all : bool;
    }

    let baking_rights_query =
      let open RPC_query in
      query (fun levels cycle delegates consensus_keys max_round all ->
          {levels; cycle; delegates; consensus_keys; max_round; all})
      |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
      |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
      |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
             t.delegates)
      |+ multi_field "consensus_key" Signature.Public_key_hash.rpc_arg (fun t ->
             t.consensus_keys)
      |+ opt_field "max_round" RPC_arg.uint (fun t -> t.max_round)
      |+ flag "all" (fun t -> t.all)
      |> seal

    let baking_rights =
      RPC_service.get_service
        ~description:
          (Format.sprintf
             "Retrieves the list of delegates allowed to bake a block.\n\
              By default, it gives the best baking opportunities (in terms of \
              rounds) for bakers that have at least one opportunity below the \
              %dth round for the next block.\n\
              Parameters `level` and `cycle` can be used to specify the \
              (valid) level(s) in the past or future at which the baking \
              rights have to be returned.\n\
              Parameter `delegate` can be used to restrict the results to the \
              given delegates. Parameter `consensus_key` can be used to \
              restrict the results to the given consensus_keys. If parameter \
              `all` is set, all the baking opportunities for each baker at \
              each level are returned, instead of just the first one.\n\
              Returns the list of baking opportunities up to round %d. Also \
              returns the minimal timestamps that correspond to these \
              opportunities. The timestamps are omitted for levels in the \
              past, and are only estimates for levels higher that the next \
              block's, based on the hypothesis that all predecessor blocks \
              were baked at the first round."
             default_max_round
             default_max_round)
        ~query:baking_rights_query
        ~output:(list encoding)
        path
  end

  let baking_rights_at_level ctxt max_round level =
    let open Lwt_result_syntax in
    let* current_round = Round.get ctxt in
    let current_level = Level.current ctxt in
    let current_timestamp = Timestamp.current ctxt in
    let round_durations = Alpha_context.Constants.round_durations ctxt in
    let rec loop ctxt acc round =
      if Round.(round > max_round) then
        (* returns the ctxt with an updated cache of slot holders *)
        return (ctxt, List.rev acc)
      else
        let* ( ctxt,
               _slot,
               {
                 Consensus_key.consensus_pkh;
                 delegate;
                 consensus_pk = _;
                 companion_pk = _;
                 companion_pkh = _;
               } ) =
          Stake_distribution.baking_rights_owner ctxt level ~round
        in
        let*? timestamp =
          estimated_time
            round_durations
            ~current_level
            ~current_round
            ~current_timestamp
            ~level
            ~round
        in
        let acc =
          {
            level = level.level;
            delegate;
            consensus_key = consensus_pkh;
            round;
            timestamp;
          }
          :: acc
        in
        loop ctxt acc (Round.succ round)
    in
    loop ctxt [] Round.zero

  let remove_duplicated_delegates rights =
    List.rev @@ fst
    @@ List.fold_left
         (fun (acc, previous) r ->
           if
             Signature.Public_key_hash.Set.exists
               (Signature.Public_key_hash.equal r.delegate)
               previous
           then (acc, previous)
           else (r :: acc, Signature.Public_key_hash.Set.add r.delegate previous))
         ([], Signature.Public_key_hash.Set.empty)
         rights

  let register () =
    let open Lwt_result_syntax in
    Registration.register0 ~chunked:true S.baking_rights (fun ctxt q () ->
        let cycles = match q.cycle with None -> [] | Some cycle -> [cycle] in
        let levels =
          requested_levels
            ~default_level:(Level.succ ctxt (Level.current ctxt))
            ctxt
            cycles
            q.levels
        in
        let*? max_round =
          Round.of_int
            (match q.max_round with
            | None -> default_max_round
            | Some max_round ->
                Compare.Int.min
                  max_round
                  (Constants.consensus_committee_size ctxt))
        in
        let+ _ctxt, rights =
          List.fold_left_map_es
            (fun ctxt l -> baking_rights_at_level ctxt max_round l)
            ctxt
            levels
        in
        let rights =
          if q.all then List.concat rights
          else List.concat_map remove_duplicated_delegates rights
        in
        let rights =
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              List.filter is_requested rights
        in
        let rights =
          match q.consensus_keys with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.consensus_key)
                  delegates
              in
              List.filter is_requested rights
        in
        rights)

  let get ctxt ?(levels = []) ?cycle ?(delegates = []) ?(consensus_keys = [])
      ?(all = false) ?max_round block =
    RPC_context.make_call0
      S.baking_rights
      ctxt
      block
      {levels; cycle; delegates; consensus_keys; max_round; all}
      ()
end

module Attestation_rights = struct
  type delegate_rights = {
    delegate : Signature.Public_key_hash.t;
    consensus_key : Signature.Public_key_hash.t;
    first_slot : Slot.t;
    attesting_power : int;
  }

  type t = {
    level : Raw_level.t;
    delegates_rights : delegate_rights list;
    estimated_time : Time.t option;
  }

  let delegate_rights_encoding =
    let open Data_encoding in
    conv
      (fun {delegate; consensus_key; first_slot; attesting_power} ->
        (delegate, first_slot, attesting_power, consensus_key))
      (fun (delegate, first_slot, attesting_power, consensus_key) ->
        {delegate; first_slot; attesting_power; consensus_key})
      (obj4
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "first_slot" Slot.encoding)
         (req "attesting_power" uint16)
         (req "consensus_key" Signature.Public_key_hash.encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; delegates_rights; estimated_time} ->
        (level, delegates_rights, estimated_time))
      (fun (level, delegates_rights, estimated_time) ->
        {level; delegates_rights; estimated_time})
      (obj3
         (req "level" Raw_level.encoding)
         (req "delegates" (list delegate_rights_encoding))
         (opt "estimated_time" Timestamp.encoding))

  module S = struct
    open Data_encoding

    let attestation_path = RPC_path.(path / "attestation_rights")

    type attestation_rights_query = {
      levels : Raw_level.t list;
      cycle : Cycle.t option;
      delegates : Signature.Public_key_hash.t list;
      consensus_keys : Signature.Public_key_hash.t list;
    }

    let attestation_rights_query =
      let open RPC_query in
      query (fun levels cycle delegates consensus_keys ->
          {levels; cycle; delegates; consensus_keys})
      |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
      |+ opt_field "cycle" Cycle.rpc_arg (fun t -> t.cycle)
      |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
             t.delegates)
      |+ multi_field "consensus_key" Signature.Public_key_hash.rpc_arg (fun t ->
             t.consensus_keys)
      |> seal

    let attestation_rights =
      RPC_service.get_service
        ~description:
          "Retrieves the delegates allowed to attest a block.\n\
           By default, it gives the attestation power for delegates that have \
           at least one attestation slot for the next block.\n\
           Parameters `level` and `cycle` can be used to specify the (valid) \
           level(s) in the past or future at which the attestation rights have \
           to be returned. Parameter `delegate` can be used to restrict the \
           results to the given delegates.\n\
           Parameter `consensus_key` can be used to restrict the results to \
           the given consensus_keys. \n\
           Returns the smallest attestation slots and the attestation power. \
           Also returns the minimal timestamp that corresponds to attestation \
           at the given level. The timestamps are omitted for levels in the \
           past, and are only estimates for levels higher that the next \
           block's, based on the hypothesis that all predecessor blocks were \
           baked at the first round."
        ~query:attestation_rights_query
        ~output:(list encoding)
        attestation_path
  end

  let attestation_rights_at_level ctxt attested_level =
    let open Lwt_result_syntax in
    let* ctxt, rights =
      Baking.attesting_rights_by_first_slot ctxt ~attested_level
    in
    let* current_round = Round.get ctxt in
    let current_level = Level.current ctxt in
    let current_timestamp = Timestamp.current ctxt in
    let round_durations = Alpha_context.Constants.round_durations ctxt in
    let*? estimated_time =
      estimated_time
        round_durations
        ~current_level
        ~current_round
        ~current_timestamp
        ~level:attested_level
        ~round:Round.zero
    in
    let rights =
      Slot.Map.fold
        (fun first_slot
             ({
                consensus_key =
                  {
                    Consensus_key.delegate;
                    consensus_pk = _;
                    consensus_pkh = consensus_key;
                    companion_pk = _;
                    companion_pkh = _;
                  };
                attesting_power;
                dal_power = _;
              } :
               Consensus_key.power)
             acc
           ->
          {
            delegate;
            consensus_key;
            first_slot;
            (* TODO ABAAB (this RPC is only used for tests) *)
            attesting_power = Attesting_power.get_slots attesting_power;
          }
          :: acc)
        rights
        []
    in
    (* returns the ctxt with an updated cache of slot holders *)
    return
      ( ctxt,
        {
          level = attested_level.level;
          delegates_rights = rights;
          estimated_time;
        } )

  let get_attestation_rights ctxt (q : S.attestation_rights_query) =
    let open Lwt_result_syntax in
    let cycles = match q.cycle with None -> [] | Some cycle -> [cycle] in
    let levels =
      requested_levels ~default_level:(Level.current ctxt) ctxt cycles q.levels
    in
    let+ _ctxt, rights_per_level =
      List.fold_left_map_es attestation_rights_at_level ctxt levels
    in
    let rights_per_level =
      match (q.consensus_keys, q.delegates) with
      | [], [] -> rights_per_level
      | _, _ ->
          let is_requested p =
            List.exists
              (Signature.Public_key_hash.equal p.consensus_key)
              q.consensus_keys
            || List.exists
                 (Signature.Public_key_hash.equal p.delegate)
                 q.delegates
          in
          List.filter_map
            (fun rights_at_level ->
              match
                List.filter is_requested rights_at_level.delegates_rights
              with
              | [] -> None
              | delegates_rights -> Some {rights_at_level with delegates_rights})
            rights_per_level
    in
    rights_per_level

  let register () =
    Registration.register0 ~chunked:true S.attestation_rights (fun ctxt q () ->
        get_attestation_rights ctxt q)

  let get ctxt ?(levels = []) ?cycle ?(delegates = []) ?(consensus_keys = [])
      block =
    RPC_context.make_call0
      S.attestation_rights
      ctxt
      block
      {levels; cycle; delegates; consensus_keys}
      ()
end

module Validators = struct
  type delegate = {
    delegate : Signature.Public_key_hash.t;
    consensus_key : Signature.public_key_hash;
    companion_key : Bls.Public_key_hash.t option;
    rounds : Round.t list;
    attesting_power : int64;
    attestation_slot : Slot.t;
  }

  type t = {
    level : Raw_level.t;
    consensus_threshold : int64;
    consensus_committee : int64;
    abaab_activation_flag : bool;
    delegates : delegate list;
  }

  let delegate_encoding =
    let open Data_encoding in
    conv
      (fun {
             delegate;
             consensus_key;
             companion_key;
             rounds;
             attesting_power;
             attestation_slot;
           }
         ->
        ( delegate,
          rounds,
          consensus_key,
          companion_key,
          attesting_power,
          attestation_slot ))
      (fun ( delegate,
             rounds,
             consensus_key,
             companion_key,
             attesting_power,
             attestation_slot )
         ->
        {
          delegate;
          consensus_key;
          companion_key;
          rounds;
          attesting_power;
          attestation_slot;
        })
      (obj6
         (req "delegate" Signature.Public_key_hash.encoding)
         (req "rounds" (list Round.encoding))
         (req "consensus_key" Signature.Public_key_hash.encoding)
         (opt "companion_key" Bls.Public_key_hash.encoding)
         (req "attesting_power" int64)
         (req "attestation_slot" Slot.encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             level;
             consensus_threshold;
             consensus_committee;
             abaab_activation_flag;
             delegates;
           }
         ->
        ( level,
          consensus_threshold,
          consensus_committee,
          abaab_activation_flag,
          delegates ))
      (fun ( level,
             consensus_threshold,
             consensus_committee,
             abaab_activation_flag,
             delegates )
         ->
        {
          level;
          consensus_threshold;
          consensus_committee;
          abaab_activation_flag;
          delegates;
        })
      (obj5
         (req "level" Raw_level.encoding)
         (req "consensus_threshold" int64)
         (req "consensus_committee" int64)
         (req "all_bakers_attest_activated" bool)
         (req "delegates" (list delegate_encoding)))

  module S = struct
    open Data_encoding

    let path = RPC_path.(path / "validators")

    type validators_query = {
      levels : Raw_level.t list;
      delegates : Signature.Public_key_hash.t list;
      consensus_keys : Signature.Public_key_hash.t list;
    }

    let validators_query =
      let open RPC_query in
      query (fun levels delegates consensus_keys ->
          {levels; delegates; consensus_keys})
      |+ multi_field "level" Raw_level.rpc_arg (fun t -> t.levels)
      |+ multi_field "delegate" Signature.Public_key_hash.rpc_arg (fun t ->
             t.delegates)
      |+ multi_field "consensus_key" Signature.Public_key_hash.rpc_arg (fun t ->
             t.consensus_keys)
      |> seal

    let validators =
      RPC_service.get_service
        ~description:
          "Retrieves the level, the attestation slots and the public key hash \
           of each delegate allowed to attest a block. Also returns each \
           delegate's current consensus key, and current companion key when \
           needed for crafting and validating attestations at this level.\n\
           By default, it provides this information for the next level.\n\
           Parameter `level` can be used to specify the (valid) level(s) in \
           the past or future at which the attestation rights have to be \
           returned. Parameter `delegate` can be used to restrict the results \
           results to the given delegates. Parameter `consensus_key` can be \
           used to restrict the results to the given consensus_keys.\n"
        ~query:validators_query
        ~output:(list encoding)
        path
  end

  let attestation_slots_at_level ctxt attested_level =
    let open Lwt_result_syntax in
    let* ctxt, rights = Baking.attesting_rights ctxt ~attested_level in
    let aggregate_attestation = Constants.aggregate_attestation ctxt in
    return
      ( ctxt,
        Signature.Public_key_hash.Map.fold
          (fun _pkh
               {
                 Baking.delegate;
                 consensus_key;
                 companion_key;
                 rounds;
                 attesting_power;
                 attestation_slot;
               }
               acc
             ->
            let companion_key =
              match consensus_key with
              | Bls _ when aggregate_attestation -> companion_key
              | _ -> None
            in
            {
              delegate;
              consensus_key;
              companion_key;
              rounds;
              attesting_power;
              attestation_slot;
            }
            :: acc)
          rights
          [] )

  let register () =
    let open Lwt_result_syntax in
    Registration.register0 ~chunked:true S.validators (fun ctxt q () ->
        let levels =
          requested_levels ~default_level:(Level.current ctxt) ctxt [] q.levels
        in
        let+ _ctxt, rights =
          List.fold_left_es
            (fun (ctxt, acc) attested_level ->
              let abaab_activation_flag =
                Attesting_power.check_all_bakers_attest_at_level
                  ctxt
                  ~attested_level
              in
              let* ctxt, consensus_threshold =
                Attesting_power.consensus_threshold ctxt ~attested_level
              in
              let* ctxt, consensus_committee =
                Attesting_power.consensus_committee ctxt ~attested_level
              in
              let* ctxt, delegates =
                attestation_slots_at_level ctxt attested_level
              in
              return
                ( ctxt,
                  ( {
                      level = attested_level.level;
                      consensus_threshold;
                      consensus_committee;
                      abaab_activation_flag;
                      delegates = [];
                    },
                    delegates )
                  :: acc ))
            (ctxt, [])
            (List.rev levels)
        in
        let filter_with f list =
          List.map
            (fun (level_info, delegates) ->
              (level_info, List.filter f delegates))
            list
        in
        let rights =
          match q.delegates with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.delegate)
                  delegates
              in
              filter_with is_requested rights
        in
        let rights =
          match q.consensus_keys with
          | [] -> rights
          | _ :: _ as delegates ->
              let is_requested p =
                List.exists
                  (Signature.Public_key_hash.equal p.consensus_key)
                  delegates
              in
              filter_with is_requested rights
        in
        let rights =
          List.map
            (fun (level_info, delegates) -> {level_info with delegates})
            rights
        in
        rights)

  let get ctxt ?(levels = []) ?(delegates = []) ?(consensus_keys = []) block =
    RPC_context.make_call0
      S.validators
      ctxt
      block
      {levels; delegates; consensus_keys}
      ()
end

let get_blocks_preservation_cycles ~get_context =
  let open Lwt_option_syntax in
  let constants_key = [Constants_repr.version; "constants"] in
  let* ctxt = Lwt.map Option.some (get_context ()) in
  let* bytes = Tezos_protocol_environment.Context.find ctxt constants_key in
  let*? constants_parametrics =
    Data_encoding.Binary.of_bytes_opt Constants_parametric_repr.encoding bytes
  in
  return constants_parametrics.blocks_preservation_cycles

module S = struct
  open Data_encoding

  type level_query = {offset : int32}

  let level_query : level_query RPC_query.t =
    let open RPC_query in
    query (fun offset -> {offset})
    |+ field "offset" RPC_arg.int32 0l (fun t -> t.offset)
    |> seal

  let current_level =
    RPC_service.get_service
      ~description:
        "Returns the level of the interrogated block, or the one of a block \
         located `offset` blocks after it in the chain. For instance, the next \
         block if `offset` is 1. The offset cannot be negative."
      ~query:level_query
      ~output:Level.encoding
      RPC_path.(path / "current_level")

  let levels_in_current_cycle =
    RPC_service.get_service
      ~description:"Levels of a cycle"
      ~query:level_query
      ~output:
        (obj2 (req "first" Raw_level.encoding) (req "last" Raw_level.encoding))
      RPC_path.(path / "levels_in_current_cycle")

  let round =
    RPC_service.get_service
      ~description:
        "Returns the round of the interrogated block, or the one of a block \
         located `offset` blocks after in the chain (or before when negative). \
         For instance, the next block if `offset` is 1."
      ~query:RPC_query.empty
      ~output:Round.encoding
      RPC_path.(path / "round")

  let consecutive_round_zero =
    RPC_service.get_service
      ~description:
        "Returns the number of blocks consecutively baked at round zero."
      ~query:RPC_query.empty
      ~output:int32
      RPC_path.(path / "consecutive_round_zero")

  let total_baking_power =
    RPC_service.get_service
      ~description:"Returns the total baking power for the current cycle"
      ~query:RPC_query.empty
      ~output:Data_encoding.int64
      RPC_path.(path / "total_baking_power")

  let baking_power_distribution_for_current_cycle =
    RPC_service.get_service
      ~description:
        "Returns the total baking power and the list of active delegates with \
         their respective baking power, used to determine consensus rights for \
         the current cycle. Note that these baking powers correspond to the \
         staked and delegated balances that were held by bakers at the end of \
         (current_cycle - CONSENSUS_RIGHTS_DELAY - 1)."
      ~query:RPC_query.empty
      ~output:
        Data_encoding.(tup2 int64 (list (tup2 Consensus_key.encoding int64)))
      RPC_path.(path / "baking_power_distribution_for_current_cycle")

  let cycle_query : Cycle.t option RPC_query.t =
    let open RPC_query in
    query Fun.id |+ opt_field "cycle" Cycle.rpc_arg Fun.id |> seal

  let tz4_baker_number_ratio =
    RPC_service.get_service
      ~description:"Returns the ratio of active bakers using a tz4."
      ~query:cycle_query
      ~output:Data_encoding.(string Plain)
      RPC_path.(path / "tz4_baker_number_ratio")

  let abaab_activation_level =
    RPC_service.get_service
      ~description:
        "Returns the activation level of All Bakers Attest. If the level is \
         not set, returns `null`."
      ~query:RPC_query.empty
      ~output:Data_encoding.(option Level_repr.encoding)
      RPC_path.(path / "all_bakers_attest_activation_level")
end

type Environment.Error_monad.error += Negative_level_offset

let () =
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"negative_level_offset"
    ~title:"The specified level offset is negative"
    ~description:"The specified level offset is negative"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The specified level offset should be positive.")
    Data_encoding.unit
    (function Negative_level_offset -> Some () | _ -> None)
    (fun () -> Negative_level_offset)

let register () =
  let open Lwt_result_syntax in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/7369 *)
  Contract_services.register () ;
  Delegate_services.register () ;
  Destination_services.register () ;
  Scripts.register () ;
  Forge.register () ;
  Parse.register () ;
  Contract.register () ;
  Big_map.register () ;
  Baking_rights.register () ;
  Attestation_rights.register () ;
  Validators.register () ;
  Protocol.register () ;
  Sc_rollup.register () ;
  Dal.register () ;
  Registration.register0 ~chunked:false S.current_level (fun ctxt q () ->
      if q.offset < 0l then Environment.Error_monad.tzfail Negative_level_offset
      else
        Lwt.return
          (Level.from_raw_with_offset
             ctxt
             ~offset:q.offset
             (Level.current ctxt).level)) ;
  Registration.opt_register0
    ~chunked:true
    S.levels_in_current_cycle
    (fun ctxt q () ->
      let rev_levels = Level.levels_in_current_cycle ctxt ~offset:q.offset () in
      match rev_levels with
      | [] -> return_none
      | [level] -> return_some (level.level, level.level)
      | last :: default_first :: rest ->
          (* The [rev_levels] list is reversed, the last level is the head *)
          let first = List.last default_first rest in
          return_some (first.level, last.level)) ;
  Registration.register0 ~chunked:false S.round (fun ctxt () () ->
      Round.get ctxt) ;
  Registration.register0
    ~chunked:false
    S.consecutive_round_zero
    (fun ctxt () () -> Consecutive_round_zero.get ctxt) ;
  Registration.register0 ~chunked:false S.total_baking_power (fun ctxt () () ->
      Stake_distribution.For_RPC.total_baking_power
        ctxt
        (Level.current ctxt).cycle) ;
  Registration.register0
    ~chunked:false
    S.baking_power_distribution_for_current_cycle
    (fun ctxt () () ->
      let* (_ctxt : t), total_stake, stake_info_list =
        Stake_distribution.stake_info ctxt (Level.current ctxt)
      in
      let stake_info_list =
        List.map (fun (x, y) -> (Consensus_key.pkh x, y)) stake_info_list
      in
      return (total_stake, stake_info_list)) ;
  Registration.register0
    ~chunked:false
    S.tz4_baker_number_ratio
    (fun ctxt q () ->
      let cycle = Option.value ~default:(Level.current ctxt).cycle q in
      let* _, _total_stake, stake_info_list =
        Stake_distribution.stake_info_for_cycle ctxt cycle
      in
      let tz4_bakers =
        List.fold_left
          (fun acc (x, _) ->
            match x.Consensus_key.consensus_pk with
            | Bls _ -> acc + 1
            | _ -> acc)
          0
          stake_info_list
      in
      let num = Z.(mul (of_int tz4_bakers) (of_int 100)) in
      let den = Z.of_int (List.length stake_info_list) in
      let whole_part, rest = Z.(div_rem num den) in
      let decimals = Z.(div (mul rest (of_int 100)) den) in
      return
      @@ Format.asprintf "%d.%02d%%" (Z.to_int whole_part) (Z.to_int decimals)) ;
  Registration.register0
    ~chunked:false
    S.abaab_activation_level
    (fun ctxt () () ->
      Storage.All_bakers_attest_activation.find
        (Alpha_context.Internal_for_tests.to_raw ctxt))

let current_level ctxt ?(offset = 0l) block =
  RPC_context.make_call0 S.current_level ctxt block {offset} ()

let levels_in_current_cycle ctxt ?(offset = 0l) block =
  RPC_context.make_call0 S.levels_in_current_cycle ctxt block {offset} ()

let consecutive_round_zero ctxt block =
  RPC_context.make_call0 S.consecutive_round_zero ctxt block () ()

let baking_power_distribution_for_current_cycle ctxt block =
  RPC_context.make_call0
    S.baking_power_distribution_for_current_cycle
    ctxt
    block
    ()
    ()

let tz4_baker_number_ratio ctxt ?cycle block =
  RPC_context.make_call0 S.tz4_baker_number_ratio ctxt block cycle ()

let rpc_services =
  register () ;
  RPC_directory.merge rpc_services !Registration.patched_services
