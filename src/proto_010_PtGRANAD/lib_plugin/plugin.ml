(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type Environment.Error_monad.error += Cannot_parse_operation (* `Branch *)

type Environment.Error_monad.error += Cannot_serialize_log

let () =
  Environment.Error_monad.register_error_kind
    `Branch
    ~id:"operation.cannot_parse"
    ~title:"Cannot parse operation"
    ~description:"The operation is ill-formed or for another protocol version"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation cannot be parsed")
    Data_encoding.unit
    (function Cannot_parse_operation -> Some () | _ -> None)
    (fun () -> Cannot_parse_operation) ;
  (* Cannot serialize log *)
  Environment.Error_monad.register_error_kind
    `Temporary
    ~id:"michelson_v1.cannot_serialize_log"
    ~title:"Not enough gas to serialize execution trace"
    ~description:
      "Execution trace with stacks was to big to be serialized with the \
       provided gas"
    Data_encoding.empty
    (function Cannot_serialize_log -> Some () | _ -> None)
    (fun () -> Cannot_serialize_log)

module View_helpers = struct
  open Tezos_micheline

  type Environment.Error_monad.error += Viewed_contract_has_no_script

  type Environment.Error_monad.error += View_callback_origination_failed

  type Environment.Error_monad.error +=
    | Illformed_view_type of string * Script.expr

  type Environment.Error_monad.error +=
    | View_never_returns of string * Contract.t

  type Environment.Error_monad.error +=
    | View_unexpected_return of string * Contract.t

  let () =
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewedContractHasNoScript"
      ~title:"Viewed contract has no script"
      ~description:"A view was called on a contract with no script."
      ~pp:(fun ppf () ->
        Format.fprintf ppf "A view was called on a contract with no script.")
      Data_encoding.(unit)
      (function Viewed_contract_has_no_script -> Some () | _ -> None)
      (fun () -> Viewed_contract_has_no_script) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewCallbackOriginationFailed"
      ~title:"View callback origination failed"
      ~description:"View callback origination failed"
      ~pp:(fun ppf () ->
        Format.fprintf ppf "Error during origination of view callback contract.")
      Data_encoding.(unit)
      (function View_callback_origination_failed -> Some () | _ -> None)
      (fun () -> View_callback_origination_failed) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"illformedViewType"
      ~title:"An entrypoint type is incompatible with TZIP-4 view type."
      ~description:"An entrypoint type is incompatible with TZIP-4 view type."
      ~pp:(fun ppf (entrypoint, typ) ->
        Format.fprintf
          ppf
          "The view %s has type %a, it is not compatible with a TZIP-4 view \
           type."
          entrypoint
          Micheline_printer.print_expr
          (Micheline_printer.printable
             (fun x -> x)
             (Michelson_v1_primitives.strings_of_prims typ)))
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "type" Script.expr_encoding))
      (function Illformed_view_type (etp, exp) -> Some (etp, exp) | _ -> None)
      (fun (etp, exp) -> Illformed_view_type (etp, exp)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewNeverReturns"
      ~title:
        "A view never returned a transaction to the given callback contract"
      ~description:
        "A view never initiated a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %s never initiated a transaction to the given callback \
           contract %a."
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c)) ;
    Environment.Error_monad.register_error_kind
      `Permanent
      ~id:"viewUnexpectedReturn"
      ~title:"A view returned an unexpected list of operations"
      ~description:
        "A view initiated a list of operations while the TZIP-4 standard \
         expects only a transaction to the given callback contract."
      ~pp:(fun ppf (entrypoint, callback) ->
        Format.fprintf
          ppf
          "The view %s initiated a list of operations while the TZIP-4 \
           standard expects only a transaction to the given callback contract \
           %a."
          entrypoint
          Contract.pp
          callback)
      Data_encoding.(
        obj2 (req "entrypoint" string) (req "callback" Contract.encoding))
      (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
      (fun (e, c) -> View_never_returns (e, c))

  (* This script is actually never run, its usage is to ensure a
     contract that has the type `contract <ty>` is originated, which
     will be required as callback of the view. *)
  let make_viewer_script ty : Script.t =
    let loc = 0 in
    let ty = Micheline.root ty in
    let code =
      Micheline.strip_locations
      @@ Micheline.Seq
           ( loc,
             [
               Micheline.Prim (loc, Script.K_parameter, [ty], []);
               Micheline.Prim
                 ( loc,
                   Script.K_storage,
                   [Micheline.Prim (loc, Script.T_unit, [], [])],
                   [] );
               Micheline.Prim
                 ( loc,
                   Script.K_code,
                   [Micheline.Prim (loc, Script.I_FAILWITH, [], [])],
                   [] );
             ] )
    in
    let storage =
      Micheline.strip_locations (Micheline.Prim (loc, Script.D_Unit, [], []))
    in
    {code = Script.lazy_expr code; storage = Script.lazy_expr storage}

  let make_view_parameter input callback =
    let loc = 0 in
    Micheline.strip_locations
      (Micheline.Prim
         ( loc,
           Script.D_Pair,
           [
             input;
             Micheline.Bytes
               ( loc,
                 Data_encoding.Binary.to_bytes_exn Contract.encoding callback );
           ],
           [] ))

  let extract_view_output_type entrypoint ty =
    match Micheline.root ty with
    | Micheline.Prim
        ( _,
          Script.T_pair,
          [_; Micheline.Prim (_, Script.T_contract, [ty], _)],
          _ ) ->
        ok (Micheline.strip_locations ty)
    | _ -> Environment.Error_monad.error (Illformed_view_type (entrypoint, ty))

  (* 'view' entrypoints returns their value by calling a callback contract, thus
     the expected result is a unique internal transaction to this callback. *)
  let extract_parameter_from_operations entrypoint operations callback =
    let unexpected_return =
      Environment.Error_monad.error
      @@ View_unexpected_return (entrypoint, callback)
    in
    match operations with
    | [
     Internal_operation
       {operation = Transaction {destination; parameters; _}; _};
    ]
      when Contract.equal destination callback ->
        ok parameters
    | [] ->
        Environment.Error_monad.error
          (View_never_returns (entrypoint, callback))
    | _ -> unexpected_return
end

module RPC = struct
  open Environment
  open Alpha_context
  open Environment.Error_monad

  let parse_operation (op : Operation.raw) =
    match
      Data_encoding.Binary.of_bytes Operation.protocol_data_encoding op.proto
    with
    | Some protocol_data -> ok {shell = op.shell; protocol_data}
    | None -> error Cannot_parse_operation

  let path = RPC_path.(open_root / "helpers")

  module Registration = struct
    let patched_services =
      ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)

    let register0_fullctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let register0 s f = register0_fullctxt s (fun {context; _} -> f context)

    let register0_noctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun _ q i -> f q i)

    let opt_register0_fullctxt s f =
      patched_services :=
        RPC_directory.opt_register !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)

    let opt_register0 s f =
      opt_register0_fullctxt s (fun {context; _} -> f context)

    let register1_fullctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun (ctxt, arg) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt arg q i)

    let register1 s f = register1_fullctxt s (fun {context; _} x -> f context x)

    let register2_fullctxt s f =
      patched_services :=
        RPC_directory.register
          !patched_services
          s
          (fun ((ctxt, arg1), arg2) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt ->
            f ctxt arg1 arg2 q i)

    let register2 s f =
      register2_fullctxt s (fun {context; _} a1 a2 q i -> f context a1 a2 q i)
  end

  let unparsing_mode_encoding =
    let open Script_ir_translator in
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Readable"
          (constant "Readable")
          (function
            | Readable -> Some () | Optimized | Optimized_legacy -> None)
          (fun () -> Readable);
        case
          (Tag 1)
          ~title:"Optimized"
          (constant "Optimized")
          (function
            | Optimized -> Some () | Readable | Optimized_legacy -> None)
          (fun () -> Optimized);
        case
          (Tag 2)
          ~title:"Optimized_legacy"
          (constant "Optimized_legacy")
          (function
            | Optimized_legacy -> Some () | Readable | Optimized -> None)
          (fun () -> Optimized_legacy);
      ]

  module Scripts = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "scripts")

      let run_code_input_encoding =
        merge_objs
          (obj10
             (req "script" Script.expr_encoding)
             (req "storage" Script.expr_encoding)
             (req "input" Script.expr_encoding)
             (req "amount" Tez.encoding)
             (req "balance" Tez.encoding)
             (req "chain_id" Chain_id.encoding)
             (opt "source" Contract.encoding)
             (opt "payer" Contract.encoding)
             (opt "gas" Gas.Arith.z_integral_encoding)
             (dft "entrypoint" string "default"))
          (obj1 (opt "unparsing_mode" unparsing_mode_encoding))

      let run_code_output_encoding =
        conv
          (fun (storage, operations, lazy_storage_diff) ->
            (storage, operations, lazy_storage_diff, lazy_storage_diff))
          (fun (storage, operations, legacy_lazy_storage_diff, lazy_storage_diff)
             ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, lazy_storage_diff))
          (obj4
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let trace_code_input_encoding = run_code_input_encoding

      let trace_encoding =
        def "scripted.trace" @@ list
        @@ obj3
             (req "location" Script.location_encoding)
             (req "gas" Gas.encoding)
             (req
                "stack"
                (list
                   (obj2 (req "item" Script.expr_encoding) (opt "annot" string))))

      let trace_code_output_encoding =
        conv
          (fun (storage, operations, trace, lazy_storage_diff) ->
            (storage, operations, trace, lazy_storage_diff, lazy_storage_diff))
          (fun ( storage,
                 operations,
                 trace,
                 legacy_lazy_storage_diff,
                 lazy_storage_diff )
             ->
            let lazy_storage_diff =
              Option.either lazy_storage_diff legacy_lazy_storage_diff
            in
            (storage, operations, trace, lazy_storage_diff))
          (obj5
             (req "storage" Script.expr_encoding)
             (req "operations" (list Operation.internal_operation_encoding))
             (req "trace" trace_encoding)
             (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
             (opt "lazy_storage_diff" Lazy_storage.encoding))

      let run_view_encoding =
        let open Data_encoding in
        obj8
          (req "contract" Contract.encoding)
          (req "entrypoint" string)
          (req "input" Script.expr_encoding)
          (req "chain_id" Chain_id.encoding)
          (opt "source" Contract.encoding)
          (opt "payer" Contract.encoding)
          (opt "gas" Gas.Arith.z_integral_encoding)
          (req "unparsing_mode" unparsing_mode_encoding)

      let run_code =
        RPC_service.post_service
          ~description:"Run a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:run_code_input_encoding
          ~output:run_code_output_encoding
          RPC_path.(path / "run_code")

      let run_code_normalized =
        RPC_service.post_service
          ~description:
            "Deprecated alias of the .../helpers/scripts/run_code RPC"
          ~query:RPC_query.empty
          ~input:run_code_input_encoding
          ~output:run_code_output_encoding
          RPC_path.(path / "run_code" / "normalized")

      let trace_code =
        RPC_service.post_service
          ~description:
            "Run a piece of code in the current context, keeping a trace"
          ~query:RPC_query.empty
          ~input:trace_code_input_encoding
          ~output:trace_code_output_encoding
          RPC_path.(path / "trace_code")

      let trace_code_normalized =
        RPC_service.post_service
          ~description:
            "Deprecated alias of the .../helpers/scripts/trace_code RPC"
          ~query:RPC_query.empty
          ~input:trace_code_input_encoding
          ~output:trace_code_output_encoding
          RPC_path.(path / "trace_code" / "normalized")

      let run_view =
        RPC_service.post_service
          ~description:
            "Simulate a call to a view following the TZIP-4 standard. See \
             https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints."
          ~input:run_view_encoding
          ~output:(obj1 (req "data" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "run_view")

      let typecheck_code =
        RPC_service.post_service
          ~description:"Typecheck a piece of code in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj3
               (req "program" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
          ~output:
            (obj2
               (req "type_map" Script_tc_errors_registration.type_map_enc)
               (req "gas" Gas.encoding))
          RPC_path.(path / "typecheck_code")

      let typecheck_data =
        RPC_service.post_service
          ~description:
            "Check that some data expression is well formed and of a given \
             type in the current context"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (opt "gas" Gas.Arith.z_integral_encoding)
               (opt "legacy" bool))
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
          ~output:(obj2 (req "packed" bytes) (req "gas" Gas.encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "pack_data")

      let normalize_data =
        RPC_service.post_service
          ~description:
            "Normalizes some data expression using the requested unparsing mode"
          ~input:
            (obj4
               (req "data" Script.expr_encoding)
               (req "type" Script.expr_encoding)
               (req "unparsing_mode" unparsing_mode_encoding)
               (opt "legacy" bool))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_data")

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
            "Normalizes some Michelson type by expanding `pair a b c` as `pair \
             a (pair b c)"
          ~input:(obj1 (req "type" Script.expr_encoding))
          ~output:(obj1 (req "normalized" Script.expr_encoding))
          ~query:RPC_query.empty
          RPC_path.(path / "normalize_type")

      let run_operation =
        RPC_service.post_service
          ~description:"Run an operation without signature checks"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operation" Operation.encoding)
               (req "chain_id" Chain_id.encoding))
          ~output:Apply_results.operation_data_and_metadata_encoding
          RPC_path.(path / "run_operation")

      let entrypoint_type =
        RPC_service.post_service
          ~description:"Return the type of the given entrypoint"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "script" Script.expr_encoding)
               (dft "entrypoint" string "default"))
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
      val unparsing_mode : Script_ir_translator.unparsing_mode
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
        (* We drop the gas limit as this function is only used for debugging/errors. *)
        let ctxt = Gas.set_unlimited ctxt in
        let rec unparse_stack : type a s.
            (a, s) Script_typed_ir.stack_ty * (a * s) ->
            (Script.expr * string option) list tzresult Lwt.t = function
          | Bot_t, (EmptyCell, EmptyCell) -> return_nil
          | Item_t (ty, rest_ty, annot), (v, rest) ->
              Script_ir_translator.unparse_data
                ctxt
                Unparsing_mode.unparsing_mode
                ty
                v
              >>=? fun (data, _ctxt) ->
              unparse_stack (rest_ty, rest) >|=? fun rest ->
              let annot =
                match Script_ir_annot.unparse_var_annot annot with
                | [] -> None
                | [a] -> Some a
                | _ -> assert false
              in
              let data = Micheline.strip_locations data in
              (data, annot) :: rest
        in
        unparse_stack (stack_ty, stack)

      let trace_logger () : Script_typed_ir.logger =
        let log : log_element list ref = ref [] in
        let log_interp _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_entry _ _ctxt _loc _sty _stack = () in
        let log_exit _ ctxt loc sty stack =
          log := Log (ctxt, loc, stack, sty) :: !log
        in
        let log_control _ = () in
        let get_log () =
          map_s
            (fun (Log (ctxt, loc, stack, stack_ty)) ->
              trace Cannot_serialize_log (unparse_stack ctxt (stack, stack_ty))
              >>=? fun stack -> return (loc, Gas.level ctxt, stack))
            !log
          >>=? fun res -> return (Some (List.rev res))
        in
        {log_exit; log_entry; log_interp; get_log; log_control}

      let execute ctxt step_constants ~script ~entrypoint ~parameter =
        let open Script_interpreter in
        let logger = trace_logger () in
        execute
          ~logger
          ctxt
          Unparsing_mode.unparsing_mode
          step_constants
          ~script
          ~entrypoint
          ~parameter
          ~internal:true
        >>=? fun {ctxt; storage; lazy_storage_diff; operations} ->
        logger.get_log () >|=? fun trace ->
        let trace = Option.value ~default:[] trace in
        ({ctxt; storage; lazy_storage_diff; operations}, trace)
    end

    let typecheck_data :
        legacy:bool ->
        context ->
        Script.expr * Script.expr ->
        context tzresult Lwt.t =
     fun ~legacy ctxt (data, exp_ty) ->
      record_trace
        (Script_tc_errors.Ill_formed_type (None, exp_ty, 0))
        (Script_ir_translator.parse_parameter_ty
           ctxt
           ~legacy
           (Micheline.root exp_ty))
      >>?= fun (Ex_ty exp_ty, ctxt) ->
      trace_eval
        (fun () ->
          Lwt.return
            ( Script_ir_translator.serialize_ty_for_error ctxt exp_ty
            >|? fun (exp_ty, _ctxt) ->
              Script_tc_errors.Ill_typed_data (None, data, exp_ty) ))
        (let allow_forged =
           true
           (* Safe since we ignore the value afterwards. *)
         in
         Script_ir_translator.parse_data
           ctxt
           ~legacy
           ~allow_forged
           exp_ty
           (Micheline.root data))
      >|=? fun (_, ctxt) -> ctxt

    module Unparse_types = struct
      (* Same as the unparsing functions for types in Script_ir_translator but
         does not consume gas and never folds (pair a (pair b c)) *)

      open Script_ir_translator
      open Micheline
      open Michelson_v1_primitives
      open Script_ir_annot
      open Script_typed_ir

      let rec unparse_comparable_ty : type a. a comparable_ty -> Script.node =
        function
        | Unit_key tname -> Prim (-1, T_unit, [], unparse_type_annot tname)
        | Never_key tname -> Prim (-1, T_never, [], unparse_type_annot tname)
        | Int_key tname -> Prim (-1, T_int, [], unparse_type_annot tname)
        | Nat_key tname -> Prim (-1, T_nat, [], unparse_type_annot tname)
        | Signature_key tname ->
            Prim (-1, T_signature, [], unparse_type_annot tname)
        | String_key tname -> Prim (-1, T_string, [], unparse_type_annot tname)
        | Bytes_key tname -> Prim (-1, T_bytes, [], unparse_type_annot tname)
        | Mutez_key tname -> Prim (-1, T_mutez, [], unparse_type_annot tname)
        | Bool_key tname -> Prim (-1, T_bool, [], unparse_type_annot tname)
        | Key_hash_key tname ->
            Prim (-1, T_key_hash, [], unparse_type_annot tname)
        | Key_key tname -> Prim (-1, T_key, [], unparse_type_annot tname)
        | Timestamp_key tname ->
            Prim (-1, T_timestamp, [], unparse_type_annot tname)
        | Address_key tname -> Prim (-1, T_address, [], unparse_type_annot tname)
        | Chain_id_key tname ->
            Prim (-1, T_chain_id, [], unparse_type_annot tname)
        | Pair_key ((l, al), (r, ar), pname) ->
            let tl = add_field_annot al None (unparse_comparable_ty l) in
            let tr = add_field_annot ar None (unparse_comparable_ty r) in
            Prim (-1, T_pair, [tl; tr], unparse_type_annot pname)
        | Union_key ((l, al), (r, ar), tname) ->
            let tl = add_field_annot al None (unparse_comparable_ty l) in
            let tr = add_field_annot ar None (unparse_comparable_ty r) in
            Prim (-1, T_or, [tl; tr], unparse_type_annot tname)
        | Option_key (t, tname) ->
            Prim
              (-1, T_option, [unparse_comparable_ty t], unparse_type_annot tname)

      let unparse_memo_size memo_size =
        let z = Alpha_context.Sapling.Memo_size.unparse_to_z memo_size in
        Int (-1, z)

      let rec unparse_ty : type a. a ty -> Script.node =
       fun ty ->
        let return (name, args, annot) = Prim (-1, name, args, annot) in
        match ty with
        | Unit_t tname -> return (T_unit, [], unparse_type_annot tname)
        | Int_t tname -> return (T_int, [], unparse_type_annot tname)
        | Nat_t tname -> return (T_nat, [], unparse_type_annot tname)
        | Signature_t tname -> return (T_signature, [], unparse_type_annot tname)
        | String_t tname -> return (T_string, [], unparse_type_annot tname)
        | Bytes_t tname -> return (T_bytes, [], unparse_type_annot tname)
        | Mutez_t tname -> return (T_mutez, [], unparse_type_annot tname)
        | Bool_t tname -> return (T_bool, [], unparse_type_annot tname)
        | Key_hash_t tname -> return (T_key_hash, [], unparse_type_annot tname)
        | Key_t tname -> return (T_key, [], unparse_type_annot tname)
        | Timestamp_t tname -> return (T_timestamp, [], unparse_type_annot tname)
        | Address_t tname -> return (T_address, [], unparse_type_annot tname)
        | Operation_t tname -> return (T_operation, [], unparse_type_annot tname)
        | Chain_id_t tname -> return (T_chain_id, [], unparse_type_annot tname)
        | Never_t tname -> return (T_never, [], unparse_type_annot tname)
        | Bls12_381_g1_t tname ->
            return (T_bls12_381_g1, [], unparse_type_annot tname)
        | Bls12_381_g2_t tname ->
            return (T_bls12_381_g2, [], unparse_type_annot tname)
        | Bls12_381_fr_t tname ->
            return (T_bls12_381_fr, [], unparse_type_annot tname)
        | Contract_t (ut, tname) ->
            let t = unparse_ty ut in
            return (T_contract, [t], unparse_type_annot tname)
        | Pair_t ((utl, l_field, l_var), (utr, r_field, r_var), tname) ->
            let annot = unparse_type_annot tname in
            let utl = unparse_ty utl in
            let tl = add_field_annot l_field l_var utl in
            let utr = unparse_ty utr in
            let tr = add_field_annot r_field r_var utr in
            return (T_pair, [tl; tr], annot)
        | Union_t ((utl, l_field), (utr, r_field), tname) ->
            let annot = unparse_type_annot tname in
            let utl = unparse_ty utl in
            let tl = add_field_annot l_field None utl in
            let utr = unparse_ty utr in
            let tr = add_field_annot r_field None utr in
            return (T_or, [tl; tr], annot)
        | Lambda_t (uta, utr, tname) ->
            let ta = unparse_ty uta in
            let tr = unparse_ty utr in
            return (T_lambda, [ta; tr], unparse_type_annot tname)
        | Option_t (ut, tname) ->
            let annot = unparse_type_annot tname in
            let ut = unparse_ty ut in
            return (T_option, [ut], annot)
        | List_t (ut, tname) ->
            let t = unparse_ty ut in
            return (T_list, [t], unparse_type_annot tname)
        | Ticket_t (ut, tname) ->
            let t = unparse_comparable_ty ut in
            return (T_ticket, [t], unparse_type_annot tname)
        | Set_t (ut, tname) ->
            let t = unparse_comparable_ty ut in
            return (T_set, [t], unparse_type_annot tname)
        | Map_t (uta, utr, tname) ->
            let ta = unparse_comparable_ty uta in
            let tr = unparse_ty utr in
            return (T_map, [ta; tr], unparse_type_annot tname)
        | Big_map_t (uta, utr, tname) ->
            let ta = unparse_comparable_ty uta in
            let tr = unparse_ty utr in
            return (T_big_map, [ta; tr], unparse_type_annot tname)
        | Sapling_transaction_t (memo_size, tname) ->
            return
              ( T_sapling_transaction,
                [unparse_memo_size memo_size],
                unparse_type_annot tname )
        | Sapling_state_t (memo_size, tname) ->
            return
              ( T_sapling_state,
                [unparse_memo_size memo_size],
                unparse_type_annot tname )
    end

    let register () =
      let originate_dummy_contract ctxt script balance =
        let ctxt = Contract.init_origination_nonce ctxt Operation_hash.zero in
        Lwt.return (Contract.fresh_contract_from_current_nonce ctxt)
        >>=? fun (ctxt, dummy_contract) ->
        Contract.originate
          ctxt
          dummy_contract
          ~balance
          ~delegate:None
          ~script:(script, None)
        >>=? fun ctxt -> return (ctxt, dummy_contract)
      in
      let script_entrypoint_type ctxt expr entrypoint =
        let ctxt = Gas.set_unlimited ctxt in
        let legacy = false in
        let open Script_ir_translator in
        Lwt.return
          ( ( parse_toplevel ~legacy expr >>? fun (arg_type, _, _, root_name) ->
              parse_parameter_ty ctxt ~legacy arg_type
              >>? fun (Ex_ty arg_type, _) ->
              Script_ir_translator.find_entrypoint
                ~root_name
                arg_type
                entrypoint )
          >>? fun (_f, Ex_ty ty) ->
            unparse_ty ctxt ty >|? fun (ty_node, _) ->
            Micheline.strip_locations ty_node )
      in
      let run_code_registration ctxt ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              source,
              payer,
              gas,
              entrypoint ),
            unparsing_mode ) =
        let unparsing_mode =
          Option.value ~default:Script_ir_translator.Readable unparsing_mode
        in
        let storage = Script.lazy_expr storage in
        let code = Script.lazy_expr code in
        originate_dummy_contract ctxt {storage; code} balance
        >>=? fun (ctxt, dummy_contract) ->
        let source, payer =
          match (source, payer) with
          | Some source, Some payer -> (source, payer)
          | Some source, None -> (source, source)
          | None, Some payer -> (payer, payer)
          | None, None -> (dummy_contract, dummy_contract)
        in
        let gas =
          match gas with
          | Some gas -> gas
          | None -> Constants.hard_gas_limit_per_operation ctxt
        in
        let ctxt = Gas.set_limit ctxt gas in
        let step_constants =
          let open Script_interpreter in
          {source; payer; self = dummy_contract; amount; chain_id}
        in
        Script_interpreter.execute
          ctxt
          unparsing_mode
          step_constants
          ~script:{storage; code}
          ~entrypoint
          ~parameter
          ~internal:true
        >|=?
        fun {Script_interpreter.storage; operations; lazy_storage_diff; _} ->
        (storage, operations, lazy_storage_diff)
      in
      Registration.register0 S.run_code run_code_registration ;
      Registration.register0 S.run_code_normalized run_code_registration ;
      let trace_code_registration ctxt ()
          ( ( code,
              storage,
              parameter,
              amount,
              balance,
              chain_id,
              source,
              payer,
              gas,
              entrypoint ),
            unparsing_mode ) =
        let unparsing_mode =
          Option.value ~default:Script_ir_translator.Readable unparsing_mode
        in
        let storage = Script.lazy_expr storage in
        let code = Script.lazy_expr code in
        originate_dummy_contract ctxt {storage; code} balance
        >>=? fun (ctxt, dummy_contract) ->
        let source, payer =
          match (source, payer) with
          | Some source, Some payer -> (source, payer)
          | Some source, None -> (source, source)
          | None, Some payer -> (payer, payer)
          | None, None -> (dummy_contract, dummy_contract)
        in
        let gas =
          match gas with
          | Some gas -> gas
          | None -> Constants.hard_gas_limit_per_operation ctxt
        in
        let ctxt = Gas.set_limit ctxt gas in
        let step_constants =
          let open Script_interpreter in
          {source; payer; self = dummy_contract; amount; chain_id}
        in
        let module Unparsing_mode = struct
          let unparsing_mode = unparsing_mode
        end in
        let module Interp = Traced_interpreter (Unparsing_mode) in
        Interp.execute
          ctxt
          step_constants
          ~script:{storage; code}
          ~entrypoint
          ~parameter
        >|=?
        fun ( {Script_interpreter.storage; operations; lazy_storage_diff; _},
              trace )
          -> (storage, operations, trace, lazy_storage_diff)
      in
      Registration.register0 S.trace_code trace_code_registration ;
      Registration.register0 S.trace_code_normalized trace_code_registration ;
      Registration.register0
        S.run_view
        (fun
          ctxt
          ()
          ( contract,
            entrypoint,
            input,
            chain_id,
            source,
            payer,
            gas,
            unparsing_mode )
        ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script_opt) ->
          Option.fold
            ~some:ok
            ~none:(Error_monad.error View_helpers.Viewed_contract_has_no_script)
            script_opt
          >>?= fun script ->
          Script_repr.(force_decode script.code) >>?= fun decoded_script ->
          script_entrypoint_type ctxt decoded_script entrypoint
          >>=? fun view_ty ->
          View_helpers.extract_view_output_type entrypoint view_ty
          >>?= fun ty ->
          Error_monad.trace View_helpers.View_callback_origination_failed
          @@ originate_dummy_contract
               ctxt
               (View_helpers.make_viewer_script ty)
               Tez.zero
          >>=? fun (ctxt, viewer_contract) ->
          let source, payer =
            match (source, payer) with
            | Some source, Some payer -> (source, payer)
            | Some source, None -> (source, source)
            | None, Some payer -> (payer, payer)
            | None, None -> (contract, contract)
          in
          let gas =
            Option.value
              ~default:(Constants.hard_gas_limit_per_operation ctxt)
              gas
          in
          let ctxt = Gas.set_limit ctxt gas in
          let step_constants =
            let open Script_interpreter in
            {source; payer; self = contract; amount = Tez.zero; chain_id}
          in
          let parameter =
            View_helpers.make_view_parameter
              (Micheline.root input)
              viewer_contract
          in
          Script_interpreter.execute
            ctxt
            unparsing_mode
            step_constants
            ~script
            ~entrypoint
            ~parameter
            ~internal:true
          >>=? fun {Script_interpreter.operations; _} ->
          View_helpers.extract_parameter_from_operations
            entrypoint
            operations
            viewer_contract
          >>?= fun parameter -> Lwt.return (Script_repr.force_decode parameter)) ;
      Registration.register0
        S.typecheck_code
        (fun ctxt () (expr, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          Script_ir_translator.typecheck_code ~legacy ctxt expr
          >|=? fun (res, ctxt) -> (res, Gas.level ctxt)) ;
      Registration.register0
        S.typecheck_data
        (fun ctxt () (data, ty, maybe_gas, legacy) ->
          let legacy = Option.value ~default:false legacy in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          typecheck_data ~legacy ctxt (data, ty) >|=? fun ctxt -> Gas.level ctxt) ;
      Registration.register0 S.pack_data (fun ctxt () (expr, typ, maybe_gas) ->
          let open Script_ir_translator in
          let ctxt =
            match maybe_gas with
            | None -> Gas.set_unlimited ctxt
            | Some gas -> Gas.set_limit ctxt gas
          in
          parse_packable_ty ctxt ~legacy:true (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data
            ctxt
            ~legacy:true
            ~allow_forged:true
            typ
            (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.pack_data ctxt typ data >|=? fun (bytes, ctxt) ->
          (bytes, Gas.level ctxt)) ;
      Registration.register0
        S.normalize_data
        (fun ctxt () (expr, typ, unparsing_mode, legacy) ->
          let open Script_ir_translator in
          let legacy = Option.value ~default:false legacy in
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.parse_any_ty ctxt ~legacy (Micheline.root typ)
          >>?= fun (Ex_ty typ, ctxt) ->
          parse_data ctxt ~legacy ~allow_forged:true typ (Micheline.root expr)
          >>=? fun (data, ctxt) ->
          Script_ir_translator.unparse_data ctxt unparsing_mode typ data
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0
        S.normalize_script
        (fun ctxt () (script, unparsing_mode) ->
          let ctxt = Gas.set_unlimited ctxt in
          Script_ir_translator.unparse_code
            ctxt
            unparsing_mode
            (Micheline.root script)
          >|=? fun (normalized, _ctxt) -> Micheline.strip_locations normalized) ;
      Registration.register0 S.normalize_type (fun ctxt () typ ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          (* Unfortunately, Script_ir_translator.parse_any_ty is not exported *)
          Script_ir_translator.parse_ty
            ctxt
            ~legacy:true
            ~allow_lazy_storage:true
            ~allow_operation:true
            ~allow_contract:true
            ~allow_ticket:true
            (Micheline.root typ)
          >>?= fun (Ex_ty typ, _ctxt) ->
          let normalized = Unparse_types.unparse_ty typ in
          return @@ Micheline.strip_locations normalized) ;
      Registration.register0
        S.run_operation
        (fun
          ctxt
          ()
          ({shell; protocol_data = Operation_data protocol_data}, chain_id)
        ->
          (* this code is a duplicate of Apply without signature check *)
          let partial_precheck_manager_contents (type kind) ctxt
              (op : kind Kind.manager contents) : context tzresult Lwt.t =
            let (Manager_operation
                   {source; fee; counter; operation; gas_limit; storage_limit})
                =
              op
            in
            Gas.consume_limit_in_block ctxt gas_limit >>?= fun ctxt ->
            let ctxt = Gas.set_limit ctxt gas_limit in
            Fees.check_storage_limit ctxt ~storage_limit >>?= fun () ->
            Contract.must_be_allocated ctxt (Contract.implicit_contract source)
            >>=? fun () ->
            Contract.check_counter_increment ctxt source counter >>=? fun () ->
            (match operation with
            | Reveal pk -> Contract.reveal_manager_key ctxt source pk
            | Transaction {parameters; _} ->
                (* Here the data comes already deserialized, so we need to fake the deserialization to mimic apply *)
                let arg_bytes =
                  Data_encoding.Binary.to_bytes_exn
                    Script.lazy_expr_encoding
                    parameters
                in
                let arg =
                  match
                    Data_encoding.Binary.of_bytes
                      Script.lazy_expr_encoding
                      arg_bytes
                  with
                  | Some arg -> arg
                  | None -> assert false
                in
                Lwt.return
                @@ record_trace Apply.Gas_quota_exceeded_init_deserialize
                @@
                (* Fail if not enough gas for complete deserialization cost *)
                ( Script.force_decode_in_context ctxt arg >|? fun (_arg, ctxt) ->
                  ctxt )
            | Origination {script; _} ->
                (* Here the data comes already deserialized, so we need to fake the deserialization to mimic apply *)
                let script_bytes =
                  Data_encoding.Binary.to_bytes_exn Script.encoding script
                in
                let script =
                  match
                    Data_encoding.Binary.of_bytes Script.encoding script_bytes
                  with
                  | Some script -> script
                  | None -> assert false
                in
                Lwt.return
                @@ record_trace Apply.Gas_quota_exceeded_init_deserialize
                @@
                (* Fail if not enough gas for complete deserialization cost *)
                ( Script.force_decode_in_context ctxt script.code
                >>? fun (_code, ctxt) ->
                  Script.force_decode_in_context ctxt script.storage
                  >|? fun (_storage, ctxt) -> ctxt )
            | _ -> return ctxt)
            >>=? fun ctxt ->
            Contract.get_manager_key ctxt source >>=? fun _public_key ->
            (* signature check unplugged from here *)
            Contract.increment_counter ctxt source >>=? fun ctxt ->
            Contract.spend ctxt (Contract.implicit_contract source) fee
          in
          let rec partial_precheck_manager_contents_list : type kind.
              Alpha_context.t ->
              kind Kind.manager contents_list ->
              context tzresult Lwt.t =
           fun ctxt contents_list ->
            match contents_list with
            | Single (Manager_operation _ as op) ->
                partial_precheck_manager_contents ctxt op
            | Cons ((Manager_operation _ as op), rest) ->
                partial_precheck_manager_contents ctxt op >>=? fun ctxt ->
                partial_precheck_manager_contents_list ctxt rest
          in
          let ret contents =
            ( Operation_data protocol_data,
              Apply_results.Operation_metadata {contents} )
          in
          let operation : _ operation = {shell; protocol_data} in
          let hash = Operation.hash {shell; protocol_data} in
          let ctxt = Contract.init_origination_nonce ctxt hash in
          let baker = Tezos_crypto.Signature.V0.Public_key_hash.zero in
          match protocol_data.contents with
          | Single (Manager_operation _) as op ->
              partial_precheck_manager_contents_list ctxt op >>=? fun ctxt ->
              Apply.apply_manager_contents_list ctxt Optimized baker chain_id op
              >|= fun (_ctxt, result) -> ok @@ ret result
          | Cons (Manager_operation _, _) as op ->
              partial_precheck_manager_contents_list ctxt op >>=? fun ctxt ->
              Apply.apply_manager_contents_list ctxt Optimized baker chain_id op
              >|= fun (_ctxt, result) -> ok @@ ret result
          | _ ->
              Apply.apply_contents_list
                ctxt
                chain_id
                Optimized
                shell.branch
                baker
                operation
                operation.protocol_data.contents
              >|=? fun (_ctxt, result) -> ret result) ;
      Registration.register0
        S.entrypoint_type
        (fun ctxt () (expr, entrypoint) ->
          script_entrypoint_type ctxt expr entrypoint) ;
      Registration.register0 S.list_entrypoints (fun ctxt () expr ->
          let ctxt = Gas.set_unlimited ctxt in
          let legacy = false in
          let open Script_ir_translator in
          Lwt.return
            ( parse_toplevel ~legacy expr >>? fun (arg_type, _, _, root_name) ->
              parse_parameter_ty ctxt ~legacy arg_type
              >>? fun (Ex_ty arg_type, _) ->
              Script_ir_translator.list_entrypoints ~root_name arg_type ctxt
              >|? fun (unreachable_entrypoint, map) ->
              ( unreachable_entrypoint,
                Entrypoints_map.fold
                  (fun entry (_, ty) acc ->
                    (entry, Micheline.strip_locations ty) :: acc)
                  map
                  [] ) ))

    let run_code ?unparsing_mode ?gas ?(entrypoint = "default") ~script ~storage
        ~input ~amount ~balance ~chain_id ~source ~payer ctxt block =
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
            source,
            payer,
            gas,
            entrypoint ),
          unparsing_mode )

    let trace_code ?unparsing_mode ?gas ?(entrypoint = "default") ~script
        ~storage ~input ~amount ~balance ~chain_id ~source ~payer ctxt block =
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
            source,
            payer,
            gas,
            entrypoint ),
          unparsing_mode )

    let run_view ?gas ~contract ~entrypoint ~input ~chain_id ?source ?payer
        ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.run_view
        ctxt
        block
        ()
        ( contract,
          entrypoint,
          input,
          chain_id,
          source,
          payer,
          gas,
          unparsing_mode )

    let typecheck_code ?gas ?legacy ~script ctxt block =
      RPC_context.make_call0 S.typecheck_code ctxt block () (script, gas, legacy)

    let typecheck_data ?gas ?legacy ~data ~ty ctxt block =
      RPC_context.make_call0
        S.typecheck_data
        ctxt
        block
        ()
        (data, ty, gas, legacy)

    let pack_data ?gas ~data ~ty ctxt block =
      RPC_context.make_call0 S.pack_data ctxt block () (data, ty, gas)

    let normalize_data ?legacy ~data ~ty ~unparsing_mode ctxt block =
      RPC_context.make_call0
        S.normalize_data
        ctxt
        block
        ()
        (data, ty, unparsing_mode, legacy)

    let normalize_script ctxt block ~script ~unparsing_mode =
      RPC_context.make_call0
        S.normalize_script
        ctxt
        block
        ()
        (script, unparsing_mode)

    let normalize_type ctxt block ~ty =
      RPC_context.make_call0 S.normalize_type ctxt block () ty

    let run_operation ctxt block ~op ~chain_id =
      RPC_context.make_call0 S.run_operation ctxt block () (op, chain_id)

    let entrypoint_type ctxt block ~script ~entrypoint =
      RPC_context.make_call0 S.entrypoint_type ctxt block () (script, entrypoint)

    let list_entrypoints ctxt block ~script =
      RPC_context.make_call0 S.list_entrypoints ctxt block () script
  end

  module Contract = struct
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
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~query:RPC_query.empty
          ~output:(option Script.encoding)
          RPC_path.(path /: Contract.rpc_arg / "script" / "normalized")
    end

    let register () =
      (* Patched RPC: get_storage *)
      Registration.register1
        S.get_storage_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, ctxt) ->
              Script.force_decode_in_context ctxt script.storage
              >>?= fun (storage, _ctxt) -> return_some storage) ;
      (* Patched RPC: get_script *)
      Registration.register1
        S.get_script_normalized
        (fun ctxt contract () unparsing_mode ->
          Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
          match script with
          | None -> return_none
          | Some script ->
              let ctxt = Gas.set_unlimited ctxt in
              let open Script_ir_translator in
              parse_script
                ctxt
                ~legacy:true
                ~allow_forged_in_storage:true
                script
              >>=? fun (Ex_script script, ctxt) ->
              unparse_script ctxt unparsing_mode script
              >>=? fun (script, _ctxt) -> return_some script)

    let get_storage_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_storage_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode

    let get_script_normalized ctxt block ~contract ~unparsing_mode =
      RPC_context.make_call1
        S.get_script_normalized
        ctxt
        block
        contract
        ()
        unparsing_mode
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
            "Access the value associated with a key in a big map, normalize \
             the output using the requested unparsing mode."
          ~query:RPC_query.empty
          ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
          ~output:Script.expr_encoding
          RPC_path.(
            path /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg
            / "normalized")
    end

    let register () =
      Registration.register2
        S.big_map_get_normalized
        (fun ctxt id key () unparsing_mode ->
          let open Script_ir_translator in
          let ctxt = Gas.set_unlimited ctxt in
          Big_map.exists ctxt id >>=? fun (ctxt, types) ->
          match types with
          | None -> raise Not_found
          | Some (_, value_type) -> (
              parse_big_map_value_ty
                ctxt
                ~legacy:true
                (Micheline.root value_type)
              >>?= fun (Ex_ty value_type, ctxt) ->
              Big_map.get_opt ctxt id key >>=? fun (_ctxt, value) ->
              match value with
              | None -> raise Not_found
              | Some value ->
                  parse_data
                    ctxt
                    ~legacy:true
                    ~allow_forged:true
                    value_type
                    (Micheline.root value)
                  >>=? fun (value, ctxt) ->
                  unparse_data ctxt unparsing_mode value_type value
                  >|=? fun (value, _ctxt) -> Micheline.strip_locations value))

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

  module Forge = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "forge")

      let operations =
        RPC_service.post_service
          ~description:"Forge an operation"
          ~query:RPC_query.empty
          ~input:Operation.unsigned_encoding
          ~output:bytes
          RPC_path.(path / "operations")

      let empty_proof_of_work_nonce =
        Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

      let protocol_data =
        RPC_service.post_service
          ~description:"Forge the protocol-specific part of a block header"
          ~query:RPC_query.empty
          ~input:
            (obj4
               (req "priority" uint16)
               (opt "nonce_hash" Nonce_hash.encoding)
               (dft
                  "proof_of_work_nonce"
                  (Fixed.bytes Alpha_context.Constants.proof_of_work_nonce_size)
                  empty_proof_of_work_nonce)
               (dft "liquidity_baking_escape_vote" bool false))
          ~output:(obj1 (req "protocol_data" bytes))
          RPC_path.(path / "protocol_data")
    end

    let register () =
      Registration.register0_noctxt S.operations (fun () (shell, proto) ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Operation.unsigned_encoding
               (shell, proto))) ;
      Registration.register0_noctxt
        S.protocol_data
        (fun
          ()
          ( priority,
            seed_nonce_hash,
            proof_of_work_nonce,
            liquidity_baking_escape_vote )
        ->
          return
            (Data_encoding.Binary.to_bytes_exn
               Block_header.contents_encoding
               {
                 priority;
                 seed_nonce_hash;
                 proof_of_work_nonce;
                 liquidity_baking_escape_vote;
               }))

    module Manager = struct
      let operations ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          ~gas_limit ~storage_limit operations =
        Contract_services.manager_key ctxt block source >>= function
        | Error _ as e -> Lwt.return e
        | Ok revealed ->
            let ops =
              List.map
                (fun (Manager operation) ->
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       }))
                operations
            in
            let ops =
              match (sourcePubKey, revealed) with
              | None, _ | _, Some _ -> ops
              | Some pk, None ->
                  let operation = Reveal pk in
                  Contents
                    (Manager_operation
                       {
                         source;
                         counter;
                         operation;
                         fee;
                         gas_limit;
                         storage_limit;
                       })
                  :: ops
            in
            RPC_context.make_call0
              S.operations
              ctxt
              block
              ()
              ({branch}, Operation.of_list ops)

      let reveal ctxt block ~branch ~source ~sourcePubKey ~counter ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ~sourcePubKey
          ~counter
          ~fee
          ~gas_limit:Gas.Arith.zero
          ~storage_limit:Z.zero
          []

      let transaction ctxt block ~branch ~source ?sourcePubKey ~counter ~amount
          ~destination ?(entrypoint = "default") ?parameters ~gas_limit
          ~storage_limit ~fee () =
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
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [Manager (Transaction {amount; parameters; destination; entrypoint})]

      let origination ctxt block ~branch ~source ?sourcePubKey ~counter ~balance
          ?delegatePubKey ~script ~gas_limit ~storage_limit ~fee () =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
          ~counter
          ~fee
          ~gas_limit
          ~storage_limit
          [
            Manager
              (Origination
                 {
                   delegate = delegatePubKey;
                   script;
                   credit = balance;
                   preorigination = None;
                 });
          ]

      let delegation ctxt block ~branch ~source ?sourcePubKey ~counter ~fee
          delegate =
        operations
          ctxt
          block
          ~branch
          ~source
          ?sourcePubKey
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

    let endorsement ctxt b ~branch ~level () =
      operation ctxt b ~branch (Endorsement {level})

    let proposals ctxt b ~branch ~source ~period ~proposals () =
      operation ctxt b ~branch (Proposals {source; period; proposals})

    let ballot ctxt b ~branch ~source ~period ~proposal ~ballot () =
      operation ctxt b ~branch (Ballot {source; period; proposal; ballot})

    let failing_noop ctxt b ~branch ~message () =
      operation ctxt b ~branch (Failing_noop message)

    let seed_nonce_revelation ctxt block ~branch ~level ~nonce () =
      operation ctxt block ~branch (Seed_nonce_revelation {level; nonce})

    let double_baking_evidence ctxt block ~branch ~bh1 ~bh2 () =
      operation ctxt block ~branch (Double_baking_evidence {bh1; bh2})

    let double_endorsement_evidence ctxt block ~branch ~op1 ~op2 ~slot () =
      operation
        ctxt
        block
        ~branch
        (Double_endorsement_evidence {op1; op2; slot})

    let empty_proof_of_work_nonce =
      Bytes.make Constants_repr.proof_of_work_nonce_size '\000'

    let protocol_data ctxt block ~priority ?seed_nonce_hash
        ?(proof_of_work_nonce = empty_proof_of_work_nonce)
        ~liquidity_baking_escape_vote () =
      RPC_context.make_call0
        S.protocol_data
        ctxt
        block
        ()
        ( priority,
          seed_nonce_hash,
          proof_of_work_nonce,
          liquidity_baking_escape_vote )
  end

  module Parse = struct
    module S = struct
      open Data_encoding

      let path = RPC_path.(path / "parse")

      let operations =
        RPC_service.post_service
          ~description:"Parse operations"
          ~query:RPC_query.empty
          ~input:
            (obj2
               (req "operations" (list (dynamic_size Operation.raw_encoding)))
               (opt "check_signature" bool))
          ~output:(list (dynamic_size Operation.encoding))
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
        Data_encoding.Binary.of_bytes
          Block_header.protocol_data_encoding
          protocol_data
      with
      | None -> Stdlib.failwith "Cant_parse_protocol_data"
      | Some protocol_data -> protocol_data

    let register () =
      Registration.register0 S.operations (fun _ctxt () (operations, check) ->
          map_s
            (fun raw ->
              parse_operation raw >>?= fun op ->
              (match check with
              | Some true -> return_unit (* FIXME *)
              (* I.check_signature ctxt *)
              (* op.protocol_data.signature op.shell op.protocol_data.contents *)
              | Some false | None -> return_unit)
              >|=? fun () -> op)
            operations) ;
      Registration.register0_noctxt S.block (fun () raw_block ->
          return @@ parse_protocol_data raw_block.protocol_data)

    let operations ctxt block ?check operations =
      RPC_context.make_call0 S.operations ctxt block () (operations, check)

    let block ctxt block shell protocol_data =
      RPC_context.make_call0
        S.block
        ctxt
        block
        ()
        ({shell; protocol_data} : Block_header.raw)
  end

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
           located `offset` blocks after in the chain (or before when \
           negative). For instance, the next block if `offset` is 1."
        ~query:level_query
        ~output:Level.encoding
        RPC_path.(path / "current_level")

    let levels_in_current_cycle =
      RPC_service.get_service
        ~description:"Levels of a cycle"
        ~query:level_query
        ~output:
          (obj2
             (req "first" Raw_level.encoding)
             (req "last" Raw_level.encoding))
        RPC_path.(path / "levels_in_current_cycle")
  end

  let register () =
    Scripts.register () ;
    Forge.register () ;
    Parse.register () ;
    Contract.register () ;
    Big_map.register () ;
    Registration.register0 S.current_level (fun ctxt q () ->
        Level.from_raw ctxt ~offset:q.offset (Level.current ctxt).level
        |> return) ;
    Registration.opt_register0 S.levels_in_current_cycle (fun ctxt q () ->
        let rev_levels =
          Level.levels_in_current_cycle ctxt ~offset:q.offset ()
        in
        match rev_levels with
        | [] -> return_none
        | [level] -> return (Some (level.level, level.level))
        | last :: (_ :: _ as rest) ->
            (* The [rev_levels] list is reversed, the last level is the head *)
            let first = List.hd (List.rev rest) in
            return (Some (first.level, last.level)))

  let current_level ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.current_level ctxt block {offset} ()

  let levels_in_current_cycle ctxt ?(offset = 0l) block =
    RPC_context.make_call0 S.levels_in_current_cycle ctxt block {offset} ()

  let rpc_services =
    register () ;
    RPC_directory.merge rpc_services !Registration.patched_services

  let get_blocks_preservation_cycles ~get_context:_ = Lwt.return_none
end
