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
      Micheline.strip_locations (Micheline.Prim (loc, Script.I_NONE, [ty], []))
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

  type Environment.Error_monad.error += Cannot_serialize_log_normalized

  let () =
    (* Cannot serialize log *)
    Environment.Error_monad.register_error_kind
      `Temporary
      ~id:"michelson_v1.cannot_serialize_log_normalized"
      ~title:"Not enough gas to serialize normalized execution trace"
      ~description:
        "Execution trace with normalized stacks was to big to be serialized \
         with the provided gas"
      Data_encoding.empty
      (function Cannot_serialize_log_normalized -> Some () | _ -> None)
      (fun () -> Cannot_serialize_log_normalized)

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
      | Key_hash_key tname -> Prim (-1, T_key_hash, [], unparse_type_annot tname)
      | Key_key tname -> Prim (-1, T_key, [], unparse_type_annot tname)
      | Timestamp_key tname ->
          Prim (-1, T_timestamp, [], unparse_type_annot tname)
      | Address_key tname -> Prim (-1, T_address, [], unparse_type_annot tname)
      | Chain_id_key tname -> Prim (-1, T_chain_id, [], unparse_type_annot tname)
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

  let helpers_path = RPC_path.(open_root / "helpers" / "scripts")

  let contract_root =
    (RPC_path.(open_root / "context" / "contracts")
      : RPC_context.t RPC_path.context)

  let big_map_root =
    (RPC_path.(open_root / "context" / "big_maps")
      : RPC_context.t RPC_path.context)

  let unparsing_mode_encoding =
    let open Data_encoding in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          ~title:"Readable"
          (constant "Readable")
          (function
            | Script_ir_translator.Readable -> Some ()
            | Script_ir_translator.Optimized
            | Script_ir_translator.Optimized_legacy ->
                None)
          (fun () -> Script_ir_translator.Readable);
        case
          (Tag 1)
          ~title:"Optimized"
          (constant "Optimized")
          (function
            | Script_ir_translator.Optimized -> Some ()
            | Script_ir_translator.Readable
            | Script_ir_translator.Optimized_legacy ->
                None)
          (fun () -> Script_ir_translator.Optimized);
        case
          (Tag 2)
          ~title:"Optimized_legacy"
          (constant "Optimized_legacy")
          (function
            | Script_ir_translator.Optimized_legacy -> Some ()
            | Script_ir_translator.Readable | Script_ir_translator.Optimized ->
                None)
          (fun () -> Script_ir_translator.Optimized_legacy);
      ]

  let run_code_input_encoding =
    let open Data_encoding in
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
      (obj1 (req "unparsing_mode" unparsing_mode_encoding))

  let normalize_type =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Normalizes some Michelson type by expanding `pair a b c` as `pair a \
         (pair b c)"
      ~input:(obj1 (req "type" Script.expr_encoding))
      ~output:(obj1 (req "normalized" Script.expr_encoding))
      ~query:RPC_query.empty
      RPC_path.(helpers_path / "normalize_type")

  let get_storage_normalized =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Access the data of the contract and normalize it using the requested \
         unparsing mode."
      ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
      ~query:RPC_query.empty
      ~output:(option Script.expr_encoding)
      RPC_path.(contract_root /: Contract.rpc_arg / "storage" / "normalized")

  let get_script_normalized =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Access the script of the contract and normalize it using the \
         requested unparsing mode."
      ~input:(obj1 (req "unparsing_mode" unparsing_mode_encoding))
      ~query:RPC_query.empty
      ~output:(option Script.encoding)
      RPC_path.(contract_root /: Contract.rpc_arg / "script" / "normalized")

  let run_code_normalized =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Run a piece of code in the current context, normalize the output \
         using the requested unparsing mode."
      ~query:RPC_query.empty
      ~input:run_code_input_encoding
      ~output:
        (conv
           (fun (storage, operations, lazy_storage_diff) ->
             (storage, operations, lazy_storage_diff, lazy_storage_diff))
           (fun ( storage,
                  operations,
                  legacy_lazy_storage_diff,
                  lazy_storage_diff )
              ->
             let lazy_storage_diff =
               match lazy_storage_diff with
               | Some s -> Some s
               | None -> legacy_lazy_storage_diff
             in
             (storage, operations, lazy_storage_diff))
           (obj4
              (req "storage" Script.expr_encoding)
              (req
                 "operations"
                 (list Alpha_context.Operation.internal_operation_encoding))
              (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
              (opt "lazy_storage_diff" Lazy_storage.encoding)))
      RPC_path.(helpers_path / "run_code" / "normalized")

  let trace_encoding =
    let open Data_encoding in
    def "scripted.trace" @@ list
    @@ obj3
         (req "location" Script.location_encoding)
         (req "gas" Gas.encoding)
         (req
            "stack"
            (list (obj2 (req "item" Script.expr_encoding) (opt "annot" string))))

  let trace_code_normalized =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Run a piece of code in the current context, keeping a trace, \
         normalize the output using the requested unparsing mode."
      ~query:RPC_query.empty
      ~input:run_code_input_encoding
      ~output:
        (conv
           (fun (storage, operations, trace, lazy_storage_diff) ->
             (storage, operations, trace, lazy_storage_diff, lazy_storage_diff))
           (fun ( storage,
                  operations,
                  trace,
                  legacy_lazy_storage_diff,
                  lazy_storage_diff )
              ->
             let lazy_storage_diff =
               match lazy_storage_diff with
               | Some s -> Some s
               | None -> legacy_lazy_storage_diff
             in
             (storage, operations, trace, lazy_storage_diff))
           (obj5
              (req "storage" Script.expr_encoding)
              (req
                 "operations"
                 (list Alpha_context.Operation.internal_operation_encoding))
              (req "trace" trace_encoding)
              (opt "big_map_diff" Lazy_storage.legacy_big_map_diff_encoding)
              (opt "lazy_storage_diff" Lazy_storage.encoding)))
      RPC_path.(helpers_path / "trace_code" / "normalized")

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
        big_map_root /: Big_map.Id.rpc_arg /: Script_expr_hash.rpc_arg
        / "normalized")

  (* `unparsing_mode_encoding` is not exported by
     lib_protocol/helpers_services.ml for proto_009. *)
  let unparsing_mode_encoding =
    let open Data_encoding in
    let open Script_ir_translator in
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

  let run_view =
    let open Data_encoding in
    RPC_service.post_service
      ~description:
        "Simulate a call to a view following the TZIP-4 standard. See \
         https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-4/tzip-4.md#view-entrypoints."
      ~input:run_view_encoding
      ~output:(obj1 (req "data" Script.expr_encoding))
      ~query:RPC_query.empty
      RPC_path.(helpers_path / "run_view")

  let rpc_services =
    let patched_services =
      ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)
    in
    let register0_fullctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun ctxt q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt q i)
    in
    let register0 s f = register0_fullctxt s (fun {context; _} -> f context) in
    let register1_fullctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun (ctxt, arg) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt -> f ctxt arg q i)
    in
    let register1 s f =
      register1_fullctxt s (fun {context; _} x -> f context x)
    in
    let _register1_noctxt s f =
      patched_services :=
        RPC_directory.register !patched_services s (fun (_, arg) q i ->
            f arg q i)
    in
    let register2_fullctxt s f =
      patched_services :=
        RPC_directory.register
          !patched_services
          s
          (fun ((ctxt, arg1), arg2) q i ->
            Services_registration.rpc_init ctxt >>=? fun ctxt ->
            f ctxt arg1 arg2 q i)
    in
    let register2 s f =
      register2_fullctxt s (fun {context; _} a1 a2 q i -> f context a1 a2 q i)
    in
    let register_field s f =
      register1 s (fun ctxt contract () () ->
          Contract.exists ctxt contract >>=? function
          | true -> f ctxt contract
          | false -> raise Not_found)
    in
    let _register_opt_field s f =
      register_field s (fun ctxt a1 ->
          f ctxt a1 >|=? function None -> raise Not_found | Some v -> v)
    in
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
    (* Extracted and adapted from Contract_services: this function is
       not exported and cannot be refactored since it is in the
       protocol, and its associated service needs a RPC_context, while
       we use an Alpha_context.t *)
    let script_entrypoint_type ctxt expr entrypoint =
      let ctxt = Gas.set_unlimited ctxt in
      let legacy = true in
      let open Script_ir_translator in
      Lwt.return
        ( Script.force_decode_in_context ctxt expr >>? fun (expr, _) ->
          ( parse_toplevel ~legacy expr >>? fun (arg_type, _, _, root_name) ->
            parse_parameter_ty ctxt ~legacy arg_type
            >>? fun (Ex_ty arg_type, _) ->
            Script_ir_translator.find_entrypoint ~root_name arg_type entrypoint
          )
          >>? fun (_f, Ex_ty ty) ->
          unparse_ty ctxt ty >|? fun (ty_node, _) ->
          Micheline.strip_locations ty_node )
    in
    register0 normalize_type (fun ctxt () typ ->
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
    (* Patched RPC: get_storage *)
    register1 get_storage_normalized (fun ctxt contract () unparsing_mode ->
        Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
        match script with
        | None -> return_none
        | Some script ->
            let ctxt = Gas.set_unlimited ctxt in
            let open Script_ir_translator in
            parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
            >>=? fun (Ex_script script, ctxt) ->
            unparse_script ctxt unparsing_mode script >>=? fun (script, ctxt) ->
            Script.force_decode_in_context ctxt script.storage
            >>?= fun (storage, _ctxt) -> return_some storage) ;
    (* Patched RPC: get_script *)
    register1 get_script_normalized (fun ctxt contract () unparsing_mode ->
        Contract.get_script ctxt contract >>=? fun (ctxt, script) ->
        match script with
        | None -> return_none
        | Some script ->
            let ctxt = Gas.set_unlimited ctxt in
            let open Script_ir_translator in
            parse_script ctxt ~legacy:true ~allow_forged_in_storage:true script
            >>=? fun (Ex_script script, ctxt) ->
            unparse_script ctxt unparsing_mode script
            >>=? fun (script, _ctxt) -> return_some script) ;
    register0
      run_code_normalized
      (fun
        ctxt
        ()
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
          unparsing_mode )
      ->
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
        (storage, operations, lazy_storage_diff)) ;
    register0
      trace_code_normalized
      (fun
        ctxt
        ()
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
          unparsing_mode )
      ->
        let module Traced_interpreter = struct
          type log_element =
            | Log :
                context * Script.location * 'a * 'a Script_typed_ir.stack_ty
                -> log_element

          let unparse_stack ctxt (stack, stack_ty) =
            (* We drop the gas limit as this function is only used for debugging/errors. *)
            let ctxt = Gas.set_unlimited ctxt in
            let rec unparse_stack : type a.
                a Script_typed_ir.stack_ty * a ->
                (Script.expr * string option) list
                Environment.Error_monad.tzresult
                Lwt.t = function
              | Empty_t, () -> return_nil
              | Item_t (ty, rest_ty, annot), (v, rest) ->
                  Script_ir_translator.unparse_data ctxt unparsing_mode ty v
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

          module Trace_logger () : Script_interpreter.STEP_LOGGER = struct
            let log : log_element list ref = ref []

            let log_interp ctxt (descr : (_, _) Script_typed_ir.descr) stack =
              log := Log (ctxt, descr.loc, stack, descr.bef) :: !log

            let log_entry _ctxt _descr _stack = ()

            let log_exit ctxt (descr : (_, _) Script_typed_ir.descr) stack =
              log := Log (ctxt, descr.loc, stack, descr.aft) :: !log

            let get_log () =
              Environment.Error_monad.map_s
                (fun (Log (ctxt, loc, stack, stack_ty)) ->
                  Environment.Error_monad.trace
                    Cannot_serialize_log_normalized
                    (unparse_stack ctxt (stack, stack_ty))
                  >>=? fun stack -> return (loc, Gas.level ctxt, stack))
                !log
              >>=? fun res -> return (Some (List.rev res))
          end
        end in
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
        let module Logger = Traced_interpreter.Trace_logger () in
        let logger = (module Logger : Script_interpreter.STEP_LOGGER) in
        Script_interpreter.execute
          ~logger
          ctxt
          unparsing_mode
          step_constants
          ~script:{storage; code}
          ~entrypoint
          ~parameter
          ~internal:true
        >>=? fun {storage; lazy_storage_diff; operations; _} ->
        Logger.get_log () >|=? fun trace ->
        let trace = Option.value ~default:[] trace in
        (storage, operations, trace, lazy_storage_diff)) ;
    register2 big_map_get_normalized (fun ctxt id key () unparsing_mode ->
        let open Script_ir_translator in
        let ctxt = Gas.set_unlimited ctxt in
        Big_map.exists ctxt id >>=? fun (ctxt, types) ->
        match types with
        | None -> raise Not_found
        | Some (_, value_type) -> (
            parse_big_map_value_ty ctxt ~legacy:true (Micheline.root value_type)
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
                >|=? fun (value, _ctxt) -> Micheline.strip_locations value)) ;
    register0
      run_view
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
        script_entrypoint_type ctxt script.Script.code entrypoint
        >>=? fun view_ty ->
        View_helpers.extract_view_output_type entrypoint view_ty >>?= fun ty ->
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
        >>?= fun parameter ->
        Lwt.return (Script_repr.force_decode parameter >|? fst)) ;
    RPC_directory.merge rpc_services !patched_services

  let normalize_type ctxt block ~ty =
    RPC_context.make_call0 normalize_type ctxt block () ty

  let get_storage_normalized ctxt block ~contract ~unparsing_mode =
    RPC_context.make_call1
      get_storage_normalized
      ctxt
      block
      contract
      ()
      unparsing_mode

  let get_script_normalized ctxt block ~contract ~unparsing_mode =
    RPC_context.make_call1
      get_script_normalized
      ctxt
      block
      contract
      ()
      unparsing_mode

  let run_code_normalized ?gas ?(entrypoint = "default") ~script ~storage ~input
      ~amount ~balance ~chain_id ~source ~payer ~unparsing_mode ctxt block =
    RPC_context.make_call0
      run_code_normalized
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

  let trace_code_normalized ?gas ?(entrypoint = "default") ~script ~storage
      ~input ~amount ~balance ~chain_id ~source ~payer ~unparsing_mode ctxt
      block =
    RPC_context.make_call0
      trace_code_normalized
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

  let big_map_get_normalized ctxt block id key ~unparsing_mode =
    RPC_context.make_call2
      big_map_get_normalized
      ctxt
      block
      id
      key
      ()
      unparsing_mode

  let run_view ?gas ~contract ~entrypoint ~input ~chain_id ~source ~payer
      ~unparsing_mode ctxt block =
    RPC_context.make_call0
      run_view
      ctxt
      block
      ()
      (contract, entrypoint, input, chain_id, source, payer, gas, unparsing_mode)

  let get_blocks_preservation_cycles ~get_context:_ = Lwt.return_none
end
