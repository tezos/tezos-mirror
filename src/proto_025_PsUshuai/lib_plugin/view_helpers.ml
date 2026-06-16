(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
open Tezos_micheline

type Environment.Error_monad.error += Viewed_contract_has_no_script

type Environment.Error_monad.error += View_callback_origination_failed

type Environment.Error_monad.error +=
  | Illformed_view_type of Entrypoint.t * Script.expr

type Environment.Error_monad.error +=
  | View_never_returns of Entrypoint.t * Contract_hash.t

type Environment.Error_monad.error +=
  | View_unexpected_return of Entrypoint.t * Contract_hash.t

type Environment.Error_monad.error += View_not_found of Contract_hash.t * string

type Environment.Error_monad.error += Viewer_unexpected_storage

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
        "The view %a has type %a, it is not compatible with a TZIP-4 view type."
        Entrypoint.pp
        entrypoint
        Micheline_printer.print_expr
        (Micheline_printer.printable
           (fun x -> x)
           (Michelson_v1_primitives.strings_of_prims typ)))
    Data_encoding.(
      obj2
        (req "entrypoint" Entrypoint.simple_encoding)
        (req "type" Script.expr_encoding))
    (function Illformed_view_type (etp, exp) -> Some (etp, exp) | _ -> None)
    (fun (etp, exp) -> Illformed_view_type (etp, exp)) ;
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"viewNeverReturns"
    ~title:"A view never returned a transaction to the given callback contract"
    ~description:
      "A view never initiated a transaction to the given callback contract."
    ~pp:(fun ppf (entrypoint, callback) ->
      Format.fprintf
        ppf
        "The view %a never initiated a transaction to the given callback \
         contract %a."
        Entrypoint.pp
        entrypoint
        Contract_hash.pp
        callback)
    Data_encoding.(
      obj2
        (req "entrypoint" Entrypoint.simple_encoding)
        (req "callback" Contract.originated_encoding))
    (function View_never_returns (e, c) -> Some (e, c) | _ -> None)
    (fun (e, c) -> View_never_returns (e, c)) ;
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"viewUnexpectedReturn"
    ~title:"A view returned an unexpected list of operations"
    ~description:
      "A view initiated a list of operations while the TZIP-4 standard expects \
       only a transaction to the given callback contract."
    ~pp:(fun ppf (entrypoint, callback) ->
      Format.fprintf
        ppf
        "The view %a initiated a list of operations while the TZIP-4 standard \
         expects only a transaction to the given callback contract %a."
        Entrypoint.pp
        entrypoint
        Contract_hash.pp
        callback)
    Data_encoding.(
      obj2
        (req "entrypoint" Entrypoint.simple_encoding)
        (req "callback" Contract.originated_encoding))
    (function View_unexpected_return (e, c) -> Some (e, c) | _ -> None)
    (fun (e, c) -> View_unexpected_return (e, c)) ;
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"viewNotFound"
    ~title:"A view could not be found"
    ~description:"The contract does not have a view of the given name."
    ~pp:(fun ppf (contract, name) ->
      Format.fprintf
        ppf
        "The contract %a does not have a view named `%s`."
        Contract_hash.pp
        contract
        name)
    Data_encoding.(
      obj2 (req "contract" Contract.originated_encoding) (req "view" string))
    (function View_not_found (k, n) -> Some (k, n) | _ -> None)
    (fun (k, n) -> View_not_found (k, n)) ;
  Environment.Error_monad.register_error_kind
    `Permanent
    ~id:"viewerUnexpectedStorage"
    ~title:"A VIEW instruction returned an unexpected value"
    ~description:"A VIEW instruction returned an unexpected value."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The simulated view returned an unexpected value.")
    Data_encoding.unit
    (function Viewer_unexpected_storage -> Some () | _ -> None)
    (fun () -> Viewer_unexpected_storage)

(* This script is actually never run, its usage is to ensure a
   contract that has the type `contract <ty>` is originated, which
   will be required as callback of the view. *)
let make_tzip4_viewer_script ty : Script.michelson_with_storage =
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
             (loc, Data_encoding.Binary.to_bytes_exn Contract.encoding callback);
         ],
         [] ))

let extract_view_output_type entrypoint ty =
  match Micheline.root ty with
  | Micheline.Prim
      (_, Script.T_pair, [_; Micheline.Prim (_, Script.T_contract, [ty], _)], _)
    ->
      Ok (Micheline.strip_locations ty)
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
   Script_typed_ir.Internal_operation
     {
       operation =
         Transaction_to_smart_contract
           {
             destination;
             unparsed_parameters;
             entrypoint = _;
             amount = _;
             parameters = _;
             parameters_ty = _;
             location = _;
           };
       sender = _;
       nonce = _;
     };
  ]
    when Contract_hash.equal destination callback ->
      Ok unparsed_parameters
  | [] ->
      Environment.Error_monad.error (View_never_returns (entrypoint, callback))
  | _ -> unexpected_return

(* [make_michelson_viewer_script contract view input input_ty output_ty]
   generates a script that calls a view from a given contract, and stores the
   result in its storage. *)
let make_michelson_viewer_script address view input input_ty output_ty :
    Script.michelson_with_storage =
  let loc = 0 in
  let address = Micheline.String (loc, Contract.to_b58check address) in
  let push ty value = Micheline.Prim (loc, Script.I_PUSH, [ty; value], []) in
  let storage_decl = Micheline.Prim (loc, Script.T_option, [output_ty], []) in
  let body =
    Micheline.Seq
      ( loc,
        [
          Micheline.Prim (loc, Script.I_DROP, [], []);
          push (Micheline.Prim (loc, Script.T_address, [], [])) address;
          push input_ty (Micheline.root input);
          Micheline.Prim
            (loc, Script.I_VIEW, [Micheline.String (loc, view); output_ty], []);
          Micheline.Prim
            ( loc,
              Script.I_NIL,
              [Micheline.Prim (loc, Script.T_operation, [], [])],
              [] );
          Micheline.Prim (loc, Script.I_PAIR, [], []);
        ] )
  in
  let code =
    Micheline.strip_locations
    @@ Micheline.Seq
         ( loc,
           [
             Micheline.Prim
               ( loc,
                 Script.K_parameter,
                 [Micheline.Prim (loc, Script.T_unit, [], [])],
                 [] );
             Micheline.Prim (loc, Script.K_storage, [storage_decl], []);
             Micheline.Prim (loc, Script.K_code, [body], []);
           ] )
  in
  let storage =
    Micheline.strip_locations (Micheline.Prim (loc, Script.D_None, [], []))
  in
  {code = Script.lazy_expr code; storage = Script.lazy_expr storage}

(* Extracts the value from the mock script generated by
   [make_michelson_viewer_script]. *)
let extract_value_from_storage (storage : Script.expr) =
  match Micheline.root storage with
  | Micheline.Prim (_, Script.D_Some, [value], []) -> Ok value
  | _ -> Environment.Error_monad.error @@ Viewer_unexpected_storage
