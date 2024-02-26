(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Protocol_client_context
open Alpha_context

type error += Contract_without_code of Contract.t

let () =
  register_error_kind
    `Permanent
    ~id:"contractWithoutCode"
    ~title:"The given contract has no code"
    ~description:
      "Attempt to get the code of a contract failed because it has nocode. No \
       scriptless contract should remain."
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract has no code %a." Contract.pp contract)
    Data_encoding.(obj1 (req "contract" Contract.encoding))
    (function Contract_without_code c -> Some c | _ -> None)
    (fun c -> Contract_without_code c)

let print_errors (cctxt : #Client_context.printer) errs =
  let open Lwt_result_syntax in
  let*! () = cctxt#error "%a" Error_monad.pp_print_trace errs in
  return_unit

let script_entrypoint_type cctxt ~(chain : Chain_services.chain) ~block
    (program : Script.expr) ~entrypoint =
  let open Lwt_result_syntax in
  let*! ty_opt =
    Plugin.RPC.Scripts.entrypoint_type
      cctxt
      (chain, block)
      ~script:program
      ~entrypoint
  in
  match ty_opt with
  | Ok ty -> return_some ty
  | Error
      (Environment.Ecoproto_error (Script_tc_errors.No_such_entrypoint _) :: _)
    ->
      return_none
  | Error _ as err -> Lwt.return err

let contract_entrypoint_type cctxt ~(chain : Chain_services.chain) ~block
    ~contract ~entrypoint ~normalize_types =
  let open Lwt_result_syntax in
  let*! ty_opt =
    Alpha_services.Contract.entrypoint_type
      cctxt
      (chain, block)
      contract
      entrypoint
      ~normalize_types
  in
  match ty_opt with
  | Ok ty -> return_some ty
  | Error (Tezos_rpc.Context.Not_found _ :: _) -> return_none
  | Error _ as err -> Lwt.return err

let print_entrypoint_type (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name ~entrypoint
    =
  let open Lwt_result_syntax in
  function
  | Ok (Some ty) ->
      let*! () =
        if emacs then
          cctxt#message
            "@[<v 2>((entrypoint . %a) (type . %a))@]@."
            Entrypoint.pp
            entrypoint
            Michelson_v1_emacs.print_expr
            ty
        else
          cctxt#message
            "@[<v 2>Entrypoint %a: %a@]@."
            Entrypoint.pp
            entrypoint
            Michelson_v1_printer.print_expr
            ty
      in
      return_unit
  | Ok None ->
      let*! () =
        cctxt#message
          "@[<v 2>No entrypoint named %a%a%a@]@."
          Entrypoint.pp
          entrypoint
          (Format.pp_print_option (fun ppf ->
               Format.fprintf ppf " for contract %a" Contract_hash.pp))
          contract
          (Format.pp_print_option (fun ppf ->
               Format.fprintf ppf " for script %s"))
          script_name
      in
      return_unit
  | Error errs -> on_errors errs

let list_contract_unreachables_and_entrypoints cctxt ~chain ~block ~contract
    ~normalize_types =
  Alpha_services.Contract.list_entrypoints
    cctxt
    (chain, block)
    contract
    ~normalize_types

let list_contract_unreachables cctxt ~chain ~block ~contract =
  let open Lwt_result_syntax in
  let normalize_types =
    (* no need to normalize types as typed entrypoints are ignored *)
    false
  in
  let* unreachables, _types_entrypoints =
    list_contract_unreachables_and_entrypoints
      cctxt
      ~chain
      ~block
      ~contract
      ~normalize_types
  in
  return unreachables

let list_contract_entrypoints cctxt ~chain ~block ~contract ~normalize_types =
  let open Lwt_result_syntax in
  let* _, entrypoints =
    list_contract_unreachables_and_entrypoints
      cctxt
      ~chain
      ~block
      ~contract
      ~normalize_types
  in
  if not @@ List.mem_assoc ~equal:String.equal "default" entrypoints then
    let*! ty_opt =
      contract_entrypoint_type
        cctxt
        ~chain
        ~block
        ~contract
        ~entrypoint:Entrypoint.default
        ~normalize_types
    in
    match ty_opt with
    | Ok (Some ty) -> return (("default", ty) :: entrypoints)
    | Ok None -> return entrypoints
    | Error _ as err -> Lwt.return err
  else return entrypoints

let list_unreachables cctxt ~chain ~block (program : Script.expr) =
  let open Lwt_result_syntax in
  let* unreachables, _ =
    Plugin.RPC.Scripts.list_entrypoints cctxt (chain, block) ~script:program
  in
  return unreachables

let list_entrypoints cctxt ~chain ~block (program : Script.expr) =
  let open Lwt_result_syntax in
  let* _, entrypoints =
    Plugin.RPC.Scripts.list_entrypoints cctxt (chain, block) ~script:program
  in
  if not @@ List.mem_assoc ~equal:String.equal "default" entrypoints then
    let*! ty_opt =
      script_entrypoint_type
        cctxt
        ~chain
        ~block
        program
        ~entrypoint:Entrypoint.default
    in
    match ty_opt with
    | Ok (Some ty) -> return (("default", ty) :: entrypoints)
    | Ok None -> return entrypoints
    | Error _ as err -> Lwt.return err
  else return entrypoints

let print_entrypoints_list (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name =
  let open Lwt_result_syntax in
  function
  | Ok entrypoint_list ->
      let*! () =
        if emacs then
          cctxt#message
            "@[<v 2>(@[%a@])@."
            (Format.pp_print_list
               ~pp_sep:Format.pp_print_cut
               (fun ppf (entrypoint, ty) ->
                 Format.fprintf
                   ppf
                   "@[<v 2>( ( entrypoint . %s ) ( type . @[%a@]))@]"
                   entrypoint
                   Michelson_v1_emacs.print_expr
                   ty))
            entrypoint_list
        else
          cctxt#message
            "@[<v 2>Entrypoints%a%a: @,%a@]@."
            (Format.pp_print_option (fun ppf ->
                 Format.fprintf ppf " for contract %a" Contract_hash.pp))
            contract
            (Format.pp_print_option (fun ppf ->
                 Format.fprintf ppf " for script %s"))
            script_name
            (Format.pp_print_list
               ~pp_sep:Format.pp_print_cut
               (fun ppf (entrypoint, ty) ->
                 Format.fprintf
                   ppf
                   "@[<v 2>%s: @[%a@]@]"
                   entrypoint
                   Michelson_v1_printer.print_expr
                   ty))
            entrypoint_list
      in
      return_unit
  | Error errs -> on_errors errs

let print_unreachables (cctxt : #Client_context.printer)
    ?(on_errors = print_errors cctxt) ~emacs ?contract ?script_name =
  let open Lwt_result_syntax in
  function
  | Ok unreachable ->
      let*! () =
        if emacs then
          cctxt#message
            "@[<v 2>(@[%a@])@."
            (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf path ->
                 Format.fprintf
                   ppf
                   "@[<h>( unreachable-path . %a )@]"
                   (Format.pp_print_list
                      ~pp_sep:Format.pp_print_space
                      (fun ppf prim ->
                        Format.pp_print_string ppf
                        @@ Michelson_v1_primitives.string_of_prim prim))
                   path))
            unreachable
        else
          match unreachable with
          | [] -> cctxt#message "@[<v 2>None.@]@."
          | _ ->
              cctxt#message
                "@[<v 2>Unreachable paths in the argument%a%a: @[%a@]@."
                (Format.pp_print_option (fun ppf ->
                     Format.fprintf ppf " of contract %a" Contract_hash.pp))
                contract
                (Format.pp_print_option (fun ppf ->
                     Format.fprintf ppf " of script %s"))
                script_name
                (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf ->
                     Format.fprintf
                       ppf
                       "@[<h> %a @]"
                       (Format.pp_print_list
                          ~pp_sep:(fun ppf _ -> Format.pp_print_string ppf "/")
                          (fun ppf prim ->
                            Format.pp_print_string ppf
                            @@ Michelson_v1_primitives.string_of_prim prim))))
                unreachable
      in
      return_unit
  | Error errs -> on_errors errs
