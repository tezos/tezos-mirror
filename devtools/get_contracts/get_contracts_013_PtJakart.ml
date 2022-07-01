(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Proto = struct
  let wrap_tzresult = Environment.wrap_tzresult

  module Context = struct
    type t = Raw_context.t

    let prepare ~level ~predecessor_timestamp ~timestamp ctxt =
      Lwt.map wrap_tzresult
      @@ Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  end

  type context = Context.t

  module Contract = struct
    type repr = Contract_repr.t

    let pp = Contract_repr.pp

    let is_implicit c = Option.is_some @@ Contract_repr.is_implicit c

    let get_code ctxt contract =
      Lwt.map wrap_tzresult @@ Storage.Contract.Code.get ctxt contract

    let get_storage ctxt contract =
      Lwt.map wrap_tzresult @@ Storage.Contract.Storage.get ctxt contract

    let fold ctxt ~init ~f =
      Storage.Contract.fold ctxt ~order:`Undefined ~init ~f
  end

  module Script = struct
    include Alpha_context.Script
    module Hash = Script_expr_hash

    let print_expr = Michelson_v1_printer.print_expr
  end

  module Translator = struct
    type toplevel = Script_ir_translator.toplevel

    type ex_ty = Script_ir_translator.ex_ty

    type ex_code = Script_ir_translator.ex_code

    let expected_code_size Script_ir_translator.(Ex_code (Code {code_size; _}))
        =
      Obj.magic code_size

    let actual_code_size Script_ir_translator.(Ex_code (Code {code; _})) =
      8 * Obj.(reachable_words @@ repr code)

    let parse_ty (ctxt : Raw_context.t) ~legacy ~allow_lazy_storage
        ~allow_operation ~allow_contract ~allow_ticket script =
      let open Result_syntax in
      let+ ty, _ =
        wrap_tzresult
        @@ Script_ir_translator.parse_ty
             (Obj.magic ctxt)
             ~legacy
             ~allow_lazy_storage
             ~allow_operation
             ~allow_contract
             ~allow_ticket
             script
      in
      ty

    let parse_data ?type_logger (ctxt : Raw_context.t) ~legacy ~allow_forged ty
        expr =
      let open Lwt_result_syntax in
      let+ data, _ =
        Lwt.map wrap_tzresult
        @@ Script_ir_translator.parse_data
             ?type_logger
             (Obj.magic ctxt)
             ~legacy
             ~allow_forged
             ty
             expr
      in
      data

    let unparse_ty (ctxt : Raw_context.t) (Script_ir_translator.Ex_ty ty) =
      let open Result_syntax in
      let+ expr, _ =
        wrap_tzresult
        @@ Script_ir_translator.unparse_ty ~loc:0 (Obj.magic ctxt) ty
      in
      expr

    let parse_toplevel (ctxt : Raw_context.t) ~legacy expr =
      let open Lwt_result_syntax in
      let+ toplevel, _ =
        Lwt.map wrap_tzresult
        @@ Script_ir_translator.parse_toplevel (Obj.magic ctxt) ~legacy expr
      in
      toplevel

    let parse_code ctxt ~legacy code =
      let open Lwt_result_syntax in
      let+ parsed_code, _ =
        Lwt.map wrap_tzresult
        @@ Script_ir_translator.parse_code (Obj.magic ctxt) ~legacy ~code
      in
      parsed_code
  end

  module Storage = struct
    type big_map_id = Storage.Big_map.id

    let id_to_z = Lazy_storage_kind.Big_map.Id.unparse_to_z

    let list_values ?offset ?length (ctxt, id) =
      Lwt.map wrap_tzresult
      @@ Storage.Big_map.Contents.list_values ?offset ?length (ctxt, id)

    let get ctxt id =
      Lwt.map wrap_tzresult @@ Storage.Big_map.Value_type.get ctxt id

    let fold ctxt ~init ~f =
      Storage.Big_map.fold ctxt ~order:`Undefined ~init ~f
  end

  module Lambda = struct
    type ex_lambda =
      | Ex_lambda :
          (('a, 'b) Script_typed_ir.lambda, _) Script_typed_ir.ty
          * ('a, 'b) Script_typed_ir.lambda
          -> ex_lambda

    type ex_ty_lambdas =
      | Ex_ty_lambdas :
          ('a, _) Script_typed_ir.ty * ('a -> ex_lambda list) list
          -> ex_ty_lambdas

    let lam_node (Ex_lambda (_, Lam (_, node))) = node

    let rec find_lambda_tys :
        type a c. (a, c) Script_typed_ir.ty -> (a -> ex_lambda list) list =
     fun ty ->
      let open Script_typed_ir in
      match ty with
      | Unit_t | Int_t | Nat_t | Signature_t | String_t | Bytes_t | Mutez_t
      | Key_hash_t | Key_t | Timestamp_t | Address_t | Bool_t | Set_t _
      | Big_map_t _ | Contract_t _ | Operation_t | Sapling_transaction_t _
      | Sapling_transaction_deprecated_t _ | Sapling_state_t _ | Never_t
      | Bls12_381_g1_t | Bls12_381_g2_t | Bls12_381_fr_t | Ticket_t _
      | Chain_id_t | Chest_key_t | Chest_t | Tx_rollup_l2_address_t ->
          []
      | Pair_t (t1, t2, _, _) ->
          let g1 = List.map (fun g (v, _) -> g v) @@ find_lambda_tys t1 in
          let g2 = List.map (fun g (_, v) -> g v) @@ find_lambda_tys t2 in
          g1 @ g2
      | Union_t (t1, t2, _, _) ->
          let g1 =
            List.map (fun g -> function L v -> g v | R _ -> [])
            @@ find_lambda_tys t1
          in
          let g2 =
            List.map (fun g -> function L _ -> [] | R v -> g v)
            @@ find_lambda_tys t2
          in
          g1 @ g2
      | Lambda_t _ -> [(fun g -> [Ex_lambda (ty, g)])]
      | Option_t (t, _, _) ->
          List.map (fun g -> function None -> [] | Some v -> g v)
          @@ find_lambda_tys t
      | List_t (t, _) ->
          List.map (fun g l -> List.flatten @@ List.map g l.elements)
          @@ find_lambda_tys t
      | Map_t (_, tv, _) -> find_lambda_tys_map tv

    and find_lambda_tys_map :
        type tk tv c.
        (tv, c) Script_typed_ir.ty ->
        ((tk, tv) Script_typed_ir.map -> ex_lambda list) list =
     fun tv ->
      let open Script_typed_ir in
      List.map (fun g (Map_tag (module Box) : (tk, tv) map) ->
          Box.OPS.fold (fun _k v acc -> g v @ acc) Box.boxed [])
      @@ find_lambda_tys tv

    let collect_lambda_tys (Script_ir_translator.Ex_ty ty) =
      match find_lambda_tys ty with
      | [] -> None
      | lams -> Some (Ex_ty_lambdas (ty, lams))

    let fold_ex_ty_lambdas (type a) ~(ctxt : Context.t) ~(expr : Script.node)
        ~(f : a -> Script.node -> ex_lambda list -> a) ~(acc : a)
        (Ex_ty_lambdas (ty, getters)) =
      let open Lwt_syntax in
      let+ parse_result =
        Translator.parse_data ctxt ~legacy:true ~allow_forged:true ty expr
      in
      match parse_result with
      | Error _ -> acc
      | Ok data -> (
          match Script_ir_translator.unparse_ty ~loc:0 (Obj.magic ctxt) ty with
          | Error _ -> assert false
          | Ok (ty_expr, _) ->
              List.fold_left (fun acc g -> f acc ty_expr @@ g data) acc getters)
  end

  let is_unpack = function
    | Michelson_v1_primitives.I_UNPACK -> true
    | _ -> false

  let code_storage_type ({storage_type; _} : Translator.toplevel) = storage_type
end

module Main = Get_contracts.Make (Proto)

let () = Get_contracts.main (module Main)
