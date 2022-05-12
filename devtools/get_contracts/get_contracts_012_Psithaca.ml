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

module Main = Get_contracts.Make (struct
  include Tezos_raw_protocol_012_Psithaca

  let wrap_tzresult =
    Tezos_protocol_environment_012_Psithaca.Environment.wrap_tzresult

  module Context = struct
    type t = Raw_context.t

    let prepare ~level ~predecessor_timestamp ~timestamp ~fitness:_ ctxt =
      Lwt.map wrap_tzresult
      @@ Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  end

  type ('k, 'v) map = ('k, 'v) Script_typed_ir.map

  type ('a, 'r) lambda = ('a, 'r) Script_typed_ir.lambda

  type 'a ty = 'a Script_typed_ir.ty

  type context = Context.t

  module Translator = struct
    type toplevel = Script_ir_translator.toplevel

    type ex_ty = Script_ir_translator.ex_ty = Ex_ty : 'a ty -> ex_ty

    type type_logger = Script_ir_translator.type_logger

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

    let unparse_ty (ctxt : Raw_context.t) ty =
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
  end

  module Contract = struct
    type repr = Contract_repr.t

    let pp = Contract_repr.pp

    let is_implicit = Contract_repr.is_implicit

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

    let print_expr = Tezos_client_012_Psithaca.Michelson_v1_printer.print_expr
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

  module Unparse_types =
    Tezos_protocol_plugin_012_Psithaca.Plugin.RPC.Scripts.Unparse_types

  let is_unpack = function
    | Michelson_v1_primitives.I_UNPACK -> true
    | _ -> false

  let lam_node (Script_typed_ir.Lam (_, node)) = node

  open Script_ir_translator

  let code_storage_type ({storage_type; _} : toplevel) = storage_type
end)

let () = Main.main ()
