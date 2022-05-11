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

  type ('k, 'v) map = ('k, 'v) Script_typed_ir.map

  type ('a, 'r) lambda = ('a, 'r) Script_typed_ir.lambda

  type 'a ty = 'a Script_typed_ir.ty

  module Error_monad =
    Tezos_protocol_environment_012_Psithaca.Environment.Error_monad

  module Unparse_types =
    Tezos_protocol_plugin_012_Psithaca.Plugin.RPC.Scripts.Unparse_types

  module Raw_context = struct
    include Raw_context

    let prepare ~level ~predecessor_timestamp ~timestamp ~fitness:_ ctxt =
      prepare ~level ~predecessor_timestamp ~timestamp ctxt
  end

  module Script_ir_translator = struct
    include Script_ir_translator

    let unparse_ty ctxt ty = unparse_ty ~loc:0 ctxt ty
  end

  module Contract = struct
    type repr = Contract_repr.t

    let pp = Contract_repr.pp

    let is_implicit = Contract_repr.is_implicit

    let get_code = Storage.Contract.Code.get

    let get_storage = Storage.Contract.Storage.get

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

    let list_values = Storage.Big_map.Contents.list_values

    let get = Storage.Big_map.Value_type.get

    let fold ctxt ~init ~f =
      Storage.Big_map.fold ctxt ~order:`Undefined ~init ~f
  end

  let wrap_tzresult =
    Tezos_protocol_environment_012_Psithaca.Environment.wrap_tzresult

  let is_unpack = function
    | Michelson_v1_primitives.I_UNPACK -> true
    | _ -> false

  let lam_node (Script_typed_ir.Lam (_, node)) = node

  open Script_ir_translator

  let code_storage_type ({storage_type; _} : toplevel) = storage_type
end)

let () = Main.main ()
