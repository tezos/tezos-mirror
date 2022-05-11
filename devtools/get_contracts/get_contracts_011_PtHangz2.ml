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
  include Tezos_raw_protocol_011_PtHangz2

  module Error_monad =
    Tezos_protocol_environment_011_PtHangz2.Environment.Error_monad

  module Unparse_types =
    Tezos_protocol_plugin_011_PtHangz2.Plugin.RPC.Scripts.Unparse_types

  module Client = Tezos_client_011_PtHangz2

  let wrap_tzresult =
    Tezos_protocol_environment_011_PtHangz2.Environment.wrap_tzresult

  let is_unpack = function
    | Michelson_v1_primitives.I_UNPACK -> true
    | _ -> false

  let lam_node (Script_typed_ir.Lam (_, node)) = node

  open Script_ir_translator

  let code_storage_type ({storage_type; _} : toplevel) = storage_type
end)

let () = Main.main ()
