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

type deposit_parameters = {
  ex_ticket : Ticket_scanner.ex_ticket;
  zkru_operation : Alpha_context.Zk_rollup.Operation.t;
}

let get_deposit_parameters :
    type a comparable.
    ( (a Script_typed_ir.ticket, bytes) Script_typed_ir.pair,
      comparable )
    Script_typed_ir.ty ->
    (a Script_typed_ir.ticket, bytes) Script_typed_ir.pair ->
    deposit_parameters tzresult =
 fun ty contents ->
  let open Script_typed_ir in
  match (ty, contents) with
  | Pair_t (Ticket_t (ty, _), Bytes_t, _, _), (ticket, op_bytes) -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Alpha_context.Zk_rollup.Operation.encoding
          op_bytes
      with
      | None -> error Alpha_context.Zk_rollup.Errors.Wrong_deposit_parameters
      | Some zkru_operation ->
          ok {ex_ticket = Ticket_scanner.Ex_ticket (ty, ticket); zkru_operation}
      )
