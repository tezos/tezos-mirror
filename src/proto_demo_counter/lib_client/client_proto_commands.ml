(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
module Demo_block_services = Block_services.Make (Protocol) (Protocol)

let bake (cctxt : Protocol_client_context.full) message : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* {validated; _} =
    Demo_block_services.Mempool.pending_operations cctxt ()
  in
  let operations = List.map snd validated in
  let block_header_data = Header.create message in
  let* shell, preapply_result =
    Demo_block_services.Helpers.Preapply.block
      cctxt
      [operations]
      ~protocol_data:block_header_data
  in
  let block_header_data_encoded =
    Data_encoding.Binary.to_bytes_exn Header.encoding block_header_data
  in
  let header : Block_header.t =
    {shell; protocol_data = block_header_data_encoded}
  in
  let header_encoded =
    Data_encoding.Binary.to_bytes_exn Block_header.encoding header
  in
  let preapply_result =
    WithExceptions.Option.get ~loc:__LOC__ @@ List.hd preapply_result
  in
  let operations = [List.map snd preapply_result.applied] in
  let* block_hash =
    Shell_services.Injection.block cctxt header_encoded operations
  in
  let*! () = cctxt#message "Injected block %a" Block_hash.pp_short block_hash in
  return_unit

let operation_encoding =
  let open Data_encoding in
  merge_objs
    Operation.shell_header_encoding
    (obj1 (req "contents" Proto_operation.encoding))

let forge_op = Data_encoding.Binary.to_bytes_exn operation_encoding

let inject_op (cctxt : Protocol_client_context.full) (pop : Proto_operation.t) =
  let open Lwt_result_syntax in
  let* (block_hash : Block_hash.t) = Demo_block_services.hash cctxt () in
  let shell_header : Operation.shell_header = Operation.{branch = block_hash} in
  let op : operation = {shell = shell_header; protocol_data = pop} in
  let* result = Demo_block_services.Helpers.Preapply.operations cctxt [op] in
  match result with
  | [(_op_data, op_receipt)] ->
      let receipt_str = Receipt.to_string op_receipt in
      let*! () = cctxt#message "Operation receipt: %s" receipt_str in
      let mbytes = forge_op (shell_header, pop) in
      let* op_hash = Shell_services.Injection.operation cctxt mbytes in
      let injected = Operation_hash.to_short_b58check op_hash in
      let*! () = cctxt#message "Injected: %s" injected in
      return_unit
  | _ -> assert false

let get_counter (cctxt : Protocol_client_context.full) account =
  let open Lwt_result_syntax in
  let* cnt = Services.get_counter cctxt (cctxt#chain, cctxt#block) account in
  let*! () = cctxt#message "The counter value is %d" (Int32.to_int cnt) in
  return_unit
