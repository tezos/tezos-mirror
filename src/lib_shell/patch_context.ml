(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

let patch_context (genesis : Genesis.t) key_json ctxt =
  let open Lwt_result_syntax in
  let*! ctxt =
    match key_json with
    | None -> Lwt.return ctxt
    | Some (key, json) ->
        Tezos_context.Context.add
          ctxt
          [key]
          (Data_encoding.Binary.to_bytes_exn Data_encoding.json json)
  in
  let* proto = Registered_protocol.get_result genesis.protocol in
  let module Proto = (val proto) in
  let ctxt = Shell_context.wrap_disk_context ctxt in
  let* {context; _} =
    Proto.init
      ctxt
      {
        level = 0l;
        proto_level = 0;
        predecessor = genesis.block;
        timestamp = genesis.time;
        validation_passes = 0;
        operations_hash = Operation_list_list_hash.empty;
        fitness = [];
        context = Context_hash.zero;
      }
  in
  return (Shell_context.unwrap_disk_context context)
