(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

open Genesis_chain

let patch_context key_json ctxt =
  ( match key_json with
  | None ->
      Lwt.return ctxt
  | Some (key, json) ->
      Tezos_storage.Context.set
        ctxt
        [key]
        (Data_encoding.Binary.to_bytes_exn Data_encoding.json json) )
  >>= fun ctxt ->
  (* TODO: this code seems to be shared with validator.ml, function run:
     can we share it? *)
  match Registered_protocol.get genesis.protocol with
  | None ->
      assert false (* FIXME error *)
  | Some proto -> (
      let module Proto = (val proto) in
      let ctxt = Shell_context.wrap_disk_context ctxt in
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
      >>= function
      | Error _ ->
          assert false (* FIXME error *)
      | Ok {context; _} ->
          let context = Shell_context.unwrap_disk_context context in
          Lwt.return context )
