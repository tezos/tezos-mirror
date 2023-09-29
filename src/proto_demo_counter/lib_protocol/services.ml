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

module S = struct
  let path = RPC_path.open_root

  let service_counter_a =
    RPC_service.get_service
      ~description:"Value of counter A"
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "counter" / "a")

  let service_counter_b =
    RPC_service.get_service
      ~description:"Value of counter B"
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(path / "counter" / "b")
end

let rpc_services : Updater.rpc_context RPC_directory.t =
  let open Lwt_result_syntax in
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register ~chunked:false dir S.service_counter_a (fun ctxt () () ->
        let context = ctxt.Updater.context in
        let*! state = State.get_state context in
        return state.State.a)
  in
  let dir =
    RPC_directory.register ~chunked:false dir S.service_counter_b (fun ctxt () () ->
        let context = ctxt.Updater.context in
        let*! state = State.get_state context in
        return state.State.b)
  in
  dir

let get_counter rpc_ctxt chain_blk counter_name =
  match counter_name with
  | `A ->
      RPC_context.make_call0 S.service_counter_a rpc_ctxt chain_blk () ()
  | `B ->
      RPC_context.make_call0 S.service_counter_b rpc_ctxt chain_blk () ()
