(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  open Data_encoding

  let path = Tezos_rpc.Path.(root / "broadcast")

  let dests_query =
    let open Tezos_rpc.Query in
    query (fun dests ->
        object
          method dests = dests
        end)
    |+ multi_field "dests" Tezos_rpc.Arg.int (fun t -> t#dests)
    |> seal

  (* copied from lib_shell_services/injection_services.ml *)
  let block_param =
    obj2
      (req "block" (dynamic_size Block_header.encoding))
      (req
         "operations"
         (list (dynamic_size (list (dynamic_size Operation.encoding)))))

  let block =
    Tezos_rpc.Service.post_service
      ~description:"Broadcast a block."
      ~query:dests_query
      ~input:block_param
      ~output:unit
      Tezos_rpc.Path.(path / "block")

  let operation =
    Tezos_rpc.Service.post_service
      ~description:"Broadcast an operation."
      ~query:dests_query
      ~input:Alpha_context.Operation.encoding
      ~output:unit
      Tezos_rpc.Path.(path / "operation")
end

open Tezos_rpc.Context

let block ctxt ?(dests = []) raw operations =
  make_call
    S.block
    ctxt
    ()
    (object
       method dests = dests
    end)
    (raw, operations)

let operation ctxt ?(dests = []) operation =
  make_call
    S.operation
    ctxt
    ()
    (object
       method dests = dests
    end)
    operation
