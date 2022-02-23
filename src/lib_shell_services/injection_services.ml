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
  open Data_encoding

  let path = RPC_path.(root / "injection")

  let block_query =
    let open RPC_query in
    query (fun async force chain ->
        object
          method async = async

          method force = force

          method chain = chain
        end)
    |+ flag "async" (fun t -> t#async)
    |+ flag "force" (fun t -> t#force)
    |+ opt_field "chain" Chain_services.chain_arg (fun t -> t#chain)
    |> seal

  let block_param =
    obj2
      (req "data" bytes)
      (req
         "operations"
         (list (dynamic_size (list (dynamic_size Operation.encoding)))))

  let block =
    RPC_service.post_service
      ~description:
        "Inject a block in the node and broadcast it. The `operations` \
         embedded in `blockHeader` might be pre-validated using a contextual \
         RPCs from the latest block (e.g. '/blocks/head/context/preapply'). \
         Returns the ID of the block. By default, the RPC will wait for the \
         block to be validated before answering. If ?async is true, the \
         function returns immediately. Otherwise, the block will be validated \
         before the result is returned. If ?force is true, it will be injected \
         even on non strictly increasing fitness. An optional ?chain parameter \
         can be used to specify whether to inject on the test chain or the \
         main chain."
      ~query:block_query
      ~input:block_param
      ~output:Block_hash.encoding
      RPC_path.(path / "block")

  let operation_query =
    let open RPC_query in
    query (fun async chain ->
        object
          method async = async

          method chain = chain
        end)
    |+ flag "async" (fun t -> t#async)
    |+ opt_field "chain" Chain_services.chain_arg (fun t -> t#chain)
    |> seal

  (* If [private_] is set, the [private/injection/operation] path is used,
   * otherwise, it is [/injection/operation].
     This RPC does less checks than [injection/operation] and should be used for
     test or internal use only. The [private/] prefix is used to forbid the use
     of such RPC on a public node *)
  let operation ~private_ =
    RPC_service.post_service
      ~description:
        "Inject an operation in node and broadcast it. Returns the ID of the \
         operation. The `signedOperationContents` should be constructed using \
         a contextual RPCs from the latest block and signed by the client. By \
         default, the RPC will wait for the operation to be (pre-)validated \
         before answering. See RPCs under /blocks/prevalidation for more \
         details on the prevalidation context. If ?async is true, the function \
         returns immediately. Otherwise, the operation will be validated \
         before the result is returned. An optional ?chain parameter can be \
         used to specify whether to inject on the test chain or the main \
         chain."
      ~query:operation_query
      ~input:bytes
      ~output:Operation_hash.encoding
      (if private_ then RPC_path.(root / "private" / "injection" / "operation")
      else RPC_path.(path / "operation"))

  let private_operation = operation ~private_:true

  let operation = operation ~private_:false

  let protocol_query =
    let open RPC_query in
    query (fun async ->
        object
          method async = async
        end)
    |+ flag "async" (fun t -> t#async)
    |> seal

  let protocol =
    RPC_service.post_service
      ~description:
        "Inject a protocol in node. Returns the ID of the protocol. If ?async \
         is true, the function returns immediately. Otherwise, the protocol \
         will be validated before the result is returned."
      ~query:protocol_query
      ~input:Protocol.encoding
      ~output:Protocol_hash.encoding
      RPC_path.(path / "protocol")
end

open RPC_context

let block ctxt ?(async = false) ?(force = false) ?chain raw operations =
  make_call
    S.block
    ctxt
    ()
    (object
       method async = async

       method force = force

       method chain = chain
    end)
    (raw, operations)

let operation_rpc ctxt ~private_rpc ?(async = false) ?chain operation =
  make_call
    (if private_rpc then S.private_operation else S.operation)
    ctxt
    ()
    (object
       method async = async

       method chain = chain
    end)
    operation

let private_operation ctxt ?async ?chain operation =
  operation_rpc ctxt ~private_rpc:true ?async ?chain operation

let operation ctxt ?async ?chain operation =
  operation_rpc ctxt ~private_rpc:false ?async ?chain operation

let protocol ctxt ?(async = false) protocol =
  make_call
    S.protocol
    ctxt
    ()
    (object
       method async = async
    end)
    protocol
