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

type Error_monad.error += Injection_operations_error

type Error_monad.error += Injection_operation_succeed_case of Operation_hash.t

type Error_monad.error += Injection_operation_error_case of Operation_hash.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"injection_operations_error"
    ~title:"Injection operations error"
    ~description:
      "While injecting several operations at once, one or several injections \
       failed."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "While injecting several operations, one or several injections failed. \
         Errors are the one below in the trace.")
    unit
    (function Injection_operations_error -> Some () | _ -> None)
    (function () -> Injection_operations_error) ;
  register_error_kind
    `Permanent
    ~id:"injection_operation_succeed_case"
    ~title:"Injection operation succeed"
    ~description:
      "The injection of this operation succeed among a list of injections \
       containing at least one error."
    ~pp:(fun ppf oph ->
      Format.fprintf ppf "Injection of %a succeeded." Operation_hash.pp oph)
    (obj1 (req "oph" Operation_hash.encoding))
    (function Injection_operation_succeed_case oph -> Some oph | _ -> None)
    (function oph -> Injection_operation_succeed_case oph) ;
  register_error_kind
    `Permanent
    ~id:"injection_operation_error_case"
    ~title:"Injection operation error"
    ~description:
      "The injection of this operation failed. The error trace are the \
       following errors in this list."
    ~pp:(fun ppf oph ->
      Format.fprintf
        ppf
        "Injection of %a failed. Error is next."
        Operation_hash.pp
        oph)
    (obj1 (req "oph" Operation_hash.encoding))
    (function Injection_operation_error_case oph -> Some oph | _ -> None)
    (function oph -> Injection_operation_error_case oph)

module S = struct
  open Data_encoding

  let path = Tezos_rpc.Path.(root / "injection")

  let block_query =
    let open Tezos_rpc.Query in
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
    Tezos_rpc.Service.post_service
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
      Tezos_rpc.Path.(path / "block")

  let operation_query =
    let open Tezos_rpc.Query in
    query (fun async chain ->
        object
          method async = async

          method chain = chain
        end)
    |+ flag "async" (fun t -> t#async)
    |+ opt_field "chain" Chain_services.chain_arg (fun t -> t#chain)
    |> seal

  let operations_query =
    let open Tezos_rpc.Query in
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

  (* If [private_] is set, the [private/injection/operation] path is used,
   * otherwise, it is [/injection/operation].
     This RPC does less checks than [injection/operation] and should be used for
     test or internal use only. The [private/] prefix is used to forbid the use
     of such RPC on a public node *)
  let operation ~private_ =
    Tezos_rpc.Service.post_service
      ~description:
        "Inject an operation in node and broadcast it. Returns the ID of the \
         operation. The `signedOperationContents` should be constructed using \
         contextual RPCs from the latest block and signed by the client. The \
         injection of the operation will apply it on the current mempool \
         context. This context may change at each operation injection or \
         operation reception from peers. By default, the RPC will wait for the \
         operation to be (pre-)validated before returning. However, if ?async \
         is true, the function returns immediately. The optional ?chain \
         parameter can be used to specify whether to inject on the test chain \
         or the main chain."
      ~query:operation_query
      ~input:bytes
      ~output:Operation_hash.encoding
      (if private_ then
         Tezos_rpc.Path.(root / "private" / "injection" / "operation")
       else Tezos_rpc.Path.(path / "operation"))

  let private_operations =
    Tezos_rpc.Service.post_service
      ~description:
        "Inject a list of operations in a node. If [force] is [true] then the \
         operations are immediatly injected. The injection will succeed, but \
         it does not mean the operations are (all) valid. In any case, the \
         injection will be quick, hence [async] will be taken into account but \
         should have almost no impact. If [async] is [true], all the promises \
         returned by injecting an operation will be dropped. Each injection is \
         done independently, and does not depend on the other injected \
         operations result. Otherwise ([async]=[force]=[false]), for each \
         operation, we record a list of promises. If all the injections \
         succeed, the result is the list of operation hashes injected, \
         otherwise an error (\"injection_operations_error\") is returned. This \
         error is followed by markers for each operation: \
         \"injection_operation_succeed\" for success and \
         \"injection_operation_error\" for failure (followed by the errors \
         specific to this injection)."
      ~query:operations_query
      ~input:(list (dynamic_size bytes))
      ~output:(list Operation_hash.encoding)
      Tezos_rpc.Path.(root / "private" / "injection" / "operations")

  let private_operation = operation ~private_:true

  let operation = operation ~private_:false

  let protocol_query =
    let open Tezos_rpc.Query in
    query (fun async ->
        object
          method async = async
        end)
    |+ flag "async" (fun t -> t#async)
    |> seal

  let protocol =
    Tezos_rpc.Service.post_service
      ~description:
        "Inject a protocol in node. Returns the ID of the protocol. If ?async \
         is true, the function returns immediately. Otherwise, the protocol \
         will be validated before the result is returned."
      ~query:protocol_query
      ~input:Protocol.encoding
      ~output:Protocol_hash.encoding
      Tezos_rpc.Path.(path / "protocol")
end

open Tezos_rpc.Context

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

let private_operations ctxt ?(async = false) ?(force = false) ?chain operations
    =
  make_call
    S.private_operations
    ctxt
    ()
    (object
       method async = async

       method chain = chain

       method force = force
    end)
    operations

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
