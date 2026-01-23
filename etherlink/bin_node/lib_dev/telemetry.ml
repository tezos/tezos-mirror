(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Jsonrpc = struct
  open Rpc_encodings

  let id_to_otel_value : JSONRPC.id_repr option -> Opentelemetry.value =
    function
    | None -> `None
    | Some (JSONRPC.Id_float f) -> `Float f
    | Some (Id_string s) -> `String s

  let trace_batch_with ~service_name ~batch_size =
    let span_name = Format.sprintf "%s/batch" service_name in
    Opentelemetry_lwt.Trace.with_
      ~service_name
      ~kind:Span_kind_server
      ~attrs:
        [
          ("rpc.system", `String "jsonrpc");
          ("rpc.jsonrpc.version", `String "2.0");
          ("rpc.jsonrpc.batch_length", `Int batch_size);
        ]
      span_name

  let trace_dispatch_with ?(websocket = false) ~service_name method_ id =
    let span_name = Format.sprintf "%s/%s" service_name method_ in
    Opentelemetry_lwt.Trace.with_
      ~service_name
      ~kind:Span_kind_server
      ~attrs:
        [
          ("rpc.system", `String "jsonrpc");
          ("rpc.jsonrpc.version", `String "2.0");
          ("rpc.method", `String method_);
          ("rpc.jsonrpc.request_id", id_to_otel_value id);
          ( "network.protocol.name",
            `String (if websocket then "websocket" else "http") );
        ]
      span_name

  let return_error : JSONRPC.error -> JSONRPC.return_value Lwt.t =
   fun err ->
    (match Opentelemetry.Scope.get_ambient_scope () with
    | Some scope ->
        Opentelemetry.Scope.add_attrs
          scope
          (Fun.const
             [
               ("rpc.jsonrpc.error_code", `Int err.code);
               ("rpc.jsonrpc.error_message", `String err.message);
             ])
    | None -> ()) ;
    Lwt.return (JSONRPC.Direct (Error err))
end

module Attributes = struct
  module Transaction = struct
    let hash hash =
      ( "etherlink.transaction.hash",
        `String (Ethereum_types.hash_to_string hash) )

    let execution_gas qty =
      ("etherlink.transaction.execution_gas", `Int (Z.to_int qty))
  end

  module Block = struct
    let number (Ethereum_types.Qty number) =
      ("etherlink.block.number", `Int (Z.to_int number))

    let execution_gas qty =
      ("etherlink.block.execution_gas", `Int (Z.to_int qty))

    let transaction_count qty = ("etherlink.block.transactions_count", `Int qty)
  end
end
