(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let trace_name service =
  let meth =
    Tezos_rpc.Service.meth service |> Tezos_rpc.Service.string_of_meth
  in
  let path = Tezos_rpc.Service.path service |> Resto.Path.to_string in
  String.concat " " [meth; path]

class with_telemetry ~service_name (cctxt : #Tezos_rpc.Context.generic) :
  Tezos_rpc.Context.generic =
  object
    method base = cctxt#base

    method call_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        'p ->
        'q ->
        'i ->
        'o tzresult Lwt.t =
      fun service params query body ->
        let trace_name = trace_name service in
        Trace.with_tzresult ~service_name ~kind:Span_kind_client trace_name
        @@ fun _ -> cctxt#call_service service params query body

    method call_streamed_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), 'pr, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        on_chunk:('o -> unit) ->
        on_close:(unit -> unit) ->
        'p ->
        'q ->
        'i ->
        (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        let trace_name = trace_name service in
        Trace.with_tzresult ~service_name ~kind:Span_kind_client trace_name
        @@ fun _ ->
        cctxt#call_streamed_service
          service
          ~on_chunk
          ~on_close
          params
          query
          body

    method generic_media_type_call meth ?body uri =
      let trace_name =
        String.concat " " [Tezos_rpc.Service.string_of_meth meth; Uri.path uri]
      in
      Trace.with_tzresult ~service_name ~kind:Span_kind_client trace_name
      @@ fun _ -> cctxt#generic_media_type_call meth ?body uri
  end
