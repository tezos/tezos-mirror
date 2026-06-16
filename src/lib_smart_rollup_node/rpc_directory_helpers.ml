(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Conveniences to construct RPC directory
   against a subcontext of the Node_context *)

open Tezos_rpc

module type PARAM = sig
  type prefix

  type context

  type subcontext

  val context_of_prefix : context -> prefix -> subcontext tzresult Lwt.t

  val prefix : (unit, prefix) Path.t
end

module type SUBDIRECTORY = sig
  type prefix

  type context

  type subcontext

  val register0 :
    ([< Resto.meth], 'prefix, 'prefix, 'query, 'input, 'output) Service.t ->
    (subcontext -> 'query -> 'input -> 'output tzresult Lwt.t) ->
    unit

  val register1 :
    ( [< Resto.meth],
      'prefix,
      'prefix * 'param1,
      'query,
      'input,
      'output )
    Service.t ->
    (subcontext -> 'param1 -> 'query -> 'input -> 'output tzresult Lwt.t) ->
    unit

  val gen_register0 :
    ([< Resto.meth], 'prefix, 'prefix, 'query, 'input, 'output) Service.t ->
    (subcontext -> 'query -> 'input -> 'output Answer.t Lwt.t) ->
    unit

  val build_sub_directory : context -> prefix Directory.t
end

module Make_sub_directory (S : PARAM) = struct
  open S

  let directory : subcontext tzresult Tezos_rpc.Directory.t ref =
    ref Tezos_rpc.Directory.empty

  let service_name = "Rpc_server"

  let register service f =
    let prefix = Resto.Path.to_string prefix in
    let path = Tezos_rpc.Service.path service |> Resto.Path.to_string in
    let route = prefix ^ path in
    let meth =
      Tezos_rpc.Service.meth service |> Tezos_rpc.Service.string_of_meth
    in
    let trace_name = String.concat " " [meth; route] in
    let f a q i =
      Octez_telemetry.Trace.with_tzresult
        ~kind:Span_kind_server
        ~service_name
        trace_name
        ~attrs:[("http.route", `String route)]
      @@ fun _scope -> f a q i
    in
    directory := Tezos_rpc.Directory.register !directory service f

  let register0 service f =
    let open Lwt_result_syntax in
    register (Tezos_rpc.Service.subst0 service) @@ fun ctxt query input ->
    let*? ctxt in
    f ctxt query input

  let register1 service f =
    let open Lwt_result_syntax in
    register (Tezos_rpc.Service.subst1 service)
    @@ fun (ctxt, arg) query input ->
    let*? ctxt in
    f ctxt arg query input

  let build_sub_directory node_ctxt =
    !directory
    |> Tezos_rpc.Directory.map (fun prefix ->
           context_of_prefix node_ctxt prefix)

  let gen_register service f =
    directory := Tezos_rpc.Directory.gen_register !directory service f

  let gen_register0 service f =
    gen_register (Tezos_rpc.Service.subst0 service) @@ fun ctxt query input ->
    match ctxt with
    | Error e -> Tezos_rpc.Answer.fail e
    | Ok ctxt -> f ctxt query input
end

module Make_directory (S : PARAM) = struct
  include Make_sub_directory (S)

  let of_subdirectory = Tezos_rpc.Directory.prefix S.prefix

  let build_directory node_ctxt =
    build_sub_directory node_ctxt |> Tezos_rpc.Directory.prefix S.prefix
end
