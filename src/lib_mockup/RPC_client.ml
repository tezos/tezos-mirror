(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

class mockup_ctxt (base_dir : string) (mem_only : bool)
  (mockup_env : Tezos_mockup_registration.Registration.mockup_environment)
  (chain_id : Chain_id.t) (rpc_context : Tezos_protocol_environment.rpc_context)
  protocol_data :
  Tezos_rpc.Context.generic =
  let local_ctxt =
    Tezos_mockup_proxy.RPC_client.local_ctxt
      (Local_services.build_directory
         base_dir
         mem_only
         mockup_env
         chain_id
         rpc_context
         protocol_data)
  in
  object
    method base = local_ctxt#base

    method generic_media_type_call meth ?body uri =
      local_ctxt#generic_media_type_call meth ?body uri

    method call_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        'p ->
        'q ->
        'i ->
        'o tzresult Lwt.t =
      fun service params query body ->
        local_ctxt#call_service service params query body

    method call_streamed_service :
        'm 'p 'q 'i 'o.
        (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
        on_chunk:('o -> unit) ->
        on_close:(unit -> unit) ->
        'p ->
        'q ->
        'i ->
        (unit -> unit) tzresult Lwt.t =
      fun service ~on_chunk ~on_close params query body ->
        local_ctxt#call_streamed_service
          service
          ~on_chunk
          ~on_close
          params
          query
          body
  end
