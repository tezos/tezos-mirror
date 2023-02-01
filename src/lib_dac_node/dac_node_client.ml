(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(* TODO https://gitlab.com/tezos/tezos/-/issues/4705
   Dac node client context should be properly tested *)

class type cctxt =
  object
    inherit Tezos_rpc.Context.generic
  end

class unix_cctxt ~rpc_config : cctxt =
  object
    inherit
      Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
        rpc_config
        (Tezos_rpc_http.Media_type.Command_line.of_command_line
           rpc_config.media_type)
  end

let make_unix_cctxt ~scheme ~host ~port =
  let endpoint =
    Uri.with_uri
      ~scheme:(Some scheme)
      ~host:(Some host)
      ~port:(Some port)
      Uri.empty
  in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  new unix_cctxt ~rpc_config

let call (cctxt : #cctxt) = cctxt#call_service

let streamed_call (cctxt : #cctxt) = cctxt#call_streamed_service
