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
  let endpoint = Uri.make ~scheme ~host ~port () in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  new unix_cctxt ~rpc_config

let of_uri uri =
  let endpoint = uri in
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  new unix_cctxt ~rpc_config

module V0 = struct
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4895
     If the preimage was generated using a different plugin, the computation of
     the hash might fail. In practice it would be better to retrieve the
     hash of the protocol that the coordinator was using when the page hash
     was computed.
  *)
  let get_preimage (cctxt : #cctxt) ~page_hash =
    cctxt#call_service RPC_services.V0.get_preimage ((), page_hash) () ()

  let post_store_preimage (cctxt : #cctxt) ~payload =
    cctxt#call_service RPC_services.V0.post_store_preimage () () payload

  let get_verify_signature (cctxt : #cctxt) ~external_message =
    cctxt#call_service
      RPC_services.V0.get_verify_signature
      ()
      external_message
      ()

  let put_dac_member_signature (cctxt : #cctxt) ~signature =
    cctxt#call_service RPC_services.V0.put_dac_member_signature () () signature

  let get_certificate (cctxt : #cctxt) ~root_page_hash =
    cctxt#call_service
      RPC_services.V0.get_certificate
      ((), root_page_hash)
      ()
      ()

  let get_serialized_certificate (cctxt : #cctxt) ~root_page_hash =
    cctxt#call_service
      RPC_services.V0.get_serialized_certificate
      ((), root_page_hash)
      ()
      ()

  let monitor_certificate (cctxt : #cctxt) ~root_hash =
    Monitor_services.V0.certificate cctxt root_hash

  module Coordinator = struct
    let post_preimage (cctxt : #cctxt) ~payload =
      cctxt#call_service RPC_services.V0.Coordinator.post_preimage () () payload
  end

  module Observer = struct
    let get_missing_page (cctxt : #cctxt) page_hash =
      cctxt#call_service RPC_services.V0.get_missing_page ((), page_hash) () ()
  end
end
