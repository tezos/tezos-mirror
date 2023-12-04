(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let node_tls () =
  Test.register
    ~title:"Test TLS"
    ~tags:["node"; "tls"]
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
  @@ fun () ->
  let certificate_path = "tezt/tests/tls/tezos.crt" in
  let key_path = "tezt/tests/tls/tezos.key" in
  let rpc_tls = Node.{certificate_path; key_path} in
  let* node = Node.init ~rpc_tls [] in
  Log.info "Check that a curl call to a node RPC fails without --cacert" ;
  let get_version_url =
    RPC_core.make_uri (Node.as_rpc_endpoint node) Node.RPC.get_version
    |> Uri.to_string
  in
  let* () =
    let*? process = Curl.get get_version_url in
    Process.check_error process
  in
  Log.info "Check that a curl to a node RPC works with --cacert" ;
  let*! (_ : JSON.t) =
    Curl.get ~args:["--cacert"; certificate_path] get_version_url
  in
  unit

let register_protocol_independent () = node_tls ()
