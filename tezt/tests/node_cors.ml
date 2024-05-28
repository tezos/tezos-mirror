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

(* Testing
   -------
   Component:    Tests CORS
   Invocation:   dune exec tezt/tests/main.exe -- --file node_cors.ml
   Subject:      Tests the node's [--cors-origin]  flag
*)

let rpc ~verb ~headers uri =
  let* response, response_body =
    Cohttp_lwt_unix.Client.call
      ~headers:(Cohttp.Header.of_list headers)
      verb
      uri
  in
  Log.debug
    ~prefix:"RPC"
    "RPC response: %s"
    (Cohttp.Code.string_of_status response.status) ;
  return (response, response_body)

let check_header ~__LOC__ (response : Cohttp.Response.t) header expected_value =
  Check.(
    (Cohttp.Header.get response.headers header = expected_value)
      (option string)
      ~__LOC__
      ~error_msg:("Expected header " ^ header ^ " to have value %R, got %L"))

let nodes_args = Node.[Cors_origin "*"]

let test_preflight =
  Protocol.register_test ~__FILE__ ~title:"CORS preflight" ~tags:["cors"]
  @@ fun protocol ->
  let* node, _client =
    Client.init_with_protocol ~nodes_args `Client ~protocol ()
  in
  let origin = Constant.default_host in
  let port = Node.rpc_port node in
  let headers =
    [
      ("Origin", origin);
      ("Access-Control-Request-Method", "GET");
      ("Access-Control-Request-Headers", "Content-Type");
    ]
  in
  let uri =
    Uri.make
      ~scheme:"http"
      ~host:origin
      ~port
      ~path:"/chains/main/blocks/head/header/shell"
      ()
  in
  let* response, _response_body = rpc ~verb:`OPTIONS ~headers uri in
  check_header ~__LOC__ response "access-control-allow-origin" (Some "*") ;
  check_header ~__LOC__ response "access-control-allow-methods" (Some "GET") ;
  check_header
    ~__LOC__
    response
    "access-control-allow-headers"
    (Some "Content-Type") ;
  unit

let test_request =
  Protocol.register_test ~__FILE__ ~title:"CORS request" ~tags:["cors"]
  @@ fun protocol ->
  let* node, _client =
    Client.init_with_protocol ~nodes_args `Client ~protocol ()
  in
  let origin = Constant.default_host in
  let port = Node.rpc_port node in
  let headers = [("Origin", origin); ("Content-Type", "application/json")] in
  let uri =
    Uri.make
      ~scheme:"http"
      ~host:origin
      ~port
      ~path:"/chains/main/blocks/head/header/shell"
      ()
  in
  let* response, _response_body = rpc ~verb:`OPTIONS ~headers uri in
  check_header ~__LOC__ response "access-control-allow-origin" (Some "*") ;
  unit

let register ~protocols =
  test_preflight protocols ;
  test_request protocols
