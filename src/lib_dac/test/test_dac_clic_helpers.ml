(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

(** Testing
    -------
    Component:  Lib_dac Dac_clic_helpers
    Invocation: dune exec src/lib_dac/test/main.exe -- --file test_dac_clic_helpers.ml
    Subject:    Tests for the Dac_clic_helpers.
*)

open Dac_clic_helpers

module Parsed_rpc_test = struct
  type public_parsed_rpc = {scheme : string; host : string; port : int}

  let public_parsed_rpc Parsed_rpc.{scheme; host; port} = {scheme; host; port}

  let get_ok = Stdlib.Result.get_ok

  let public_ok_parsed_rpc rpc =
    get_ok @@ Parsed_rpc.of_string rpc |> public_parsed_rpc

  let test_ipaddr_host_and_port () =
    let rpc = "127.0.0.1:12345" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "http"; host = "127.0.0.1"; port = 12345}

  let test_ipaddr_port_defaults_to_80 () =
    let rpc = "127.0.0.1" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "http"; host = "127.0.0.1"; port = 80}

  let test_invalid_ipaddr_fails () =
    let rpc = Parsed_rpc.of_string "127.123:12345" in
    Assert.equal
      ~loc:__LOC__
      rpc
      (Error (`Parse_rpc_error "Not a valid rpc address."))

  let test_empty_addr_fails () =
    let rpc = Parsed_rpc.of_string "" in
    Assert.equal
      ~loc:__LOC__
      rpc
      (Error (`Parse_rpc_error "Not a valid rpc address."))

  let test_endpoint_host_http_and_port () =
    let rpc = "http://tezos.com:80" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "http"; host = "tezos.com"; port = 80}

  let test_endpoint_host_https_and_port () =
    let rpc = "https://tezos.com:10832" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "https"; host = "tezos.com"; port = 10832}

  let test_endpoint_http_defaults_port_80 () =
    let rpc = "http://tezos.com" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "http"; host = "tezos.com"; port = 80}

  let test_endpoint_https_defaults_port_443 () =
    let rpc = "https://tezos.com" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "https"; host = "tezos.com"; port = 443}

  let test_ipaddr_with_http_treated_is_ok () =
    let rpc = "http://127.0.0.1:10832" in
    Assert.equal
      ~loc:__LOC__
      (public_ok_parsed_rpc rpc)
      {scheme = "http"; host = "127.0.0.1"; port = 10832}

  let tests =
    [
      Alcotest.test_case
        "Test `{ipaddr}:{port}` is valid"
        `Quick
        test_ipaddr_host_and_port;
      Alcotest.test_case
        "Test `{ipaddr}` defaults port to 80"
        `Quick
        test_ipaddr_port_defaults_to_80;
      Alcotest.test_case
        "Test invalid ipaddr fails."
        `Quick
        test_invalid_ipaddr_fails;
      Alcotest.test_case "Test empty rpc fails" `Quick test_empty_addr_fails;
      Alcotest.test_case
        "Test `http://{endpoint}:{port}` is valid"
        `Quick
        test_endpoint_host_http_and_port;
      Alcotest.test_case
        "Test `https://{endpoint}:{port}` is valid"
        `Quick
        test_endpoint_host_https_and_port;
      Alcotest.test_case
        "Test `http://{endpoint}` defaults port to 80"
        `Quick
        test_endpoint_http_defaults_port_80;
      Alcotest.test_case
        "Test `https://{endpoint}` defaults port to 443"
        `Quick
        test_endpoint_https_defaults_port_443;
      Alcotest.test_case
        "Test `http://{ipaddr}:{port}` is valid"
        `Quick
        test_ipaddr_with_http_treated_is_ok;
    ]
end

let () =
  Alcotest.run
    ~__FILE__
    "lib_dac"
    [("Dac_clic_helpers.Parsed_rpc", Parsed_rpc_test.tests)]
