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
   Component: Client - normalize command
   Invocation: dune exec tezt/tests/main.exe -- --file normalize.ml
   Subject: Test the client's command 'normalize data .. of type ...'
  *)

let data = "{Pair 0 3 6 9; Pair 1 (Pair 4 (Pair 7 10)); {2; 5; 8; 11}}"

let typ = "list (pair nat nat nat nat)"

let normalize_modes =
  let open Client in
  [Readable; Optimized; Optimized_legacy]

let execute_all_modes client =
  let legacy = true in
  Lwt_list.map_s
    (fun mode -> Client.normalize_data ~mode ~legacy ~data ~typ client)
    normalize_modes

let test_normalize_vanilla ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "%s: normalize data" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "normalize"; "data"]
  @@ fun () ->
  let* node = Node.init [] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let test_normalize_mockup ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "%s: normalize data (mockup)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "mockup"; "normalize"; "data"]
  @@ fun () ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let test_normalize_proxy ~protocol =
  Test.register
    ~__FILE__
    ~title:(sf "%s: normalize data (proxy)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "normalize"; "data"]
  @@ fun () ->
  let* (_, client) = Proxy.init ~protocol () in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let register protocol =
  test_normalize_vanilla ~protocol ;
  test_normalize_mockup ~protocol ;
  test_normalize_proxy ~protocol
