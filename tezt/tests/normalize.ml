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

let test_normalize_vanilla =
  Protocol.register_test
    ~__FILE__
    ~title:(sf "normalize data")
    ~tags:["normalize"; "data"]
  @@ fun protocol ->
  let* node = Node.init [] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let test_normalize_mockup =
  Protocol.register_test
    ~__FILE__
    ~title:"normalize data (mockup)"
    ~tags:["mockup"; "normalize"; "data"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let test_normalize_proxy =
  Protocol.register_test
    ~__FILE__
    ~title:"normalize data (proxy)"
    ~tags:["proxy"; "normalize"; "data"]
  @@ fun protocol ->
  let* (_, client) = Proxy.init ~protocol () in
  let* _ = execute_all_modes client in
  Lwt.return_unit

let register ~protocols =
  test_normalize_vanilla protocols ;
  test_normalize_mockup protocols ;
  test_normalize_proxy protocols
