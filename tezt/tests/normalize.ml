(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file normalize.ml
   Subject:      Regression tests for the "normalize data" command.
*)

let hooks = Tezos_regression.hooks

let modes = Client.[None; Some Readable; Some Optimized; Some Optimized_legacy]

let test_normalize_unparsing_mode =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test normalize in unparsing mode"
    ~tags:["client"; "normalize"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data = "{Pair 0 3 6 9; Pair 1 (Pair 4 (Pair 7 10)); {2; 5; 8; 11}}" in
  let typ = "list (pair nat nat nat nat)" in
  let* () =
    modes
    |> Lwt_list.iter_s (fun mode ->
           let* _ = Client.normalize_data client ~hooks ?mode ~data ~typ in
           unit)
  in
  unit

let test_normalize_legacy_flag =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test normalize with legacy flag"
    ~tags:["client"; "normalize"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data = "{Elt %a 0 1}" in
  let typ = "map nat nat" in
  let* () =
    let* _ = Client.normalize_data client ~legacy:true ~hooks ~data ~typ in
    unit
  in
  let* () =
    Client.spawn_normalize_data client ~legacy:false ~hooks ~data ~typ
    |> Process.check_error ~msg:(rex "unexpected annotation.")
  in
  unit

let test_normalize_script =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test normalize script"
    ~tags:["client"; "normalize"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () =
    modes
    |> Lwt_list.iter_s (fun mode ->
           let* _ =
             Client.normalize_script
               client
               ~hooks
               ?mode
               ~script:
                 "file:./tezt/tests/contracts/proto_alpha/comb-literals.tz"
           in
           unit)
  in
  unit

let register ~protocols =
  test_normalize_unparsing_mode protocols ;
  test_normalize_legacy_flag protocols ;
  test_normalize_script protocols
