(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Subject:      Regression tests for Michelson normalization commands.
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
    |> Lwt_list.iter_s @@ fun mode ->
       let* _ = Client.normalize_data client ~hooks ?mode ~data ~typ in
       unit
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

let test_normalize_stack =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test Michelson stack normalization"
    ~tags:["client"; "normalize"]
    ~supports:(From_protocol 17)
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let stack_elt ty v = sf "Stack_elt %s %s" ty v in
  let elt1 = stack_elt "(pair nat nat nat nat)" "(Pair 0 3 6 9)" in
  let elt2 =
    stack_elt
      "(pair nat (pair nat (pair nat nat)))"
      "(Pair 1 (Pair 4 (Pair 7 10)))"
  in
  let elt3 = stack_elt "(pair nat nat (pair nat nat))" "{2; 5; 8; 11}" in
  let* () =
    modes
    |> Lwt_list.iter_s @@ fun mode ->
       [[]; [elt1]; [elt1; elt2]; [elt1; elt2; elt3]]
       |> Lwt_list.iter_s @@ fun stack ->
          let print_stack fmt =
            Format.fprintf
              fmt
              "{%a}"
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
                 Format.pp_print_string)
          in
          let stack = Format.asprintf "%a" print_stack stack in
          let* _ = Client.normalize_stack client ~hooks ?mode ~stack in
          unit
  in
  let* () =
    modes
    |> Lwt_list.iter_s @@ fun mode ->
       [
         "";
         "{";
         "0";
         "{Stack_elt}";
         "{Stack_elt nat}";
         "{Stack_elt 0 nat}";
         "{Stack_elt nat 0 1}";
         "Stack_elt nat 0";
         "{Stack_elt nat 0; Stack_elt}";
       ]
       |> Lwt_list.iter_s @@ fun stack ->
          Client.spawn_normalize_stack client ~hooks ?mode ~stack
          |> Process.check_error
  in
  unit

let test_normalize_script =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test normalize script"
    ~tags:["client"; "normalize"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script =
    Michelson_script.(find ["opcodes"; "comb-literals"] protocol |> path)
  in
  let* () =
    modes
    |> Lwt_list.iter_s @@ fun mode ->
       let* _ = Client.normalize_script client ~hooks ?mode ~script in
       unit
  in
  unit

let test_normalize_type =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test normalize type"
    ~tags:["client"; "normalize"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () =
    [
      "nat";
      "list nat";
      "pair nat int";
      "list (pair nat int)";
      "pair nat int bool";
      "list (pair nat int bool)";
      "pair nat int bool bytes";
      "list (pair nat int bool bytes)";
    ]
    |> Lwt_list.iter_s @@ fun typ ->
       let* _ = Client.normalize_type client ~hooks ~typ in
       unit
  in
  unit

let register ~protocols =
  test_normalize_unparsing_mode protocols ;
  test_normalize_legacy_flag protocols ;
  test_normalize_stack protocols ;
  test_normalize_script protocols ;
  test_normalize_type protocols
