(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
    Component:  Protocol (Michelson block-time instructions)
    Invocation: dune exec src/proto_024_PtTALLiN/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_block_time_instructions.ml
    Subject:    This module tests that Michelson instructions related to block time are correct.
*)

open Tezos_protocol_024_PtTALLiN_parameters
open Protocol
open Alpha_context

let context_with_constants constants =
  let open Lwt_result_syntax in
  let* block, _contracts = Context.init_with_constants1 constants in
  let+ incremental = Incremental.begin_construction block in
  Incremental.alpha_ctxt incremental

let test_min_block_time () =
  let open Lwt_result_syntax in
  let* context = context_with_constants Default_parameters.constants_mainnet in
  let* result, _ =
    Contract_helpers.run_script
      context
      ~storage:"0"
      ~parameter:"Unit"
      {| { parameter unit; storage nat; code { DROP; MIN_BLOCK_TIME; NIL operation; PAIR } } |}
      ()
  in

  let expected_value =
    Default_parameters.constants_mainnet.minimal_block_delay
    |> Period.to_seconds |> Z.of_int64
  in

  match Micheline.root result.storage with
  | Int (_, result_storage) when Z.equal result_storage expected_value ->
      return_unit
  | _ ->
      failwith
        "Expected storage to be %a, but got %a"
        Z.pp_print
        expected_value
        Micheline_printer.print_expr
        (Micheline_printer.printable
           Michelson_v1_primitives.string_of_prim
           result.storage)

let tests =
  [
    Tztest.tztest
      "MIN_BLOCK_TIME gives current minimal block delay"
      `Quick
      test_min_block_time;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("block time instructions", tests)]
  |> Lwt_main.run
