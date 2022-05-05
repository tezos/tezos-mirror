(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Shell (Plugin)
    Invocation:   dune exec src/proto_alpha/lib_plugin/test/test_plugin.exe
    Subject:      Unit tests the plugin
*)

open Plugin.Mempool
open Test_utils

let count = Some 1000

(** This test checks that the filter state is cleared (prechecked related
    values are reseted) [on_flush] whether a [validation_state] is given or not
 *)
let test_flush () =
  let l =
    QCheck2.Gen.generate
      ~n:(Option.value ~default:1 count)
      (QCheck2.Gen.pair
         Generators.filter_state_with_operation_gen
         QCheck2.Gen.bool)
  in
  List.iter_es
    (fun ( (filter_state, ((op_info : manager_op_info), pkh)),
           with_validation_state ) ->
      let filter_state =
        add_manager_restriction filter_state op_info.operation_hash op_info pkh
      in
      Context.init 1 >>= function
      | Error e ->
          failwith "Error at context initialisation: %a" pp_print_trace e
      | Ok (b, _contracts) -> (
          let predecessor =
            Block_header.
              {shell = b.header.shell; protocol_data = Bytes.make 10 ' '}
          in
          (if with_validation_state then
           Incremental.begin_construction b >>= function
           | Error e ->
               failwith
                 "Error at begin_construction of the incremental state: %a"
                 pp_print_trace
                 e
           | Ok inc -> return_some (Incremental.validation_state inc)
          else return_none)
          >>=? fun validation_state ->
          on_flush default_config ?validation_state filter_state ~predecessor ()
          >>= function
          | Error e -> failwith "Error during on_flush: %a" pp_print_trace e
          | Ok flushed_state ->
              if eq_state flushed_state empty then return_unit
              else
                failwith
                  "Flushed state is not empty : %a"
                  pp_state
                  flushed_state))
    l

let () =
  Alcotest_lwt.run
    "Plugin"
    [
      ( "on_flush",
        [
          Tztest.tztest
            "[on_flush ~validation_state ...] yields an empty state "
            `Quick
            test_flush;
        ] );
    ]
  |> Lwt_main.run
