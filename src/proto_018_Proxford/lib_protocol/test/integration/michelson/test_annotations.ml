(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Component:  Protocol (Michelson annotations)
    Invocation: dune exec src/proto_018_Proxford/lib_protocol/test/integration/michelson/main.exe \
                  -- --file test_annotations.ml
    Subject:    This module tests that Michelson annotations are properly handled.
*)

open Protocol
open Alpha_context

let type_with_annotations =
  "(option :a (or :b (pair %c :d (int %e :f) (nat :g %h)) (bool %i :j)))"

let contract_with_annotations =
  Printf.sprintf
    "{ parameter %s ;\n  storage %s ;\n  code { FAILWITH } }"
    type_with_annotations
    type_with_annotations

let contract_factory_with_annotations =
  Printf.sprintf
    "{ parameter %s ;\n\
    \  storage (option address) ;\n\
    \  code { CAR ;\n\
    \         AMOUNT ;\n\
    \         NONE key_hash ;\n\
    \         CREATE_CONTRACT %s ;\n\
    \         DIP { SOME ;\n\
    \               NIL operation } ;\n\
    \         CONS ;\n\
    \         PAIR } }"
    type_with_annotations
    contract_with_annotations

let lazy_none = Script.lazy_expr (Expr.from_string "None")

let init_and_originate contract_code_string =
  let open Lwt_result_syntax in
  let* b, source = Context.init1 ~consensus_threshold:0 () in
  let* inc = Incremental.begin_construction b in
  let code = Expr.toplevel_from_string contract_code_string in
  let script = Script.{code = lazy_expr code; storage = lazy_none} in
  let* operation, addr = Op.contract_origination_hash (I inc) source ~script in
  let+ inc = Incremental.add_operation inc operation in
  (inc, source, addr)

let assert_stored_script_equal inc addr expected_code_string =
  let open Lwt_result_syntax in
  let* stored_script = Context.Contract.script (I inc) addr in
  Assert.equal_string
    ~loc:__LOC__
    expected_code_string
    (Expr.to_string stored_script)

let get_address_from_storage inc factory_addr =
  let open Lwt_result_wrap_syntax in
  let* factory_storage = Context.Contract.storage (I inc) factory_addr in
  let ctxt = Incremental.alpha_ctxt inc in
  let*?@ option_address_t = Script_typed_ir.(option_t 0 address_t) in
  let*! res =
    Script_ir_translator.parse_data
      ctxt
      ~elab_conf:(Script_ir_translator_config.make ~legacy:false ())
      ~allow_forged:false
      option_address_t
      (Micheline.root factory_storage)
  in
  let*?@ factory_storage, _ctxt = res in
  match factory_storage with
  | Some {entrypoint; _} when not (Entrypoint.is_default entrypoint) ->
      failwith "Did not expect non-default entrypoint"
  | Some {destination = Contract (Implicit _); _} ->
      failwith "Did not expect implict account"
  | Some {destination = Contract (Originated addr); entrypoint = _it_is_default}
    ->
      return addr
  | _ ->
      failwith
        "The factory contract should have stored the address of the originated \
         contract"

(* Checks that [contract_with_annotations] once originated is stored as is. *)
let test_external_origination () =
  let open Lwt_result_syntax in
  let* inc, _source, addr = init_and_originate contract_with_annotations in
  assert_stored_script_equal inc addr contract_with_annotations

(* Checks that [contract_with_annotations] originated from
   [contract_factory_with_annotations] is stored as is. *)
let test_internal_origination () =
  let open Lwt_result_syntax in
  let* inc, source, factory =
    init_and_originate contract_factory_with_annotations
  in
  let* operation =
    Op.transaction
      (I inc)
      source
      (Contract.Originated factory)
      ~parameters:lazy_none
      Tez.zero
  in
  let* b = Incremental.finalize_block inc in
  let* inc = Incremental.begin_construction b in
  let* inc = Incremental.add_operation inc operation in
  let* addr = get_address_from_storage inc factory in
  assert_stored_script_equal inc addr contract_with_annotations

let tests =
  [
    Tztest.tztest
      "External origination preserves annotations"
      `Quick
      test_external_origination;
    Tztest.tztest
      "Internal origination preserves annotations"
      `Quick
      test_internal_origination;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("annotations", tests)]
  |> Lwt_main.run
