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
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^annotations$"
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

(* Checks that [contract_with_annotations] once originated is stored as is. *)
let test_external_origination () =
  Context.init ~consensus_threshold:0 1 >>=? fun (b, contracts) ->
  Incremental.begin_construction b >>=? fun inc ->
  let source = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  let code = Expr.toplevel_from_string contract_with_annotations in
  let script = Script.{code = lazy_expr code; storage = lazy_none} in
  Op.contract_origination (I inc) source ~script >>=? fun (operation, addr) ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Context.Contract.script (I inc) addr >>=? fun stored_script ->
  Assert.equal_string
    ~loc:__LOC__
    contract_with_annotations
    (Expr.to_string stored_script)

(* Checks that [contract_with_annotations] originated from
   [contract_factory_with_annotations] is stored as is. *)
let test_internal_origination () =
  Context.init ~consensus_threshold:0 1 >>=? fun (b, contracts) ->
  Incremental.begin_construction b >>=? fun inc ->
  let source = WithExceptions.Option.get ~loc:__LOC__ @@ List.hd contracts in
  let code = Expr.toplevel_from_string contract_factory_with_annotations in
  let script = Script.{code = lazy_expr code; storage = lazy_none} in
  Op.contract_origination (I inc) source ~script
  >>=? fun (operation, factory) ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Op.transaction (I inc) source factory ~parameters:lazy_none Tez.zero
  >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Context.Contract.storage (I inc) factory >>=? fun factory_storage ->
  let ctxt = Incremental.alpha_ctxt inc in
  Environment.wrap_tzresult Script_typed_ir.(option_t 0 address_t)
  >>?= fun option_address_t ->
  Script_ir_translator.parse_data
    ctxt
    ~legacy:false
    ~allow_forged:false
    option_address_t
    (Micheline.root factory_storage)
  >>= fun res ->
  Environment.wrap_tzresult res >>?= fun (factory_storage, _ctxt) ->
  match factory_storage with
  | Some {entrypoint; _} when not (Entrypoint.is_default entrypoint) ->
      failwith "Did not expect non-default entrypoint"
  | Some {destination = Tx_rollup _; _} ->
      failwith "Did not expect non-contract address"
  | Some {destination = Contract addr; entrypoint = _it_is_default} ->
      Context.Contract.script (I inc) addr >>=? fun stored_script ->
      Assert.equal_string
        ~loc:__LOC__
        contract_with_annotations
        (Expr.to_string stored_script)
  | _ ->
      failwith
        "The factory contract should have stored the address of the originated \
         contract"

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
