(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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
    Component:    Tree_encoding_decoding
    Invocation:   dune exec  src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "WASM Encodings"
    Subject:      Encoding tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_scoru_wasm

let qcheck ?count ?print gen f =
  let open Lwt_result_syntax in
  let test =
    QCheck2.Test.make ?count ?print gen (fun x ->
        Result.is_ok @@ Lwt_main.run (f x))
  in
  let res = QCheck_base_runner.run_tests ~verbose:true [test] in
  if res = 0 then return_unit else failwith "QCheck tests failed"

(* Use context-binary for testing. *)
module Context = Tezos_context_memory.Context_binary

module Tree = struct
  type t = Context.t

  type tree = Context.tree

  type key = Context.key

  type value = Context.value

  include Context.Tree
end

module Merklizer = Tree_encoding_decoding.Make (Tree)
module Wasm_encoding = Wasm_encoding.Make (Merklizer)

let empty_tree () =
  let open Lwt_syntax in
  let* index = Context.init "/tmp" in
  let empty_store = Context.empty index in
  return @@ Context.Tree.empty empty_store

let encode_decode enc value =
  let open Lwt_syntax in
  let* empty_tree = empty_tree () in
  let* tree = Merklizer.encode enc value empty_tree in
  Merklizer.decode enc tree

(** Test serialize/deserialize instructions. *)
let test_instr_roundtrip () =
  let open Lwt_result_syntax in
  qcheck Ast_generators.instr_gen (fun instr ->
      let*! instr' = encode_decode Wasm_encoding.instruction_encoding instr in
      assert (instr = instr') ;
      return_unit)

let assert_string_equal s1 s2 =
  let open Lwt_result_syntax in
  if String.equal s1 s2 then return_unit else failwith "Not equal"

(** Test serialize/deserialize modules. *)
let test_module_roundtrip () =
  let print = Format.asprintf "%a" Ast_printer.pp_module in
  let open Lwt_result_syntax in
  qcheck ~print (Ast_generators.module_gen ()) (fun module1 ->
      (* We need to print here in order to force lazy bindings to be evaluated. *)
      let module1_str = print module1 in
      let*! module2 =
        encode_decode Wasm_encoding.module_instance_encoding module1
      in
      let module2_str = print module2 in
      let*! module3 =
        encode_decode Wasm_encoding.module_instance_encoding module2
      in
      let module3_str = print module3 in
      (* Check that modules match. *)
      let* () = assert_string_equal module1_str module2_str in
      assert_string_equal module2_str module3_str)

(** Test serialize/deserialize modules and compare trees. *)
let test_module_tree () =
  let print = Format.asprintf "%a" Ast_printer.pp_module in
  let open Lwt_result_syntax in
  qcheck ~print (Ast_generators.module_gen ()) (fun module1 ->
      let*! empty_tree = empty_tree () in
      (* We need to print here in order to force lazy bindings to be evaluated. *)
      let _ = print module1 in
      let*! tree1 =
        Merklizer.encode
          Wasm_encoding.module_instance_encoding
          module1
          empty_tree
      in
      let*! module2 =
        Merklizer.decode Wasm_encoding.module_instance_encoding tree1
      in
      (* We need to print here in order to force lazy bindings to be evaluated. *)
      let _ = print module2 in
      let*! tree2 =
        Merklizer.encode
          Wasm_encoding.module_instance_encoding
          module2
          empty_tree
      in
      assert (Tree.equal tree1 tree2) ;
      return_unit)

let tests =
  [
    tztest "Instruction roundtrip" `Quick test_instr_roundtrip;
    tztest "Module roundtrip" `Quick test_module_roundtrip;
    tztest "Module trees" `Quick test_module_tree;
  ]
