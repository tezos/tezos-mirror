(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech  <contact@trili.tech>                        *)
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
    Component:    Lib_scoru_wasm durable
    Invocation:   dune exec src/lib_scoru_wasm/test/test_scoru_wasm.exe \
                    -- test "Durable storage"
    Subject:      Durable storage tests for the tezos-scoru-wasm library
*)

open Tztest
open Lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
include Test_encodings_util
module Wasm = Wasm_pvm.Make (Tree)
module Wrapped_tree_runner = Tree_encoding.Runner.Make (Tree_encoding.Wrapped)

let wrap_as_durable_storage tree =
  let open Lwt.Syntax in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.value ["durable"; "_keep_me"] Data_encoding.bool)
      true
      tree
  in
  let+ tree =
    Tree_encoding_runner.decode
      (Tree_encoding.scope ["durable"] Tree_encoding.wrapped_tree)
      tree
  in
  Tezos_webassembly_interpreter.Durable_storage.of_tree
  @@ Tree_encoding.Wrapped.wrap tree

(* Test checking that if [key] is missing, [store_has key] returns [false] *)
let test_store_has_missing_key () =
  let open Lwt.Syntax in
  let* tree = empty_tree () in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let src = 10l in
  let key = "/test/path" in
  let _ = Memory.store_bytes memory src key in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;
  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_has
      values
  in
  assert (result = Values.[Num (I32 (I32.of_int_s 0))]) ;
  Lwt.return @@ Result.return_unit

let assert_invalid_key run =
  let open Lwt_syntax in
  Lwt.catch
    (fun () ->
      let+ _ = run () in
      assert false)
    (fun caught_exn ->
      match caught_exn with
      | Durable.Invalid_key _ -> Lwt.return_ok ()
      | Host_funcs.Key_too_large _ -> Lwt.return_ok ()
      | x -> raise x)

(* Test checking that if [key] is too large, [store_has key] traps. *)
let test_store_has_key_too_long () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let src = 10l in
  (* Together with the '/durable' prefix, and '/_' this key is too long *)
  let key = List.repeat 240 "a" |> String.concat "" |> ( ^ ) "/" in
  let _ = Memory.store_bytes memory src key in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let* _ =
    assert_invalid_key (fun () ->
        Eval.invoke
          ~module_reg
          ~caller:module_key
          ~durable
          host_funcs_registry
          Host_funcs.Internal_for_tests.store_has
          values)
  in
  (* We can tell [store_has] that [key] is one byte less long, which makes it valid *)
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ (String.length key - 1)))]
  in
  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_has
      values
  in
  assert (result = Values.[Num (I32 (I32.of_int_s 0))]) ;
  Lwt.return_ok ()

(* Test checking that [store_list_size key] returns the number of immediate
   subtrees. *)
let test_store_list_size () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value () = Chunked_byte_vector.of_string "a very long value" in
  (*
  Store the following tree:

    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/short/path/two/_ = "..."

  We expect that the list size of "/a/short/path" is 2.

  Note that the value of "/durable/a/short/path/_" is not included in the listing.
  *)
  let key = "/a/short/path" in
  let key_steps = ["a"; "short"; "path"] in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["one"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["two"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let src = 20l in
  let _ = Memory.store_bytes memory src key in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_list_size
      values
  in
  assert (result = Values.[Num (I64 (I64.of_int_s 2))]) ;
  Lwt.return_ok ()

(* Test checking that [store_delete key] deletes the subtree at [key] from the
   durable storage. *)
let test_store_delete () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value () = Chunked_byte_vector.of_string "a very long value" in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/long/path/_ = "..."

  We expect that deleting "/a/short/path" is leaves only "/durable/a/long/path".
  *)
  let key = "/a/short/path" in
  let key_steps = ["a"; "short"; "path"] in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["one"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ["durable"; "a"; "long"; "path"; "_"]
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let src = 20l in
  let _ = Memory.store_bytes memory src key in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_delete
      values
  in
  assert (result = []) ;
  let durable = Durable.of_storage_exn durable in
  let* value_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn key
  in
  assert (Option.is_none value_opt) ;
  let* count =
    Durable.count_subtrees durable @@ Durable.key_of_string_exn key
  in
  assert (count = 0) ;
  let* value_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn "/a/long/path"
  in
  assert (Option.is_some value_opt) ;
  Lwt.return_ok ()

(* Test checking that if [key] has value/subtree, [store_has key] returns
   the correct enum value. *)
let test_store_has_existing_key () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value () = Chunked_byte_vector.of_string "a very long value" in
  let key_steps =
    [
      "thequickbrownfoxjumpedoverthelazydog";
      "THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOG";
      "0123456789";
      "key.containing.all.valid.chars";
    ]
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["one"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["two"; "three"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let src = 20l in
  let key = "/" ^ String.concat "/" key_steps in
  let _ = Memory.store_bytes memory src key in
  let src_one = 1000l in
  let key_one = key ^ "/one" in
  let _ = Memory.store_bytes memory src_one key_one in
  let src_two = 2000l in
  let key_two = key ^ "/two" in
  let _ = Memory.store_bytes memory src_two key_two in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;

  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;

  let check src key expected =
    let values =
      Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
    in
    let+ _, result =
      Eval.invoke
        ~module_reg
        ~caller:module_key
        ~durable
        host_funcs_registry
        Host_funcs.Internal_for_tests.store_has
        values
    in
    assert (result = Values.[Num (I32 (I32.of_int_s expected))])
  in
  (* key has a value, and subtrees at 'one' & 'two' *)
  let* _ = check src key 3 in
  (* key/one has a value, and no subtrees. *)
  let* _ = check src_one key_one 1 in
  (* key/two has no value, and a subtree at key/two/three. *)
  let* _ = check src_two key_two 2 in
  Lwt.return_ok ()

(* Test that find_value/find_value_exn correctly decode the
   chunked_byte_vector *)
let test_durable_find_value () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value = Chunked_byte_vector.of_string "a very long value" in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ["durable"; "hello"; "value"; "_"]
         Tree_encoding.chunked_byte_vector)
      value
      tree
  in
  let* tree = wrap_as_durable_storage tree in
  let durable = Durable.of_storage_exn tree in
  let* r =
    Durable.find_value durable @@ Durable.key_of_string_exn "/hello/value"
  in
  assert (Option.is_some r) ;
  let* x =
    match r with
    | Some y -> Chunked_byte_vector.to_string y
    | None -> assert false
  in
  assert (x = "a very long value") ;
  let* v =
    Durable.find_value_exn durable @@ Durable.key_of_string_exn "/hello/value"
  in
  let* x = Chunked_byte_vector.to_string v in
  assert (x = "a very long value") ;
  let* r =
    Durable.find_value durable @@ Durable.key_of_string_exn "/hello/other"
  in
  assert (Option.is_none r) ;
  Lwt.return_ok ()

let test_durable_count_subtrees () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let assert_subtree_count t count under =
    let* t = wrap_as_durable_storage t in
    let dbl = Durable.of_storage_exn t in
    let+ n = Durable.count_subtrees dbl @@ Durable.key_of_string_exn under in
    assert (n = count)
  in
  let add_value t at =
    let value = Chunked_byte_vector.of_string "a very long value" in
    Tree_encoding_runner.encode
      (Tree_encoding.scope at Tree_encoding.chunked_byte_vector)
      value
      t
  in
  let* () = assert_subtree_count tree 0 "/hello" in
  let* tree = add_value tree ["durable"; "hello"; "world"; "_"] in
  let* () = assert_subtree_count tree 1 "/hello" in
  let* tree = add_value tree ["durable"; "hello"; "you"; "_"] in
  let* () = assert_subtree_count tree 2 "/hello" in
  let* tree = add_value tree ["durable"; "hello"; "you"; "too"; "_"] in
  let* () = assert_subtree_count tree 2 "/hello" in
  let* () = assert_subtree_count tree 1 "/hello/you" in
  let* () = assert_subtree_count tree 0 "/hello/you/too" in
  let* () = assert_subtree_count tree 0 "/bye" in
  Lwt.return_ok ()

(* Test checking that [store_copy key] deletes the subtree at [key] from the
   durable storage. *)
let test_store_copy () =
  let open Lwt_syntax in
  let* tree = empty_tree () in
  let value () = Chunked_byte_vector.of_string "a very long value" in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/long/path/_ = "..."

  We expect that deleting "/a/short/path" is leaves only "/durable/a/long/path".
  *)
  let _key = "/a/short/path" in
  let key_steps = ["a"; "short"; "path"] in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ("durable" :: List.append key_steps ["one"; "_"])
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* tree =
    Tree_encoding_runner.encode
      (Tree_encoding.scope
         ["durable"; "a"; "long"; "path"; "_"]
         Tree_encoding.chunked_byte_vector)
      (value ())
      tree
  in
  let* durable = wrap_as_durable_storage tree in
  let module_inst = Tezos_webassembly_interpreter.Instance.empty_module_inst in
  let memory = Memory.alloc (MemoryType Types.{min = 20l; max = Some 3600l}) in
  let from_key = "/a/short/path/one" in
  let to_key = "/a/long/path/one" in
  let from_offset = 20l in
  let to_offset = 40l in
  let _ = Memory.store_bytes memory from_offset from_key in
  let _ = Memory.store_bytes memory to_offset to_key in
  let memories = Lazy_vector.Int32Vector.cons memory module_inst.memories in
  let module_inst = {module_inst with memories} in
  let host_funcs_registry = Tezos_webassembly_interpreter.Host_funcs.empty () in
  Host_funcs.register_host_funcs host_funcs_registry ;
  let module_reg = Instance.ModuleMap.create () in
  let module_key = Instance.Module_key "test" in
  Instance.update_module_ref module_reg module_key module_inst ;
  let values =
    Values.
      [
        Num (I32 from_offset);
        Num (I32 (Int32.of_int @@ String.length from_key));
        Num (I32 to_offset);
        Num (I32 (Int32.of_int @@ String.length to_key));
      ]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_copy
      values
  in
  assert (result = []) ;
  let durable = Durable.of_storage_exn durable in
  let* from_tree_opt =
    Durable.find_tree durable @@ Durable.key_of_string_exn to_key
  in
  let* to_tree_opt =
    Durable.find_tree durable @@ Durable.key_of_string_exn to_key
  in
  assert (from_tree_opt = to_tree_opt) ;
  Lwt.return_ok ()

(* Test invalid key encodings are rejected. *)
let test_durable_invalid_keys () =
  let open Lwt.Syntax in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "//hello")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "hello")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/hello//world")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/invalid_/key")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/!\"?I")
  in
  Lwt.return_ok ()

let tests =
  [
    tztest "store_has missing key" `Quick test_store_has_missing_key;
    tztest "store_has existing key" `Quick test_store_has_existing_key;
    tztest "store_has key too long key" `Quick test_store_has_key_too_long;
    tztest "store_list_size counts subtrees" `Quick test_store_list_size;
    tztest "store_delete removes subtree" `Quick test_store_delete;
    tztest "store_copy" `Quick test_store_copy;
    tztest "Durable: find value" `Quick test_durable_find_value;
    tztest "Durable: count subtrees" `Quick test_durable_count_subtrees;
    tztest "Durable: invalid keys" `Quick test_durable_invalid_keys;
  ]
