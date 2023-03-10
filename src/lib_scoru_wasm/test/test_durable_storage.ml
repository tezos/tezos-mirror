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
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_durable_storage.ml
    Subject:      Durable storage tests for the tezos-scoru-wasm library
*)

open Tztest
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
open Wasm_utils

let value_store_key_too_large =
  Values.(Num (I32 Host_funcs.Error.(code Store_key_too_large)))

let equal_chunks c1 c2 =
  let open Lwt.Syntax in
  let* c1 = Chunked_byte_vector.to_string c1 in
  let* c2 = Chunked_byte_vector.to_string c2 in
  Lwt.return @@ assert (String.equal c1 c2)

(* Test checking that if [key] is missing, [store_has key] returns [false] *)
let test_store_has_missing_key ~version () =
  let open Lwt.Syntax in
  let* durable = make_durable [] in
  let key = "/test/path" in
  let src = 10l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
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
let test_store_has_key_too_long ~version () =
  let open Lwt_syntax in
  let* durable = make_durable [] in
  (* Together with the '/durable' prefix, and '/_' this key is too long *)
  let src = 10l in
  let key = List.repeat 240 "a" |> String.concat "" |> ( ^ ) "/" in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] 10l
  in
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let* _, res =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_has
      values
  in
  assert (res = [value_store_key_too_large]) ;
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
let test_store_list_size ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:

    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/short/path/two/_ = "..."

  We expect that the list size of "/a/short/path" is 2.

  Note that the value of "/durable/a/short/path/_" is included in the listing.
  *)
  let* durable =
    make_durable
      [
        ("/a/short/path", "true");
        ("/a/short/path/one", "true");
        ("/a/short/path/two", "true");
      ]
  in
  let key = "/a/short/path" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in
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
  assert (result = Values.[Num (I64 (I64.of_int_s 3))]) ;
  Lwt.return_ok ()

(* Test checking that [store_get_nth_key key index dst max_size] returns the size
   of the name of the immediate subtree at [index]. *)
let test_store_get_nth_key ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:

    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/short/path/three/_ = "..."

  We expect that the  result at "/a/short/path/one" is 3 and at
  /durable/a/short/path/three 5. We also expect the truncated at 3
  result at /durable/a/short/path/three to be 3
  *)
  let* durable =
    make_durable
      [
        ("/a/short/path", "true");
        ("/a/short/path/one", "true");
        ("/a/short/path/three", "true");
      ]
  in
  let key = "/a/short/path" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in
  let key_length = Int32.of_int @@ String.length key in
  let dst_zero = 20l in
  let expected_string_at_zero = "" in
  let dst_one = 70l in
  let expected_string_at_one = "one" in
  let dst_two = 80l in
  let expected_string_at_two = "three" in
  let truncated_dst_two = 100l in
  let expected_truncated_string_at_two = "thr" in

  let wrong_value =
    Values.
      [
        Num (I32 0l);
        Num (I32 2l);
        Num (I64 0L);
        Num (I32 dst_zero);
        Num (I32 3600l);
      ]
  in
  let _ =
    try
      let _ =
        Eval.invoke
          ~module_reg
          ~caller:module_key
          ~durable
          host_funcs_registry
          Host_funcs.Internal_for_tests.store_get_nth_key
          wrong_value
      in
      ()
    with e -> ( match e with Not_found -> () | _ -> assert false)
  in
  let value_at_zero =
    Values.
      [
        Num (I32 src);
        Num (I32 key_length);
        Num (I64 0L);
        Num (I32 dst_zero);
        Num (I32 3600l);
      ]
  in
  let* _, result_at_zero =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_get_nth_key
      value_at_zero
  in
  let value_at_one =
    Values.
      [
        Num (I32 src);
        Num (I32 key_length);
        Num (I64 1L);
        Num (I32 dst_one);
        Num (I32 3600l);
      ]
  in
  let* _, result_at_one =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_get_nth_key
      value_at_one
  in
  let value_at_two =
    Values.
      [
        Num (I32 src);
        Num (I32 key_length);
        Num (I64 2L);
        Num (I32 dst_two);
        Num (I32 3600l);
      ]
  in
  let* _, result_at_two =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_get_nth_key
      value_at_two
  in
  let truncated_value_at_two =
    Values.
      [
        Num (I32 src);
        Num (I32 key_length);
        Num (I64 2L);
        Num (I32 truncated_dst_two);
        Num (I32 3l);
      ]
  in
  let* _, truncated_result_at_two =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_get_nth_key
      truncated_value_at_two
  in
  let* memory = retrieve_memory module_reg in
  let* string_at_zero = Memory.load_bytes memory dst_zero 0 in
  let* string_at_one = Memory.load_bytes memory dst_one 3 in
  let* string_at_two = Memory.load_bytes memory dst_two 5 in
  let* truncated_string_at_two = Memory.load_bytes memory truncated_dst_two 3 in
  assert (result_at_zero = Values.[Num (I32 (I32.of_int_s 0))]) ;
  assert (string_at_zero = expected_string_at_zero) ;
  assert (result_at_one = Values.[Num (I32 (I32.of_int_s 3))]) ;
  assert (string_at_one = expected_string_at_one) ;
  assert (result_at_two = Values.[Num (I32 (I32.of_int_s 5))]) ;
  assert (string_at_two = expected_string_at_two) ;
  assert (truncated_result_at_two = Values.[Num (I32 (I32.of_int_s 3))]) ;
  assert (truncated_string_at_two = expected_truncated_string_at_two) ;
  Lwt.return_ok ()

let test_v1_and_above ~version test =
  match version with
  | Wasm_pvm_state.V0 ->
      (* the host function is not available before [V1]. *)
      Lwt.return_ok ()
  | V1 -> test ~version

let test_store_get_hash ~version =
  let open Lwt_syntax in
  let* durable_storage = make_durable [("/foo/bar", "/foobar")] in
  let src = 20l in
  let key = "/foo" in
  let key_len = Int32.of_int @@ String.length key in
  let dst = Int32.(add src key_len) in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in
  let call_host_func values =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable:durable_storage
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_get_hash
      values
  in
  (* Test valid access *)
  let values =
    Values.[Num (I32 src); Num (I32 key_len); Num (I32 dst); Num (I32 32l)]
  in
  let* _durable, _result = call_host_func values in
  let* mem = retrieve_memory module_reg in
  let* hash = Memory.load_bytes mem dst 32 in
  let durable = Durable.of_storage_exn durable_storage in
  let* hash_expected =
    Durable.(hash_exn ~kind:Directory durable (key_of_string_exn key))
  in
  let hash_expected = Context_hash.to_string hash_expected in
  assert (hash = hash_expected) ;
  (* Test wrong access for keys *)
  let values =
    Values.
      [
        (* We allocate 20 pages of 64KiB in [make_module_inst], so this
           value is out of the bound of the memory. *)
        Num (I32 Int32.(mul 21l 0x10000l));
        Num (I32 key_len);
        Num (I32 dst);
        Num (I32 32l);
      ]
  in
  let* _durable, result = call_host_func values in
  assert (
    result = Values.[Num (I32 (Host_funcs.Error.code Memory_invalid_access))]) ;
  (* Test wrong access for result *)
  let values =
    Values.
      [
        Num (I32 20l);
        Num (I32 key_len);
        (* We allocate 20 pages of 64KiB in [make_module_inst], so this
           value is out of the bound of the memory. *)
        Num (I32 Int32.(mul 21l 0x10000l));
        Num (I32 32l);
      ]
  in
  let* _durable, result = call_host_func values in
  assert (
    result = Values.[Num (I32 (Host_funcs.Error.code Memory_invalid_access))]) ;
  Lwt.return_ok ()

let test_store_get_hash ~version () =
  test_v1_and_above ~version test_store_get_hash

let test_store_delete_generic ~kind ~version =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/long/path/_ = "..."

  We expect that deleting "/a/short/path" leaves only "/durable/a/long/path"
  if [kind = Directory], or both "/durable/a/long/path" and "/a/short/path/one"'s
  value if [kind = Value] *)
  let* durable =
    make_durable
      [
        ("/a/short/path", "true");
        ("/a/short/path/one", "true");
        ("/a/long/path", "true");
      ]
  in
  let key = "/a/short/path" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in
  let store_delete =
    match kind with
    | Durable.Directory -> Host_funcs.Internal_for_tests.store_delete
    | Value -> Host_funcs.Internal_for_tests.store_delete_value
  in
  let values =
    Values.[Num (I32 src); Num (I32 (Int32.of_int @@ String.length key))]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      store_delete
      values
  in
  assert (result = [Values.(Num (I32 0l))]) ;
  let durable = Durable.of_storage_exn durable in
  let* value_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn key
  in
  assert (Option.is_none value_opt) ;
  let* count =
    Durable.count_subtrees durable @@ Durable.key_of_string_exn key
  in
  assert (count = if kind = Value then 1 else 0) ;
  let* value_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn "/a/long/path"
  in
  assert (Option.is_some value_opt) ;
  Lwt.return_ok ()

(* Test checking that [store_delete key] deletes the subtree and the value at
   [key] from the durable storage. *)
let test_store_delete_all ~version () =
  test_store_delete_generic ~kind:Directory ~version

(* Test checking that [store_delete_value key] deletes only the value at [key]
   from the durable storage. *)
let test_store_delete_value ~version () =
  test_v1_and_above ~version (test_store_delete_generic ~kind:Value)

(* Test checking that if [key] has value/subtree, [store_has key] returns
   the correct enum value. *)
let test_store_has_existing_key ~version () =
  let open Lwt_syntax in
  let root =
    "/thequickbrownfoxjumpedoverthelazydog/THEQUICKBROWNFOXJUMPEDOVERTHELAZYDOG/0123456789."
  in
  let* durable =
    make_durable
      [(root, "true"); (root ^ "/one", "true"); (root ^ "/two/three", "true")]
  in
  let src = 20l in
  let key = root in
  let src_one = Int32.add src @@ Int32.of_int @@ String.length key in
  let key_one = key ^ "/one" in
  let src_two = Int32.add src_one @@ Int32.of_int @@ String.length key_one in
  let key_two = key ^ "/two" in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key; key_one; key_two] src
  in
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
  let* durable = make_durable [("/hello/value", "a very long value")] in
  let durable = Durable.of_storage_exn durable in
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

let test_durable_count_subtrees_and_list () =
  let open Lwt_syntax in
  let* tree =
    make_durable
      [
        ("/hello", "a very long value");
        ("/hello/world", "a very long value");
        ("/hello/you", "a very long value");
        ("/hello/you/too", "a very long value");
        ("/long/path/to/something", "hello, it's me");
      ]
  in
  let assert_subtree_count t count under =
    let dbl = Durable.of_storage_exn t in
    let+ n = Durable.count_subtrees dbl @@ Durable.key_of_string_exn under in
    Assert.Int.equal
      ~loc:__LOC__
      ~msg:(Format.sprintf "Subtrees count for path %s" under)
      n
      count
  in
  let assert_list t expected under =
    let dbl = Durable.of_storage_exn t in
    let+ subtrees = Durable.list dbl @@ Durable.key_of_string_exn under in
    Assert.equal_list
      ~loc:__LOC__
      ~msg:(Format.sprintf "Lists for path %s unequal" under)
      expected
      subtrees
  in
  let* () = assert_subtree_count tree 3 "/hello" in
  let* () = assert_subtree_count tree 2 "/hello/you" in
  let* () = assert_subtree_count tree 1 "/hello/you/too" in
  let* () = assert_subtree_count tree 0 "/bye" in
  let* () = assert_list tree [""; "hello"; "long"] "" in
  let* () = assert_list tree [""; "world"; "you"] "/hello" in
  let* () = assert_list tree [""; "too"] "/hello/you" in
  let* () = assert_list tree [""] "/hello/you/too" in
  let* () = assert_list tree [] "/hello/you/too/unexisting" in
  let* () = assert_list tree ["path"] "/long" in
  let* () = assert_list tree ["to"] "/long/path" in
  let* () = assert_list tree ["something"] "/long/path/to" in
  let* () = assert_list tree [""] "/long/path/to/something" in
  let* () = assert_list tree [] "/long/path/to/something/unexisting" in
  Lwt.return_ok ()

(* Test checking that [store_copy from_key to_key] copies the subtree at
   [from_key] to [to_key] in the durable storage.  This should overwrite
   the tree that existed previously at [to_key] *)
let test_store_copy ~version () =
  let open Lwt_syntax in
  let value () = Chunked_byte_vector.of_string "a very long value" in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "a very long value"
    /durable/a/short/path/one/_ = "a very long value"
    /durable/a/long/path/two/_ = "a very long value"
  *)
  let* durable =
    make_durable
      [
        ("/a/short/path", "a very long value");
        ("/a/short/path/one", "a very long value");
        ("/a/long/path/two", "a very long value");
        ("/hello/you/too", "a very long value");
      ]
  in
  let from_key = "/a/short/path/one" in
  let to_key = "/a/long/path" in
  let wrong_key = "/a/long/path/two" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [from_key; to_key; wrong_key] src
  in
  let from_offset = src in
  let from_length = Int32.of_int @@ String.length from_key in
  let to_offset = Int32.(add from_offset from_length) in
  let to_length = Int32.of_int @@ String.length to_key in
  let durable_st = Durable.of_storage_exn durable in
  let* old_value_from_key =
    Durable.find_value_exn durable_st @@ Durable.key_of_string_exn from_key
  in
  let* old_value_at_two =
    Durable.find_value_exn durable_st @@ Durable.key_of_string_exn wrong_key
  in
  let* () = equal_chunks old_value_at_two (value ()) in
  let values =
    Values.
      [
        Num (I32 from_offset);
        Num (I32 from_length);
        Num (I32 to_offset);
        Num (I32 to_length);
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
  (* If everything goes well, the function returns `0l`. *)
  assert (result = [Values.(Num (I32 0l))]) ;
  let durable = Durable.of_storage_exn durable in

  let* new_value_at_two =
    Durable.find_value durable @@ Durable.key_of_string_exn wrong_key
  in
  let* new_value_from_key =
    Durable.find_value_exn durable @@ Durable.key_of_string_exn from_key
  in
  let* new_value_to_key =
    Durable.find_value_exn durable @@ Durable.key_of_string_exn to_key
  in
  assert (new_value_at_two = None) ;
  let* () = equal_chunks new_value_from_key new_value_to_key in
  let* () = equal_chunks old_value_from_key new_value_from_key in
  Lwt.return_ok ()

let test_store_copy_missing_path ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/long/path/_ = "..."

  We expect that copying "/a/short/path" to "a/long/path" fails with
  Store_not_a_node
  *)
  let* durable = make_durable [("/a/long/path", "a very long value")] in
  let from_key = "/a/short/path" in
  let to_key = "/a/long/path" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [from_key; to_key] src
  in

  let from_offset = src in
  let from_length = Int32.of_int @@ String.length from_key in
  let to_offset = Int32.(add from_offset from_length) in
  let to_length = Int32.of_int @@ String.length to_key in
  let values =
    Values.
      [
        Num (I32 from_offset);
        Num (I32 from_length);
        Num (I32 to_offset);
        Num (I32 to_length);
      ]
  in
  let* _durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_copy
      values
  in
  assert (result = [Values.(Num (I32 Host_funcs.Error.(code Store_not_a_node)))]) ;
  Lwt.return_ok ()

let test_store_move ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "..."
    /durable/a/long/path/_ = "..."
    /durable/a/long/path/one/_ = "..."

  We expect that moving "/a/short/path" to "a/long/path" is leaves only
   "/durable/a/long/path".
  *)
  let* durable =
    make_durable
      [
        ("/a/short/path", "a very long value");
        ("/a/long/path", "a very long value");
        ("/a/long/path/one", "a very long value");
      ]
  in
  let from_key = "/a/short/path" in
  let to_key = "/a/long/path" in
  let bad_key = "/a/long/path/one" in
  let durable_st = Durable.of_storage_exn durable in

  let* from_tree =
    Durable.find_value_exn durable_st @@ Durable.key_of_string_exn from_key
  in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [from_key; to_key; bad_key] src
  in

  let from_offset = src in
  let from_length = Int32.of_int @@ String.length from_key in
  let to_offset = Int32.(add from_offset from_length) in
  let to_length = Int32.of_int @@ String.length to_key in
  let values =
    Values.
      [
        Num (I32 from_offset);
        Num (I32 from_length);
        Num (I32 to_offset);
        Num (I32 to_length);
      ]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_move
      values
  in
  (* If everything goes well, the function returns `0l`. *)
  assert (result = [Values.(Num (I32 0l))]) ;
  let durable = Durable.of_storage_exn durable in
  let* empty_from_tree_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn from_key
  in
  let* to_tree =
    Durable.find_value_exn durable @@ Durable.key_of_string_exn to_key
  in
  let* empty_bad_key_tree_opt =
    Durable.find_value durable @@ Durable.key_of_string_exn bad_key
  in
  assert (empty_from_tree_opt = None) ;
  assert (empty_bad_key_tree_opt = None) ;
  let* () = equal_chunks from_tree to_tree in
  Lwt.return_ok ()

let test_store_move_missing_path ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/long/path/_ = "..."

  We expect that moving "/a/short/path" to "a/long/path" fails with
  Store_not_a_node
  *)
  let* durable = make_durable [("/a/long/path", "a very long value")] in
  let from_key = "/a/short/path" in
  let to_key = "/a/long/path" in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [from_key; to_key] src
  in

  let from_offset = src in
  let from_length = Int32.of_int @@ String.length from_key in
  let to_offset = Int32.(add from_offset from_length) in
  let to_length = Int32.of_int @@ String.length to_key in
  let values =
    Values.
      [
        Num (I32 from_offset);
        Num (I32 from_length);
        Num (I32 to_offset);
        Num (I32 to_length);
      ]
  in
  let* _durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_move
      values
  in
  assert (result = [Values.(Num (I32 Host_funcs.Error.(code Store_not_a_node)))]) ;
  Lwt.return_ok ()

let test_store_read ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/path/_ = "..."

  We expect that reading from "/a/path" puts contents into memory.
  *)
  let key = "/a/path" in
  let contents = "a value of sorts" in
  let* durable = make_durable [(key, contents)] in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in

  let length = Int32.of_int @@ String.length key in
  let dst = 40l in
  let read_offset = 5 in
  let read_num_bytes = Int32.of_int @@ String.length contents in
  let values =
    Values.
      [
        Num (I32 src);
        Num (I32 length);
        Num (I32 (Int32.of_int read_offset));
        Num (I32 dst);
        Num (I32 read_num_bytes);
      ]
  in
  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_read
      values
  in
  let expected_read_bytes_len = String.length contents - read_offset in
  let expected_read_bytes =
    String.sub contents read_offset expected_read_bytes_len
  in
  assert (result = [Values.Num (I32 (Int32.of_int expected_read_bytes_len))]) ;
  let* memory = retrieve_memory module_reg in
  let* value = Memory.load_bytes memory dst expected_read_bytes_len in
  assert (value = expected_read_bytes) ;
  return_ok_unit

let test_store_read_non_value ~version () =
  let open Lwt_syntax in
  (*
  Empty durable

  We expect that reading from "/a/path/other" returns
  Error.Store_not_a_value
  *)
  let key = "/a/path" in
  let* durable = make_durable [] in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key] src
  in

  let length = Int32.of_int @@ String.length key in
  let dst = 40l in
  let read_offset = 5 in
  let values =
    Values.
      [
        Num (I32 src);
        Num (I32 length);
        Num (I32 (Int32.of_int read_offset));
        Num (I32 dst);
        Num (I32 15l);
      ]
  in
  let* _, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_read
      values
  in
  assert (result = [Values.Num (I32 Host_funcs.Error.(code Store_not_a_value))]) ;
  return_ok_unit

let test_store_value_size ~version () =
  let open Lwt_syntax in
  let to_res x = [Values.(Num (I32 x))] in
  let invoke_store_value_size ~module_reg ~caller ~durable host_funcs_reg values
      =
    let+ _durable, result =
      Eval.invoke
        ~module_reg
        ~caller
        ~durable
        host_funcs_reg
        Host_funcs.Internal_for_tests.store_value_size
        values
    in
    result
  in

  let key = "/a/b/c" in
  let invalid_key = "a/b" in
  let missing_key = "/a/b/d" in
  let contents = "foobar" in
  let contents_len = Int32.of_int (String.length contents) in
  let* durable = make_durable [(key, contents)] in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [key; invalid_key; missing_key; contents] src
  in
  let key_src, key_len = (src, Int32.of_int (String.length key)) in
  let invalid_key_src, invalid_key_len =
    (Int32.add key_src key_len, Int32.of_int (String.length invalid_key))
  in
  let missing_key_src, missing_key_len =
    ( Int32.add invalid_key_src invalid_key_len,
      Int32.of_int (String.length missing_key) )
  in
  let* result =
    invoke_store_value_size
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Values.[Num (I32 key_src); Num (I32 key_len)]
  in
  assert (result = [Values.Num (I32 contents_len)]) ;
  let* result =
    invoke_store_value_size
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Values.[Num (I32 invalid_key_src); Num (I32 invalid_key_len)]
  in
  assert (result = to_res Host_funcs.Error.(code Store_invalid_key)) ;
  let* result =
    invoke_store_value_size
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Values.[Num (I32 missing_key_src); Num (I32 missing_key_len)]
  in
  assert (result = to_res Host_funcs.Error.(code Store_not_a_value)) ;
  Lwt_result_syntax.return_unit

let test_store_write ~version () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/path/_ = "..."

  We expect that writing to "/a/path" should extend the value.
  We expect that writing to "/a/new/path" should create a new value.
  *)
  let existing_key = "/a/path" in
  let new_key = "/a/new/path" in
  let contents = "a value of sorts" in
  let* durable = make_durable [(existing_key, contents)] in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [existing_key; new_key; contents] src
  in
  let existing_key_src = src in
  let new_key_src =
    Int32.add src @@ Int32.of_int @@ String.length existing_key
  in
  let contents_src =
    Int32.add new_key_src @@ Int32.of_int @@ String.length new_key
  in
  let write_offset = 5l in
  let values =
    Values.
      [
        Num (I32 existing_key_src);
        Num (I32 (Int32.of_int @@ String.length existing_key));
        Num (I32 write_offset);
        Num (I32 contents_src);
        Num (I32 (Int32.of_int @@ String.length contents));
      ]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_write
      values
  in
  assert (result = [Values.Num (I32 0l)]) ;
  let tree = Durable.of_storage_exn durable in
  let* value =
    Durable.find_value_exn tree @@ Durable.key_of_string_exn existing_key
  in
  let* result = Chunked_byte_vector.to_string value in
  (* We started writing at an offset into the value. *)
  let expected_write_bytes =
    (String.sub contents 0 @@ Int32.to_int write_offset) ^ contents
  in
  assert (expected_write_bytes = result) ;

  (* Try writing out of bounds *)
  let values =
    Values.
      [
        Num (I32 new_key_src);
        Num (I32 (Int32.of_int @@ String.length new_key));
        Num (I32 1l);
        Num (I32 contents_src);
        Num (I32 (Int32.of_int @@ String.length contents));
      ]
  in
  let* _durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_write
      values
  in
  (match result with
  | [Values.Num (I32 x)] -> Printf.printf "result %s\n" (I32.to_string_s x)
  | _ -> assert false) ;
  assert (
    result = [Values.Num (I32 Host_funcs.Error.(code Store_invalid_access))]) ;
  (* Write a new value *)
  let values =
    Values.
      [
        Num (I32 new_key_src);
        Num (I32 (Int32.of_int @@ String.length new_key));
        Num (I32 0l);
        Num (I32 contents_src);
        Num (I32 (Int32.of_int @@ String.length contents));
      ]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_write
      values
  in
  assert (result = [Values.Num (I32 0l)]) ;
  let tree = Durable.of_storage_exn durable in
  let* value =
    Durable.find_value_exn tree @@ Durable.key_of_string_exn new_key
  in
  let* result = Chunked_byte_vector.to_string value in
  assert (contents = result) ;
  return_ok_unit

let test_store_create ~version =
  let open Lwt_syntax in
  (*
     The durable storage is initialized with the following tree:
      /durable/a/path/@ = "a value of sorts"

     - We expect that creating a value at "/a/path" returns -13
     (Store_value_already_exists), since the value already exist.
     - We expect that creating a value at "/a/new/path" returns 0, and the value
     has been allocated in the durable storage.
     - Finally, creating a value of size > 2GB should fail.
*)

  (* Let's prepare the durable storage and memory *)
  let existing_key = "/a/path" in
  let existing_key_length = String.length existing_key |> Int32.of_int in
  let new_key = "/a/new/path" in
  let new_key_length = String.length new_key |> Int32.of_int in
  let contents = "a value of sorts" in
  let contents_size = String.length contents |> Int32.of_int in
  let* durable = make_durable [(existing_key, contents)] in
  let src = 20l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst ~version [existing_key; new_key] src
  in
  let existing_key_src = src in
  let new_key_src = Int32.add src existing_key_length in

  (* Create a new value *)
  let valid_size =
    Int32.mul (Int64.to_int32 Chunked_byte_vector.Chunk.size) 2l (* == 1 kB *)
  in
  let create_values =
    Values.
      [Num (I32 new_key_src); Num (I32 new_key_length); Num (I32 valid_size)]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_create
      create_values
  in
  assert (result = [Values.Num (I32 0l)]) ;
  let tree = Durable.of_storage_exn durable in
  let* new_value =
    Durable.find_value_exn tree @@ Durable.key_of_string_exn new_key
  in
  let new_value_size_length = Chunked_byte_vector.length new_value in
  assert (Int64.of_int32 valid_size = new_value_size_length) ;
  let expected_value = String.make (Int32.to_int valid_size) '\000' in
  let* value_as_string = Chunked_byte_vector.to_string new_value in
  assert (expected_value = value_as_string) ;

  (* Check that creating an already existing value returns 1 and doesn't
     change its size. *)
  let value_size = Int32.add contents_size 100l in
  let create_existing_key_values =
    Values.
      [
        Num (I32 existing_key_src);
        Num (I32 existing_key_length);
        Num (I32 value_size);
      ]
  in
  let* durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_create
      create_existing_key_values
  in
  assert (
    result
    = [Values.Num (I32 Host_funcs.Error.(code Store_value_already_exists))]) ;
  let tree = Durable.of_storage_exn durable in
  let* value =
    Durable.find_value_exn tree @@ Durable.key_of_string_exn existing_key
  in

  (* Check the vector has been allocated, with a length but no chunks. Note that
     this test relies a lot on the encoding of values and chunked byte
     vectors, and will fail if one or both fails. *)
  let wrapped_tree =
    Tezos_webassembly_interpreter.Durable_storage.to_tree_exn durable
  in
  (* The value is located under the "@" subkey. *)
  let value_key =
    List.append
      (Durable.key_of_string_exn new_key
      |> Durable.Internal_for_tests.key_to_list)
      ["@"]
  in
  let* encoded_value_tree =
    Tezos_tree_encoding.Wrapped.find_tree wrapped_tree value_key
  in
  let encoded_value_tree =
    match encoded_value_tree with
    | None -> Stdlib.failwith "The value has not been encoded"
    | Some tree -> tree
  in
  let* encoded_value = Tezos_tree_encoding.Wrapped.list encoded_value_tree [] in
  assert (
    List.for_all (fun (key, _) -> key = "length") encoded_value
    && encoded_value <> []) ;
  (* Chunks are encoded under the subkey "contents" *)
  let* encoded_chunks =
    Tezos_tree_encoding.Wrapped.list wrapped_tree (value_key @ ["contents"])
  in
  assert (encoded_chunks = []) ;

  (* Check the value will be loaded with zero values. *)
  let value_size_in_durable = Chunked_byte_vector.length value in
  assert (value_size_in_durable = Int64.of_int32 contents_size) ;

  (* Creating a value of an invalid size (> 2GB) should fail *)
  let invalid_size = 0xffffffffl (* == 4GB as unsigned int32 *) in
  let create_with_invalid_size_values =
    Values.
      [
        Num (I32 new_key_src);
        Num (I32 (Int32.of_int @@ String.length new_key));
        Num (I32 invalid_size);
      ]
  in
  let* _durable, result =
    Eval.invoke
      ~module_reg
      ~caller:module_key
      ~durable
      host_funcs_registry
      Host_funcs.Internal_for_tests.store_create
      create_with_invalid_size_values
  in
  assert (
    result
    = [Values.Num (I32 Host_funcs.Error.(code Store_value_size_exceeded))]) ;
  return_ok_unit

let test_store_create ~version () = test_v1_and_above ~version test_store_create

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
        Lwt.return @@ Durable.key_of_string_exn "/invalid@/key")
  in
  let* _ =
    assert_invalid_key (fun () ->
        Lwt.return @@ Durable.key_of_string_exn "/!\"?I")
  in
  let* _ =
    assert_invalid_key (fun () -> Lwt.return @@ Durable.key_of_string_exn "/")
  in
  Lwt.return_ok ()

let test_readonly_key () =
  let is_readonly k =
    Durable.Internal_for_tests.key_is_readonly @@ Durable.key_of_string_exn k
  in
  assert (is_readonly "") ;
  assert (is_readonly "/readonly") ;
  assert (is_readonly "/readonly/hi") ;
  assert (not @@ is_readonly "/hi") ;
  assert (not @@ is_readonly "/readonly.actually.writeable") ;
  Lwt.return_ok ()

let tests =
  Tztest_helper.tztests_with_pvm
    ~versions:[V0; V1]
    [
      ("store_has missing key", `Quick, test_store_has_missing_key);
      ("store_has existing key", `Quick, test_store_has_existing_key);
      ("store_has key too long key", `Quick, test_store_has_key_too_long);
      ("store_list_size counts subtrees", `Quick, test_store_list_size);
      ("store_get_nth_key produces subtrees", `Quick, test_store_get_nth_key);
      ("store_delete removes subtree and value", `Quick, test_store_delete_all);
      ("store_copy", `Quick, test_store_copy);
      ("store_copy missing node", `Quick, test_store_copy_missing_path);
      ("store_move", `Quick, test_store_move);
      ("store_move missing node", `Quick, test_store_move_missing_path);
      ("store_read", `Quick, test_store_read);
      ("store_read on non-value", `Quick, test_store_read_non_value);
      ("store_write", `Quick, test_store_write);
      ("store_value_size", `Quick, test_store_value_size);
      ("store_get_hash", `Quick, test_store_get_hash);
      ("store_delete_value removes only value", `Quick, test_store_delete_value);
      ("store_create", `Quick, test_store_create);
    ]
  @ [
      tztest "Durable: find value" `Quick test_durable_find_value;
      tztest
        "Durable: count subtrees and list"
        `Quick
        test_durable_count_subtrees_and_list;
      tztest "Durable: invalid keys" `Quick test_durable_invalid_keys;
      tztest "Durable: readonly keys" `Quick test_readonly_key;
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Durable storage", tests)]
  |> Lwt_main.run
