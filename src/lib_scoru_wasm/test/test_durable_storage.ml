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
open Tezos_lazy_containers
open Tezos_webassembly_interpreter
open Tezos_scoru_wasm
include Test_encodings_util
open Wasm_utils
module Wasm = Wasm_pvm.Make (Tree)
module Wrapped_tree_runner =
  Tezos_tree_encoding.Runner.Make (Tezos_tree_encoding.Wrapped)

let value_store_key_too_large =
  Values.(Num (I32 Host_funcs.Error.(code Store_key_too_large)))

let equal_chunks c1 c2 =
  let open Lwt.Syntax in
  let* c1 = Chunked_byte_vector.to_string c1 in
  let* c2 = Chunked_byte_vector.to_string c2 in
  Lwt.return @@ assert (String.equal c1 c2)

(* Test checking that if [key] is missing, [store_has key] returns [false] *)
let test_store_has_missing_key () =
  let open Lwt.Syntax in
  let* durable = make_durable [] in
  let key = "/test/path" in
  let src = 10l in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst [key] src
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
let test_store_has_key_too_long () =
  let open Lwt_syntax in
  let* durable = make_durable [] in
  (* Together with the '/durable' prefix, and '/_' this key is too long *)
  let src = 10l in
  let key = List.repeat 240 "a" |> String.concat "" |> ( ^ ) "/" in
  let module_reg, module_key, host_funcs_registry =
    make_module_inst [key] 10l
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
let test_store_list_size () =
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
    make_module_inst [key] src
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
let test_store_get_nth_key () =
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
    make_module_inst [key] src
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

(* Test checking that [store_delete key] deletes the subtree at [key] from the
   durable storage. *)
let test_store_delete () =
  let open Lwt_syntax in
  (*
  Store the following tree:
    /durable/a/short/path/_ = "..."
    /durable/a/short/path/one/_ = "..."
    /durable/a/long/path/_ = "..."

  We expect that deleting "/a/short/path" is leaves only "/durable/a/long/path".
  *)
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
    make_module_inst [key] src
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
      Host_funcs.Internal_for_tests.store_delete
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
    make_module_inst [key; key_one; key_two] src
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

let test_durable_count_subtrees () =
  let open Lwt_syntax in
  let* tree =
    make_durable
      [
        ("/hello", "a very long value");
        ("/hello/world", "a very long value");
        ("/hello/you", "a very long value");
        ("/hello/you/too", "a very long value");
      ]
  in
  let assert_subtree_count t count under =
    let dbl = Durable.of_storage_exn t in
    let+ n = Durable.count_subtrees dbl @@ Durable.key_of_string_exn under in
    assert (n = count)
  in
  let* () = assert_subtree_count tree 3 "/hello" in
  let* () = assert_subtree_count tree 2 "/hello/you" in
  let* () = assert_subtree_count tree 1 "/hello/you/too" in
  let* () = assert_subtree_count tree 0 "/bye" in
  Lwt.return_ok ()

(* Test checking that [store_copy from_key to_key] copies the subtree at
   [from_key] to [to_key] in the durable storage.  This should overwrite
   the tree that existed previously at [to_key] *)
let test_store_copy () =
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
    make_module_inst [from_key; to_key; wrong_key] src
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

let test_store_move () =
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
    make_module_inst [from_key; to_key; bad_key] src
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

let test_store_read () =
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
    make_module_inst [key] src
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

let test_store_value_size () =
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
    make_module_inst [key; invalid_key; missing_key; contents] src
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

let test_store_write () =
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
    make_module_inst [existing_key; new_key; contents] src
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
    tztest "store_get_nth_key produces subtrees" `Quick test_store_get_nth_key;
    tztest "store_delete removes subtree" `Quick test_store_delete;
    tztest "store_copy" `Quick test_store_copy;
    tztest "store_move" `Quick test_store_move;
    tztest "store_read" `Quick test_store_read;
    tztest "store_write" `Quick test_store_write;
    tztest "store_value_size" `Quick test_store_value_size;
    tztest "Durable: find value" `Quick test_durable_find_value;
    tztest "Durable: count subtrees" `Quick test_durable_count_subtrees;
    tztest "Durable: invalid keys" `Quick test_durable_invalid_keys;
  ]
