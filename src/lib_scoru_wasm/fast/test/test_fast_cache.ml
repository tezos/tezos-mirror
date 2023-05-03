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

(** Testing
    -------
    Component:    Tezos_scoru_wasm_fast.Module_cache
    Invocation:   dune exec src/lib_scoru_wasm/fast/test/main.exe -- --file test_fast_cache.ml
    Subject:      Test the cache used for Wasmer modules
*)

module Assert = Assert

module IntHash = struct
  type t = int

  let equal i j = i = j

  let hash i = i land max_int
end

module Minimal_test_input = struct
  module Key = IntHash

  type value = string

  let delete _ = ()
end

module Simple_cache = Tezos_scoru_wasm_fast.Cache.Make (Minimal_test_input)

(* Tests *)
let test_add_to_empty () =
  let cache = Simple_cache.create 2 in
  Assert.assert_false "Empty at first" (Simple_cache.is_in cache 0) ;
  Simple_cache.replace cache 0 "zero" ;
  Assert.assert_true "Not empty anymore" (Simple_cache.is_in cache 0) ;
  Lwt_result_syntax.return_unit

let test_find () =
  let cache = Simple_cache.create 2 in
  Assert.is_none ~loc:__LOC__ (Simple_cache.find_opt cache 0) ;
  Simple_cache.replace cache 0 "zero" ;
  Assert.String.Option.equal
    ~loc:__LOC__
    (Some "zero")
    (Simple_cache.find_opt cache 0) ;
  Lwt_result_syntax.return_unit

let test_replace () =
  let removed = ref [] in
  let was_removed v = List.mem ~equal:String.equal v !removed in
  let module Test_input = struct
    module Key = IntHash

    type value = string

    let delete v = removed := v :: !removed
  end in
  let module Cache = Tezos_scoru_wasm_fast.Cache.Make (Test_input) in
  let cache = Cache.create 2 in
  Cache.replace cache 0 "zero" ;
  Assert.String.Option.equal ~loc:__LOC__ (Some "zero") (Cache.find_opt cache 0) ;
  Cache.replace cache 0 "one" ;
  Assert.String.Option.equal ~loc:__LOC__ (Some "one") (Cache.find_opt cache 0) ;
  Assert.assert_true "zero should have been deleted" (was_removed "zero") ;
  Assert.assert_false "one should not have been deleted" (was_removed "one") ;
  Lwt_result_syntax.return_unit

let test_add_max_nb () =
  let cache = Simple_cache.create 2 in
  Simple_cache.replace cache 0 "zero" ;
  Simple_cache.replace cache 1 "one" ;
  Assert.assert_true "zero is in" (Simple_cache.is_in cache 0) ;
  Assert.assert_true "one is in" (Simple_cache.is_in cache 1) ;
  Lwt_result_syntax.return_unit

let test_add_more_than_max () =
  let removed = ref [] in
  let was_removed v = List.mem ~equal:String.equal v !removed in
  let module Test_input = struct
    module Key = IntHash

    type value = string

    let delete v = removed := v :: !removed
  end in
  let module Cache = Tezos_scoru_wasm_fast.Cache.Make (Test_input) in
  let cache = Cache.create 2 in
  Assert.assert_false
    "value zero should not be marked as deleted"
    (was_removed "zero") ;
  Cache.replace cache 0 "zero" ;
  Cache.replace cache 1 "one" ;
  Cache.replace cache 2 "two" ;
  Assert.assert_false "zero is out" (Cache.is_in cache 0) ;
  Assert.assert_true "zero should have been deleted" (was_removed "zero") ;
  Assert.assert_true "one is in" (Cache.is_in cache 1) ;
  Assert.assert_true "two is in" (Cache.is_in cache 2) ;
  Lwt_result_syntax.return_unit

let tests =
  [
    Tztest.tztest "add to empty" `Quick test_add_to_empty;
    Tztest.tztest "Find" `Quick test_find;
    Tztest.tztest "Replace existing key" `Quick test_replace;
    Tztest.tztest "add max number of keys" `Quick test_add_max_nb;
    Tztest.tztest "add more than max" `Quick test_add_more_than_max;
  ]

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib scoru-wasm-fast"
    [("Fast Execution cache", tests)]
  |> Lwt_main.run
