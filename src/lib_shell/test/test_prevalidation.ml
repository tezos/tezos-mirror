(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Component:    Prevalidation
    Invocation:   dune build @src/lib_shell/test/runtest_prevalidation
    Subject:      Unit tests for [Prevalidation]
*)

let test_safe_decode () =
  let exception Custom_exception of string in
  let broken_encoding =
    Data_encoding.conv
      Fun.id
      (fun _ -> raise (Custom_exception "Should not leave the function scope"))
      Data_encoding.unit
  in
  let actual =
    Prevalidation.Internal_for_tests.safe_binary_of_bytes
      broken_encoding
      Bytes.empty
  in
  Alcotest.(
    check
      bool
      "A broken encoding should return None"
      (actual = error Validation_errors.Parse_error)
      true)

open Tezos_requester
module Classification = Prevalidation.Classification

module Parameters :
  Requester_impl.PARAMETERS
    with type key = Operation_hash.t
     and type value = int = struct
  type key = Operation_hash.t

  type value = int
end

module Hash : Requester.HASH with type t = Operation_hash.t = struct
  type t = Parameters.key

  let name = "test_with_key_Operation_hash_dot_t"

  let encoding = Operation_hash.encoding

  let pp = Operation_hash.pp
end

module Test_request = Requester_impl.Simple_request (Parameters)
module Test_disk_table = Requester_impl.Disk_memory_table (Parameters)
module Test_Requester =
  Requester_impl.Make_memory_full_requester (Hash) (Parameters) (Test_request)
module Classificator =
  Prevalidator.Internal_for_tests.Classificator (Test_Requester)

let init_full_requester_disk ?global_input () :
    Test_Requester.t * Test_Requester.store =
  let (st : Test_Requester.store) = Test_disk_table.create 16 in
  let requester = Test_Requester.create ?global_input () st in
  (requester, st)

let init_full_requester ?global_input () : Test_Requester.t =
  fst (init_full_requester_disk ?global_input ())

let mk_operation n : Operation.t =
  let base = "BLuxtLkkNKWgV8xTzBuGcHJRPukgk4nY" in
  let base_len = String.length base in
  let n_string = Int.to_string n in
  let n_string_len = String.length n_string in
  assert (0 <= n_string_len && n_string_len <= String.length base) ;
  let base_prefix = String.sub base 0 (base_len - n_string_len) in
  let hash_string = base_prefix ^ n_string in
  assert (String.length hash_string = base_len) ;
  let branch = Block_hash.of_string_exn hash_string in
  let proto = Bytes.of_string n_string in
  {shell = {branch}; proto}

(** Check that when doing a succession of inject/classify operations,
 *  the memory table of the requester stays small. In the past, this
 *  was broken; because of a missing call to function
 *  [Requester.clear_or_cancel]. This could be exploited to create
 *  a memory leak in {!Distributed_db}. *)
let test_db_leak f (nb_ops : int) (_ : unit) =
  let requester = init_full_requester () in
  let max_table_size = 32 in
  assert (nb_ops >= max_table_size) ;
  let classes = Classification.mk_empty max_table_size in
  let handle i =
    let op = mk_operation i in
    let oph = Operation.hash op in
    let injected = Lwt_main.run @@ Test_Requester.inject requester oph i in
    assert injected ;
    f (requester, classes) op oph []
  in
  List.iter handle (1 -- nb_ops) ;
  let actual_table_size = Test_Requester.memory_table_length requester in
  Alcotest.(
    check
      bool
      (Printf.sprintf
         "requester memory (%d) is less or equal than %d"
         actual_table_size
         max_table_size)
      (actual_table_size <= max_table_size)
      true)

let () =
  let nb_ops = [512; 1024; 2048] in
  let handle_refused_pair = (Classificator.handle_refused, "handle_refused") in
  let applier_funs =
    [
      (Classificator.handle_branch_refused, "handle_branch_refused");
      (Classificator.handle_branch_delayed, "handle_branch_delayed");
      handle_refused_pair;
    ]
  in
  let mk_test_cases ~test (applier_fun, applier_fun_str) =
    List.map
      (fun nb_ops ->
        Alcotest.test_case
          (Format.sprintf "%s:%d calls" applier_fun_str nb_ops)
          `Quick
          (test applier_fun nb_ops))
      nb_ops
  in
  let all_ddb_leak_tests =
    List.map (mk_test_cases ~test:test_db_leak) applier_funs |> List.concat
  in
  Alcotest.run
    "Prevalidation"
    [
      ( "Corner cases",
        [
          Alcotest.test_case
            "Raising an exception in encoding doesn't break"
            `Quick
            test_safe_decode;
        ] );
      ("Leaks", all_ddb_leak_tests);
    ]
