(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    Component:    Shell_operation and others
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_shell_operation.ml
    Subject:      Unit tests for [Shell_operation], and for other
                  components e.g. [Requester] when the tests rely on
                  the operation representation provided by
                  [Shell_operation].
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
    Shell_operation.Internal_for_tests.safe_binary_of_bytes
      broken_encoding
      Bytes.empty
  in
  Alcotest.(
    check
      bool
      "A broken encoding should return None"
      (actual = Result_syntax.tzfail Validation_errors.Parse_error)
      true)

open Tezos_requester
module Classification = Prevalidator_classification

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
  let parameters =
    Classification.
      {
        map_size_limit = max_table_size;
        on_discarded_operation = Test_Requester.clear_or_cancel requester;
      }
  in
  let classes = Classification.create parameters in
  let handle i =
    let op = mk_operation i in
    let oph = Operation.hash op in
    let op = Shell_operation.Internal_for_tests.make_operation op oph () in
    let injected = Lwt_main.run @@ Test_Requester.inject requester oph i in
    assert injected ;
    f [] op classes
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

(** Check that when doing a succession of inject/classify operations,
 *  the memory table of the requester stays small. In the past, this
 *  was broken; because of a missing call to remove elements
 *  from [Classes.in_mempool]. This could be exploited to create
 *  a memory leak. *)
let test_in_mempool_leak f (nb_ops : int) (_ : unit) =
  let requester = init_full_requester () in
  let max_table_size = 32 in
  assert (nb_ops >= max_table_size) ;
  let parameters =
    Classification.
      {
        map_size_limit = max_table_size;
        on_discarded_operation = Test_Requester.clear_or_cancel requester;
      }
  in
  let classes = Classification.create parameters in
  let handle i =
    let op = mk_operation i in
    let oph = Operation.hash op in
    let op = Shell_operation.Internal_for_tests.make_operation op oph () in
    let injected = Lwt_main.run @@ Test_Requester.inject requester oph i in
    assert injected ;
    f [] op classes
  in
  List.iter handle (1 -- nb_ops) ;
  let actual_in_mempool_size = Operation_hash.Map.cardinal classes.in_mempool in
  Alcotest.(
    check
      bool
      (Printf.sprintf
         "in_mempool size (%d) is less or equal than %d"
         actual_in_mempool_size
         max_table_size)
      (actual_in_mempool_size <= max_table_size)
      true)

(** Check that right after doing a classify operations,
 *  the memory table contains the concerned operation. This got
 *  broken in the past: it was being cleared right away when the ring was full.
 *  This could be exploited to create a memory leak in {!Distributed_db}. *)
let test_db_do_not_clear_right_away f (nb_ops : int) (_ : unit) =
  let requester = init_full_requester () in
  let max_table_size = 32 in
  assert (nb_ops >= max_table_size) ;
  let parameters =
    Classification.
      {
        map_size_limit = max_table_size;
        on_discarded_operation = Test_Requester.clear_or_cancel requester;
      }
  in
  let classes = Classification.create parameters in
  let handle i =
    let op = mk_operation i in
    let oph = Operation.hash op in
    let op = Shell_operation.Internal_for_tests.make_operation op oph () in
    Format.printf "Injecting op: %a\n" Operation_hash.pp oph ;
    let injected = Lwt_main.run @@ Test_Requester.inject requester oph i in
    assert injected ;
    f [] op classes ;
    Alcotest.(
      check
        bool
        (Format.asprintf
           "requester memory contains most recent classified operation (%a)"
           Operation_hash.pp
           oph)
        (Option.is_some @@ Lwt_main.run @@ Test_Requester.read_opt requester oph)
        true)
  in
  List.iter handle (1 -- nb_ops)

let () =
  let nb_ops = [64; 128] in
  let handle_refused_pair =
    [
      ((fun tztrace -> Classification.add (`Refused tztrace)), "handle_refused");
      ( (fun tztrace -> Classification.add (`Outdated tztrace)),
        "handle_outdated" );
    ]
  in
  let handle_branch_pairs =
    [
      ( (fun tztrace -> Classification.add (`Branch_refused tztrace)),
        "handle_branch_refused" );
      ( (fun tztrace -> Classification.add (`Branch_delayed tztrace)),
        "handle_branch_delayed" );
    ]
  in
  let applier_funs = handle_branch_pairs @ handle_refused_pair in
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
    List.concat_map (mk_test_cases ~test:test_db_leak) applier_funs
  in
  let in_mempool_leak_test =
    List.concat_map
      (mk_test_cases ~test:test_in_mempool_leak)
      handle_refused_pair
  in
  let ddb_clearing_tests =
    List.concat_map
      (mk_test_cases ~test:test_db_do_not_clear_right_away)
      handle_branch_pairs
  in
  Alcotest.run
    ~__FILE__
    "Shell_operation"
    [
      ( "Corner cases",
        [
          Alcotest.test_case
            "Raising an exception in encoding doesn't break"
            `Quick
            test_safe_decode;
        ] );
      ("Ddb leaks", all_ddb_leak_tests);
      ("Mempool Leaks", in_mempool_leak_test);
      ("Ddb clearing", ddb_clearing_tests);
    ]
