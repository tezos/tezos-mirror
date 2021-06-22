(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Requester
    Invocation:   dune build @src/lib_requester/runtest_fuzzing_requester
    Subject:      Relations between functions of [Requester]'s API
*)

include Shared
open Lib_test.Qcheck_helpers

(** Completely random key generator. Mitigated by also picking keys
 *  in the requester's domain, see [requester_and_keys_gen] below. *)
let key_arb = QCheck.string

let value_arb = QCheck.int

(** Creates a requester filled with arbitrary content.
 *  The requester's keys are also returned. *)
let domain_and_requester_arb : (string list * Test_Requester.t) QCheck.arbitrary
    =
  let create sets =
    let requester = init_full_requester () in
    let set_one (key, value) =
      Test_Requester.inject requester key value >|= ignore
    in
    Lwt_main.run @@ Lwt_list.iter_s set_one sets ;
    (List.map fst sets, requester)
  in
  QCheck.(map create (list (pair key_arb value_arb)))

(** A generator for a requester and for two keys. We generate them
 *  all simultaneously, to augment chances to have keys that are in
 *  the requester's domain. If we were generating keys randomly and without
 *  the concerned requester, the keys would most of the time be outside
 *  the requester's domain; yielding poor test distribution.
 *
 *  We could have generated 1 key or 3 keys, or whatever number of keys; but
 *  in practice, in tests below; we need at most two keys.
 *)
let requester_and_keys_gen : (Test_Requester.t * string * string) QCheck.Gen.t =
  let open QCheck.Gen in
  QCheck.gen domain_and_requester_arb >>= fun (domain, requester) ->
  let key_gen =
    let in_domain_gen = if domain = [] then [] else [oneofl domain] in
    (* Either a random key or a key in the domain *)
    oneof (QCheck.gen key_arb :: in_domain_gen)
  in
  pair key_gen key_gen >>= fun (key1, key2) -> pure (requester, key1, key2)

let requester_and_keys_arb =
  let print = QCheck.Print.(triple (Fun.const "requester") string string) in
  QCheck.make ~print requester_and_keys_gen

let test_read_read_opt =
  QCheck.Test.make
    ~name:"Result.is_ok (read t key) = Option.is_some (read_opt t key)"
    requester_and_keys_arb
  @@ fun (t, key, _) ->
  let found_by_read =
    Lwt_main.run @@ (Test_Requester.read t key >|= Result.is_ok)
  in
  let found_by_read_opt =
    Lwt_main.run @@ (Test_Requester.read_opt t key >|= Option.is_some)
  in
  qcheck_eq ~pp:Format.pp_print_bool found_by_read found_by_read_opt

let test_read_opt_known =
  QCheck.Test.make
    ~name:"known t key = Option.is_some (read_opt t key)"
    requester_and_keys_arb
  @@ fun (t, key, _) ->
  let known = Lwt_main.run @@ Test_Requester.known t key in
  let found_by_read_opt =
    Lwt_main.run @@ (Test_Requester.read_opt t key >|= Option.is_some)
  in
  qcheck_eq ~pp:Format.pp_print_bool known found_by_read_opt

let test_inject_read_opt =
  QCheck.Test.make
    ~name:"read_opt (inject t key value) = (Some value)"
    QCheck.(pair requester_and_keys_arb value_arb)
  @@ fun ((t, key, _), value) ->
  let injected = Lwt_main.run @@ Test_Requester.inject t key value in
  QCheck.assume injected ;
  let read = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq'
    ~pp:(Format.pp_print_option Format.pp_print_int)
    ~expected:(Some value)
    ~actual:read
    ()

let test_inject_read_opt_other =
  QCheck.Test.make
    ~name:"key <> key' ==> read_opt (inject t key' value) key = read_opt t key"
    QCheck.(pair requester_and_keys_arb value_arb)
  @@ fun ((t, key, key'), value) ->
  QCheck.assume (key <> key') ;
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  let _ = Lwt_main.run @@ Test_Requester.inject t key' value in
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(read_opt_before = read_opt_after)
    ()

let leq_opt opt1 opt2 =
  match (opt1, opt2) with (None, _) | (Some _, Some _) -> true | _ -> false

let test_inject_growth =
  QCheck.Test.make
    ~name:"read_opt t key <= read_opt (inject t key' value) key"
    QCheck.(pair requester_and_keys_arb value_arb)
  @@ fun ((t, key, key'), value) ->
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  let _ = Lwt_main.run @@ Test_Requester.inject t key' value in
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(leq_opt read_opt_before read_opt_after)
    ()

let test_inject_memory_table_length =
  QCheck.Test.make
    ~name:"memory_table_length t <= memory_table_length (inject t key value)"
    QCheck.(pair requester_and_keys_arb value_arb)
  @@ fun ((t, key, _), value) ->
  let length_before = Test_Requester.memory_table_length t in
  let _ = Lwt_main.run @@ Test_Requester.inject t key value in
  let length_after = Test_Requester.memory_table_length t in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(length_before <= length_after)
    ()

let test_clear_shrink =
  QCheck.Test.make
    ~name:"read_opt (clear_or_cancel t key') key <= read_opt t key"
    requester_and_keys_arb
  @@ fun (t, key, key') ->
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  Test_Requester.clear_or_cancel t key' ;
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(leq_opt read_opt_after read_opt_before)
    ()

let test_clear_memory_table_length =
  QCheck.Test.make
    ~name:"memory_length (clear_or_cancel t key') key <= memory_length t"
    requester_and_keys_arb
  @@ fun (t, key, _) ->
  let length_before = Test_Requester.memory_table_length t in
  Test_Requester.clear_or_cancel t key ;
  let length_after = Test_Requester.memory_table_length t in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(length_after <= length_before)
    ()

let () =
  Alcotest.run
    "Requester_PBT"
    [
      ("read", qcheck_wrap [test_read_read_opt; test_read_opt_known]);
      ( "inject",
        qcheck_wrap
          [
            test_inject_read_opt;
            test_inject_read_opt_other;
            test_inject_growth;
            test_inject_memory_table_length;
          ] );
      ( "clear_or_cancel",
        qcheck_wrap [test_clear_shrink; test_clear_memory_table_length] );
    ]
