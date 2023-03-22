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
    Invocation:   dune exec src/lib_requester/test/main.exe
    Subject:      Relations between functions of [Requester]'s API
*)

include Shared
open Qcheck2_helpers
open QCheck2

(** Completely random key generator. Mitigated by also picking keys
 *  in the requester's domain, see [requester_and_keys_gen] below. *)
let key_gen = Gen.small_string ?gen:None

let value_gen = Gen.int

(** Creates a requester filled with arbitrary content.
 *  The requester's keys are also returned. *)
let domain_and_requester_gen : (string list * Test_Requester.t) Gen.t =
  let open Lwt_syntax in
  let create sets =
    let requester = init_full_requester () in
    let set_one (key, value) =
      let* _ = Test_Requester.inject requester key value in
      Lwt.return_unit
    in
    Lwt_main.run @@ Lwt_list.iter_s set_one sets ;
    (List.map fst sets, requester)
  in
  Gen.(map create (list (pair key_gen value_gen)))

(** A generator for a requester and for two keys. We generate them
 *  all simultaneously, to augment chances to have keys that are in
 *  the requester's domain. If we were generating keys randomly and without
 *  the concerned requester, the keys would most of the time be outside
 *  the requester's domain; yielding poor test distribution.
 *
 *  We could have generated 1 key or 3 keys, or whatever number of keys; but
 *  in practice, in tests below; we need at most two keys.
 *)
let requester_and_keys_gen : (Test_Requester.t * string * string) Gen.t =
  let open Gen in
  let* domain, requester = domain_and_requester_gen in
  let key_gen =
    let in_domain_gen = if domain = [] then [] else [oneofl domain] in
    (* Either a random key or a key in the domain *)
    oneof (key_gen :: in_domain_gen)
  in
  let* key1, key2 = pair key_gen key_gen in
  pure (requester, key1, key2)

let print = Print.(triple (Fun.const "requester") string string)

let qcheck_eq_true ~actual =
  qcheck_eq' ~pp:Format.pp_print_bool ~expected:true ~actual ()

let test_read_read_opt =
  Test.make
    ~name:"Result.is_ok (read t key) = Option.is_some (read_opt t key)"
    ~print
    requester_and_keys_gen
  @@ fun (t, key, _) ->
  let open Lwt_syntax in
  let found_by_read =
    Lwt_main.run
    @@ let+ r = Test_Requester.read t key in
       Result.is_ok r
  in
  let found_by_read_opt =
    Lwt_main.run
    @@ let+ o = Test_Requester.read_opt t key in
       Option.is_some o
  in
  qcheck_eq ~pp:Format.pp_print_bool found_by_read found_by_read_opt

let test_read_opt_known =
  Test.make
    ~name:"known t key = Option.is_some (read_opt t key)"
    ~print
    requester_and_keys_gen
  @@ fun (t, key, _) ->
  let open Lwt_syntax in
  let known = Lwt_main.run @@ Test_Requester.known t key in
  let found_by_read_opt =
    Lwt_main.run
    @@ let+ o = Test_Requester.read_opt t key in
       Option.is_some o
  in
  qcheck_eq ~pp:Format.pp_print_bool known found_by_read_opt

let test_inject_read_opt =
  Test.make
    ~name:"read_opt (inject t key value) = (Some value)"
    ~print:Print.(pair print int)
    Gen.(pair requester_and_keys_gen value_gen)
  @@ fun ((t, key, _), value) ->
  let injected = Lwt_main.run @@ Test_Requester.inject t key value in
  assume injected ;
  let read = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq'
    ~pp:(Format.pp_print_option Format.pp_print_int)
    ~expected:(Some value)
    ~actual:read
    ()

let test_inject_read_opt_other =
  Test.make
    ~name:"key <> key' ==> read_opt (inject t key' value) key = read_opt t key"
    ~print:Print.(pair print int)
    Gen.(pair requester_and_keys_gen value_gen)
  @@ fun ((t, key, key'), value) ->
  assume (key <> key') ;
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  let _ = Lwt_main.run @@ Test_Requester.inject t key' value in
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq_true ~actual:(read_opt_before = read_opt_after)

let leq_opt opt1 opt2 =
  match (opt1, opt2) with None, _ | Some _, Some _ -> true | _ -> false

let test_inject_growth =
  Test.make
    ~name:"read_opt t key <= read_opt (inject t key' value) key"
    ~print:Print.(pair print int)
    Gen.(pair requester_and_keys_gen value_gen)
  @@ fun ((t, key, key'), value) ->
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  let _ = Lwt_main.run @@ Test_Requester.inject t key' value in
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq_true ~actual:(leq_opt read_opt_before read_opt_after)

let test_inject_memory_table_length =
  Test.make
    ~name:"memory_table_length t <= memory_table_length (inject t key value)"
    ~print:Print.(pair print int)
    Gen.(pair requester_and_keys_gen value_gen)
  @@ fun ((t, key, _), value) ->
  let length_before = Test_Requester.memory_table_length t in
  let _ = Lwt_main.run @@ Test_Requester.inject t key value in
  let length_after = Test_Requester.memory_table_length t in
  qcheck_eq_true ~actual:(length_before <= length_after)

let test_clear_shrink =
  Test.make
    ~name:"read_opt (clear_or_cancel t key') key <= read_opt t key"
    ~print
    requester_and_keys_gen
  @@ fun (t, key, key') ->
  let read_opt_before = Lwt_main.run @@ Test_Requester.read_opt t key in
  Test_Requester.clear_or_cancel t key' ;
  let read_opt_after = Lwt_main.run @@ Test_Requester.read_opt t key in
  qcheck_eq_true ~actual:(leq_opt read_opt_after read_opt_before)

let test_clear_memory_table_length =
  Test.make
    ~name:"memory_length (clear_or_cancel t key') key <= memory_length t"
    ~print
    requester_and_keys_gen
  @@ fun (t, key, _) ->
  let length_before = Test_Requester.memory_table_length t in
  Test_Requester.clear_or_cancel t key ;
  let length_after = Test_Requester.memory_table_length t in
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:true
    ~actual:(length_after <= length_before)
    ()

let test_clear_pending =
  Test.make
    ~name:"pending (clear_or_cancel (fetch (fetch t key) key) key) key = true"
    ~print
    requester_and_keys_gen
  @@ fun (t, key, _) ->
  (* ensure that [key] is not in the memory_table *)
  Test_Requester.clear_or_cancel t key ;
  let _ = Test_Requester.fetch t key false in
  let _ = qcheck_eq_true ~actual:(Test_Requester.pending t key) in
  let _ = Test_Requester.fetch t key false in
  let _ = qcheck_eq_true ~actual:(Test_Requester.pending t key) in
  Test_Requester.clear_or_cancel t key ;
  (* [key] is still in the memory_table after one call to clear_of_cancel *)
  let _ = qcheck_eq_true ~actual:(Test_Requester.pending t key) in
  Test_Requester.clear_or_cancel t key ;
  qcheck_eq'
    ~pp:Format.pp_print_bool
    ~expected:false
    ~actual:(Test_Requester.pending t key)
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
        qcheck_wrap
          [
            test_clear_shrink; test_clear_memory_table_length; test_clear_pending;
          ] );
    ]
