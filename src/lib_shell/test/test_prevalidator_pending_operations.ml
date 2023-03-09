(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Component:    Shell (Prevalidator pending operations)
    Invocation:   dune exec src/lib_shell/test/main.exe
    Subject:      Unit tests the Prevalidator pending operations APIs
*)

open Qcheck2_helpers
module Pending_ops = Prevalidator_pending_operations
module CompareListQ = Compare.List (Q)

let pending_of_list =
  List.fold_left
    (fun pendings (op, priority) ->
      if
        Operation_hash.Set.mem
          (Shell_operation.Internal_for_tests.hash_of op)
          (Pending_ops.hashes pendings)
      then (* no duplicate hashes *)
        pendings
      else Pending_ops.add op priority pendings)
    Pending_ops.empty

(* 1. Test iterators ordering *)

let test_iterators_ordering ~name ~iterator return_value =
  let open QCheck2 in
  Test.make
    ~name:
      (Format.sprintf
         "Ensure that %s returns operations in their priority ordering"
         name)
    (Gen.small_list (Generators.operation_with_hash_and_priority_gen ()))
  @@ fun ops ->
  let previous_priority = ref `High in
  let previous_prio_ok ~priority ~previous_priority =
    match (previous_priority, priority) with
    | `High, `High -> true
    | (`High | `Medium), `Medium -> true
    | (`High | `Medium), `Low _ -> true
    | `Low q_prev, `Low q_new -> CompareListQ.(q_new <= q_prev)
    | _, _ -> false
  in
  iterator
    (fun priority _hash _op () ->
      (* Here, we check the priority ordering in the iterators of
         prevalidator_pending_operations module : if the current considered
         priority is `High, it should be true that the previously seen is also
         `High. *)
      if not @@ previous_prio_ok ~priority ~previous_priority:!previous_priority
      then
        QCheck.Test.fail_reportf
          "Pending operations are not ordered by priority" ;
      previous_priority := priority ;
      return_value)
    (pending_of_list ops)
    ()
  |> fun _ -> true

let test_iter_ordering =
  test_iterators_ordering
    ~name:"iter"
    ~iterator:(fun f ops _acc -> Pending_ops.iter (fun p h o -> f p h o ()) ops)
    ()

let test_fold_ordering =
  test_iterators_ordering ~name:"fold" ~iterator:Pending_ops.fold ()

let test_fold_es_ordering =
  test_iterators_ordering
    ~name:"fold_es"
    ~iterator:Pending_ops.fold_es
    Lwt_result_syntax.return_unit

(* 2. Test partial iteration with fold_es *)

let test_partial_fold_es =
  let open QCheck2 in
  Test.make
    ~name:"Test cardinal after partial iteration with fold_es"
    (Gen.pair
       (Gen.small_list (Generators.operation_with_hash_and_priority_gen ()))
       Gen.small_nat)
  @@ fun (ops, to_process) ->
  let pending = pending_of_list ops in
  let card0 = Pending_ops.cardinal pending in
  Lwt_main.run
  @@ Pending_ops.fold_es
       (fun _priority hash _op (remaining_to_process, acc) ->
         assert (remaining_to_process >= 0) ;
         if remaining_to_process = 0 then Lwt.return_error acc
         else
           Lwt.return_ok (remaining_to_process - 1, Pending_ops.remove hash acc))
       pending
       (to_process, pending)
  |> function
  | Ok (remaining_to_process, pending) ->
      (* [Ok] means we have reached the end of the collection before exhausting
         the limit of iterations: the remaining collection is empty && we spent
         as many iterations as there were elements in the original collection. *)
      let card1 = Pending_ops.cardinal pending in
      qcheck_eq' ~pp:Format.pp_print_int ~expected:0 ~actual:card1 ()
      && qcheck_eq'
           ~pp:Format.pp_print_int
           ~expected:card0
           ~actual:(to_process - remaining_to_process)
           ()
  | Error pending ->
      (* [Error] means we have reached the limit of the number of iterations:
         the number of removed elements is exactly the number of iterations &&
         there are still elements in the resulting collection. *)
      let card1 = Pending_ops.cardinal pending in
      qcheck_eq'
        ~pp:Format.pp_print_int
        ~expected:to_process
        ~actual:(card0 - card1)
        ()
      && qcheck_neq ~pp:Format.pp_print_int card1 0

let () =
  let mk_tests label tests = (label, qcheck_wrap tests) in
  Alcotest.run
    ~__FILE__
    "Prevalidator_pending_operations"
    [
      mk_tests "iter ordering" [test_iter_ordering];
      mk_tests "fold ordering" [test_fold_ordering];
      mk_tests "fold_es ordering" [test_fold_es_ordering];
      mk_tests "partial fold_es" [test_partial_fold_es];
    ]
