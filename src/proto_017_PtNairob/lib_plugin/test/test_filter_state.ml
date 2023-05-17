(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Shell (Plugin)
    Invocation:   dune exec src/proto_017_PtNairob/lib_plugin/test/main.exe \
                  -- --file test_filter_state.ml
    Subject:      Unit tests the filter state functions of the plugin
*)

open Qcheck2_helpers
open Plugin.Mempool
open Test_utils

let count = 1000

let check_filter_state_invariants filter_state =
  qcheck_cond
    ~pp:(fun fmt filter_state ->
      Format.fprintf
        fmt
        "The following filter_state breaks invariants:@.%a"
        pp_state
        filter_state)
    ~cond:(fun filter_state ->
      filter_state.prechecked_manager_op_count
      = Operation_hash.Map.cardinal filter_state.prechecked_manager_ops
      && filter_state.prechecked_manager_op_count
         = ManagerOpWeightSet.cardinal filter_state.prechecked_op_weights
      && ManagerOpWeightSet.for_all
           (fun {operation_hash; weight} ->
             match
               Operation_hash.Map.find
                 operation_hash
                 filter_state.prechecked_manager_ops
             with
             | None -> false
             | Some info -> Q.equal weight info.weight)
           filter_state.prechecked_op_weights
      && eq_op_weight_opt
           (ManagerOpWeightSet.min_elt filter_state.prechecked_op_weights)
           filter_state.min_prechecked_op_weight)
    filter_state
    ()

(** Test that [add_manager_op] adds the given operation to the filter
    state, and removes the replaced operation if applicable. *)
let test_add_manager_op =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"Add a manager operation"
    (Gen.pair Generators.filter_state_with_two_operations_gen Gen.bool)
    (fun ((filter_state, (oph, op_info), (oph_to_replace, _)), should_replace)
    ->
      (* Both [oph] and [oph_to_replace] have even odds of being
         already present in [filter_state] or fresh. *)
      let replacement =
        if should_replace then (
          assume (not (Operation_hash.equal oph_to_replace oph)) ;
          `Replace (oph_to_replace, ()))
        else `No_replace
      in
      let filter_state = add_manager_op filter_state oph op_info replacement in
      check_filter_state_invariants filter_state
      && qcheck_cond
           ~pp:(fun fmt set ->
             Format.fprintf
               fmt
               "%a was not found in prechecked_manager_ops: %a"
               Operation_hash.pp
               oph
               pp_prechecked_manager_ops
               set)
           ~cond:(Operation_hash.Map.mem oph)
           filter_state.prechecked_manager_ops
           ()
      &&
      if should_replace then
        qcheck_cond
          ~pp:(fun fmt () ->
            Format.fprintf
              fmt
              "%a should have been removed from prechecked_manager_ops."
              Operation_hash.pp
              oph_to_replace)
          ~cond:(fun () ->
            not
              (Operation_hash.Map.mem
                 oph_to_replace
                 filter_state.prechecked_manager_ops))
          ()
          ()
      else true)

(** Test that [remove] removes the operation from the filter state if
    it was present, and that adding then removing is the same as doing
    nothing but removing the replaced operation if there is one. *)
let test_remove_present =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"Remove an existing operation hash"
    (Gen.triple
       Generators.filter_state_with_operation_gen
       Generators.oph_and_info_gen
       Gen.bool)
    (fun ((initial_state, (oph_to_replace, _)), (oph, op_info), should_replace)
    ->
      (* Add a fresh operation [oph] to the state. *)
      assume
        (not (Operation_hash.Map.mem oph initial_state.prechecked_manager_ops)) ;
      let replacement =
        if should_replace then `Replace (oph_to_replace, ()) else `No_replace
      in
      let filter_state = add_manager_op initial_state oph op_info replacement in
      (* Remove [oph] from the state, in which it was present. *)
      let filter_state = remove_operation filter_state oph in
      let (_ : bool) =
        (* Check that the state invariants are preserved and that
           [oph] has been removed. *)
        check_filter_state_invariants filter_state
        && qcheck_cond
             ~pp:(fun fmt () ->
               Format.fprintf
                 fmt
                 "%a should have been removed from prechecked_manager_ops."
                 Operation_hash.pp
                 oph)
             ~cond:(fun () ->
               not
                 (Operation_hash.Map.mem
                    oph
                    filter_state.prechecked_manager_ops))
             ()
             ()
      in
      (* Check that adding a fresh operation then removing it is the
         same as doing nothing except removing any replaced operation. *)
      let initial_state_without_replaced_op =
        if should_replace then remove_operation initial_state oph_to_replace
        else initial_state
      in
      qcheck_eq
        ~pp:pp_state
        ~eq:eq_state
        initial_state_without_replaced_op
        filter_state)

(** Test that [remove] leaves the filter state intact if the operation
    hash is unknown. *)
let test_remove_unknown =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"Remove an unknown operation hash"
    (Gen.pair Generators.filter_state_gen Generators.operation_hash_gen)
    (fun (initial_state, oph) ->
      assume
        (not (Operation_hash.Map.mem oph initial_state.prechecked_manager_ops)) ;
      let filter_state = remove_operation initial_state oph in
      qcheck_eq ~pp:pp_state ~eq:eq_state initial_state filter_state)

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [
      ("add_manager_op", qcheck_wrap [test_add_manager_op]);
      ("remove", qcheck_wrap [test_remove_present; test_remove_unknown]);
    ]
