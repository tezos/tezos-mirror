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
    Invocation:   dune exec src/proto_alpha/lib_plugin/test/test_filter_state.exe
    Subject:      Unit tests the filter tests function of the plugin
*)

open Lib_test.Qcheck2_helpers
open Plugin.Mempool
open Test_utils

let count = 1000

(** Test that [check_manager_restriction] returns [Fresh] when we use an unknown
    [pkh] in the filter state. *)
let test_check_manager_restriction_fresh =
  let open QCheck2 in
  Test.make
    ~count
    ~name:
      "[check_manager_restriction empty (remove pkh filter) pkh] returns \
       [Fresh]"
    Generators.filter_state_with_operation_gen
    (fun (filter_state, (_oph, pkh)) ->
      let config = config None in
      let filter_state =
        {
          filter_state with
          op_prechecked_managers =
            Signature.Public_key_hash.Map.remove
              pkh
              filter_state.op_prechecked_managers;
        }
      in
      match
        check_manager_restriction
          config
          filter_state
          pkh
          ~fee:Alpha_context.Tez.zero
          ~gas_limit:Alpha_context.Gas.Arith.zero
      with
      | `Fresh -> true
      | `Replace _ | `Fail (`Branch_delayed _) ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a should not be in the set of precheck managers:@,\
             %a@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state)

(** Test that [check_manager_restriction] returns [Fail (`Branch_delayed _)]
    when we use a known [pkh] that is associated with an operation having higher
    fees. *)
let test_check_manager_restriction_fail =
  let open QCheck2 in
  Test.make
    ~count
    ~name:
      "[check_manager_restriction {pkh} pkh fee gas_limit] returns [`Fail \
       `Branch_delayed _] if [fee] < [old_op_fee]"
    Generators.filter_state_with_operation_gen
    (fun (filter_state, (op_info, pkh)) ->
      let config = config None in
      let filter_state =
        {
          filter_state with
          op_prechecked_managers =
            Signature.Public_key_hash.Map.add
              pkh
              {op_info with fee = Alpha_context.Tez.one}
              (* We force higher fee than below: [one > zero]. *)
              filter_state.op_prechecked_managers;
        }
      in
      match
        check_manager_restriction
          config
          filter_state
          pkh
          ~fee:Alpha_context.Tez.zero
          ~gas_limit:Alpha_context.Gas.Arith.zero
      with
      | `Fail (`Branch_delayed _) -> true
      | `Fresh ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a should be in the set of precheck managers:@,\
             %a@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state
      | `Replace old_oph ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a is in the set of precheck managers:@,\
             %a@,\
             but should not replace the old operation:%a@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state
            Operation_hash.pp
            old_oph)

(** Test that [check_manager_restriction] returns [Replace] when we use a known
    [pkh] already associated with an operation that has a lower fees. *)
let test_check_manager_restriction_replace =
  let open QCheck2 in
  Test.make
    ~count
    ~name:
      "[check_manager_restriction {pkh} pkh fee gas_limit] returns [`Replace \
       _] if [fee] >= [old_op_fee]"
    Generators.filter_state_with_operation_gen
    (fun (filter_state, (op_info, pkh)) ->
      let config = config None in
      let fee = Alpha_context.Tez.zero in
      let filter_state =
        {
          filter_state with
          op_prechecked_managers =
            Signature.Public_key_hash.Map.add
              pkh
              op_info
              filter_state.op_prechecked_managers;
        }
      in
      match
        check_manager_restriction
          config
          filter_state
          pkh
          ~fee
          ~gas_limit:Alpha_context.Gas.Arith.zero
      with
      | `Replace _ -> true
      | `Fresh ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a should be in the set of precheck managers:@,\
             %a@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state
      | `Fail (`Branch_delayed _) ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a is in the set of prechecked managers:@,\
             %a but the old version should have been replaced because the new \
             fees(%a) are higher@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state
            Alpha_context.Tez.pp
            fee)

(** Test that [add_manager_restriction] adds the [pkh] in the filter state. *)
let test_add_manager_restriction_set =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"[mem pkh (add_manager_restriction _ _ _ pkh).op_prechecked_managers]"
    (Gen.triple
       Generators.filter_state_with_operation_gen
       Generators.operation_hash_gen
       Gen.bool)
    (fun ((filter_state, (op_info, pkh)), oph_to_replace, replace) ->
      let replace =
        if replace then `Replace (oph_to_replace, ()) else `No_replace
      in
      let filter_state =
        add_manager_restriction
          filter_state
          op_info.operation_hash
          op_info
          pkh
          replace
      in
      qcheck_cond
        ~pp:(fun fmt set ->
          Format.fprintf
            fmt
            "%a was not found in prechecked_managers : %a"
            Signature.Public_key_hash.pp
            pkh
            pp_prechecked_managers
            set)
        ~cond:(Signature.Public_key_hash.Map.mem pkh)
        filter_state.op_prechecked_managers
        ())

(** Test that [add_manager_restriction (`Replace oph)] remove the [oph] from the
    filter state. *)
let test_add_manager_restriction_replace =
  let open QCheck2 in
  Test.make
    ~count
    ~name:
      "[mem oph (add_manager_restriction _ _ _ (`Replace \
       oph).op_prechecked_managers]"
    Generators.filter_state_with_operation_gen
    (fun (filter_state, (op_info, pkh)) ->
      match
        Operation_hash.Map.choose filter_state.operation_hash_to_manager
      with
      | None -> true
      | Some (oph, _) when Operation_hash.equal oph op_info.operation_hash ->
          true
      | Some (oph_to_replace, _) ->
          let filter_state =
            add_manager_restriction
              filter_state
              op_info.operation_hash
              op_info
              pkh
              (`Replace (oph_to_replace, ()))
          in
          qcheck_cond
            ~pp:(fun fmt map ->
              Format.fprintf
                fmt
                "%a was not found in prechecked_managers : %a"
                Operation_hash.pp
                oph_to_replace
                pp_operation_hash_manager
                map)
            ~cond:(fun map -> not (Operation_hash.Map.mem oph_to_replace map))
            filter_state.operation_hash_to_manager
            ())

(** Test that [check_manager_restriction pkh] returns [Fail | Replace] on a
    filter state returned by [add_manager_restriction pkh]. *)
let test_add_manager_restriction_check =
  let open QCheck2 in
  Test.make
    ~count
    ~name:
      "[check_manager_restriction filter_state pkh] returns [`Fail \
       (`Branch_delayed)_ | `Replace _]"
    (Gen.triple
       Generators.filter_state_with_operation_gen
       Generators.operation_hash_gen
       Gen.bool)
    (fun ((filter_state, (op_info, pkh)), oph_to_replace, replace) ->
      let replace =
        if replace then `Replace (oph_to_replace, ()) else `No_replace
      in
      let config = config None in
      let filter_state =
        add_manager_restriction
          filter_state
          op_info.operation_hash
          op_info
          pkh
          replace
      in
      match
        check_manager_restriction
          config
          filter_state
          pkh
          ~fee:Alpha_context.Tez.zero
          ~gas_limit:Alpha_context.Gas.Arith.zero
      with
      | `Fresh ->
          QCheck.Test.fail_reportf
            "@[<v 2>Check manager restriction failed!@,\
             %a should be in the set of precheck managers:@,\
             %a@]"
            Signature.Public_key_hash.pp
            pkh
            pp_state
            filter_state
      | `Fail (`Branch_delayed _) | `Replace _ -> true)

(** Test that [remove pkh] removes the key from the filter state returned
    by [add_manager_restriction pkh]. *)
let test_remove =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"[removing an existing operation hash works]"
    (Gen.triple
       Generators.filter_state_with_operation_gen
       Generators.operation_hash_gen
       Gen.bool)
    (fun ((filter_state, (op_info, pkh)), oph_to_replace, replace) ->
      let replace =
        if replace then `Replace (oph_to_replace, ()) else `No_replace
      in
      let filter_state =
        add_manager_restriction
          filter_state
          op_info.operation_hash
          op_info
          pkh
          replace
      in
      let actual_state = remove ~filter_state op_info.operation_hash in
      let expected_prechecked_managers =
        Signature.Public_key_hash.Map.remove
          pkh
          filter_state.op_prechecked_managers
      in
      qcheck_eq
        ~pp:pp_prechecked_managers
        ~eq:eq_prechecked_managers
        expected_prechecked_managers
        actual_state.op_prechecked_managers)

(** Test that [remove pkh] leaves the filter state intact if [pkh] is unknown. *)
let test_remove_unknown =
  let open QCheck2 in
  Test.make
    ~count
    ~name:"[removing an unknown operation hash leaves the filter state intact"
    Generators.filter_state_with_operation_gen
    (fun (filter_state, (op_info, _pkh)) ->
      let filter_state =
        {
          filter_state with
          operation_hash_to_manager =
            Operation_hash.Map.remove
              op_info.operation_hash
              filter_state.operation_hash_to_manager;
        }
      in
      let actual_state = remove ~filter_state op_info.operation_hash in
      qcheck_eq ~pp:pp_state ~eq:eq_state filter_state actual_state)

let () =
  Alcotest.run
    "Filter_state"
    [
      ( "check_manager_restriction",
        qcheck_wrap
          [
            test_check_manager_restriction_fresh;
            test_check_manager_restriction_replace;
            test_check_manager_restriction_fail;
          ] );
      ( "add_manager_restriction",
        qcheck_wrap
          [
            test_add_manager_restriction_set;
            test_add_manager_restriction_check;
            test_add_manager_restriction_replace;
          ] );
      ("remove", qcheck_wrap [test_remove; test_remove_unknown]);
    ]
