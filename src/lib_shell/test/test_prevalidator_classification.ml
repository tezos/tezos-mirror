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
    Component:    Shell (Prevalidator classification)
    Invocation:   dune build @src/lib_shell/test/runtest
    Subject:      Unit tests the Prevalidator classification APIs
*)

open Lib_test.Qcheck_helpers

module Operation_map = struct
  let pp ppf map =
    Format.fprintf
      ppf
      "[%a]"
      (Format.pp_print_list (fun ppf (oph, (op, _tztrace)) ->
           Format.fprintf
             ppf
             "(%a: (%a, <tztrace>))"
             Operation_hash.pp
             oph
             Operation.pp
             op))
      (Operation_hash.Map.bindings map)

  (* Uses polymorphic equality on tztraces! *)
  let eq =
    Operation_hash.Map.equal (fun (o1, t1) (o2, t2) ->
        Operation.equal o1 o2 && t1 = t2)
end

module Generators = struct
  open Prevalidator_classification

  let string_gen = QCheck.Gen.string ?gen:None

  let operation_hash_gen : Operation_hash.t QCheck.Gen.t =
    let open QCheck.Gen in
    let+ key = opt (string_size (0 -- 64))
    and+ path = list_size (0 -- 100) string_gen in
    Operation_hash.hash_string ?key path

  let block_hash_gen : Block_hash.t QCheck.Gen.t =
    let open QCheck.Gen in
    let+ key = opt (string_size (0 -- 64))
    and+ path = list_size (0 -- 100) string_gen in
    Block_hash.hash_string ?key path

  (** Operations don't contain "valid" proto bytes but we don't care
   *  as far as [Prevalidator_classification] is concerned. *)
  let operation_gen : Operation.t QCheck.Gen.t =
    let open QCheck.Gen in
    let+ branch = block_hash_gen
    and+ proto = string_gen >|= Bytes.unsafe_of_string in
    Operation.{shell = {branch}; proto}

  (** Do we need richer errors? If so, how to generate those? *)
  let classification_gen : classification QCheck.Gen.t =
    QCheck.Gen.oneofl
      [`Applied; `Branch_delayed []; `Branch_refused []; `Refused []]

  let unrefused_classification_gen : classification QCheck.Gen.t =
    QCheck.Gen.oneofl [`Applied; `Branch_delayed []; `Branch_refused []]

  let parameters_gen : parameters QCheck.Gen.t =
    let open QCheck.Gen in
    let+ map_size_limit = 1 -- 100 in
    let on_discarded_operation _ = () in
    {map_size_limit; on_discarded_operation}

  let t_gen : t QCheck.Gen.t =
    let open QCheck.Gen in
    let* parameters = parameters_gen in
    let+ inputs =
      list_size
        (0 -- parameters.map_size_limit)
        (triple classification_gen operation_hash_gen operation_gen)
    in
    let t = Prevalidator_classification.create parameters in
    List.iter
      (fun (classification, operation_hash, operation) ->
        Prevalidator_classification.add
          ~notify:(fun () -> ())
          classification
          operation_hash
          operation
          t)
      inputs ;
    t
end

let qcheck_eq_true ~actual =
  let _ = qcheck_eq' ~pp:Format.pp_print_bool ~expected:true ~actual () in
  ()

let qcheck_eq_false ~actual =
  let _ = qcheck_eq' ~pp:Format.pp_print_bool ~expected:false ~actual () in
  ()

let test_clear_empties_all =
  let open QCheck in
  Test.make
    ~name:
      "[clear ~handle_branch_refused:true] empties everything except [refused]"
    (make Generators.t_gen)
  @@ fun t ->
  let refused_before = t.refused |> Prevalidator_classification.map in
  Prevalidator_classification.clear ~handle_branch_refused:true t ;
  let refused_after = t.refused |> Prevalidator_classification.map in
  let bounded_map_is_empty bounded_map =
    bounded_map |> Prevalidator_classification.map
    |> Operation_hash.Map.is_empty
  in
  qcheck_eq_true ~actual:(bounded_map_is_empty t.branch_refused) ;
  qcheck_eq_true ~actual:(bounded_map_is_empty t.branch_delayed) ;
  qcheck_eq_true ~actual:(t.applied_rev = []) ;
  qcheck_eq_true ~actual:(Operation_hash.Set.is_empty t.in_mempool) ;
  qcheck_eq'
    ~pp:Operation_map.pp
    ~eq:Operation_map.eq
    ~expected:refused_before
    ~actual:refused_after
    ()

let test_is_in_mempool_remove =
  let open QCheck in
  Test.make
    ~name:"[is_in_mempool] and [remove_*] are well-behaved"
    (make
    @@ Generators.(
         Gen.quad
           t_gen
           unrefused_classification_gen
           operation_hash_gen
           operation_gen))
  @@ fun (t, unrefused_classification, oph, op) ->
  Prevalidator_classification.add
    ~notify:(fun () -> ())
    unrefused_classification
    oph
    op
    t ;
  qcheck_eq_true ~actual:(Prevalidator_classification.is_in_mempool oph t) ;
  Prevalidator_classification.remove oph t ;
  qcheck_eq_false ~actual:(Prevalidator_classification.is_in_mempool oph t) ;
  true

let test_is_applied =
  let open QCheck in
  Test.make
    ~name:"[is_applied] is well-behaved"
    (make @@ Generators.(Gen.triple t_gen operation_hash_gen operation_gen))
  @@ fun (t, oph, op) ->
  Prevalidator_classification.add ~notify:(fun () -> ()) `Applied oph op t ;
  qcheck_eq_true ~actual:(Prevalidator_classification.is_applied oph t) ;
  qcheck_eq_true ~actual:(Prevalidator_classification.is_in_mempool oph t) ;
  Prevalidator_classification.remove oph t ;
  qcheck_eq_false ~actual:(Prevalidator_classification.is_applied oph t) ;
  qcheck_eq_false ~actual:(Prevalidator_classification.is_in_mempool oph t) ;
  true

let test_validation_result =
  let open QCheck in
  Test.make ~name:"[validation_result] is well-behaved" (make Generators.t_gen)
  @@ fun t ->
  let Preapply_result.{applied; refused; branch_refused; branch_delayed} =
    Prevalidator_classification.validation_result t
  in
  let _ =
    qcheck_eq'
      ~eq:
        (List.equal (fun (oph1, op1) (oph2, op2) ->
             Operation_hash.equal oph1 oph2 && Operation.equal op1 op2))
      ~expected:(List.rev t.applied_rev)
      ~actual:applied
      ()
  in
  let qcheck_eq_operation_hash_map ~expected ~actual =
    let _ =
      qcheck_eq'
        ~eq:
          (Operation_hash.Map.equal (fun (op1, _errors1) (op2, _errors2) ->
               Operation.equal op1 op2))
        ~expected
        ~actual
        ()
    in
    ()
  in
  qcheck_eq_operation_hash_map
    ~expected:Operation_hash.Map.empty
    ~actual:refused ;
  qcheck_eq_operation_hash_map
    ~expected:(Prevalidator_classification.map t.branch_refused)
    ~actual:branch_refused ;
  qcheck_eq_operation_hash_map
    ~expected:(Prevalidator_classification.map t.branch_delayed)
    ~actual:branch_delayed ;
  true

module Bounded = struct
  type binding = Operation_hash.t * Operation.t

  type custom =
    Prevalidator_classification.t
    * [ `Branch_delayed of tztrace
      | `Branch_refused of tztrace
      | `Refused of tztrace ]
    * binding list
    * binding list

  let custom_print : custom QCheck.Print.t =
   fun (t, classification, first_bindings, other_bindings) ->
    let classification_string =
      match classification with
      | `Branch_delayed _ -> "Branch_delayed <tztrace>"
      | `Branch_refused _ -> "Branch_refused <tztrace>"
      | `Refused _ -> "Refused <tztrace>"
    in
    let binding_pp ppf bindings =
      bindings
      |> List.map (fun (key, _value) -> key)
      |> Format.pp_print_list Operation_hash.pp ppf
    in
    Format.asprintf
      "Prevalidator_classification.t:@.%a@.Classification:@.%s@.First \
       bindings:@.%a@.Other bindings:@.%a"
      Prevalidator_classification.Internal_for_tests.pp
      t
      classification_string
      binding_pp
      first_bindings
      binding_pp
      other_bindings

  let custom_gen (discarded_operations_rev : Operation_hash.t list ref) :
      custom QCheck.Gen.t =
    let open QCheck.Gen in
    let* map_size_limit = 1 -- 20 in
    let on_discarded_operation oph =
      discarded_operations_rev := oph :: !discarded_operations_rev
    in
    let parameters =
      Prevalidator_classification.{map_size_limit; on_discarded_operation}
    in
    let* inputs =
      list_size
        (0 -- map_size_limit)
        Generators.(triple classification_gen operation_hash_gen operation_gen)
    in
    let t = Prevalidator_classification.create parameters in
    List.iter
      (fun (classification, operation_hash, operation) ->
        Prevalidator_classification.add
          ~notify:(fun () -> ())
          classification
          operation_hash
          operation
          t)
      inputs ;
    let+ error_classification =
      oneofl [`Branch_delayed []; `Branch_refused []; `Refused []]
    and+ first_bindings =
      list_size (1 -- 10) Generators.(pair operation_hash_gen operation_gen)
    and+ other_bindings =
      list_repeat
        map_size_limit
        Generators.(pair operation_hash_gen operation_gen)
    in
    (t, error_classification, first_bindings, other_bindings)

  let add_bindings bindings classification t =
    List.iter
      (fun (oph, op) ->
        Prevalidator_classification.add
          ~notify:(fun () -> ())
          classification
          oph
          op
          t)
      bindings

  let check_discarded_contains_bindings ~discarded_hashes ~bindings =
    let excess_hashes = bindings |> List.map (fun (oph, _op) -> oph) in
    if
      not
        (List.for_all
           (fun excess_hash ->
             List.mem ~equal:Operation_hash.equal excess_hash discarded_hashes)
           excess_hashes)
    then
      let hashes_pp = Format.pp_print_list Operation_hash.pp in
      QCheck.Test.fail_reportf
        "Expected all excess hashes to have been discarded but it was \
         not.@.Excess hashes:@.%a@.Discarded hashes:@.%a"
        hashes_pp
        excess_hashes
        hashes_pp
        discarded_hashes

  let check_map_is_full ~expected_size ~bounded_map =
    if
      not
        (List.length
           (Operation_hash.Map.bindings
              (Prevalidator_classification.map bounded_map))
        = expected_size)
    then
      QCheck.Test.fail_reportf
        "Expected bounded_map to be full (size = %i) but its actual size is \
         %i.@.Bounded_map content:@.%a"
        expected_size
        (List.length
           (Operation_hash.Map.bindings
              (Prevalidator_classification.map bounded_map)))
        Prevalidator_classification.Internal_for_tests.bounded_map_pp
        bounded_map

  let test_bounded =
    let discarded_operations_rev = ref [] in
    QCheck.Test.make
      ~name:
        "When more error operations than the size limit are added, then the \
         first operations are discarded"
      (QCheck.make ~print:custom_print @@ custom_gen discarded_operations_rev)
    @@ fun (t, error_classification, first_bindings, other_bindings) ->
    (* We must not have duplicate operation hashes otherwise we may not go over the bound *)
    let hashes =
      first_bindings @ other_bindings |> List.map (fun (hash, _) -> hash)
    in
    let unique_hashes = Operation_hash.Set.of_list hashes in
    QCheck.assume
      (Operation_hash.Set.cardinal unique_hashes = List.length hashes) ;
    (* Remove all operations for the tested classification *)
    let bounded_map =
      match error_classification with
      | `Branch_delayed _ -> t.branch_delayed
      | `Branch_refused _ -> t.branch_refused
      | `Refused _ -> t.refused
    in
    let () =
      Operation_hash.Map.iter
        (fun oph _op -> Prevalidator_classification.remove oph t)
        (Prevalidator_classification.map bounded_map)
    in
    discarded_operations_rev := [] ;
    (* Add the first bindings (the ones that will get discarded once the other bindings are added) *)
    add_bindings
      first_bindings
      (error_classification :> Prevalidator_classification.classification)
      t ;
    (* Now add the other bindings that should cause the first ones to get discarded *)
    add_bindings
      other_bindings
      (error_classification :> Prevalidator_classification.classification)
      t ;
    (* [add] calls [on_discarded_operation] when adding any [Refused] operation,
     * so the recorded discarded operations is a superset of the [first_bindings] ones. *)
    check_discarded_contains_bindings
      ~discarded_hashes:(!discarded_operations_rev |> List.rev)
      ~bindings:first_bindings ;
    check_map_is_full ~expected_size:t.parameters.map_size_limit ~bounded_map ;
    true
end

let () =
  Alcotest.run
    "Prevalidator_classification"
    [
      ( "",
        qcheck_wrap
          [
            test_clear_empties_all;
            test_is_in_mempool_remove;
            test_is_applied;
            test_validation_result;
            Bounded.test_bounded;
          ] );
    ]
