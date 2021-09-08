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

let test_clear_empties_all_except_branch_refused =
  let open QCheck in
  Test.make
    ~name:
      "[clear ~handle_branch_refused:false] empties everything except \
       [refused] and [branch_refused]"
    (make Generators.t_gen)
  @@ fun t ->
  let refused_before = t.refused |> Prevalidator_classification.map in
  let branch_refused_before =
    t.branch_refused |> Prevalidator_classification.map
  in
  Prevalidator_classification.clear ~handle_branch_refused:false t ;
  let refused_after = t.refused |> Prevalidator_classification.map in
  let branch_refused_after =
    t.branch_refused |> Prevalidator_classification.map
  in
  let bounded_map_is_empty bounded_map =
    bounded_map |> Prevalidator_classification.map
    |> Operation_hash.Map.is_empty
  in
  let _ =
    qcheck_eq'
      ~pp:Operation_map.pp
      ~eq:Operation_map.eq
      ~expected:branch_refused_before
      ~actual:branch_refused_after
      ()
  in
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

(** Tests of [Prevalidator_classification.to_map] *)
module To_map = struct
  module Classification = Prevalidator_classification

  let map_pp fmt x =
    let map_to_list m = Operation_hash.Map.to_seq m |> List.of_seq in
    let pp_pair fmt (oph, op) =
      Format.fprintf fmt "%a:%a" Operation_hash.pp oph Operation.pp op
    in
    Format.fprintf fmt "%a" (Format.pp_print_list pp_pair) (map_to_list x)

  let map_eq = Operation_hash.Map.equal Operation.equal

  (** [remove_all m1 m2] returns the subset of [m1] thas is not within [m2].
      Said differently, [remove_all m1 m2] removes from [m1] all keys
      that are in [m2]. *)
  let remove_all m1 m2 =
    let keys2 =
      Operation_hash.Map.bindings m2
      |> List.map fst |> Operation_hash.Set.of_list
    in
    Operation_hash.Map.filter
      (fun key _val -> not (Operation_hash.Set.mem key keys2))
      m1

  (** [eq_mod_binding m1 (k, v_opt) m2] holds iff:

      - [m1] equals [m2], or
      - [v_opt] is [Some v] and the union of [m1] and [(k,v)] equals [m2], or
      - [v_opt] is [None] and the union of [m1] and [(k,v)] equals [m2],
        for some unknown value [v]. *)
  let eq_mod_binding m1 (k, v_opt) m2 =
    let diff = remove_all m2 m1 in
    match (Operation_hash.Map.bindings diff, v_opt) with
    | ([], _) -> true
    | ([(kdiff, vdiff)], Some v)
      when Operation_hash.equal kdiff k && Operation.equal v vdiff ->
        true
    | ([(kdiff, _)], None) when Operation_hash.equal kdiff k -> true
    | _ -> false

  (** [to_map_all] calls [Classification.to_map] with all named
      arguments set to [true] *)
  let to_map_all =
    Classification.to_map
      ~applied:true
      ~branch_delayed:true
      ~branch_refused:true
      ~refused:true

  (** Tests the relationship between [Classification.add]
      and [Classification.to_map] *)
  let test_add =
    QCheck.Test.make
      ~name:"[add] extends the size of [to_map] by 0 or 1"
      (QCheck.make
         (QCheck.Gen.quad
            Generators.t_gen
            Generators.classification_gen
            Generators.operation_hash_gen
            Generators.operation_gen))
    @@ fun (t, classification, oph, op) ->
    let initial = to_map_all t in
    Classification.add ~notify:(Fun.const ()) classification oph op t ;
    (* We need to use [eq_mod_binding] because it covers the two possible cases:
       if [oph] is not in [initial], we have [initial @@ [(oph, op)] = to_map_all t]
       if [oph] is in [initial] already, we have [initial = to_map_all t] *)
    qcheck_eq'
      ~expected:true
      ~actual:(eq_mod_binding initial (oph, Some op) (to_map_all t))
      ()

  (** Tests the relationship between [Classification.remove]
      and [Classification.to_map] *)
  let test_remove =
    QCheck.Test.make
      ~name:"[remove] reduces the size of [to_map] by 0 or 1"
      (QCheck.make
         (QCheck.Gen.pair Generators.t_gen Generators.operation_hash_gen))
    @@ fun (t, oph) ->
    let initial = to_map_all t in
    Classification.remove oph t ;
    (* We need to use [eq_mod_binding] because it covers the two possible cases:
       if [oph] is not in [initial], we have [initial = to_map_all t]
       if [oph] is in [initial], we have [initial = to_map_all t @@ [(oph, op)] ] *)
    qcheck_eq'
      ~expected:true
      ~actual:(eq_mod_binding (to_map_all t) (oph, None) initial)
      ()

  let test_map_remove_add =
    (* Property checked:

       - \forall t oph class, C.to_map (C.remove t oph) + oph =
       C.to_map (C.add t oph class)

       where (+)/(-) are add/remove over maps. *)
    QCheck.Test.make
      ~name:"Check property between map, remove and add (1)"
      (QCheck.make
         (QCheck.Gen.quad
            Generators.t_gen
            Generators.classification_gen
            Generators.operation_hash_gen
            Generators.operation_gen))
    @@ fun (t, classification, oph, op) ->
    let t' = Classification.Internal_for_tests.copy t in
    Classification.remove oph t ;
    let initial = to_map_all t in
    let left = Operation_hash.Map.add oph op initial in
    Classification.add ~notify:(Fun.const ()) classification oph op t' ;
    let right = to_map_all t' in
    qcheck_eq'
      ~expected:left
      ~actual:right
      ~eq:(Operation_hash.Map.equal Operation.equal)
      ()

  let test_map_add_remove =
    (* Property checked:

       - \forall t oph class, C.to_map (C.add t oph class) - oph =
       C.to_map (C.remove t oph)

       where (+)/(-) are add/remove over maps. *)
    QCheck.Test.make
      ~name:"Check property between map, remove and add (2)"
      (QCheck.make
         (QCheck.Gen.quad
            Generators.t_gen
            Generators.classification_gen
            Generators.operation_hash_gen
            Generators.operation_gen))
    @@ fun (t, classification, oph, op) ->
    let t' = Classification.Internal_for_tests.copy t in
    Classification.add ~notify:(Fun.const ()) classification oph op t ;
    let initial = to_map_all t in
    let left = Operation_hash.Map.remove oph initial in
    Classification.remove oph t' ;
    let right = to_map_all t' in
    qcheck_eq'
      ~expected:left
      ~actual:right
      ~eq:(Operation_hash.Map.equal Operation.equal)
      ()

  (** Tests the relationship between [Classification.clear]
      and [Classification.to_map] *)
  let test_clear =
    QCheck.Test.make
      ~name:"[clear] can be emulated by [to_map ~refused:true ..]"
      (QCheck.make (QCheck.Gen.pair Generators.t_gen QCheck.Gen.bool))
    @@ fun (t, handle_branch_refused) ->
    let initial =
      Classification.to_map
        ~applied:false
        ~branch_delayed:false
        ~branch_refused:(not handle_branch_refused)
        ~refused:true
        t
    in
    Classification.clear ~handle_branch_refused t ;
    let cleared = to_map_all t in
    qcheck_eq' ~pp:map_pp ~eq:map_eq ~expected:initial ~actual:cleared ()

  (** Tests the relationship between [Classification.is_applied]
      and [Classification.to_map] *)
  let test_is_applied =
    QCheck.Test.make
      ~name:"[is_applied] can be emulated by [to_map ~applied:true]"
      (QCheck.make
         (QCheck.Gen.pair Generators.t_gen Generators.operation_hash_gen))
    @@ fun (t, oph) ->
    let is_applied = Classification.is_applied oph t in
    let map =
      Classification.to_map
        ~applied:true
        ~branch_delayed:false
        ~branch_refused:false
        ~refused:false
        t
      |> Operation_hash.Map.filter (fun oph' _val -> oph' = oph)
    in
    qcheck_eq'
      ~expected:is_applied
      ~actual:(Operation_hash.Map.cardinal map = 1)
      ()

  (** Tests the relationship between [Classification.is_in_mempool]
      and [Classification.to_map] *)
  let test_is_in_mempool =
    QCheck.Test.make
      ~name:"[is_in_mempool] can be emulated by [to_map]"
      (QCheck.make
         (QCheck.Gen.pair Generators.t_gen Generators.operation_hash_gen))
    @@ fun (t, oph) ->
    let is_in_mempool = Classification.is_in_mempool oph t in
    let map =
      to_map_all t |> Operation_hash.Map.filter (fun oph' _ -> oph' = oph)
    in
    qcheck_eq'
      ~expected:is_in_mempool
      ~actual:(Operation_hash.Map.cardinal map = 1)
      ()
end

let () =
  Alcotest.run
    "Prevalidator_classification"
    [
      ( "",
        qcheck_wrap
          [
            test_clear_empties_all;
            test_clear_empties_all_except_branch_refused;
            test_is_in_mempool_remove;
            test_is_applied;
            Bounded.test_bounded;
            To_map.test_add;
            To_map.test_remove;
            To_map.test_map_remove_add;
            To_map.test_map_add_remove;
            To_map.test_clear;
            To_map.test_is_in_mempool;
          ] );
    ]
