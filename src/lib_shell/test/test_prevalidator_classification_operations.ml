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
    Component:    Shell (Prevalidator)
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_prevalidator_classification_operations.ml
    Subject:      Unit tests [Prevalidator_classification.Internal_for_tests.handle_live_operations]
                  and [Prevalidator_classification.recyle_operations]
*)

open Qcheck2_helpers
open Shell_operation
module Op_map = Operation_hash.Map
module Classification = Prevalidator_classification
module Tree = Generators_tree.Tree
module List_extra = Generators_tree.List_extra
module Block = Generators_tree.Block

let parse hash raw = Some (Internal_for_tests.make_operation hash raw ())

(** Function to unwrap an [option] when it MUST be a [Some] *)
let force_opt ~loc = function
  | Some x -> x
  | None -> QCheck2.Test.fail_reportf "Unexpected None at %s" loc

(* Values from [start] (included) to [ancestor] (excluded) *)
let values_from_to ~(equal : 'a -> 'a -> bool) (tree : 'a Tree.tree)
    (start : 'a) (ancestor : 'a) : 'a list =
  Tree.predecessors ~equal tree start
  |> List_extra.take_until_if_found ~pred:(( = ) ancestor)
  |> force_opt ~loc:__LOC__
  |> fun preds -> start :: preds

(** Pretty print values of type [Operation_hash.Set.t] *)
let op_set_pp fmt x =
  let set_to_list m = Operation_hash.Set.to_seq m |> List.of_seq in
  Format.fprintf
    fmt
    "%a"
    (Format.pp_print_list Operation_hash.pp)
    (set_to_list x)

(** Pretty print values of type [Operation.t Operation_hash.Map] *)
let op_map_pp fmt x =
  let pp_pair fmt (hash, op) =
    Format.fprintf fmt "%a:%a" Operation_hash.pp hash Operation.pp op.raw
  in
  Format.fprintf
    fmt
    "%a"
    (Format.pp_print_list pp_pair)
    (Operation_hash.Map.bindings x)

let qcheck_cond ?pp ~cond e1 e2 () =
  if cond e1 e2 then true
  else
    match pp with
    | None ->
        QCheck2.Test.fail_reportf
          "@[<h 0>The condition check failed, but no pretty printer was \
           provided.@]"
    | Some pp ->
        QCheck2.Test.fail_reportf
          "@[<v 2>The condition check failed!@,\
           first element:@,\
           %a@,\
           second element:@,\
           %a@]"
          pp
          e1
          pp
          e2

let blocks_to_oph_set (blocks : Operation_hash.t list list list) :
    Operation_hash.Set.t =
  List.concat blocks |> List.concat |> Operation_hash.Set.of_list

(** [is_subset m1 m2] returns whether all bindings of [m1] are in [m2].
    In other words, it returns whether [m2] is a superset of [m1]. *)
let is_subset (m1 : unit operation Op_map.t) (m2 : unit operation Op_map.t) =
  let rec go (m1_seq : (Operation_hash.t * unit operation) Seq.t) =
    match m1_seq () with
    | Seq.Nil -> true
    | Seq.Cons ((m1_key, m1_value), m1_rest) -> (
        match Op_map.find m1_key m2 with
        | None -> (* A key in [m1] that is not in [m2] *) false
        | Some m2_value ->
            Operation_hash.equal m1_value.hash m2_value.hash && go m1_rest)
  in
  go (Op_map.to_seq m1)

module Handle_operations = struct
  (* This is used only if operations are not parsable*)
  let dummy_classes =
    Classification.(
      create {map_size_limit = 1; on_discarded_operation = (fun _oph -> ())})

  (** Test that operations returned by [handle_live_operations]
      are all in the alive branch. *)
  let test_handle_live_operations_is_branch_alive =
    (* Like [Generators.chain_tools_gen], but also picks a random subset of
       blocks from the tree to pass an interesting value to [is_branch_alive].
       Could be in [chain_tools_gen] itself, but only used in this test. So
       it would be overkill. *)
    let gen =
      let open QCheck2.Gen in
      let* tree, pair_blocks_opt, old_mempool =
        Generators_tree.tree_gen ?blocks:None ()
      in
      let* live_blocks = sublist (Tree.values tree) in
      let live_blocks =
        List.map (fun (blk : Block.t) -> blk.bhash) live_blocks
      in
      return
        (tree, pair_blocks_opt, old_mempool, Block_hash.Set.of_list live_blocks)
    in
    QCheck2.Test.make
      ~name:"[handle_live_operations] is a subset of alive blocks"
      gen
    @@ fun (tree, pair_blocks_opt, old_mempool, live_blocks) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let expected_superset : unit operation Op_map.t =
      (* Take all blocks *)
      Tree.values tree
      (* Keep only the ones in live_blocks *)
      |> List.to_seq
      |> Seq.filter (fun (blk : Block.t) ->
             Block_hash.Set.mem blk.bhash live_blocks)
      (* Then extract (oph, op) pairs from them *)
      |> Seq.flat_map (fun (blk : Block.t) -> List.to_seq blk.operations)
      |> Seq.flat_map List.to_seq
      |> Seq.map (fun op -> (op.hash, op))
      |> Op_map.of_seq
    in
    let actual : unit operation Op_map.t =
      Classification.Internal_for_tests.handle_live_operations
        ~classes:dummy_classes
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~is_branch_alive:(fun blk_hash ->
          Block_hash.Set.mem blk_hash live_blocks)
        ~parse
        old_mempool
      |> Lwt_main.run
    in
    qcheck_cond ~pp:op_map_pp ~cond:is_subset actual expected_superset ()

  (** Test that operations returned by [handle_live_operations] is
      the union of 1/ operations from its last argument (a map) and 2/
      operations on the "path" between [from_branch] and [to_branch] (when
      all blocks are considered live). *)
  let test_handle_live_operations_path_spec =
    QCheck2.Test.make
      ~name:"[handle_live_operations] path specification"
      (Generators_tree.tree_gen ())
    @@ fun (tree, pair_blocks_opt, _) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let equal = Block.equal in
    let ancestor : Block.t =
      Tree.find_ancestor ~equal tree from_branch to_branch
      |> force_opt ~loc:__LOC__
    in
    (* Operations from [start] (included) to [ancestor] (excluded).
       [ancestor] should be the ancestor of [start]. *)
    let operations_on_path start ancestor =
      List.map
        Block.tools.all_operation_hashes
        (values_from_to ~equal tree start ancestor)
      |> blocks_to_oph_set
    in
    let expected_superset = operations_on_path from_branch ancestor in
    let from_ancestor_to_to_branch = operations_on_path to_branch ancestor in
    (* Expected operations are the ones from [ancestor] to [from_branch],
       minus the ones from ancestor to [to_branch]. *)
    let expected =
      Operation_hash.Set.diff expected_superset from_ancestor_to_to_branch
    in
    let actual =
      Classification.Internal_for_tests.handle_live_operations
        ~classes:dummy_classes
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~is_branch_alive:(Fun.const true)
        ~parse
        Operation_hash.Map.empty
      |> Lwt_main.run |> Op_map.bindings |> List.map fst
      |> Operation_hash.Set.of_list
    in
    qcheck_eq' ~pp:op_set_pp ~eq:Operation_hash.Set.equal ~expected ~actual ()

  (** Test that operations cleared by [handle_live_operations]
      are operations on the path from [ancestor] to [to_branch] (when all
      operations are deemed up-to-date). *)
  let test_handle_live_operations_clear =
    QCheck2.Test.make
      ~name:"[handle_live_operations] clear approximation"
      Generators_tree.(tree_gen ())
    @@ fun (tree, pair_blocks_opt, old_mempool) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let cleared = ref Operation_hash.Set.empty in
    let clearer oph = cleared := Operation_hash.Set.add oph !cleared in
    let chain = {chain with clear_or_cancel = clearer} in
    let equal = Block.equal in
    let ancestor : Block.t =
      Tree.find_ancestor ~equal tree from_branch to_branch
      |> force_opt ~loc:__LOC__
    in
    let expected_superset =
      List.map
        Block.tools.all_operation_hashes
        (values_from_to ~equal tree to_branch ancestor)
      |> blocks_to_oph_set
    in
    Classification.Internal_for_tests.handle_live_operations
      ~classes:dummy_classes
      ~block_store:Block.tools
      ~chain
      ~from_branch
      ~to_branch
      ~is_branch_alive:(Fun.const true)
      ~parse
      old_mempool
    |> Lwt_main.run |> ignore ;
    qcheck_cond
      ~pp:op_set_pp
      ~cond:Operation_hash.Set.subset
      !cleared
      expected_superset
      ()

  (** Test that operations injected by [handle_live_operations]
      are operations on the path from [ancestor] to [from_branch]. *)
  let test_handle_live_operations_inject =
    QCheck2.Test.make
      ~name:"[handle_live_operations] inject approximation"
      (Generators_tree.tree_gen ())
    @@ fun (tree, pair_blocks_opt, old_mempool) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let injected = ref Operation_hash.Set.empty in
    let inject_operation oph _op =
      injected := Operation_hash.Set.add oph !injected ;
      Lwt.return_unit
    in
    let chain = {chain with inject_operation} in
    let equal = Block.equal in
    let ancestor : Block.t =
      Tree.find_ancestor ~equal tree from_branch to_branch
      |> force_opt ~loc:__LOC__
    in
    let expected_superset =
      List.map
        Block.tools.all_operation_hashes
        (values_from_to ~equal tree from_branch ancestor)
      |> blocks_to_oph_set
    in
    Classification.Internal_for_tests.handle_live_operations
      ~classes:dummy_classes
      ~block_store:Block.tools
      ~chain
      ~from_branch
      ~to_branch
      ~is_branch_alive:(Fun.const true)
      ~parse
      old_mempool
    |> Lwt_main.run |> ignore ;
    qcheck_cond
      ~pp:op_set_pp
      ~cond:Operation_hash.Set.subset
      !injected
      expected_superset
      ()
end

module Recyle_operations = struct
  (** A generator of {!Classification.t} that uses
      the given operations and hashes. It is used in place
      of {!Prevalidator_generators.t_gen} because we need to
      control the operations and hashes used (because we want them
      to be distinct from the one in the tree of blocks). This
      generator generates classifications that contains all the
      given operations and hashes, spreading them among the different
      classes of {!Prevalidator_classification.t}. This generator is NOT
      a fully random generator like {!Prevalidator_generators.t_gen}. *)
  let classification_of_ops_gen (ops : unit operation Op_map.t) :
      unit Classification.t QCheck2.Gen.t =
    let open QCheck2.Gen in
    let ops = Operation_hash.Map.bindings ops |> List.map snd in
    let length = List.length ops in
    let* empty_space = 0 -- 100 in
    (* To avoid throwing part of [ops], we want the capacity of the classification
       to be equal or larger than [length], hence: *)
    let map_size_limit = length + empty_space in
    (* Because capacity must be > 0 in Ring.create: *)
    let map_size_limit = max 1 map_size_limit in
    let parameters : Classification.parameters =
      {map_size_limit; on_discarded_operation = Fun.const ()}
    in
    let* classes = list_repeat length Generators.classification_gen in
    assert (List.compare_length_with classes length = 0) ;
    let t = Prevalidator_classification.create parameters in
    List.iter
      (fun (classification, op) ->
        Generators.add_if_not_present classification op t)
      (List.combine_drop classes ops) ;
    return t

  (** Returns data to test {!Classification.recyle_operations}:
      - an instance of [block chain_tools]
      - the tree of blocks
      - a pair of blocks (that belong to the tree) and is
        fine for being passed as [(~from_branch, ~to_branch)]; i.e.
        the two blocks have a common ancestor.
      - a classification
      - a list of pending operations

      As in production, the following lists of operations are disjoint:
      operations in the blocks, classification, and pending list. Note
      that this is not a precondition of [recycle_operations], it's
      to test the typical use case. *)
  let gen =
    let open QCheck2.Gen in
    let* blocks = Generators_tree.unique_nonempty_block_gen in
    let blocks = Block.set_to_list blocks in
    assert (blocks <> []) ;
    let to_ops (blk : Block.t) = List.concat blk.operations in
    let oph_op_list_to_map l = List.to_seq l |> Op_map.of_seq in
    let blocks_ops =
      List.concat_map to_ops blocks
      |> List.map (fun op -> (op.hash, op))
      |> oph_op_list_to_map
    in
    let blocks_hashes = List.map Block.to_hash blocks in
    let block_hash_t =
      (* For classification and pending, put 50% of them in live_blocks.
         For the remaining 50%, generate branch randomly, so likely outside
         live_blocks. *)
      frequency [(1, Generators.block_hash_gen); (1, oneofl blocks_hashes)]
    in
    let* classification_pendings_ops =
      (* For classification and pending, we want operations that are NOT in
         the blocks already. Hence: *)
      Generators.op_map_gen ~block_hash_t ()
    in
    let classification_pendings_ops =
      Op_map.filter
        (fun oph _ -> not (Op_map.mem oph blocks_ops))
        classification_pendings_ops
    in
    let* classification_ops, pending_ops =
      Op_map.bindings classification_pendings_ops
      |> Generators_tree.split_in_two
    in
    let classification_ops = oph_op_list_to_map classification_ops in
    let pending_ops = oph_op_list_to_map pending_ops in
    let* tree, from_to, _ = Generators_tree.tree_gen ~blocks () in
    let+ classification = classification_of_ops_gen classification_ops in
    (tree, from_to, classification, pending_ops)

  (** Test that {!Classification.recycle_operations} returns an empty map when
      live blocks are empty.

      We do not lift the test
      {!Handle_operations.test_handle_live_operations_is_branch_alive}
      to [recycle_operations] (checking that operations returned by
      [recycle_operations] are all in [live_blocks]), because we have
      to account for operations in [classification] and [pending], and
      we don't have the assumption that their branch are disjoint from
      each other and from branches in [tree] (because generation
      is partly random for them). This makes lifting
      the [handle_operations] test quite heavy. We don't do that. *)
  let test_recycle_operations_empty_live_blocks =
    let open QCheck2 in
    Test.make
      ~name:"[recycle_operations ~live_blocks:empty] is empty"
      Gen.(pair gen bool)
    @@ fun ((tree, pair_blocks_opt, classes, pending), handle_branch_refused) ->
    assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let actual : unit operation Op_map.t =
      Classification.recycle_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~live_blocks:Block_hash.Set.empty
        ~classes
        ~pending
        ~handle_branch_refused
        ~parse
      |> Lwt_main.run
    in
    qcheck_eq' ~pp:op_map_pp ~actual ~expected:Op_map.empty ()

  (** Test that the value returned by {!Classification.recycle_operations}
      can be approximated by unioning the sets of values:
      - returned by {!Classification.Internal_for_tests.handle_live_operations}
      - classified in the classification data structure
      - sent as [pending]. *)
  let test_recycle_operations_returned_value_spec =
    QCheck2.Test.make
      ~name:"[recycle_operations] returned value can be approximated"
      QCheck2.Gen.(pair gen bool)
    @@ fun ((tree, pair_blocks_opt, classes, pending), handle_branch_refused) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let equal = Block.equal in
    let ancestor : Block.t =
      Tree.find_ancestor ~equal tree from_branch to_branch
      |> force_opt ~loc:__LOC__
    in
    let live_blocks : Block_hash.Set.t =
      Tree.values tree |> List.map Block.to_hash |> Block_hash.Set.of_list
    in
    (* This is inherited from the behavior of [handle_live_operations] *)
    let expected_from_tree : Operation_hash.Set.t =
      List.map
        Block.tools.all_operation_hashes
        (values_from_to ~equal tree from_branch ancestor)
      |> blocks_to_oph_set
    in
    (* This is coming from [recycle_operations] itself *)
    let op_map_to_hash_list (m : 'a Operation_hash.Map.t) =
      Op_map.bindings m |> List.map fst |> Operation_hash.Set.of_list
    in
    let expected_from_classification =
      Classification.Internal_for_tests.to_map
        ~validated:true
        ~branch_delayed:true
        ~branch_refused:handle_branch_refused
        ~refused:false
        ~outdated:false
        classes
      |> op_map_to_hash_list
    in
    let expected_from_pending = op_map_to_hash_list pending in
    let expected_superset : Operation_hash.Set.t =
      Operation_hash.Set.union
        (Operation_hash.Set.union
           expected_from_tree
           expected_from_classification)
        expected_from_pending
    in
    let actual : Operation_hash.Set.t =
      Classification.recycle_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~live_blocks
        ~classes
        ~pending
        ~handle_branch_refused
        ~parse
      |> Lwt_main.run |> Op_map.bindings |> List.map fst
      |> Operation_hash.Set.of_list
    in
    qcheck_cond
      ~pp:op_set_pp
      ~cond:Operation_hash.Set.subset
      actual
      expected_superset
      ()

  (** Test that the classification is appropriately trimmed
      by {!Classification.recycle_operations} *)
  let test_recycle_operations_classification =
    QCheck2.Test.make
      ~name:"[recycle_operations] correctly trims its input classification"
      QCheck2.Gen.(pair gen bool)
    @@ fun ((tree, pair_blocks_opt, classes, pending), handle_branch_refused) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let live_blocks : Block_hash.Set.t =
      Tree.values tree |> List.map Block.to_hash |> Block_hash.Set.of_list
    in
    let expected : unit operation Op_map.t =
      Classification.Internal_for_tests.to_map
        ~validated:false
        ~branch_delayed:false
        ~branch_refused:(not handle_branch_refused)
        ~refused:true
        ~outdated:true
        classes
    in
    let from_branch, to_branch = force_opt ~loc:__LOC__ pair_blocks_opt in
    let chain = Generators_tree.classification_chain_tools tree in
    let () =
      Classification.recycle_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~live_blocks
        ~classes
        ~pending
        ~handle_branch_refused
        ~parse
      |> Lwt_main.run |> ignore
    in
    let actual =
      Classification.Internal_for_tests.to_map
        ~validated:true
        ~branch_delayed:true
        ~branch_refused:true
        ~refused:true
        ~outdated:true
        classes
    in
    qcheck_eq' ~pp:op_map_pp ~expected ~actual ()
end

let () =
  Alcotest.run
    ~__FILE__
    "Prevalidator"
    [
      (* Run only those tests with:
         dune exec src/lib_shell/test/test_prevalidator_classification_operations.exe -- test 'handle_operations' *)
      ( "handle_operations",
        qcheck_wrap
          Handle_operations.
            [
              test_handle_live_operations_is_branch_alive;
              test_handle_live_operations_path_spec;
              test_handle_live_operations_clear;
              test_handle_live_operations_inject;
            ] );
      (* Run only first two tests (for example) with:
         dune exec src/lib_shell/test/test_prevalidator_classification_operations.exe -- test 'recycle_operations' '0..2'*)
      ( "recycle_operations",
        qcheck_wrap
          Recyle_operations.
            [
              test_recycle_operations_empty_live_blocks;
              test_recycle_operations_returned_value_spec;
              test_recycle_operations_classification;
            ] );
    ]
