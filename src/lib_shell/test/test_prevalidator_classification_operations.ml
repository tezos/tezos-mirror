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
    Component:    Shell (Prevalidator)
    Invocation:   dune exec src/lib_shell/test/test_prevalidator_classification_operations.exe
    Subject:      Unit tests [Prevalidator_classification.Internal_for_tests.handle_live_operations]
                  and [Prevalidator_classification.recyle_operations]
*)

open Lib_test.Qcheck2_helpers
module Op_map = Operation_hash.Map
module Classification = Prevalidator_classification

(** Various functions about {!list} *)
module List_extra = struct
  (** [common_elem [0; 2; 3] [3; 2]] returns [Some 2]
      [common_elem [0; 2; 3] [2; 3]] returns [Some 3]
      [common_elem [0; 2; 3] [4]] returns [Nothing] *)
  let rec common_elem ~(equal : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list)
      =
    match (l1, l2) with
    | ([], _) -> None
    | (e1 :: rest1, _) ->
        if List.exists (equal e1) l2 then Some e1
        else common_elem ~equal rest1 l2

  (** [take_until_if_found ((=) 2)  [0; 3; 2; 4; 2]] returns [Some [0; 3]]
      [take_until_if_found ((=) -1) [0; 3; 2; 4; 2]] returns [None]
      [take_until_if_found ((=) 0)  [0]]             returns [Some []] *)
  let rec take_until_if_found ~(pred : 'a -> bool) (l : 'a list) =
    match l with
    | [] -> None
    | fst :: _ when pred fst -> Some []
    | fst :: rest_l -> (
        match take_until_if_found ~pred rest_l with
        | None -> None
        | Some tail -> Some (fst :: tail))

  (** [split_n l n] returns two lists, the first one containing the first
      [n] elements of [l] and the second one containing the remaining elements.
      For example:
      [split_n [] _] is [([], [])]
      [split_n ["a"] 1] is [(["a"], [])]
      [split_n ["a"; "b"; "c"] 1] is [(["a"], ["b"; "c"])] *)
  let split_n l n = (List.take_n n l, List.drop_n n l)
end

module Tree = struct
  (** Trees representing the shape of the chain. The root is the common
      ancestor of all blocks, like this:

                      head3
                        /
         head1  head2  .
            \       \ /
             .       .
              \     /
              ancestor
  *)
  type 'a tree =
    | Leaf of 'a
    | Node1 of ('a * 'a tree)
    | Node2 of ('a * 'a tree * 'a tree)

  (* Note that I intentionally do not use {!Format} as automatic
     line cutting makes reading the output (when debugging) harder. *)
  let rec to_string_aux elem_to_string t indent =
    match t with
    | Leaf e -> indent ^ elem_to_string e
    | Node1 (e, subt) ->
        let indentpp = indent ^ "  " in
        Printf.sprintf
          "%s%s\n%s"
          indent
          (elem_to_string e)
          (to_string_aux elem_to_string subt indentpp)
    | Node2 (e, t1, t2) ->
        let indentpp = indent ^ "  " in
        Printf.sprintf
          "%s%s\n%s\n%s"
          indent
          (elem_to_string e)
          (to_string_aux elem_to_string t1 indentpp)
          (to_string_aux elem_to_string t2 indentpp)

  (* [to_string] is unused but useful when debugging, renaming it to [_to_string] to keep it around  *)
  let _to_string elem_to_string t = to_string_aux elem_to_string t ""

  let rec depth = function
    | Leaf _ -> 1
    | Node1 (_, t1) -> 1 + depth t1
    | Node2 (_, t1, t2) -> 1 + max (depth t1) (depth t2)

  (** The root value of a tree *)
  let value : 'a tree -> 'a = function
    | Leaf a -> a
    | Node1 (a, _) -> a
    | Node2 (a, _, _) -> a

  let rec values : 'a tree -> 'a list = function
    | Leaf a -> [a]
    | Node1 (a, t1) -> a :: values t1
    | Node2 (a, t1, t2) -> a :: values t1 @ values t2

  (** Predicate to check that all values are different. We want
      this property for trees of blocks. If generation of block
      were to repeat a block, this property could get broken. *)
  let well_formed (type a) (compare : a -> a -> int) (t : a tree) =
    let module Ord = struct
      type t = a

      let compare = compare
    end in
    let module Set = Set.Make (Ord) in
    let values_list = values t in
    let values_set = Set.of_list values_list in
    Compare.List_length_with.(values_list = Set.cardinal values_set)

  (** Given a tree of values, returns an association list from a value to
      its parent (i.e. predecessor) in the tree. I.e. given :

             c1   c2  c3
              \    \ /
               b0   b1
                \  /
                 a0

      return: [(b0, a0); (c1, b0); (b1, a0); (c2, b1); (c3; b1)]
  *)
  let rec predecessor_pairs (tree : 'a tree) : ('a * 'a) list =
    match tree with
    | Leaf _ -> []
    | Node1 (e, subtree) ->
        let child = value subtree in
        (child, e) :: predecessor_pairs subtree
    | Node2 (e, subtree1, subtree2) ->
        let child1 = value subtree1 in
        let child2 = value subtree2 in
        (child1, e) :: (child2, e) :: predecessor_pairs subtree1
        @ predecessor_pairs subtree2

  (** Returns the predecessors of a tree node. I.e., given
      such a tree:

             c1   c2  c3
              \    \ /
               b0   b1
                \  /
                 a0

      [predecessors [c1]] is [b0; a0]
      [predecessors [a0]] is []
      [predecessors [b1]] is [a0]
  *)
  let predecessors ~(equal : 'a -> 'a -> bool) (tree : 'a tree) (e : 'a) =
    let predecessor_pairs = predecessor_pairs tree in
    let rec main (x : 'a) =
      match List.assoc ~equal x predecessor_pairs with
      | None -> []
      | Some parent -> parent :: main parent
    in
    main e

  let predecessors ~(equal : 'a -> 'a -> bool) (tree : 'a tree) (e : 'a) =
    let res = predecessors ~equal tree e in
    (* If this assertion breaks, the tree is illformed *)
    assert (not (List.mem ~equal e res)) ;
    res

  (** [elems t] returns all values within [t] *)
  let rec elems : 'a tree -> 'a list = function
    | Leaf a -> [a]
    | Node1 (a, t1) -> a :: elems t1
    | Node2 (a, t1, t2) -> a :: elems t1 @ elems t2

  (** [find_ancestor tree e1 e2] returns the common ancestor of [e1] and [e2]
      in [tree], if any *)
  let find_ancestor ~(equal : 'a -> 'a -> bool) (tree : 'a tree) (e1 : 'a)
      (e2 : 'a) : 'a option =
    let parents1 = predecessors ~equal tree e1 in
    let parents2 = predecessors ~equal tree e2 in
    if List.mem ~equal e1 parents2 then Some e1
    else if List.mem ~equal e2 parents1 then Some e2
    else List_extra.common_elem ~equal parents1 parents2
end

(** Module concerning the type with which [Prevalidator.Internal_for_tests.block_tools]
    and [Prevalidator.Internal_for_tests.chain_tools] are instantiated *)
module Block = struct
  (** The block-like interface that suffices to test
      [Prevalidator.Internal_for_tests.handle_live_operations] *)
  type t = {
    hash : Block_hash.t;
    operations : (Operation_hash.t * Operation.t) list list;
  }

  (* Because we use hashes to implement equality, we must make sure
     that for any pair of generated blocks [(b1, b2)], [b1.hash <> b2.hash]
     implies [b1 <> b2] where [<>] is polymorphic inequality. Said
     differently, hashes should not be faked. *)
  let equal : t -> t -> bool = fun t1 t2 -> Block_hash.equal t1.hash t2.hash

  let compare (t1 : t) (t2 : t) = Block_hash.compare t1.hash t2.hash

  (** [hash_of_blocks ops_and_hashes] is used to compute the hash of a block whose
      [operations] field contains [ops_and_hashes].

      We want the hash to be sound, because it is used to implement equality
      (see {!equal} above), like in the production implementation. Given
      that {!t} above contains a single field besides the [hash], we hash
      the content of this field to obtain the hash of a block. That
      is why we hash the hashes of operations. *)
  let hash_of_block ops_and_hashes =
    let hash =
      Operation_list_hash.compute (List.map fst @@ List.concat ops_and_hashes)
    in
    (* We forge a fake [block_header] hash by first hashing the operations
       and change the [b58] signature into a signature that looks like
       the one of a block header by prefixing it with the letter [B]. *)
    let hash_string = Operation_list_hash.to_b58check hash in
    let suffix = String.sub hash_string 2 31 in
    match Block_hash.of_string @@ "B" ^ suffix with
    | Error err ->
        Format.printf "Unexpected error: %a" Error_monad.pp_print_trace err ;
        assert false
    | Ok hash -> hash

  (** Returns the [hash] field of a {!t} *)
  let to_hash (blk : t) = blk.hash

  let tools : t Classification.block_tools =
    let operations block = List.map (List.map snd) block.operations in
    let all_operation_hashes block = List.map (List.map fst) block.operations in
    {hash = to_hash; operations; all_operation_hashes}

  let to_string t =
    let ops_list_to_string ops =
      String.concat
        "|"
        (List.map Operation_hash.to_short_b58check (List.map fst ops))
    in
    let ops_string =
      List.fold_left
        (fun acc ops -> Format.sprintf "%s[%s]" acc (ops_list_to_string ops))
        ""
        t.operations
    in
    Format.asprintf "%a:[%s]" Block_hash.pp t.hash ops_string

  (* [pp_list] is unused but useful when debugging, renaming it to [_pp_list] to keep it around  *)

  (** Pretty prints a list of {!t}, using [sep] as the separator *)
  let _pp_list ~(sep : string) (ts : t list) =
    String.concat sep @@ List.map to_string ts

  module Ord = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (Ord)

  let set_to_list s = Set.to_seq s |> List.of_seq
end

module External_generators = Generators

(** [QCheck2] generators used in tests below *)
module Generators = struct
  let block_gen : Block.t QCheck2.Gen.t =
    let open QCheck2.Gen in
    let* ops =
      let ops_list_gen =
        (* Having super long list of operations isn't necessary.
           In addition it slows everything down. *)
        list_size
          (int_range 0 10)
          (External_generators.operation_with_hash_gen ())
      in
      (* In production these lists are exactly of size 4, being more general *)
      ops_list_gen |> list_size (int_range 0 8)
    in
    let hash = Block.hash_of_block ops in
    return Block.{hash; operations = ops}

  (* A generator of lists of {!Block.t} where all elements are guaranteed
     to be different. *)
  let unique_block_gen : Block.Set.t QCheck2.Gen.t =
    QCheck2.Gen.(small_list block_gen >|= Block.Set.of_list)

  (* A generator of lists of {!Block.t} where all elements are guaranteed
     to be different and returned lists are guaranteed to be non empty. *)
  let unique_nonempty_block_gen =
    let open QCheck2.Gen in
    let+ block = block_gen and+ l = unique_block_gen in
    Block.Set.add block l

  (** A tree generator. Written in a slightly unusual style because it
      generates all values beforehand, to make sure they are all different.
      This is a property we want for trees of blocks. To do so,
      this generator first generates a list of elements [e1; e2; e3; e4; e5; e6]
      and then progressively splits this list to build the subtrees.

      For example it takes [e1] for the root value and then splits
      the rest into [e2; e3] and [e4; e5; e6]. Then it recurses, sending
      [e2; e3] as values to create the left subtree and [e4; e5; e6] to
      create the right subtree.

      This generator takes as parameter an optional list of blocks. If
      they are given, they are used to build the tree; otherwise fresh
      ones are generated. *)
  let tree_gen ?blocks () =
    let open QCheck2.Gen in
    let* (blocks : Block.t list) =
      match blocks with
      | None ->
          (* no blocks received: generate them, use the [nonempty] flavor
             of the generator, to guarantee [blocks <> []] below. *)
          unique_nonempty_block_gen >|= Block.set_to_list
      | Some [] ->
          QCheck2.Test.fail_report
            "tree_gen should not be called with an empty list of blocks"
      | Some blocks ->
          (* take blocks passed as parameters *)
          return blocks
    in
    assert (blocks <> []) ;
    let ret x = return (Some x) in
    let rec go = function
      | [] -> return None
      | [x] -> ret (Tree.Leaf x)
      | x :: xs -> (
          let* one_child = QCheck2.Gen.bool in
          if one_child then
            let* sub = go xs in
            match sub with
            | None -> ret (Tree.Leaf x)
            | Some sub -> ret (Tree.Node1 (x, sub))
          else
            let* (left, right) =
              QCheck2.Gen.int_bound (List.length xs - 1)
              >|= List_extra.split_n xs
            in
            let* left = go left and* right = go right in
            match (left, right) with
            | (None, None) -> ret (Tree.Leaf x)
            | (None, Some sub) | (Some sub, None) -> ret (Tree.Node1 (x, sub))
            | (Some left, Some right) -> ret (Tree.Node2 (x, left, right)))
    in
    (* The assertion cannot break, because we made sure that [blocks] is
       not empty. *)
    go blocks >|= Option.value_f ~default:(fun () -> assert false)

  (** A generator for passing the last argument of
      [Prevalidator.handle_live_operations] *)
  let old_mempool_gen (tree : Block.t Tree.tree) :
      Operation.t Operation_hash.Map.t QCheck2.Gen.t =
    let blocks = Tree.values tree in
    let pairs =
      List.map Block.tools.operations blocks |> List.concat |> List.concat
    in
    let elements =
      List.map (fun (op : Operation.t) -> (Operation.hash op, op)) pairs
    in
    if elements = [] then QCheck2.Gen.return Operation_hash.Map.empty
    else
      let list_gen = QCheck2.Gen.(oneofl elements |> list) in
      QCheck2.Gen.map
        (fun l -> Operation_hash.Map.of_seq (List.to_seq l))
        list_gen

  (** Returns an instance of [block chain_tools] as well as:
      - the tree of blocks
      - a pair of blocks (that belong to the tree) and is
        fine for being passed as [(~from_branch, ~to_branch)]; i.e.
        the two blocks have a common ancestor.
      - a map of operations that is fine for being passed as the
        last argument of [handle_live_operations].

      If given, the specified [?blocks] are used. Otherwise they are
      generated. *)
  let chain_tools_gen ?blocks () :
      (Block.t Classification.chain_tools
      * Block.t Tree.tree
      * (Block.t * Block.t) option
      * Operation.t Operation_hash.Map.t)
      QCheck2.Gen.t =
    let open QCheck2.Gen in
    let* tree = tree_gen ?blocks () in
    assert (Tree.well_formed Block.compare tree) ;
    let predecessor_pairs = Tree.predecessor_pairs tree in
    let equal = Block.equal in
    let not_equal x y = not @@ equal x y in
    let read_predecessor_opt (block : Block.t) : Block.t option Lwt.t =
      List.assoc ~equal block predecessor_pairs |> Lwt.return
    in
    let new_blocks ~from_block ~to_block =
      match Tree.find_ancestor ~equal tree from_block to_block with
      | None -> assert false (* Like the production implementation *)
      | Some ancestor -> (
          let to_parents = Tree.predecessors ~equal tree to_block in
          match
            ( to_parents,
              List_extra.take_until_if_found ~pred:(( = ) ancestor) to_parents
            )
          with
          | ([], _) ->
              (* This case is not supported, because the production
                 implementation of new_blocks doesn't support it either
                 (since it MUST return an ancestor, acccording to its return
                 type). If you end up here, this means generated
                 data is not constrained enough: this pair [(from_block,
                 to_block)] should NOT be tried. Ideally the return type
                 of new_blocks should allow this case, hereby allowing
                 a more general test. *)
              assert false
          | (_, None) ->
              (* Should not happen, because [ancestor]
                 is a member of [to_parents] *)
              assert false
          | (_, Some path) ->
              (* Because [to_block] must be included in new_blocks'
                 returned value. *)
              let path = to_block :: path in
              Lwt.return (ancestor, List.rev path))
    in
    let tree_elems : Block.t list = Tree.elems tree in
    (* Pairs of blocks that are valid for being ~from_block and ~to_block *)
    let heads_pairs : (Block.t * Block.t) list =
      List.product tree_elems tree_elems
      (* don't take from_block=to_block*)
      |> List.filter (fun (left, right) -> not_equal left right)
      (* keep only pairs of blocks that have a common ancestor *)
      |> List.filter (fun (left, right) ->
             Tree.find_ancestor ~equal tree left right |> function
             | None -> false (* We want an ancestor *)
             | Some ancestor ->
                 (* We don't want from_block to be the parent of to_block (or vice versa),
                    because it means the chain would rollback. This is not supported
                    (it hits an assert false in new_blocks, because its return type is
                    not general enough) *)
                 not_equal ancestor left && not_equal ancestor right)
    in
    let* chosen_pair =
      if heads_pairs = [] then return None
      else map Option.some (oneofl heads_pairs)
    in
    let* old_mempool = old_mempool_gen tree in
    let res : Block.t Classification.chain_tools =
      {
        clear_or_cancel = Fun.const ();
        inject_operation = (fun _ _ -> Lwt.return_unit);
        new_blocks;
        read_predecessor_opt;
      }
    in
    return (res, tree, chosen_pair, old_mempool)

  (** [split_in_two l] is a generator producing [(l1, l2)] such that [l1 @ l2 = l] *)
  let split_in_two (l : 'a list) : ('a list * 'a list) QCheck2.Gen.t =
    let open QCheck2.Gen in
    let length = List.length l in
    let+ i = 0 -- length in
    List_extra.split_n l i
end

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
  let pp_pair fmt (oph, op) =
    Format.fprintf fmt "%a:%a" Operation_hash.pp oph Operation.pp op
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
let is_subset (m1 : Operation.t Op_map.t) (m2 : Operation.t Op_map.t) =
  let rec go (m1_seq : (Operation_hash.t * Operation.t) Seq.t) =
    match m1_seq () with
    | Seq.Nil -> true
    | Seq.Cons ((m1_key, m1_value), m1_rest) -> (
        match Op_map.find m1_key m2 with
        | None -> (* A key in [m1] that is not in [m2] *) false
        | Some m2_value -> Operation.equal m1_value m2_value && go m1_rest)
  in
  go (Op_map.to_seq m1)

module Handle_operations = struct
  (** Test that operations returned by [handle_live_operations]
      are all in the alive branch. *)
  let test_handle_live_operations_is_branch_alive =
    (* Like [Generators.chain_tools_gen], but also picks a random subset of
       blocks from the tree to pass an interesting value to [is_branch_alive].
       Could be in [chain_tools_gen] itself, but only used in this test. So
       it would be overkill. *)
    let gen =
      let open QCheck2.Gen in
      let* (chain, tree, pair_blocks_opt, old_mempool) =
        Generators.chain_tools_gen ?blocks:None ()
      in
      let* live_blocks =
        sublist (Tree.values tree)
        >|= List.map (fun (blk : Block.t) -> blk.hash)
      in
      return
        ( chain,
          tree,
          pair_blocks_opt,
          old_mempool,
          Block_hash.Set.of_list live_blocks )
    in
    QCheck2.Test.make
      ~name:"[handle_live_operations] is a subset of alive blocks"
      gen
    @@ fun (chain, tree, pair_blocks_opt, old_mempool, live_blocks) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
    let expected_superset : Operation.t Op_map.t =
      (* Take all blocks *)
      Tree.values tree
      (* Keep only the ones in live_blocks *)
      |> List.filter (fun (blk : Block.t) ->
             Block_hash.Set.mem blk.hash live_blocks)
      (* Then extract (oph, op) pairs from them *)
      |> List.map (fun (blk : Block.t) -> blk.operations)
      |> List.concat |> List.concat |> List.to_seq |> Op_map.of_seq
    in
    let actual : Operation.t Op_map.t =
      Classification.Internal_for_tests.handle_live_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~is_branch_alive:(fun blk_hash ->
          Block_hash.Set.mem blk_hash live_blocks)
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
      (Generators.chain_tools_gen ())
    @@ fun (chain, tree, pair_blocks_opt, _) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
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
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~is_branch_alive:(Fun.const true)
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
      Generators.(chain_tools_gen ())
    @@ fun (chain, tree, pair_blocks_opt, old_mempool) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
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
      ~block_store:Block.tools
      ~chain
      ~from_branch
      ~to_branch
      ~is_branch_alive:(Fun.const true)
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
      (Generators.chain_tools_gen ())
    @@ fun (chain, tree, pair_blocks_opt, old_mempool) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
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
      ~block_store:Block.tools
      ~chain
      ~from_branch
      ~to_branch
      ~is_branch_alive:(Fun.const true)
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
  let classification_of_ops_gen (ops : Operation.t Op_map.t) :
      Classification.t QCheck2.Gen.t =
    let open QCheck2.Gen in
    let bindings = Operation_hash.Map.bindings ops in
    let length = List.length bindings in
    let* empty_space = 0 -- 100 in
    (* To avoid throwing part of [ops], we want the capacity of the classification
       to be equal or larger than [length], hence: *)
    let map_size_limit = length + empty_space in
    (* Because capacity must be > 0 in Ring.create: *)
    let map_size_limit = max 1 map_size_limit in
    let parameters : Classification.parameters =
      {map_size_limit; on_discarded_operation = Fun.const ()}
    in
    let* classes =
      list_size (pure length) External_generators.classification_gen
    in
    assert (List.length classes == length) ;
    let t = Prevalidator_classification.create parameters in
    List.iter
      (fun (classification, (oph, op)) ->
        External_generators.add_if_not_present classification oph op t)
      (List.combine_drop classes bindings) ;
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
    let* blocks = Generators.unique_nonempty_block_gen >|= Block.set_to_list in
    assert (blocks <> []) ;
    let to_ops (blk : Block.t) = List.concat blk.operations in
    let oph_op_list_to_map l = List.to_seq l |> Op_map.of_seq in
    let blocks_ops =
      List.map to_ops blocks |> List.concat |> oph_op_list_to_map
    in
    let both f (a, b) = (f a, f b) in
    let blocks_hashes = List.map Block.to_hash blocks in
    let block_hash_t =
      (* For classification and pending, put 50% of them in live_blocks.
         For the remaining 50%, generate branch randomly, so likely outside
         live_blocks. *)
      frequency
        [(1, External_generators.block_hash_gen); (1, oneofl blocks_hashes)]
    in
    let* classification_pendings_ops =
      (* For classification and pending, we want operations that are NOT in
         the blocks already. Hence: *)
      External_generators.op_map_gen ~block_hash_t ()
      >|= Op_map.filter (fun oph _ -> not (Op_map.mem oph blocks_ops))
    in
    let* (classification_ops, pending_ops) =
      Op_map.bindings classification_pendings_ops
      |> Generators.split_in_two >|= both oph_op_list_to_map
    in
    let* (chain_tools, tree, from_to, _) =
      Generators.chain_tools_gen ~blocks ()
    in
    let+ classification = classification_of_ops_gen classification_ops in
    (chain_tools, tree, from_to, classification, pending_ops)

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
    @@ fun ( (chain, _tree, pair_blocks_opt, classification, pending),
             handle_branch_refused ) ->
    assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
    let actual : Operation.t Op_map.t =
      Classification.recycle_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~live_blocks:Block_hash.Set.empty
        ~classification
        ~pending
        ~handle_branch_refused
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
    @@ fun ( (chain, tree, pair_blocks_opt, classification, pending),
             handle_branch_refused ) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
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
        ~applied:true
        ~prechecked:true
        ~branch_delayed:true
        ~branch_refused:handle_branch_refused
        ~refused:false
        ~outdated:false
        classification
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
        ~classification
        ~pending
        ~handle_branch_refused
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
    @@ fun ( (chain, tree, pair_blocks_opt, classification, pending),
             handle_branch_refused ) ->
    QCheck2.assume @@ Option.is_some pair_blocks_opt ;
    let live_blocks : Block_hash.Set.t =
      Tree.values tree |> List.map Block.to_hash |> Block_hash.Set.of_list
    in
    let expected : Operation.t Op_map.t =
      Classification.Internal_for_tests.to_map
        ~applied:false
        ~prechecked:false
        ~branch_delayed:false
        ~branch_refused:(not handle_branch_refused)
        ~refused:true
        ~outdated:true
        classification
    in
    let (from_branch, to_branch) = force_opt ~loc:__LOC__ pair_blocks_opt in
    let () =
      Classification.recycle_operations
        ~block_store:Block.tools
        ~chain
        ~from_branch
        ~to_branch
        ~live_blocks
        ~classification
        ~pending
        ~handle_branch_refused
      |> Lwt_main.run |> ignore
    in
    let actual =
      Classification.Internal_for_tests.to_map
        ~applied:true
        ~prechecked:true
        ~branch_delayed:true
        ~branch_refused:true
        ~refused:true
        ~outdated:true
        classification
    in
    qcheck_eq' ~pp:op_map_pp ~expected ~actual ()
end

let () =
  Alcotest.run
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
