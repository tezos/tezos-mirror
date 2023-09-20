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

(** Generators building on top of {!Generators}, that are capable of
    producing trees of blocks. *)

open Shell_operation
module Classification = Prevalidator_classification

(** Various functions about {!list} *)
module List_extra = struct
  (** [common_elem l1 l2] returns the first element of [l1] that
      occurs in [l2]. If [l1] and [l2] do not have a common element,
      [Nothing] is returned. Examples:

      [common_elem [0; 2; 3] [3; 2]] returns [Some 2]
      [common_elem [0; 2; 3] [2; 3]] returns [Some 3]
      [common_elem [0; 2; 3] [4]] returns [Nothing] *)
  let rec common_elem ~(equal : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list)
      =
    match (l1, l2) with
    | [], _ -> None
    | e1 :: rest1, _ ->
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

  (** [values t] returns all values within [t] *)
  let rec values : 'a tree -> 'a list = function
    | Leaf a -> [a]
    | Node1 (a, t1) -> a :: values t1
    | Node2 (a, t1, t2) -> (a :: values t1) @ values t2

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
        ((child1, e) :: (child2, e) :: predecessor_pairs subtree1)
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
  type t = {bhash : Block_hash.t; operations : unit operation list list}

  (* Because we use hashes to implement equality, we must make sure
     that for any pair of generated blocks [(b1, b2)], [b1.hash <> b2.hash]
     implies [b1 <> b2] where [<>] is polymorphic inequality. Said
     differently, hashes should not be faked. *)
  let equal : t -> t -> bool = fun t1 t2 -> Block_hash.equal t1.bhash t2.bhash

  let compare (t1 : t) (t2 : t) = Block_hash.compare t1.bhash t2.bhash

  (** [hash_of_blocks ops] is used to compute the hash of a block whose
      [operations] field contains [ops].

      We want the hash to be sound, because it is used to implement equality
      (see {!equal} above), like in the production implementation. Given
      that {!t} above contains a single field besides the [hash], we hash
      the content of this field to obtain the hash of a block. That
      is why we hash the hashes of operations. *)
  let hash_of_block ops =
    let hash =
      Operation_list_hash.compute
        (List.map (fun op -> op.hash) @@ List.concat ops)
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
  let to_hash (blk : t) = blk.bhash

  let tools : t Classification.block_tools =
    let operations block =
      List.map (List.map (fun op -> op.raw)) block.operations
    in
    let all_operation_hashes block =
      List.map (List.map (fun op -> op.hash)) block.operations
    in
    {bhash = to_hash; operations; all_operation_hashes}

  let to_string t =
    let ops_list_to_string ops =
      String.concat
        "|"
        (List.map
           Operation_hash.to_short_b58check
           (List.map (fun op -> op.hash) ops))
    in
    let ops_string =
      List.fold_left
        (fun acc ops -> Format.sprintf "%s[%s]" acc (ops_list_to_string ops))
        ""
        t.operations
    in
    Format.asprintf "%a:[%s]" Block_hash.pp t.bhash ops_string

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

(** [block_gen ?proto_gen ()] generates a block. [proto_gen] is used
    to generate protocol bytes of operations. *)
let block_gen ?proto_gen () : Block.t QCheck2.Gen.t =
  let open QCheck2.Gen in
  let ops_list_gen =
    (* Having super long list of operations isn't necessary.
       In addition it slows everything down. *)
    list_size
      (int_range 0 10)
      (External_generators.operation_with_hash_gen ?proto_gen ())
  in
  let* ops =
    (* In production these lists are exactly of size 4, being more general *)
    list_size (int_range 0 8) ops_list_gen
  in
  let bhash = Block.hash_of_block ops in
  return Block.{bhash; operations = ops}

(* A generator of sets of {!Block.t} where all elements are guaranteed
   to be different. [list_gen] is an optional list generator. If omitted
   it is defaulted to {!QCheck2.Gen.small_list}. [?proto_gen] is an
   optional generator for protocol bytes of operations. *)
let unique_block_gen ?(list_gen = QCheck2.Gen.small_list) ?proto_gen () :
    Block.Set.t QCheck2.Gen.t =
  QCheck2.Gen.(map Block.Set.of_list @@ list_gen (block_gen ?proto_gen ()))

(* A generator of sets of {!Block.t} where all elements are guaranteed
   to be different and returned sets are guaranteed to be non empty. *)
let unique_nonempty_block_gen =
  let open QCheck2.Gen in
  let+ block = block_gen () and+ l = unique_block_gen () in
  Block.Set.add block l

(** [unique_block_gen n] returns sets of {!Block.t} such that:
    - all blocks are different
    - the cardinal of returned sets is equal or greater than [n].

    [?proto_gen] is an optional generator for protocol bytes of operations. *)
let unique_block_gen_gt ?proto_gen ~(n : int) () : Block.Set.t QCheck2.Gen.t =
  assert (n >= 0) ;
  let open QCheck2.Gen in
  let list_gen = list_repeat n in
  let rec go generated =
    if Block.Set.cardinal generated >= n then return generated
    else
      let* new_blocks = unique_block_gen ?proto_gen ~list_gen () in
      go (Block.Set.union generated new_blocks)
  in
  go Block.Set.empty

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
        map Block.set_to_list unique_nonempty_block_gen
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
          let* n = QCheck2.Gen.int_bound (List.length xs - 1) in
          let left, right = List.split_n n xs in
          let* left = go left and* right = go right in
          match (left, right) with
          | None, None -> ret (Tree.Leaf x)
          | None, Some sub | Some sub, None -> ret (Tree.Node1 (x, sub))
          | Some left, Some right -> ret (Tree.Node2 (x, left, right)))
  in
  (* The assertion cannot break, because we made sure that [blocks] is
     not empty. *)
  map (WithExceptions.Option.get ~loc:__LOC__) (go blocks)

(** A generator for passing the last argument of
      [Prevalidator.handle_live_operations] *)
let old_mempool_gen (tree : Block.t Tree.tree) :
    unit operation Operation_hash.Map.t QCheck2.Gen.t =
  let blocks = Tree.values tree in
  let pairs = List.concat_map Block.tools.operations blocks |> List.concat in
  let elements =
    List.map
      (fun (op : Operation.t) ->
        let hash = Operation.hash op in
        Internal_for_tests.make_operation hash op ())
      pairs
  in
  if elements = [] then QCheck2.Gen.return Operation_hash.Map.empty
  else
    let list_gen = QCheck2.Gen.(oneofl elements |> list) in
    QCheck2.Gen.map
      (fun l ->
        List.to_seq l
        |> Seq.map (fun op -> (op.hash, op))
        |> Operation_hash.Map.of_seq)
      list_gen

(** Function to implement
    {!Prevalidator_classification.chain_tools.new_blocks} *)
let new_blocks (type a) ~(equal : a -> a -> bool) (tree : a Tree.tree)
    ~from_block ~to_block =
  match Tree.find_ancestor ~equal tree from_block to_block with
  | None -> assert false (* Like the production implementation *)
  | Some ancestor -> (
      let to_parents = Tree.predecessors ~equal tree to_block in
      match
        ( to_parents,
          List_extra.take_until_if_found ~pred:(( = ) ancestor) to_parents )
      with
      | [], _ ->
          (* This case is not supported, because the production
             implementation of new_blocks doesn't support it either
             (since it MUST return an ancestor, acccording to its return
             type). If you end up here, this means generated
             data is not constrained enough: this pair [(from_block,
             to_block)] should NOT be tried. Ideally the return type
             of new_blocks should allow this case, hereby allowing
             a more general test. *)
          assert false
      | _, None ->
          (* Should not happen, because [ancestor]
             is a member of [to_parents] *)
          assert false
      | _, Some path ->
          (* Because [to_block] must be included in new_blocks'
             returned value. *)
          let path = to_block :: path in
          Lwt.return (ancestor, List.rev path))

(** Function to implement
  {!Prevalidator_classification.chain_tools.read_predecessor_opt} *)
let read_predecessor_opt (type a) ~(compare : a -> a -> int)
    (tree : a Tree.tree) (a : a) : a option Lwt.t =
  let module Ord = struct
    type t = a

    let compare = compare
  end in
  let module Map = Map.Make (Ord) in
  let predecessors_map =
    Tree.predecessor_pairs tree |> List.to_seq |> Map.of_seq
  in
  Map.find a predecessors_map |> Lwt.return

(** Function providing the instance of
    {!Prevalidator_classification.chain_tools} for a given {!Tree.tree} *)
let generic_classification_chain_tools (type a) ~(compare : a -> a -> int)
    (tree : a Tree.tree) : a Classification.chain_tools =
  let equal a b = compare a b = 0 in
  Classification.
    {
      clear_or_cancel = Fun.const ();
      inject_operation = (fun _ _ -> Lwt.return_unit);
      new_blocks = new_blocks ~equal tree;
      read_predecessor_opt = read_predecessor_opt ~compare tree;
    }

(** A specific instance of {!generic_classification_chain_tools},
    for handiness of users. *)
let classification_chain_tools (tree : Block.t Tree.tree) :
    Block.t Classification.chain_tools =
  generic_classification_chain_tools ~compare:Block.compare tree

(** Returns:
      - An instance of [Tree.tree]: the tree of blocks
      - a pair of blocks (that belong to the tree) and is
        fine for being passed as [(~from_branch, ~to_branch)]; i.e.
        the two blocks have a common ancestor.
      - a map of operations that is fine for being passed as the
        last argument of [handle_live_operations].

      If given, the specified [?blocks] are used. Otherwise they are
      generated. *)
let tree_gen ?blocks () :
    (Block.t Tree.tree
    * (Block.t * Block.t) option
    * unit operation Operation_hash.Map.t)
    QCheck2.Gen.t =
  let open QCheck2.Gen in
  let* tree = tree_gen ?blocks () in
  assert (Tree.well_formed Block.compare tree) ;
  let equal = Block.equal in
  let not_equal x y = not @@ equal x y in
  let tree_elems : Block.t list = Tree.values tree in
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
  let+ old_mempool = old_mempool_gen tree in
  (tree, chosen_pair, old_mempool)

(** [split_in_two l] is a generator producing [(l1, l2)] such that [l1 @ l2 = l] *)
let split_in_two (l : 'a list) : ('a list * 'a list) QCheck2.Gen.t =
  let open QCheck2.Gen in
  let length = List.length l in
  let+ i = 0 -- length in
  List.split_n i l
