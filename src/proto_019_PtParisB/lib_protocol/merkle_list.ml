(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error += Merkle_list_invalid_position

let max_depth ~count_limit =
  (* We assume that the Merkle_tree implementation computes a tree in a
     logarithmic size of the number of leaves. *)
  let log2 n = Z.numbits (Z.of_int n) in
  log2 count_limit

let () =
  register_error_kind
    `Temporary
    ~id:"Merkle_list_invalid_position"
    ~title:"Merkle_list_invalid_position"
    ~description:"Merkle_list_invalid_position"
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" "Merkle_list_invalid_position")
    Data_encoding.empty
    (function Merkle_list_invalid_position -> Some () | _ -> None)
    (fun () -> Merkle_list_invalid_position)

module type T = sig
  type t

  type h

  type elt

  type path

  val dummy_path : path

  val pp_path : Format.formatter -> path -> unit

  val nil : t

  val empty : h

  val root : t -> h

  val snoc : t -> elt -> t

  val snoc_tr : t -> elt -> t

  val of_list : elt list -> t

  val compute : elt list -> h

  val path_encoding : path Data_encoding.t

  val bounded_path_encoding : ?max_length:int -> unit -> path Data_encoding.t

  val compute_path : t -> int -> path tzresult

  val check_path : path -> int -> elt -> h -> bool tzresult

  val path_depth : path -> int

  val elt_bytes : elt -> Bytes.t

  module Internal_for_tests : sig
    val path_to_list : path -> h list

    val equal : t -> t -> bool

    val to_list : t -> h list
  end
end

module Make (El : sig
  type t

  val to_bytes : t -> bytes
end)
(H : S.HASH) : T with type elt = El.t and type h = H.t = struct
  type h = H.t

  type elt = El.t

  let elt_bytes = El.to_bytes

  (*
  The goal of this structure is to model an append-only list.
  Its internal representation is that of a binary tree whose
  leaves are all at the same level (the tree's height).

  To insert a new element in a full tree t, we create a new root with t
  as its left subtree and a new tree t' as its right subtree. t' is just a
  left-spine of the same height as t. Visually,

    t =    / \           t' =   /      snoc 4 t =     /     \
         /\   /\              /                     / \     /
        0 1  2  3            4                    /\  /\   /
                                                 0 1 2 3  4

  Then, this is a balanced tree by construction.
  As the key in the tree for a given position is the position's
  binary decomposition of size height(tree), the tree is dense.
  For that reason, the use of extenders is not needed.
  *)

  type tree = Empty | Leaf of h | Node of (h * tree * tree)

  (* The tree has the following invariants:
     A node [Node left right] if valid iff
       1. [right] is Empty and [left] is not Empty, or
       2. [right] is not Empty and [left] is full
     Additionally:
      [t.depth] is the height of [t.tree] and
      [t.next_pos] is the number of leaves in [t.tree] *)
  type t = {tree : tree; depth : int; next_pos : int}

  type path = h list

  let dummy_path = []

  let pp_path ppf =
    Format.fprintf
      ppf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         H.pp)

  let empty = H.zero

  let root = function Empty -> empty | Leaf h -> h | Node (h, _, _) -> h

  let nil = {tree = Empty; depth = 0; next_pos = 0}

  let hash_elt el = H.hash_bytes [elt_bytes el]

  let leaf_of el = Leaf (hash_elt el)

  let hash2 h1 h2 = H.(hash_bytes [to_bytes h1; to_bytes h2])

  let node_of t1 t2 = Node (hash2 (root t1) (root t2), t1, t2)

  (* to_bin computes the [depth]-long binary representation of [pos]
     (left-padding with 0s if required). This corresponds to the tree traversal
     of en element at position [pos] (false = left, true = right).

     Pre-condition: pos >= 0 /| pos <  2^depth
     Post-condition: len(to_bin pos depth) = depth *)
  let to_bin ~pos ~depth =
    let rec aux acc pos depth =
      let pos', dir = (pos / 2, pos mod 2) in
      match depth with
      | 0 -> acc
      | d -> aux (Compare.Int.(dir = 1) :: acc) pos' (d - 1)
    in
    aux [] pos depth

  (* Constructs a tree of a given depth in which every right subtree is empty
   * and the only leaf contains the hash of el. *)
  let make_spine_with el =
    let rec aux left = function
      | 0 -> left
      | d -> (aux [@tailcall]) (node_of left Empty) (d - 1)
    in
    aux (leaf_of el)

  let snoc t (el : elt) =
    let rec traverse tree depth key =
      match (tree, key) with
      | Node (_, t_left, Empty), true :: _key ->
          (* The base case where the left subtree is full and we start
           * the right subtree by creating a new tree the size of the remaining
           * depth and placing the new element in its leftmost position. *)
          let t_right = make_spine_with el (depth - 1) in
          node_of t_left t_right
      | Node (_, t_left, Empty), false :: key ->
          (* Traversing left, the left subtree is not full (and thus the right
           * subtree is empty). Recurse on left subtree. *)
          let t_left = traverse t_left (depth - 1) key in
          node_of t_left Empty
      | Node (_, t_left, t_right), true :: key ->
          (* Traversing right, the left subtree is full.
           * Recurse on right subtree *)
          let t_right = traverse t_right (depth - 1) key in
          node_of t_left t_right
      | _, _ ->
          (* Impossible by construction of the tree and of the key.
           * See [tree] invariants and [to_bin]. *)
          assert false
    in

    let tree', depth' =
      match (t.tree, t.depth, t.next_pos) with
      | Empty, 0, 0 -> (node_of (leaf_of el) Empty, 1)
      | tree, depth, pos when Int32.(equal (shift_left 1l depth) (of_int pos))
        ->
          let t_right = make_spine_with el depth in
          (node_of tree t_right, depth + 1)
      | tree, depth, pos ->
          let key = to_bin ~pos ~depth in
          (traverse tree depth key, depth)
    in
    {tree = tree'; depth = depth'; next_pos = t.next_pos + 1}

  type zipper = Left of zipper * tree | Right of tree * zipper | Top

  let rec rebuild_tree z t =
    match z with
    | Top -> t
    | Left (z, r) -> (rebuild_tree [@tailcall]) z (node_of t r)
    | Right (l, z) -> (rebuild_tree [@tailcall]) z (node_of l t)

  let snoc_tr t (el : elt) =
    let rec traverse (z : zipper) tree depth key =
      match (tree, key) with
      | Node (_, t_left, Empty), true :: _key ->
          let t_right = make_spine_with el (depth - 1) in
          rebuild_tree z (node_of t_left t_right)
      | Node (_, t_left, Empty), false :: key ->
          let z = Left (z, Empty) in
          (traverse [@tailcall]) z t_left (depth - 1) key
      | Node (_, t_left, t_right), true :: key ->
          let z = Right (t_left, z) in
          (traverse [@tailcall]) z t_right (depth - 1) key
      | _, _ ->
          (* Impossible by construction of the tree and of the key.
           * See [tree] invariants and [to_bin]. *)
          assert false
    in

    let tree', depth' =
      match (t.tree, t.depth, t.next_pos) with
      | Empty, 0, 0 -> (node_of (leaf_of el) Empty, 1)
      | tree, depth, pos when Int32.(equal (shift_left 1l depth) (of_int pos))
        ->
          let t_right = make_spine_with el depth in
          (node_of tree t_right, depth + 1)
      | tree, depth, pos ->
          let key = to_bin ~pos ~depth in
          (traverse Top tree depth key, depth)
    in
    {tree = tree'; depth = depth'; next_pos = t.next_pos + 1}

  let rec tree_to_list = function
    | Empty -> []
    | Leaf h -> [h]
    | Node (_, t_left, t_right) -> tree_to_list t_left @ tree_to_list t_right

  let path_encoding = Data_encoding.(list H.encoding)

  let bounded_path_encoding ?max_length () =
    match max_length with
    | None -> path_encoding
    | Some max_length -> Data_encoding.((list ~max_length) H.encoding)

  (* The order of the path is from bottom to top *)
  let compute_path {tree; depth; next_pos} pos =
    let open Result_syntax in
    if Compare.Int.(pos < 0 || pos >= next_pos) then
      tzfail Merkle_list_invalid_position
    else
      let key = to_bin ~pos ~depth in
      let rec aux acc tree key =
        match (tree, key) with
        | Leaf _, [] -> return acc
        | Node (_, l, r), b :: key ->
            if b then aux (root l :: acc) r key else aux (root r :: acc) l key
        | _ -> tzfail Merkle_list_invalid_position
      in
      aux [] tree key

  let check_path path pos el expected_root =
    let open Result_syntax in
    let depth = List.length path in
    if
      Compare.Int.(pos >= 0)
      && Compare.Z.(Z.of_int pos < Z.shift_left Z.one depth)
    then
      let key = List.rev @@ to_bin ~pos ~depth in
      let computed_root =
        List.fold_left
          (fun acc (sibling, b) ->
            if b then hash2 sibling acc else hash2 acc sibling)
          (hash_elt el)
          (List.combine_drop path key)
      in
      return (H.equal computed_root expected_root)
    else tzfail Merkle_list_invalid_position

  let path_depth path = List.length path

  let breadth_first_traversal ~leaf_func ~node_func ~empty ~res l =
    let rec aux ~depth l =
      let rec pairs acc = function
        | [] -> List.rev acc
        | [x] -> List.rev (node_func x empty :: acc)
        | x :: y :: xs -> pairs (node_func x y :: acc) xs
      in
      match pairs [] l with
      | [] -> res depth empty
      | [t] -> res depth t
      | pl -> aux ~depth:(depth + 1) pl
    in
    aux (List.map leaf_func l) ~depth:0

  let compute =
    breadth_first_traversal
      ~leaf_func:hash_elt
      ~node_func:hash2
      ~empty
      ~res:(fun _ x -> x)

  let of_list l =
    let depth, tree =
      breadth_first_traversal
        ~leaf_func:leaf_of
        ~node_func:node_of
        ~empty:Empty
        ~res:(fun d l -> (d + 1, l))
        l
    in
    {tree; depth; next_pos = List.length l}

  let root t = root t.tree

  module Internal_for_tests = struct
    let path_to_list x = x

    let to_list tree = tree_to_list tree.tree

    let equal t1 t2 =
      let rec eq_tree t1 t2 =
        match (t1, t2) with
        | Empty, Empty -> true
        | Leaf h1, Leaf h2 -> H.equal h1 h2
        | Node (h1, l1, r1), Node (h2, l2, r2) ->
            H.equal h1 h2 && eq_tree l1 l2 && eq_tree r1 r2
        | _ -> false
      in
      Compare.Int.equal t1.depth t2.depth
      && Compare.Int.equal t1.next_pos t2.next_pos
      && eq_tree t1.tree t2.tree
  end
end
