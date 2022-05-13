(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

module Make_Storage (C : Core_sig.Validator) = struct
  module Tree = struct
    module H = C.Hash

    let max_uint32 = 4294967295L

    (** Incremental Merkle Tree
     *
     * A tree of height h contains 2^h leaves and h+1 levels of nodes with
     * leaves at level 0 and root at level h.
     *
     * The leaves are commitments and the tree is treated as always filled
     * with a default value H.uncommitted. This allows to have proofs of
     * membership, or witnesses, of fixed size.
     *
     * All the nodes at the same level of an empty tree have the same hash,
     * which can be computed from the default value of the leaves. This is
     * stored in the [uncommitted] list.
     *
     * Any subtree filled with default values is represented by the Empty
     * constructor and given its height it's possible to compute its hash
     * using the [uncommitted] list.
     *
     * The leaves are indexed by their position [pos], ranging from 0 to
     * (2^h)-1. The encoding of [pos] limits the possible size of the tree.
     * In any case the only valid height for the Sapling library is 32, so even
     * if the library encodes positions as uint64, they never exceed uint32.
     *
     * The tree is incremental in the sense that leaves cannot be modified but
     * only added and exclusively in successive positions.
     * The type t contains the size of the tree which is also the next position
     * to fill.
     *
     * Given that elements are added and retrieved by position, it is possible
     * to use this information to efficiently navigate the tree.
     * Given a tree of height [h] and a position [pos], if pos < pow2 (h-1) only
     * the left subtree needs to be inspected recursively. Otherwise only the
     * right needs to be visited, decreasing [pos] by [pow2 (h-1)].
     *
     * In order to avoid storing the height for each subtree (or worse
     * recomputing it), each function with suffix `_height` expects the height
     * of the tree as parameter. These functions are only for internal use and
     * are later aliased by functions using the default height of a Sapling
     * incremental Merkle tree.
     *)

    type tree = Empty | Leaf of C.Commitment.t | Node of (H.t * tree * tree)

    let tree_encoding =
      let open Data_encoding in
      mu "merkle_tree" (fun merkle_tree ->
          let empty_case =
            case
              ~title:"empty"
              (Tag 0)
              Data_encoding.empty
              (function Empty -> Some () | _ -> None)
              (fun () -> Empty)
          in
          let leaf_case =
            case
              ~title:"leaf"
              (Tag 1)
              C.Commitment.encoding
              (fun tree -> match tree with Leaf h -> Some h | _ -> None)
              (fun h -> Leaf h)
          in
          let node_case =
            let payload_encoding = tup3 H.encoding merkle_tree merkle_tree in
            case
              ~title:"node"
              (Tag 2)
              payload_encoding
              (fun tree ->
                match tree with Node (h, l, r) -> Some (h, l, r) | _ -> None)
              (fun (h, l, r) -> Node (h, l, r))
          in
          union [empty_case; leaf_case; node_case])

    let get_root_height tree height =
      assert (Compare.Int.(height >= 0 && height <= 32)) ;
      match tree with
      | Empty -> H.uncommitted ~height
      | Node (h, _, _) -> h
      | Leaf h -> H.of_commitment h

    let pow2 h = Int64.(shift_left 1L h)

    let rec split_at n l =
      assert (Compare.Int64.(n >= 0L)) ;
      if Compare.Int64.(n = 0L) then ([], l)
      else
        match l with
        | [] -> ([], l)
        | x :: xs ->
            let l1, l2 = split_at Int64.(pred n) xs in
            (x :: l1, l2)

    let hash ~height t1 t2 =
      H.merkle_hash
        ~height
        (get_root_height t1 height)
        (get_root_height t2 height)

    (*
      [insert tree height pos cms] inserts the list of commitments
      [cms] in the tree [tree] of height [height] at the next position [pos].
      Pre: incremental tree /\
           size tree + List.length cms <= pow2 height /\
           pos = size tree /\
      Post: incremental tree /\
            to_list (insert tree height pos cms) = to_list t @ cms
    *)
    let rec insert tree height pos cms =
      assert (Compare.Int64.(pos >= 0L && pos <= pow2 height)) ;
      assert (Compare.Int.(height >= 0 && height <= 32)) ;
      match (tree, height, cms) with
      | _, _, [] -> tree
      | Empty, 0, [cm] -> Leaf cm
      | Leaf _, _, _
      (* The second conjuntion of the precondition is violated by a Leaf (which
         is already full) and a non empty cms. *)
      | _, 0, _ ->
          (* Only leaves can be at height 0. *)
          assert false
      | Empty, height, _ -> insert_node Empty Empty (height - 1) pos cms
      | Node (_, t1, t2), height, _ -> insert_node t1 t2 (height - 1) pos cms

    and insert_node t1 t2 height pos cms =
      let t1, t2 =
        if Compare.Int64.(pos < pow2 height) then (
          assert (t2 = Empty) ;
          let at = Int64.(sub (pow2 height) pos) in
          let cml, cmr = split_at at cms in
          let t1 = insert t1 height pos cml in
          let t2 = insert t2 height 0L cmr in
          (t1, t2))
        else (
          assert (t1 <> Empty) ;
          let pos = Int64.(sub pos (pow2 height)) in
          let t2 = insert t2 height pos cms in
          (t1, t2))
      in
      let h = hash ~height t1 t2 in
      Node (h, t1, t2)

    let rec get_cm_height tree pos height =
      assert (Compare.Int64.(pos >= 0L && pos <= pow2 height)) ;
      assert (Compare.Int.(height >= 0 && height <= 32)) ;
      match tree with
      | Empty -> assert false
      | Node (_, l, r) ->
          let full = pow2 (height - 1) in
          if Compare.Int64.(pos < full) then get_cm_height l pos (height - 1)
          else
            let pos = Int64.(sub pos full) in
            get_cm_height r pos (height - 1)
      | Leaf h ->
          assert (Compare.Int.(height = 0)) ;
          h

    (** Create the witness in the format required by Sapling.
        - height of the merkle tree (32 for Sapling)
        - a list of size of hash (32 for Pedersen hash) and hash
        - the position as little-endian uint64 *)
    let get_witness_height tree height pos =
      assert (Compare.Int64.(pos >= 0L && pos <= pow2 height)) ;
      assert (Compare.Int.(height >= 0 && height <= 32)) ;
      (* get the neighbors hashes *)
      let rec aux height acc pos = function
        | Empty -> assert false
        | Leaf _ -> acc
        | Node (_, l, r) ->
            let full = pow2 (height - 1) in
            if Compare.Int64.(pos < full) then
              aux (height - 1) (get_root_height r (height - 1) :: acc) pos l
            else
              let pos = Int64.(sub pos full) in
              aux (height - 1) (get_root_height l (height - 1) :: acc) pos r
      in
      (* ordered by height *)
      let hashes = aux height [] pos tree in
      (* ordered by depth *)
      let d = Bytes.make 1 (char_of_int height) in
      let hs = List.rev_map (fun h -> Bytes.cat d @@ H.to_bytes h) hashes in
      let p =
        let b = Bytes.create 8 in
        Bytes.set_int64_le b 0 pos ;
        b
      in
      Bytes.concat Bytes.empty ([d] @ hs @ [p])

    let rec fold_from_height ~pos ~f ~init tree height =
      assert (Compare.Int64.(pos >= 0L && pos <= pow2 height)) ;
      assert (Compare.Int.(height >= 0 && height <= 32)) ;
      match tree with
      | Empty -> init
      | Leaf c -> f init c
      | Node (_, t1, t2) ->
          let full = pow2 (height - 1) in
          if Compare.Int64.(pos < full) then
            let init = fold_from_height ~pos ~f ~init t1 (height - 1) in
            (* fold on the whole subtree t2 *)
            fold_from_height ~pos:0L ~f ~init t2 (height - 1)
          else
            let pos = Int64.(sub pos full) in
            fold_from_height ~pos ~f ~init t2 (height - 1)

    type t = int64 * tree

    let encoding =
      let open Data_encoding in
      obj2 (req "size" int64) (req "tree" tree_encoding)

    let empty = (0L, Empty)

    let size (n, _) = n

    let default_height = 32

    let max_size = Int64.shift_left 1L default_height

    let get_root (_, tree) = get_root_height tree default_height

    let add (n, t) cms =
      let l = List.length cms in
      (* insert is not tail-recursive so we put a hard limit on the size of
         its argument *)
      assert (l <= 1000) ;
      let n' = Int64.(add n (of_int l)) in
      assert (Compare.Int64.(n' <= max_size)) ;
      let t = insert t default_height n cms in
      (n', t)

    let get_witness (n, t) pos =
      if Compare.Int64.(pos < n) then
        Some (get_witness_height t default_height pos)
      else None

    let get_cm (n, tree) pos =
      if Compare.Int64.(pos < n) then
        Some (get_cm_height tree pos default_height)
      else None

    let get_from (n, tree) pos =
      if Compare.Int64.(pos < n) then
        let l =
          fold_from_height
            ~pos
            ~f:(fun acc c -> c :: acc)
            ~init:[]
            tree
            default_height
        in
        Some (List.rev l)
      else None
  end

  module Roots = struct
    module Map = Map.Make (Int32)

    type t = Int32.t * C.Hash.t Map.t

    let size = 500l

    let empty : t = (0l, Map.empty)

    let add e ((pos, map) : t) : t =
      let map = Map.add pos e map in
      let pos = Int32.rem (Int32.succ pos) size in
      (pos, map)

    let mem e ((_, map) : t) =
      Map.exists (fun _ v -> Compare.Int.(C.Hash.compare e v = 0)) map

    let to_list ((_, m) : t) = List.map snd (Map.bindings m)

    let of_list = List.fold_left (fun m e -> add e m) empty

    let encoding =
      let open Data_encoding in
      conv
        to_list
        of_list
        (list ~max_length:(Int32.to_int size) C.Hash.encoding)
  end

  module Nullifiers = struct
    include Set.Make (C.Nullifier)

    let encoding =
      let open Data_encoding in
      conv
        (fun set -> elements set)
        (fun list -> of_list list)
        (list C.Nullifier.encoding)

    let get_from t pos =
      let es, _ =
        fold
          (fun e (acc, cnt) ->
            if Compare.Int64.(cnt >= pos) then (e :: acc, Int64.succ cnt)
            else (acc, Int64.succ cnt))
          t
          ([], 0L)
      in
      List.rev es

    let size = cardinal
  end

  module Ciphertexts = struct
    module Map = Map.Make (Int64)

    type t = C.Ciphertext.t Map.t

    let empty = Map.empty

    let add e t =
      let size = Map.cardinal t in
      Map.add (Int64.of_int size) e t

    let size = Map.cardinal

    let find_opt = Map.find

    let mem = Map.mem

    let iter = Map.iter

    let encoding =
      let open Data_encoding in
      conv
        (fun ciphertext_map ->
          Map.fold
            (fun pos cipher list -> (pos, cipher) :: list)
            ciphertext_map
            [])
        (fun ciphertext_list ->
          List.fold_left
            (fun map (pos, cipher) -> Map.add pos cipher map)
            Map.empty
            ciphertext_list)
        (list
           (obj2
              (req "ciphertext pos" int64)
              (req "ciphertext" C.Ciphertext.encoding)))

    let get_from t pos =
      let rec aux pos acc =
        match Map.find pos t with
        | None -> acc
        | Some e -> aux (Int64.succ pos) (e :: acc)
      in
      List.rev (aux pos [])
  end

  type state = {
    tree : Tree.t;
    nullifiers : Nullifiers.t;
    roots : Roots.t;
    ciphertexts : Ciphertexts.t;
    memo_size : int;
  }

  let create_empty_state ~memo_size =
    {
      tree = Tree.empty;
      nullifiers = Nullifiers.empty;
      roots = Roots.add (Tree.get_root Tree.empty) Roots.empty;
      ciphertexts = Ciphertexts.empty;
      memo_size;
    }

  let state_encoding =
    let open Data_encoding in
    let check_memo_size memo_size ciphertexts =
      Ciphertexts.iter
        (fun _pos ciphertext ->
          assert (C.Ciphertext.get_memo_size ciphertext = memo_size))
        ciphertexts
    in
    conv
      (fun s ->
        check_memo_size s.memo_size s.ciphertexts ;
        (s.tree, s.nullifiers, s.roots, s.ciphertexts, s.memo_size))
      (fun (tree, nullifiers, roots, ciphertexts, memo_size) ->
        check_memo_size memo_size ciphertexts ;
        {tree; nullifiers; roots; ciphertexts; memo_size})
      (obj5
         (req "tree" Tree.encoding)
         (req "nullifiers" Nullifiers.encoding)
         (req "roots" Roots.encoding)
         (req "ciphertexts" Ciphertexts.encoding)
         (req "memo_size" uint16))

  let mem_nullifier state nullifier = Nullifiers.mem nullifier state.nullifiers

  let add_nullifier state nullifier =
    let new_nullifiers = Nullifiers.add nullifier state.nullifiers in
    {
      tree = state.tree;
      nullifiers = new_nullifiers;
      roots = state.roots;
      ciphertexts = state.ciphertexts;
      memo_size = state.memo_size;
    }

  let add state cm_cipher_list =
    let cm_list, cipher_list = List.split cm_cipher_list in
    assert (
      List.for_all
        (fun cipher -> C.Ciphertext.get_memo_size cipher = state.memo_size)
        cipher_list) ;
    let new_tree = Tree.add state.tree cm_list in
    let new_roots = Roots.add (Tree.get_root new_tree) state.roots in
    let new_ciphertexts =
      List.fold_left
        (fun ciphertexts ciphertext -> Ciphertexts.add ciphertext ciphertexts)
        state.ciphertexts
        cipher_list
    in
    {
      tree = new_tree;
      nullifiers = state.nullifiers;
      roots = new_roots;
      ciphertexts = new_ciphertexts;
      memo_size = state.memo_size;
    }

  let mem_root state hash = Roots.mem hash state.roots

  let empty = create_empty_state

  let get_root state = Tree.get_root state.tree

  let size state =
    (Tree.size state.tree, Int64.of_int @@ Nullifiers.size state.nullifiers)

  let get_memo_size state = state.memo_size

  let get_witness state pos =
    match Tree.get_witness state.tree pos with
    | None -> failwith "Position greater or equal than size."
    | Some w -> w

  let get state pos =
    match Tree.get_cm state.tree pos with
    | None -> failwith "Position greater or equal than size."
    | Some cm -> (
        match Ciphertexts.find_opt pos state.ciphertexts with
        | None -> assert false
        | Some ciphertext -> (cm, ciphertext))

  let mem state pos = Ciphertexts.mem pos state.ciphertexts
end

module Client : Storage_sig.STORAGE = Make_Storage (Core.Validator)

include Client
