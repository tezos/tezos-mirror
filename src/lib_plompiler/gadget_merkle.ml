(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

module Make (H : sig
  module P : Hash_sig.P_HASH

  module V : Hash_sig.HASH
end) =
struct
  open Lang_stdlib
  open Lang_core

  let left = true

  let right = false

  module P = struct
    type tree = Leaf of S.t | Node of S.t * tree * tree

    let root = function Leaf h -> h | Node (h, _, _) -> h

    let generate_tree ?(leaves = [||]) depth =
      let size = Z.(to_int (pow (of_int 2) depth)) in
      if not (leaves = [||]) then assert (Array.length leaves = size) ;
      let leaves =
        if leaves = [||] then Array.init size (fun _ -> S.random ()) else leaves
      in
      let rec build_tree depth index =
        if depth = 0 then Leaf leaves.(index)
        else
          let index = 2 * index in
          let tree1 = build_tree (depth - 1) index in
          let tree2 = build_tree (depth - 1) (index + 1) in
          let root = H.P.direct ~input_length:2 [|root tree1; root tree2|] in
          Node (root, tree1, tree2)
      in
      build_tree depth 0

    let get_depth tree =
      let rec aux acc tree =
        match tree with Leaf _ -> acc | Node (_, t, _) -> aux (acc + 1) t
      in
      aux 0 tree

    let get_leaves tree =
      let depth = get_depth tree in
      let size = Z.(to_int (pow (of_int 2) depth)) in
      let leaves = Array.init size (fun _i -> S.zero) in
      let rec aux tree index =
        match tree with
        | Node (_, t1, t2) ->
            let index = 2 * index in
            aux t1 index ;
            aux t2 (index + 1)
        | Leaf leaf -> Array.set leaves index leaf
      in
      aux tree 0 ;
      leaves

    let print_tree tree =
      let rec aux tree layer index =
        match tree with
        | Leaf h ->
            Printf.printf
              "\n leaf layer %d - index %d: %s"
              layer
              index
              (S.to_string h)
        | Node (h, t1, t2) ->
            Printf.printf
              "\n node layer %d - index %d: %s"
              layer
              index
              (S.to_string h) ;
            aux t1 (layer + 1) (2 * index) ;
            aux t2 (layer + 1) ((2 * index) + 1)
      in
      aux tree 0 0

    type leaf = H.P.scalar

    type path = (H.P.scalar * bool) list

    (* pos is an integer which represents leaves increasingly from right to left *)
    let proof_path pos tree : leaf * path =
      let rec to_bin acc x index =
        if index = 0 then acc
        else
          match x mod 2 with
          | 0 -> to_bin (0 :: acc) (x / 2) (index - 1)
          | 1 -> to_bin (1 :: acc) ((x - 1) / 2) (index - 1)
          | _ -> assert false
      in
      let size = get_depth tree in
      let binary = to_bin [] pos size in
      let rec aux path bin = function
        | Leaf leaf -> (leaf, path)
        | Node (_, t1, t2) ->
            if List.hd bin = 0 then
              aux ((root t2, left) :: path) (List.tl bin) t1
            else aux ((root t1, right) :: path) (List.tl bin) t2
      in
      aux [] binary tree

    let update_tree ?input_length tree pos leaf =
      let depth = get_depth tree in
      let size = Z.(to_int (pow (of_int 2) depth)) in
      assert (pos < size) ;
      let _, path = proof_path pos tree in
      let _, updated_path =
        List.fold_left
          (fun (index, acc_list) (x, b) ->
            if index = -1 then (1, [leaf])
            else
              let acc = List.hd acc_list in
              let left, right = if b then (acc, x) else (x, acc) in
              let node = H.P.direct ?input_length [|left; right|] in
              (index + 1, node :: acc_list))
          (-1, [])
          ((leaf, false) :: path)
      in
      let rec update_tree_with_path tree path updated_path =
        match tree with
        | Leaf _h -> Leaf leaf
        | Node (_h, t1, t2) ->
            let node = List.hd path in
            let updated_node = List.hd updated_path in

            let t, t_bar = if snd node = left then (t1, t2) else (t2, t1) in
            let updated_branch =
              update_tree_with_path t (List.tl path) (List.tl updated_path)
            in
            let t1, t2 =
              if snd node = left then (updated_branch, t_bar)
              else (t_bar, updated_branch)
            in
            Node (updated_node, t1, t2)
      in
      update_tree_with_path tree (List.rev path) updated_path
  end

  module type MERKLE = functor (L : LIB) -> sig
    open L
    open Encodings

    type path

    type leaf = scalar

    type root = scalar

    val path_encoding : (P.path, path, (scalar * bool) list) encoding

    val merkle_proof : path -> leaf repr -> root repr -> bool repr t
  end

  module V : MERKLE =
  functor
    (L : LIB)
    ->
    struct
      open L
      module Hash = H.V (L)

      type path = (scalar * bool) list repr

      type leaf = scalar

      type root = scalar

      let path_encoding =
        let open Encodings in
        atomic_list_encoding
          (atomic_obj2_encoding scalar_encoding bool_encoding)

      let merkle_proof : path -> leaf repr -> root repr -> bool repr t =
       fun path leaf expected_root ->
        with_label ~label:"Merkle.merkle_proof"
        @@ let* root =
             foldM
               (fun computed_h step ->
                 let w, direction = of_pair step in
                 let* leftright = Bool.swap direction w computed_h in
                 let l, r = of_pair leftright in
                 Hash.digest ~input_length:2 (to_list [l; r]))
               leaf
               (of_list path)
           in
           equal root expected_root
    end
end
