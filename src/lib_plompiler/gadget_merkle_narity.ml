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

open Lang_stdlib
open Lang_core

module type HASH = Hash_sig.HASH

module type MERKLE_NARITY = functor (H : HASH) (L : LIB) -> sig
  open L

  type direction = scalar

  type proof = (scalar * (scalar list * direction) list) repr

  type root = scalar repr

  val merkle_proof : int -> proof -> root -> bool repr t
end

module P (Mec : Hash_sig.P_HASH) = struct
  type tree = Leaf of S.t | Branch of S.t * tree list

  let root = function Leaf h -> h | Branch (h, _) -> h

  let rec generate_tree n depth =
    if depth = 0 then Leaf (S.random ())
    else
      let children = List.init n (fun _ -> generate_tree n (depth - 1)) in
      let root = Mec.direct (Array.of_list (List.map root children)) in
      Branch (root, children)

  (* This function assumes that the length of pos and the depth of tree coincide.
     Furthermore, that pos is a list of integers between 0 and n-1, where n is
     the arity of the tree (which is consistent in arity).
  *)
  let proof_path_narity pos tree =
    let rec aux path pos = function
      | Leaf h -> (path, h)
      | Branch (_, children) ->
          let direction = List.hd pos in
          let t = List.nth children direction in
          let level_witnesses =
            List.filteri (fun j _ -> direction <> j) children |> List.map root
          in
          aux
            ((level_witnesses, S.of_z (Z.of_int direction)) :: path)
            (List.tl pos)
            t
    in
    aux [] pos tree
end

module V : MERKLE_NARITY =
functor
  (H : HASH)
  (L : LIB)
  ->
  struct
    open L

    open H (L)

    type direction = scalar

    (* leaf, path *)
    type proof = (scalar * (scalar list * direction) list) repr

    type root = scalar repr

    let merkle_proof : int -> proof -> root -> bool repr t =
     fun n witness expected_root ->
      with_label ~label:"MerkleNArity.merkle_proof"
      @@
      let module E = Enum (struct
        let n = n
      end) in
      let leaf, path = of_pair witness in
      let* root =
        foldM
          (fun computed_h step ->
            let level_witnesses, direction = of_pair step in
            let level_witnesses = of_list level_witnesses in
            let* hash_inputs =
              foldiM
                (fun hash_inputs i ->
                  let hash_inputs = of_list hash_inputs in
                  let cand_i =
                    if i > 0 then List.nth level_witnesses (i - 1)
                    else computed_h
                  in
                  let cand_i' =
                    if i < n - 1 then List.nth level_witnesses i else computed_h
                  in
                  let candidates =
                    to_list
                    @@ List.init n (fun j ->
                           if j < i then cand_i
                           else if j = i then computed_h
                           else cand_i')
                  in
                  let* input_i = E.switch_case direction candidates in
                  let hash_inputs = to_list (input_i :: hash_inputs) in
                  ret hash_inputs)
                (to_list [])
                n
            in
            digest (to_list @@ List.rev (of_list hash_inputs)))
          leaf
          (of_list path)
      in
      equal root expected_root
  end
