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

let find_and_remove_opt f list =
  let rec aux acc = function
    | [] -> None
    | x :: xs -> if f x then Some (x, List.rev acc @ xs) else aux (x :: acc) xs
  in
  aux [] list

let find_and_remove f list =
  match find_and_remove_opt f list with None -> raise Not_found | Some x -> x

let find2_and_remove f1 f2 list =
  match find_and_remove_opt f1 list with
  | None -> find_and_remove f2 list
  | Some x -> x

let rec split_last = function
  | [] -> assert false
  | [x] -> ([], x)
  | x :: xs ->
      let xs, last = split_last xs in
      (x :: xs, last)

let split_first = function [] -> assert false | x :: xs -> (x, xs)

(* TODO: implement this function in O(n) complexity *)
let list_intersection ~equal l1 l2 =
  List.fold_left
    (fun acc x -> if List.exists (equal x) l1 then x :: acc else acc)
    []
    l2

(* [complete ~n acc l] moves elements from [l] to [acc] until
   [acc] contains exactly [n] elements, it returns two lists
   corresponding to the updated [acc] and [l] respectively. *)
let rec complete ~n acc = function
  | [] -> (acc, [])
  | h :: t ->
      let r = List.compare_length_with acc n in
      if r > 0 then assert false
      else if r = 0 then (acc, h :: t)
      else complete ~n (acc @ [h]) t

let rev_first_n ~n l =
  let rec aux (acc, k) = function
    | [] -> acc
    | h :: t -> if k = n then acc else aux (h :: acc, succ k) t
  in
  aux ([], 0) l

let permute_list permutation l =
  let l = Array.of_list l in
  List.map (fun j -> l.(j)) permutation

let rec is_identity_perm ?(cnt = 0) permutation =
  match permutation with
  | [] -> true
  | x :: xs -> x = cnt && is_identity_perm ~cnt:(cnt + 1) xs

(* Given two lists [l1] and [l2], it returns a permutation [p] of [l1] such that
   for all i:  l2.(i) = l1.(p i)  OR  l2.(i) < 0  OR  l1.(p i) < 0.
   (Where a permutation is modeled as a list of integers). *)
let adapt l1 l2 =
  (* return the identity if they are already compatible *)
  if List.for_all2 (fun x1 x2 -> x1 = x2 || x1 < 0 || x2 < 0) l1 l2 then
    Some (List.init (List.length l1) Fun.id)
  else
    let common = list_intersection ~equal:( = ) l1 l2 in
    try
      let perm, _ =
        List.fold_left
          (fun (perm, pending) y ->
            let equals_y (_, x) = x = y in
            let is_free (_, x) = x < 0 in
            let is_unrelated (_, x) = x >= 0 && (not @@ List.mem x common) in
            let x, pending =
              if y >= 0 then
                if List.mem y (List.map snd pending) then
                  find_and_remove equals_y pending
                else find_and_remove is_free pending
              else find2_and_remove is_unrelated is_free pending
            in
            (perm @ [fst x], pending))
          ([], List.mapi (fun i x -> (i, x)) l1)
          l2
      in
      Some perm
    with Not_found -> None

let list_sub n l =
  let rec aux k acc l =
    if k = n then List.rev acc else aux (k + 1) (List.hd l :: acc) (List.tl l)
  in
  aux 0 [] l

let shuffle_list ?seed l =
  (match seed with None -> () | Some i -> Random.init i) ;
  List.rev_map (fun x -> (Random.bits (), x)) l
  |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
  |> List.rev_map snd
