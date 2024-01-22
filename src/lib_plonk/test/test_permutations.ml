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

open Plonk
open Kzg.Bls

let create_random_permutation n =
  (* This function does not sample permutations uniformly at random,
       it seems to be biased towards permutations similar to the identity *)
  let l = List.init n (fun i -> (i, Random.int n)) in
  let compare (_, k1) (_, k2) = compare k1 k2 in
  let permutation_list = List.map (fun (i, _) -> i) (List.sort compare l) in
  Array.of_list permutation_list

let expand_permutation ~n permutation =
  let m = Array.length permutation in
  List.init m (fun i -> Array.init n (fun j -> (permutation.(i) * n) + j))
  |> Array.concat

let arbitrary_perm =
  let print_perm perm =
    String.concat " " (List.map string_of_int (Array.to_list perm))
  in

  let perm_gen =
    QCheck.Gen.(
      sized @@ fun d _st ->
      let d = if d > 10 then 10 else if d < 2 then 2 else d in
      let n = Z.(pow (of_int 2) d |> to_int) in
      let perm = create_random_permutation n in
      perm)
  in
  QCheck.make perm_gen ~print:print_perm

module Internal = struct
  open Permutation_gate.Permutation_gate_impl (Polynomial_protocol)

  let generate_random_polynomial n =
    Poly.of_coefficients (List.init n (fun i -> (Scalar.random (), i)))

  let build_gi_list generator n =
    let rec aux acc i =
      if i = n then List.rev acc
      else
        let g_i_min_1 = List.hd acc in
        aux (Scalar.mul g_i_min_1 generator :: acc) (i + 1)
    in
    aux [generator] 2

  let test_cycles_to_permutation () =
    let cycles_list_to_cycles_map cycles_list =
      let open Partition in
      let cycles_set = List.map IntSet.of_list cycles_list in
      let aux (i, map) s = (i + 1, IntMap.add i s map) in
      let _, cycles = List.fold_left aux (0, IntMap.empty) cycles_set in
      cycles
    in
    let partition =
      [[0; 1; 2; 3]; [4; 5; 6]; [7; 8; 9]] |> cycles_list_to_cycles_map
    in
    let permutation = Partition.partition_to_permutation partition in
    let res = [|1; 2; 3; 0; 5; 6; 4; 8; 9; 7|] in
    assert (permutation = res) ;
    let t = 300 in
    let kn = 1000 in
    (* i-th cycle is the equivalence class modulo t for integers under kn ; cycles is a partition of [0, kn-1] *)
    let cycles_list =
      let cycles_array = Array.init t (fun _ -> []) in
      let rec aux i =
        if i = -1 then Array.to_list cycles_array
        else
          let p = i mod t in
          cycles_array.(p) <- i :: cycles_array.(p) ;
          aux (i - 1)
      in
      aux (kn - 1)
    in
    let partition = cycles_list_to_cycles_map cycles_list in
    let permutation = Partition.partition_to_permutation partition in
    let verify_cycle_in_permutation cycle =
      let a0 = List.hd cycle in
      let rec aux prec l =
        match l with
        | [] -> permutation.(prec) = a0
        | e :: r -> permutation.(prec) = e && aux e r
      in
      (aux a0 (List.tl cycle), List.length cycle)
    in
    let aux (v_acc, k_acc) cycle =
      let v, k = verify_cycle_in_permutation cycle in
      (v && v_acc, k + k_acc)
    in
    let cycles_in_perm, sum_card_cycles =
      List.fold_left aux (true, 0) cycles_list
    in
    (* (cycles ⊆ permutation) & (card(cycles) = card(permutation)) => cycles = permutation *)
    assert cycles_in_perm ;
    assert (sum_card_cycles = Array.length permutation)
end

module External = struct
  module PP = Polynomial_protocol
  module Perm = Permutation_gate.Permutation_gate_impl (PP)

  let nb_wires = 3

  let test_prop_perm_check (perm : int array) =
    let n = Array.length perm in
    let log = Z.(log2up (of_int n)) in
    let domain = Domain.build_power_of_two log in
    let l1 = Perm.Preprocessing.compute_l1 domain in

    (* Check that L1(g^1) = 1 and L(g^i) = 0 for all i <> 1 *)
    let _lconsistency (i, g) =
      let v = Poly.evaluate l1 g in
      if i = 1 then Scalar.is_one v else Scalar.is_zero v
    in
    (*     Array.for_all *)
    (*       lconsistency *)
    (*       (Array.mapi (fun i g -> (i, g)) (Domain.to_array domain)) *)
    (*     && *)
    Perm.srs_size ~zero_knowledge:true ~n = n + 9
    && Perm.srs_size ~zero_knowledge:false ~n = n
end

let tests =
  [
    Alcotest.test_case
      "test_cycles_to_permutation"
      `Quick
      Internal.test_cycles_to_permutation;
    QCheck_alcotest.to_alcotest
      (QCheck.Test.make
         ~count:30
         ~name:"permutation_properties"
         arbitrary_perm
         External.test_prop_perm_check);
  ]
