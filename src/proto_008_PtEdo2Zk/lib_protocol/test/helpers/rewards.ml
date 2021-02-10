(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2019 Cryptium Labs <hello@cryptium.ch>                      *)
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

(** The tables are precomputed using this the following formulas:

let max_endos = 32
let max_reward = 80

let r = 0.5
let a = 6.
let b = 1.5

let ( -- ) i j = List.init (j - i + 1) (fun x -> x + i)

let baking_rewards =
  let reward p e =
    let r_aux =
      if p = 0 then
        r *. (float_of_int max_reward)
      else
        a
    in
    let r = r_aux *. (float_of_int e) /. (float_of_int max_endos) in
    let r = 1_000_000. *. r in
    Float.to_int (floor r) in

  let ps = 0 -- 2 in
  let es = 0 -- 32 in

  List.map (fun p ->
      List.map (fun e ->
          reward p e
        ) es |> Array.of_list
    ) ps |> Array.of_list


let endorsing_rewards =
  let reward p e =
    let r_aux =
                (1. -. r) *.
                (float_of_int max_reward) /.
                (float_of_int max_endos) in
    let r = if p = 0 then r_aux else r_aux /. b in
    let r = 1_000_000. *. r in
    Float.to_int ((float_of_int e) *. (floor r)) in

  let ps = 0 -- 2 in
  let es = 0 -- 32 in

  List.map (fun p ->
      List.map (fun e ->
          reward p e
        ) es |> Array.of_list
    ) ps |> Array.of_list

  *)

let baking_rewards : int array array =
  [| [| 0;
        1250000;
        2500000;
        3750000;
        5000000;
        6250000;
        7500000;
        8750000;
        10000000;
        11250000;
        12500000;
        13750000;
        15000000;
        16250000;
        17500000;
        18750000;
        20000000;
        21250000;
        22500000;
        23750000;
        25000000;
        26250000;
        27500000;
        28750000;
        30000000;
        31250000;
        32500000;
        33750000;
        35000000;
        36250000;
        37500000;
        38750000;
        40000000 |];
     [| 0;
        187500;
        375000;
        562500;
        750000;
        937500;
        1125000;
        1312500;
        1500000;
        1687500;
        1875000;
        2062500;
        2250000;
        2437500;
        2625000;
        2812500;
        3000000;
        3187500;
        3375000;
        3562500;
        3750000;
        3937500;
        4125000;
        4312500;
        4500000;
        4687500;
        4875000;
        5062500;
        5250000;
        5437500;
        5625000;
        5812500;
        6000000 |];
     [| 0;
        187500;
        375000;
        562500;
        750000;
        937500;
        1125000;
        1312500;
        1500000;
        1687500;
        1875000;
        2062500;
        2250000;
        2437500;
        2625000;
        2812500;
        3000000;
        3187500;
        3375000;
        3562500;
        3750000;
        3937500;
        4125000;
        4312500;
        4500000;
        4687500;
        4875000;
        5062500;
        5250000;
        5437500;
        5625000;
        5812500;
        6000000 |] |]

let endorsing_rewards : int array array =
  [| [| 0;
        1250000;
        2500000;
        3750000;
        5000000;
        6250000;
        7500000;
        8750000;
        10000000;
        11250000;
        12500000;
        13750000;
        15000000;
        16250000;
        17500000;
        18750000;
        20000000;
        21250000;
        22500000;
        23750000;
        25000000;
        26250000;
        27500000;
        28750000;
        30000000;
        31250000;
        32500000;
        33750000;
        35000000;
        36250000;
        37500000;
        38750000;
        40000000 |];
     [| 0;
        833333;
        1666666;
        2499999;
        3333332;
        4166665;
        4999998;
        5833331;
        6666664;
        7499997;
        8333330;
        9166663;
        9999996;
        10833329;
        11666662;
        12499995;
        13333328;
        14166661;
        14999994;
        15833327;
        16666660;
        17499993;
        18333326;
        19166659;
        19999992;
        20833325;
        21666658;
        22499991;
        23333324;
        24166657;
        24999990;
        25833323;
        26666656 |];
     [| 0;
        833333;
        1666666;
        2499999;
        3333332;
        4166665;
        4999998;
        5833331;
        6666664;
        7499997;
        8333330;
        9166663;
        9999996;
        10833329;
        11666662;
        12499995;
        13333328;
        14166661;
        14999994;
        15833327;
        16666660;
        17499993;
        18333326;
        19166659;
        19999992;
        20833325;
        21666658;
        22499991;
        23333324;
        24166657;
        24999990;
        25833323;
        26666656 |] |]
