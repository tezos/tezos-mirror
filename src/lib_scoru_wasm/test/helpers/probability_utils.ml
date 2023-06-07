(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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

open Tezos_test_helpers

(* Probability is in range between 0 and 1 *)
module type Probability_sig = sig
  type t

  val of_percent : int -> t

  val to_percent : t -> int

  val ( < ) : t -> t -> bool

  val ( + ) : t -> t -> t
end

module Probability : Probability_sig = struct
  type t = int

  let of_percent x =
    Assert.Int.leq
      ~loc:__LOC__
      ~msg:"Probability should be greater or equal than 0.0"
      0
      x ;
    Assert.Int.leq
      ~loc:__LOC__
      ~msg:"Probability should be less or equal than 1.0"
      x
      100 ;
    x

  let to_percent = Fun.id

  let ( < ) x y = x < y

  let ( + ) x y =
    Assert.Int.leq
      ~loc:__LOC__
      ~msg:"Sum of probabilities should be less or equal than 1.0"
      (x + y)
      100 ;
    x + y
end

module Distributions = struct
  let uniform_distribution_l (a : 'a list) : (int * 'a) list =
    List.map (fun x -> (1, x)) a

  (* This one generates: n, n - 1, ... 1 weights.
     So the first operation will occur n times more often than the last one. *)
  let descending_distribution_l (a : 'a list) : (int * 'a) list =
    let n = List.length a in
    List.mapi (fun i x -> (n - i, x)) a

  (* This one generates: 1, 2 ... n/2 - 1, n/2, n/2, n/2 - 1 ... 1*)
  let centered_distribution_l (a : 'a list) : (int * 'a) list =
    let n = List.length a in
    let n2 = n - Int.div n 2 in
    List.mapi (fun i x -> ((if i < n2 then i + 1 else n - i), x)) a

  (* Generates 0, ... 1, ... 0, 0, where 1 is n-th position*)
  let one_of_n n (a : 'a list) : (int * 'a) list =
    List.mapi (fun i x -> if i = n then (1, x) else (0, x)) a
end
