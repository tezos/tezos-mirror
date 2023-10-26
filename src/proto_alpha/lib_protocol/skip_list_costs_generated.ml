(* Do not edit this file manually.
   This file was automatically generated from benchmark models
   If you wish to update a function in this file,
   a. update the corresponding model, or
   b. move the function to another module and edit it there. *)

[@@@warning "-33"]

module S = Saturation_repr
open S.Syntax

(* model skip_list/hash_cell *)
(* fun size -> max 10 (250. + (57. * size)) *)
let cost_hash_cell size = (size * S.safe_int 57) + S.safe_int 250

(* model skip_list/next *)
(* fun size -> max 10 (19.2125537461 * (log2 (1 + size))) *)
let cost_next size =
  let w1 = log2 (size + S.safe_int 1) in
  S.max (S.safe_int 10) ((w1 * S.safe_int 19) + (w1 lsr 1))