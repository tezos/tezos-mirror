(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* The state of rewriting is a typed term *)
type t = {typing : Inference.state lazy_t; term : Mikhailsky.node}

let compare (term1 : t) (term2 : t) =
  let tag1 = Mikhailsky.tag term1.term in
  let tag2 = Mikhailsky.tag term2.term in
  if tag1 < tag2 then -1 else if tag1 > tag2 then 1 else 0

let equal (term1 : t) (term2 : t) =
  let tag1 = Mikhailsky.tag term1.term in
  let tag2 = Mikhailsky.tag term2.term in
  tag1 = tag2

let hash (t : t) = Mikhailsky.hash t.term

type node_statistics = {
  mutable size : int;
  mutable bytes : int;
  mutable holes : int;
  mutable depth : int;
}

let pp_statistics fmtr stats =
  Format.fprintf
    fmtr
    "{ size = %d ; bytes = %d ; holes = %d }"
    stats.size
    stats.bytes
    stats.holes

let rec statistics stats depth (n : Mikhailsky.node) =
  stats.size <- stats.size + 1 ;
  stats.depth <- max depth stats.depth ;
  match n with
  | Micheline.Int (_, z) -> stats.bytes <- stats.bytes + (Z.numbits z / 8)
  | Micheline.String (_, s) -> stats.bytes <- stats.bytes + String.length s
  | Micheline.Bytes (_, b) -> stats.bytes <- stats.bytes + Bytes.length b
  | Micheline.Prim (_, Mikhailsky_prim.I_Hole, _, _)
  | Micheline.Prim (_, Mikhailsky_prim.D_Hole, _, _) ->
      stats.holes <- stats.holes + 1
  | Micheline.Prim (_, _, subterms, _) | Micheline.Seq (_, subterms) ->
      List.iter (statistics stats (depth + 1)) subterms

let statistics {term; _} =
  let stats = {size = 0; bytes = 0; holes = 0; depth = 0} in
  statistics stats 0 term ;
  stats

let pp fmtr (state : t) =
  Format.fprintf fmtr "current term:@." ;
  Format.fprintf fmtr "%a@." Mikhailsky.pp state.term ;
  Format.fprintf fmtr "stats:@." ;
  Format.fprintf fmtr "%a:@." pp_statistics (statistics state)
