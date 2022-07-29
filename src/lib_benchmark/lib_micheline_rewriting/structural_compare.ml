(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Structural comparison of Michelson values (ignoring locations and
   annotations). *)

let rec compare :
    prim_compare:('prim -> 'prim -> int) ->
    ('annot, 'prim) Micheline.node ->
    ('annot, 'prim) Micheline.node ->
    int =
 fun ~prim_compare node1 node2 ->
  match (node1, node2) with
  | Int (_, z1), Int (_, z2) -> Z.compare z1 z2
  | Int _, _ -> -1
  | String _, Int _ -> 1
  | String (_, s1), String (_, s2) -> String.compare s1 s2
  | String _, _ -> -1
  | Bytes _, Int _ | Bytes _, String _ -> 1
  | Bytes (_, b1), Bytes (_, b2) -> Bytes.compare b1 b2
  | Bytes _, _ -> -1
  | Prim _, Int _ | Prim _, String _ | Prim _, Bytes _ -> 1
  | Prim (_, prim1, subterms1, _), Prim (_, prim2, subterms2, _) ->
      let c = prim_compare prim1 prim2 in
      if c <> 0 then c else list_compare ~prim_compare subterms1 subterms2
  | Prim _, _ -> -1
  | Seq _, Int _ | Seq _, String _ | Seq _, Bytes _ | Seq _, Prim _ -> 1
  | Seq (_, subterms1), Seq (_, subterms2) ->
      list_compare ~prim_compare subterms1 subterms2

and list_compare ~prim_compare subterms1 subterms2 =
  match (subterms1, subterms2) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | hd1 :: tl1, hd2 :: tl2 ->
      let c = compare ~prim_compare hd1 hd2 in
      if c <> 0 then c else list_compare ~prim_compare tl1 tl2
