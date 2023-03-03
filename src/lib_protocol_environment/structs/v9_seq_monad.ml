(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

include Tezos_error_monad.TzLwtreslib.Seq

let first s = find (fun _ -> true) s

let fold_left_e = E.fold_left

let fold_left_s = S.fold_left

let fold_left_es = ES.fold_left

let iter_e = E.iter

let iter_s = S.iter

let iter_es = ES.iter

let iteri_e = E.iteri

let iteri_s = S.iteri

let iteri_es = ES.iteri

let fold_lefti_e = E.fold_lefti

let fold_lefti_s = S.fold_lefti

let fold_lefti_es = ES.fold_lefti

let for_all_e = E.for_all

let for_all_s = S.for_all

let for_all_es = ES.for_all

let exists_e = E.exists

let exists_s = S.exists

let exists_es = ES.exists

let find_e = E.find

let find_s = S.find

let find_es = ES.find

let find_map_e = E.find_map

let find_map_s = S.find_map

let find_map_es = ES.find_map

let iter2_e = E.iter2

let iter2_s = S.iter2

let iter2_es = ES.iter2

let fold_left2_e = E.fold_left2

let fold_left2_s = S.fold_left2

let fold_left2_es = ES.fold_left2

let for_all2_e = E.for_all2

let for_all2_s = S.for_all2

let for_all2_es = ES.for_all2

let exists2_e = E.exists2

let exists2_s = S.exists2

let exists2_es = ES.exists2
