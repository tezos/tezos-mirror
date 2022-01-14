(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let mean list =
  let count = ref 0 in
  let sum = ref 0. in
  List.iter
    (fun value ->
      incr count ;
      sum := !sum +. value)
    list ;
  !sum /. float !count

let median list =
  let sorted = List.sort Float.compare list |> Array.of_list in
  let count = Array.length sorted in
  if count > 0 then
    if count mod 2 = 0 then
      let i = count / 2 in
      (sorted.(i - 1) +. sorted.(i)) /. 2.
    else sorted.(count / 2)
  else invalid_arg "Long_test.median: empty list"

let stddev list =
  let list_mean = mean list in
  let squared_diffs =
    List.map
      (fun x ->
        let d = x -. list_mean in
        d *. d)
      list
  in
  sqrt (mean squared_diffs)
