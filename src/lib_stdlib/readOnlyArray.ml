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

(**

   This is an internal module used to factorize code between {!FallbackArray}
   and {!FunctionalArray}.

*)

type 'a t = 'a Array.t

(**

    [make length fallback] creates an array of [length] + 1
    elements. The final cell is reserved to store the [fallback]
    value.

*)
let make length fallback =
  let length = 1 + max 0 length in
  Array.make length fallback

let init length fallback make_cell =
  let a = make length fallback in
  for i = 0 to length - 1 do
    Array.unsafe_set a i (make_cell i)
  done ;
  a

let fallback array =
  let len = Array.length array in
  Array.unsafe_get array (len - 1)

let get array idx =
  let len = Array.length array in
  if idx >= 0 && idx < len then Array.unsafe_get array idx
  else Array.unsafe_get array (len - 1)

let length array = Array.length array - 1

let iter f array =
  for idx = 0 to length array - 1 do
    f (Array.unsafe_get array idx)
  done

let iteri f array =
  for idx = 0 to length array - 1 do
    f idx (Array.unsafe_get array idx)
  done

let map f array =
  let out = make (length array) (f (fallback array)) in
  for idx = 0 to length array - 1 do
    Array.unsafe_set out idx (f (Array.unsafe_get array idx))
  done ;
  out

let mapi f array =
  let out = make (length array) (f (-1) (fallback array)) in
  for idx = 0 to length array - 1 do
    Array.unsafe_set out idx (f idx (Array.unsafe_get array idx))
  done ;
  out

let fold f array init =
  let rec aux accu idx =
    if idx > length array - 1 then accu
    else aux (f accu (Array.unsafe_get array idx)) (idx + 1)
  in
  aux init 0

let fold_map f array init fallback =
  let output = Array.(make (length array)) fallback in
  let rec aux accu idx =
    if idx > length array - 1 then accu
    else
      let accu, y = f accu (Array.unsafe_get array idx) in
      Array.unsafe_set output idx y ;
      aux accu (idx + 1)
  in
  (aux init 0, output)
