(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let load_json file =
  Result.catch
    (fun () -> In_channel.with_open_text file Ezjsonm.from_channel)
    ~catch_only:(function Sys_error _ -> true | _ -> false)

let make_progress_printer fmtr total message =
  let counter = ref 1 in
  fun () ->
    Format.fprintf fmtr "\r%s %d/%d%!" message !counter total ;
    incr counter

let int_encoding : int Data_encoding.encoding =
  let open Data_encoding in
  conv (fun i -> Int64.of_int i) (fun l -> Int64.to_int l) int64

(* -------------------------------------------------------------------------- *)
(* Enumerate all ways of picking n elements from a list (ie all injections
   from [n] to [l]) *)

let rec enumerate_injections n l current_pick acc =
  if n = 0 then List.rev current_pick :: acc
  else enumerate_picks n l current_pick acc

and enumerate_picks n l current_pick acc =
  match l with
  | [] -> acc
  | x :: tl ->
      let extended_pick = x :: current_pick in
      let acc = enumerate_injections (n - 1) tl extended_pick acc in
      enumerate_picks n tl current_pick acc

let enumerate_subsets n l = enumerate_injections n l [] []

(* -------------------------------------------------------------------------- *)
(* Seq helpers *)

let seq_progress fmtr msg seq =
  let c = ref 1 in
  Seq.map
    (fun x ->
      Format.fprintf fmtr "\r%s (%d)%!" msg !c ;
      incr c ;
      x)
    seq

(* To be removed when in OCaml stdlib. The code below this line is under the
   following license: *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let rec take_aux n xs =
  if n = 0 then Seq.empty
  else fun () ->
    match xs () with
    | Seq.Nil -> Nil
    | Cons (x, xs) -> Cons (x, take_aux (n - 1) xs)

let take n xs =
  if n < 0 then invalid_arg "Seq.take" ;
  take_aux n xs

let rec force_drop n xs =
  match xs () with
  | Seq.Nil -> Seq.Nil
  | Cons (_, xs) ->
      let n = n - 1 in
      if n = 0 then xs () else force_drop n xs

let drop n xs =
  if n < 0 then invalid_arg "Seq.drop"
  else if n = 0 then xs
  else fun () -> force_drop n xs

let rec iteri_aux f i xs =
  match xs () with
  | Seq.Nil -> ()
  | Cons (x, xs) ->
      f i x ;
      iteri_aux f (i + 1) xs

let[@inline] iteri f xs = iteri_aux f 0 xs

let rec map2 f xs ys () =
  match xs () with
  | Seq.Nil -> Seq.Nil
  | Cons (x, xs) -> (
      match ys () with
      | Seq.Nil -> Nil
      | Cons (y, ys) -> Cons (f x y, map2 f xs ys))
