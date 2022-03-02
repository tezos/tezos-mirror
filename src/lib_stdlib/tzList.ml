(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let rev_sub l n =
  if n < 0 then invalid_arg "Utils.rev_sub: `n` must be non-negative." ;
  let rec append_rev_sub acc l = function
    | 0 -> acc
    | n -> (
        match l with
        | [] -> acc
        | hd :: tl -> append_rev_sub (hd :: acc) tl (n - 1))
  in
  append_rev_sub [] l n

let sub l n = rev_sub l n |> List.rev

let rec remove nb = function
  | [] -> []
  | l when nb <= 0 -> l
  | _ :: tl -> remove (nb - 1) tl

let rec repeat n x = if n <= 0 then [] else x :: repeat (pred n) x

let split_n n l =
  let rec loop acc n = function
    | [] -> (l, [])
    | rem when n <= 0 -> (List.rev acc, rem)
    | x :: xs -> loop (x :: acc) (pred n) xs
  in
  loop [] n l

let take_n_unsorted n l = fst (split_n n l)

let take_n_sorted (type a) compare n l =
  let module B = Bounded_heap.Make (struct
    type t = a

    let compare = compare
  end) in
  let t = B.create n in
  List.iter (fun x -> B.insert x t) l ;
  B.get t

let take_n ?compare n l =
  match compare with
  | None -> take_n_unsorted n l
  | Some compare -> take_n_sorted compare n l

let rec drop_n n l =
  if n <= 0 then l else match l with [] -> [] | _ :: xs -> drop_n (n - 1) xs
