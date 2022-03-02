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

let is_empty = function [] -> true | _ -> false

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

let select n l =
  let rec loop n acc = function
    | [] -> invalid_arg "Utils.select"
    | x :: xs when n <= 0 -> (x, List.rev_append acc xs)
    | x :: xs -> loop (pred n) (x :: acc) xs
  in
  loop n [] l

let rec product a b =
  match a with
  | [] -> []
  | hd :: tl -> List.map (fun x -> (hd, x)) b @ product tl b

(* Use Fisher-Yates shuffle as described by Knuth
   https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle *)
let shuffle ?rng_state l =
  let randint =
    match rng_state with
    | None -> Random.int
    | Some rng_state -> Random.State.int rng_state
  in
  let a = Array.of_list l in
  let len = Array.length a in
  for i = len downto 2 do
    let m = randint i in
    let n' = i - 1 in
    if m <> n' then (
      let tmp = a.(m) in
      a.(m) <- a.(n') ;
      a.(n') <- tmp)
  done ;
  Array.to_list a

let index_of ?(compare = Stdlib.compare) item list =
  let rec find index = function
    | [] -> None
    | head :: tail ->
        if compare head item = 0 then Some index else find (index + 1) tail
  in
  find 0 list

let filter_some l =
  (* Written like this (building backward and with a final List.rev)
     so that it is tail-recursive *)
  let rec go acc = function
    | [] -> List.rev acc
    | None :: xs -> go acc xs
    | Some x :: xs -> go (x :: acc) xs
  in
  go [] l

let rec find_map f = function
  | [] -> None
  | x :: l -> ( match f x with None -> find_map f l | r -> r)

let fold_left_i f init l =
  List.fold_left
    (fun (i, accu) x ->
      let accu = f i accu x in
      (i + 1, accu))
    (0, init)
    l
  |> snd
