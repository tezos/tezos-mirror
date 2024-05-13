(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type 'a t =
  | Tuple0
  | Tuple1 of 'a
  | Tuple2 of 'a * 'a
  | Tuple3 of 'a * 'a * 'a
  | Tuple4 of 'a * 'a * 'a * 'a
  | Tuple5 of 'a * 'a * 'a * 'a * 'a
  | Tuple6 of 'a * 'a * 'a * 'a * 'a * 'a
  | Many of 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a list

let empty = Tuple0

let cons x = function
  | Tuple0 -> Tuple1 x
  | Tuple1 a -> Tuple2 (x, a)
  | Tuple2 (a, b) -> Tuple3 (x, a, b)
  | Tuple3 (a, b, c) -> Tuple4 (x, a, b, c)
  | Tuple4 (a, b, c, d) -> Tuple5 (x, a, b, c, d)
  | Tuple5 (a, b, c, d, e) -> Tuple6 (x, a, b, c, d, e)
  | Tuple6 (a, b, c, d, e, f) -> Many (x, a, b, c, d, e, f, [])
  | Many (a, b, c, d, e, f, g, l) -> Many (x, a, b, c, d, e, f, g :: l)

let map ~f:fn = function
  | Tuple0 -> Tuple0
  | Tuple1 a -> Tuple1 (fn a)
  | Tuple2 (a, b) -> Tuple2 (fn a, fn b)
  | Tuple3 (a, b, c) -> Tuple3 (fn a, fn b, fn c)
  | Tuple4 (a, b, c, d) -> Tuple4 (fn a, fn b, fn c, fn d)
  | Tuple5 (a, b, c, d, e) -> Tuple5 (fn a, fn b, fn c, fn d, fn e)
  | Tuple6 (a, b, c, d, e, f) -> Tuple6 (fn a, fn b, fn c, fn d, fn e, fn f)
  | Many (a, b, c, d, e, f, g, l) ->
      Many (fn a, fn b, fn c, fn d, fn e, fn f, fn g, List.map fn l)

let iter ~f:fn = function
  | Tuple0 -> ()
  | Tuple1 a -> fn a
  | Tuple2 (a, b) ->
      fn a;
      fn b
  | Tuple3 (a, b, c) ->
      fn a;
      fn b;
      fn c
  | Tuple4 (a, b, c, d) ->
      fn a;
      fn b;
      fn c;
      fn d
  | Tuple5 (a, b, c, d, e) ->
      fn a;
      fn b;
      fn c;
      fn d;
      fn e
  | Tuple6 (a, b, c, d, e, f) ->
      fn a;
      fn b;
      fn c;
      fn d;
      fn e;
      fn f
  | Many (a, b, c, d, e, f, g, l) ->
      fn a;
      fn b;
      fn c;
      fn d;
      fn e;
      fn f;
      fn g;
      List.iter fn l

let exists ~f:fn = function
  | Tuple0 -> false
  | Tuple1 a -> fn a
  | Tuple2 (a, b) -> fn a || fn b
  | Tuple3 (a, b, c) -> fn a || fn b || fn c
  | Tuple4 (a, b, c, d) -> fn a || fn b || fn c || fn d
  | Tuple5 (a, b, c, d, e) -> fn a || fn b || fn c || fn d || fn e
  | Tuple6 (a, b, c, d, e, f) -> fn a || fn b || fn c || fn d || fn e || fn f
  | Many (a, b, c, d, e, f, g, l) ->
      fn a || fn b || fn c || fn d || fn e || fn f || fn g || List.exists fn l

(* TODO(4.10): use [Stdlib.List.find_map] instead. *)
let rec list_find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> list_find_map f l)

let[@ocamlformat "disable"] find_map ~f:fn = function
  | Tuple0 -> raise Not_found
  | Tuple1 a -> fn a
  | Tuple2 (a, b) -> (
      match fn a with Some _ as r -> r | None ->
        fn b)
  | Tuple3 (a, b, c) -> (
      match fn a with Some _ as r -> r | None ->
      match fn b with Some _ as r -> r | None ->
        fn c)
  | Tuple4 (a, b, c, d) -> (
      match fn a with Some _ as r -> r | None ->
      match fn b with Some _ as r -> r | None ->
      match fn c with Some _ as r -> r | None ->
        fn d)
  | Tuple5 (a, b, c, d, e) -> (
      match fn a with Some _ as r -> r | None ->
      match fn b with Some _ as r -> r | None ->
      match fn c with Some _ as r -> r | None ->
      match fn d with Some _ as r -> r | None ->
        fn e)
  | Tuple6 (a, b, c, d, e, f) -> (
      match fn a with Some _ as r -> r | None ->
      match fn b with Some _ as r -> r | None ->
      match fn c with Some _ as r -> r | None ->
      match fn d with Some _ as r -> r | None ->
      match fn e with Some _ as r -> r | None ->
        fn f)
  | Many (a, b, c, d, e, f, g, l) -> (
      match fn a with Some _ as r -> r | None ->
      match fn b with Some _ as r -> r | None ->
      match fn c with Some _ as r -> r | None ->
      match fn d with Some _ as r -> r | None ->
      match fn e with Some _ as r -> r | None ->
      match fn f with Some _ as r -> r | None ->
      match fn g with Some _ as r -> r | None ->
        list_find_map fn l)

let to_list = function
  | Tuple0 -> []
  | Tuple1 a -> [ a ]
  | Tuple2 (a, b) -> [ a; b ]
  | Tuple3 (a, b, c) -> [ a; b; c ]
  | Tuple4 (a, b, c, d) -> [ a; b; c; d ]
  | Tuple5 (a, b, c, d, e) -> [ a; b; c; d; e ]
  | Tuple6 (a, b, c, d, e, f) -> [ a; b; c; d; e; f ]
  | Many (a, b, c, d, e, f, g, l) -> a :: b :: c :: d :: e :: f :: g :: l

let to_array = function
  | Tuple0 -> [||]
  | Tuple1 a -> [| a |]
  | Tuple2 (a, b) -> [| a; b |]
  | Tuple3 (a, b, c) -> [| a; b; c |]
  | Tuple4 (a, b, c, d) -> [| a; b; c; d |]
  | Tuple5 (a, b, c, d, e) -> [| a; b; c; d; e |]
  | Tuple6 (a, b, c, d, e, f) -> [| a; b; c; d; e; f |]
  | Many (a, b, c, d, e, f, g, l) ->
      let len = 7 + List.length l in
      let arr = Array.make len a in
      arr.(1) <- b;
      arr.(2) <- c;
      arr.(3) <- d;
      arr.(4) <- e;
      arr.(5) <- f;
      arr.(6) <- g;
      List.iteri (fun i elt -> arr.(i + 7) <- elt) l;
      arr

let of_list = function
  | [] -> Tuple0
  | [ a ] -> Tuple1 a
  | [ a; b ] -> Tuple2 (a, b)
  | [ a; b; c ] -> Tuple3 (a, b, c)
  | [ a; b; c; d ] -> Tuple4 (a, b, c, d)
  | [ a; b; c; d; e ] -> Tuple5 (a, b, c, d, e)
  | [ a; b; c; d; e; f ] -> Tuple6 (a, b, c, d, e, f)
  | a :: b :: c :: d :: e :: f :: g :: l -> Many (a, b, c, d, e, f, g, l)

let fold_left ~f:fn ~init = function
  | Tuple0 -> init
  | Tuple1 a -> fn init a
  | Tuple2 (a, b) -> fn (fn init a) b
  | Tuple3 (a, b, c) -> fn (fn (fn init a) b) c
  | Tuple4 (a, b, c, d) -> fn (fn (fn (fn init a) b) c) d
  | Tuple5 (a, b, c, d, e) -> fn (fn (fn (fn (fn init a) b) c) d) e
  | Tuple6 (a, b, c, d, e, f) -> fn (fn (fn (fn (fn (fn init a) b) c) d) e) f
  | Many (a, b, c, d, e, f, g, l) ->
      List.fold_left fn (fn (fn (fn (fn (fn (fn (fn init a) b) c) d) e) f) g) l
