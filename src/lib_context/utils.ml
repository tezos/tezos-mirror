(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tarides <contact@tarides.com>                          *)
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

include Utils_intf

module Arena : Arena = struct
  type t = {
    elt_length : int;
    mutable data : Bigstring.t;
    mutable next_offset : int;
  }

  (* An offset into a data bigstring. *)
  type id = int

  let create ~elt_length ~initial_capacity =
    {
      elt_length;
      data = Bigstringaf.create (elt_length * initial_capacity);
      next_offset = 0;
    }

  let elt_equal t offset elt =
    Bigstringaf.memcmp_string t.data offset elt 0 t.elt_length = 0

  let is_full t = t.next_offset = Bigstringaf.length t.data

  let allocate t elt =
    if is_full t then Fmt.invalid_arg "Arena.allocate: arena is full" ;
    (* Write the element to the next available arena offset. *)
    let offset = t.next_offset in
    Bigstringaf.blit_from_string
      elt
      ~src_off:0
      t.data
      ~dst_off:offset
      ~len:t.elt_length ;
    t.next_offset <- offset + t.elt_length ;
    offset

  let dereference t off =
    if off + t.elt_length > t.next_offset then
      Fmt.invalid_arg
        "Arena.dereference: reference doesn't belong to this arena" ;
    Bigstringaf.substring t.data ~off ~len:t.elt_length

  let expand t size =
    let old_len = Bigstringaf.length t.data in
    let new_len = size * t.elt_length in
    if new_len < old_len then
      Fmt.invalid_arg "Arena.expand: can't reduce the size of an existing arena" ;
    let new_data = Bigstringaf.create new_len in
    Bigstringaf.blit t.data ~src_off:0 new_data ~dst_off:0 ~len:t.next_offset ;
    t.data <- new_data
end

module Small_list : Small_list = struct
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

  let iter fn = function
    | Tuple0 -> ()
    | Tuple1 a -> fn a
    | Tuple2 (a, b) ->
        fn a ;
        fn b
    | Tuple3 (a, b, c) ->
        fn a ;
        fn b ;
        fn c
    | Tuple4 (a, b, c, d) ->
        fn a ;
        fn b ;
        fn c ;
        fn d
    | Tuple5 (a, b, c, d, e) ->
        fn a ;
        fn b ;
        fn c ;
        fn d ;
        fn e
    | Tuple6 (a, b, c, d, e, f) ->
        fn a ;
        fn b ;
        fn c ;
        fn d ;
        fn e ;
        fn f
    | Many (a, b, c, d, e, f, g, l) ->
        fn a ;
        fn b ;
        fn c ;
        fn d ;
        fn e ;
        fn f ;
        fn g ;
        List.iter fn l

  let exists fn = function
    | Tuple0 -> false
    | Tuple1 a -> fn a
    | Tuple2 (a, b) -> fn a || fn b
    | Tuple3 (a, b, c) -> fn a || fn b || fn c
    | Tuple4 (a, b, c, d) -> fn a || fn b || fn c || fn d
    | Tuple5 (a, b, c, d, e) -> fn a || fn b || fn c || fn d || fn e
    | Tuple6 (a, b, c, d, e, f) -> fn a || fn b || fn c || fn d || fn e || fn f
    | Many (a, b, c, d, e, f, g, l) ->
        fn a || fn b || fn c || fn d || fn e || fn f || fn g || List.exists fn l
end

module String_set : String_set = struct
  (* String elements are stored in an arena (to avoid header words + padding),
     and we keep a hash-set of pointers into the arena. *)
  type t = {arena : Arena.t; mutable hashset : Arena.id Small_list.t array}

  let hash_elt : string -> int = Hashtbl.hash

  let arena_capacity ~bucket_count =
    (* Expand the hashset when there are ~2 elements per bucket *)
    2 * bucket_count

  let create ~elt_length ~initial_capacity =
    let bucket_count = max 1 (initial_capacity / 2) in
    let hashset = Array.make bucket_count Small_list.empty in
    let arena =
      Arena.create ~elt_length ~initial_capacity:(arena_capacity ~bucket_count)
    in
    {hashset; arena}

  let elt_index t elt = hash_elt elt mod Array.length t.hashset

  let mem t elt =
    let bucket = t.hashset.(elt_index t elt) in
    Small_list.exists (fun id -> Arena.elt_equal t.arena id elt) bucket

  let iter_hashset hashset f = Array.iter (Small_list.iter f) hashset

  let resize t =
    (* Scale the number of hashset buckets. *)
    let new_bucket_count = (2 * Array.length t.hashset) + 1 in
    let new_hashset = Array.make new_bucket_count Small_list.empty in
    iter_hashset t.hashset (fun index ->
        let elt = Arena.dereference t.arena index in
        let new_index = hash_elt elt mod new_bucket_count in
        new_hashset.(new_index) <- Small_list.cons index new_hashset.(new_index)) ;
    t.hashset <- new_hashset ;
    (* Scale the arena size. *)
    Arena.expand t.arena (arena_capacity ~bucket_count:new_bucket_count)

  let add t elt =
    if Arena.is_full t.arena then resize t ;
    let arena_idx = Arena.allocate t.arena elt in
    (* Add the arena offset to the hashset. *)
    let elt_idx = elt_index t elt in
    t.hashset.(elt_idx) <- Small_list.cons arena_idx t.hashset.(elt_idx)
end
