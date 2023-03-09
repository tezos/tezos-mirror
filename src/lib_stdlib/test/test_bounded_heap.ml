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

(** Testing
    _______

    Invocation: dune build @src/lib_stdlib/test/runtest
 *)

module Assert = Assert
module B = Bounded_heap.Make (Int)

let take_nth_biggest n l =
  let rec take_n acc i l =
    if i = n then acc
    else match l with [] -> acc | x :: xs -> take_n (x :: acc) (i + 1) xs
  in
  take_n [] 0 (List.sort (fun x y -> -Compare.Int.compare x y) l)

(* At least 2 elements, since we'll create a bounded set of size #elements / 2
   *)
let size = QCheck2.Gen.int_range 2 1000

(* Checks whether inserting the elements of list [l] of size [2 * n] inside a
   bounded heap of size [n], and getting its list view gives the same result as
   sorting list [l] and taking the first [n] elements. *)
let test_bounded_heap =
  QCheck2.Test.make
    ~name:"bounded_heap (qcheck)"
    ~count:1000
    ~print:QCheck2.Print.(list int)
    QCheck2.(Gen.list_size size Gen.int)
    (fun l ->
      let sz = List.length l / 2 in
      QCheck2.assume (sz > 0) ;
      let t = B.create sz in
      List.iter (fun x -> B.insert x t) l ;
      let contents = B.get t in
      let biggest = take_nth_biggest sz l in
      let ln = List.length contents in
      let pp_list =
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          Format.pp_print_int
      in
      Qcheck2_helpers.qcheck_eq'
        ~pp:Format.pp_print_int
        ~eq:( = )
        ~actual:ln
        ~expected:sz
        ()
      && Qcheck2_helpers.qcheck_eq'
           ~pp:pp_list
           ~eq:( = )
           ~actual:contents
           ~expected:biggest
           ())

let test_empty_works () =
  let b = B.create 0 in
  Assert.equal ~msg:__LOC__ (B.get b) [] ;
  Assert.equal ~msg:__LOC__ (B.peek b) None ;
  B.insert 1 b ;
  Assert.equal ~msg:__LOC__ (B.get b) []

let () =
  Alcotest.run
    ~__FILE__
    "stdlib"
    [
      ( "Bounded_heap",
        [
          ("create 0 works", `Quick, test_empty_works);
          QCheck_alcotest.to_alcotest test_bounded_heap;
        ] );
    ]
