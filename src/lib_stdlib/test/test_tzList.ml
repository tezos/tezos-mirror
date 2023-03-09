(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open TzList

let test_repeat _ =
  Assert.equal ~msg:__LOC__ (repeat 0 0) [] ;
  Assert.equal ~msg:__LOC__ (repeat 1 0) [0] ;
  Assert.equal ~msg:__LOC__ (repeat 2 0) [0; 0] ;
  Assert.equal ~msg:__LOC__ (repeat (-1) 0) [] ;
  match repeat 2 (fun x -> x + 1) with
  | [a; b] -> assert (a == b)
  | _ -> assert false

let test_drop_take_split _ =
  let t loc n l =
    let a, b = split_n n l in
    let aa = take_n n l in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) a aa ;
    let bb = drop_n n l in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) b bb ;
    Assert.equal
      ~msg:(string_of_int __LINE__ ^ "/" ^ loc)
      (List.length l)
      (List.length a + List.length b) ;
    let ll = a @ b in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) l ll
  in
  t (string_of_int __LINE__) (-1) [] ;
  t (string_of_int __LINE__) (-1) [0] ;
  t (string_of_int __LINE__) (-1) [0; 1] ;
  t (string_of_int __LINE__) 0 [] ;
  t (string_of_int __LINE__) 0 [0] ;
  t (string_of_int __LINE__) 0 [0; 1] ;
  t (string_of_int __LINE__) 1 [] ;
  t (string_of_int __LINE__) 1 [0] ;
  t (string_of_int __LINE__) 1 [0; 1] ;
  t (string_of_int __LINE__) 2 [] ;
  t (string_of_int __LINE__) 2 [0] ;
  t (string_of_int __LINE__) 2 [0; 1] ;
  t (string_of_int __LINE__) 3 [] ;
  t (string_of_int __LINE__) 3 [0] ;
  t (string_of_int __LINE__) 3 [0; 1]

let test_drop_take_split_rev _ =
  let t loc n l =
    let a, b = rev_split_n n l in
    let aa = rev_take_n n l in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) a aa ;
    let bb = drop_n n l in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) b bb ;
    Assert.equal
      ~msg:(string_of_int __LINE__ ^ "/" ^ loc)
      (List.length l)
      (List.length a + List.length b) ;
    let ll = List.rev_append a b in
    Assert.equal ~msg:(string_of_int __LINE__ ^ "/" ^ loc) l ll
  in
  t (string_of_int __LINE__) (-1) [] ;
  t (string_of_int __LINE__) (-1) [0] ;
  t (string_of_int __LINE__) (-1) [0; 1] ;
  t (string_of_int __LINE__) 0 [] ;
  t (string_of_int __LINE__) 0 [0] ;
  t (string_of_int __LINE__) 0 [0; 1] ;
  t (string_of_int __LINE__) 1 [] ;
  t (string_of_int __LINE__) 1 [0] ;
  t (string_of_int __LINE__) 1 [0; 1] ;
  t (string_of_int __LINE__) 2 [] ;
  t (string_of_int __LINE__) 2 [0] ;
  t (string_of_int __LINE__) 2 [0; 1] ;
  t (string_of_int __LINE__) 3 [] ;
  t (string_of_int __LINE__) 3 [0] ;
  t (string_of_int __LINE__) 3 [0; 1]

let test_split _ =
  Assert.equal ~msg:__LOC__ (TzList.split_n (-1) [1; 2; 3]) ([], [1; 2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.split_n 0 [1; 2; 3]) ([], [1; 2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.split_n 1 [1; 2; 3]) ([1], [2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.split_n 2 [1; 2; 3]) ([1; 2], [3]) ;
  Assert.equal ~msg:__LOC__ (TzList.split_n 3 [1; 2; 3]) ([1; 2; 3], []) ;
  Assert.equal ~msg:__LOC__ (TzList.split_n 4 [1; 2; 3]) ([1; 2; 3], [])

let test_rev_split _ =
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n (-1) [1; 2; 3]) ([], [1; 2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n 0 [1; 2; 3]) ([], [1; 2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n 1 [1; 2; 3]) ([1], [2; 3]) ;
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n 2 [1; 2; 3]) ([2; 1], [3]) ;
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n 3 [1; 2; 3]) ([3; 2; 1], []) ;
  Assert.equal ~msg:__LOC__ (TzList.rev_split_n 4 [1; 2; 3]) ([3; 2; 1], [])

let test_all_equal _ =
  Assert.equal ~msg:__LOC__ (TzList.all_equal ( = ) [1; 1; 1]) true ;
  Assert.equal ~msg:__LOC__ (TzList.all_equal ( = ) [1; 1; 2]) false ;
  Assert.equal
    ~msg:__LOC__
    (TzList.all_equal (fun (x, _) (y, _) -> x = y) [(3, 1); (3, 2); (3, 3)])
    true ;
  Assert.equal
    ~msg:__LOC__
    (TzList.all_equal (fun (x, _) (y, _) -> x = y) [(3, 1); (4, 1); (3, 3)])
    false ;
  Assert.equal ~msg:__LOC__ (TzList.all_equal ( = ) [1]) true ;
  Assert.equal ~msg:__LOC__ (TzList.all_equal ( = ) []) true

let () =
  Alcotest.run
    ~__FILE__
    "stdlib"
    [
      ( "tzList",
        [
          ("repeat", `Quick, test_repeat);
          ("consistency", `Quick, test_drop_take_split);
          ("consistency(rev)", `Quick, test_drop_take_split_rev);
          ("split", `Quick, test_split);
          ("rev_split", `Quick, test_rev_split);
          ("all_equal", `Quick, test_all_equal);
        ] );
    ]
