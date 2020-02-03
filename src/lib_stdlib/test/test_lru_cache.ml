(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module IntTable = Hashtbl.Make (struct
  type t = int

  let equal = Compare.Int.equal

  let hash x = x
end)

module Int_cache = Lru_cache.Make (IntTable)

let test_bad_create _ =
  Alcotest.check_raises
    "below capacity"
    (Invalid_argument "Lru_cache.create: negative or null capacity")
    (fun () ->
      ignore (Int_cache.create ~capacity:(-1)) ;
      Alcotest.fail "Creation with negative capacity")

let test_singleton _ =
  let cache = Int_cache.create ~capacity:1 in
  Int_cache.check_consistency cache ;
  Int_cache.push cache 5 5 ;
  assert (Int_cache.size cache = 1) ;
  assert (Int_cache.is_cached cache 5) ;
  Int_cache.check_consistency cache ;
  Int_cache.push cache 5 5 ;
  assert (Int_cache.is_cached cache 5) ;
  Int_cache.check_consistency cache ;
  Int_cache.push cache 4 4 ;
  assert (Int_cache.is_cached cache 4) ;
  assert (not (Int_cache.is_cached cache 5)) ;
  Int_cache.check_consistency cache ;
  Int_cache.push cache 4 4 ;
  Int_cache.check_consistency cache ;
  Int_cache.push cache 6 6 ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.is_cached cache 6) ;
  assert (not (Int_cache.is_cached cache 4)) ;
  assert (not (Int_cache.is_cached cache 5)) ;
  Int_cache.remove cache 6 ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 0) ;
  assert (not (Int_cache.is_cached cache 6)) ;
  assert (Int_cache.bindings cache = [])

open Utils.Infix

let test_casual_cache _ =
  let cache = Int_cache.create ~capacity:5 in
  Int_cache.check_consistency cache ;
  List.iter (fun i -> Int_cache.push cache i i) (1 -- 5) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 5) ;
  List.iter (fun i -> assert (Int_cache.is_cached cache i)) (1 -- 5) ;
  List.iter (fun i -> Int_cache.push cache i i) (1 -- 5) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 5) ;
  List.iter (fun i -> assert (Int_cache.is_cached cache i)) (1 -- 5) ;
  List.iter (fun i -> Int_cache.push cache i i) (6 -- 9) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 5) ;
  List.iter (fun i -> assert (not (Int_cache.is_cached cache i))) (1 -- 4) ;
  List.iter (fun i -> assert (Int_cache.is_cached cache i)) (5 -- 9) ;
  Int_cache.remove cache 5 ;
  assert (Int_cache.size cache = 4) ;
  Int_cache.check_consistency cache ;
  assert (not (Int_cache.is_cached cache 5)) ;
  let expected_elements = List.rev (List.map (fun i -> (i, i)) (6 -- 9)) in
  List.iter (fun i -> assert (Int_cache.is_cached cache i)) (6 -- 9) ;
  assert (Int_cache.bindings cache = expected_elements) ;
  List.iter (fun i -> Int_cache.remove cache i) (6 -- 9) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 0)

let test_get_cache _ =
  let cache = Int_cache.create ~capacity:10 in
  let fetch x = x in
  let fetch_fail _ = Alcotest.fail "unexpected fetch" in
  List.iter
    (fun i ->
      (* acts as push *)
      let x = Int_cache.get cache fetch i in
      assert (i = x))
    (1 -- 5) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 5) ;
  List.iter (fun i -> assert (Int_cache.is_cached cache i)) (1 -- 5) ;
  List.iter
    (fun i ->
      let x = Int_cache.get cache fetch_fail i in
      assert (i = x) ;
      assert (Int_cache.is_cached cache i))
    (1 -- 5) ;
  Int_cache.check_consistency cache ;
  assert (Int_cache.size cache = 5) ;
  let expected_elements = List.rev (List.map (fun i -> (i, i)) (1 -- 5)) in
  assert (Int_cache.bindings cache = expected_elements)

let tests =
  [ ("bad create", `Quick, test_bad_create);
    ("singleton cache", `Quick, test_singleton);
    ("casual cache", `Quick, test_casual_cache);
    ("get cache", `Quick, test_get_cache) ]

let () = Alcotest.run "stdlib" [("lru_cache", tests)]
