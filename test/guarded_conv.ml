(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let int8s = Data_encoding.(list int8)

(* A first series of tests that defines an encoding for non-empty lists, the
   decoding of which is guarded by conv_with_guard to prevent the creation of empty
   non-empty lists. *)

module NonEmptyList : sig
  type 'a t = private 'a list

  val hd : 'a t -> 'a

  val cons : 'a -> 'a list -> 'a t

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end = struct
  type 'a t = 'a list

  let cons x xs = x :: xs

  let hd = function [] -> assert false | x :: _ -> x

  let encoding e =
    let open Data_encoding in
    conv_with_guard
      (fun xs -> xs)
      (function
        | [] -> Error "NonEmptyList cannot be empty" | _ :: _ as xs -> Ok xs)
      (list e)
end

let non_empty_int8s = NonEmptyList.encoding Data_encoding.int8

(* Checking that it encodes/decodes correctly. *)
let non_empty_list_roundtrip x xs =
  let data = NonEmptyList.cons x xs in
  let s = Data_encoding.Binary.to_string non_empty_int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  let d = Data_encoding.Binary.of_string non_empty_int8s s in
  assert (Stdlib.Result.is_ok d) ;
  assert (Stdlib.Result.get_ok d = data) ;
  ()

let non_empty_list_roundtrip () =
  non_empty_list_roundtrip 0 [] ;
  non_empty_list_roundtrip 0 [1] ;
  non_empty_list_roundtrip 0 [0; 0] ;
  non_empty_list_roundtrip 4 [0; 0] ;
  non_empty_list_roundtrip 10 [9; 8; 4; 3; 1; 0; 0; 0; 0; 0] ;
  ()

(* Checking that we can decode encoded lists when they are not empty *)
let non_empty_list_is_a_list_that_is_not_empty xs =
  let data = xs in
  (* encode with list *)
  let s = Data_encoding.Binary.to_string int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  (* decode with non-empty-list *)
  let d = Data_encoding.Binary.of_string non_empty_int8s s in
  assert (Stdlib.Result.is_ok d) ;
  let d = Stdlib.Result.get_ok d in
  assert ((d : int NonEmptyList.t :> int list) = data) ;
  ()

let non_empty_list_is_a_list_that_is_not_empty () =
  non_empty_list_is_a_list_that_is_not_empty [0] ;
  non_empty_list_is_a_list_that_is_not_empty [1] ;
  non_empty_list_is_a_list_that_is_not_empty [1; 2; 3; 4; 5] ;
  non_empty_list_is_a_list_that_is_not_empty [0; 1; 2; 4; 8] ;
  ()

(* Checking that we cannot decode encoded lists when they are empty *)
let non_empty_list_is_not_an_empty_list () =
  let data = [] in
  (* encode with list *)
  let s = Data_encoding.Binary.to_string int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  (* decode with non-empty-list *)
  let d = Data_encoding.Binary.of_string non_empty_int8s s in
  assert (Stdlib.Result.is_error d) ;
  let d = Stdlib.Result.get_error d in
  assert (
    match d with
    | Data_encoding.Binary.User_invariant_guard _ -> true
    | _ -> false) ;
  ()

(* A second series of tests that defines an encoding for sorted lists. *)

module SortedList : sig
  type 'a t = private 'a list

  val of_list : 'a list -> 'a t

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end = struct
  type 'a t = 'a list

  let of_list xs = List.sort compare xs

  let encoding e =
    let open Data_encoding in
    conv_with_guard
      (fun xs -> xs)
      (fun xs ->
        let ys = of_list xs in
        if xs = ys then Ok ys else Error "SortedList must be sorted")
      (list e)
end

let sorted_int8s = SortedList.encoding Data_encoding.int8

(* Checking that it encodes/decodes correctly. *)
let sorted_list_roundtrips xs =
  let data = SortedList.of_list xs in
  let s = Data_encoding.Binary.to_string sorted_int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  let d = Data_encoding.Binary.of_string sorted_int8s s in
  assert (Stdlib.Result.is_ok d) ;
  assert (Stdlib.Result.get_ok d = data) ;
  ()

let sorted_list_roundtrips () =
  sorted_list_roundtrips [] ;
  sorted_list_roundtrips [0] ;
  sorted_list_roundtrips [0; 1] ;
  sorted_list_roundtrips [0; 2] ;
  sorted_list_roundtrips [0; 1; 2] ;
  sorted_list_roundtrips [0; 0; 0; 0; 0; 0] ;
  sorted_list_roundtrips [1; 5; 10; 23; 76; 95] ;
  ()

(* Checking that we can decode encoded lists when they are sorted *)
let sorted_list_is_a_list_that_is_sorted xs =
  let data = List.sort compare xs in
  (* encode with list *)
  let s = Data_encoding.Binary.to_string int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  (* decode with sorted-list *)
  let d = Data_encoding.Binary.of_string sorted_int8s s in
  assert (Stdlib.Result.is_ok d) ;
  let d = Stdlib.Result.get_ok d in
  assert ((d : int SortedList.t :> int list) = data) ;
  ()

let sorted_list_is_a_list_that_is_sorted () =
  sorted_list_is_a_list_that_is_sorted [] ;
  sorted_list_is_a_list_that_is_sorted [1] ;
  sorted_list_is_a_list_that_is_sorted [0; 1; 3] ;
  sorted_list_is_a_list_that_is_sorted [0; 0; 0; 0; 0; 0; 0; 0; 0; 0] ;
  ()

(* Checking that we cannot decode encoded lists when they are not sorted *)
let sorted_list_is_not_a_list_that_is_not_sorted xs =
  let data = xs in
  (* encode with list *)
  let s = Data_encoding.Binary.to_string int8s data in
  assert (Stdlib.Result.is_ok s) ;
  let s = Stdlib.Result.get_ok s in
  (* decode with sorted-list *)
  let d = Data_encoding.Binary.of_string sorted_int8s s in
  assert (Stdlib.Result.is_error d) ;
  let d = Stdlib.Result.get_error d in
  assert (
    match d with
    | Data_encoding.Binary.User_invariant_guard _ -> true
    | _ -> false) ;
  ()

let sorted_list_is_not_a_list_that_is_not_sorted () =
  sorted_list_is_not_a_list_that_is_not_sorted [1; 0] ;
  sorted_list_is_not_a_list_that_is_not_sorted [1; 3; 0] ;
  sorted_list_is_not_a_list_that_is_not_sorted [0; 0; 1; 2; 4; 3; 0; 0; 0; 0] ;
  ()

let tests =
  [
    ("non-empty roundtrip", `Quick, non_empty_list_roundtrip);
    ("non-empty is a list", `Quick, non_empty_list_is_a_list_that_is_not_empty);
    ("non-empty is not empty", `Quick, non_empty_list_is_not_an_empty_list);
    ("sorted roundtrip", `Quick, sorted_list_roundtrips);
    ("sorted is a list", `Quick, sorted_list_is_a_list_that_is_sorted);
    ("sorted is not empty", `Quick, sorted_list_is_not_a_list_that_is_not_sorted);
  ]
