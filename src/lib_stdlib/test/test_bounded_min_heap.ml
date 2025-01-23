(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Standard library
    Invocation:   dune exec src/lib_stdlib/test/main.exe \
                -- -f src/lib_stdlib/test/test_bounded_min_heap.ml
    Subject:    Unit tests bounded min heap
*)

module Assert = Assert

module Id = struct
  include String

  let hash = Stdlib.Hashtbl.hash
end

let id i = string_of_int i

module B =
  Bounded_min_heap.Make
    (Id)
    (struct
      include Int

      let id = id
    end)

let take_nth_smallest n l =
  TzList.take_n n (List.sort (fun x y -> Compare.Int.compare x y) l)

(* At least 2 elements, since we'll create a bounded set of size #elements / 2
   *)
let size = QCheck2.Gen.int_range 2 1000

let check_invariant b =
  B.Internal_for_tests.check_heap_invariant
    ~pp_id:Format.pp_print_string
    ~pp_elt:Format.pp_print_int
    b

let check_insert_heap ?(expect_heap_is_full = false) v b =
  let res = B.insert v b in
  let () =
    if expect_heap_is_full then
      let error = "no space left in heap" in
      Assert.equal ~msg:__LOC__ res (Error error)
    else Assert.equal ~msg:__LOC__ res (Ok ())
  in
  check_invariant b

let test_empty_works () =
  let b = B.create 0 in
  Assert.equal ~msg:__LOC__ (B.elements b) [] ;
  Assert.equal ~msg:__LOC__ (B.pop b) None ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) None ;
  check_insert_heap ~expect_heap_is_full:true 1 b ;
  Assert.equal ~msg:__LOC__ (B.elements b) []

let test_heap_works () =
  let b = B.create 10 in
  let check_insert_list l = List.iter (fun v -> check_insert_heap v b) l in
  let check_pop ~__LOC__ =
    List.iter (fun v ->
        let extracted = B.pop b in
        check_invariant b ;
        Assert.equal
          ~pp:Format.(pp_print_option pp_print_int)
          ~msg:__LOC__
          extracted
          (Some v))
  in
  Assert.equal ~msg:__LOC__ (B.elements b) [] ;
  Assert.equal ~msg:__LOC__ (B.pop b) None ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) None ;
  check_insert_list [3; 2; 1] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [1; 2; 3] ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) (Some 1) ;
  check_pop ~__LOC__ [1; 2; 3] ;
  Assert.equal ~msg:__LOC__ (B.pop b) None ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) None ;
  check_insert_list [10; 4; 8; 2; 6] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [2; 4; 6; 8; 10] ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) (Some 2) ;
  check_pop ~__LOC__ [2; 4] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [6; 8; 10] ;
  Assert.equal ~msg:__LOC__ (B.peek_min b) (Some 6) ;
  check_insert_list [9; 1; 7; 3; 5; 2; 4] ;
  Assert.equal
    ~pp:Format.(pp_print_list pp_print_int)
    ~msg:__LOC__
    (B.elements b)
    [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] ;
  check_insert_heap ~expect_heap_is_full:true 0 b ;
  check_insert_heap ~expect_heap_is_full:true 11 b ;
  Assert.is_true
    ~loc:__LOC__
    (List.for_all (fun v -> B.mem v b) [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]) ;
  Assert.is_false ~loc:__LOC__ (B.mem 0 b) ;
  Assert.is_false ~loc:__LOC__ (B.mem 11 b) ;
  Assert.equal
    ~msg:__LOC__
    (List.for_all
       (fun v ->
         let id = id v in
         let v' = B.find_opt id b in
         v' = Some v)
       [10; 9; 8; 7; 6; 5; 4; 3; 2; 1])
    true ;
  Assert.equal ~msg:__LOC__ (B.find_opt (id 0) b) None ;
  Assert.equal ~msg:__LOC__ (B.find_opt (id 11) b) None ;
  B.clear b ;
  check_invariant b ;
  Assert.equal ~msg:__LOC__ (B.elements b) [] ;
  check_insert_list [1; 2; 3; 4; 4; 3; 2; 1] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [1; 2; 3; 4] ;
  B.remove b (string_of_int 4) ;
  check_invariant b ;
  B.remove b (string_of_int 2) ;
  check_invariant b ;
  B.remove b (string_of_int 0) ;
  check_invariant b ;
  B.remove b (string_of_int 1) ;
  check_invariant b ;
  B.remove b (string_of_int 3) ;
  check_invariant b ;
  B.remove b (string_of_int 0) ;
  check_invariant b ;
  Assert.equal ~msg:__LOC__ (B.elements b) [] ;
  check_insert_list [4; 3; 2; 1] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [1; 2; 3; 4] ;
  let removed_ids = B.remove_predicate (fun i -> i <= 2) b in
  Assert.equal ~msg:__LOC__ (List.sort String.compare removed_ids) ["1"; "2"] ;
  Assert.equal ~msg:__LOC__ (B.elements b) [3; 4] ;
  check_invariant b ;

  ()

let () =
  Alcotest.run
    ~__FILE__
    "stdlib"
    [
      ( "Bounded_min_heap",
        [
          ("create 0 works", `Quick, test_empty_works);
          ("basic operation works", `Quick, test_heap_works);
        ] );
    ]
