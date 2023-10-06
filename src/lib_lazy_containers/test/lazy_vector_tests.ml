(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(* Testing
   -------
   Component:    Lazy containers
   Invocation:   dune exec src/lib_lazy_containers/test/main.exe \
                  -- --file lazy_vector_tests.ml
   Subject:      Test lazy vector
*)

open QCheck_alcotest
open QCheck2
open Lazy_vector

let gen_of_list gen_items =
  let open Gen in
  let+ list = gen_items in
  IntVector.of_list list

let gen_create gen_items =
  let open Gen in
  let+ array = gen_items in
  IntVector.create
    ~produce_value:(fun i -> Lwt.return array.(i))
    (Array.length array)

let gen gen_item =
  let open Gen in
  let gen_create =
    oneof [gen_of_list (list gen_item); gen_create (array gen_item)]
  in
  let gen_concat =
    let+ lhs = gen_create and+ rhs = gen_create in
    Lwt_main.run @@ IntVector.concat lhs rhs
  in
  let gen_base_or_concat = oneof [gen_create; gen_concat] in
  let gen_cons =
    let+ prefix = small_list gen_item and+ map = gen_base_or_concat in
    List.fold_left (fun map prefix -> IntVector.cons prefix map) map prefix
  in
  oneof [gen_base_or_concat; gen_cons]

let of_list_constructs_correctly =
  Test.make
    ~name:"of_list creates the data structure correctly"
    Gen.(list int)
    (fun items ->
      let open Lwt.Syntax in
      Lwt_main.run
      @@
      let map = IntVector.of_list items in
      let+ checked =
        Lwt_list.mapi_p
          (fun i v ->
            let+ v' = IntVector.get i map in
            v' = v)
          items
      in
      List.for_all Fun.id checked
      && IntVector.num_elements map = List.length items)

let create_constructs_correctly =
  Test.make ~name:"create constructs correctly" Gen.nat (fun len ->
      let open Lwt.Syntax in
      Lwt_main.run
      @@
      let map = IntVector.create ~produce_value:Lwt.return len in
      Lwt_list.for_all_p
        (fun i ->
          let+ v = IntVector.get i map in
          v = i)
        (List.init len Fun.id))

let grow_works =
  Test.make
    ~name:"grow works"
    Gen.(pair (gen int) nat)
    (fun (map, len) ->
      let open Lwt.Syntax in
      Lwt_main.run
      @@
      let map2 = IntVector.grow ~default:(fun () -> 2) len map in
      let+ check1 =
        Lwt_list.for_all_p (fun i ->
            let+ v = IntVector.get i map2 and+ v' = IntVector.get i map in
            v = v')
        @@ List.init (IntVector.num_elements map) Fun.id
      and+ check2 =
        Lwt_list.for_all_p (fun i ->
            let key = i + IntVector.num_elements map in
            let+ v = IntVector.get key map2 in
            v = 2)
        @@ List.init len Fun.id
      in
      let check3 =
        IntVector.num_elements map + len = IntVector.num_elements map2
      in
      check1 && check2 && check3)

let cons_works =
  Test.make
    ~name:"cons works"
    Gen.(pair (gen int) int)
    (fun (map, value) ->
      let open Lwt.Syntax in
      Lwt_main.run
      @@
      let map2 = IntVector.cons value map in
      let* v = IntVector.get 0 map2 in
      let check1 = v = value in
      let+ check2 =
        Lwt_list.for_all_p (fun i ->
            let+ x = IntVector.get i map and+ y = IntVector.get (i + 1) map2 in
            x = y)
        @@ List.init (IntVector.num_elements map) Fun.id
      in
      check1 && check2)

let concat_works () =
  let open Lwt.Syntax in
  Lwt_main.run
  @@
  let map1 =
    IntVector.create ~produce_value:(fun x -> Lwt.return @@ Int.succ x) 1
    |> IntVector.cons 10
  in
  let map2 =
    IntVector.create ~produce_value:(fun x -> Lwt.return @@ Int.pred x) 1
    |> IntVector.cons 20
  in
  let* map = IntVector.concat map1 map2 in
  let open Alcotest in
  let lwt_check x p =
    let+ y = p in
    check int "exact value" x y
  in
  check int "exact value" 4 (IntVector.num_elements map) ;
  let+ () = lwt_check 10 (IntVector.get 0 map)
  and+ () = lwt_check 1 (IntVector.get 1 map)
  and+ () = lwt_check 20 (IntVector.get 2 map)
  and+ () = lwt_check (-1) (IntVector.get 3 map) in
  ()

let drop_works =
  Test.make
    ~name:"drop works"
    Gen.(gen int)
    (fun map ->
      let open Lwt.Syntax in
      Lwt_main.run
      @@ Lwt.catch
           (fun () ->
             let map_with_drop = IntVector.drop map in
             let* value, map_with_pop = IntVector.pop map in
             let* value_0_from_map = IntVector.get 0 map in
             let check_value_dropped = value = value_0_from_map in
             let check_map_size =
               IntVector.num_elements map
               = IntVector.num_elements map_with_drop + 1
               && IntVector.num_elements map_with_drop
                  = IntVector.num_elements map_with_pop
             in
             let iterable_list =
               List.init (IntVector.num_elements map_with_drop) Fun.id
             in
             let* check_dropped_vector =
               Lwt_list.for_all_p (fun i ->
                   let+ x = IntVector.get i map_with_drop
                   and+ y = IntVector.get (i + 1) map in
                   x = y)
               @@ iterable_list
             in
             let+ check_dropped_equal_popped =
               Lwt_list.for_all_p (fun i ->
                   let+ x = IntVector.get i map_with_drop
                   and+ y = IntVector.get i map_with_pop in
                   x = y)
               @@ iterable_list
             in
             check_value_dropped && check_map_size && check_dropped_vector
             && check_dropped_equal_popped)
           (function
             | Bounds -> Lwt.return (IntVector.num_elements map = 0)
             | _ -> Lwt.return_false))

let check_overflow () =
  let open Lwt.Syntax in
  let expect_size_overflow f =
    Lwt.catch
      (fun () ->
        let+ () = f () in
        failwith "This test should have overflown, but didn't.")
      (function SizeOverflow -> Lwt.return_unit | exn -> Lwt.reraise exn)
  in
  (* Creates a vector of the maximum size possible (2^63) *)
  let v = IntVector.create (-1) in
  (* Unfortunately, we cannot put the previous QCheck tests into the Lwt monad
     to be runned by Alcotest_lwt, since they use `Lwt_main.run` themselves. As
     such, we directly run the monad in the test. *)
  Lwt_main.run
    ((* Check overflow on cons *)
     let* () =
       expect_size_overflow (fun () ->
           let _ = IntVector.cons 0 v in
           Lwt.return_unit)
     in
     (* Check overflow on grow *)
     let* () =
       expect_size_overflow (fun () ->
           let _ = IntVector.grow 1 v in
           Lwt.return_unit)
     in
     (* Check overflow on append *)
     let* () =
       expect_size_overflow (fun () ->
           let _ = IntVector.append 1 v in
           Lwt.return_unit)
     in
     (* Check overflow on concat *)
     expect_size_overflow (fun () ->
         let _ = IntVector.concat v v in
         Lwt.return_unit))

let tests =
  [
    to_alcotest of_list_constructs_correctly;
    to_alcotest create_constructs_correctly;
    to_alcotest grow_works;
    to_alcotest cons_works;
    to_alcotest drop_works;
    ("concat works lazily", `Quick, concat_works);
    ("check size overflow", `Quick, check_overflow);
  ]

let () = Alcotest.run ~__FILE__ "Lazy structs tests" [("Lazy_vector", tests)]
