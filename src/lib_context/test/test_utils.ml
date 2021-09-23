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

let check_bool pos ~expected actual =
  Alcotest.(check ~pos bool) "" expected actual

let check_invalid_arg pos f =
  let fail got =
    Alcotest.failf
      ~pos
      "Expected function to raise `Invalid_argument`, but raised: %a"
      Fmt.(Dump.option exn)
      got
  in
  match f () with
  | _ -> fail None
  | exception Invalid_argument _ -> ()
  | exception exn -> fail (Some exn)

module Small_list = struct
  module Small_list = struct
    include Tezos_context.Utils.Small_list

    let to_list sl =
      let acc = ref [] in
      iter (fun x -> acc := x :: !acc) sl ;
      List.rev !acc

    let of_list l = List.fold_right cons l empty
  end

  (* A length large enough to exceed [Small_list]'s optimisation threshold. *)
  let not_small = 10

  (* Ensures that [iter], [cons] and [empty] are consistent with each other by
     round-tripping from the regular list type. *)
  let test_round_trip () =
    for i = 1 to not_small do
      let l = Stdlib.List.init i Fun.id in
      let msg = Fmt.str "Round-trip on list of length %d" (List.length l) in
      Small_list.to_list (Small_list.of_list l)
      |> Alcotest.(check (list int)) msg l
    done

  let test_exists () =
    let long_list =
      Small_list.of_list (Stdlib.List.init (3 * not_small) Fun.id)
    in

    Small_list.exists (fun _ -> assert false) Small_list.empty
    |> check_bool __POS__ ~expected:false ;

    Small_list.exists (fun _ -> true) Small_list.(cons 1 empty)
    |> check_bool __POS__ ~expected:true ;

    Small_list.exists (fun _ -> false) long_list
    |> check_bool __POS__ ~expected:false ;

    (* Ensure that we check the optimised segment of the list: *)
    Small_list.exists (fun x -> x > 5) long_list
    |> check_bool __POS__ ~expected:true ;

    (* Ensure that we check the non-optimised segment of the list: *)
    Small_list.exists (fun x -> x > not_small) long_list
    |> check_bool __POS__ ~expected:true
end

module Arena = struct
  module Arena = Tezos_context.Utils.Arena

  let test_is_full () =
    let arena = Arena.create ~elt_length:1 ~initial_capacity:0 in
    Arena.is_full arena |> check_bool __POS__ ~expected:true ;
    Arena.expand arena 1 ;
    Arena.is_full arena |> check_bool __POS__ ~expected:false ;
    let (_ : Arena.id) = Arena.allocate arena "x" in
    Arena.is_full arena |> check_bool __POS__ ~expected:true

  (* Exercises [allocate] and [dereference]. *)
  let test_read_write () =
    let arena = Arena.create ~elt_length:1 ~initial_capacity:0 in
    check_invalid_arg __POS__ (fun () -> Arena.allocate arena "x") ;

    (* Add some elements and ensure they're dereferenced correctly: *)
    Arena.expand arena 4 ;
    let elts = ["a"; "b"; "c"; "d"] in
    let ids = List.map (Arena.allocate arena) elts in
    check_invalid_arg __POS__ (fun () -> Arena.allocate arena "e") ;
    Stdlib.ListLabels.iter2 elts ids ~f:(fun expected id ->
        let got = Arena.dereference arena id in
        Alcotest.(check string) "Element is dereferenced correctly" expected got)

  let test_expand () =
    let arena = Arena.create ~elt_length:100 ~initial_capacity:0 in
    Arena.expand arena 0 (* No-op expands are fine *) ;
    check_invalid_arg __POS__ (fun () -> Arena.expand arena (-1)) ;
    Arena.expand arena 1 ;
    Arena.expand arena 3 ;

    (* Not allowed to contract the arena (even when the space is unused): *)
    check_invalid_arg __POS__ (fun () -> Arena.expand arena 2)

  let test_elt_equal () =
    let arena = Arena.create ~elt_length:1 ~initial_capacity:1 in
    let a_ref = Arena.allocate arena "a" in
    Arena.elt_equal arena a_ref "a" |> check_bool __POS__ ~expected:true ;
    Arena.elt_equal arena a_ref "b" |> check_bool __POS__ ~expected:false ;
    ()

  let test_smuggled_id () =
    let elt_length = 30 in
    let arena = Arena.create ~elt_length ~initial_capacity:3 in

    (* Build an invalid ID into [arena] by interacting with a different one: *)
    let smuggled_id =
      let elt_length = 50 in
      let arena = Arena.create ~elt_length ~initial_capacity:2 in
      let elt = String.make elt_length 'x' in
      let (_ : Arena.id) = Arena.allocate arena elt in
      Arena.allocate arena elt
    in
    let check_deref_invalid pos =
      check_invalid_arg pos (fun () -> Arena.dereference arena smuggled_id) ;
      check_invalid_arg pos (fun () -> Arena.elt_equal arena smuggled_id "")
    in

    check_deref_invalid __POS__ (* id = 50, Arena offset = 0 *) ;
    let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'a') in
    check_deref_invalid __POS__ (* id = 50, Arena offset = 30 *) ;
    let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'b') in
    check_deref_invalid __POS__ (* id = 50, arena offset = 60 (elt len = 30) *) ;
    let (_ : Arena.id) = Arena.allocate arena (String.make elt_length 'c') in

    (* This time, the smuggled ID is a 'valid' pointer into the new arena, so we
       can't guard against invalid usage. We read over the boundary between
       elements 2 and 3 instead: *)
    let result = Arena.dereference arena smuggled_id in
    let expected = String.make 10 'b' ^ String.make 20 'c' in
    Alcotest.(check ~pos:__POS__ string) "" expected result

  let test_invalid_length () =
    let arena = Arena.create ~elt_length:1 ~initial_capacity:100 in
    check_invalid_arg __POS__ (fun () -> Arena.allocate arena "")
end

module String_set = struct
  module String_set = Tezos_context.Utils.String_set

  let test_simple () =
    let set = String_set.create ~elt_length:1 ~initial_capacity:0 in
    String_set.mem set "a" |> check_bool __POS__ ~expected:false ;
    String_set.add set "a" ;
    String_set.mem set "a" |> check_bool __POS__ ~expected:true ;
    String_set.add set "b" ;
    String_set.add set "c" ;
    String_set.mem set "a" |> check_bool __POS__ ~expected:true ;
    String_set.mem set "b" |> check_bool __POS__ ~expected:true ;
    String_set.mem set "c" |> check_bool __POS__ ~expected:true

  let test_random () =
    let elt_length = 32 in
    let set = String_set.create ~elt_length ~initial_capacity:0 in
    let reference_tbl = Stdlib.Hashtbl.create 0 in
    let reference_vector = Vector.create ~dummy:"" in
    let random_string () =
      String.init elt_length (fun _ -> char_of_int (Random.int 256))
    in
    for i = 0 to 1_000_000 do
      (* Add a new element. *)
      let new_elt = random_string () in
      String_set.add set new_elt ;
      Stdlib.Hashtbl.add reference_tbl new_elt () ;
      Vector.push reference_vector new_elt ;

      (* Pick a random existing element and check [mem] is true. *)
      let elt = Vector.get reference_vector (Random.int (i + 1)) in
      assert (Stdlib.Hashtbl.mem reference_tbl elt) ;
      String_set.mem set elt |> check_bool __POS__ ~expected:true ;

      (* Pick a random non-existing element and check [mem] is false. *)
      let non_elt = random_string () in
      assert (not (Stdlib.Hashtbl.mem reference_tbl non_elt)) ;
      String_set.mem set non_elt |> check_bool __POS__ ~expected:false
    done
end

let tests =
  let test name fn = Alcotest_lwt.test_case_sync name `Quick fn in
  [
    test "Small_list.round_trip" Small_list.test_round_trip;
    test "Small_list.exists" Small_list.test_exists;
    test "Arena.is_full" Arena.test_is_full;
    test "Arena.read_write" Arena.test_read_write;
    test "Arena.expand" Arena.test_expand;
    test "Arena.elt_equal" Arena.test_elt_equal;
    test "Arena.smuggled_id" Arena.test_smuggled_id;
    test "Arena.invalid_length" Arena.test_invalid_length;
    test "String_set.simple" String_set.test_simple;
    test "String_set.random" String_set.test_random;
  ]
