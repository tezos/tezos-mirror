(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
   Component:    Base
   Invocation:   dune exec src/lib_base/test/main.exe \
                  -- --file test_sized.ml
   Subject:      Check that the sized map and set functions behave correctly
*)

module IntSet = TzLwtreslib.Set.Make (Int)
module SizedSet = Sized.MakeSizedSet (IntSet)
module IntMap = TzLwtreslib.Map.Make (Int)
module SizedMap = Sized.MakeSizedMap (IntMap)

module SizedSet_test = struct
  open QCheck2

  let assert_consistent t =
    SizedSet.cardinal t = IntSet.cardinal (SizedSet.to_set t)

  let generator = Gen.(small_list small_nat >|= SizedSet.of_list)

  let seq_generator = Gen.(small_list small_nat >|= List.to_seq)

  let empty =
    Alcotest.test_case "empty" `Quick (fun () ->
        assert (SizedSet.is_empty SizedSet.empty))

  let singleton =
    Alcotest.test_case "singleton" `Quick (fun () ->
        assert (SizedSet.singleton 1 |> assert_consistent))

  let equal = Test.make ~name:"equal" generator (fun s -> SizedSet.equal s s)

  let inequal =
    Test.make
      ~name:"inequal"
      Gen.(pair generator small_nat)
      (fun (s, v) -> not (SizedSet.equal s (SizedSet.add (-v - 1) s)))

  let add =
    Test.make
      ~name:"add"
      Gen.(pair generator small_nat)
      (fun (s, v) -> SizedSet.add v s |> assert_consistent)

  let remove =
    Test.make
      ~name:"remove"
      Gen.(pair generator small_nat)
      (fun (s, v) -> SizedSet.remove v s |> assert_consistent)

  let union =
    Test.make
      ~name:"union"
      Gen.(pair generator generator)
      (fun (s1, s2) -> SizedSet.union s1 s2 |> assert_consistent)

  let inter =
    Test.make
      ~name:"inter"
      Gen.(pair generator generator)
      (fun (s1, s2) -> SizedSet.inter s1 s2 |> assert_consistent)

  let diff =
    Test.make
      ~name:"diff"
      Gen.(pair generator generator)
      (fun (s1, s2) -> SizedSet.diff s1 s2 |> assert_consistent)

  let map =
    Test.make
      ~name:"map"
      Gen.(pair generator (fun1 Observable.int int))
      (fun (s, f) -> SizedSet.map (Fn.apply f) s |> assert_consistent)

  let filter =
    Test.make
      ~name:"filter"
      Gen.(pair generator (fun1 Observable.int bool))
      (fun (s, f) -> SizedSet.filter (Fn.apply f) s |> assert_consistent)

  let filter_map =
    Test.make
      ~name:"filter_map"
      Gen.(pair generator (fun1 Observable.int (opt int)))
      (fun (s, f) -> SizedSet.filter_map (Fn.apply f) s |> assert_consistent)

  let partition =
    Test.make
      ~name:"partition"
      Gen.(pair generator (fun1 Observable.int bool))
      (fun (s, f) ->
        let s1, s2 = SizedSet.partition (Fn.apply f) s in
        assert_consistent s1 && assert_consistent s2)

  let split =
    Test.make
      ~name:"split"
      Gen.(pair generator small_nat)
      (fun (s, v) ->
        let s1, _, s2 = SizedSet.split v s in
        assert_consistent s1 && assert_consistent s2)

  let add_seq =
    Test.make
      ~name:"add_seq"
      Gen.(pair generator seq_generator)
      (fun (s, seq) -> SizedSet.add_seq seq s |> assert_consistent)

  let of_seq =
    Test.make ~name:"of_seq" seq_generator (fun seq ->
        SizedSet.of_seq seq |> assert_consistent)

  let test =
    [empty; singleton]
    @ Qcheck2_helpers.qcheck_wrap
        [
          equal;
          inequal;
          add;
          remove;
          union;
          inter;
          diff;
          map;
          filter;
          filter_map;
          partition;
          split;
          add_seq;
          of_seq;
        ]
end

module SizedMap_test = struct
  open QCheck2

  let assert_consistent t =
    SizedMap.cardinal t = IntMap.cardinal (SizedMap.to_map t)

  let seq_generator =
    Gen.(small_list (pair small_nat small_nat) >|= List.to_seq)

  let generator = Gen.(seq_generator >|= SizedMap.of_seq)

  let empty =
    Alcotest.test_case "empty" `Quick (fun () ->
        assert (SizedMap.is_empty SizedMap.empty))

  let singleton =
    Alcotest.test_case "singleton" `Quick (fun () ->
        assert (SizedMap.singleton 1 1 |> assert_consistent))

  let equal =
    Test.make ~name:"equal" generator (fun m -> SizedMap.equal Int.equal m m)

  let inequal =
    Test.make
      ~name:"inequal"
      Gen.(triple generator small_nat small_nat)
      (fun (m, v1, v2) ->
        not (SizedMap.equal Int.equal m (SizedMap.add (-v1 - 1) (-v2 - 1) m)))

  let add =
    Test.make
      ~name:"add"
      Gen.(triple generator small_nat small_nat)
      (fun (m, k, v) -> SizedMap.add k v m |> assert_consistent)

  let update =
    Test.make
      ~name:"update"
      Gen.(
        triple
          generator
          small_nat
          (fun1 Observable.(option int) (opt small_nat)))
      (fun (m, k, f) -> SizedMap.update k (Fn.apply f) m |> assert_consistent)

  let remove =
    Test.make
      ~name:"remove"
      Gen.(pair generator small_nat)
      (fun (m, v) -> SizedMap.remove v m |> assert_consistent)

  let merge =
    Test.make
      ~name:"merge"
      Gen.(
        triple
          generator
          generator
          (fun3
             Observable.int
             Observable.(option int)
             Observable.(option int)
             (opt int)))
      (fun (m1, m2, f) ->
        SizedMap.merge (Fn.apply f) m1 m2 |> assert_consistent)

  let union =
    Test.make
      ~name:"union"
      Gen.(
        triple
          generator
          generator
          (fun3 Observable.int Observable.int Observable.int (opt int)))
      (fun (m1, m2, f) ->
        SizedMap.union (Fn.apply f) m1 m2 |> assert_consistent)

  let map =
    Test.make
      ~name:"map"
      Gen.(pair generator (fun1 Observable.int (opt int)))
      (fun (m, f) -> SizedMap.map (Fn.apply f) m |> assert_consistent)

  let mapi =
    Test.make
      ~name:"mapi"
      Gen.(pair generator (fun2 Observable.int Observable.int (opt int)))
      (fun (m, f) -> SizedMap.mapi (Fn.apply f) m |> assert_consistent)

  let filter =
    Test.make
      ~name:"filter"
      Gen.(pair generator (fun2 Observable.int Observable.int bool))
      (fun (m, f) -> SizedMap.filter (Fn.apply f) m |> assert_consistent)

  let filter_map =
    Test.make
      ~name:"filter_map"
      Gen.(pair generator (fun2 Observable.int Observable.int (opt int)))
      (fun (m, f) -> SizedMap.filter_map (Fn.apply f) m |> assert_consistent)

  let partition =
    Test.make
      ~name:"partition"
      Gen.(pair generator (fun2 Observable.int Observable.int bool))
      (fun (m, f) ->
        let s1, s2 = SizedMap.partition (Fn.apply f) m in
        assert_consistent s1 && assert_consistent s2)

  let split =
    Test.make
      ~name:"split"
      Gen.(pair generator small_nat)
      (fun (m, v) ->
        let s1, _, s2 = SizedMap.split v m in
        assert_consistent s1 && assert_consistent s2)

  let add_seq =
    Test.make
      ~name:"add_seq"
      Gen.(pair generator seq_generator)
      (fun (s, seq) -> SizedMap.add_seq seq s |> assert_consistent)

  let test =
    [empty; singleton]
    @ Qcheck2_helpers.qcheck_wrap
        [
          equal;
          inequal;
          add;
          update;
          remove;
          merge;
          union;
          map;
          mapi;
          filter;
          filter_map;
          partition;
          split;
          add_seq;
        ]
end

let () =
  Alcotest.run
    ~__FILE__
    "Sized"
    [("SizedSet", SizedSet_test.test); ("SizedMap", SizedMap_test.test)]
