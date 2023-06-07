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
    -------
    Component:    Protocol Environment
    Invocation:   dune exec src/lib_protocol_environment/test/main.exe
    Dependencies: src/lib_protocol_environment/test/assert.ml
    Subject:      Low-level operations on memory contexts.
*)

(** Context creation *)

(*
  Genesis -- block2 -- block3a
                  \
                   \-- block3b
*)

module Assert = Assert

let create_block2 ctxt =
  let open Lwt_syntax in
  let* ctxt = Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
  let* ctxt = Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
  let* ctxt = Context.add ctxt ["version"] (Bytes.of_string "0.0") in
  Lwt.return ctxt

let create_block3a ctxt =
  let open Lwt_syntax in
  let* ctxt = Context.remove ctxt ["a"; "b"] in
  let* ctxt = Context.add ctxt ["a"; "d"] (Bytes.of_string "Mars") in
  Lwt.return ctxt

let create_block3b ctxt =
  let open Lwt_syntax in
  let* ctxt = Context.remove ctxt ["a"; "c"] in
  let* ctxt = Context.add ctxt ["a"; "d"] (Bytes.of_string "Février") in
  Lwt.return ctxt

type t = {
  genesis : Context.t;
  block2 : Context.t;
  block3a : Context.t;
  block3b : Context.t;
}

let wrap_context_init f _ () =
  let open Lwt_syntax in
  let genesis = Tezos_protocol_environment.Memory_context.empty in
  let* block2 = create_block2 genesis in
  let* block3a = create_block3a block2 in
  let* block3b = create_block3b block2 in
  let* result = f {genesis; block2; block3a; block3b} in
  Lwt.return result

(** Simple test *)

let c = Option.map Bytes.to_string

(** Restore the context applied until [block2]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - [(["a"; "c"], "Juin")]
*)
let test_simple {block2 = ctxt; _} =
  let open Lwt_syntax in
  let* version = Context.find ctxt ["version"] in
  Assert.String.Option.equal ~loc:__LOC__ (c version) (Some "0.0") ;
  let* novembre = Context.find ctxt ["a"; "b"] in
  Assert.String.Option.equal (Some "Novembre") (c novembre) ;
  let* juin = Context.find ctxt ["a"; "c"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Juin") (c juin) ;

  (* Mem function returns "true" if the key given leads to an existing leaf *)
  let* res = Context.mem ctxt ["a"] in
  Assert.Bool.equal false res ;
  let* res = Context.mem ctxt ["a"; "c"] in
  Assert.Bool.equal true res ;
  let* res = Context.mem ctxt ["a"; "x"] in
  Assert.Bool.equal false res ;

  (* Mem_tree is like "mem", but also returns "true" for a trunk node *)
  let* res = Context.mem_tree ctxt ["a"] in
  Assert.Bool.equal true res ;
  let* res = Context.mem_tree ctxt ["a"; "c"] in
  Assert.Bool.equal true res ;
  let* res = Context.mem_tree ctxt ["a"; "x"] in
  Assert.Bool.equal false res ;
  return_unit

(** Restore the context applied until [block3a]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "c"], ["Juin"])
    - (["a"; "d"], ["Mars"])
    Additionally, the key ["a"; "b"] is associated with nothing as it
    has been removed by block [block3a].
*)
let test_continuation {block3a = ctxt; _} =
  let open Lwt_syntax in
  let* version = Context.find ctxt ["version"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "0.0") (c version) ;
  let* novembre = Context.find ctxt ["a"; "b"] in
  Assert.is_none ~loc:__LOC__ (c novembre) ;
  let* juin = Context.find ctxt ["a"; "c"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Juin") (c juin) ;
  let* mars = Context.find ctxt ["a"; "d"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Mars") (c mars) ;
  Lwt.return_unit

(** Restore the context applied until [block3b]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - (["a"; "d"], ["Février"])
    Additionally, the key ["a"; "c"] is associated with nothing as it
    has been removed by block [block3b].
*)
let test_fork {block3b = ctxt; _} =
  let open Lwt_syntax in
  let* version = Context.find ctxt ["version"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "0.0") (c version) ;
  let* novembre = Context.find ctxt ["a"; "b"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
  let* juin = Context.find ctxt ["a"; "c"] in
  Assert.is_none ~loc:__LOC__ (c juin) ;
  let* mars = Context.find ctxt ["a"; "d"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Février") (c mars) ;
  Lwt.return_unit

(** Restore the context at [genesis] and explicitly replay
    setting/getting key-values.
*)
let test_replay {genesis = ctxt0; _} =
  let open Lwt_syntax in
  let* ctxt1 = Context.add ctxt0 ["version"] (Bytes.of_string "0.0") in
  let* ctxt2 = Context.add ctxt1 ["a"; "b"] (Bytes.of_string "Novembre") in
  let* ctxt3 = Context.add ctxt2 ["a"; "c"] (Bytes.of_string "Juin") in
  let* ctxt4a = Context.add ctxt3 ["a"; "d"] (Bytes.of_string "July") in
  let* ctxt4b = Context.add ctxt3 ["a"; "d"] (Bytes.of_string "Juillet") in
  let* ctxt5a = Context.add ctxt4a ["a"; "b"] (Bytes.of_string "November") in
  let* novembre = Context.find ctxt4a ["a"; "b"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
  let* november = Context.find ctxt5a ["a"; "b"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "November") (c november) ;
  let* july = Context.find ctxt5a ["a"; "d"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "July") (c july) ;
  let* novembre = Context.find ctxt4b ["a"; "b"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
  let* juillet = Context.find ctxt4b ["a"; "d"] in
  Assert.String.Option.equal ~loc:__LOC__ (Some "Juillet") (c juillet) ;
  Lwt.return_unit

(** Restore the context at [genesis] and fold upon a context a series
    of key prefixes using {!Context.fold}.
*)
let test_fold_keys {genesis = ctxt; _} =
  let open Lwt_syntax in
  let* ctxt = Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
  let* ctxt = Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
  let* ctxt = Context.add ctxt ["a"; "d"; "e"] (Bytes.of_string "Septembre") in
  let* ctxt = Context.add ctxt ["f"] (Bytes.of_string "Avril") in
  let* ctxt = Context.add ctxt ["g"; "h"] (Bytes.of_string "Avril") in
  let* l = Test_mem_context_common.keys ctxt [] in
  Assert.String.List_list.equal
    ~loc:__LOC__
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]; ["f"]; ["g"; "h"]]
    (List.sort compare l) ;
  let* l = Test_mem_context_common.keys ctxt ["a"] in
  Assert.String.List_list.equal
    ~loc:__LOC__
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]]
    (List.sort compare l) ;
  let* l = Test_mem_context_common.keys ctxt ["f"] in
  Assert.String.List_list.equal ~loc:__LOC__ [] l ;
  let* l = Test_mem_context_common.keys ctxt ["g"] in
  Assert.String.List_list.equal ~loc:__LOC__ [["g"; "h"]] l ;
  let* l = Test_mem_context_common.keys ctxt ["i"] in
  Assert.String.List_list.equal ~loc:__LOC__ [] l ;
  Lwt.return_unit

let test_fold {genesis = ctxt; _} =
  let open Lwt_syntax in
  let foo1 = Bytes.of_string "foo1" in
  let foo2 = Bytes.of_string "foo2" in
  let* ctxt = Context.add ctxt ["foo"; "toto"] foo1 in
  let* ctxt = Context.add ctxt ["foo"; "bar"; "toto"] foo2 in
  let fold depth ecs ens =
    let* cs, ns =
      Context.fold
        ?depth
        ctxt
        []
        ~f:(fun path tree (cs, ns) ->
          match Context.Tree.kind tree with
          | `Value -> Lwt.return (path :: cs, ns)
          | `Tree -> Lwt.return (cs, path :: ns))
        ~order:`Sorted
        ~init:([], [])
    in
    Assert.String.List_list.equal ~loc:__LOC__ ecs cs ;
    Assert.String.List_list.equal ~loc:__LOC__ ens ns ;
    Lwt.return ()
  in
  let* () =
    fold
      None
      [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
      [["foo"; "bar"]; ["foo"]; []]
  in
  let* () = fold (Some (`Eq 0)) [] [[]] in
  let* () = fold (Some (`Eq 1)) [] [["foo"]] in
  let* () = fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] in
  let* () = fold (Some (`Lt 2)) [] [["foo"]; []] in
  let* () =
    fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
  in
  let* () =
    fold
      (Some (`Ge 2))
      [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
      [["foo"; "bar"]]
  in
  fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] []

let steps =
  ["00"; "01"; "02"; "03"; "05"; "06"; "07"; "09"; "0a"; "0b"; "0c";
   "0e"; "0f"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "19";
   "1a"; "1b"; "1c"; "1d"; "1e"; "1f"; "20"; "22"; "23"; "25"; "26";
   "27"; "28"; "2a"; "2b"; "2f"; "30"; "31"; "32"; "33"; "35"; "36";
   "37"; "3a"; "3b"; "3c"; "3d"; "3e"; "3f"; "40"; "42"; "43"; "45";
   "46"; "47"; "48"; "4a"; "4b"; "4c"; "4e"; "4f"; "50"; "52"; "53";
   "54"; "55"; "56"; "57"; "59"; "5b"; "5c"; "5f"; "60"; "61"; "62";
   "63"; "64"; "65"; "66"; "67"; "69"; "6b"; "6c"; "6d"; "6e"; "6f";
   "71"; "72"; "73"; "74"; "75"; "78"; "79"; "7a"; "7b"; "7c"; "7d";
   "7e"; "80"; "82"; "83"; "84"; "85"; "86"; "88"; "8b"; "8c"; "8d";
   "8f"; "92"; "93"; "94"; "96"; "97"; "99"; "9a"; "9b"; "9d"; "9e";
   "9f"; "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "aa";
   "ab"; "ac"; "ad"; "ae"; "af"; "b0"; "b1"; "b2"; "b3"; "b4"; "b6";
   "b8"; "b9"; "bb"; "bc"; "bf"; "c0"; "c1"; "c2"; "c3"; "c4"; "c5";
   "c8"; "c9"; "cb"; "cc"; "cd"; "ce"; "d0"; "d1"; "d2"; "d4"; "d5";
   "d7"; "d8"; "d9"; "da"; "e0"; "e3"; "e6"; "e8"; "e9"; "ea"; "ec";
   "ee"; "ef"; "f0"; "f1"; "f5"; "f7"; "f8"; "f9"; "fb"; "fc"; "fd";
   "fe"; "ff"]
[@@ocamlformat "disable"]

let bindings =
  let zero = Bytes.make 10 '0' in
  List.map (fun x -> (["root"; x], zero)) steps

let test_fold_order {genesis = ctxt; _} =
  let open Lwt_syntax in
  let* ctxt =
    List.fold_left_s (fun ctxt (k, v) -> Context.add ctxt k v) ctxt bindings
  in
  (* check that folding over a in-memory checkout is ok. It would be
     nice to test this on a checkout as well, but [Context] doesn't
     expose the right hooks (yet?). *)
  let* bs =
    Test_mem_context_common.fold_keys ctxt ["root"] ~init:[] ~f:(fun k acc ->
        Lwt.return (k :: acc))
  in
  let bs = List.rev bs in
  Assert.String.List_list.equal ~loc:__LOC__ (List.map fst bindings) bs ;
  Lwt.return_unit

let test_trees {genesis = ctxt; _} =
  let open Lwt_syntax in
  let* () =
    Context.Tree.fold
      ~depth:(`Eq 1)
      ~order:`Sorted
      ~init:()
      (Context.Tree.empty ctxt)
      []
      ~f:(fun k _ () ->
        assert (Compare.List_length_with.(k = 1)) ;
        Assert.fail_msg "empty")
  in
  let foo1 = Bytes.of_string "foo1" in
  let foo2 = Bytes.of_string "foo2" in
  Context.Tree.empty ctxt |> fun v1 ->
  let* v1 = Context.Tree.add v1 ["foo"; "toto"] foo1 in
  let* v1 = Context.Tree.add v1 ["foo"; "bar"; "toto"] foo2 in
  let fold depth ecs ens =
    let* cs, ns =
      Context.Tree.fold
        v1
        ?depth
        []
        ~f:(fun path tree (cs, ns) ->
          match Context.Tree.kind tree with
          | `Value -> Lwt.return (path :: cs, ns)
          | `Tree -> Lwt.return (cs, path :: ns))
        ~order:`Sorted
        ~init:([], [])
    in
    Assert.String.List_list.equal ~loc:__LOC__ ecs cs ;
    Assert.String.List_list.equal ~loc:__LOC__ ens ns ;
    Lwt.return ()
  in
  let* () =
    fold
      None
      [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
      [["foo"; "bar"]; ["foo"]; []]
  in
  let* () = fold (Some (`Eq 0)) [] [[]] in
  let* () = fold (Some (`Eq 1)) [] [["foo"]] in
  let* () = fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] in
  let* () = fold (Some (`Lt 2)) [] [["foo"]; []] in
  let* () =
    fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
  in
  let* () =
    fold
      (Some (`Ge 2))
      [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
      [["foo"; "bar"]]
  in
  let* () = fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] [] in
  let* v1 = Context.Tree.remove v1 ["foo"; "bar"; "toto"] in
  let* v = Context.Tree.find v1 ["foo"; "bar"; "toto"] in
  Assert.Bytes.Option.equal ~loc:__LOC__ None v ;
  let* v = Context.Tree.find v1 ["foo"; "toto"] in
  Assert.Bytes.Option.equal ~loc:__LOC__ (Some foo1) v ;
  Context.Tree.empty ctxt |> fun v1 ->
  let* v1 = Context.Tree.add v1 ["foo"; "1"] foo1 in
  let* v1 = Context.Tree.add v1 ["foo"; "2"] foo2 in
  let* v1 = Context.Tree.remove v1 ["foo"; "1"] in
  let* v1 = Context.Tree.remove v1 ["foo"; "2"] in
  let* v = Context.Tree.find v1 ["foo"; "1"] in
  Assert.Bytes.Option.equal ~loc:__LOC__ None v ;
  let* v1 = Context.Tree.remove v1 [] in
  Assert.Bool.equal ~loc:__LOC__ true (Context.Tree.is_empty v1) ;
  Lwt.return ()

(* We now test the [keys] function.
 *
 * These tests are important for [Test_mem_context_array_theory] that
 * relies on this function. We don't want the tests of [keys] to be
 * in [Test_mem_context_array_theory] because it uses [QCheck2].
 *
 * We need [keys] to be correct, because it's at the core of checking
 * the second axiom of array theory in [Test_mem_context_array_theory].
 *)

module StringListOrd : Stdlib.Set.OrderedType with type t = string list = struct
  type t = string list

  let compare = Stdlib.compare
end

module StringListSet = Set.Make (StringListOrd)

module PP = struct
  let key ppf k =
    let atom_pp fmt s = Format.fprintf fmt "%s" s in
    Format.pp_print_list
      atom_pp
      ppf
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "->")
      k

  let domain ppf d =
    let l = StringListSet.elements d in
    Format.pp_print_list
      key
      ppf
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      l

  let domain ppf d = Format.fprintf ppf "[%a]" domain d
end

let check_eq_domains d1 d2 =
  Assert.equal ~eq:StringListSet.equal ~pp:PP.domain d1 d2

let test_domain0 () =
  let open Lwt_syntax in
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"] in
  let k2 = ["b"] in
  let k3 = ["c"] in
  let ctxt = Tezos_protocol_environment.Memory_context.empty in
  let* ctxt = Context.add ctxt k1 b0 in
  let* ctxt = Context.add ctxt k2 b0 in
  let* ctxt = Context.add ctxt k3 b0 in
  let expected_domain = [k1; k2; k3] |> StringListSet.of_list in
  let* actual_domain = Test_mem_context_common.domain ctxt in
  let actual_domain = StringListSet.of_list actual_domain in
  check_eq_domains expected_domain actual_domain ;
  Lwt.return_unit

let test_domain1 () =
  let open Lwt_syntax in
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"; "b"] in
  let k2 = ["a"; "c"; "d"] in
  let ctxt = Tezos_protocol_environment.Memory_context.empty in
  let* ctxt = Context.add ctxt k1 b0 in
  let* ctxt = Context.add ctxt k2 b0 in
  let expected_domain = [k1; k2] |> StringListSet.of_list in
  let* actual_domain = Test_mem_context_common.domain ctxt in
  let actual_domain = StringListSet.of_list actual_domain in
  check_eq_domains expected_domain actual_domain ;
  Lwt.return_unit

let test_domain2 () =
  let open Lwt_syntax in
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"; "b"] in
  let k2 = ["a"; "c"; "d"] in
  let k3 = ["a"; "c"; "e"] in
  let k4 = ["x"] in
  let ctxt = Tezos_protocol_environment.Memory_context.empty in
  let* ctxt = Context.add ctxt k1 b0 in
  let* ctxt = Context.add ctxt k2 b0 in
  let* ctxt = Context.add ctxt k3 b0 in
  let* ctxt = Context.add ctxt k4 b0 in
  let expected_domain = [k1; k2; k3; k4] |> StringListSet.of_list in
  let* actual_domain = Test_mem_context_common.domain ctxt in
  let actual_domain = StringListSet.of_list actual_domain in
  check_eq_domains expected_domain actual_domain ;
  Lwt.return_unit

(******************************************************************************)

let tests =
  [
    ("simple", test_simple);
    ("continuation", test_continuation);
    ("fork", test_fork);
    ("replay", test_replay);
    ("fold_keys", test_fold_keys);
    ("fold", test_fold);
    ("fold order", test_fold_order);
    ("trees", test_trees);
  ]

let domain_tests =
  [
    ("domain0", test_domain0);
    ("domain1", test_domain1);
    ("domain2", test_domain2);
  ]

let tests =
  List.map
    (fun (n, f) -> Alcotest_lwt.test_case n `Quick (wrap_context_init f))
    tests
  @ List.map
      (fun (n, f) -> Alcotest_lwt.test_case n `Quick (fun _ _ -> f ()))
      domain_tests

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-shell-context" [("mem_context", tests)]
  |> Lwt_main.run
