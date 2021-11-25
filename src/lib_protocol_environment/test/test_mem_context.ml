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
    Invocation:   dune build @src/lib_protocol_environment/runtest
    Dependencies: src/lib_protocol_environment/test/assert.ml
    Subject:      Low-level operations on memory contexts.
*)

(** Context creation *)

(*
  Genesis -- block2 -- block3a
                  \
                   \-- block3b
*)

let create_block2 ctxt =
  Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") >>= fun ctxt ->
  Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") >>= fun ctxt ->
  Context.add ctxt ["version"] (Bytes.of_string "0.0") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3a ctxt =
  Context.remove ctxt ["a"; "b"] >>= fun ctxt ->
  Context.add ctxt ["a"; "d"] (Bytes.of_string "Mars") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3b ctxt =
  Context.remove ctxt ["a"; "c"] >>= fun ctxt ->
  Context.add ctxt ["a"; "d"] (Bytes.of_string "Février") >>= fun ctxt ->
  Lwt.return ctxt

type t = {
  genesis : Context.t;
  block2 : Context.t;
  block3a : Context.t;
  block3b : Context.t;
}

let wrap_context_init f _ () =
  let genesis = Memory_context.empty in
  create_block2 genesis >>= fun block2 ->
  create_block3a block2 >>= fun block3a ->
  create_block3b block2 >>= fun block3b ->
  f {genesis; block2; block3a; block3b} >>= fun result -> Lwt.return result

(** Simple test *)

let c = function None -> None | Some s -> Some (Bytes.to_string s)

(** Restore the context applied until [block2]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - [(["a"; "c"], "Juin")]
*)
let test_simple {block2 = ctxt; _} =
  Context.find ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
  Context.find ctxt ["a"; "b"] >>= fun novembre ->
  Assert.equal_string_option (Some "Novembre") (c novembre) ;
  Context.find ctxt ["a"; "c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  Lwt.return_unit

(** Restore the context applied until [block3a]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "c"], ["Juin"])
    - (["a"; "d"], ["Mars"])
    Additionally, the key ["a"; "b"] is associated with nothing as it
    has been removed by block [block3a].
*)
let test_continuation {block3a = ctxt; _} =
  Context.find ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  Context.find ctxt ["a"; "b"] >>= fun novembre ->
  Assert.is_none ~msg:__LOC__ (c novembre) ;
  Context.find ctxt ["a"; "c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  Context.find ctxt ["a"; "d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Mars") (c mars) ;
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
  Context.find ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  Context.find ctxt ["a"; "b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.find ctxt ["a"; "c"] >>= fun juin ->
  Assert.is_none ~msg:__LOC__ (c juin) ;
  Context.find ctxt ["a"; "d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
  Lwt.return_unit

(** Restore the context at [genesis] and explicitly replay
    setting/getting key-values.
*)
let test_replay {genesis = ctxt0; _} =
  Context.add ctxt0 ["version"] (Bytes.of_string "0.0") >>= fun ctxt1 ->
  Context.add ctxt1 ["a"; "b"] (Bytes.of_string "Novembre") >>= fun ctxt2 ->
  Context.add ctxt2 ["a"; "c"] (Bytes.of_string "Juin") >>= fun ctxt3 ->
  Context.add ctxt3 ["a"; "d"] (Bytes.of_string "July") >>= fun ctxt4a ->
  Context.add ctxt3 ["a"; "d"] (Bytes.of_string "Juillet") >>= fun ctxt4b ->
  Context.add ctxt4a ["a"; "b"] (Bytes.of_string "November") >>= fun ctxt5a ->
  Context.find ctxt4a ["a"; "b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.find ctxt5a ["a"; "b"] >>= fun november ->
  Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
  Context.find ctxt5a ["a"; "d"] >>= fun july ->
  Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
  Context.find ctxt4b ["a"; "b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.find ctxt4b ["a"; "d"] >>= fun juillet ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
  Lwt.return_unit

let fold_keys s root ~init ~f =
  Context.fold s root ~order:`Sorted ~init ~f:(fun k v acc ->
      match Context.Tree.kind v with
      | `Value -> f (root @ k) acc
      | `Tree -> Lwt.return acc)

let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

(** Restore the context at [genesis] and fold upon a context a series
    of key prefixes using {!Context.fold}.
*)
let test_fold_keys {genesis = ctxt; _} =
  Context.add ctxt ["a"; "b"] (Bytes.of_string "Novembre") >>= fun ctxt ->
  Context.add ctxt ["a"; "c"] (Bytes.of_string "Juin") >>= fun ctxt ->
  Context.add ctxt ["a"; "d"; "e"] (Bytes.of_string "Septembre") >>= fun ctxt ->
  Context.add ctxt ["f"] (Bytes.of_string "Avril") >>= fun ctxt ->
  Context.add ctxt ["g"; "h"] (Bytes.of_string "Avril") >>= fun ctxt ->
  keys ctxt [] >>= fun l ->
  Assert.equal_string_list_list
    ~msg:__LOC__
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]; ["f"]; ["g"; "h"]]
    (List.sort compare l) ;
  keys ctxt ["a"] >>= fun l ->
  Assert.equal_string_list_list
    ~msg:__LOC__
    [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]]
    (List.sort compare l) ;
  keys ctxt ["f"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  keys ctxt ["g"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [["g"; "h"]] l ;
  keys ctxt ["i"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  Lwt.return_unit

let test_fold {genesis = ctxt; _} =
  let foo1 = Bytes.of_string "foo1" in
  let foo2 = Bytes.of_string "foo2" in
  Context.add ctxt ["foo"; "toto"] foo1 >>= fun ctxt ->
  Context.add ctxt ["foo"; "bar"; "toto"] foo2 >>= fun ctxt ->
  let fold depth ecs ens =
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
    >>= fun (cs, ns) ->
    Assert.equal_string_list_list ~msg:__LOC__ ecs cs ;
    Assert.equal_string_list_list ~msg:__LOC__ ens ns ;
    Lwt.return ()
  in
  fold
    None
    [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
    [["foo"; "bar"]; ["foo"]; []]
  >>= fun () ->
  fold (Some (`Eq 0)) [] [[]] >>= fun () ->
  fold (Some (`Eq 1)) [] [["foo"]] >>= fun () ->
  fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] >>= fun () ->
  fold (Some (`Lt 2)) [] [["foo"]; []] >>= fun () ->
  fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
  >>= fun () ->
  fold (Some (`Ge 2)) [["foo"; "toto"]; ["foo"; "bar"; "toto"]] [["foo"; "bar"]]
  >>= fun () -> fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] []

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
  Lwt_list.fold_left_s (fun ctxt (k, v) -> Context.add ctxt k v) ctxt bindings
  >>= fun ctxt ->
  (* check that folding over a in-memory checkout is ok. It would be
     nice to test this on a checkout as well, but [Context] doesn't
     expose the right hooks (yet?). *)
  fold_keys ctxt ["root"] ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
  >>= fun bs ->
  let bs = List.rev bs in
  Assert.equal_string_list_list ~msg:__LOC__ (List.map fst bindings) bs ;
  Lwt.return_unit

let test_trees {genesis = ctxt; _} =
  Context.Tree.fold
    ~depth:(`Eq 1)
    ~order:`Sorted
    ~init:()
    (Context.Tree.empty ctxt)
    []
    ~f:(fun k _ () ->
      assert (Compare.List_length_with.(k = 1)) ;
      Assert.fail_msg "empty")
  >>= fun () ->
  let foo1 = Bytes.of_string "foo1" in
  let foo2 = Bytes.of_string "foo2" in
  Context.Tree.empty ctxt |> fun v1 ->
  Context.Tree.add v1 ["foo"; "toto"] foo1 >>= fun v1 ->
  Context.Tree.add v1 ["foo"; "bar"; "toto"] foo2 >>= fun v1 ->
  let fold depth ecs ens =
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
    >>= fun (cs, ns) ->
    Assert.equal_string_list_list ~msg:__LOC__ ecs cs ;
    Assert.equal_string_list_list ~msg:__LOC__ ens ns ;
    Lwt.return ()
  in
  fold
    None
    [["foo"; "toto"]; ["foo"; "bar"; "toto"]]
    [["foo"; "bar"]; ["foo"]; []]
  >>= fun () ->
  fold (Some (`Eq 0)) [] [[]] >>= fun () ->
  fold (Some (`Eq 1)) [] [["foo"]] >>= fun () ->
  fold (Some (`Eq 2)) [["foo"; "toto"]] [["foo"; "bar"]] >>= fun () ->
  fold (Some (`Lt 2)) [] [["foo"]; []] >>= fun () ->
  fold (Some (`Le 2)) [["foo"; "toto"]] [["foo"; "bar"]; ["foo"]; []]
  >>= fun () ->
  fold (Some (`Ge 2)) [["foo"; "toto"]; ["foo"; "bar"; "toto"]] [["foo"; "bar"]]
  >>= fun () ->
  fold (Some (`Gt 2)) [["foo"; "bar"; "toto"]] [] >>= fun () ->
  Context.Tree.remove v1 ["foo"; "bar"; "toto"] >>= fun v1 ->
  Context.Tree.find v1 ["foo"; "bar"; "toto"] >>= fun v ->
  Assert.equal_bytes_option ~msg:__LOC__ None v ;
  Context.Tree.find v1 ["foo"; "toto"] >>= fun v ->
  Assert.equal_bytes_option ~msg:__LOC__ (Some foo1) v ;
  Context.Tree.empty ctxt |> fun v1 ->
  Context.Tree.add v1 ["foo"; "1"] foo1 >>= fun v1 ->
  Context.Tree.add v1 ["foo"; "2"] foo2 >>= fun v1 ->
  Context.Tree.remove v1 ["foo"; "1"] >>= fun v1 ->
  Context.Tree.remove v1 ["foo"; "2"] >>= fun v1 ->
  Context.Tree.find v1 ["foo"; "1"] >>= fun v ->
  Assert.equal_bytes_option ~msg:__LOC__ None v ;
  Context.Tree.remove v1 [] >>= fun v1 ->
  Assert.equal_bool ~msg:__LOC__ true (Context.Tree.is_empty v1) ;
  Lwt.return ()

(* We now test the [keys] function.
 *
 * These tests are important for [Test_mem_context_array_theory] that
 * relies on this function. We don't want the tests of [keys] to be
 * in [Test_mem_context_array_theory] because it uses [QCheck].
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
    let l = StringListSet.to_seq d |> List.of_seq in
    Format.pp_print_list
      key
      ppf
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      l

  let domain ppf d = Format.fprintf ppf "[%a]" domain d

  let domain_to_string d = Format.asprintf "%a" domain d
end

let domain ctxt = keys ctxt []

let check_eq_domains d1 d2 =
  let eq d d' = StringListSet.subset d d' && StringListSet.subset d' d in
  Assert.equal ~eq ~prn:PP.domain_to_string d1 d2

let test_domain0 () =
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"] in
  let k2 = ["b"] in
  let k3 = ["c"] in
  let ctxt = Memory_context.empty in
  Context.add ctxt k1 b0 >>= fun ctxt ->
  Context.add ctxt k2 b0 >>= fun ctxt ->
  Context.add ctxt k3 b0 >>= fun ctxt ->
  let expected_domain = [k1; k2; k3] |> StringListSet.of_list in
  domain ctxt >>= fun actual_domain ->
  let actual_domain = StringListSet.of_list actual_domain in
  check_eq_domains expected_domain actual_domain ;
  Lwt.return_unit

let test_domain1 () =
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"; "b"] in
  let k2 = ["a"; "c"; "d"] in
  let ctxt = Memory_context.empty in
  Context.add ctxt k1 b0 >>= fun ctxt ->
  Context.add ctxt k2 b0 >>= fun ctxt ->
  let expected_domain = [k1; k2] |> StringListSet.of_list in
  domain ctxt >>= fun actual_domain ->
  let actual_domain = StringListSet.of_list actual_domain in
  check_eq_domains expected_domain actual_domain ;
  Lwt.return_unit

let test_domain2 () =
  let b0 = Bytes.of_string "0" in
  let k1 = ["a"; "b"] in
  let k2 = ["a"; "c"; "d"] in
  let k3 = ["a"; "c"; "e"] in
  let k4 = ["x"] in
  let ctxt = Memory_context.empty in
  Context.add ctxt k1 b0 >>= fun ctxt ->
  Context.add ctxt k2 b0 >>= fun ctxt ->
  Context.add ctxt k3 b0 >>= fun ctxt ->
  Context.add ctxt k4 b0 >>= fun ctxt ->
  let expected_domain = [k1; k2; k3; k4] |> StringListSet.of_list in
  domain ctxt >>= fun actual_domain ->
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
