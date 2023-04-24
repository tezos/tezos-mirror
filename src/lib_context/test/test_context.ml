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

(* Testing
   -------
   Component:    Context
   Invocation:   dune exec src/lib_context/test/main.exe \
                  -- --file test_context.ml
   Subject:      On context features.
*)

(** Module concerning assertions about raw trees i.e. values of type
    [[< `Tree of 'a Tezos_base.TzPervasives.String.Map.t | `Value of bytes] as 'a] *)
module Raw_Tree = struct
  (** [equal loc msg rt0 rt1] checks that the raw tree [rt0] and [rt1] are equal.
      Will fail with the message [msg] displaying the location [loc] otherwise *)
  let equal ?loc ?msg r1 r2 =
    let rec aux r1 r2 =
      match (r1, r2) with
      | `Value v1, `Value v2 ->
          Assert.Bytes.equal ?loc ?msg v1 v2 ;
          true
      | `Tree t1, `Tree t2 ->
          if not (Tezos_base.TzPervasives.String.Map.equal aux t1 t2) then
            Assert.String.fail "<tree>" "<tree>" ?msg ?loc
          else true
      | `Tree _, `Value v ->
          Assert.String.fail ?loc ?msg "<tree>" (Bytes.to_string v)
      | `Value v, `Tree _ ->
          Assert.String.fail ?loc ?msg (Bytes.to_string v) "<tree>"
    in
    let _b : bool = aux r1 r2 in
    ()
end

module Assert = Assert

let equal_context_hash ?loc ?msg l1 l2 =
  Assert.equal ?loc ~eq:Context_hash.( = ) ~pp:Context_hash.pp ?msg l1 l2

let ( let* ) = Lwt.bind

let ( let+ ) p f = Lwt.map f p

(* Same as [let* but handle errors using [Assert.fail_msg]. *)
let ( let*!! ) x f =
  let* result = x in
  match result with
  | Error trace ->
      let message = Format.asprintf "%a" Error_monad.pp_print_trace trace in
      Assert.fail_msg "%s" message
  | Ok x -> f x

open Filename.Infix

(** Basic blocks *)

let genesis_block =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

let genesis_protocol =
  Protocol_hash.of_b58check_exn
    "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

let genesis_time = Time.Protocol.of_seconds 0L

let chain_id = Chain_id.of_block_hash genesis_block

(** Test functors *)

(* Context-generic tests *)
module Make_generic (Tag : sig
  val tag : string
end) (Type_parameters : sig
  type memory_context_tree
end)
(Context : Tezos_context_sigs.Context.TEZOS_CONTEXT
             with type memory_context_tree :=
               Type_parameters.memory_context_tree) =
struct
  open Context

  (** Context creation *)

  let commit = commit ~time:Time.Protocol.epoch ~message:""

  let create_block2 idx genesis_commit =
    let* o = checkout idx genesis_commit in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let* ctxt = add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
        let* ctxt = add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
        let* ctxt = add ctxt ["version"] (Bytes.of_string "0.0") in
        commit ctxt

  let create_block3a idx block2_commit =
    let* o = checkout idx block2_commit in
    match o with
    | None -> Assert.fail_msg "checkout block2"
    | Some ctxt ->
        let* ctxt = remove ctxt ["a"; "b"] in
        let* ctxt = add ctxt ["a"; "d"] (Bytes.of_string "Mars") in
        commit ctxt

  let create_block3b idx block2_commit =
    let* o = checkout idx block2_commit in
    match o with
    | None -> Assert.fail_msg "checkout block3b"
    | Some ctxt ->
        let* ctxt = remove ctxt ["a"; "c"] in
        let* ctxt = add ctxt ["a"; "d"] (Bytes.of_string "Février") in
        commit ctxt

  type t = {
    idx : Context.index;
    genesis : Context_hash.t;
    block2 : Context_hash.t;
    block3a : Context_hash.t;
    block3b : Context_hash.t;
  }

  let wrap_context_init f _ () =
    Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
        let root = base_dir // "context" in
        let* idx = Context.init root in
        let*!! genesis =
          Context.commit_genesis
            idx
            ~chain_id
            ~time:genesis_time
            ~protocol:genesis_protocol
        in
        let* block2 = create_block2 idx genesis in
        let* block3a = create_block3a idx block2 in
        let* block3b = create_block3b idx block2 in
        f {idx; genesis; block2; block3a; block3b})

  (** Simple test *)

  let c = function None -> None | Some s -> Some (Bytes.to_string s)

  (** Checkout the context applied until [block2]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - (["a; "c""], ["Juin"]) *)
  let test_simple {idx; block2; _} =
    let* o = checkout idx block2 in
    match o with
    | None -> Assert.fail_msg "checkout block2"
    | Some ctxt ->
        let* version = find ctxt ["version"] in
        Assert.String.Option.equal ~loc:__LOC__ (c version) (Some "0.0") ;
        let* novembre = find ctxt ["a"; "b"] in
        Assert.String.Option.equal (Some "Novembre") (c novembre) ;
        let* juin = find ctxt ["a"; "c"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Juin") (c juin) ;
        Lwt.return_unit

  let test_list {idx; block2; _} =
    let* o = checkout idx block2 in
    match o with
    | None -> Assert.fail_msg "checkout block2"
    | Some ctxt ->
        let* ls = list ctxt ["a"] in
        let ls = List.sort compare (List.map fst ls) in
        Assert.String.List.equal ~loc:__LOC__ ["b"; "c"] ls ;
        Lwt.return_unit

  (** Checkout the context applied until [block3a]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "c"], ["Juin"])
    - (["a"; "d"], ["Mars"])
    Additionally, the key ["a"; "b"] is associated with nothing as it
    has been removed by block [block3a]. *)
  let test_continuation {idx; block3a; _} =
    let* o = checkout idx block3a in
    match o with
    | None -> Assert.fail_msg "checkout block3a"
    | Some ctxt ->
        let* version = find ctxt ["version"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "0.0") (c version) ;
        let* novembre = find ctxt ["a"; "b"] in
        Assert.is_none ~loc:__LOC__ (c novembre) ;
        let* juin = find ctxt ["a"; "c"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Juin") (c juin) ;
        let* mars = find ctxt ["a"; "d"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Mars") (c mars) ;
        Lwt.return_unit

  (** Checkout the context applied until [block3b]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["Novembre"])
    - (["a"; "d"], ["Février"])
    Additionally, the key ["a"; "c"] is associated with nothing as it
    has been removed by block [block3b]. *)
  let test_fork {idx; block3b; _} =
    let* o = checkout idx block3b in
    match o with
    | None -> Assert.fail_msg "checkout block3b"
    | Some ctxt ->
        let* version = find ctxt ["version"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "0.0") (c version) ;
        let* novembre = find ctxt ["a"; "b"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
        let* juin = find ctxt ["a"; "c"] in
        Assert.is_none ~loc:__LOC__ (c juin) ;
        let* mars = find ctxt ["a"; "d"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Février") (c mars) ;
        Lwt.return_unit

  (** Checkout the context at [genesis] and explicitly replay
    setting/getting key-values. *)
  let test_replay {idx; genesis; _} =
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt0 ->
        let* ctxt1 = add ctxt0 ["version"] (Bytes.of_string "0.0") in
        let* ctxt2 = add ctxt1 ["a"; "b"] (Bytes.of_string "Novembre") in
        let* ctxt3 = add ctxt2 ["a"; "c"] (Bytes.of_string "Juin") in
        let* ctxt4a = add ctxt3 ["a"; "d"] (Bytes.of_string "July") in
        let* ctxt4b = add ctxt3 ["a"; "d"] (Bytes.of_string "Juillet") in
        let* ctxt5a = add ctxt4a ["a"; "b"] (Bytes.of_string "November") in
        let* novembre = find ctxt4a ["a"; "b"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
        let* november = find ctxt5a ["a"; "b"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "November") (c november) ;
        let* july = find ctxt5a ["a"; "d"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "July") (c july) ;
        let* novembre = find ctxt4b ["a"; "b"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Novembre") (c novembre) ;
        let* juillet = find ctxt4b ["a"; "d"] in
        Assert.String.Option.equal ~loc:__LOC__ (Some "Juillet") (c juillet) ;
        Lwt.return_unit

  let fold_keys s root ~order ~init ~f =
    fold s root ~order ~init ~f:(fun k v acc ->
        match Tree.kind v with
        | `Value -> f (root @ k) acc
        | `Tree -> Lwt.return acc)

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

  let test_fold_keys ~order {idx; genesis; _} =
    let keys t =
      fold_keys t ~order ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))
    in
    let sort_keys l =
      match order with
      | `Sorted -> List.rev l
      | `Undefined -> List.sort compare l
    in
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let* ctxt = add ctxt ["a"; "b"] (Bytes.of_string "Novembre") in
        let* ctxt = add ctxt ["a"; "c"] (Bytes.of_string "Juin") in
        let* ctxt = add ctxt ["a"; "d"; "e"] (Bytes.of_string "Septembre") in
        let* ctxt = add ctxt ["f"] (Bytes.of_string "Avril") in
        let* ctxt = add ctxt ["g"; "h"] (Bytes.of_string "Avril") in
        let* l = keys ctxt [] in
        let l = sort_keys l in
        Assert.String.List_list.equal
          ~loc:__LOC__
          [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]; ["f"]; ["g"; "h"]]
          l ;
        let* l = keys ctxt ["a"] in
        let l = sort_keys l in
        Assert.String.List_list.equal
          ~loc:__LOC__
          [["a"; "b"]; ["a"; "c"]; ["a"; "d"; "e"]]
          l ;
        let* l = keys ctxt ["f"] in
        Assert.String.List_list.equal ~loc:__LOC__ [] l ;
        let* l = keys ctxt ["g"] in
        Assert.String.List_list.equal ~loc:__LOC__ [["g"; "h"]] l ;
        let* l = keys ctxt ["i"] in
        Assert.String.List_list.equal ~loc:__LOC__ [] l ;
        let* ctxt =
          Lwt_list.fold_left_s (fun ctxt (k, v) -> add ctxt k v) ctxt bindings
        in
        let* h = commit ctxt in
        let* ctxt = checkout_exn idx h in
        let* bs =
          fold_keys ctxt ["root"] ~order ~init:[] ~f:(fun k acc ->
              Lwt.return (k :: acc))
        in
        let bs = sort_keys bs in
        Assert.String.List_list.equal ~loc:__LOC__ (List.map fst bindings) bs ;
        Lwt.return_unit

  let test_fold_keys_sorted = test_fold_keys ~order:`Sorted

  let test_fold_keys_undefined = test_fold_keys ~order:`Undefined

  (** Checkout the context at [genesis] and fold upon a context a series
    of key settings. *)
  let test_fold {idx; genesis; _} =
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let foo1 = Bytes.of_string "foo1" in
        let foo2 = Bytes.of_string "foo2" in
        let* ctxt = add ctxt ["foo"; "toto"] foo1 in
        let* ctxt = add ctxt ["foo"; "bar"; "toto"] foo2 in
        let fold depth ecs ens =
          let* cs, ns =
            fold
              ?depth
              ctxt
              []
              ~order:`Sorted
              ~init:([], [])
              ~f:(fun path t (cs, ns) ->
                match Tree.kind t with
                | `Tree -> Lwt.return (cs, path :: ns)
                | `Value -> Lwt.return (path :: cs, ns))
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

  let test_trees {idx; genesis; _} =
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let* () =
          Tree.fold
            ~depth:(`Eq 1)
            ~order:`Sorted
            ~init:()
            (Tree.empty ctxt)
            []
            ~f:(fun k _ () ->
              assert (Compare.List_length_with.(k = 1)) ;
              Assert.fail_msg "empty")
        in
        let foo1 = Bytes.of_string "foo1" in
        let foo2 = Bytes.of_string "foo2" in
        Tree.empty ctxt |> fun v1 ->
        let* v1 = Tree.add v1 ["foo"; "toto"] foo1 in
        let* v1 = Tree.add v1 ["foo"; "bar"; "toto"] foo2 in
        let fold depth ecs ens =
          let* cs, ns =
            Tree.fold
              v1
              ?depth
              []
              ~order:`Sorted
              ~init:([], [])
              ~f:(fun path t (cs, ns) ->
                match Tree.kind t with
                | `Tree -> Lwt.return (cs, path :: ns)
                | `Value -> Lwt.return (path :: cs, ns))
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
        let* v1 = Tree.remove v1 ["foo"; "bar"; "toto"] in
        let* v = Tree.find v1 ["foo"; "bar"; "toto"] in
        Assert.Bytes.Option.equal ~loc:__LOC__ None v ;
        let* v = Tree.find v1 ["foo"; "toto"] in
        Assert.Bytes.Option.equal ~loc:__LOC__ (Some foo1) v ;
        Tree.empty ctxt |> fun v1 ->
        let* v1 = Tree.add v1 ["foo"; "1"] foo1 in
        let* v1 = Tree.add v1 ["foo"; "2"] foo2 in
        let* v1 = Tree.remove v1 ["foo"; "1"] in
        let* v1 = Tree.remove v1 ["foo"; "2"] in
        let* v = Tree.find v1 ["foo"; "1"] in
        Assert.Bytes.Option.equal ~loc:__LOC__ None v ;
        let* v1 = Tree.remove v1 [] in
        Assert.Bool.equal ~loc:__LOC__ true (Tree.is_empty v1) ;
        Lwt.return ()

  let test_raw {idx; genesis; _} =
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let foo1 = Bytes.of_string "foo1" in
        let foo2 = Bytes.of_string "foo2" in
        let* ctxt = add ctxt ["foo"; "toto"] foo1 in
        let* ctxt = add ctxt ["foo"; "bar"; "toto"] foo2 in
        let* tree = find_tree ctxt [] in
        let tree = WithExceptions.Option.get ~loc:__LOC__ tree in
        let* raw = Tree.to_raw tree in
        let a = String.Map.singleton "toto" (`Value foo1) in
        let b = String.Map.singleton "toto" (`Value foo2) in
        let c = String.Map.add "bar" (`Tree b) a in
        let d = String.Map.singleton "foo" (`Tree c) in
        let e = `Tree d in
        Raw_Tree.equal ~loc:__LOC__ e raw ;
        Lwt.return ()

  let string n = String.make n 'a'

  let test_encoding {idx; genesis; _} =
    let* o = checkout idx genesis in
    match o with
    | None -> Assert.fail_msg "checkout genesis_block"
    | Some ctxt ->
        let foo1 = Bytes.of_string "foo1" in
        let foo2 = Bytes.of_string "foo2" in
        let* ctxt = add ctxt ["a"; string 7] foo1 in
        let* ctxt = add ctxt ["a"; string 8] foo2 in
        let* ctxt = add ctxt [string 16] foo2 in
        let* ctxt = add ctxt [string 32] foo2 in
        let* ctxt = add ctxt [string 64] foo2 in
        let* ctxt = add ctxt [string 127] foo2 in
        let* h = commit ctxt in
        equal_context_hash
          ~loc:__LOC__
          (Context_hash.of_b58check_exn
             "CoWJsL2ehZ39seTr8inBCJb5tVjW8KGNweJ5cvuVq51mAASrRmim")
          h ;
        let* ctxt = add ctxt [string 255] foo2 in
        let* h = commit ctxt in
        equal_context_hash
          ~loc:__LOC__
          (Context_hash.of_b58check_exn
             "CoVexcEHMXmSA2k42aNc5MCDtVJFRs3CC6vcQWYwFoj7EFsBPw1c")
          h ;
        Lwt.return ()

  let test_is_empty {idx; block2; _} =
    let* o = checkout idx block2 in
    match o with
    | None -> Assert.fail_msg "checkout block2"
    | Some ctxt -> (
        (* By [create_block2] above, [ctxt] maps "a/b", "a/c", and "version" *)
        let etree = Context.Tree.empty ctxt in
        Assert.Bool.equal true (Tree.is_empty etree) ;
        let* o = Context.find_tree ctxt ["a"] in
        match o with
        | None -> Assert.fail_msg "dir 'a/' not found"
        | Some dir_a ->
            let* dir_a = Tree.remove dir_a ["b"] in
            let* dir_a = Tree.remove dir_a ["c"] in
            let* ls = Tree.list dir_a [] in
            let assert_equal_ls =
              Assert.equal_list ~loc:__LOC__ ~eq:( = ) ~pp:(fun ppf e ->
                  Format.pp_print_string ppf (fst e))
            in
            assert_equal_ls
              ~msg:"length of directory /a/ is unexpectedly not 0"
              []
              ls ;
            equal_context_hash
              ~loc:__LOC__
              ~msg:
                "A fresh empty tree has the same hash as a tree containing \
                 data after removing all its data"
              (Tree.hash etree)
              (Tree.hash dir_a) ;
            Assert.Bool.equal
              ~loc:__LOC__
              ~msg:"directory /a/ is unexpectedly not empty"
              true
              (Context.Tree.is_empty dir_a) ;
            Lwt.return_unit)

  (** Test that [get_hash_version succeeds] *)
  let test_get_version_hash {idx; block2; _} =
    let+ ctxt = Context.checkout_exn idx block2 in
    let _ = get_hash_version ctxt in
    ()

  (** Test [set_hash_version] on values on which it goes into the error monad *)
  let test_set_version_hash_tzresult {idx; block2; _} =
    List.iter_s
      (fun wrong_version ->
        let* ctxt = Context.checkout_exn idx block2 in
        let+ r =
          set_hash_version ctxt @@ Context_hash.Version.of_int wrong_version
        in
        match r with
        | Ok _ ->
            Assert.fail_msg "set_hash_version should have returned Error _"
        | Error _ -> ())
      (* Only version 0 is supported atm *)
      [1; 2; 256]

  let test_to_memory_tree {idx; block2; _} : unit Lwt.t =
    let open Lwt_syntax in
    let* ctxt = Context.checkout_exn idx block2 in
    let* tree = Context.to_memory_tree ctxt ["a"; "b"] in
    let () = Assert.Bool.equal true (Option.is_some tree) in
    let* tree = Context.to_memory_tree ctxt ["a"; "x"] in
    let () = Assert.Bool.equal true (Option.is_none tree) in
    return_unit

  let tree_of_list ls {idx; _} =
    let ctxt = Context.empty idx in
    let tree = Tree.empty ctxt in
    Lwt_list.fold_left_s (fun tree (k, v) -> Tree.add tree k v) tree ls

  let hash_of_contents tree key =
    let* tree = Tree.find_tree tree key in
    match tree with
    | None -> Assert.fail_msg "contents not found in tree"
    | Some t -> Lwt.return (Tree.hash t)

  let test_proof_exn ctxt =
    let open Lwt_syntax in
    let open Context.Proof in
    let bytes s = Bytes.of_string s in
    let x = bytes "x" in
    let y = bytes "y" in
    let* tree = tree_of_list [(["bx"], x); (["by"], y)] ctxt in
    let hash = Tree.hash tree in
    let* hx = hash_of_contents tree ["bx"] in
    let* hy = hash_of_contents tree ["by"] in
    let stream_elt1 : Stream.elt = Value y in
    let stream_elt2 : Stream.elt = Value x in
    let stream_elt3 : Stream.elt =
      Node [("bx", `Value hx); ("by", `Value hy)]
    in
    let stream_all =
      {
        version = 1;
        before = `Node hash;
        after = `Node hash;
        state = List.to_seq [stream_elt3; stream_elt2; stream_elt1];
      }
    in
    let stream_short =
      {
        version = 1;
        before = `Node hash;
        after = `Node hash;
        state = List.to_seq [stream_elt3; stream_elt2];
      }
    in
    let f_all t =
      let* _ = Context.Tree.find t ["bx"] in
      let+ _ = Context.Tree.find t ["by"] in
      (t, ())
    in
    let f_short t =
      let+ _ = Context.Tree.find t ["bx"] in
      (t, ())
    in
    (* Test the Stream_too_long error. *)
    let* r = Context.verify_stream_proof stream_all f_short in
    let* () =
      match r with
      | Error (`Stream_too_long _) -> Lwt.return_unit
      | _ -> Assert.fail_msg "expected Stream_too_long error"
    in
    (* Test the Stream_too_short error. *)
    let* r = Context.verify_stream_proof stream_short f_all in
    let* () =
      match r with
      | Error (`Stream_too_short _) -> Lwt.return_unit
      | _ -> Assert.fail_msg "expected Stream_too_short error"
    in
    (* Test the correct usecase. *)
    let* r = Context.verify_stream_proof stream_all f_all in
    let* () =
      match r with
      | Ok (_, ()) -> return_unit
      | Error e -> (
          match e with
          | `Proof_mismatch str ->
              Assert.fail_msg "unexpected Proof_mismatch error: %s" str
          | `Stream_too_long str ->
              Assert.fail_msg "unexpected Stream_too_long error: %s" str
          | `Stream_too_short str ->
              Assert.fail_msg "unexpected Stream_too_short error: %s" str)
    in
    return_unit

  (******************************************************************************)

  let tests : (string * (t -> unit Lwt.t)) list =
    let test name f = (Printf.sprintf "%s:%s" Tag.tag name, f) in
    [
      test "is_empty" test_is_empty;
      test "simple" test_simple;
      test "list" test_list;
      test "continuation" test_continuation;
      test "fork" test_fork;
      test "replay" test_replay;
      test "fold_keys_sorted" test_fold_keys_sorted;
      test "fold_keys_undefined" test_fold_keys_undefined;
      test "fold" test_fold;
      test "trees" test_trees;
      test "raw" test_raw;
      (* NOTE: importing the context from a snapshot requires using an [`Always]
         indexing strategy. See the docs for [Context.restore_context] for more
         details. *)
      test "encoding" test_encoding;
      test "get_hash_version" test_get_version_hash;
      test "set_hash_version_tzresult" test_set_version_hash_tzresult;
      test "to_memory_tree" test_to_memory_tree;
      test "proof exn" test_proof_exn;
    ]

  let tests =
    List.map
      (fun (s, f) -> Alcotest_lwt.test_case s `Quick (wrap_context_init f))
      tests
end

module Generic_disk =
  Make_generic
    (struct
      let tag = "disk"
    end)
    (struct
      type memory_context_tree = Tezos_context_memory.Context.tree
    end)
    (Tezos_context_disk.Context)

module Generic_memory =
  Make_generic
    (struct
      let tag = "memory"
    end)
    (struct
      type memory_context_tree = Tezos_context_memory.Context.tree
    end)
    (Tezos_context_memory.Context)

let () =
  Lwt_main.run
    (Alcotest_lwt.run
       ~__FILE__
       "tezos-context"
       [("context", List.concat [Generic_disk.tests; Generic_memory.tests])])
