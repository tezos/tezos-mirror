(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Invocation:   dune build @src/lib_protocol_environment/runtest_mem_context_array_theory
    Dependencies: src/lib_protocol_environment/test/test_mem_context.ml
    Subject:      get/set operations on memory contexts.
*)

(**
    Tests [Memory_context] by making sure it's an instance of array theory.
    Being an array theory means honoring the following axioms (see the
    literature on the corresponding decision procedure):

    * get (set m k v) v = v
    * forall k1 <> k2, get (set m k1 v) k2 = get m k2

    The first axiom is tested in [test_get_set] while the second axiom
    is tested in [test_get_set_other].

    These tests complement [Test_mem_context]: while [Test_mem_context]
    creates values of Context.t manually, this file
    use automatically generated values; thanks to [Crowbar].
*)

type key = Context.key

type value = Context.value

(** On the one hand, it seemed interesting to have a limited
    domain for keys (see [key_gen] implementation) and on the other hand fully
    random values are good too. Hence testing both modes. We do
    the same for values, for consistency. *)
type mode = Random | Limited

let mode_to_string = function Random -> "Random" | Limited -> "Limited"

let key_gen mode =
  let open Crowbar in
  match mode with
  | Limited ->
      (* We voluntarily choose a limited subset, so that get and set
       * events likely have conflicting keys. That's where the most likely
       * failures could happen. This is complementary to data generated
       * by a really random generator *)
      let strs = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] in
      List.map const strs |> choose |> list1
  | Random ->
      bytes |> list

let value_gen =
  let open Crowbar in
  map [bytes] Bytes.of_string

let key_value_gen kmode =
  let open Crowbar in
  map [key_gen kmode; value_gen] (fun k v -> (k, v))

(* Function set in memory_context.ml relies on a lower-level function
   named 'raw_set' which can fail with an exception when setting a value
   below an existing Key entry. Function [safe_set] circumvents this by
   aborting the enclosing test (by calling [Crowbar.bad_test]) if the (k, v)
   pair to set would trigger the exception. *)
let safe_set m k v =
  let prefix l =
    if l = [] then None
    else
      Some
        ( List.rev l |> List.tl
        |> WithExceptions.Option.get ~loc:__LOC__
        |> List.rev )
  in
  let rec any_prefix_mem m k =
    let prefix = prefix k in
    match prefix with
    | None ->
        Lwt.return_false
    | Some prefix -> (
        Context.mem m prefix
        >>= function
        | true -> Lwt.return_true | false -> any_prefix_mem m prefix )
  in
  (* If any_prefix_mem m k holds, Context.set will throw an exception; hence: *)
  any_prefix_mem m k
  >>= function true -> Crowbar.bad_test () | false -> Context.add m k v

(* We generate contexts by starting from a fresh one and
   doing a sequence of calls to Context.set *)
let context_gen kv_gen : Context.t Crowbar.gen =
  let open Crowbar in
  let rec set_seq ctxt key_value_list =
    match key_value_list with
    | [] ->
        Lwt.return ctxt
    | (k, v) :: rest ->
        safe_set ctxt k v >>= fun ctxt' -> set_seq ctxt' rest
  in
  let set_seq ctxt kvs = Lwt_main.run @@ set_seq ctxt kvs in
  map [list1 kv_gen] (set_seq Memory_context.empty)

(* We're done with generators. *)

(* Test that [Test_mem_context.domain] is correct. Important because
   this function is used in the test of the second axiom of array theory:
   [test_get_set_other]. Also this serves as a specification of
   [Test_mem_context.domain]. *)
let test_domain_spec (ctxt, k) =
  if k = [] then
    (* This is a bit puzzling, but the empty key is special; because
       of the implementation of memory_context.ml's raw_get method which
       returns true on the empty key. This means the empty key
       is considered to exist in an empty context, on which,
       [Test_mem_contex.domain] appropriately returns an empty list. One
       could complexify this test to support this case, but I didn't want
       to spend too much time on this; we're testing a test after all here. *)
    Crowbar.bad_test ()
  else
    let domain = Lwt_main.run @@ Test_mem_context.domain ctxt in
    Crowbar.check_eq (Lwt_main.run @@ Context.mem ctxt k) (List.mem k domain)

(* Tests that (get (set m k v) k) equals v.
   This is the first axiom of array theory *)
let test_get_set (ctxt, (k, v)) =
  (* The initial context *)
  let at_k =
    Lwt_main.run
      (* Map k to v *)
      ( safe_set ctxt k v
      >>= fun ctxt' ->
      (* Read value at k *)
      Context.find ctxt' k )
  in
  match at_k with
  | None ->
      (* k must be mapped *)
      Crowbar.fail "at_k not mapped after set"
  | Some at_k ->
      (* and it must be mapped to v *)
      Crowbar.check_eq at_k v

let value_opt ppf value_opt =
  let ppv ppf v = Format.fprintf ppf "%s" @@ Bytes.to_string v in
  Format.pp_print_option ppv ppf value_opt

(* Tests that: forall k2 <> k1, (get (set m k1 v) k2) equals get m k2;
 * i.e. setting a key doesn't affect other keys.
 * This is the second axiom of array theory *)
let test_get_set_other (ctxt, (k1, v)) =
  let ctxt' = Lwt_main.run @@ safe_set ctxt k1 v in
  let keys = Lwt_main.run @@ Test_mem_context.domain ctxt' in
  let check_key k2 =
    if k1 = k2 then ()
    else
      let v_before = Lwt_main.run @@ Context.find ctxt k2 in
      let v_after = Lwt_main.run @@ Context.find ctxt' k2 in
      Crowbar.check_eq ~pp:value_opt v_before v_after
  in
  List.iter check_key keys

let test_set_domain (ctxt, (k, v)) =
  let domain = Lwt_main.run @@ Test_mem_context.domain ctxt in
  let ctxt' = Lwt_main.run @@ safe_set ctxt k v in
  let domain' = Lwt_main.run @@ Test_mem_context.domain ctxt' in
  Crowbar.check
    (List.for_all
       (fun in_domain' -> in_domain' = k || List.mem in_domain' domain)
       domain')

let () =
  List.iter
    (fun kmode ->
      let k_gen = key_gen kmode in
      let kv_gen = key_value_gen kmode in
      let context_gen = context_gen kv_gen in
      let mode_str = Printf.sprintf "[%s]" (mode_to_string kmode) in
      Crowbar.add_test
        ~name:("Test_mem_context.domain's specification " ^ mode_str)
        [Crowbar.pair context_gen k_gen]
        test_domain_spec ;
      Crowbar.add_test
        ~name:("get (set m k v) k = v " ^ mode_str)
        [Crowbar.pair context_gen kv_gen]
        test_get_set ;
      Crowbar.add_test
        ~name:("forall k1 <> k2, get (set m k1 v) k2 = get m k2 " ^ mode_str)
        [Crowbar.pair context_gen kv_gen]
        test_get_set_other ;
      Crowbar.add_test
        ~name:
          ( "forall k2 in domain (set m k1 v), k2 in domain m || k1 = k2 "
          ^ mode_str )
        [Crowbar.pair context_gen kv_gen]
        test_set_domain)
    [Random; Limited]
