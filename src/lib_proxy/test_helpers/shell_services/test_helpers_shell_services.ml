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

open Qcheck2_helpers
module Proof = Tezos_context_sigs.Context.Proof_types

let raw_context_gen =
  let open Proof in
  let module MapGen = MakeMapGen (String.Map) in
  let open QCheck2 in
  let open Gen in
  (* Factor used to limit the depth of the tree. *)
  let max_depth_factor = 10 in
  fix
    (fun self current_depth_factor ->
      frequency
        [
          (max_depth_factor, map (fun b -> Key b) bytes_gen);
          (max_depth_factor, pure Cut);
          ( current_depth_factor,
            map
              (fun d -> Dir d)
              (MapGen.gen_of_size
                 (0 -- 10)
                 string
                 (self (current_depth_factor / 2))) );
        ])
    max_depth_factor

let print_raw_context = Format.asprintf "%a" Proof.pp_raw_context

module Store = Tezos_context_memory.Context

let empty = Tezos_context_memory.make_empty_context ()

(* Stolen from src/lib_proxy/test/test_fuzzing_light.ml *)
let irmin_tree_gen =
  let open QCheck2.Gen in
  let+ entries =
    (* we want the list of entries to be nonempty, hence the "+1" *)
    list_size
      (map (( + ) 1) small_nat)
      (pair (small_list (small_string ~gen:char)) small_bytes_gen)
  in
  let tree =
    List.fold_left_s
      (fun built_tree (path, bytes) -> Store.Tree.add built_tree path bytes)
      (Store.Tree.empty empty)
      entries
    |> Lwt_main.run
  in
  (tree, entries)

let ( let** ) = Lwt_syntax.( let* )

let merkle_proof_gen =
  let open QCheck2.Gen in
  let* tree, entries = irmin_tree_gen and* root = small_string ~gen:char in
  let store =
    (let** store = Store.add_tree empty [root] tree in
     let** _ = Store.commit ~time:Time.Protocol.epoch store in
     Lwt.return store)
    |> Lwt_main.run
  in
  match Store.Tree.kinded_key tree with
  | None ->
      raise
        (Invalid_argument
           "In-memory context.tree has no kinded_key after commit")
  | Some kinded_key ->
      let* path = map fst @@ oneofl entries in
      let proof, _ =
        Store.produce_tree_proof
          (Store.index store)
          kinded_key
          (let open Lwt_syntax in
          fun t ->
            let* _ = Store.Tree.find t path in
            return (t, ()))
        |> Lwt_main.run
      in
      return (proof, tree, path)

let print_merkle_proof (_, tree, path) =
  Format.asprintf
    "<Merkle proof> on tree\n%a\nwith path %s"
    Store.Tree.pp
    tree
    (String.concat ";" path)
