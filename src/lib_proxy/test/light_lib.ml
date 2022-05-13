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

(** Definitions used in files with actual tests *)

module Store = Tezos_proxy.Local_context
open Lib_test.Qcheck_helpers

let check_irmin_tree_eq t1 t2 =
  qcheck_eq ~pp:Store.Tree.pp ~eq:Store.Tree.equal t1 t2

(** [raw_context_rm_empty rc] returns [None] if [rc] is empty, otherwise
    a variant of [rc] where empty subtrees have been removed. *)
let rec raw_context_rm_empty =
  let open Tezos_shell_services.Block_services in
  function
  | Key _ as rc -> Some rc
  | Dir dir ->
      let dir' = String.Map.filter_map (fun _ -> raw_context_rm_empty) dir in
      if String.Map.is_empty dir' then None else Some (Dir dir')
  | Cut -> None

let rec merkle_node_rm_empty =
  let open Tezos_shell_services.Block_services in
  function
  | Hash _ as h -> Some h
  | Data raw_context ->
      Option.map (fun x -> Data x) @@ raw_context_rm_empty raw_context
  | Continue mtree ->
      Option.map (fun x -> Continue x) @@ merkle_tree_rm_empty mtree

(** [merkle_tree_rm_empty mtree] returns [None] if [mtree] is empty, otherwise
    a variant of [mtree] where empty subtrees have been removed. *)
and merkle_tree_rm_empty mtree =
  let mtree' = String.Map.filter_map (fun _ -> merkle_node_rm_empty) mtree in
  if String.Map.is_empty mtree' then None else Some mtree'

(** [merkle_tree_rm_empty mtree] returns a variant of [mtree] where
    empty subtrees have been removed. *)
let merkle_tree_rm_empty mtree =
  Option.value ~default:String.Map.empty @@ merkle_tree_rm_empty mtree

module StringMap = String.Map

type simple_tree = SLeaf | SDir of simple_tree StringMap.t

let rec pp_simple_tree ppf = function
  | SLeaf -> Format.fprintf ppf "leaf"
  | SDir pairs ->
      Format.fprintf
        ppf
        "{@[<v 1>@,%a@]@,}"
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut (fun ppf (s, t) ->
             Format.fprintf ppf "%s : %a" s pp_simple_tree t))
        (StringMap.bindings pairs)

module Bifunctor = struct
  let second f = List.map (fun (a, b) -> (a, f b))
end

let sdir_of_list l =
  let dir = StringMap.of_seq @@ List.to_seq l in
  SDir dir

let rec raw_context_to_simple_tree raw_context : simple_tree =
  let open Tezos_shell_services.Block_services in
  match raw_context with
  | Cut -> SLeaf
  | Key _ -> SLeaf
  | Dir dir -> SDir (String.Map.map raw_context_to_simple_tree dir)

let is_empty = function SLeaf -> true | SDir dir -> StringMap.is_empty dir

let rec simple_tree_eq t1 t2 =
  match (t1, t2) with
  | SLeaf, SLeaf -> true
  | SDir dir1, SDir dir2 ->
      let b1 = StringMap.bindings dir1 in
      let b2 = StringMap.bindings dir2 in
      if List.length b1 != List.length b2 then false
      else
        List.for_all (fun ((k1, t1), (k2, t2)) ->
            k1 = k2 && simple_tree_eq t1 t2)
        @@ List.combine_drop b1 b2
  | SLeaf, d | d, SLeaf -> is_empty d

let rec irmin_tree_to_simple_tree tree =
  let open Lwt_syntax in
  match Store.Tree.kind tree with
  | `Value -> Lwt.return SLeaf
  | `Tree ->
      if Store.Tree.is_shallow tree then Lwt.return SLeaf
      else
        let* pairs = Store.Tree.list tree [] in
        let+ l =
          List.map_s
            (fun (k, i) ->
              let+ st = irmin_tree_to_simple_tree i in
              (k, st))
            pairs
        in
        sdir_of_list l

let rec merkle_node_to_simple_tree node =
  let open Tezos_shell_services.Block_services in
  match node with
  | Hash _ -> SLeaf
  | Data raw_context -> raw_context_to_simple_tree raw_context
  | Continue tree -> merkle_tree_to_simple_tree tree

and merkle_tree_to_simple_tree tree =
  sdir_of_list
    (Bifunctor.second merkle_node_to_simple_tree (StringMap.bindings tree))

let check_simple_tree_eq t1 t2 =
  qcheck_eq ~pp:pp_simple_tree ~eq:simple_tree_eq t1 t2
