(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

type key = string list

type kind = Value | Directory

module Children = Map.Make (String)

type 'a t = {
  value : 'a option;
  children : 'a t Children.t;
  keys_count : int;
  nodes_count : int; (* including empty one *)
}

let empty =
  {value = None; children = Children.empty; keys_count = 0; nodes_count = 1}

let nonexisting =
  {value = None; children = Children.empty; keys_count = 0; nodes_count = 0}

let replace_value t new_v =
  match t.value with
  | None -> {t with value = Some new_v; keys_count = t.keys_count + 1}
  | Some _ -> {t with value = Some new_v}

let remove_value t =
  match t.value with
  | None -> t
  | Some _ -> {t with value = None; keys_count = t.keys_count - 1}

let replace_child t step new_c =
  let old_c =
    Option.value ~default:nonexisting @@ Children.find step t.children
  in
  {
    t with
    children = Children.add step new_c t.children;
    keys_count = t.keys_count - old_c.keys_count + new_c.keys_count;
    nodes_count = t.nodes_count - old_c.nodes_count + new_c.nodes_count;
  }

let remove_child t step =
  let old_c =
    Option.value ~default:nonexisting @@ Children.find step t.children
  in
  {
    t with
    children = Children.remove step t.children;
    keys_count = t.keys_count - old_c.keys_count;
    nodes_count = t.nodes_count - old_c.nodes_count;
  }

(* Helpers *)

let guard_opt b = if b then Some () else None

let is_key_readonly = function "readonly" :: _ -> true | _ -> false

let rec create_path (t : 'a t) f_t key =
  match key with
  | [] -> f_t t
  | k :: rest ->
      let child = Option.value ~default:empty @@ Children.find k t.children in
      let new_child = create_path child f_t rest in
      replace_child t k new_child

let lookup key root =
  let open Option_syntax in
  let rec lookup_impl t = function
    | [] -> Some t
    | k :: rest ->
        let* child = Children.find k t.children in
        lookup_impl child rest
  in
  lookup_impl root key

let readonly_guard key edit_readonly =
  guard_opt (if is_key_readonly key then edit_readonly else true)

(* Public functions.
   Functions return Some if an operation has completed successfully.
*)
let set_value ~edit_readonly key v root =
  let open Option_syntax in
  let+ () = readonly_guard key edit_readonly in
  create_path root (fun t -> replace_value t v) key

let get_value key root = Option.bind (lookup key root) (fun x -> x.value)

let subtrees_size key root =
  Option.fold ~none:0 ~some:(fun x -> Children.cardinal x.children)
  @@ lookup key root

let delete ~edit_readonly kind key root =
  let open Option_syntax in
  let* () = readonly_guard key edit_readonly in
  let is_empty {value; children; _} =
    Option.is_none value && Children.is_empty children
  in
  (* Return None if tree is not changed in result of deletion *)
  let rec delete_tree_impl t = function
    | [] -> None
    | [k] when kind = Directory -> Some (remove_child t k)
    | [k] when kind = Value ->
        let* child = Children.find k t.children in
        Some (remove_value child)
    | k :: rest ->
        let* child = Children.find k t.children in
        let+ new_child = delete_tree_impl child rest in
        let new_t = remove_child t k in
        (* If k is the only child of t and has no value,
           then we should "collapse" this branch *)
        if is_empty new_child && is_empty new_t then empty
          (* If new_child is empty: we don't need to store it anymore *)
        else if is_empty new_child then new_t
          (* Just replace old k with new one*)
        else replace_child t k new_child
  in
  delete_tree_impl root key

let copy_tree ~edit_readonly ~from_key ~to_key root =
  let open Option_syntax in
  let* from_t = lookup from_key root in
  let+ () = readonly_guard to_key edit_readonly in
  create_path root (Fun.const from_t) to_key

let move_tree ~from_key ~to_key root =
  let open Option_syntax in
  let* () =
    guard_opt ((not (is_key_readonly from_key)) && not (is_key_readonly to_key))
  in
  let* from_t = lookup from_key root in
  let+ root = delete ~edit_readonly:true Directory from_key root in
  create_path root (Fun.const from_t) to_key
