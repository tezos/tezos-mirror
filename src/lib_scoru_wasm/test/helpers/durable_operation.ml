(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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

open Tezos_test_helpers

type key = string list

let key_to_str key_list = String.concat "/" ("" :: key_list)

let key_len key_list =
  List.fold_left (fun acc seg -> String.length seg + acc + 1) 0 ("" :: key_list)

let kind_to_str = function `Value -> "Value" | `Subtree -> "Subtree"

(* GADT type, each constructor's type represents a type parameters
   which are taken as input of corresponding operation *)
type _ operation_kind =
  (* key *)
  | Find_value : key operation_kind
  (* edit_readonly, key, value *)
  | Find_value_exn : key operation_kind
  (* edit_readonly, key, value *)
  | Set_value_exn : (bool * key * string) operation_kind
  (* edit_readonly, key_from, key_to *)
  | Copy_tree_exn : (bool * key * key) operation_kind
  (* key_from, key_to *)
  | Move_tree_exn : (key * key) operation_kind
  (* edit_readonly, key *)
  | Delete : (bool * key) operation_kind
  (* key *)
  | List : key operation_kind
  (* key *)
  | Count_subtrees : key operation_kind
  (* key, idx*)
  | Substree_name_at : (key * int) operation_kind
  (* key *)
  | Hash : key operation_kind
  (* key *)
  | Hash_exn : key operation_kind
  (* edit_readonly, key, offset, value *)
  | Write_value_exn : (bool * key * int64 * string) operation_kind
  (* key, offset, len *)
  | Read_value_exn : (key * int64 * int64) operation_kind

let pp_operation_kind (type a) fmt (x : a operation_kind) =
  match x with
  | Find_value -> Format.fprintf fmt "find_value"
  | Find_value_exn -> Format.fprintf fmt "find_value_exn"
  | Set_value_exn -> Format.fprintf fmt "set_value_exn"
  | Copy_tree_exn -> Format.fprintf fmt "copy_tree_exn"
  | Move_tree_exn -> Format.fprintf fmt "move_tree_exn"
  | Delete -> Format.fprintf fmt "delete"
  | List -> Format.fprintf fmt "list"
  | Count_subtrees -> Format.fprintf fmt "count_subtrees"
  | Substree_name_at -> Format.fprintf fmt "substree_name_at"
  | Hash -> Format.fprintf fmt "hash"
  | Hash_exn -> Format.fprintf fmt "hash_exn"
  | Write_value_exn -> Format.fprintf fmt "write_value_exn"
  | Read_value_exn -> Format.fprintf fmt "read_value_exn"

(* Existentially quantified operation_kind *)
type operation_tag = Operation_tag : 'a operation_kind -> operation_tag

let pp_operation_tag fmt (x : operation_tag) =
  match x with Operation_tag op -> pp_operation_kind fmt op

(* Operation kind + operation arguments *)
type t = Operation : 'a operation_kind * 'a -> t

let pp fmt (x : t) =
  match x with
  | Operation (Find_value, key) ->
      Format.fprintf fmt "%a(%s)" pp_operation_kind Find_value @@ key_to_str key
  | Operation (Find_value_exn, key) ->
      Format.fprintf fmt "%a(%s)" pp_operation_kind Find_value_exn
      @@ key_to_str key
  | Operation (Set_value_exn, (edit_readonly, key, _value)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s, value: %s)"
        pp_operation_kind
        Set_value_exn
        Fmt.bool
        edit_readonly
        (key_to_str key)
        "<value>"
  | Operation (Copy_tree_exn, (edit_readonly, from, to_)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, from: %s, to: %s)"
        pp_operation_kind
        Copy_tree_exn
        Fmt.bool
        edit_readonly
        (key_to_str from)
        (key_to_str to_)
  | Operation (Move_tree_exn, (from, to_)) ->
      Format.fprintf
        fmt
        "%a(from: %s, to: %s)"
        pp_operation_kind
        Move_tree_exn
        (key_to_str from)
        (key_to_str to_)
  | Operation (Delete, (edit_readonly, key)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s)"
        pp_operation_kind
        Delete
        Fmt.bool
        edit_readonly
        (key_to_str key)
  | Operation (List, key) ->
      Format.fprintf fmt "%a(%s)" pp_operation_kind List @@ key_to_str key
  | Operation (Count_subtrees, key) ->
      Format.fprintf fmt "%a(%s)" pp_operation_kind Count_subtrees
      @@ key_to_str key
  | Operation (Substree_name_at, (key, idx)) ->
      Format.fprintf
        fmt
        "%a(key: %s, index: %d)"
        pp_operation_kind
        Substree_name_at
        (key_to_str key)
        idx
  | Operation (Hash, key) ->
      Format.fprintf fmt "%a(key: %s)" pp_operation_kind Hash (key_to_str key)
  | Operation (Hash_exn, key) ->
      Format.fprintf
        fmt
        "%a(key: %s)"
        pp_operation_kind
        Hash_exn
        (key_to_str key)
  | Operation (Write_value_exn, (edit_readonly, key, offset, _value)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s, offset: %Ld, value: %s)"
        pp_operation_kind
        Write_value_exn
        Fmt.bool
        edit_readonly
        (key_to_str key)
        offset
        "<value>"
  | Operation (Read_value_exn, (key, offset, len)) ->
      Format.fprintf
        fmt
        "%a(key: %s, offset: %Ld, len: %Ld)"
        pp_operation_kind
        Read_value_exn
        (key_to_str key)
        offset
        len

module Map = Map.Make (struct
  type t = operation_tag

  let compare = Stdlib.compare
end)

module Set = Set.Make (struct
  type t = operation_tag

  let compare = Stdlib.compare
end)

let write_operation_tags : operation_tag list =
  [Operation_tag Write_value_exn; Operation_tag Set_value_exn]

let read_operation_tags : operation_tag list =
  [
    Operation_tag Find_value;
    Operation_tag Read_value_exn;
    Operation_tag Find_value_exn;
  ]

let structure_inspection_operation_tags : operation_tag list =
  [
    Operation_tag Hash;
    Operation_tag List;
    Operation_tag Count_subtrees;
    Operation_tag Substree_name_at;
    Operation_tag Hash_exn;
  ]

let structure_modification_operation_tags : operation_tag list =
  [
    Operation_tag Delete;
    Operation_tag Copy_tree_exn;
    Operation_tag Move_tree_exn;
  ]

let all_operation_tags : operation_tag list =
  let all =
    List.concat
      [
        write_operation_tags;
        read_operation_tags;
        structure_modification_operation_tags;
        structure_inspection_operation_tags;
      ]
  in
  Assert.Int.equal
    ~loc:__LOC__
    ~msg:"Not exhaust list of durable operations"
    (List.length all)
    13 ;
  all
