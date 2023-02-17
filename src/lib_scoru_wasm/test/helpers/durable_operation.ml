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

(* GADT type, each constructor's type represents a type parameters
   which are taken as input of corresponding operation *)
type _ t =
  (* key *)
  | Find_value : key t
  (* edit_readonly, key, value *)
  | Find_value_exn : key t
  (* edit_readonly, key, value *)
  | Set_value_exn : (bool * key * string) t
  (* edit_readonly, key_from, key_to *)
  | Copy_tree_exn : (bool * key * key) t
  (* key_from, key_to *)
  | Move_tree_exn : (key * key) t
  (* edit_readonly, key *)
  | Delete : (bool * key) t
  (* key *)
  | List : key t
  (* key *)
  | Count_subtrees : key t
  (* key, idx*)
  | Substree_name_at : (key * int) t
  (* key *)
  | Hash : key t
  (* key *)
  | Hash_exn : key t
  (* edit_readonly, key, offset, value *)
  | Write_value_exn : (bool * key * int64 * string) t
  (* key, offset, len *)
  | Read_value_exn : (key * int64 * int64) t

let pp (type a) fmt (x : a t) =
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

type some_op = Some_op : 'a t -> some_op

let pp_some_op fmt (x : some_op) = match x with Some_op op -> pp fmt op

type some_input = Some_input : 'a t * 'a -> some_input

let pp_some_input fmt (x : some_input) =
  match x with
  | Some_input (Find_value, key) ->
      Format.fprintf fmt "%a(%s)" pp Find_value @@ key_to_str key
  | Some_input (Find_value_exn, key) ->
      Format.fprintf fmt "%a(%s)" pp Find_value_exn @@ key_to_str key
  | Some_input (Set_value_exn, (edit_readonly, key, _value)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s, value: %s)"
        pp
        Set_value_exn
        Fmt.bool
        edit_readonly
        (key_to_str key)
        "<value>"
  | Some_input (Copy_tree_exn, (edit_readonly, from, to_)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, from: %s, to: %s)"
        pp
        Copy_tree_exn
        Fmt.bool
        edit_readonly
        (key_to_str from)
        (key_to_str to_)
  | Some_input (Move_tree_exn, (from, to_)) ->
      Format.fprintf
        fmt
        "%a(from: %s, to: %s)"
        pp
        Move_tree_exn
        (key_to_str from)
        (key_to_str to_)
  | Some_input (Delete, (edit_readonly, key)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s)"
        pp
        Delete
        Fmt.bool
        edit_readonly
        (key_to_str key)
  | Some_input (List, key) ->
      Format.fprintf fmt "%a(%s)" pp List @@ key_to_str key
  | Some_input (Count_subtrees, key) ->
      Format.fprintf fmt "%a(%s)" pp Count_subtrees @@ key_to_str key
  | Some_input (Substree_name_at, (key, idx)) ->
      Format.fprintf
        fmt
        "%a(key: %s, index: %d)"
        pp
        Substree_name_at
        (key_to_str key)
        idx
  | Some_input (Hash, key) ->
      Format.fprintf fmt "%a(%s)" pp Hash (key_to_str key)
  | Some_input (Hash_exn, key) ->
      Format.fprintf fmt "%a(%s)" pp Hash_exn (key_to_str key)
  | Some_input (Write_value_exn, (edit_readonly, key, offset, _value)) ->
      Format.fprintf
        fmt
        "%a(edit_readonly: %a, key: %s, offset: %Ld, value: %s)"
        pp
        Write_value_exn
        Fmt.bool
        edit_readonly
        (key_to_str key)
        offset
        "<value>"
  | Some_input (Read_value_exn, (key, offset, len)) ->
      Format.fprintf
        fmt
        "%a(key: %s, offset: %Ld, len: %Ld)"
        pp
        Read_value_exn
        (key_to_str key)
        offset
        len

module Map = Map.Make (struct
  type t = some_op

  let compare = Stdlib.compare
end)

module Set = Set.Make (struct
  type t = some_op

  let compare = Stdlib.compare
end)

let write_operations : some_op list =
  [Some_op Write_value_exn; Some_op Set_value_exn]

let read_operations : some_op list =
  [Some_op Find_value; Some_op Read_value_exn; Some_op Find_value_exn]

let structure_inspection_operations : some_op list =
  [
    Some_op Hash;
    Some_op List;
    Some_op Count_subtrees;
    Some_op Substree_name_at;
    Some_op Hash_exn;
  ]

let structure_modification_operations : some_op list =
  [Some_op Delete; Some_op Copy_tree_exn; Some_op Move_tree_exn]

let all_operations : some_op list =
  let all =
    List.concat
      [
        write_operations;
        read_operations;
        structure_modification_operations;
        structure_inspection_operations;
      ]
  in
  Assert.Int.equal
    ~loc:__LOC__
    ~msg:"Not exhaust list of durable operations"
    (List.length all)
    13 ;
  all
