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

open QCheck2
open Durable_operation_generator

type key = string list

type program = Durable_operation.t list

type testcase = {inital_state : (key * string) list; operations : program}

let range i j =
  WithExceptions.List.init ~loc:__LOC__ (1 + j - i) (fun n -> i + n)

let range_chars (i : char) (j : char) =
  List.map Char.chr @@ range (Char.code i) (Char.code j)

let max_path_segments = 10

let gen_path_char =
  Gen.oneof
    [
      Gen.oneofl ['.'; '-'; '_'];
      Gen.map Char.chr (Gen.int_range (Char.code 'a') (Char.code 'z'));
      Gen.map Char.chr (Gen.int_range (Char.code 'A') (Char.code 'Z'));
      Gen.map Char.chr (Gen.int_range (Char.code '0') (Char.code '9'));
    ]

let generate_initial_keys (initial_tree_size : int) =
  let open Gen in
  let+ keys =
    Gen.list_size (Gen.return initial_tree_size)
    @@ gen_arbitrary_path
         ~max_len:Key_generator_params.max_key_length
         ~max_segments_num:20
         Key_generator_params.key_alphabet
  and+ values =
    Gen.list_size
      (Gen.return initial_tree_size)
      (Gen.string_size ~gen:Gen.char (Gen.int_bound 2048))
  in
  let kv = WithExceptions.List.combine ~loc:__LOC__ keys values in
  (* 20% of all keys go to readonly subpath *)
  let ro_ops = Int.(div (List.length kv) 5) in
  List.mapi
    (fun i (k, v) -> if i < ro_ops then ("readonly" :: k, v) else (k, v))
    kv

let apply_operation_to_trie trie operation =
  let open Durable_operation in
  Option.value ~default:trie
  @@
  match operation with
  | Operation (Set_value_exn, (edit_readonly, key, value)) ->
      Trie.set_value ~edit_readonly key (String.length value) trie
  | Operation (Copy_tree_exn, (edit_readonly, from_key, to_key)) ->
      Trie.copy_tree ~edit_readonly ~from_key ~to_key trie
  | Operation (Move_tree_exn, (from_key, to_key)) ->
      Trie.move_tree ~from_key ~to_key trie
  | Operation (Delete, (edit_readonly, key)) ->
      Trie.delete ~edit_readonly Directory key trie
  | Operation (Write_value_exn, (edit_readonly, key, offset, value)) ->
      let new_value =
        Int.max
          (Option.value ~default:0 @@ Trie.get_value key trie)
          (Int64.to_int offset + String.length value)
      in
      Trie.set_value ~edit_readonly key new_value trie
  | _ -> None

let generate_operation trie ops_distribution =
  Gen.frequency @@ List.map (fun (w, g) -> (w, g trie)) ops_distribution

let rec gen_operations (trie : int Trie.t) ops_distribution n ops =
  let open Gen in
  if n <= 0 then Gen.return @@ List.rev ops
  else
    let* operation = generate_operation trie ops_distribution in
    let new_trie = apply_operation_to_trie trie operation in
    (gen_operations [@tailcall])
      new_trie
      ops_distribution
      (n - 1)
      (operation :: ops)

let gen_testcase ~(initial_size : int) ~(operations_number : int)
    (distirbution : operations_distribution) =
  let open Gen in
  let* init_kvs = generate_initial_keys initial_size in
  let initial_trie =
    List.fold_left
      (fun trie (k, v) ->
        WithExceptions.Option.get ~loc:__LOC__
        @@ Trie.set_value ~edit_readonly:true k (String.length v) trie)
      Trie.empty
      init_kvs
  in
  let+ ops = gen_operations initial_trie distirbution operations_number [] in
  {inital_state = init_kvs; operations = ops}
