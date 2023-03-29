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

open Durable_program_generator
open Durable_snapshot_util

module Make_durable_program_runner
    (Durable : Durable_snapshot_util.Testable_durable_sig) =
struct
  open Lwt_syntax
  open Tezos_scoru_wasm_helpers.Encodings_util
  open Tezos_scoru_wasm_helpers.Wasm_utils

  let durable_exn_handler (act : unit -> 'a Lwt.t)
      (cont : ('a, exn) result -> 'b Lwt.t) =
    Lwt.try_bind
      act
      (fun res -> cont @@ Ok res)
      (fun e ->
        Tezos_scoru_wasm_durable_snapshot.Durable.(
          match e with
          | Invalid_key _ | Index_too_large _ | Value_not_found | Tree_not_found
          | Out_of_bounds _ | Durable_empty | Readonly_value | IO_too_large
          | Tezos_lazy_containers.Chunked_byte_vector.Bounds
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4958 *)
          | Tezos_tree_encoding.Key_not_found _ ->
              cont @@ Error e
          (* If it's another kind of exn:
             something went wrong, re-throw it*)
          | _ -> raise e))

  (* Stress test doesn't care about exceptions
     thrown out of functions.
     It's implied that underlying Durable has already checked them.
  *)
  type op_res = Tree : Durable.t -> op_res | Value : 'a -> op_res

  let tree_res (act : Durable.t Lwt.t) = Lwt.map (fun x -> Tree x) act

  let value_res (act : 'a Lwt.t) = Lwt.map (fun x -> Value x) act

  let supress_durable_exn dur (act : unit -> op_res Lwt.t) =
    durable_exn_handler act (fun x ->
        match x with
        | Ok (Tree t) -> Lwt.return t
        | Ok (Value _value) -> Lwt.return dur
        | _ -> Lwt.return dur)

  (* Create new tree with passed list of key values *)
  let initialize_tree (kvs : (key * string) list) =
    let open Lwt_syntax in
    let open Tezos_scoru_wasm_durable_snapshot in
    let ro, wo =
      List.partition
        (fun (k, _) -> Option.equal String.equal (List.hd k) (Some "readonly"))
        kvs
    in
    let ro = List.map (fun (k, v) -> (Durable_operation.key_to_str k, v)) ro in
    let wo = List.map (fun (k, v) -> (Durable_operation.key_to_str k, v)) wo in
    (* Create Durable_storage out of WO keys.
       Basically taking advantage of Current durable encoding
    *)
    let* init_wo = Lwt.map Durable.of_storage_exn @@ make_durable wo in
    (* Add RO keys in the tree *)
    let* init_tezos_durable =
      Lwt_list.fold_left_s
        (fun dur (k, v) ->
          Durable.set_value_exn
            ~edit_readonly:true
            dur
            (Durable.key_of_string_exn k)
            v)
        init_wo
        ro
    in
    (* Encode tree to the irmin one *)
    let* init_tree = empty_tree () in
    Tree_encoding_runner.encode
      Tezos_scoru_wasm_durable_snapshot.Durable.encoding
      init_tezos_durable
      init_tree

  let run_testcase {inital_state; operations} =
    let open Durable_operation in
    let* init_tree = initialize_tree inital_state in
    (* Decode initial paired durable *)
    let* init_durable =
      Tree_encoding_runner.decode Durable.encoding init_tree
    in
    Lwt_list.fold_left_s
      (fun dur op ->
        supress_durable_exn dur @@ fun () ->
        match op with
        | Operation (Find_value, key) ->
            value_res
            @@ Durable.find_value
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Find_value_exn, key) ->
            value_res
            @@ Durable.find_value_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Set_value_exn, (edit_readonly, key, value)) ->
            tree_res
            @@ Durable.set_value_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 value
        | Operation (Copy_tree_exn, (edit_readonly, from, to_)) ->
            tree_res
            @@ Durable.copy_tree_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str from)
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str to_)
        | Operation (Move_tree_exn, (from, to_)) ->
            tree_res
            @@ Durable.move_tree_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str from)
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str to_)
        | Operation (Delete, (edit_readonly, key)) ->
            tree_res
            @@ Durable.delete
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (List, key) ->
            value_res
            @@ Durable.list
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Count_subtrees, key) ->
            value_res
            @@ Durable.count_subtrees
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Substree_name_at, (key, idx)) ->
            value_res
            @@ Durable.subtree_name_at
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 idx
        | Operation (Hash, (key, kind)) ->
            value_res
            @@ Durable.hash
                 ~kind
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Hash_exn, (key, kind)) ->
            value_res
            @@ Durable.hash_exn
                 ~kind
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
        | Operation (Write_value_exn, (edit_readonly, key, offset, value)) ->
            tree_res
            @@ Durable.write_value_exn
                 ~edit_readonly
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 offset
                 value
        | Operation (Read_value_exn, (key, offset, len)) ->
            value_res
            @@ Durable.read_value_exn
                 dur
                 (Durable.key_of_string_exn @@ Durable_operation.key_to_str key)
                 offset
                 len)
      init_durable
      operations
end

module Verifiable_program_runner =
  Make_durable_program_runner (Verifiable_current_durable)
