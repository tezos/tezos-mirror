(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

(* Version of durable storage corresponding to
   https://gitlab.com/tezos/tezos/-/blob/668fe735aa20ce0c68b9f836208e57fa15d389c1/src/lib_scoru_wasm/durable.ml
*)

open Tezos_lazy_containers
module T = Tezos_tree_encoding.Wrapped
module Runner = Tezos_tree_encoding.Runner.Make (Tezos_tree_encoding.Wrapped)
module E = Tezos_tree_encoding
module Storage = Tezos_webassembly_interpreter.Durable_storage

type t = T.tree

(* The maximum size of bytes allowed to be read/written at once. *)
let max_store_io_size = 2048L

exception Invalid_key of string

exception Index_too_large of int

exception Value_not_found

exception Tree_not_found

exception Durable_empty = Storage.Durable_empty

exception Out_of_bounds of (int64 * int64)

exception IO_too_large

exception Readonly_value

let encoding = E.wrapped_tree

let of_storage ~default s =
  match Storage.to_tree s with Some t -> t | None -> default

let of_storage_exn s = Storage.to_tree_exn s

let to_storage d = Storage.of_tree d

type key = Writeable of string list | Readonly of string list

(* A key is bounded to 250 bytes, including the implicit '/durable' prefix.
   Additionally, values are implicitly appended with '_'. **)
let max_key_length = 250 - String.length "/durable" - String.length "/@"

let key_of_string_exn s =
  if String.length s > max_key_length then raise (Invalid_key s) ;
  let key =
    match String.split '/' s with
    | "" :: tl -> tl (* Must start with '/' *)
    | _ -> raise (Invalid_key s)
  in
  let assert_valid_char = function
    | '.' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> ()
    | _ -> raise (Invalid_key s)
  in
  let all_steps_valid =
    List.for_all (fun x ->
        x <> ""
        &&
        (String.iter assert_valid_char x ;
         true))
  in
  if all_steps_valid key then
    match key with "readonly" :: _ | [] -> Readonly key | _ -> Writeable key
  else raise (Invalid_key s)

let key_of_string_opt s =
  try Some (key_of_string_exn s) with Invalid_key _ -> None

(** We append all values with '@', which is an invalid key-character w.r.t.
    external use.

    This ensures that an external user is prevented from accidentally writing a
    value to a place which is part of another value (e.g. writing a
    chunked_byte_vector to "/a/length", where "/a/length" previously existed as
    part of another chunked_byte_vector encoding.)
*)
let value_marker = "@"

let to_value_key k = List.append k [value_marker]

let assert_key_writeable = function
  | Readonly _ -> raise Readonly_value
  | Writeable _ -> ()

let assert_max_bytes max_bytes =
  if max_store_io_size < max_bytes then raise IO_too_large

let key_contents = function Readonly k | Writeable k -> k

let find_value tree key =
  let open Lwt.Syntax in
  let key = key_contents key in
  let* opt = T.find_tree tree @@ to_value_key key in
  match opt with
  | None -> Lwt.return_none
  | Some subtree ->
      let+ value = Runner.decode Chunked_byte_vector.encoding subtree in
      Some value

let find_value_exn tree key =
  let open Lwt.Syntax in
  let+ opt = find_value tree key in
  match opt with None -> raise Value_not_found | Some value -> value

(** helper function used in the copy/move *)
let find_tree_exn tree key =
  let open Lwt.Syntax in
  let key = key_contents key in
  let+ opt = T.find_tree tree key in
  match opt with None -> raise Tree_not_found | Some subtree -> subtree

let copy_tree_exn tree ?(edit_readonly = false) from_key to_key =
  let open Lwt.Syntax in
  if not edit_readonly then assert_key_writeable to_key ;
  let* move_tree = find_tree_exn tree from_key in
  let to_key = key_contents to_key in
  T.add_tree tree to_key move_tree

let count_subtrees tree key = T.length tree @@ key_contents key

let list tree key =
  let open Lwt.Syntax in
  let+ subtrees = T.list tree @@ key_contents key in
  List.map (fun (name, _) -> if name = "@" then "" else name) subtrees

let delete ?(edit_readonly = false) tree key =
  if not edit_readonly then assert_key_writeable key ;
  T.remove tree @@ key_contents key

let subtree_name_at tree key index =
  let open Lwt.Syntax in
  let* subtree = find_tree_exn tree key in
  let* list = T.list ~offset:index ~length:1 subtree [] in
  let nth = List.nth list 0 in
  match nth with
  | Some (step, _) when Compare.String.(step = value_marker) -> Lwt.return ""
  | Some (step, _) -> Lwt.return step
  | None -> raise (Index_too_large index)

let move_tree_exn tree from_key to_key =
  let open Lwt.Syntax in
  assert_key_writeable from_key ;
  assert_key_writeable to_key ;
  let* move_tree = find_tree_exn tree from_key in
  let* tree = delete tree from_key in
  T.add_tree tree (key_contents to_key) move_tree

let hash tree key =
  let open Lwt.Syntax in
  let key = to_value_key (key_contents key) in
  let+ opt = T.find_tree tree key in
  Option.map (fun subtree -> T.hash subtree) opt

let hash_exn tree key =
  let open Lwt.Syntax in
  let+ opt = hash tree key in
  match opt with None -> raise Value_not_found | Some hash -> hash

let set_value_exn tree ?(edit_readonly = false) key str =
  if not edit_readonly then assert_key_writeable key ;
  let key = to_value_key @@ key_contents key in
  let encoding = E.scope key Chunked_byte_vector.encoding in
  Runner.encode
    encoding
    (Tezos_lazy_containers.Chunked_byte_vector.of_string str)
    tree

let write_value_exn tree ?(edit_readonly = false) key offset bytes =
  if not edit_readonly then assert_key_writeable key ;

  let open Lwt.Syntax in
  let open Tezos_lazy_containers in
  let num_bytes = Int64.of_int @@ String.length bytes in
  assert_max_bytes num_bytes ;

  let key = to_value_key @@ key_contents key in
  let* opt = T.find_tree tree key in
  let encoding = E.scope key Chunked_byte_vector.encoding in
  let* value =
    match opt with
    | None -> Lwt.return @@ Chunked_byte_vector.allocate 0L
    | Some _subtree -> Runner.decode encoding tree
  in
  let vec_len = Chunked_byte_vector.length value in
  if offset > vec_len then raise (Out_of_bounds (offset, vec_len)) ;
  let grow_by = Int64.(num_bytes |> add offset |> Fun.flip sub vec_len) in
  if Int64.compare grow_by 0L > 0 then Chunked_byte_vector.grow value grow_by ;
  let* () =
    Chunked_byte_vector.store_bytes value offset @@ Bytes.of_string bytes
  in
  Runner.encode encoding value tree

let read_value_exn tree key offset num_bytes =
  let open Lwt.Syntax in
  let open Tezos_lazy_containers in
  assert_max_bytes num_bytes ;

  let* value = find_value_exn tree key in
  let vec_len = Chunked_byte_vector.length value in

  if offset < 0L || offset >= vec_len then
    raise (Out_of_bounds (offset, vec_len)) ;

  let num_bytes =
    Int64.(num_bytes |> add offset |> min vec_len |> Fun.flip sub offset)
  in
  let+ bytes = Chunked_byte_vector.load_bytes value offset num_bytes in
  Bytes.to_string bytes

module Internal_for_tests = struct
  let key_is_readonly = function Readonly _ -> true | Writeable _ -> false

  let key_to_list = key_contents
end
