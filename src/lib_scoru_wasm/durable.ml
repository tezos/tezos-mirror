(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

module T = Tezos_tree_encoding.Wrapped
module Runner = Tezos_tree_encoding.Runner.Make (Tezos_tree_encoding.Wrapped)
module E = Tezos_tree_encoding
module Storage = Tezos_webassembly_interpreter.Durable_storage

type t = T.tree

exception Invalid_key of string

exception Index_too_large of int

exception Not_found

exception Durable_empty = Storage.Durable_empty

exception Out_of_bounds of (int64 * int64)

let encoding = E.wrapped_tree

let of_storage ~default s =
  match Storage.to_tree s with Some t -> T.select t | None -> default

let of_storage_exn s = T.select @@ Storage.to_tree_exn s

let to_storage d = Storage.of_tree @@ T.wrap d

type key = string list

(* A key is bounded to 250 bytes, including the implicit '/durable' prefix.
   Additionally, values are implicitly appended with '_'. **)
let max_key_length = 250 - String.length "/durable" - String.length "/_"

let key_of_string_exn s =
  if String.length s > max_key_length then raise (Invalid_key s) ;
  let key =
    match String.split '/' s with
    | "" :: tl -> tl (* Must start with '/' *)
    | _ -> raise (Invalid_key s)
  in
  let assert_valid_char = function
    | '.' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> ()
    | _ -> raise (Invalid_key s)
  in
  let all_steps_valid =
    List.for_all (fun x ->
        x <> ""
        &&
        (String.iter assert_valid_char x ;
         true))
  in
  if all_steps_valid key then key else raise (Invalid_key s)

let key_of_string_opt s =
  try Some (key_of_string_exn s) with Invalid_key _ -> None

(** We append all values with '_', which is an invalid key-character w.r.t.
    external use.

    This ensures that an external user is prevented from accidentally writing a
    value to a place which is part of another value (e.g. writing a
    chunked_byte_vector to "/a/length", where "/a/length" previously existed as
    part of another chunked_byte_vector encoding.)
*)
let to_value_key k = List.append k ["_"]

let find_value tree key =
  let open Lwt.Syntax in
  let* opt = T.find_tree tree @@ to_value_key key in
  match opt with
  | None -> Lwt.return_none
  | Some subtree ->
      let+ value = Runner.decode E.chunked_byte_vector subtree in
      Some value

let find_value_exn tree key =
  let open Lwt.Syntax in
  let* opt = T.find_tree tree @@ to_value_key key in
  match opt with
  | None -> raise Not_found
  | Some subtree -> Runner.decode E.chunked_byte_vector subtree

(** helper function used in the copy/move *)
let find_tree_exn tree key =
  let open Lwt.Syntax in
  let* opt = T.find_tree tree key in
  match opt with None -> raise Not_found | Some subtree -> Lwt.return subtree

let copy_tree_exn tree from_key to_key =
  let open Lwt.Syntax in
  let* move_tree = find_tree_exn tree from_key in
  T.add_tree tree to_key move_tree

let count_subtrees tree key = T.length tree key

let delete tree key = T.remove tree key

let subtree_name_at tree key index =
  let open Lwt.Syntax in
  let* subtree = find_tree_exn tree key in
  let* list = T.list ~offset:index ~length:1 subtree [] in
  let nth = List.nth list 0 in
  match nth with
  | Some ("_", _) -> Lwt.return ""
  | Some (step, _) -> Lwt.return step
  | None -> raise (Index_too_large index)

let move_tree_exn tree from_key to_key =
  let open Lwt.Syntax in
  let* tree = copy_tree_exn tree from_key to_key in
  delete tree from_key

let hash_exn tree key =
  let open Lwt.Syntax in
  let+ opt = T.find_tree tree (to_value_key key) in
  match opt with None -> raise Not_found | Some subtree -> T.hash subtree

(* The maximum size of bytes allowed to be read/written at once. *)
let max_store_io_size = 4096L

let write_value_exn tree key offset bytes =
  let open Lwt.Syntax in
  let open Tezos_lazy_containers in
  let num_bytes = Int64.of_int @@ String.length bytes in
  assert (num_bytes <= max_store_io_size) ;

  let key = to_value_key key in
  let* opt = T.find_tree tree key in
  let encoding = E.scope key E.chunked_byte_vector in
  let* value =
    match opt with
    | None -> Lwt.return @@ Chunked_byte_vector.allocate num_bytes
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
  assert (num_bytes <= max_store_io_size) ;

  let* value = find_value_exn tree key in
  let vec_len = Chunked_byte_vector.length value in

  if offset < 0L || offset >= vec_len then
    raise (Out_of_bounds (offset, vec_len)) ;

  let num_bytes =
    Int64.(num_bytes |> add offset |> min vec_len |> Fun.flip sub offset)
  in
  let+ bytes = Chunked_byte_vector.load_bytes value offset num_bytes in
  Bytes.to_string bytes
