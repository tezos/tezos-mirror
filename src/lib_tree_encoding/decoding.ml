(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

exception Key_not_found of key

exception No_tag_matched_on_decoding

exception Decode_error of {key : key; error : Data_encoding.Binary.read_error}

(** Given the tail key, construct a full key. *)
type prefix_key = key -> key

(** [of_key key] constructs a [prefix_key] where [key] is the prefix. *)
let of_key key tail =
  let rec go = function [] -> tail | x :: xs -> x :: go xs in
  go key

(** [append_key prefix key] append [key] to [prefix] in order to create a new
      [prefix_key]. *)
let append_key prefix key tail = prefix (of_key key tail)

type 'a t = {
  decode : 'tree. 'tree Tree.backend -> 'tree -> prefix_key -> 'a Lwt.t;
}
[@@unboxed]

type ('tag, 'a) case =
  | Case : {
      tag : 'tag;
      extract : 'b -> 'a Lwt.t;
      decode : 'b t;
    }
      -> ('tag, 'a) case

let delayed f = {decode = (fun backend -> (f ()).decode backend)}

let of_lwt lwt = {decode = (fun _backend _tree _prefix -> lwt)}

let map f {decode} =
  {decode = (fun backend tree prefix -> Lwt.map f (decode backend tree prefix))}

let map_lwt f {decode} =
  {
    decode = (fun backend tree prefix -> Lwt.bind (decode backend tree prefix) f);
  }

module Syntax = struct
  let return value = {decode = (fun _backend _tree _prefix -> Lwt.return value)}

  let bind {decode} f =
    {
      decode =
        (fun backend tree prefix ->
          Lwt.bind (decode backend tree prefix) (fun x ->
              (f x).decode backend tree prefix));
    }

  let both lhs rhs =
    {
      decode =
        (fun backend tree prefix ->
          Lwt.both
            (lhs.decode backend tree prefix)
            (rhs.decode backend tree prefix));
    }

  let ( let+ ) m f = map f m

  let ( and+ ) = both

  let ( let* ) = bind

  let ( and* ) = ( and+ )
end

let run backend {decode} tree = decode backend tree Fun.id

let raw key =
  {
    decode =
      (fun backend tree prefix ->
        let open Lwt_syntax in
        let key = prefix key in
        let+ value = Tree.find backend tree key in
        match value with
        | Some value -> value
        | None -> raise (Key_not_found key));
  }

let value_option key decoder =
  {
    decode =
      (fun backend tree prefix ->
        let open Lwt_syntax in
        let key = prefix key in
        let* value = Tree.find backend tree key in
        match value with
        | Some value -> (
            match Data_encoding.Binary.of_bytes decoder value with
            | Ok value -> return_some value
            | Error error -> raise (Decode_error {key; error}))
        | None -> return_none);
  }

let value ?default key decoder =
  {
    decode =
      (fun backend tree prefix ->
        let open Lwt_syntax in
        let* value = (value_option key decoder).decode backend tree prefix in
        match (value, default) with
        | Some value, _ -> return value
        | None, Some default -> return default
        | None, None -> raise (Key_not_found (prefix key)));
  }

let subtree backend tree prefix =
  let open Lwt_syntax in
  let tmp_directory = "tmp" in
  let* subtree = Tree.find_tree backend tree (prefix []) in
  let+ subtree =
    match subtree with
    | Some subtree -> return subtree
    | None -> (
        let* tree =
          Tree.add backend tree (prefix [tmp_directory]) (Bytes.of_string "")
        in
        let* subtree = Tree.find_tree backend tree (prefix []) in
        match subtree with
        | Some subtree -> Tree.remove backend subtree [tmp_directory]
        | None ->
            (* This case is impossible, because we have added something
               in the tree to avoid it. *)
            assert false)
  in
  subtree

let scope key {decode} =
  {
    decode =
      (fun backend tree prefix -> decode backend tree (append_key prefix key));
  }

let lazy_mapping to_key field_enc =
  {
    decode =
      (fun backend input_tree input_prefix ->
        let open Lwt_syntax in
        let produce_value index =
          (scope (to_key index) field_enc).decode
            backend
            input_tree
            input_prefix
        in
        let+ tree = subtree backend input_tree input_prefix in
        (Some (Tree.Wrapped_tree (tree, backend)), produce_value));
  }

let case_lwt tag decode extract = Case {tag; decode; extract}

let case tag decode extract =
  case_lwt tag decode (fun x -> Lwt.return @@ extract x)

let tagged_union ?default decode_tag cases =
  {
    decode =
      (fun backend input_tree prefix ->
        let open Lwt_syntax in
        Lwt.try_bind
          (fun () ->
            (scope ["tag"] decode_tag).decode backend input_tree prefix)
          (fun target_tag ->
            (* Search through the cases to find a matching branch. *)
            let candidate =
              List.find_map
                (fun (Case {tag; decode; extract}) ->
                  if tag = target_tag then
                    Some
                      ((map_lwt extract (scope ["value"] decode)).decode
                         backend
                         input_tree
                         prefix)
                  else None)
                cases
            in
            match candidate with
            | Some case -> case
            | None -> raise No_tag_matched_on_decoding)
          (function
            | Key_not_found _ as exn -> (
                match default with
                | Some default -> return (default ())
                | None -> Lwt.reraise exn)
            | exn -> Lwt.reraise exn));
  }

let wrapped_tree =
  {
    decode =
      (fun backend tree prefix ->
        let open Lwt.Syntax in
        let+ tree = subtree backend tree prefix in
        Tree.Wrapped_tree (tree, backend));
  }
