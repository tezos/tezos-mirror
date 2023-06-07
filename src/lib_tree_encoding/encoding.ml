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

exception No_tag_matched_on_encoding

(** [append_key prefix key] append [key] to [prefix] in order to create a new
      [prefix_key]. *)
let append_key prefix key tail = prefix (List.append key tail)

(** Given the tail key, construct a full key. *)
type prefix_key = key -> key

type -'a t = {
  encode : 'tree. 'tree Tree.backend -> 'a -> prefix_key -> 'tree -> 'tree Lwt.t;
}
[@@unboxed]

let ignore = {encode = (fun _backend _val _key tree -> Lwt.return tree)}

let run backend {encode} value tree = encode backend value Fun.id tree

let lwt {encode} =
  {
    encode =
      (fun backend value prefix tree ->
        let open Lwt_syntax in
        let* v = value in
        encode backend v prefix tree);
  }

let delayed f =
  {encode = (fun backend x key tree -> (f ()).encode backend x key tree)}

let contramap f {encode} =
  {encode = (fun backend value -> encode backend (f value))}

let contramap_lwt f {encode} =
  {
    encode =
      (fun backend value prefix tree ->
        let open Lwt_syntax in
        let* v = f value in
        encode backend v prefix tree);
  }

let tup2 lhs rhs =
  {
    encode =
      (fun backend (l, r) prefix tree ->
        let open Lwt.Syntax in
        let* tree = lhs.encode backend l prefix tree in
        rhs.encode backend r prefix tree);
  }

let tup3 encode_a encode_b encode_c =
  {
    encode =
      (fun backend (a, b, c) prefix tree ->
        let open Lwt.Syntax in
        let* tree = encode_a.encode backend a prefix tree in
        let* tree = encode_b.encode backend b prefix tree in
        encode_c.encode backend c prefix tree);
  }

let raw suffix =
  {
    encode =
      (fun backend bytes prefix tree ->
        Tree.add backend tree (prefix suffix) bytes);
  }

let value suffix enc =
  {
    encode =
      (fun backend v prefix tree ->
        (contramap (Data_encoding.Binary.to_bytes_exn enc) (raw suffix)).encode
          backend
          v
          prefix
          tree);
  }

let value_option key encoding =
  {
    encode =
      (fun backend v prefix tree ->
        match v with
        | Some v -> (value key encoding).encode backend v prefix tree
        | None -> Tree.remove backend tree (prefix key));
  }

let scope key {encode} =
  {
    encode =
      (fun backend value prefix tree ->
        encode backend value (append_key prefix key) tree);
  }

let lazy_mapping to_key enc_value =
  {
    encode =
      (fun backend (origin_opt, bindings) prefix tree ->
        let open Lwt_syntax in
        let* tree =
          match origin_opt with
          | Some (Tree.Wrapped_tree (origin, origin_backend)) ->
              Tree.add_tree
                backend
                tree
                (prefix [])
                (Tree.select backend @@ Tree.wrap origin_backend origin)
              (* Will fetch a tree of the same type as backend or throw an error.
                 Basically checking that origin's backend and encoding backeds are the same *)
          | None -> Tree.remove backend tree (prefix [])
        in
        List.fold_left_s
          (fun tree (k, v) ->
            let key = append_key prefix (to_key k) in
            let* tree = Tree.remove backend tree (key []) in
            match v with
            | Some v -> enc_value.encode backend v key tree
            | None -> Lwt.return tree)
          tree
          bindings);
  }

type ('tag, 'a) case =
  | Case : {
      tag : 'tag;
      probe : 'a -> 'b Lwt.t option;
      encode : 'b t;
    }
      -> ('tag, 'a) case

let case_lwt tag encode probe = Case {tag; encode; probe}

let case tag encode probe =
  let probe x = Option.map Lwt.return @@ probe x in
  case_lwt tag encode probe

let tagged_union encode_tag cases =
  {
    encode =
      (fun backend value prefix target_tree ->
        let open Lwt_syntax in
        let encode_tag = scope ["tag"] encode_tag in
        let match_case (Case {probe; tag; encode}) =
          match probe value with
          | Some res ->
              let* target_tree =
                encode_tag.encode backend tag prefix target_tree
              in
              let* value = res in
              let* x =
                (scope ["value"] encode).encode backend value prefix target_tree
              in
              return (Some x)
          | None -> return None
        in
        let* tree_opt = List.find_map_s match_case cases in
        match tree_opt with
        | None -> raise No_tag_matched_on_encoding
        | Some tree -> return tree);
  }

let wrapped_tree =
  {
    encode =
      (fun backend (Tree.Wrapped_tree (subtree, backend')) prefix target_tree ->
        let subtree = Tree.select backend (Tree.wrap backend' subtree) in
        let key = prefix [] in
        Tree.add_tree backend target_tree key subtree);
  }
