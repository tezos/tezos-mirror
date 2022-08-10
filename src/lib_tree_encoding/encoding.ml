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

type key = string list

exception No_tag_matched_on_encoding

module type S = sig
  type tree

  type -'a t

  val delayed : (unit -> 'a t) -> 'a t

  val contramap : ('a -> 'b) -> 'b t -> 'a t

  val contramap_lwt : ('a -> 'b Lwt.t) -> 'b t -> 'a t

  val ignore : 'a t

  val run : 'a t -> 'a -> tree -> tree Lwt.t

  val with_subtree :
    ('a -> Lazy_containers.Lazy_map.tree option) -> 'a t -> 'a t

  val raw : key -> bytes t

  val value_option : key -> 'a Data_encoding.t -> 'a option t

  val value : key -> 'a Data_encoding.t -> 'a t

  val scope : key -> 'a t -> 'a t

  val lazy_mapping : ('k -> key) -> 'v t -> ('k * 'v) list t

  type ('tag, 'a) case

  val case : 'tag -> 'b t -> ('a -> 'b option) -> ('tag, 'a) case

  val case_lwt : 'a -> 'b t -> ('c -> 'b Lwt.t option) -> ('a, 'c) case

  val tagged_union : 'tag t -> ('tag, 'a) case list -> 'a t

  val lwt : 'a t -> 'a Lwt.t t

  val tup2 : 'a t -> 'b t -> ('a * 'b) t

  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
end

module Make (T : Tree.S) = struct
  (** [append_key prefix key] append [key] to [prefix] in order to create a new
      [prefix_key]. *)
  let append_key prefix key tail = prefix (List.append key tail)

  type tree = T.tree

  (** Given the tail key, construct a full key. *)
  type prefix_key = key -> key

  type -'a t = 'a -> prefix_key -> tree -> tree Lwt.t

  let ignore _val _key tree = Lwt.return tree

  let run enc value tree = enc value Fun.id tree

  let with_subtree get_subtree enc value prefix input_tree =
    let open Lwt.Syntax in
    match get_subtree value with
    | Some tree ->
        let* input_tree = T.remove input_tree (prefix []) in
        let* input_tree = T.add_tree input_tree (prefix []) (T.select tree) in
        enc value prefix input_tree
    | None -> enc value prefix input_tree

  let lwt enc value prefix tree =
    let open Lwt_syntax in
    let* v = value in
    enc v prefix tree

  let delayed f x key tree = f () x key tree

  let contramap f enc value = enc (f value)

  let contramap_lwt f enc value prefix tree =
    let open Lwt_syntax in
    let* v = f value in
    enc v prefix tree

  let tup2 lhs rhs (l, r) prefix tree =
    let open Lwt.Syntax in
    let* tree = lhs l prefix tree in
    rhs r prefix tree

  let tup3 encode_a encode_b encode_c (a, b, c) prefix tree =
    let open Lwt.Syntax in
    let* tree = encode_a a prefix tree in
    let* tree = encode_b b prefix tree in
    encode_c c prefix tree

  let raw suffix bytes prefix tree = T.add tree (prefix suffix) bytes

  let value suffix enc v prefix tree =
    contramap (Data_encoding.Binary.to_bytes_exn enc) (raw suffix) v prefix tree

  let value_option key encoding v prefix tree =
    match v with
    | Some v -> value key encoding v prefix tree
    | None -> T.remove tree (prefix key)

  let scope key enc value prefix tree = enc value (append_key prefix key) tree

  let lazy_mapping to_key enc_value bindings prefix tree =
    List.fold_left_s
      (fun tree (k, v) ->
        let key = append_key prefix (to_key k) in
        enc_value v key tree)
      tree
      bindings

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

  let tagged_union encode_tag cases value prefix target_tree =
    let open Lwt_syntax in
    let encode_tag = scope ["tag"] encode_tag in
    let match_case (Case {probe; tag; encode}) =
      match probe value with
      | Some res ->
          let* target_tree = encode_tag tag prefix target_tree in
          let* value = res in
          let* x = scope ["value"] encode value prefix target_tree in
          return (Some x)
      | None -> return None
    in
    let* tree_opt = List.find_map_s match_case cases in
    match tree_opt with
    | None -> raise No_tag_matched_on_encoding
    | Some tree -> return tree
end
