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

exception Key_not_found of key

exception Decode_error of {key : key; error : Data_encoding.Binary.read_error}

module type S = sig
  type tree

  type 'a t

  val run : 'a t -> tree -> 'a Lwt.t

  val raw : key -> bytes t

  val value : key -> 'a Data_encoding.t -> 'a t

  val tree : key -> 'a t -> 'a t

  val lazy_mapping : ('i -> key) -> 'a t -> ('i -> 'a Lwt.t) t

  val return : 'a -> 'a t

  val of_lwt : 'a Lwt.t -> 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module Make (T : Tree.S) : S with type tree = T.tree = struct
  (** Given the tail key, construct a full key. *)
  type prefix_key = key -> key

  (** [of_key key] constructs a [prefix_key] where [key] is the prefix. *)
  let of_key key tail =
    let rec go = function [] -> tail | x :: xs -> x :: go xs in
    go key

  (** [append_key prefix key] append [key] to [prefix] in order to create a new
      [prefix_key]. *)
  let append_key prefix key tail = prefix (of_key key tail)

  module Tree = T

  type tree = T.tree

  type 'a t = Tree.tree -> prefix_key -> 'a Lwt.t

  let return value _tree _prefix = Lwt.return value

  let of_lwt lwt _tree _prefix = lwt

  let ( let+ ) dec f tree prefix = Lwt.map f (dec tree prefix)

  let ( and+ ) lhs rhs tree prefix =
    Lwt.Syntax.( and+ ) (lhs tree prefix) (rhs tree prefix)

  let ( let* ) dec f tree prefix =
    Lwt.bind (dec tree prefix) (fun x -> f x tree prefix)

  let ( and* ) = ( and+ )

  let run dec tree = dec tree Fun.id

  let raw key tree prefix =
    let open Lwt.Syntax in
    let key = prefix key in
    let+ value = Tree.find tree key in
    match value with Some value -> value | None -> raise (Key_not_found key)

  let value key decoder tree prefix =
    let open Lwt.Syntax in
    let key = prefix key in
    let* value = Tree.find tree key in
    match value with
    | Some value -> (
        match Data_encoding.Binary.of_bytes decoder value with
        | Ok value -> Lwt.return value
        | Error error -> raise (Decode_error {key; error}))
    | None -> raise (Key_not_found key)

  let tree key dec tree prefix = dec tree (append_key prefix key)

  let lazy_mapping to_key field_enc input_tree input_prefix =
    let produce_value index =
      tree (to_key index) field_enc input_tree input_prefix
    in
    Lwt.return produce_value
end
