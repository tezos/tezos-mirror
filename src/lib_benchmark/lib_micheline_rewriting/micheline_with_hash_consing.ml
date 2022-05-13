(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Custom_weak

(* ------------------------------------------------------------------------- *)

(* Hash-consing Micheline. *)

type hcons_info = {tag : int; hash : int}

module Make
    (X : Signature.S) (P : sig
      val initial_size : int option
    end) : Micheline_sig.S with type label = hcons_info and type head = X.t =
struct
  type label = hcons_info

  type head = X.t

  type node = (hcons_info, head) Micheline.node

  let default_label = {tag = -1; hash = -1}

  let get_info = Micheline.location

  module Table = Make_table (struct
    type t = node

    let equal (n1 : t) (n2 : t) =
      let l1 = Micheline.location n1 in
      let l2 = Micheline.location n2 in
      Int.equal l1.tag l2.tag

    let hash (n : t) =
      let l = Micheline.location n in
      l.hash
  end)

  let table =
    match P.initial_size with
    | None -> Table.create 101
    | Some size -> Table.create size

  let new_tag =
    let x = ref ~-1 in
    fun () ->
      incr x ;
      !x

  let alloc_int (hash : int) (z : Z.t) =
    let info = {tag = new_tag (); hash} in
    let res = Micheline.Int (info, z) in
    Table.add table res ;
    res

  let alloc_string (hash : int) (s : string) =
    let info = {tag = new_tag (); hash} in
    let res = Micheline.String (info, s) in
    Table.add table res ;
    res

  let alloc_bytes (hash : int) (b : Bytes.t) =
    let info = {tag = new_tag (); hash} in
    let res = Micheline.Bytes (info, b) in
    Table.add table res ;
    res

  let alloc_prim (hash : int) (prim : head) (subterms : node list)
      (annots : string list) =
    let info = {tag = new_tag (); hash} in
    let res = Micheline.Prim (info, prim, subterms, annots) in
    Table.add table res ;
    res

  let alloc_seq (hash : int) (subterms : node list) =
    let info = {tag = new_tag (); hash} in
    let res = Micheline.Seq (info, subterms) in
    Table.add table res ;
    res

  let int (z : Z.t) =
    let hash = Z.hash z in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_int hash z
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | Micheline.Int (_, z') -> Compare.Z.equal z z' | _ -> false)
            bucket
        in
        match exists with Some res -> res | None -> alloc_int hash z)

  let string (s : string) =
    let hash = Hashtbl.hash s in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_string hash s
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | Micheline.String (_, s') -> Compare.String.equal s s'
              | _ -> false)
            bucket
        in
        match exists with Some res -> res | None -> alloc_string hash s)

  let bytes (b : Bytes.t) =
    let hash = Hashtbl.hash b in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_bytes hash b
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | Micheline.Bytes (_, b') -> Bytes.equal b b' | _ -> false)
            bucket
        in
        match exists with Some res -> res | None -> alloc_bytes hash b)

  let hash_empty_list = Hashtbl.hash []

  let hash_node_list (l : node list) : int =
    List.fold_left
      (fun h elt -> Hashtbl.hash (h, (get_info elt).hash))
      hash_empty_list
      l

  let hash_string_list (l : string list) : int =
    List.fold_left
      (fun h elt -> Hashtbl.hash (h, Hashtbl.hash elt))
      hash_empty_list
      l

  let terms_equal (x : node) (y : node) = (get_info x).tag = (get_info y).tag

  let rec term_lists_equal (lx : node list) (ly : node list) =
    match (lx, ly) with
    | [], _ :: _ | _ :: _, [] -> false
    | [], [] -> true
    | hx :: tlx, hy :: tly -> terms_equal hx hy && term_lists_equal tlx tly

  let rec string_lists_equal (lx : string list) (ly : string list) =
    match (lx, ly) with
    | [], _ :: _ | _ :: _, [] -> false
    | [], [] -> true
    | hx :: tlx, hy :: tly ->
        Compare.String.equal hx hy && string_lists_equal tlx tly

  let prim (head : head) (subterms : node list) (annots : string list) =
    let subterms_hash = hash_node_list subterms in
    let annots_hash = hash_string_list annots in
    let head_hash = X.hash head in
    let hash = Hashtbl.hash (head_hash, subterms_hash, annots_hash) in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_prim hash head subterms annots
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | Micheline.Prim (_, head', subterms', annots') ->
                  X.compare head head' = 0
                  && term_lists_equal subterms subterms'
                  && string_lists_equal annots annots'
              | _ -> false)
            bucket
        in
        match exists with
        | Some res -> res
        | None -> alloc_prim hash head subterms annots)

  let seq (subterms : node list) =
    let subterms_hash = hash_node_list subterms in
    let hash = subterms_hash in
    match Table.find_all_by_hash table hash with
    | [] -> alloc_seq hash subterms
    | bucket -> (
        let exists =
          List.find_opt
            (function
              | Micheline.Seq (_, subterms') ->
                  term_lists_equal subterms subterms'
              | _ -> false)
            bucket
        in
        match exists with Some res -> res | None -> alloc_seq hash subterms)

  let label = Micheline.location
end
