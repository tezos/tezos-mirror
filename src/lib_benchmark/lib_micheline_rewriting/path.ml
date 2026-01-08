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

(* Paths in terms. *)

module type S = sig
  type desc = private Root | At_index of int * t

  and t = private {tag : int; hash : int; rev_path_desc : desc}

  val compare : t -> t -> int

  val root : t

  val at_index : int -> t -> t

  val concat : above:t -> under:t -> t

  val to_string : t -> string
end

module Without_hash_consing : S = struct
  type desc = Root | At_index of int * t

  and t = {tag : int; hash : int; rev_path_desc : desc}

  let root = {tag = 0; hash = 0; rev_path_desc = Root}

  let at_index i path = {tag = 0; hash = 0; rev_path_desc = At_index (i, path)}

  let rec concat ~above:path1 ~under:path2 =
    match path2.rev_path_desc with
    | Root -> path1
    | At_index (i, p) -> at_index i (concat ~above:path1 ~under:p)

  let rec compare path1 path2 =
    match (path1.rev_path_desc, path2.rev_path_desc) with
    | Root, Root -> 0
    | Root, _ -> -1
    | _, Root -> 1
    | At_index (i1, p1), At_index (i2, p2) ->
        let c = Compare.Int.compare i1 i2 in
        if c = 0 then compare p1 p2 else c

  let rec to_string (x : t) =
    match x.rev_path_desc with
    | Root -> "*"
    | At_index (i, up) -> Printf.sprintf "%d -> %s" i (to_string up)
end

module With_hash_consing (P : sig
  val initial_size : int option
end) : S = struct
  type desc = Root | At_index of int * t

  and t = {tag : int; hash : int; rev_path_desc : desc}

  let table =
    match P.initial_size with
    | None -> Hashtbl.create 101
    | Some size -> Hashtbl.create size

  let new_tag =
    let x = ref ~-1 in
    fun () ->
      incr x ;
      !x

  let root =
    let hash = Hashtbl.hash Root in
    let tag = new_tag () in
    let res = {tag; hash; rev_path_desc = Root} in
    Hashtbl.add table hash res ;
    res

  let add_new_index hash i path =
    let res = {tag = new_tag (); hash; rev_path_desc = At_index (i, path)} in
    Hashtbl.add table hash res ;
    res

  let at_index i path =
    let hash = Hashtbl.hash (i, path.tag) in
    match Hashtbl.find_all table hash with
    | [] -> add_new_index hash i path
    | bucket -> (
        let exists =
          List.find_opt
            (fun {rev_path_desc; _} ->
              match rev_path_desc with
              | At_index (i', path') -> i' = i && path'.tag = path.tag
              | _ -> false)
            bucket
        in
        match exists with Some res -> res | None -> add_new_index hash i path)

  let rec concat ~above:path1 ~under:path2 =
    match path2.rev_path_desc with
    | Root -> path1
    | At_index (i, p) -> at_index i (concat ~above:path1 ~under:p)

  let compare path1 path2 = Compare.Int.compare path1.tag path2.tag

  let rec to_string (x : t) =
    match x.rev_path_desc with
    | Root -> "*"
    | At_index (i, up) -> Printf.sprintf "%d -> %s" i (to_string up)
end
