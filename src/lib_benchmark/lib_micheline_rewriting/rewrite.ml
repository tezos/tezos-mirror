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

(* Rewriting Micheline. *)

module type S = sig
  type label

  type head

  type path

  type patt

  type node = (label, head) Micheline.node

  exception Rewrite_error of string * node option

  val get_subterm : term:node -> path:path -> node

  val subst : term:node -> path:path -> replacement:node -> node

  val pattern_matches : patt -> node -> bool

  val all_matches : patt -> node -> path list
end

module Make
    (X : Algebraic_signature.S)
    (M : Micheline_sig.S with type head = X.t)
    (Path : Path.S)
    (Patt :
      Pattern.S
        with type head = X.t
         and type path = Path.t
         and type node = M.node) :
  S
    with type label = M.label
     and type head = X.t
     and type path = Path.t
     and type patt = Patt.t = struct
  type label = M.label

  type head = X.t

  type path = Path.t

  type patt = Patt.t

  type node = M.node

  type forward_path = int list

  exception Rewrite_error of string * node option

  let string_of_forward_path (p : int list) =
    List.fold_left (fun acc i -> Printf.sprintf "%s :: %d" acc i) "" p

  let rec reverse : Path.t -> forward_path -> forward_path =
   fun path acc ->
    let desc = path.Path.rev_path_desc in
    match desc with
    | Path.Root -> acc
    | Path.At_index (i, path) -> reverse path (i :: acc)

  let reverse path = reverse path []

  let rec get_subterm_aux : term:node -> path:forward_path -> node =
   fun ~term ~path ->
    match path with
    | [] -> term
    | index :: l -> (
        match term with
        | Int _ | String _ | Bytes _ ->
            let msg =
              Printf.sprintf
                "get_subterm_aux: non-empty path (%s)"
                (string_of_forward_path path)
            in
            raise (Rewrite_error (msg, Some term))
        | Prim (_, _, subterms, _) | Seq (_, subterms) ->
            get_subterm_at subterms index l)

  and get_subterm_at : node list -> int -> forward_path -> node =
   fun subterms index path ->
    match (subterms, index) with
    | [], _ ->
        let msg =
          Printf.sprintf
            "get_subterm_at: non-empty path (%s)"
            (string_of_forward_path path)
        in
        raise (Rewrite_error (msg, None))
    | hd :: _, 0 -> get_subterm_aux ~term:hd ~path
    | _ :: tl, _ -> get_subterm_at tl (index - 1) path

  let get_subterm : term:node -> path:path -> node =
   fun ~term ~path ->
    let path = reverse path in
    get_subterm_aux ~term ~path

  let rec subst_aux : term:node -> path:forward_path -> replacement:node -> node
      =
   fun ~term ~path ~replacement ->
    match path with
    | [] -> replacement
    | index :: l -> (
        match term with
        | Int _ | String _ | Bytes _ ->
            let msg = "subst_aux: non-empty path" in
            raise (Rewrite_error (msg, Some term))
        | Prim (_, head, subterms, annot) ->
            M.prim head (subst_at subterms index l replacement) annot
        | Seq (_, subterms) -> M.seq (subst_at subterms index l replacement))

  and subst_at : node list -> int -> forward_path -> node -> node list =
   fun subterms index path replacement ->
    match (subterms, index) with
    | [], _ ->
        let msg = Printf.sprintf "subst_at: empty list (%d)" index in
        raise (Rewrite_error (msg, None))
    | hd :: tl, 0 -> subst_aux ~term:hd ~path ~replacement :: tl
    | hd :: tl, _ -> hd :: subst_at tl (index - 1) path replacement

  let subst :
      term:('l, head) Micheline.node -> path:Path.t -> replacement:node -> node
      =
   fun ~term ~path ~replacement ->
    let path = reverse path in
    subst_aux ~term ~path ~replacement

  let pattern_matches patt (n : node) = Patt.pattern_matches patt n

  let all_matches (patt : patt) (node : node) = Patt.all_matches patt node
end
