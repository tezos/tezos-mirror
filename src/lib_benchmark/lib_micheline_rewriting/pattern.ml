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

(* Pattern-matching Micheline. *)

exception Invalid_pattern

type focused = Focused

let _focused = Focused

type unfocused = Unfocused

let _unfocused = Unfocused

type 'f focus_tag =
  | Focused_tag : focused focus_tag
  | Unfocused_tag : unfocused focus_tag

type ('f1, 'f2, 'f) join =
  | UU : (unfocused, unfocused, unfocused) join
  | FU : (focused, unfocused, focused) join
  | UF : (unfocused, focused, focused) join
  | FF : (focused, focused, focused) join

type (_, _) pattern_desc =
  | Patt_int : (Z.t -> bool) option -> ('p, unfocused) pattern_desc
  | Patt_string : (string -> bool) option -> ('p, unfocused) pattern_desc
  | Patt_bytes : (Bytes.t -> bool) option -> ('p, unfocused) pattern_desc
  | Patt_prim : 'p head_pred * ('p, 'f) pattern_list -> ('p, 'f) pattern_desc
  | Patt_seq : ('p, 'f) pattern_list -> ('p, 'f) pattern_desc
  | Patt_any : ('p, unfocused) pattern_desc
  | Patt_focus : ('p, unfocused) pattern -> ('p, focused) pattern_desc

and ('p, 'f) pattern = {patt_desc : ('p, 'f) pattern_desc; patt_uid : int}

and (_, _) pattern_list =
  | Patt_list_empty : ('p, unfocused) pattern_list
  | Patt_list_any : ('p, unfocused) pattern_list
  | Patt_list_cons :
      ('p, 'f1) pattern * ('p, 'f2) pattern_list * ('f1, 'f2, 'f) join
      -> ('p, 'f) pattern_list

and 'p head_pred = Patt_head_equal of 'p | Patt_pred of ('p -> bool)

(* type 'p focused_pattern = ('p, focused) pattern *)

(* type 'p unfocused_pattern = ('p, unfocused) pattern *)

let rec get_focus : type f. ('p, f) pattern -> f focus_tag =
 fun {patt_desc; _} ->
  match patt_desc with
  | Patt_int _ -> Unfocused_tag
  | Patt_string _ -> Unfocused_tag
  | Patt_bytes _ -> Unfocused_tag
  | Patt_prim (_head, subpatts) -> get_focus_list subpatts
  | Patt_seq subpatts -> get_focus_list subpatts
  | Patt_any -> Unfocused_tag
  | Patt_focus _ -> Focused_tag

and get_focus_list : type f. ('p, f) pattern_list -> f focus_tag =
 fun patt_list ->
  match patt_list with
  | Patt_list_empty -> Unfocused_tag
  | Patt_list_any -> Unfocused_tag
  | Patt_list_cons (_, _, wit) -> (
      match wit with
      | UU -> Unfocused_tag
      | FU -> Focused_tag
      | UF -> Focused_tag
      | FF -> Focused_tag)

module type S = sig
  type head

  type path

  type t

  type plist

  type node

  val pattern_matches : t -> node -> bool

  val all_matches : t -> node -> path list

  val focus_matches : t -> path list -> path list

  val int : (Z.t -> bool) option -> t

  val string : (string -> bool) option -> t

  val bytes : (Bytes.t -> bool) option -> t

  val prim : head -> plist -> t

  val prim_pred : (head -> bool) -> plist -> t

  val seq : plist -> t

  val any : t

  val focus : t -> t

  val list_any : plist

  val list_empty : plist

  val list_cons : t -> plist -> plist

  val ( @. ) : t -> plist -> plist

  val pp : Format.formatter -> t -> unit

  val uid : t -> int
end

module Make
    (X : Algebraic_signature.S)
    (Micheline : Micheline_sig.S with type head = X.t)
    (Path : Path.S) =
struct
  type head = X.t

  type path = Path.t

  type t = Ex_patt : (head, 'f) pattern -> t

  type plist = Ex_patt_list : (X.t, 'f) pattern_list -> plist

  type node = Micheline.node

  let rec get_paths_of_focuses :
      ('p, focused) pattern -> Path.t -> Path.t list -> Path.t list =
   fun {patt_desc; _} position acc ->
    match patt_desc with
    | Patt_prim (_, subterms) ->
        get_paths_of_focuses_list subterms position 0 acc
    | Patt_seq subterms -> get_paths_of_focuses_list subterms position 0 acc
    | Patt_focus _ -> position :: acc

  and get_paths_of_focuses_list :
      ('p, focused) pattern_list -> Path.t -> int -> Path.t list -> Path.t list
      =
   fun patt_list position index acc ->
    match patt_list with
    | Patt_list_cons (head, tail, wit) -> (
        match wit with
        | FU ->
            let head_position = Path.at_index index position in
            get_paths_of_focuses head head_position acc
        | UF -> get_paths_of_focuses_list tail position (index + 1) acc
        | FF ->
            let head_position = Path.at_index index position in
            let acc = get_paths_of_focuses head head_position acc in
            get_paths_of_focuses_list tail position (index + 1) acc)

  let rec pattern_matches_aux : type f. (X.t, f) pattern -> node -> bool =
   fun patt node ->
    match (patt.patt_desc, node) with
    | Patt_focus patt, _ -> pattern_matches_aux patt node
    | Patt_any, _ -> true
    | Patt_int None, Int (_, _z) -> true
    | Patt_int (Some zpred), Int (_, z) -> zpred z
    | Patt_string None, String (_, _s) -> true
    | Patt_string (Some spred), String (_, s) -> spred s
    | Patt_bytes None, Bytes (_, _b) -> true
    | Patt_bytes (Some bpred), Bytes (_, s) -> bpred s
    | Patt_prim (hpred, subpatts), Prim (_, head, subterms, _) -> (
        match hpred with
        | Patt_head_equal h ->
            if X.compare h head = 0 then list_matches subpatts subterms
            else false
        | Patt_pred pred ->
            if pred head then list_matches subpatts subterms else false)
    | Patt_seq subpatts, Seq (_, subterms) -> list_matches subpatts subterms
    | _ -> false

  and list_matches : type f. (X.t, f) pattern_list -> node list -> bool =
   fun patts nodes ->
    match (patts, nodes) with
    | Patt_list_any, _ -> true
    | Patt_list_empty, [] -> true
    | Patt_list_empty, _ :: _ -> false
    | Patt_list_cons (_, _, _), [] -> false
    | Patt_list_cons (p, lpatt, _), n :: lnodes ->
        pattern_matches_aux p n && list_matches lpatt lnodes

  let pattern_matches (patt : t) (node : node) =
    match patt with Ex_patt patt -> pattern_matches_aux patt node

  let rec all_matches_aux : t -> node -> Path.t -> Path.t list -> Path.t list =
   fun patt node position acc ->
    match node with
    | Int _ | String _ | Bytes _ ->
        if pattern_matches patt node then position :: acc else acc
    | Prim (_, _, subterms, _) | Seq (_, subterms) ->
        let _, acc =
          List.fold_left
            (fun (index, acc) subterm ->
              let position = Path.at_index index position in
              (index + 1, all_matches_aux patt subterm position acc))
            (0, acc)
            subterms
        in
        if pattern_matches patt node then position :: acc else acc

  let focus_matches pattern paths =
    match pattern with
    | Ex_patt patt -> (
        match get_focus patt with
        | Unfocused_tag -> []
        | Focused_tag ->
            List.fold_left
              (fun acc context_path ->
                get_paths_of_focuses patt context_path acc)
              []
              paths)

  let all_matches pattern node =
    match pattern with
    | Ex_patt patt -> (
        match get_focus patt with
        | Unfocused_tag -> all_matches_aux pattern node Path.root []
        | Focused_tag ->
            let paths = all_matches_aux pattern node Path.root [] in
            List.fold_left
              (fun acc context_path ->
                get_paths_of_focuses patt context_path acc)
              []
              paths)

  let uid_gen =
    let x = ref 0 in
    fun () ->
      let r = !x in
      incr x ;
      r

  let ex_patt patt_desc = Ex_patt {patt_desc; patt_uid = uid_gen ()}

  let int pred = ex_patt (Patt_int pred)

  let string pred = ex_patt (Patt_string pred)

  let bytes pred = ex_patt (Patt_bytes pred)

  let prim head patts =
    match patts with
    | Ex_patt_list patts -> ex_patt (Patt_prim (Patt_head_equal head, patts))

  let prim_pred head_pred patts =
    match patts with
    | Ex_patt_list patts -> ex_patt (Patt_prim (Patt_pred head_pred, patts))

  let seq patts =
    match patts with Ex_patt_list patts -> ex_patt (Patt_seq patts)

  let any = ex_patt Patt_any

  let focus patt =
    match patt with
    | Ex_patt patt -> (
        match get_focus patt with
        | Focused_tag -> raise Invalid_pattern
        | Unfocused_tag -> ex_patt (Patt_focus patt))

  let list_any = Ex_patt_list Patt_list_any

  let list_empty = Ex_patt_list Patt_list_empty

  let list_cons (Ex_patt patt) (Ex_patt_list patt_list) =
    match get_focus patt with
    | Focused_tag -> (
        match get_focus_list patt_list with
        | Focused_tag -> Ex_patt_list (Patt_list_cons (patt, patt_list, FF))
        | Unfocused_tag -> Ex_patt_list (Patt_list_cons (patt, patt_list, FU)))
    | Unfocused_tag -> (
        match get_focus_list patt_list with
        | Focused_tag -> Ex_patt_list (Patt_list_cons (patt, patt_list, UF))
        | Unfocused_tag -> Ex_patt_list (Patt_list_cons (patt, patt_list, UU)))

  let ( @. ) = list_cons

  let rec pp_patt : type f. Format.formatter -> (head, f) pattern -> unit =
   fun fmtr patt ->
    match patt.patt_desc with
    | Patt_int _ -> Format.pp_print_string fmtr "int"
    | Patt_string _ -> Format.pp_print_string fmtr "string"
    | Patt_bytes _ -> Format.pp_print_string fmtr "bytes"
    | Patt_prim (Patt_head_equal head, subpatts) ->
        Format.fprintf fmtr "[%a](%a)" X.pp head pp_patt_list subpatts
    | Patt_prim (Patt_pred _, subpatts) ->
        Format.fprintf fmtr "[opaque_pred](%a)" pp_patt_list subpatts
    | Patt_seq subpatts -> Format.fprintf fmtr "Seq(%a)" pp_patt_list subpatts
    | Patt_any -> Format.pp_print_string fmtr "_"
    | Patt_focus patt -> Format.fprintf fmtr "[> %a <]" pp_patt patt

  and pp_patt_list : type f. Format.formatter -> (head, f) pattern_list -> unit
      =
   fun fmtr patts ->
    match patts with
    | Patt_list_empty -> Format.pp_print_string fmtr "[]"
    | Patt_list_any -> Format.pp_print_string fmtr "_"
    | Patt_list_cons (hd, tail, _) ->
        Format.fprintf fmtr "%a :: %a" pp_patt hd pp_patt_list tail

  let pp (fmtr : Format.formatter) (patt : t) =
    match patt with Ex_patt patt -> pp_patt fmtr patt

  let uid (Ex_patt patt) = patt.patt_uid
end

module Make_with_hash_consing
    (X : Algebraic_signature.S)
    (Micheline :
      Micheline_sig.S
        with type head = X.t
         and type label = Micheline_with_hash_consing.hcons_info)
    (Path : Path.S) : sig
  include
    S with type head = X.t and type path = Path.t and type node = Micheline.node

  val all_matches_with_hash_consing : t -> node -> path list
end = struct
  include Make (X) (Micheline) (Path)

  type key = {patt_uid : int; node_tag : int}

  let table : (key, path list) Hashtbl.t = Hashtbl.create 7919

  let rec all_matches_aux : t -> node -> Path.t -> Path.t list -> Path.t list =
   fun patt node position acc ->
    let patt_uid = uid patt in
    let node_tag = (Micheline.label node).tag in
    match Hashtbl.find_opt table {patt_uid; node_tag} with
    | None -> (
        match node with
        | Int _ | String _ | Bytes _ ->
            if pattern_matches patt node then position :: acc else acc
        | Prim (_, _, subterms, _) | Seq (_, subterms) ->
            let _, acc =
              List.fold_left
                (fun (index, acc) subterm ->
                  let position = Path.at_index index position in
                  (index + 1, all_matches_aux patt subterm position acc))
                (0, acc)
                subterms
            in
            if pattern_matches patt node then position :: acc else acc)
    | Some res -> List.rev_append res acc

  let all_matches_with_hash_consing pattern node =
    match pattern with
    | Ex_patt patt -> (
        match get_focus patt with
        | Unfocused_tag -> all_matches_aux pattern node Path.root []
        | Focused_tag ->
            let paths = all_matches_aux pattern node Path.root [] in
            List.fold_left
              (fun acc context_path ->
                get_paths_of_focuses patt context_path acc)
              []
              paths)
end
