(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Alpha_context
open Micheline
open Script_tc_errors

type var_annot = Var_annot

type type_annot = Type_annot

type field_annot = Field_annot of Non_empty_string.t [@@ocaml.unboxed]

let error_unexpected_annot loc annot =
  let open Result_syntax in
  match annot with
  | [] -> return_unit
  | _ :: _ -> tzfail (Unexpected_annotation loc)

(* Check that the predicate p holds on all s.[k] for k >= i *)
let string_iter p s i =
  let open Result_syntax in
  let len = String.length s in
  let rec aux i =
    if Compare.Int.(i >= len) then return_unit
    else
      let* () = p s.[i] in
      aux (i + 1)
  in
  aux i

let is_allowed_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9' -> true
  | _ -> false

(* Valid annotation characters as defined by the allowed_annot_char function from lib_micheline/micheline_parser *)
let check_char loc c =
  let open Result_syntax in
  if is_allowed_char c then return_unit else tzfail (Unexpected_annotation loc)

(* This constant is defined in lib_micheline/micheline_parser which is not available in the environment. *)
let max_annot_length = 255

type annot_opt =
  | Field_annot_opt of Non_empty_string.t option
  | Type_annot_opt of type_annot option
  | Var_annot_opt of var_annot option

let at = Non_empty_string.of_string_exn "@"

let parse_annot loc s =
  let open Result_syntax in
  (* allow empty annotations as wildcards but otherwise only accept
     annotations that start with [a-zA-Z_] *)
  let sub_or_wildcard wrap s =
    match Non_empty_string.of_string s with
    | None -> return @@ wrap None
    | Some s -> (
        match (s :> string).[0] with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
            (* check that all characters are valid*)
            let* () = string_iter (check_char loc) (s :> string) 1 in
            return @@ wrap (Some s)
        | _ -> tzfail (Unexpected_annotation loc))
  in
  let len = String.length s in
  if Compare.Int.(len = 0 || len > max_annot_length) then
    tzfail (Unexpected_annotation loc)
  else
    let rest = String.sub s 1 (len - 1) in
    match s.[0] with
    | ':' ->
        sub_or_wildcard
          (fun a ->
            Type_annot_opt
              (Option.map (fun (_ : Non_empty_string.t) -> Type_annot) a))
          rest
    | '@' ->
        sub_or_wildcard
          (fun a ->
            Var_annot_opt
              (Option.map (fun (_ : Non_empty_string.t) -> Var_annot) a))
          rest
    | '%' -> sub_or_wildcard (fun a -> Field_annot_opt a) rest
    | _ -> tzfail (Unexpected_annotation loc)

let parse_annots loc ?(allow_special_var = false) ?(allow_special_field = false)
    l =
  let open Result_syntax in
  List.map_e
    (function
      | "@%" when allow_special_var -> return @@ Var_annot_opt (Some Var_annot)
      | "@%%" when allow_special_var -> return @@ Var_annot_opt (Some Var_annot)
      | "%@" when allow_special_field -> return @@ Field_annot_opt (Some at)
      | s -> parse_annot loc s)
    l

let opt_field_of_field_opt = function
  | None -> None
  | Some a -> Some (Field_annot a)

let classify_annot loc l :
    (var_annot option list * type_annot option list * field_annot option list)
    tzresult =
  let open Result_syntax in
  try
    let _, rv, _, rt, _, rf =
      List.fold_left
        (fun (in_v, rv, in_t, rt, in_f, rf) a ->
          match (a, in_v, rv, in_t, rt, in_f, rf) with
          | Var_annot_opt a, true, _, _, _, _, _
          | Var_annot_opt a, false, [], _, _, _, _ ->
              (true, a :: rv, false, rt, false, rf)
          | Type_annot_opt a, _, _, true, _, _, _
          | Type_annot_opt a, _, _, false, [], _, _ ->
              (false, rv, true, a :: rt, false, rf)
          | Field_annot_opt a, _, _, _, _, true, _
          | Field_annot_opt a, _, _, _, _, false, [] ->
              (false, rv, false, rt, true, opt_field_of_field_opt a :: rf)
          | _ -> raise Exit)
        (false, [], false, [], false, [])
        l
    in
    return (List.rev rv, List.rev rt, List.rev rf)
  with Exit -> tzfail (Ungrouped_annotations loc)

let get_one_annot loc =
  let open Result_syntax in
  function
  | [] -> return_none
  | [a] -> return a
  | _ -> tzfail (Unexpected_annotation loc)

let get_two_annot loc =
  let open Result_syntax in
  function
  | [] -> return (None, None)
  | [a] -> return (a, None)
  | [a; b] -> return (a, b)
  | _ -> tzfail (Unexpected_annotation loc)

let check_type_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc vars in
  let* () = error_unexpected_annot loc fields in
  let+ (_a : type_annot option) = get_one_annot loc types in
  ()

let parse_field_annot :
    Script.location -> string -> Non_empty_string.t option tzresult =
  let open Result_syntax in
  fun loc annot ->
    if Compare.Int.(String.length annot <= 0) || Compare.Char.(annot.[0] <> '%')
    then return_none
    else
      let+ annot_opt = parse_annot loc annot in
      match annot_opt with Field_annot_opt annot_opt -> annot_opt | _ -> None

let is_field_annot loc a =
  let open Result_syntax in
  let+ result = parse_field_annot loc a in
  Option.is_some result

let extract_field_annot :
    Script.node -> (Script.node * Non_empty_string.t option) tzresult =
  let open Result_syntax in
  function
  | Prim (loc, prim, args, annot) as expr ->
      let rec extract_first acc = function
        | [] -> return (expr, None)
        | s :: rest -> (
            let* str_opt = parse_field_annot loc s in
            match str_opt with
            | None -> extract_first (s :: acc) rest
            | Some _ as some_field_annot ->
                let annot = List.rev_append acc rest in
                return (Prim (loc, prim, args, annot), some_field_annot))
      in
      extract_first [] annot
  | expr -> return (expr, None)

let has_field_annot node =
  let open Result_syntax in
  let+ _node, result = extract_field_annot node in
  Option.is_some result

let remove_field_annot node =
  let open Result_syntax in
  let+ node, _a = extract_field_annot node in
  node

let extract_entrypoint_annot node =
  let open Result_syntax in
  let+ node, field_annot_opt = extract_field_annot node in
  ( node,
    Option.bind field_annot_opt (fun field_annot ->
        Entrypoint.of_annot_lax_opt field_annot) )

let check_var_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc types in
  let* () = error_unexpected_annot loc fields in
  let+ (_a : var_annot option) = get_one_annot loc vars in
  ()

let check_constr_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots ~allow_special_field:true loc annot in
    classify_annot loc annots
  in
  let* (_v : var_annot option) = get_one_annot loc vars in
  let* (_t : type_annot option) = get_one_annot loc types in
  let+ _f1, _f2 = get_two_annot loc fields in
  ()

let check_two_var_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc types in
  let* () = error_unexpected_annot loc fields in
  let+ _a1, _a2 = get_two_annot loc vars in
  ()

let check_destr_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc ~allow_special_var:true annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc types in
  let* (_v : var_annot option) = get_one_annot loc vars in
  let+ (_f : field_annot option) = get_one_annot loc fields in
  ()

let check_unpair_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc ~allow_special_var:true annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc types in
  let* _vcar, _vcdr = get_two_annot loc vars in
  let+ _f1, _f2 = get_two_annot loc fields in
  ()

let parse_entrypoint_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc types in
  let* f = get_one_annot loc fields in
  let+ (_v : var_annot option) = get_one_annot loc vars in
  f

let parse_entrypoint_annot_strict loc annot =
  let open Result_syntax in
  let* entrypoint_annot = parse_entrypoint_annot loc annot in
  match entrypoint_annot with
  | None -> Ok Entrypoint.default
  | Some (Field_annot a) -> Entrypoint.of_annot_strict ~loc a

let parse_entrypoint_annot_lax loc annot =
  let open Result_syntax in
  let* entrypoint_annot = parse_entrypoint_annot loc annot in
  match entrypoint_annot with
  | None -> Ok Entrypoint.default
  | Some (Field_annot annot) -> Entrypoint.of_annot_lax annot

let check_var_type_annot loc annot =
  let open Result_syntax in
  let* vars, types, fields =
    let* annots = parse_annots loc annot in
    classify_annot loc annots
  in
  let* () = error_unexpected_annot loc fields in
  let* (_v : var_annot option) = get_one_annot loc vars in
  let+ (_t : type_annot option) = get_one_annot loc types in
  ()
