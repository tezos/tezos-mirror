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

type var_annot = Var_annot of Non_empty_string.t [@@ocaml.unboxed]

type type_annot = Type_annot of Non_empty_string.t [@@ocaml.unboxed]

type field_annot = Field_annot of Non_empty_string.t [@@ocaml.unboxed]

module FOR_TESTS = struct
  let unsafe_type_annot_of_string s =
    Type_annot (Non_empty_string.of_string_exn s)

  let unsafe_field_annot_of_string s =
    Field_annot (Non_empty_string.of_string_exn s)
end

let unparse_field_annot : field_annot option -> string list = function
  | None -> []
  | Some (Field_annot a) -> ["%" ^ (a :> string)]

let field_annot_opt_to_entrypoint_strict ~loc = function
  | None -> Ok Entrypoint.default
  | Some (Field_annot a) -> Entrypoint.of_annot_strict ~loc a

let field_annot_opt_eq_entrypoint_lax field_annot_opt entrypoint =
  match field_annot_opt with
  | None -> false
  | Some (Field_annot a) -> (
      match Entrypoint.of_annot_lax_opt a with
      | None -> false
      | Some a' -> Entrypoint.(a' = entrypoint))

let merge_field_annot :
    type error_trace.
    legacy:bool ->
    error_details:error_trace error_details ->
    field_annot option ->
    field_annot option ->
    (field_annot option, error_trace) result =
 fun ~legacy ~error_details annot1 annot2 ->
  match (annot1, annot2) with
  | (None, None) | (Some _, None) | (None, Some _) -> Result.return_none
  | (Some (Field_annot a1), Some (Field_annot a2)) ->
      if legacy || Non_empty_string.(a1 = a2) then ok annot1
      else
        Error
          (match error_details with
          | Fast -> Inconsistent_types_fast
          | Informative ->
              trace_of_error
              @@ Inconsistent_annotations
                   ("%" ^ (a1 :> string), "%" ^ (a2 :> string)))

let error_unexpected_annot loc annot =
  match annot with
  | [] -> Result.return_unit
  | _ :: _ -> error (Unexpected_annotation loc)

(* Check that the predicate p holds on all s.[k] for k >= i *)
let string_iter p s i =
  let len = String.length s in
  let rec aux i =
    if Compare.Int.(i >= len) then Result.return_unit
    else p s.[i] >>? fun () -> aux (i + 1)
  in
  aux i

let is_allowed_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '.' | '%' | '@' | '0' .. '9' -> true
  | _ -> false

(* Valid annotation characters as defined by the allowed_annot_char function from lib_micheline/micheline_parser *)
let check_char loc c =
  if is_allowed_char c then Result.return_unit
  else error (Unexpected_annotation loc)

(* This constant is defined in lib_micheline/micheline_parser which is not available in the environment. *)
let max_annot_length = 255

type annot_opt =
  | Field_annot_opt of Non_empty_string.t option
  | Type_annot_opt of Non_empty_string.t option
  | Var_annot_opt of Non_empty_string.t option

let percent = Non_empty_string.of_string_exn "%"

let percent_percent = Non_empty_string.of_string_exn "%%"

let at = Non_empty_string.of_string_exn "@"

let parse_annots loc ?(allow_special_var = false) ?(allow_special_field = false)
    l =
  (* allow empty annotations as wildcards but otherwise only accept
     annotations that start with [a-zA-Z_] *)
  let sub_or_wildcard wrap s =
    match Non_empty_string.of_string s with
    | None -> ok @@ wrap None
    | Some s -> (
        match (s :> string).[0] with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' ->
            (* check that all characters are valid*)
            string_iter (check_char loc) (s :> string) 1 >>? fun () ->
            ok @@ wrap (Some s)
        | _ -> error (Unexpected_annotation loc))
  in
  List.map_e
    (function
      | "@%" when allow_special_var -> ok @@ Var_annot_opt (Some percent)
      | "@%%" when allow_special_var ->
          ok @@ Var_annot_opt (Some percent_percent)
      | "%@" when allow_special_field -> ok @@ Field_annot_opt (Some at)
      | s -> (
          let len = String.length s in
          if Compare.Int.(len = 0 || len > max_annot_length) then
            error (Unexpected_annotation loc)
          else
            let rest = String.sub s 1 (len - 1) in
            match s.[0] with
            | ':' -> sub_or_wildcard (fun a -> Type_annot_opt a) rest
            | '@' -> sub_or_wildcard (fun a -> Var_annot_opt a) rest
            | '%' -> sub_or_wildcard (fun a -> Field_annot_opt a) rest
            | _ -> error (Unexpected_annotation loc)))
    l

let opt_var_of_var_opt = function None -> None | Some a -> Some (Var_annot a)

let opt_field_of_field_opt = function
  | None -> None
  | Some a -> Some (Field_annot a)

let opt_type_of_type_opt = function
  | None -> None
  | Some a -> Some (Type_annot a)

let classify_annot loc l :
    (var_annot option list * type_annot option list * field_annot option list)
    tzresult =
  try
    let (_, rv, _, rt, _, rf) =
      List.fold_left
        (fun (in_v, rv, in_t, rt, in_f, rf) a ->
          match (a, in_v, rv, in_t, rt, in_f, rf) with
          | (Var_annot_opt a, true, _, _, _, _, _)
          | (Var_annot_opt a, false, [], _, _, _, _) ->
              (true, opt_var_of_var_opt a :: rv, false, rt, false, rf)
          | (Type_annot_opt a, _, _, true, _, _, _)
          | (Type_annot_opt a, _, _, false, [], _, _) ->
              (false, rv, true, opt_type_of_type_opt a :: rt, false, rf)
          | (Field_annot_opt a, _, _, _, _, true, _)
          | (Field_annot_opt a, _, _, _, _, false, []) ->
              (false, rv, false, rt, true, opt_field_of_field_opt a :: rf)
          | _ -> raise Exit)
        (false, [], false, [], false, [])
        l
    in
    ok (List.rev rv, List.rev rt, List.rev rf)
  with Exit -> error (Ungrouped_annotations loc)

let get_one_annot loc = function
  | [] -> Result.return_none
  | [a] -> ok a
  | _ -> error (Unexpected_annotation loc)

let get_two_annot loc = function
  | [] -> ok (None, None)
  | [a] -> ok (a, None)
  | [a; b] -> ok (a, b)
  | _ -> error (Unexpected_annotation loc)

let check_type_annot : Script.location -> string list -> unit tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc vars >>? fun () ->
  error_unexpected_annot loc fields >>? fun () ->
  get_one_annot loc types >|? fun _a -> ()

let parse_composed_type_annot :
    Script.location ->
    string list ->
    (field_annot option * field_annot option) tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc vars >>? fun () ->
  get_one_annot loc types >>? fun _t ->
  get_two_annot loc fields >|? fun (f1, f2) -> (f1, f2)

let parse_field_annot :
    Script.location -> string list -> field_annot option tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc vars >>? fun () ->
  error_unexpected_annot loc types >>? fun () -> get_one_annot loc fields

let extract_field_annot :
    Script.node -> (Script.node * field_annot option) tzresult = function
  | Prim (loc, prim, args, annot) ->
      let rec extract_first acc = function
        | [] -> (None, annot)
        | s :: rest ->
            if Compare.Int.(String.length s > 0) && Compare.Char.(s.[0] = '%')
            then (Some s, List.rev_append acc rest)
            else extract_first (s :: acc) rest
      in
      let (field_annot, annot) = extract_first [] annot in
      (match field_annot with
      | None -> Result.return_none
      | Some field_annot -> parse_field_annot loc [field_annot])
      >|? fun field_annot -> (Prim (loc, prim, args, annot), field_annot)
  | expr -> ok (expr, None)

let check_correct_field :
    field_annot option -> field_annot option -> unit tzresult =
 fun f1 f2 ->
  match (f1, f2) with
  | (None, _) | (_, None) -> Result.return_unit
  | (Some (Field_annot s1), Some (Field_annot s2)) ->
      if Non_empty_string.(s1 = s2) then Result.return_unit
      else
        error
          (Inconsistent_field_annotations
             ("%" ^ (s1 :> string), "%" ^ (s2 :> string)))

let check_var_annot : Script.location -> string list -> unit tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc types >>? fun () ->
  error_unexpected_annot loc fields >>? fun () ->
  get_one_annot loc vars >|? fun (_a : var_annot option) -> ()

let ignore_special f =
  match f with
  | Some (Field_annot fa) when Non_empty_string.(fa = at) -> ok None
  | _ -> ok f

let parse_constr_annot :
    Script.location ->
    string list ->
    (field_annot option * field_annot option) tzresult =
 fun loc annot ->
  parse_annots ~allow_special_field:true loc annot >>? classify_annot loc
  >>? fun (vars, types, fields) ->
  get_one_annot loc vars >>? fun (_v : var_annot option) ->
  get_one_annot loc types >>? fun (_t : type_annot option) ->
  get_two_annot loc fields >>? fun (f1, f2) ->
  ignore_special f1 >>? fun f1 ->
  ignore_special f2 >|? fun f2 -> (f1, f2)

let check_two_var_annot : Script.location -> string list -> unit tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc types >>? fun () ->
  error_unexpected_annot loc fields >>? fun () ->
  get_two_annot loc vars >|? fun (_a1, _a2) -> ()

let parse_destr_annot :
    Script.location -> string list -> field_annot option tzresult =
 fun loc annot ->
  parse_annots loc ~allow_special_var:true annot >>? classify_annot loc
  >>? fun (vars, types, fields) ->
  error_unexpected_annot loc types >>? fun () ->
  get_one_annot loc vars >>? fun (_v : var_annot option) ->
  get_one_annot loc fields

let parse_unpair_annot :
    Script.location ->
    string list ->
    (field_annot option * field_annot option) tzresult =
 fun loc annot ->
  parse_annots loc ~allow_special_var:true annot >>? classify_annot loc
  >>? fun (vars, types, fields) ->
  error_unexpected_annot loc types >>? fun () ->
  get_two_annot loc vars >>? fun (_vcar, _vcdr) -> get_two_annot loc fields

let parse_entrypoint_annot :
    Script.location -> string list -> field_annot option tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc types >>? fun () ->
  get_one_annot loc fields >>? fun f ->
  get_one_annot loc vars >|? fun (_v : var_annot option) -> f

let parse_var_type_annot : Script.location -> string list -> unit tzresult =
 fun loc annot ->
  parse_annots loc annot >>? classify_annot loc >>? fun (vars, types, fields) ->
  error_unexpected_annot loc fields >>? fun () ->
  get_one_annot loc vars >>? fun (_v : var_annot option) ->
  get_one_annot loc types >|? fun (_t : type_annot option) -> ()
