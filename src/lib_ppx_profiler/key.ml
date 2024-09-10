(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type content =
  | Empty
  | Ident of string
  | String of string
  | List of Ppxlib.expression list
  | Apply of Ppxlib.expression * Parsetree.expression list
  | Other of Ppxlib.expression

type t = {label : string option; content : content}

let[@inline] content {content; _} = content

let rec embed_list loc = function
  | [] -> [%expr []]
  | [e] -> [%expr [[%e e]]]
  | a :: q -> [%expr [%e a] :: [%e embed_list loc q]]

let to_label loc {label; _} =
  match label with
  | Some name ->
      Ppxlib.Ast_builder.Default.(
        econstruct
          (constructor_declaration
             ~loc
             ~name:(Located.mk ~loc name)
             ~args:(Ppxlib_ast.Ast.Pcstr_tuple [])
             ~res:None))
        None
  | None ->
      Ppxlib.Ast_builder.Default.(
        econstruct
          (constructor_declaration
             ~loc
             ~name:(Located.mk ~loc "Terse")
             ~args:(Ppxlib_ast.Ast.Pcstr_tuple [])
             ~res:None))
        None

let to_expression loc {content; _} =
  match content with
  | Empty -> Ppxlib.Ast_builder.Default.eunit ~loc
  | Ident expr -> Ppxlib.Ast_builder.Default.(eapply ~loc (evar ~loc expr) [])
  | String string -> Ppxlib.Ast_builder.Default.estring ~loc string
  | Apply (expr, expr_list) ->
      Ppxlib.Ast_builder.Default.(eapply ~loc expr expr_list)
  | List expr_list -> embed_list loc expr_list
  | Other expr -> expr

let pp_expr_list ppf expr_list =
  (Format.pp_print_list Pprintast.expression) ppf expr_list

let pp_content ppf {content; _} =
  match content with
  | Empty -> Format.fprintf ppf "Empty"
  | Ident ident -> Format.fprintf ppf "Ident %s" ident
  | String string -> Format.fprintf ppf "String \"%s\"" string
  | List expr_list -> Format.fprintf ppf "List %a" pp_expr_list expr_list
  | Apply (expr, expr_list) ->
      Format.fprintf
        ppf
        "Apply %a %a"
        Pprintast.expression
        expr
        pp_expr_list
        expr_list
  | Other expr -> Format.fprintf ppf "Other %a" Pprintast.expression expr

let pp ppf t =
  Format.fprintf
    ppf
    "%s %a"
    (Option.value ~default:"No label" t.label)
    pp_content
    t
