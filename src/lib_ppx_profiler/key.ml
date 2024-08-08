(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Empty
  | Ident of string
  | String of string
  | List of Ppxlib.expression list
  | Apply of Ppxlib.expression * Parsetree.expression list
  | Other of Ppxlib.expression

let rec embed_list loc = function
  | [] -> [%expr []]
  | [e] -> [%expr [[%e e]]]
  | a :: q -> [%expr [%e a] :: [%e embed_list loc q]]

let to_expression loc = function
  | Empty -> Ppxlib.Ast_builder.Default.eunit ~loc
  | Ident expr -> Ppxlib.Ast_builder.Default.(eapply ~loc (evar ~loc expr) [])
  | String string -> Ppxlib.Ast_builder.Default.estring ~loc string
  | Apply (expr, expr_list) ->
      Ppxlib.Ast_builder.Default.(eapply ~loc expr expr_list)
  | List expr_list -> embed_list loc expr_list
  | Other expr -> expr

let pp_expr_list ppf expr_list =
  (Format.pp_print_list Pprintast.expression) ppf expr_list

let pp ppf = function
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
