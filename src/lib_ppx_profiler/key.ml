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
  | Apply of Ppxlib.expression * (Ppxlib.arg_label * Ppxlib.expression) list
  | Other of Ppxlib.expression

type t = {
  verbosity : string option;
  profiler_module : string option;
  cpu_profiling : bool option;
  metadata : Ppxlib.expression option;
  content : content;
  driver_ids : Handled_drivers.t;
}

let[@inline] content {content; _} = content

let rec embed_list loc = function
  | [] -> [%expr []]
  | [e] -> [%expr [[%e e]]]
  | a :: q -> [%expr [%e a] :: [%e embed_list loc q]]

let get_verbosity loc {verbosity; _} =
  match verbosity with
  | Some name ->
      Some
        (Ppxlib.Ast_builder.Default.(
           econstruct
             (constructor_declaration
                ~loc
                ~name:(Located.mk ~loc name)
                ~args:(Ppxlib_ast.Ast.Pcstr_tuple [])
                ~res:None))
           None)
  | None -> None

(* This could return a module_expr instead but right now it works
   and that's all we're asking *)
let get_profiler_module {profiler_module; _} =
  match profiler_module with
  | Some name -> Ppxlib.Lident name
  | None -> Ppxlib.Lident "Profiler"

let to_expression loc {content; _} =
  match content with
  | Empty -> Ppxlib.Ast_builder.Default.eunit ~loc
  | Ident expr -> Ppxlib.Ast_builder.Default.(eapply ~loc (evar ~loc expr) [])
  | String string -> Ppxlib.Ast_builder.Default.estring ~loc string
  | Apply (expr, expr_list) ->
      Ppxlib.Ast_builder.Default.(pexp_apply ~loc expr expr_list)
  | List expr_list -> embed_list loc expr_list
  | Other expr -> expr

let pp_expr_list ppf expr_list =
  (Format.pp_print_list Ppxlib.Pprintast.expression) ppf expr_list

let pp_labeled_expr_list ppf expr_list =
  Format.pp_print_list
    (fun ppf (label, expr) ->
      Format.fprintf
        ppf
        "%s%a"
        (match label with
        | Ppxlib.Nolabel -> ""
        | Labelled name -> "~" ^ name
        | Optional name -> "?" ^ name)
        Ppxlib.Pprintast.expression
        expr)
    ppf
    expr_list

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
        Ppxlib.Pprintast.expression
        expr
        pp_labeled_expr_list
        expr_list
  | Other expr -> Format.fprintf ppf "Other %a" Ppxlib.Pprintast.expression expr

let pp ppf t =
  Format.fprintf
    ppf
    "@[<v 0>verbosity: %s;@,profiler_module: %s;@,content: %a@]"
    (Option.value ~default:"No verbosity" t.verbosity)
    (Option.value ~default:"No profiler module" t.profiler_module)
    pp_content
    t
